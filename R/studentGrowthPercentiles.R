`studentGrowthPercentiles` <-
function(panel.data,         ## REQUIRED
         sgp.labels,         ## REQUIRED
         panel.data.vnames,
	 additional.vnames.to.return=NULL,
         grade.progression,
         content.area.progression=NULL,
         year.progression=NULL,
         year.progression.lags=NULL,
         num.prior,
         max.order.for.percentile=NULL,
         subset.grade,
         percentile.cuts=NULL,
         growth.levels, 
         use.my.knots.boundaries,
         use.my.coefficient.matrices=NULL,
         calculate.confidence.intervals=NULL,
         print.other.gp=FALSE,
         print.sgp.order=FALSE, 
         calculate.sgps=TRUE, 
         rq.method="br",
         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),
         knots.boundaries.by.panel=FALSE,
         exact.grade.progression.sequence=FALSE,
         drop.nonsequential.grade.progression.variables=TRUE,
         convert.0and100=TRUE,
         sgp.quantiles="Percentiles",
         sgp.loss.hoss.adjustment=NULL,
         percuts.digits=0,
         isotonize=TRUE,
         convert.using.loss.hoss=TRUE,
         goodness.of.fit=TRUE,
         return.prior.scale.score=TRUE,
         return.prior.scale.score.standardized=TRUE,
         return.norm.group.identifier=TRUE,
         print.time.taken=TRUE,
         parallel.config=NULL,
         calculate.simex=NULL,
	 verbose.output=FALSE) {

	started.at <- proc.time()
	started.date <- date()

	##########################################################
	###
	### Internal utility functions
	###
	##########################################################

	.smooth.isotonize.row <- function(x, iso=isotonize) {
        	x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
        	if (iso) return(sort(x))
        	else return(x)
	}

        .create.path <- function(labels, pieces=c("my.subject", "my.year", "my.extra.label")) {
            sub(' ', '_', toupper(sub('\\.+$', '', paste(unlist(sapply(labels[pieces], as.character)), collapse="."))))
        }

	.get.knots.boundaries <- function(data, by.grade) {

		num.panels <- (dim(data)[2]-1)/2

		if (knots.boundaries.by.panel) {
			tmp.years <- rep(yearIncrement(sgp.labels$my.year, (-num.panels+1):-1), each=dim(data)[1])
		} else {
			tmp.years <- rep(sgp.labels$my.year, dim(data)[1]*(num.panels-1))
		}

		if (by.grade) {
			tmp.grades <- as.vector(sapply(data[,2:(2+num.panels-2), with=FALSE], as.character))
		} else {
			tmp.grades <- rep(head(tmp.gp, -1), each=dim(data)[1])
		}

		tmp.stack <- data.table(
			VALID_CASE="VALID_CASE",
			CONTENT_AREA=rep(head(content.area.progression, -1), each=dim(data)[1]),
			GRADE=tmp.grades, 
			SCALE_SCORE=as.vector(sapply(data[,(2+num.panels):(2+2*num.panels-2), with=FALSE], as.numeric)),
			YEAR=tmp.years, key=c("VALID_CASE", "CONTENT_AREA", "YEAR")) 

		createKnotsBoundaries(tmp.stack, knot.cut.percentiles)
	}

	.get.panel.data <- function(tmp.data, k, by.grade) {
		str1 <- paste(" & !is.na(tmp.data[[", 1+2*num.panels, "]])", sep="")
		str2 <- paste(" & tmp.data[[", 1+num.panels, "]]=='", tmp.last, "'", sep="")
		str3 <- 1+2*num.panels
		for (i in 1:k) {
			str1 <- paste(str1, " & !is.na(tmp.data[[", 1+2*num.panels-i, "]])", sep="")
			str2 <- paste(str2, " & tmp.data[[", 1+num.panels-i, "]]=='", rev(as.character(tmp.gp))[i+1], "'", sep="")
			str3 <- c(1+2*num.panels-i, str3)
		}
		if (by.grade) {
			tmp.data[eval(parse(text=paste(substring(str1, 4), str2, sep="")))][, c(1, str3), with=FALSE]
		} else {
			tmp.data[eval(parse(text=substring(str1, 3)))][, c(1, str3), with=FALSE]
		}
	}

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.knots.boundaries.names <- 
			names(Knots_Boundaries[[tmp.path.knots.boundaries]])[content_area==sapply(strsplit(names(Knots_Boundaries[[tmp.path.knots.boundaries]]), "[.]"), '[', 1)]
		if (length(tmp.knots.boundaries.names)==0) {
			return(paste("[['", tmp.path.knots.boundaries, "']]", sep=""))
		} else {
			tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), function(x) x[2])
			if (any(!is.na(tmp.knots.boundaries.years))) {
				if (year %in% tmp.knots.boundaries.years) {
					return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", year, "']]", sep=""))
				} else {
					if (year==sort(c(year, tmp.knots.boundaries.years))[1]) {
						return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, "']]", sep=""))
					} else {
						return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", rev(sort(tmp.knots.boundaries.years))[1], "']]", sep=""))
					}
				}
			} else {
				return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, "']]", sep=""))
			}
		}
	}
        
	.create.coefficient.matrices <- function(data, k, by.grade) {
		tmp.data <- .get.panel.data(data, k, by.grade)
		if (dim(tmp.data)[1]==0) return(NULL)
		tmp.num.variables <- dim(tmp.data)[2]
		mod <- character()
		s4Ks <- "Knots=list("
		s4Bs <- "Boundaries=list("
		tmp.gp.iter <- rev(tmp.gp)[2:(k+1)]
		for (i in seq_along(tmp.gp.iter)) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(rev(content.area.progression)[i+1], yearIncrement(rev(year.progression)[i+1], 0))
			.check.knots.boundaries(names(eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, sep="")))), tmp.gp.iter[i])
			knt <- paste("Knots_Boundaries", my.path.knots.boundaries, "[['knots_", tmp.gp.iter[i], "']]", sep="")
			bnd <- paste("Knots_Boundaries", my.path.knots.boundaries, "[['boundaries_", tmp.gp.iter[i], "']]", sep="")
			mod <- paste(mod, " + bs(tmp.data[[", tmp.num.variables-i, "]], knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
			s4Ks <- paste(s4Ks, "knots_", tmp.gp.iter[i], "=", knt, ",", sep="")
			s4Bs <- paste(s4Bs, "boundaries_", tmp.gp.iter[i], "=", bnd, ",", sep="")
		}
		if (is.null(parallel.config)) {
			tmp.mtx <- eval(parse(text=paste("rq(tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=taus, data=tmp.data, method=rq.method)[['coefficients']]", sep="")))
		} else {
			par.start <- startParallel(parallel.config, 'TAUS', qr.taus=taus) #  Need new argument here - default to missing
	
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp.mtx <- foreach(j = iter(par.start$TAUS.LIST), .combine = "cbind", .packages="quantreg", .inorder=TRUE, .options.mpi=par.start$foreach.options,
					.options.multicore=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					eval(parse(text=paste("rq(tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=j, data=tmp.data, method=rq.method)[['coefficients']]", sep="")))
				}
			} else {
				if (par.start$par.type == 'MULTICORE') {
					tmp.mtx <- mclapply(par.start$TAUS.LIST, function(x) eval(parse(text=paste("rq(tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), 
						", tau=x, data=tmp.data, method=rq.method)[['coefficients']]", sep=""))), mc.cores=par.start$workers)
					tmp.mtx <- do.call(cbind, tmp.mtx)
				}
				
				if (par.start$par.type == 'SNOW') {
					tmp.mtx <- parLapply(par.start$internal.cl, par.start$TAUS.LIST, function(x) eval(parse(text=paste("rq(tmp.data[[", 
						tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=x, data=tmp.data, method=rq.method)[['coefficients']]", sep=""))))
					tmp.mtx <- do.call(cbind, tmp.mtx)
				}
			}
			stopParallel(parallel.config, par.start)
		}

		tmp.version <- list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date())

		eval(parse(text=paste("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1, nchar(s4Ks)-1), "), ", substring(s4Bs, 1, nchar(s4Bs)-1), "), ",
			"Content_Areas=list(as.character(tail(content.area.progression, k+1))), ",
			"Grade_Progression=list(as.character(tail(tmp.slot.gp, k+1))), ",
			"Time=list(as.character(tail(year.progression, k+1))), ",
			"Time_Lags=list(as.integer(tail(year.progression.lags, k))), ",
			"Version=tmp.version)", sep="")))

	} ### END .create.coefficient.matrices

	.check.knots.boundaries <- function(names, grade) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		if (!grade %in% tmp[tmp[,1]=="knots", 2]) stop(paste("knots_", grade, " not found in Knots_Boundaries.", sep=""))
		if (!grade %in% tmp[tmp[,1]=="boundaries", 2]) stop(paste("boundaries_", grade, " not found in Knots_Boundaries.", sep=""))                           
	}

	.create_taus <- function(sgp.quantiles) {
		if (is.character(sgp.quantiles)) {
			taus <- switch(sgp.quantiles,
				PERCENTILES = (1:100-0.5)/100)
		}
		if (is.numeric(sgp.quantiles)) {
			taus <- sgp.quantiles
		}
		return(taus)
	}

	get.coefficient.matrix.name <- function(tmp.last, k) {
		return(paste("qrmatrix_", tmp.last, "_", k, sep=""))
	}

	.get.percentile.predictions <- function(my.data, my.matrix) {
		SCORE <- NULL
		tmp.num.variables <- dim(my.data)[2]
		mod <- character()
		int <- "cbind(rep(1, dim(my.data)[1]),"
		for (k in seq(length(my.matrix@Time_Lags[[1]]))) {
			knt <- paste("my.matrix@Knots[[", k, "]]", sep="")
			bnd <- paste("my.matrix@Boundaries[[", k, "]]", sep="")
			mod <- paste(mod, ", bs(my.data[[", tmp.num.variables-k, "]], knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
		}	
		tmp <- eval(parse(text=paste(int, substring(mod, 2), ") %*% my.matrix", sep="")))
		return(round(matrix(data.table(ID=rep(seq(dim(tmp)[1]), each=100), SCORE=as.vector(t(tmp)))[,.smooth.isotonize.row(SCORE), by=ID][['V1']], ncol=100, byrow=TRUE), digits=5))
	}

	.get.quantiles <- function(data1, data2) {
		TMP_TF <- NULL
		tmp <- data.table(ID=rep(seq(dim(data1)[1]), each=101), TMP_TF=as.vector(t(cbind(data1 < data2, FALSE))))[,which.min(TMP_TF)-1, by=ID][['V1']]
		if (!is.null(sgp.loss.hoss.adjustment)) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
			my.hoss <- eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']][2]", sep="")))
			tmp.index <- which(data2==my.hoss)
			if (length(tmp.index) > 0) {
				tmp[tmp.index] <- apply(cbind(data1 > data2, TRUE)[tmp.index,,drop=FALSE], 1, function(x) which.max(x)-1)
			}
		}
		if (convert.0and100) {
			tmp[tmp==0] <- 1
			tmp[tmp==100] <- 99
		}
		return(as.integer(tmp))
	}

	.get.percentile.cuts <- function(data1) {
		tmp <- round(data1[ , percentile.cuts+1, drop=FALSE], digits=percuts.digits)
		if (convert.using.loss.hoss) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
			bnd <- eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']]", sep="")))
			tmp[tmp < bnd[1]] <- bnd[1]
			tmp[tmp > bnd[2]] <- bnd[2]
		}
		colnames(tmp) <- paste("PERCENTILE_CUT_", percentile.cuts, sep="")
		return(tmp)
	} 

	.simex.sgp <- function(state, variable, lambda, B, extrapolation) {
		GRADE <- CONTENT_AREA <- YEAR <- V1 <- Lambda <- tau <- b <- PREDICTED_VALUES <- NULL ## To avoid R CMD check warnings
		content_area <- sgp.labels$my.subject
		year <- sgp.labels$my.year
		my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
    
		fitted <- extrap <- tmp.quantiles.simex <- list()
		loss.hoss <- matrix(nrow=2,ncol=length(tmp.gp)-1)
		for (g in 1:ncol(loss.hoss)) {
			loss.hoss[,g] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", rev(tmp.gp)[-1][g], sep="")]]
		}
		
		rqfit <- function(tmp.gp.iter, lam) {  #  AVI added in the lam  argument here to index the lambda specific knots and boundaries
		  mod <- character()
		  for (i in seq_along(tmp.gp.iter)) {
		    knt <- paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", lam, "']][['knots_", tmp.gp.iter[i], "']]", sep="")
		    bnd <- paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", lam, "']][['boundaries_", tmp.gp.iter[i], "']]", sep="")
		    mod <- paste(mod, " + bs(prior_", i, ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
		  }
		  return(parse(text=paste("rq(final.yr ~", substring(mod,4), ", tau=taus,data=data, method=rq.method)[['fitted.values']]", sep="")))
		}
		
		for (k in coefficient.matrix.priors) {
			data <- .get.panel.data(ss.data, k, by.grade)
			tmp.num.variables <- dim(data)[2]
			tmp.gp.iter <- rev(tmp.gp)[2:(k+1)]
			csem.int <- matrix(nrow=dim(data)[1],ncol=length(tmp.gp.iter)) # build matrix to store interpolated csem
			colnames(csem.int) <- paste("icsem", tmp.gp.iter,sep="")
			
			# interpolate csem for all scale scores except that of the last grade

			if (!is.null(state)){
				for (g in seq_along(tmp.gp.iter)) {
			        if ("YEAR" %in% names(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			        	CSEM_Data <- subset(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==tmp.gp.iter[g] & CONTENT_AREA==content_area & YEAR==year)
			        } else {
			        	CSEM_Data <- subset(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==tmp.gp.iter[g] & CONTENT_AREA==content_area)
			        }
			        CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
			        csem.int[,paste("icsem",tmp.gp.iter[g],sep="")] <- CSEM_Function(data[[tmp.num.variables-g]])
				}
			}
			if (!is.null(variable)){
				for (g in tmp.gp.iter) {
					csem.int[,paste("icsem",g,sep="")] <- variable[[paste("CSEM.grade",g,sep="")]]
				}
			}
			
			# naive model

			fitted[[paste("order_",k,sep="")]]<-matrix(0,nrow=length(lambda),ncol=dim(data)[1]*length(taus))
			tmp.matrix <- getsplineMatrix(
				Coefficient_Matrices[[tmp.path.coefficient.matrices]], 
				tail(content.area.progression, k+1), 
				tail(grade.progression, k+1),
				tail(year.progression, k+1),
				tail(year.progression.lags, k),
				my.matrix.order=k)
			
			fitted[[paste("order_",k,sep="")]][1,] <- as.vector(.get.percentile.predictions(data, tmp.matrix))
			
			# perturbe data
			
			for (L in lambda[-1]) {
				big.data <- rbindlist(replicate(B, data, simplify = FALSE))
				big.data[, Lambda := rep(L, each=dim(data)[1]*B)]
				big.data[, b := rep(1:B, each=dim(data)[1])]
				setnames(big.data,tmp.num.variables,"final.yr")
					for (g in seq_along(tmp.gp.iter)) {
						big.data[, paste("icsem", tmp.gp.iter[g], sep="") := rep(csem.int[, paste("icsem", tmp.gp.iter[g], sep="")], time=B)]
						big.data[, tmp.num.variables-g := eval(parse(text=paste("big.data[[", tmp.num.variables-g,
							"]] + sqrt(big.data[['Lambda']]) * big.data[['icsem", tmp.gp.iter[g], "']] * rnorm(dim(big.data)[1])", sep="")))]
						col.index <- tmp.num.variables-g
						big.data[big.data[[col.index]] < loss.hoss[1,g], col.index := loss.hoss[1,g], with=F]
						big.data[big.data[[col.index]] > loss.hoss[2,g], col.index := loss.hoss[2,g], with=F] 
						ks <- big.data[, as.list(as.vector(unlist(round(quantile(big.data[[col.index]], probs=c(0.2,0.4,0.6,0.8), na.rm=TRUE), digits=3))))] # Knots
						bs <- big.data[, as.list(as.vector(round(extendrange(big.data[[col.index]], f=0.1), digits=3)))] # Boundaries
						lh <- big.data[, as.list(as.vector(round(extendrange(big.data[[col.index]], f=0.0), digits=3)))] # LOSS/HOSS
			    
						eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['knots_", tmp.gp.iter[g], 
							"']] <- c(ks[,V1], ks[,V2], ks[,V3], ks[,V4])", sep="")))
						eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['boundaries_", tmp.gp.iter[g], 
							"']] <- c(bs[,V1], bs[,V2])", sep="")))
						eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['loss.hoss_", tmp.gp.iter[g], 
							"']] <- c(lh[,V1], lh[,V2])", sep="")))
            
						setnames(big.data,tmp.num.variables-g,paste("prior_",g,sep=""))
					}
			  		  
				if (is.null(parallel.config)) { # Sequential
					fitted.b <- big.data[,eval(rqfit(tmp.gp.iter[1:k], lam=L)), by='b']
					fitted.b[, ID := rep(data$ID, time = (B*length(taus)))]
					fitted.b[,tau := rep(taus, each=dim(data)[1], time=B)]
				} else {	# Parallel over 1:B
					if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
						##  Don't offer this option now.  But this could ultimately be the BEST option for this because we could have 
						##  nested foreach loops around Lambda, B and even the priors/orders if we have access to enough cores (cluster)
						message("\t\tNOTE: FOREACH backend in not currently available for SIMEX.  Changing to BACKEND='PARALLEL' and TYPE will be set to OS default.")
						parallel.config[["BACKEND"]] <- "PARALLEL"
					} 
			    
					par.start <- startParallel(parallel.config, 'SIMEX') 
					##  Note, that if you use the parallel.config for SIMEX here, you also need to use it for TAUS in the naive analysis
					##  Example parallel.config argument:  '... parallel.config=list(BACKEND="PARALLEL", TYPE="SOCK", WORKERS=list(SIMEX = 4, TAUS = 4))'
					if (par.start$par.type == 'MULTICORE') {
						tmp.fitted.b <- mclapply(1:B, function(x) big.data[b==x][,eval(rqfit(tmp.gp.iter[1:k], lam=L))], mc.cores=par.start$workers)
					}
					if (par.start$par.type == 'SNOW') {
						tmp.fitted.b <- parLapply(par.start$internal.cl, 1:B, function(x) big.data[b==x][,eval(rqfit(tmp.gp.iter[1:k], lam=L))])
					}
					fitted.b <- data.table(do.call(c, as.vector(tmp.fitted.b)), ID = rep(data$ID, time=(B*length(taus))), 
						tau=rep(taus, each=dim(data)[1], time=B), b = rep(1:B, each = dim(data)[1]*length(taus)), key=c('ID', 'b'))
					stopParallel(parallel.config, par.start)
				}

				# Make new variable 'PREDICTED_VALUES' to compare with 'V1' and to preserve 'tau'
				fitted.b[, PREDICTED_VALUES := .smooth.isotonize.row(V1), by=list(ID, b)] #  Could just use V1 := ... in final implimentation to keep from adding a column here
				fitted[[paste("order_",k,sep="")]][which(lambda==L),] <- fitted.b[, mean(PREDICTED_VALUES), by=list(ID, tau)][['V1']] 
			
			} ### END for (L in lambda[-1])

			switch(extrapolation, QUADRATIC = fit <- lm(fitted[[paste("order_",k,sep="")]] ~ lambda + I(lambda^2)), LINEAR = fit <- lm(fitted[[paste("order_",k,sep="")]]~ lambda))
			extrap[[paste("order_",k,sep="")]]<-as.data.table(matrix(predict(fit,newdata=data.frame(lambda=-1)), nrow=dim(data)[1]))
			tmp.quantiles.simex[[k]] <- data.table(ID=data[["ID"]], ORDER=k, SGP_SIMEX=.get.quantiles(extrap[[paste("order_",k,sep="")]], data[[tmp.num.variables]]))
		} ### END for (k in coefficient.matrix.priors)

		quantile.data.simex <- data.table(rbindlist(tmp.quantiles.simex),key="ID")
		
		if (print.other.gp) {
			quantile.data.simex <- data.table(reshape(quantile.data.simex, idvar="ID", timevar="ORDER", direction="wide"),
				SGP_SIMEX=quantile.data.simex[c(which(!duplicated(quantile.data.simex))[-1]-1L, nrow(quantile.data.simex))][["SGP_SIMEX"]])
		} else {
			if (print.sgp.order | return.norm.group.identifier) {
				quantile.data.simex <- quantile.data.simex[c(which(!duplicated(quantile.data.simex))[-1]-1L, nrow(quantile.data.simex))]
			} else {
				quantile.data.simex <- quantile.data.simex[c(which(!duplicated(quantile.data.simex))[-1]-1L, nrow(quantile.data.simex)), c("ID", "SGP_SIMEX"), with=FALSE]
			}
		}
		quantile.data.simex[, ORDER:=NULL]
		return (quantile.data.simex)
	} ### END .simex.sgp function

	split.location <- function(years) sapply(strsplit(years, '_'), length)[1]


	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	ID <- tmp.messages <- ORDER <- SCALE_SCORE_PRIOR <- TEMP_SGP_SIM <- NULL

	if (missing(panel.data)) {
		stop("User must supply student achievement data for student growth percentile calculations. NOTE: data is now supplied to function using panel.data argument. See help page for details.")
	}
	if (!(is.matrix(panel.data) | is.list(panel.data))) {
		stop("Supplied panel.data not of a supported class. See help for details of supported classes")
	}
	if (identical(class(panel.data), "list")) {
		if (!("Panel_Data" %in% names(panel.data))) {
			stop("Supplied panel.data missing Panel_Data")
	}
	}
	if (identical(class(panel.data), "list")) {
		if (!is.data.frame(panel.data[["Panel_Data"]]) & !is.data.table(panel.data[["Panel_Data"]])) {
			stop("Supplied panel.data$Panel_Data is not a data.frame or a data.table")
		}
	}
	if (identical(class(panel.data), "list") & !is.null(panel.data[['Coefficient_Matrices']])) {
		panel.data[['Coefficient_Matrices']] <- checksplineMatrix(panel.data[['Coefficient_Matrices']])
	}

	if (!missing(sgp.labels)) {
		if (!is.list(sgp.labels)) {
			stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
	}}
	if (!identical(names(sgp.labels), c("my.year", "my.subject")) &
		!identical(names(sgp.labels), c("my.year", "my.subject", "my.extra.label"))) {
		stop("Please specify an appropriate list for sgp.labels. See help page for details.")
	}
	sgp.labels <- lapply(sgp.labels, toupper)
	tmp.path <- .create.path(sgp.labels)

	if (!missing(growth.levels)) {
		tmp.growth.levels <- list()
		if (!is.list(growth.levels) & !is.character(growth.levels)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: growth.levels must be supplied as a list or character abbreviation. See help page for details. studentGrowthPercentiles will be calculated without augmented growth.levels\n")
			tf.growth.levels <- FALSE
		}
		if (is.list(growth.levels)) {
			if (!identical(names(growth.levels), c("my.cuts", "my.levels"))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for growth.levels. See help page for details. Student growth percentiles will be calculated without augmented growth.levels\n")
				tf.growth.levels <- FALSE
			} else {
				tmp.growth.levels <- growth.levels
				tf.growth.levels <- TRUE
			} 
		}
		if (is.character(growth.levels)) {
			if (is.null(SGPstateData[[growth.levels]][["Growth"]][["Levels"]])) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Growth Levels are currently not specified for the indicated state. \n\tPlease contact the SGP package administrator to have your state's data included in the package. Student growth percentiles will be calculated without augmented growth levels\n")
				tf.growth.levels <- FALSE
			} else {
			tmp.growth.levels[["my.cuts"]] <- SGPstateData[[growth.levels]][["Growth"]][["Cutscores"]][["Cuts"]]
			tmp.growth.levels[["my.levels"]] <- SGPstateData[[growth.levels]][["Growth"]][["Levels"]]
				tf.growth.levels <- TRUE
			}
		}
	} else {
		tf.growth.levels <- FALSE
	}

	if (!missing(use.my.knots.boundaries)) {
		if (!is.list(use.my.knots.boundaries) & !is.character(use.my.knots.boundaries)) {
			stop("use.my.knots.boundaries must be supplied as a list or character abbreviation. See help page for details.")
		}
		if (is.list(use.my.knots.boundaries)) {
			if (!identical(class(panel.data), "list")) {
				stop("use.my.knots.boundaries is only appropriate when panel data is of class list. See help page for details.")
			}
			if (!identical(names(use.my.knots.boundaries), c("my.year", "my.subject")) & 
				!identical(names(use.my.knots.boundaries), c("my.year", "my.subject", "my.extra.label"))) {
					stop("Please specify an appropriate list for use.my.knots.boundaries. See help page for details.")
			}
			tmp.path.knots.boundaries <- .create.path(use.my.knots.boundaries, pieces=c("my.subject", "my.year"))
			if (is.null(panel.data[["Knots_Boundaries"]]) | is.null(panel.data[["Knots_Boundaries"]][[tmp.path.knots.boundaries]])) {
				stop("Knots and Boundaries indicated by use.my.knots.boundaries are not included.")
			}
		}
		if (is.character(use.my.knots.boundaries)) {
			if (is.null(SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) { 
				tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Knots and Boundaries are currently not implemented for the state indicated (", use.my.knots.boundaries, "). Knots and boundaries will be calculated from the data. Please contact the SGP package administrator to have your Knots and Boundaries included in the package\n", sep=""))
			}
    	 		tmp.path.knots.boundaries <- .create.path(sgp.labels, pieces=c("my.subject", "my.year"))
		}
	} else {
     		tmp.path.knots.boundaries <- .create.path(sgp.labels, pieces=c("my.subject", "my.year"))
	}

	if (!is.null(use.my.coefficient.matrices) & !identical(use.my.coefficient.matrices, TRUE)) {
		if (!identical(class(panel.data), "list")) {
			stop("use.my.coefficient.matrices is only appropriate when panel data is of class list. See help page for details.")
		}
		if (!is.list(use.my.coefficient.matrices)) {
			stop("Please specify an appropriate list for argument 'use.my.coefficient.matrices'. See help page for details.")
		}
		if (!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject")) & 
			!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject", "my.extra.label"))) {
				stop("Please specify an appropriate list for argument 'use.my.coefficient.matrices'. See help page for details.")
		}
		tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices)
		if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
			stop("Coefficient matrices indicated by argument 'use.my.coefficient.matrices' are not included.")
		}
	} else {
		tmp.path.coefficient.matrices <- tmp.path
	}

	if (is.character(sgp.quantiles)) {
		sgp.quantiles <- toupper(sgp.quantiles)
		if (sgp.quantiles != "PERCENTILES") {
			stop("Character options for sgp.quantiles include only Percentiles at this time. Other options available by specifying a numeric quantity. See help page for details.")
	}} 
	if (is.numeric(sgp.quantiles)) {
		if (!(all(sgp.quantiles > 0 & sgp.quantiles < 1))) {
			stop("Specify sgp.quantiles as as a vector of probabilities between 0 and 1.")
	}}
	if (!is.null(percentile.cuts)) {
		if (sgp.quantiles != "PERCENTILES") {
			stop("percentile.cuts only appropriate for growth percentiles. Set sgp.quantiles to Percentiles to produce requested percentile.cuts.")
		}
		if (!all(percentile.cuts %in% 0:100)) {
			stop("Specified percentile.cuts must be integers between 0 and 100.")
	}}
	if (!calculate.sgps & (is.character(goodness.of.fit) | goodness.of.fit==TRUE)) {
		tmp.messages <- c(tmp.messages, "\t\tNOTE: Goodness-of-Fit tables only produced when calculating SGPs.\n")
	}
	if (!is.null(calculate.confidence.intervals)) {
		csem.tf <- TRUE
		if (!is.character(calculate.confidence.intervals) & !is.list(calculate.confidence.intervals)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply an appropriate state acronym, variable or list containing details to calculate.confidence.intervals. See help page for details. SGPs will be calculated without confidence intervals.\n")
			csem.tf <- FALSE
		}
		if (is.list(calculate.confidence.intervals)) {
			if (!(("state" %in% names(calculate.confidence.intervals)) | ("variable" %in% names(calculate.confidence.intervals)))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for calculate.confidence.intervals including state/csem variable, confidence.quantiles, simulation.iterations, distribution and round. See help page for details. SGPs will be calculated without confidence intervals.\n")
				csem.tf <- FALSE
			}
			if ("variable" %in% names(calculate.confidence.intervals) & missing(panel.data.vnames)) {
				stop("To utilize a supplied CSEM variable for confidence interval calculation you must specify the variables to be used for student growth percentile calculations with the panel.data.vnames argument. See help page for details.")
			}
			if (all(c("state", "variable") %in% names(calculate.confidence.intervals))) {
				stop("Please specify EITHER a state OR a CSEM variable for SGP confidence interval calculation. See help page for details.")
			}
		} 
		if (is.character(calculate.confidence.intervals)) {
			if (!calculate.confidence.intervals %in% c(ls(SGPstateData), names(panel.data))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without confidence intervals.\n")
			csem.tf <- FALSE
			}
			if (calculate.confidence.intervals %in% ls(SGPstateData)) {
				if ("YEAR" %in% names(SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\t\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. Simulated SGPs and confidence intervals will not be calculated.\n")
						csem.tf <- FALSE
					} 
				}
				if (!sgp.labels$my.subject %in% unique(SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]])) {
					tmp.messages <- c(tmp.messages, paste("\t\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '", sgp.labels$my.subject, "'. Simulated SGPs and confidence intervals will not be calculated.\n", sep=""))
					csem.tf <- FALSE
				}
				calculate.confidence.intervals <- list(state=calculate.confidence.intervals)
			}
			if (calculate.confidence.intervals %in% names(panel.data)) {
				calculate.confidence.intervals <- list(variable=calculate.confidence.intervals)
			}
		}
	} else {
		csem.tf <- FALSE
	}

	if (!is.null(calculate.simex)) {
		simex.tf <- TRUE
		if (!is.character(calculate.simex) & !is.list(calculate.simex)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply an appropriate state acronym, variable or list containing details to calculate.simex. See help page for details. SGPs will be calculated without measurement error correction.\n")
			simex.tf <- FALSE
		}
		if (is.list(calculate.simex)) {
			if (!(("state" %in% names(calculate.simex)) | ("variable" %in% names(calculate.simex)))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for calculate.simex including state/csem variable, simulation.iterations, lambda and extrapolation. See help page for details. SGPs will be calculated without measurement error correction.\n")
				simex.tf <- FALSE
			}
			if ("variable" %in% names(calculate.simex) & missing(panel.data.vnames)) {
				stop("To utilize a supplied CSEM variable for measurement error correction you must specify the variables to be used for student growth percentile calculations with the panel.data.vnames argument. See help page for details.")
			}
			if (all(c("state", "variable") %in% names(calculate.simex))) {
				stop("Please specify EITHER a state OR a CSEM variable for SGP measurement error correction. See help page for details.")
			}
			if (!is.null(calculate.simex$lambda)){
				if (!is.numeric(calculate.simex$lambda)) {
	        tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply numeric values to lambda. See help page for details. SGPs will be calculated without measurement error correction.\n")
	        simex.tf <- FALSE
				}
				if (any(calculate.simex$lambda < 0)) {
					warning("lambda should not contain negative values. Negative values will be ignored", call. = FALSE)
					lambda <- calculate.simex$lambda[calculate.simex$lambda >= 0]} else lambda=calculate.simex$lambda}
		}
		if (is.character(calculate.simex)) {
			if (!calculate.simex %in% c(names(SGPstateData), names(panel.data))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without measurement error correction.\n")
				simex.tf <- FALSE
			}
			if (calculate.simex %in% names(SGPstateData)) {
				if ("YEAR" %in% names(SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\t\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. SGPs will be calculated without measurement error correction.\n")
						simex.tf <- FALSE
					}
				}
				if (!sgp.labels$my.subject %in% unique(SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]])) {
					tmp.messages <- c(tmp.messages, paste("\t\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '", 
						sgp.labels$my.subject, "'. SGPs will be calculated without measurement error correction.\n", sep=""))
					simex.tf <- FALSE
				}
				calculate.simex <- list(state=calculate.simex)
			}
			if (calculate.simex %in% names(panel.data)) {
				calculate.simex <- list(variable=calculate.simex)
			}
		}
		if (is.null(calculate.simex$simulation.iterations)) calculate.simex$simulation.iterations <- 20
		if (is.null(calculate.simex$lambda)) calculate.simex$lambda <- seq(0,2,0.5)
		if (is.null(calculate.simex$extrapolation)) {
			calculate.simex$extrapolation <- "LINEAR"
		} else {
			calculate.simex$extrapolation <- toupper(calculate.simex$extrapolation)
		}
		if (!any(calculate.simex$extrapolation == c("QUADRATIC", "LINEAR", "NATURAL"))) {
			message("\t\tNOTE: Extrapolation not implemented. Using: linear", call. = FALSE)
			calculate.simex$extrapolation <- "LINEAR"
		}
		
	} else {
		simex.tf <- FALSE
	}

	if (!is.null(additional.vnames.to.return)) {
		if (!all(names(additional.vnames.to.return) %in% names(panel.data[["Panel_Data"]]))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'additional.vnames.to.return' are not all contained in supplied panel.data. No additional variables will be returned.\n")
			additional.vnames.to.return <- NULL
		}
	}


	### Create object to store the studentGrowthPercentiles objects

	tmp.objects <- c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "Panel_Data", "SGPercentiles", "SGProjections", "Simulated_SGPs") 
	Coefficient_Matrices <- Cutscores <- Goodness_of_Fit <- Knots_Boundaries <- Panel_Data <- SGPercentiles <- SGProjections <- Simulated_SGPs <- SGP_STANDARD_ERROR <- Verbose_Messages <- NULL
	SGP_SIMEX <- NULL

	if (identical(class(panel.data), "list")) {
		for (i in tmp.objects) {
			if (!is.null(panel.data[[i]])) {
				assign(i, panel.data[[i]])
			}
		}

		## Check class and construction of coefficient matrices

		if (!is.null(panel.data[['Coefficient_Matrices']])) {
			tmp.matrices <- Coefficient_Matrices; tmp.changes <- FALSE
			for (i in names(tmp.matrices)) {
				splineMatrix.tf <- sapply(tmp.matrices[[i]], validObject, test=TRUE)==TRUE
				if (!any(splineMatrix.tf)) {
					tmp.changes <- TRUE
					tmp.content_area <- unlist(strsplit(i, "[.]"))[1]; tmp.year <- unlist(strsplit(i, "[.]"))[2]
					for (j in names(tmp.matrices[[i]])[!splineMatrix.tf]) {
						message(paste("\t\tUpdating Existing Coefficient Matrix", i, j, "to new splineMatrix class."))
						tmp.matrices[[i]][[j]] <- 
							as.splineMatrix(matrix_argument=tmp.matrices[[i]][[j]], matrix_argument_name=j, content_area=tmp.content_area, year=tmp.year, sgp_object=panel.data)
					}
				}
			}
			if (tmp.changes) {
				Coefficient_Matrices <- tmp.matrices
			}
		}
	} ### if (identical(class(panel.data), "list"))


	### Create Panel_Data based upon class of input data

	if (is.matrix(panel.data)) {
		Panel_Data <- panel.data <- as.data.frame(panel.data, stringsAsFactors=FALSE)
	}
	if (identical(class(panel.data), "list")) {
        	if (!identical(class(panel.data[["Panel_Data"]]), "data.frame")) {
			Panel_Data <- as.data.frame(panel.data[["Panel_Data"]], stringsAsFactors=FALSE)
	}}
	if (identical(class(panel.data), "data.frame")) {
		Panel_Data <- panel.data
	}
	if (identical(class(panel.data), "list")) {
		Panel_Data <- panel.data[["Panel_Data"]]
	}
	
	### Create ss.data from Panel_Data

	if (!missing(panel.data.vnames)) {
		if (!all(panel.data.vnames %in% names(Panel_Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'panel.data.vnames' are not all in the supplied Panel_Data. Analyses will continue with the intersection names contain in Panel_Data.\n")
		}
		ss.data <- Panel_Data[,intersect(panel.data.vnames, names(Panel_Data))]
	} else {
		ss.data <- Panel_Data
	}
	if (dim(ss.data)[2] %% 2 != 1) {
		stop(paste("Number of columns of supplied panel data (", dim(ss.data)[2], ") does not conform to data requirements. See help page for details."))
	}

	num.panels <- (dim(ss.data)[2]-1)/2

	### Rename variables in ss.data based upon grade progression

	if (!missing(grade.progression)) {
		tmp.gp <- grade.progression
		by.grade <- TRUE

		if (length(tmp.gp[!is.na(tmp.gp)]) > num.panels) {
			tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Supplied grade progression, grade.progress=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.\n\t\t Analyses will utilize maximum number of priors supplied by the data.\n", sep=""))
		tmp.gp <- tail(grade.progression, num.panels)
	}}
	if (!missing(subset.grade) & missing(grade.progression)) {
		tmp.gp <- (subset.grade-num.panels+1):subset.grade
		by.grade <- TRUE
	}
	if (missing(subset.grade) & missing(grade.progression)) {
		tmp.gp <- 1:num.panels
		by.grade <- FALSE
	}
	if (!missing(num.prior) & !exact.grade.progression.sequence) {
		if (length(num.prior) > 1 | !((num.prior-round(num.prior)) < .Machine$double.eps^0.5) | num.prior <= 0) {
			stop("Specified num.prior not positive integer(s)")
		}
		if (num.prior > length(tmp.gp[!is.na(tmp.gp)])-1) {
			tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Specified argument num.prior (", num.prior, ") exceeds number of panels of data supplied. Analyses will utilize maximum number of priors possible.\n", sep=""))
			num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1
		} else {
			tmp.gp <- grade.progression <- tail(tmp.gp[!is.na(tmp.gp)], num.prior+1)
			if (!is.null(content.area.progression) && length(content.area.progression > num.prior+1)) content.area.progression <- tail(content.area.progression, num.prior+1)
			
	}} else {
		num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1
	}

	if (exact.grade.progression.sequence){
		tmp.gp <- grade.progression
		by.grade <- TRUE
		num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1
	}

	if (is.character(tmp.gp)) {
		tmp.slot.gp <- tmp.gp
		tmp.gp <- tmp.gp[!is.na(tmp.gp)]
	} else {
		tmp.slot.gp <- grade.progression
	}

	if (!is.null(max.order.for.percentile)) {
		tmp.gp <- tail(tmp.gp, max.order.for.percentile+1)
		num.prior <- min(num.prior, max.order.for.percentile)
		if (!is.null(content.area.progression)) content.area.progression <- tail(content.area.progression, length(tmp.gp))
		if (!is.null(year.progression)) year.progression <- year.progression.for.norm.group <- tail(year.progression, length(tmp.gp))
	}

	if (is.numeric(tmp.gp) & drop.nonsequential.grade.progression.variables && any(diff(tmp.gp) > 1)) {
		ss.data <- ss.data[,c(1, (num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1), (2*num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1))]
                num.panels <- (dim(ss.data)[2]-1)/2
	}

	### Create ss.data

	tmp.last <- tail(tmp.gp, 1)
	ss.data <- data.table(ss.data[,c(1, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels))], key=names(ss.data)[1])
        num.panels <- (dim(ss.data)[2]-1)/2
	if (is.factor(ss.data[[1]])) ss.data[[1]] <- as.character(ss.data[[1]])
	if (exact.grade.progression.sequence) tmp.num.prior <- num.prior else tmp.num.prior <- 1

        if (dim(.get.panel.data(ss.data, tmp.num.prior, by.grade))[1] == 0) {
                tmp.messages <- "\t\tNOTE: Supplied data together with grade progression contains no data. Check data, function arguments and see help page for details.\n"
                message(paste("\tStarted studentGrowthPercentiles", started.date))
                message(paste("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
                message(paste(tmp.messages, "\tFinished SGP Student Growth Percentile Analysis", date(), "in", timetaken(started.at), "\n"))

                return(
                list(Coefficient_Matrices=Coefficient_Matrices,
                        Cutscores=Cutscores,
                        Goodness_of_Fit=Goodness_of_Fit,
                        Knots_Boundaries=Knots_Boundaries,
                        Panel_Data=Panel_Data,
                        SGPercentiles=SGPercentiles,
                        SGProjections=SGProjections,
                        Simulated_SGPs=Simulated_SGPs))
        } 


	if (is.null(content.area.progression)) {
		content.area.progression <- rep(sgp.labels$my.subject, length(tmp.gp))
	} else {
		if (!identical(class(content.area.progression), "character")) {
			stop("content.area.progression should be a character vector. See help page for details.")
		}
		if (!identical(tail(content.area.progression, 1), sgp.labels[['my.subject']])) {
			stop("The last element in the content.area.progression must be identical to 'my.subject' of the sgp.labels. See help page for details.")
		}
		if (length(content.area.progression) != length(tmp.gp)) {
			tmp.messages <- c(tmp.messages, "\tNOTE: The content.area.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (is.null(year.progression) & is.null(year.progression.lags)) {
		if (is.character(type.convert(as.character(grade.progression), as.is=TRUE))) {
			stop("\tNOTE: Non-numeric grade progressions must be accompanied by arguments 'year.progression' and 'year.progression.lags'")
		} else {
			year.progression <- year.progression.for.norm.group <- rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(diff(type.convert(as.character(grade.progression))))))))
		}
	}

	if (is.null(year.progression) & !is.null(year.progression.lags)) {
		if (!identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- year.progression.for.norm.group <- rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year.progression.lags)))))
		}
		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- rep("BASELINE", length(tmp.gp))
			year.progression.for.norm.group <- rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year.progression.lags)))))
		}
		if (!identical(class(year.progression), "character")) {
			stop("year.area.progression should be a character vector. See help page for details.")
		}
		if (!identical(sgp.labels[['my.extra.label']], "BASELINE") & !identical(tail(year.progression, 1), sgp.labels[['my.year']])) {
			stop("The last element in the year.progression must be identical to 'my.year' of the sgp.labels. See help page for details.")
		}
		if (length(year.progression) != length(tmp.gp)) {
			tmp.messages <- c(tmp.messages, "\tNOTE: The year.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (!is.null(year.progression) & is.null(year.progression.lags)) {
		if (year.progression[1] == "BASELINE") {
			year.progression.lags <- rep(1, length(year.progression)-1)
			year.progression.for.norm.group <- year.progression
		} else {
			year.progression.lags <- diff(as.numeric(sapply(strsplit(year.progression, '_'), '[', split.location(year.progression))))
			year.progression.for.norm.group <- year.progression
		}
	}

	### Create Knots and Boundaries if requested (uses only grades in tmp.gp)

	if (missing(use.my.knots.boundaries)) {
		tmp.knots <- c(Knots_Boundaries[[tmp.path.knots.boundaries]], .get.knots.boundaries(ss.data, by.grade))
		Knots_Boundaries[[tmp.path.knots.boundaries]] <- tmp.knots[!duplicated(names(tmp.knots))]
	} else {
		if (is.character(use.my.knots.boundaries)) {
			if (!is.null(SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) {
				for (h in unique(content.area.progression)) {
					for (i in grep(h, names(SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
						Knots_Boundaries[[tmp.path.knots.boundaries]][[i]] <- SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]][[i]]
					}
				}
			}
		} 
	}

	### QR Calculations: coefficient matrices are saved/read into/from panel.data[["Coefficient_Matrices"]]

	if (is.null(use.my.coefficient.matrices)) {
		taus <- .create_taus(sgp.quantiles)
		if (exact.grade.progression.sequence) {
			coefficient.matrix.priors <- num.prior
		} else {
			coefficient.matrix.priors <- seq(num.prior)
		}
		for (k in coefficient.matrix.priors) {
			Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']] <- .create.coefficient.matrices(ss.data, k, by.grade)
			names(Coefficient_Matrices[[tmp.path.coefficient.matrices]])[length(Coefficient_Matrices[[tmp.path.coefficient.matrices]])] <- get.coefficient.matrix.name(tmp.last, k)

			if (verbose.output) {
				tmp.coefficient.matrix.name <- get.coefficient.matrix.name(tmp.last, k)
				tmp.grade.names <- paste("Grade", rev(head(unlist(Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Grade_Progression), -1)))
				for (l in seq_along(tmp.grade.names)) {
					tmp.knots <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Knots[l])
					tmp.boundaries <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Boundaries[l])
					Verbose_Messages <- c(Verbose_Messages, paste("\t\tNOTE: Coefficient Matrix ", tmp.coefficient.matrix.name, " created using Knots: ", tmp.knots, " and Boundaries: ", tmp.boundaries, ".\n", sep=""))
				}
			}
		}
	}

	### Calculate growth percentiles (if requested),  percentile cuts (if requested), and simulated confidence intervals (if requested)

	if (calculate.sgps) {
		max.order <- max(getsplineMatrix(
					Coefficient_Matrices[[tmp.path.coefficient.matrices]], 
					content.area.progression, 
					grade.progression, 
					year.progression,
					year.progression.lags, 
					what.to.return="ORDERS"))

		if (max.order < num.prior) {
			tmp.messages <- c(tmp.messages, paste("\tNOTE: Requested number of prior scores (num.prior=", num.prior, ") exceeds maximum matrix order (max.order=", max.order, "). 
				Only matrices of order up to max.order=", max.order, " will be used.\n", sep=""))
		}
		if (max.order > num.prior) {
			tmp.messages <- c(tmp.messages, paste("\tNOTE: Maximum coefficient matrix order (max.order=", max.order, ") exceeds that of specified number of priors, 
				(num.prior=", num.prior, "). Only matrices of order up to num.prior=", num.prior, " will be used.\n", sep=""))
			max.order <- num.prior
		}
		if (exact.grade.progression.sequence) {
			orders <- max.order
		} else {
			orders <- seq(max.order)
		}

		tmp.quantiles <- tmp.percentile.cuts <- tmp.csem.quantiles <- list()

		for (j in orders) {
			tmp.data <- .get.panel.data(ss.data, j, by.grade)
			if (dim(tmp.data)[1] > 0) {

				tmp.matrix <- getsplineMatrix(
					Coefficient_Matrices[[tmp.path.coefficient.matrices]], 
					tail(content.area.progression, j+1), 
					tail(grade.progression, j+1),
					tail(year.progression, j+1),
					tail(year.progression.lags, j),
					my.matrix.order=j)

				tmp.predictions <- .get.percentile.predictions(tmp.data, tmp.matrix)
				tmp.quantiles[[j]] <- data.table(ID=tmp.data[[1]], ORDER=j, SGP=.get.quantiles(tmp.predictions, tmp.data[[dim(tmp.data)[2]]]))
				if (csem.tf) {
					if (is.null(calculate.confidence.intervals$simulation.iterations)) calculate.confidence.intervals[['simulation.iterations']] <- 100
					if (!is.null(calculate.confidence.intervals$variable)) {
						if (missing(panel.data.vnames)) {
							tmp.csem.variable <- Panel_Data[Panel_Data[,1] %in% 
								ss.data[tmp.data[[1]]][[1]], calculate.confidence.intervals$variable] 
						} else {
							tmp.csem.variable <- Panel_Data[Panel_Data[,panel.data.vnames[1]] %in% 
								ss.data[tmp.data[[1]]][[1]], calculate.confidence.intervals$variable] 
						}
					} else {
						tmp.csem.variable <- NULL
					}

					tmp.csem.quantiles[[j]] <- data.table(ID=tmp.data[[1]])
					for (k in seq(calculate.confidence.intervals[['simulation.iterations']])) { 
						set.seed(k)
						tmp.csem.quantiles[[j]][,TEMP_SGP_SIM:=.get.quantiles(
								tmp.predictions, 
								csemScoreSimulator(
									scale_scores=tmp.data[[dim(tmp.data)[2]]],
									grade=tmp.last,
									content_area=sgp.labels$my.subject,
									year=sgp.labels$my.year,
									state=calculate.confidence.intervals$state,
									variable=tmp.csem.variable,
									distribution=calculate.confidence.intervals$distribution,
									round=calculate.confidence.intervals$round))]
						setnames(tmp.csem.quantiles[[j]], "TEMP_SGP_SIM", paste("SGP_SIM", k, sep="_"))
					} ## END k loop
				} ## END CSEM analysis

				if (!is.null(percentile.cuts)) {
					tmp.percentile.cuts[[j]] <- data.table(ID=tmp.data[[1]], .get.percentile.cuts(tmp.predictions))
				}
				if ((is.character(goodness.of.fit) | goodness.of.fit==TRUE | return.prior.scale.score) & j==1) prior.ss <- tmp.data[[dim(tmp.data)[2]-1]]
				if (exact.grade.progression.sequence & return.prior.scale.score) prior.ss <- tmp.data[[dim(tmp.data)[2]-1]]
			} ### END if (dim(tmp.data)[1] > 0)
		} ## END j loop

		quantile.data <- data.table(rbindlist(tmp.quantiles), key="ID")

		if (print.other.gp) {
			quantile.data <- data.table(reshape(quantile.data, idvar="ID", timevar="ORDER", direction="wide", sep="_ORDER_"),
				SGP=quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data))][["SGP"]],
				ORDER=as.integer(quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data))][["ORDER"]]))
		} else {
			if (print.sgp.order | return.norm.group.identifier) {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data))]
			} else {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data)), c("ID", "SGP"), with=FALSE]
			}
		}

		quantile.data[,SCALE_SCORE_PRIOR:=prior.ss]

		if (return.prior.scale.score.standardized) {
			SCALE_SCORE_PRIOR_STANDARDIZED <- NULL
			quantile.data[,SCALE_SCORE_PRIOR_STANDARDIZED:=round(as.numeric(scale(prior.ss)), digits=3)]
		}

		if (tf.growth.levels) {
			SGP_LEVEL <- NULL
			quantile.data[, SGP_LEVEL:=factor(findInterval(quantile.data[["SGP"]], tmp.growth.levels[["my.cuts"]]), 
				levels=seq(length(tmp.growth.levels[["my.levels"]]))-1, ## Necessary in case the full range of SGPs isn't present
				labels=tmp.growth.levels[["my.levels"]], ordered=TRUE)]
		}

		if (csem.tf) {
			simulation.data <- data.table(rbindlist(tmp.csem.quantiles), key="ID")
			simulation.data <- simulation.data[c(which(!duplicated(simulation.data))[-1]-1, nrow(simulation.data))]

			if (is.character(calculate.confidence.intervals) | is.list(calculate.confidence.intervals)) {
				if (is.null(calculate.confidence.intervals$confidence.quantiles) | identical(toupper(calculate.confidence.intervals$confidence.quantiles), "STANDARD_ERROR")) {
					quantile.data[,SGP_STANDARD_ERROR:=round(sqrt(2*apply(simulation.data[, -1, with=FALSE], 1, sd, na.rm=TRUE)), digits=2)]
				} else {
					if (!(is.numeric(calculate.confidence.intervals$confidence.quantiles) & all(calculate.confidence.intervals$confidence.quantiles < 1) & 
						all(calculate.confidence.intervals$confidence.quantiles > 0)))  {
						stop("Argument to 'calculate.confidence.intervals$confidence.quantiles' must be numeric and consist of quantiles.")
					}
					tmp.cq <- data.table(round(t(apply(simulation.data[, -1, with=FALSE], 1, quantile, probs = calculate.confidence.intervals$confidence.quantiles))))
					quantile.data[,paste("SGP_", calculate.confidence.intervals$confidence.quantiles, "_CONFIDENCE_BOUND", sep=""):=tmp.cq]
				}
			}
			if (!is.null(calculate.confidence.intervals$confidence.quantiles)) {
				tmp.cq <- data.table(round(t(apply(simulation.data[, -1, with=FALSE], 1, quantile, probs = calculate.confidence.intervals$confidence.quantiles))))
				quantile.data[,paste("SGP_", calculate.confidence.intervals$confidence.quantiles, "_CONFIDENCE_BOUND", sep=""):=tmp.cq]
			}
			Simulated_SGPs[[tmp.path]] <- rbind.fill(simulation.data, as.data.frame(Simulated_SGPs[[tmp.path]])) 
		}

		if (simex.tf){
			quantile.data.simex<-.simex.sgp(state=calculate.simex$state, variable=calculate.simex$variable, lambda=calculate.simex$lambda, 
				B=calculate.simex$simulation.iterations, extrapolation=calculate.simex$extrapolation)
			quantile.data[, SGP_SIMEX:=quantile.data.simex[["SGP_SIMEX"]]]
		}
		
		if (!is.null(percentile.cuts)){
			cuts.best <- data.table(rbindlist(tmp.percentile.cuts), key="ID")
			cuts.best <- cuts.best[c(which(!duplicated(cuts.best))[-1]-1, nrow(cuts.best))][,-1, with=FALSE]
			quantile.data <- data.table(quantile.data, cuts.best)
		}

		if (print.sgp.order | return.norm.group.identifier) {
			if (exact.grade.progression.sequence) {
				norm.groups <- paste(paste(year.progression.for.norm.group, paste(content.area.progression, grade.progression, sep="_"), sep="/"), collapse="; ")
			} else {
				norm.groups <- sapply(seq_along(year.progression.for.norm.group)[-1][1:(num.panels-1)], 
				 function(x) paste(tail(paste(year.progression.for.norm.group, paste(content.area.progression, grade.progression, sep="_"), sep="/"), x), collapse="; "))
			}
			norm.var.name <- paste(c("SGP_NORM_GROUP", sgp.labels[['my.extra.label']]), collapse="_")
			sgp.order.name <- paste(c("SGP", sgp.labels[['my.extra.label']], "ORDER"), collapse="_")
			if (!print.sgp.order) { # Return only SGP_NORM_GROUP
				if (exact.grade.progression.sequence) {
					quantile.data[, norm.var.name:=factor(factor(ORDER, labels=norm.groups)), with=FALSE]
				} else {
					quantile.data[, norm.var.name:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups)), with=FALSE]
				}
				quantile.data[, ORDER:=NULL]
			} else {  # Return both ORDER and SGP_NORM_GROUP
				if (exact.grade.progression.sequence) {
					quantile.data[, norm.var.name:=factor(factor(ORDER, labels=norm.groups)), with=FALSE]
				} else {
					quantile.data[, norm.var.name:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups)), with=FALSE]
				}
				setnames(quantile.data, "ORDER", sgp.order.name)
			}
		}

		if (dim(quantile.data)[1] <= 250) {
			message("\tNOTE: Due to small number of cases (", dim(quantile.data)[1], ") no goodness of fit plots produced.")
			goodness.of.fit <- FALSE
		}

		if (is.character(goodness.of.fit) | goodness.of.fit==TRUE) {
			if (is.character(goodness.of.fit) & goodness.of.fit %in% ls(SGPstateData)) {
				GRADE <- YEAR <- CONTENT_AREA <- NULL
				tmp.gof.data <- getAchievementLevel(
							sgp_data=data.table(
								SCALE_SCORE=quantile.data[['SCALE_SCORE_PRIOR']],
								SGP=quantile.data[['SGP']],
								VALID_CASE="VALID_CASE",
								CONTENT_AREA=rev(content.area.progression)[2],
								YEAR=rev(year.progression)[2], 
								GRADE=rev(tmp.gp)[2]),
							state=goodness.of.fit,
							year=rev(year.progression)[2],
							content_area=rev(content.area.progression)[2],
							grade=tail(tmp.gp, 2)[1])[,GRADE:=tmp.last][,YEAR:=sgp.labels[['my.year']]]
				setnames(tmp.gof.data, c("SCALE_SCORE", "ACHIEVEMENT_LEVEL", "CONTENT_AREA"), c("SCALE_SCORE_PRIOR", "ACHIEVEMENT_LEVEL_PRIOR", "CONTENT_AREA_PRIOR"))
				tmp.gof.data[["CONTENT_AREA"]] <- sgp.labels[['my.subject']]
				content_areas_prior <- tmp.gof.data[['CONTENT_AREA_PRIOR']][1]

				Goodness_of_Fit[[tmp.path]][['TMP_NAME']] <- gofSGP(
										sgp_object=tmp.gof.data,
										state=goodness.of.fit,
										years=sgp.labels[['my.year']],
										content_areas=sgp.labels[['my.subject']],
										content_areas_prior=content_areas_prior,
										grades=tmp.last,
										output.format="GROB")
			} else {
				tmp.gof.data <- data.table(
							SCALE_SCORE_PRIOR=quantile.data[['SCALE_SCORE_PRIOR']],
							SGP=quantile.data[['SGP']],
							VALID_CASE="VALID_CASE", 
							CONTENT_AREA=sgp.labels[['my.subject']], 
							YEAR=sgp.labels[['my.year']], 
							GRADE=tmp.last)
 
				Goodness_of_Fit[[tmp.path]][['TMP_NAME']] <- gofSGP(
										sgp_object=tmp.gof.data,
										years=sgp.labels[['my.year']],
										content_areas=sgp.labels[['my.subject']],
										grades=tmp.last,
										output.format="GROB")
			}
			if (!all(sapply(2:length(content.area.progression), function(f) identical(content.area.progression[f-1], content.area.progression[f])))) {
				if (tail(grade.progression, 1) == "EOCT") {
					tmp.grade.name <- "EOCT" 
				} else {
					tmp.grade.name <- paste("GRADE_", tail(grade.progression, 1), sep="")
				}
				tmp.name <- paste(paste(sapply(strsplit(head(year.progression, -1), '_'), '[', split.location(year.progression)), 
					paste(head(content.area.progression, -1), head(grade.progression, -1), sep=" "), sep=" "), collapse=", ")
				tmp.name <- gsub("MATHEMATICS", "MATH", tmp.name)
				names(Goodness_of_Fit[[tmp.path]])[length(Goodness_of_Fit[[tmp.path]])] <- paste(tmp.grade.name, " (Priors- ", tmp.name, ")", sep="")
			} else {
				names(Goodness_of_Fit[[tmp.path]])[length(Goodness_of_Fit[[tmp.path]])] <- paste("GRADE_", 
					paste(paste(tmp.gp, year.progression, sep="_"), collapse="-"), sep="")
			}
		}

		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) setnames(quantile.data, "SGP", "SGP_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & "SGP_STANDARD_ERROR" %in% names(quantile.data)) setnames(quantile.data, "SGP_STANDARD_ERROR", "SGP_BASELINE_STANDARD_ERROR")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & print.other.gp) {
			my.tmp.names <- grep("SGP_ORDER", names(quantile.data), value=TRUE)
			setnames(quantile.data, my.tmp.names, gsub("SGP_ORDER", "SGP_BASELINE_ORDER", my.tmp.names))
		}

		if (!is.null(additional.vnames.to.return)) {
			quantile.data <- data.table(panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return))], key="ID")[quantile.data]
			setnames(quantile.data, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
		}

		if (!return.prior.scale.score) {
			quantile.data[,SCALE_SCORE_PRIOR:=NULL]
		}

		SGPercentiles[[tmp.path]] <- rbind.fill(quantile.data, as.data.frame(SGPercentiles[[tmp.path]]))

	} ## End if calculate.sgps


	### Start/Finish Message & Return SGP Object

	if (print.time.taken) {
		message(paste("\tStarted studentGrowthPercentiles:", started.date))
		if (calculate.sgps) {
			message(paste("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, " (N=", format(dim(quantile.data)[1], big.mark=","), ")", sep=""))
		} else {
			message(paste("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
		}
		if (verbose.output) message(Verbose_Messages)
		message(c(tmp.messages, "\tFinished SGP Student Growth Percentile Analysis: ", date(), " in ", timetaken(started.at), "\n")) 
	}

	list(Coefficient_Matrices=Coefficient_Matrices,
		Cutscores=Cutscores, 
		Goodness_of_Fit=Goodness_of_Fit, 
		Knots_Boundaries=Knots_Boundaries,
		Panel_Data=Panel_Data, 
		SGPercentiles=SGPercentiles,
		SGProjections=SGProjections,
		Simulated_SGPs=Simulated_SGPs)

} ### END studentGrowthPercentiles Function
