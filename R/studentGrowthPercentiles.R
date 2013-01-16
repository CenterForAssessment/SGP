`studentGrowthPercentiles` <-
function(panel.data,         ## REQUIRED
         sgp.labels,         ## REQUIRED
         panel.data.vnames,
         grade.progression,
         content.area.progression,
         year.progression,
         year.progression.lags,
         num.prior,
         max.order.for.percentile=NULL,
         subset.grade,
         percentile.cuts=NULL,
         growth.levels, 
         use.my.knots.boundaries,
         use.my.coefficient.matrices=NULL,
         calculate.confidence.intervals,
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
         return.norm.group.identifier=TRUE,
         print.time.taken=TRUE,
         parallel.config=NULL) {

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
			tmp.years <- rep(.year.increment(sgp.labels$my.year, (-num.panels+1):-1), each=dim(data)[1])
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

        .year.increment <- function(year, increment) {
		if (identical(year, "BASELINE")) {
			return(rep("BASELINE", length(increment)))
		} else {
			sapply(increment, function(x) paste(as.numeric(unlist(strsplit(as.character(year), "_")))+x, collapse="_"))
		}
        }

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.knots.boundaries.names <- names(Knots_Boundaries[[tmp.path.knots.boundaries]])[match(content_area, names(Knots_Boundaries[[tmp.path.knots.boundaries]]))]
		if (is.na(tmp.knots.boundaries.names)) {
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
			my.path.knots.boundaries <- get.my.knots.boundaries.path(rev(content.area.progression)[i+1], .year.increment(rev(year.progression)[i+1], 0))
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

	.csem.score.simulator <- function(scale_scores, grade, content_area, year, state, variable, distribution, round) {
		GRADE <- CONTENT_AREA <- YEAR <- NULL ## To avoid R CMD check warnings
		if (is.null(round)) round <- 1
		if (is.null(distribution)) distribution <- "Normal"
		if (!is.null(state)) min.max <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", grade, sep="")]]
		if (!is.null(variable)) min.max <- range(scale_scores, na.rm=TRUE)
		if (!is.null(state)) {
			if ("YEAR" %in% names(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
				CSEM_Data <- subset(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==grade & CONTENT_AREA==content_area & YEAR==year)
			} else {
				CSEM_Data <- subset(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==grade & CONTENT_AREA==content_area)
			}
			CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
			tmp.scale <- CSEM_Function(scale_scores)
		} 
		if (!is.null(variable)) {
			tmp.scale <- variable
		}
			if(distribution=="Skew-Normal") {
				tmp.shape <- tan((pi/2)*((min.max[1]+min.max[2]) - 2*scale_scores)/(min.max[2]-min.max[1]))
				tmp.score <- round_any(as.numeric(rsn(length(scale_scores), location=scale_scores, scale=tmp.scale, shape=tmp.shape)), round)
			}
			if(distribution=="Normal") {
				tmp.score <- round_any(as.numeric(rnorm(length(scale_scores), mean=scale_scores, sd=tmp.scale)), round)
			}
			tmp.score[tmp.score < min.max[1]] <- min.max[1]
			tmp.score[tmp.score > min.max[2]] <- min.max[2]
			return(tmp.score)
	}

	split.location <- function(years) sapply(strsplit(years, '_'), length)[1]

	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	ID <- tmp.messages <- ORDER <- TEMP_SGP_SIM <- NULL

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
			tmp.messages <- c(tmp.messages, "\tNOTE: growth.levels must be supplied as a list or character abbreviation. See help page for details. studentGrowthPercentiles will be calculated without augmented growth.levels\n")
			tf.growth.levels <- FALSE
		}
		if (is.list(growth.levels)) {
			if (!identical(names(growth.levels), c("my.cuts", "my.levels"))) {
				tmp.messages <- c(tmp.messages, "\tNOTE: Please specify an appropriate list for growth.levels. See help page for details. Student growth percentiles will be calculated without augmented growth.levels\n")
				tf.growth.levels <- FALSE
			} else {
				tmp.growth.levels <- growth.levels
				tf.growth.levels <- TRUE
			} 
		}
		if (is.character(growth.levels)) {
			if (is.null(SGPstateData[[growth.levels]][["Growth"]][["Levels"]])) {
				tmp.messages <- c(tmp.messages, "\tNOTE: Growth Levels are currently not specified for the indicated state. \n\tPlease contact the SGP package administrator to have your state's data included in the package. Student growth percentiles will be calculated without augmented growth levels\n")
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
				tmp.messages <- c(tmp.messages, paste("\tNOTE: Knots and Boundaries are currently not implemented for the state indicated (", use.my.knots.boundaries, "). Knots and boundaries will be calculated from the data. Please contact the SGP package administrator to have your Knots and Boundaries included in the package\n", sep=""))
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
			stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		if (!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject")) & 
			!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject", "my.extra.label"))) {
				stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices)
		if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
			stop("Coefficient matrices indicated by use.my.coefficient.matrices are not included.")
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
		tmp.messages <- c(tmp.messages, "\tNOTE: Goodness-of-Fit tables only produced when calculating SGPs.\n")
	}
	if (!missing(calculate.confidence.intervals)) {
		csem.tf <- TRUE
		if (!is.character(calculate.confidence.intervals) & !is.list(calculate.confidence.intervals)) {
			tmp.messages <- c(tmp.messages, "\tNOTE: Please supply an appropriate state acronym, variable or list containing details to calculate.confidence.intervals. See help page for details. SGPs will be calculated without confidence intervals.\n")
			csem.tf <- FALSE
		}
		if (is.list(calculate.confidence.intervals)) {
			if (!(("state" %in% names(calculate.confidence.intervals)) | ("variable" %in% names(calculate.confidence.intervals)))) {
				tmp.messages <- c(tmp.messages, "\tNOTE: Please specify an appropriate list for calculate.confidence.intervals including state/csem variable, confidence.quantiles, simulation.iterations, distribution and round. See help page for details. SGPs will be calculated without confidence intervals.\n")
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
				tmp.messages <- c(tmp.messages, "\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without confidence intervals.\n")
			csem.tf <- FALSE
			}
			if (calculate.confidence.intervals %in% ls(SGPstateData)) {
				if ("YEAR" %in% names(SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. Simulated SGPs and confidence intervals will not be calculated.\n")
						csem.tf <- FALSE
					} 
				}
				if (!sgp.labels$my.subject %in% unique(SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]])) {
					tmp.messages <- c(tmp.messages, paste("\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '", sgp.labels$my.subject, "'. Simulated SGPs and confidence intervals will not be calculated.\n", sep=""))
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

	### Create object to store the studentGrowthPercentiles objects

	tmp.objects <- c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "Panel_Data", "SGPercentiles", "SGProjections", "Simulated_SGPs") 
	Coefficient_Matrices <- Cutscores <- Goodness_of_Fit <- Knots_Boundaries <- Panel_Data <- SGPercentiles <- SGProjections <- Simulated_SGPs <- SGP_STANDARD_ERROR <- NULL

	if (identical(class(panel.data), "list")) {
		for (i in tmp.objects) {
			if (!is.null(panel.data[[i]])) {
				assign(i, panel.data[[i]])
			}
		}
		## Check class and construction of coefficient matrices

		if (!is.null(Coefficient_Matrices)) {
			tmp.matrices <- Coefficient_Matrices; tmp.changes <- FALSE
			for (i in names(tmp.matrices)) {
				splineMatrix.tf <- sapply(tmp.matrices[[i]], validObject, test=TRUE)==TRUE
				if (!any(splineMatrix.tf)) {
					tmp.changes <- TRUE
					for (j in names(tmp.matrices[[i]])[!splineMatrix.tf]) {
						message(paste("\tUpdating Existing Coefficient Matrix", i, j, "to new splineMatrix class."))
						tmp.matrices[[i]][[j]] <- as.splineMatrix(matrix_argument=tmp.matrices[[i]][[j]], matrix_argument_name=j, sgp_object=panel.data)
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
			tmp.messages <- c(tmp.messages, "\tNOTE: Supplied 'panel.data.vnames' are not all in the supplied Panel_Data. Analyses will continue with the intersection names contain in Panel_Data.\n")
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
			tmp.messages <- c(tmp.messages, paste("\tNOTE: Supplied grade progression, grade.progress=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.\n\t\t Analyses will utilize maximum number of priors supplied by the data.\n", sep=""))
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
	if (!missing(num.prior)) {
		if (length(num.prior) > 1 | !((num.prior-round(num.prior)) < .Machine$double.eps^0.5) | num.prior <= 0) {
			stop("Specified num.prior not positive integer(s)")
		}
		if (num.prior > length(tmp.gp[!is.na(tmp.gp)])-1) {
			tmp.messages <- c(tmp.messages, paste("\tNOTE: Specified argument num.prior (", num.prior, ") exceeds number of panels of data supplied. Analyses will utilize maximum number of priors possible.\n", sep=""))
			num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1
		} else {
			tmp.gp <- tail(tmp.gp[!is.na(tmp.gp)], num.prior+1)
			
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
	}	else tmp.slot.gp <- grade.progression

	if (!is.null(max.order.for.percentile)) {
		tmp.gp <- tail(tmp.gp, max.order.for.percentile+1)
		num.prior <- min(num.prior, max.order.for.percentile)
		if (!missing(content.area.progression)) content.area.progression <- tail(content.area.progression, length(tmp.gp))
		if (!missing(year.progression)) year.progression <- tail(year.progression, length(tmp.gp))
	}

	if (is.numeric(tmp.gp) & drop.nonsequential.grade.progression.variables && any(diff(tmp.gp) > 1)) {
		ss.data <- ss.data[,c(1, (num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1), (2*num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1))]
                num.panels <- (dim(ss.data)[2]-1)/2
	}

	### Create ss.data

	tmp.last <- tail(tmp.gp, 1)
	ss.data <- data.table(ss.data[,c(1, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels))], key=names(ss.data)[1])
        num.panels <- (dim(ss.data)[2]-1)/2

        if (dim(.get.panel.data(ss.data, 1, by.grade))[1] == 0) {
                tmp.messages <- "\tNOTE: Supplied data together with grade progression contains no data. Check data, function arguments and see help page for details.\n"
                message(paste("\tStarted studentGrowthPercentiles", started.date))
                message(paste("\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
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


	if (missing(content.area.progression)) {
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

	if (missing(year.progression) & !identical(sgp.labels[['my.extra.label']], "BASELINE")) {
		year.progression <- .year.increment(sgp.labels[['my.year']], rev(seq(0, length=length(tmp.gp), by=-1)))
	} else {
		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- rep("BASELINE", length(tmp.gp))
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

	if (missing(year.progression.lags)) {
		if (year.progression[1] == "BASELINE") {
			year.progression.lags <- rep(1, length(year.progression)-1)
		} else {
			year.progression.lags <- diff(as.numeric(sapply(strsplit(year.progression, '_'), '[', split.location(year.progression))))
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
				tmp.quantiles[[j]] <- data.table(ID=tmp.data[["ID"]], ORDER=j, SGP=.get.quantiles(tmp.predictions, tmp.data[[dim(tmp.data)[2]]]))
				if (csem.tf) {
					if (is.null(calculate.confidence.intervals$simulation.iterations)) calculate.confidence.intervals[['simulation.iterations']] <- 100
					if (!is.null(calculate.confidence.intervals$variable)) {
						if (missing(panel.data.vnames)) {
							tmp.csem.variable <- Panel_Data[Panel_Data[,1] %in% 
								ss.data[tmp.data[['ID']]][["ID"]],calculate.confidence.intervals$variable] 
						} else {
							tmp.csem.variable <- Panel_Data[Panel_Data[,panel.data.vnames[1]] %in% 
								ss.data[tmp.data[['ID']]][["ID"]],calculate.confidence.intervals$variable] 
						}
					} else {
						tmp.csem.variable <- NULL
					}

					tmp.csem.quantiles[[j]] <- data.table(ID=tmp.data[['ID']])
					for (k in seq(calculate.confidence.intervals[['simulation.iterations']])) { 
						set.seed(k)
						tmp.csem.quantiles[[j]][,TEMP_SGP_SIM:=.get.quantiles(
								tmp.predictions, 
								.csem.score.simulator(
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
					tmp.percentile.cuts[[j]] <- data.table(ID=tmp.data[["ID"]], .get.percentile.cuts(tmp.predictions))
				}
				if ((is.character(goodness.of.fit) | goodness.of.fit==TRUE | return.prior.scale.score) & j==1) prior.ss <- tmp.data[[dim(tmp.data)[2]-1]]
				if (exact.grade.progression.sequence & return.prior.scale.score) prior.ss <- tmp.data[[dim(tmp.data)[2]-1]]
			} ### END if (dim(tmp.data)[1] > 0)
		} ## END j loop

		quantile.data <- data.table(rbindlist(tmp.quantiles), key="ID")

		if (print.other.gp) {
			quantile.data <- data.table(reshape(quantile.data, idvar="ID", timevar="ORDER", direction="wide"),
				SGP=quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data))][["SGP"]])
		} else {
			if (print.sgp.order | return.norm.group.identifier) {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data))]
			} else {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data))[-1]-1L, nrow(quantile.data)), c("ID", "SGP"), with=FALSE]
			}
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
					quantile.data[,SGP_STANDARD_ERROR:=round(apply(simulation.data[, -1, with=FALSE], 1, sd, na.rm=TRUE))]
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

		if (!is.null(percentile.cuts)){
			cuts.best <- data.table(rbindlist(tmp.percentile.cuts), key="ID")
			cuts.best <- cuts.best[c(which(!duplicated(cuts.best))[-1]-1, nrow(cuts.best))][,-1, with=FALSE]
			quantile.data <- cbind(quantile.data, cuts.best)
		}

		if (return.prior.scale.score) {
			SCALE_SCORE_PRIOR <- NULL
			quantile.data[,SCALE_SCORE_PRIOR:=prior.ss]
		}

		if (print.sgp.order | return.norm.group.identifier) {
			if (exact.grade.progression.sequence) {
				norm.groups <- paste(paste(year.progression, paste(content.area.progression, grade.progression, sep="_"), sep="/"), collapse="; ")
			} else {
				norm.groups <- sapply(seq_along(year.progression)[-1][1:(num.panels-1)], 
				 function(x) paste(tail(paste(year.progression, paste(content.area.progression, grade.progression, sep="_"), sep="/"), x), collapse="; "))
			}
			norm.var.name <- paste(c("SGP_NORM_GROUP", sgp.labels[['my.extra.label']]), collapse="_")
			sgp.order.name <- paste(c("SGP", sgp.labels[['my.extra.label']], "ORDER"), collapse="_")
			if (!print.sgp.order) { # Return only SGP_NORM_GROUP
				quantile.data[, norm.var.name:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups)), with=FALSE] ### double factor to wipe unused levels
				quantile.data[, ORDER:=NULL]
			} else {  # Return both ORDER and SGP_NORM_GROUP
				quantile.data[, norm.var.name:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups)), with=FALSE]
				setnames(quantile.data, "ORDER", sgp.order.name)
			}
		}

		if (dim(quantile.data)[1] <= 250) {
			message("\tNOTE: Due to small number of cases (", dim(quantile.data)[1], ") no goodness of fit plots produced.")
			goodness.of.fit <- FALSE
		}

		if (is.character(goodness.of.fit) | goodness.of.fit==TRUE) {
			if (is.character(goodness.of.fit) & goodness.of.fit %in% ls(SGPstateData)) {
				tmp.gof.data <- getAchievementLevel(
							sgp_data=data.table(
								SCALE_SCORE=quantile.data[['SCALE_SCORE_PRIOR']],
								SGP=quantile.data[['SGP']],
								VALID_CASE="VALID_CASE", 
								CONTENT_AREA=sgp.labels[['my.subject']], 
								YEAR=sgp.labels[['my.year']], 
								GRADE=tmp.last),
							state=goodness.of.fit,
							year=sgp.labels[['my.year']],
							content_area=sgp.labels[['my.subject']],
							grade=tmp.last)
				setnames(tmp.gof.data, c("SCALE_SCORE", "ACHIEVEMENT_LEVEL"), c("SCALE_SCORE_PRIOR", "ACHIEVEMENT_LEVEL_PRIOR"))

				Goodness_of_Fit[[tmp.path]][['TMP_NAME']] <- gofSGP(
										sgp_object=tmp.gof.data,
										state=goodness.of.fit,
										years=sgp.labels[['my.year']],
										content_areas=sgp.labels[['my.subject']],
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
			names(Goodness_of_Fit[[tmp.path]])[length(Goodness_of_Fit[[tmp.path]])] <- paste("GRADE_", paste(tmp.gp, collapse="-"), sep="")
		}

		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) setnames(quantile.data, "SGP", "SGP_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_BASELINE")
		SGPercentiles[[tmp.path]] <- rbind.fill(quantile.data, as.data.frame(SGPercentiles[[tmp.path]]))

	} ## End if calculate.sgps

	### Start/Finish Message & Return SGP Object

	if (print.time.taken) {
		message(paste("\tStarted studentGrowthPercentiles:", started.date))
		message(paste("\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
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

} ## END studentGrowthPercentiles Function
