`studentGrowthPercentiles` <-
function(panel.data,         ## REQUIRED
         sgp.labels,         ## REQUIRED
         panel.data.vnames=NULL,
         additional.vnames.to.return=NULL,
         grade.progression,
         content_area.progression=NULL,
         year.progression=NULL,
         year_lags.progression=NULL,
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
         max.n.for.coefficient.matrices=NULL,
         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),
         knots.boundaries.by.panel=FALSE,
         exact.grade.progression.sequence=FALSE,
         drop.nonsequential.grade.progression.variables=TRUE,
         convert.0and100=TRUE,
         sgp.quantiles="Percentiles",
         sgp.quantiles.labels=NULL,
         sgp.loss.hoss.adjustment=NULL,
         sgp.cohort.size=NULL,
         percuts.digits=0,
         isotonize=TRUE,
         convert.using.loss.hoss=TRUE,
         goodness.of.fit=TRUE,
         goodness.of.fit.minimum.n=NULL,
         return.prior.scale.score=TRUE,
         return.prior.scale.score.standardized=TRUE,
         return.norm.group.identifier=TRUE,
         return.norm.group.scale.scores=NULL,
         return.panel.data=identical(parent.frame(), .GlobalEnv),
         print.time.taken=TRUE,
         parallel.config=NULL,
         calculate.simex=NULL,
         sgp.percentiles.set.seed=314159,
         sgp.percentiles.equated=NULL,
         sgp.time=NULL,
         verbose.output=FALSE) {

	started.at <- proc.time()
	started.date <- date()

	##########################################################
	###
	### Internal utility functions
	###
	##########################################################

	.smooth.isotonize.row <- function(x, iso=isotonize, sgp.loss.hoss.adjustment) {
		if (!is.null(sgp.loss.hoss.adjustment)) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
			bnd <- eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']]", sep="")))
			x[x > bnd[2]] <- bnd[2]
		}
		x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
		if (iso) return(sort(x))
		else return(x)
	}

	.smooth.bound.iso.row <- function(x, grade, tmp.year, tmp.content_area, iso=isotonize, missing.taus, na.replace) {
		bnd <- eval(parse(text=paste("panel.data[['Knots_Boundaries']]", get.my.knots.boundaries.path(tmp.content_area, tmp.year), "[['loss.hoss_", grade, "']]", sep="")))
		x[x < bnd[1]] <- bnd[1] ; x[x > bnd[2]] <- bnd[2]
		if (!iso) return(round(x, digits=5)) # Results are the same whether NAs present or not...
		if (iso & missing.taus) {
			na.row <- rep(NA,100)
			na.row[na.replace] <- round(sort(x[!is.na(x)]), digits=5)
			return(na.row)
		} else {
			x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
			return(round(sort(x), digits=5))
		}
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
			CONTENT_AREA=rep(head(content_area.progression, -1), each=dim(data)[1]),
			GRADE=tmp.grades, 
			SCALE_SCORE=as.vector(sapply(data[,(2+num.panels):(2+2*num.panels-2), with=FALSE], as.numeric)),
			YEAR=tmp.years, key=c("VALID_CASE", "CONTENT_AREA", "YEAR")) 

		createKnotsBoundaries(tmp.stack, knot.cut.percentiles)
	}

	.get.panel.data <- function(tmp.data, k, by.grade) {
		str1 <- str2 <- str3 <- NULL
		for (i in 0:k) {
			str1 <- paste(str1, " & !is.na(tmp.data[[", 1+2*num.panels-i, "]])", sep="")
			str2 <- paste(str2, " & tmp.data[[", 1+num.panels-i, "]]=='", rev(as.character(tmp.gp))[i+1], "'", sep="")
			str3 <- c(1+2*num.panels-i, str3)
		}
		if (by.grade) {
			tmp.data[eval(parse(text=paste(substring(str1, 4), str2, sep="")))][, c(1, str3), with=FALSE]
		} else {
			tmp.data[eval(parse(text=substring(str1, 4)))][, c(1, str3), with=FALSE]
		}
	}

	get.my.knots.boundaries.path <- function(content_area, year) {
		if (is.null(sgp.percentiles.equated)) {
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
		} else {
			return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", sgp.percentiles.equated[['Year']], "']]", sep=""))
		}
	}

	.create.coefficient.matrices <- function(data, k, by.grade, max.n.for.coefficient.matrices) {
		tmp.data <- .get.panel.data(data, k, by.grade)
		if (dim(tmp.data)[1]==0) return(NULL)
		if (dim(tmp.data)[1] < sgp.cohort.size) return("Insufficient N")
		if (!is.null(max.n.for.coefficient.matrices) && dim(tmp.data)[1] > max.n.for.coefficient.matrices) tmp.data <- tmp.data[sample(seq(dim(tmp.data)[1]), max.n.for.coefficient.matrices)]
		tmp.num.variables <- dim(tmp.data)[2]
		mod <- character()
		s4Ks <- "Knots=list("
		s4Bs <- "Boundaries=list("
		tmp.gp.iter <- rev(tmp.gp)[2:(k+1)]
		for (i in seq_along(tmp.gp.iter)) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(rev(content_area.progression)[i+1], yearIncrement(rev(year.progression)[i+1], 0))
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
				# tmp.data <<- tmp.data
				tmp.mtx <- foreach(j = iter(par.start$TAUS.LIST), .export=c("tmp.data", "Knots_Boundaries", "rq.method"), .combine = "cbind", .packages="quantreg", 
				.inorder=TRUE, .options.mpi = par.start$foreach.options, .options.multicore = par.start$foreach.options, .options.snow = par.start$foreach.options) %dopar% {
					eval(parse(text=paste("rq(tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=j, data=tmp.data, method=rq.method)[['coefficients']]", sep="")))
				}
			} else {
				if (par.start$par.type == 'MULTICORE') {
					tmp.mtx <- mclapply(par.start$TAUS.LIST, function(x) eval(parse(text=paste("rq(tmp.data[[", tmp.num.variables, "]] ~ ", 
						substring(mod,4), ", tau=x, data=tmp.data, method=rq.method)[['coefficients']]", sep=""))), mc.cores=par.start$workers, mc.preschedule = FALSE)
					tmp.mtx <- do.call(cbind, tmp.mtx)
				}
				
				if (par.start$par.type == 'SNOW') {
					tmp.mtx <- parLapplyLB(par.start$internal.cl, par.start$TAUS.LIST, function(x) eval(parse(text=paste("rq(tmp.data[[", 
						tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=x, data=tmp.data, method=rq.method)[['coefficients']]", sep=""))))
					tmp.mtx <- do.call(cbind, tmp.mtx)
				}
			}
			stopParallel(parallel.config, par.start)
		}

		tmp.version <- list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date(), Matrix_Information=list(N=dim(tmp.data)[1]))

		eval(parse(text=paste("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1, nchar(s4Ks)-1), "), ", substring(s4Bs, 1, nchar(s4Bs)-1), "), ",
			"Content_Areas=list(as.character(tail(content_area.progression, k+1))), ",
			"Grade_Progression=list(as.character(tail(tmp.slot.gp, k+1))), ",
			"Time=list(as.character(tail(year.progression, k+1))), ",
			"Time_Lags=list(as.numeric(tail(year_lags.progression, k))), ",
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
		mod <- character()
		int <- "cbind(rep(1, dim(my.data)[1]),"
		for (k in seq_along(my.matrix@Time_Lags[[1]])) {
			knt <- paste("my.matrix@Knots[[", k, "]]", sep="")
			bnd <- paste("my.matrix@Boundaries[[", k, "]]", sep="")
			mod <- paste(mod, ", bs(my.data[[", dim(my.data)[2]-k, "]], knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
		}	
		tmp <- eval(parse(text=paste(int, substring(mod, 2), ") %*% my.matrix", sep="")))
		return(round(matrix(data.table(ID=rep(seq(dim(tmp)[1]), each=length(taus)), SCORE=as.vector(t(tmp)))[,.smooth.isotonize.row(SCORE, isotonize, sgp.loss.hoss.adjustment), by=ID][['V1']], 
				ncol=length(taus), byrow=TRUE), digits=5))
	}

	.get.quantiles <- function(data1, data2) {
		TMP_TF <- SGP <- SGP_NEW <- .EACHI <- NULL
		tmp <- data.table(ID=rep(seq(dim(data1)[1]), each=length(taus)+1), TMP_TF=as.vector(t(cbind(data1 < data2, FALSE))))[,which.min(TMP_TF)-1, by=ID][['V1']]
		if (!is.null(sgp.quantiles.labels)) {
			setattr(tmp <- as.factor(tmp), "levels", sgp.quantiles.labels)
			return(as.integer(levels(tmp))[tmp])
		} else {
			if (!is.null(sgp.loss.hoss.adjustment)) {
				my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
				tmp.hoss <- eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']][2]", sep="")))
				tmp.index <- which(data2==tmp.hoss)
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
			if (is.null(SGP::SGPstateData[[growth.levels]][["Growth"]][["Levels"]])) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Growth Levels are currently not specified for the indicated state. \n\tPlease contact the SGP package administrator to have your state's data included in the package. Student growth percentiles will be calculated without augmented growth levels\n")
				tf.growth.levels <- FALSE
			} else {
				tmp.growth.levels[["my.cuts"]] <- SGP::SGPstateData[[growth.levels]][["Growth"]][["Cutscores"]][["Cuts"]]
				tmp.growth.levels[["my.levels"]] <- SGP::SGPstateData[[growth.levels]][["Growth"]][["Levels"]]
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
			if (is.null(SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) { 
				tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Knots and Boundaries are currently not implemented for the state indicated (",
				use.my.knots.boundaries, "). Knots and boundaries will be calculated from the data.", "
				Please contact the SGP package administrator to have your Knots and Boundaries included in the package\n", sep=""))
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
		tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices, pieces=c("my.subject", "my.year"))
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
		}
		taus <- .create_taus(sgp.quantiles)
		sgp.quantiles.labels <- NULL
	} 
	if (is.numeric(sgp.quantiles)) {
		if (!(all(sgp.quantiles > 0 & sgp.quantiles < 1))) {
			stop("Specify sgp.quantiles as as a vector of probabilities between 0 and 1.")
		}
		taus <- .create_taus(sgp.quantiles)
		if (!is.null(sgp.quantiles.labels)) {
			if (length(sgp.quantiles.labels)!=length(sgp.quantiles)+1) stop("Supplied 'sgp.quantiles.labels' must be 1 longer than supplied 'sgp.quantiles'.")
			if (any(is.na(as.integer(sgp.quantiles.labels)))) stop("Supplied 'sgp.quantiles.labels' must be integer values.")
			sgp.quantiles.labels <- as.integer(sgp.quantiles.labels)
		} else {
			sgp.quantiles.labels <- as.integer(c(100*taus, 100))
		}
	}
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
			if ("variable" %in% names(calculate.confidence.intervals) & is.null(panel.data.vnames)) {
				stop("To utilize a supplied CSEM variable for confidence interval calculation you must specify the variables to be used for student growth percentile calculations with the panel.data.vnames argument. See help page for details.")
			}
			if (all(c("state", "variable") %in% names(calculate.confidence.intervals))) {
				stop("Please specify EITHER a state OR a CSEM variable for SGP confidence interval calculation. See help page for details.")
			}
		} 
		if (is.character(calculate.confidence.intervals)) {
			if (!calculate.confidence.intervals %in% c(objects(SGP::SGPstateData), names(panel.data[['Panel_Data']]))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without confidence intervals.\n")
				csem.tf <- FALSE
			}
			if (calculate.confidence.intervals %in% objects(SGP::SGPstateData)) {
				if ("YEAR" %in% names(SGP::SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGP::SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\t\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. Simulated SGPs and confidence intervals will not be calculated.\n")
						csem.tf <- FALSE
					} 
				}
				if (!sgp.labels$my.subject %in% unique(SGP::SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]])) {
					tmp.messages <- c(tmp.messages, paste("\t\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '", sgp.labels$my.subject, "'. Simulated SGPs and confidence intervals will not be calculated.\n", sep=""))
					csem.tf <- FALSE
				}
				calculate.confidence.intervals <- list(state=calculate.confidence.intervals)
			}
			if (calculate.confidence.intervals %in% names(panel.data[['Panel_Data']])) {
				calculate.confidence.intervals <- list(variable=calculate.confidence.intervals)
			}
		}
		if (sgp.quantiles != "PERCENTILES") {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: When 'sgp.quantiles' is supplied and not equal to PERCENTILES, simulation based standard errors/confidences intervals for SGPs are not available.\n")
			csem.tf <- FALSE
		}
	} else {
		csem.tf <- FALSE
	}

	if (is.logical(calculate.simex) && !calculate.simex) calculate.simex <- NULL # check for calculate.simex=FALSE - same as calculate.simex=NULL
	if (!is.null(calculate.simex)) {
		simex.tf <- TRUE
		if (!is.character(calculate.simex) & !is.list(calculate.simex)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply an appropriate state acronym, variable or list containing details to calculate.simex. See help page for details. SGPs will be calculated without measurement error correction.\n")
			simex.tf <- FALSE
		}
		if (is.list(calculate.simex)) {
			if (!("state" %in% names(calculate.simex)) & !("variable" %in% names(calculate.simex)) & !("csem.data.vnames" %in% names(calculate.simex))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for calculate.simex including state/csem variable, simulation.iterations, lambda and extrapolation. See help page for details. SGPs will be calculated without measurement error correction.\n")
				simex.tf <- FALSE
			}
			if (all(c("state", "variable") %in% names(calculate.simex))) {
				stop("Please specify EITHER a state OR a CSEM variable for SGP measurement error correction. See help page for details.")
			}
			if (!is.null(calculate.simex$lambda)) {
				if (!is.numeric(calculate.simex$lambda)) {
					tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply numeric values to lambda. See help page for details. SGPs will be calculated without measurement error correction.\n")
					simex.tf <- FALSE
				}
				if (any(calculate.simex$lambda < 0)) {
					message("lambda should not contain negative values. Negative values will be ignored", call. = FALSE)
					lambda <- calculate.simex$lambda[calculate.simex$lambda >= 0]
				} else lambda=calculate.simex$lambda
				if (is.null(panel.data.vnames) & !is.null(calculate.simex$csem.data.vnames)) stop("Use of csem.data.vnames in SIMEX requires panel.data.vnames be provided.")
			}
		}
		if (is.character(calculate.simex)) {
			if (!calculate.simex %in% c(objects(SGP::SGPstateData), names(panel.data))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without measurement error correction.\n")
				simex.tf <- FALSE
			}
			if (calculate.simex %in% objects(SGP::SGPstateData)) {
				if ("YEAR" %in% names(SGP::SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGP::SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\t\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. SGPs will be calculated without measurement error correction.\n")
						simex.tf <- FALSE
					}
				}
				if (!sgp.labels$my.subject %in% unique(SGP::SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]])) {
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
		if (!is.null(calculate.simex$simex.sample.size) && !is.numeric(calculate.simex$simex.sample.size)) calculate.simex$simulation.sample.size <- NULL
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

	if (is.null(sgp.cohort.size)) sgp.cohort.size <- 0

	if (is.null(goodness.of.fit.minimum.n)) goodness.of.fit.minimum.n <- 250

	if (!is.null(sgp.percentiles.set.seed)) set.seed(as.integer(sgp.percentiles.set.seed))

	if (!is.null(sgp.time)) {
		if (identical(sgp.time, TRUE)) sgp.time <- list(TIME="TIME", TIME_LAG="TIME_LAG")
		if (is.list(sgp.time) && !all(c("TIME", "TIME_LAG") %in% names(sgp.time))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: 'TIME' and 'TIME_LAG' are not contained in list supplied to 'sgp.time' argument. sgp.time is set to NULL")
			sgp.time <- NULL
		} else {
			if (!((all(unlist(sgp.time) %in% names(panel.data))) | (all(unlist(sgp.time) %in% names(panel.data$Panel_Data))))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(unlist(sgp.time), collapse=", "), "are not all contained in the supplied 'panel.data'. 'sgp.time' is set to NULL.\n")
				sgp.time <- NULL
			}
		}
	}

	### Create object to store the studentGrowthPercentiles objects

	tmp.objects <- c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "Panel_Data", "SGPercentiles", "SGProjections", "Simulated_SGPs") 
	Coefficient_Matrices <- Cutscores <- Goodness_of_Fit <- Knots_Boundaries <- Panel_Data <- SGPercentiles <- SGProjections <- Simulated_SGPs <- SGP_STANDARD_ERROR <- Verbose_Messages <- NULL
	SGP_SIMEX <- SGP_NORM_GROUP_SCALE_SCORES <- SGP_NORM_GROUP <- NULL

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
						tmp.matrices[[i]][[j]] <- as.splineMatrix(matrix_argument=tmp.matrices[[i]][[j]], 
							matrix_argument_name=j, content_area=tmp.content_area, year=tmp.year, sgp_object=panel.data)
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
		Panel_Data <- panel.data <- as.data.table(panel.data)
	}
	if (is.data.frame(panel.data)) {
		Panel_Data <- as.data.table(panel.data)
	}
	if (identical(class(panel.data), "list") && !is.data.table(panel.data[["Panel_Data"]])) {
			Panel_Data <- as.data.table(panel.data[["Panel_Data"]])
	}
	
	### Create ss.data from Panel_Data

	if (dim(Panel_Data)[1]==0 | dim(Panel_Data)[2]<3) {
		tmp.messages <- paste("\t\tNOTE: Supplied data together with grade progression contains no data (dim = ", paste(dim(Panel_Data), collapse=", "), "). Check data, function arguments and see help page for details.\n", sep="")
		message(paste("\tStarted studentGrowthPercentiles", started.date))
		message(paste("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", 
			paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
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

	if (!is.null(panel.data.vnames)) {
		if (!all(panel.data.vnames %in% names(Panel_Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'panel.data.vnames' are not all in the supplied Panel_Data. Analyses will continue with the intersection names contain in Panel_Data.\n")
		}
		ss.data <- Panel_Data[,intersect(panel.data.vnames, names(Panel_Data)), with=FALSE]
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
			tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Supplied 'grade progression', grade.progression=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.\n\t\t Analyses will utilize maximum number of priors supplied by the data.\n", sep=""))
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
			if (!is.null(content_area.progression) && length(content_area.progression > num.prior+1)) content_area.progression <- tail(content_area.progression, num.prior+1)
			
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
		if (!is.null(content_area.progression)) content_area.progression <- tail(content_area.progression, length(tmp.gp))
		if (!is.null(year.progression)) year.progression <- year.progression.for.norm.group <- tail(year.progression, length(tmp.gp))
	}

	if (is.numeric(tmp.gp) & drop.nonsequential.grade.progression.variables && any(diff(tmp.gp) > 1)) {
		ss.data <- ss.data[,c(1, (num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1), (2*num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1))]
		num.panels <- (dim(ss.data)[2]-1)/2
	}

	## Run this check before the setup of ss.data - otherwise function chokes on negative subscripts
	if (exact.grade.progression.sequence & num.prior > num.panels) {
		tmp.messages <- paste("\t\tNOTE: Supplied data together with EXACT grade progression contains fewer panel years than required. \n\t\t
			Check data, function arguments and see help page for details.\n")
		message(paste("\tStarted studentGrowthPercentiles", started.date))
		message(paste("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", 
			paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
		message(paste(tmp.messages, "\tStudent Growth Percentile Analysis NOT RUN", date(), "\n"))

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

	### Create ss.data

	tmp.last <- tail(tmp.gp, 1)
	ss.data <- data.table(ss.data[,c(1, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels)), with=FALSE], key=names(ss.data)[1])
	num.panels <- (dim(ss.data)[2]-1)/2
	if (is.factor(ss.data[[1]])) ss.data[[1]] <- as.character(ss.data[[1]])
	if (exact.grade.progression.sequence) tmp.num.prior <- num.prior else tmp.num.prior <- 1

	max.cohort.size <- dim(.get.panel.data(ss.data, tmp.num.prior, by.grade))[1]
	if (max.cohort.size == 0) {
		tmp.messages <- "\t\tNOTE: Supplied data together with grade progression contains no data. Check data, function arguments and see help page for details.\n"
		message(paste("\tStarted studentGrowthPercentiles", started.date))
		message(paste("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", 
			paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
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

	if (max.cohort.size < sgp.cohort.size) {
		tmp.messages <- paste("\t\tNOTE: Supplied data together with grade progression contains fewer than the minimum cohort size. \n\t\tOnly", max.cohort.size, 
			"valid cases provided with", sgp.cohort.size, "indicated as minimum cohort N size. Check data, function arguments and see help page for details.\n")
		message(paste("\tStarted studentGrowthPercentiles", started.date))
		message(paste("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", 
			paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
		message(paste(tmp.messages, "\tStudent Growth Percentile Analysis NOT RUN", date(), "\n"))

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

	### PROGRESSION variable creation:

	grade.progression <- tmp.gp
	if (is.null(content_area.progression)) {
		content_area.progression <- rep(sgp.labels$my.subject, length(tmp.gp))
	} else {
		if (!identical(class(content_area.progression), "character")) {
			stop("content_area.progression should be a character vector. See help page for details.")
		}
		if (!identical(tail(content_area.progression, 1), sgp.labels[['my.subject']])) {
			stop("The last element in the content_area.progression must be identical to 'my.subject' of the sgp.labels. See help page for details.")
		}
		if (length(content_area.progression) != length(tmp.gp)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: The content_area.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (is.null(year.progression) & is.null(year_lags.progression)) {
		if (is.character(type.convert(as.character(grade.progression), as.is=TRUE))) {
			stop("\tNOTE: Non-numeric grade progressions must be accompanied by arguments 'year.progression' and 'year_lags.progression'")
		} else {
			year.progression <- year.progression.for.norm.group <- rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(diff(type.convert(as.character(grade.progression))))))))
		}
	}

	if (is.null(year.progression) & !is.null(year_lags.progression)) {
		if (!identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- year.progression.for.norm.group <- rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year_lags.progression)))))
		}
		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- rep("BASELINE", length(tmp.gp))
			year.progression.for.norm.group <- rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year_lags.progression)))))
		}
		if (!identical(class(year.progression), "character")) {
			stop("year.area.progression should be a character vector. See help page for details.")
		}
		if (!identical(sgp.labels[['my.extra.label']], "BASELINE") & !identical(tail(year.progression, 1), sgp.labels[['my.year']])) {
			stop("The last element in the year.progression must be identical to 'my.year' of the sgp.labels. See help page for details.")
		}
		if (length(year.progression) != length(tmp.gp)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: The year.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (!is.null(year.progression) & is.null(year_lags.progression)) {
		if (year.progression[1] == "BASELINE") {
			year_lags.progression <- rep(1, length(year.progression)-1)
			year.progression.for.norm.group <- year.progression
		} else {
			year_lags.progression <- diff(as.numeric(sapply(strsplit(year.progression, '_'), '[', split.location(year.progression))))
			year.progression.for.norm.group <- year.progression
		}
	}

	### Create Knots and Boundaries if requested (uses only grades in tmp.gp)

	if (missing(use.my.knots.boundaries)) {
		tmp.knots <- c(Knots_Boundaries[[tmp.path.knots.boundaries]], .get.knots.boundaries(ss.data, by.grade))
		Knots_Boundaries[[tmp.path.knots.boundaries]] <- tmp.knots[!duplicated(names(tmp.knots))]
	} else {
		if (is.character(use.my.knots.boundaries)) {
			if (!is.null(SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) {
				for (h in unique(content_area.progression)) {
					for (i in grep(h, names(SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
						Knots_Boundaries[[tmp.path.knots.boundaries]][[i]] <- SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]][[i]]
					}
				}
			}
		} 
	}

	### QR Calculations: coefficient matrices are saved/read into/from panel.data[["Coefficient_Matrices"]]

	if (is.null(use.my.coefficient.matrices)) {
		if (exact.grade.progression.sequence) {
			coefficient.matrix.priors <- num.prior
		} else {
			coefficient.matrix.priors <- seq(num.prior)
		}
		for (k in coefficient.matrix.priors) {
			Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']] <- .create.coefficient.matrices(ss.data, k, by.grade, max.n.for.coefficient.matrices)
			if (identical(Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']], "Insufficient N")) {
				tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Some grade progressions contain fewer than the minimum cohort size.",
					"\n\t\tOnly analyses with MAX grade progression", paste(rev(rev(tmp.gp)[1:k]), collapse = ', '), "will be produced given", sgp.cohort.size,
					"indicated as minimum cohort N size. \n\t\tCheck data, function arguments and see help page for details.\n"))
				Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']] <- NULL
				grade.progression <- tmp.gp <- rev(rev(tmp.gp)[1:k])
				# num.prior <- length(tmp.gp[2:k]) # Force lots of warnings (?)
				break
			}
			names(Coefficient_Matrices[[tmp.path.coefficient.matrices]])[length(Coefficient_Matrices[[tmp.path.coefficient.matrices]])] <- get.coefficient.matrix.name(tmp.last, k)

			if (verbose.output) {
				tmp.coefficient.matrix.name <- get.coefficient.matrix.name(tmp.last, k)
				tmp.grade.names <- paste("Grade", 
					rev(head(unlist(Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Grade_Progression), -1)))
				Verbose_Messages <- paste("\t\tNOTE: Coefficient Matrix ", tmp.coefficient.matrix.name, " created using:", sep="")
				for (l in seq_along(tmp.grade.names)) {
					tmp.knots <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Knots[l])
					tmp.boundaries <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Boundaries[l])
					Verbose_Messages <- c(Verbose_Messages, paste("\n\t\t\tKnots: ", tmp.knots, " and Boundaries: ", tmp.boundaries, ".", sep=""))
				}
			}
		}
	}

	### Calculate SIMEX corrected coefficient matrices and percentiles (if requested)

	if (simex.tf) {
		environment(simexSGP) <- environment()
		quantile.data.simex <- simexSGP(
			state=calculate.simex$state,
			variable=calculate.simex$variable,
			csem.data.vnames=calculate.simex$csem.data.vnames,
			csem.loss.hoss=calculate.simex$csem.loss.hoss, 
			lambda=calculate.simex$lambda, 
			B=calculate.simex$simulation.iterations,
			simex.sample.size=calculate.simex$simex.sample.size,
			extrapolation=calculate.simex$extrapolation, 
			save.matrices=calculate.simex$save.matrices, 
			simex.use.my.coefficient.matrices=calculate.simex$simex.use.my.coefficient.matrices,
			calculate.simex.sgps=calculate.sgps,
			verbose=calculate.simex$verbose)
								 
		if (!is.null(quantile.data.simex[['MATRICES']])) {
			tmp_sgp_1 <- list(Coefficient_Matrices = list(TMP_SIMEX=Coefficient_Matrices[[paste(tmp.path.coefficient.matrices, '.SIMEX', sep="")]]))
			tmp_sgp_2 <- list(Coefficient_Matrices = list(TMP_SIMEX=quantile.data.simex[['MATRICES']]))
			tmp_sgp_combined <- mergeSGP(tmp_sgp_1, tmp_sgp_2)
			Coefficient_Matrices[[paste(tmp.path.coefficient.matrices, '.SIMEX', sep="")]] <- tmp_sgp_combined[["Coefficient_Matrices"]][["TMP_SIMEX"]]
		}
	}
		
	### Calculate growth percentiles (if requested), percentile cuts (if requested), and simulated confidence intervals (if requested)

	if (calculate.sgps) {

		tmp.matrices <- getsplineMatrices(
					Coefficient_Matrices[[tmp.path.coefficient.matrices]], 
					content_area.progression, 
					grade.progression, 
					year.progression,
					year_lags.progression,
					exact.grade.progression.sequence)

		tmp.orders <- sapply(tmp.matrices, function(x) length(x@Grade_Progression[[1]])-1)
		max.order <- max(tmp.orders)

		if (max.order < num.prior) {
			tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Requested number of prior scores (num.prior=", num.prior, ") exceeds maximum matrix order (max.order=", 
			max.order, "). Only matrices of order up to max.order=", max.order, " will be used.\n", sep=""))
		}
		if (max.order > num.prior) {
			tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Maximum coefficient matrix order (max.order=", max.order, ") exceeds that of specified number of priors, 
				(num.prior=", num.prior, "). Only matrices of order up to num.prior=", num.prior, " will be used.\n", sep=""))
			tmp.matrices <- tmp.matrices[tmp.orders <= max.order]
		}


		tmp.quantiles <- tmp.percentile.cuts <- tmp.csem.quantiles <- list()

		for (j in seq_along(tmp.orders)) {
			tmp.data <- .get.panel.data(ss.data, tmp.orders[j], by.grade)
			if (dim(tmp.data)[1] > 0) {
				tmp.matrix <- tmp.matrices[[j]]
				tmp.predictions <- .get.percentile.predictions(tmp.data, tmp.matrix)
				tmp.quantiles[[j]] <- data.table(ID=tmp.data[[1]], ORDER=tmp.orders[j], SGP=.get.quantiles(tmp.predictions, tmp.data[[dim(tmp.data)[2]]]))
				if (csem.tf) {
					if (is.null(calculate.confidence.intervals[['simulation.iterations']])) calculate.confidence.intervals[['simulation.iterations']] <- 100
					if (!is.null(calculate.confidence.intervals[['variable']])) {
						tmp.csem.variable <- Panel_Data[Panel_Data[[1]] %in% ss.data[list(tmp.data[[1]])][[1]], calculate.confidence.intervals[['variable']]] 
					} else {
						tmp.csem.variable <- NULL
					}

					tmp.csem.quantiles[[j]] <- tmp.data[,names(tmp.data)[1], with=FALSE]
					setnames(tmp.csem.quantiles[[j]], names(tmp.csem.quantiles[[j]])[1], "ID")
					if (!is.null(additional.vnames.to.return)) {
						tmp.csem.quantiles[[j]] <- data.table(panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return)), with=FALSE], key="ID")[tmp.csem.quantiles[[j]]]
						setnames(tmp.csem.quantiles[[j]], names(additional.vnames.to.return), unlist(additional.vnames.to.return))
					}
					for (k in seq(calculate.confidence.intervals[['simulation.iterations']])) { 
						tmp.csem.quantiles[[j]][,TEMP_SGP_SIM:=.get.quantiles(
								tmp.predictions, 
								csemScoreSimulator(
									scale_scores=tmp.data[[dim(tmp.data)[2]]],
									grade=tmp.last,
									content_area=sgp.labels[['my.subject']],
									year=sgp.labels[['my.year']],
									state=calculate.confidence.intervals[['state']],
									variable=tmp.csem.variable,
									distribution=calculate.confidence.intervals[['distribution']],
									round=calculate.confidence.intervals[['round']]))]
						setnames(tmp.csem.quantiles[[j]], "TEMP_SGP_SIM", paste("SGP_SIM", k, sep="_"))
					} ## END k loop
				} ## END CSEM analysis

				if (!is.null(percentile.cuts)) {
					tmp.percentile.cuts[[j]] <- data.table(ID=tmp.data[[1]], .get.percentile.cuts(tmp.predictions))
				}
				if ((is.character(goodness.of.fit) | goodness.of.fit==TRUE | return.prior.scale.score) & j==1) prior.ss <- tmp.data[[dim(tmp.data)[2]-1]]
				if (exact.grade.progression.sequence & return.prior.scale.score) prior.ss <- tmp.data[[dim(tmp.data)[2]-1]]
				if (!is.null(return.norm.group.scale.scores)) tmp.quantiles[[j]][, SGP_NORM_GROUP_SCALE_SCORES:=do.call(paste, c(tmp.data[,-1,with=FALSE], list(sep="; ")))]
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
					quantile.data[,SGP_STANDARD_ERROR:=round(apply(simulation.data[, -1, with=FALSE], 1, sd, na.rm=TRUE), digits=2)]
				} else {
					if (!(is.numeric(calculate.confidence.intervals$confidence.quantiles) & all(calculate.confidence.intervals$confidence.quantiles < 1) & 
						all(calculate.confidence.intervals$confidence.quantiles > 0))) {
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
			Simulated_SGPs[[tmp.path]] <- rbindlist(list(simulation.data, Simulated_SGPs[[tmp.path]]), fill=TRUE) 
		}

		if (simex.tf) quantile.data[, SGP_SIMEX:=quantile.data.simex[['DT']][["SGP_SIMEX"]]]

		if (!is.null(percentile.cuts)){
			cuts.best <- data.table(rbindlist(tmp.percentile.cuts), key="ID")
			cuts.best <- cuts.best[c(which(!duplicated(cuts.best))[-1]-1, nrow(cuts.best))][,-1, with=FALSE]
			quantile.data <- data.table(quantile.data, cuts.best)
		}

		if (print.sgp.order | return.norm.group.identifier) {
			if (exact.grade.progression.sequence) {
				norm.groups <- paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression, sep="_"), sep="/"), num.prior+1), collapse="; ")
			} else {
				norm.groups <- sapply(seq_along(year.progression.for.norm.group)[-1][1:(num.panels-1)], 
				function(x) paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression, sep="_"), sep="/"), x), collapse="; "))
			}
			if (!print.sgp.order) { # Return only SGP_NORM_GROUP
				if (exact.grade.progression.sequence) {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, labels=norm.groups))]
				} else {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups))]
				}
				quantile.data[, ORDER:=NULL]
			} else { # Return both ORDER and SGP_NORM_GROUP
				if (exact.grade.progression.sequence) {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, labels=norm.groups))]
				} else {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups))]
				}
				setnames(quantile.data, "ORDER", "SGP_ORDER")
			}
		}

		if ((is.character(goodness.of.fit) | goodness.of.fit==TRUE) & dim(quantile.data)[1] <= goodness.of.fit.minimum.n) {
			message("\tNOTE: Due to small number of cases (", dim(quantile.data)[1], ") no goodness of fit plots produced.")
			goodness.of.fit <- FALSE
		}

		if (is.character(goodness.of.fit) | goodness.of.fit==TRUE) {
			if (simex.tf) {
				sgps.for.gof <- c("SGP", "SGP_SIMEX")
				sgps.for.gof.path <- c(tmp.path, paste(tmp.path, "SIMEX", sep="."))
			} else {
				sgps.for.gof <- "SGP"
				sgps.for.gof.path <- tmp.path
			}
			if (is.character(goodness.of.fit) & goodness.of.fit %in% objects(SGP::SGPstateData) &&
				!is.null(SGP::SGPstateData[[goodness.of.fit]][['Achievement']][['Cutscores']][[rev(content_area.progression)[2]]][[paste("GRADE_", rev(tmp.gp)[2], sep="")]])) {
				GRADE <- YEAR <- CONTENT_AREA <- NULL
				tmp.gof.data <- getAchievementLevel(
							sgp_data=data.table(
								SCALE_SCORE=quantile.data[['SCALE_SCORE_PRIOR']],
								quantile.data[, c(sgps.for.gof, "SGP_NORM_GROUP"), with=FALSE],
								VALID_CASE="VALID_CASE",
								CONTENT_AREA=rev(content_area.progression)[2],
								YEAR=rev(year.progression.for.norm.group)[2], 
								GRADE=rev(tmp.gp)[2],
								CONTENT_AREA_CURRENT=sgp.labels[['my.subject']],
								YEAR_CURRENT=sgp.labels[['my.year']],
								GRADE_CURRENT=tmp.last),
							state=goodness.of.fit,
							year=rev(year.progression.for.norm.group)[2],
							content_area=rev(content_area.progression)[2],
							grade=tail(tmp.gp, 2)[1])[,!c("YEAR", "GRADE"), with=FALSE]

				setnames(tmp.gof.data, c("SCALE_SCORE", "ACHIEVEMENT_LEVEL", "CONTENT_AREA", "CONTENT_AREA_CURRENT", "YEAR_CURRENT", "GRADE_CURRENT"), 
					c("SCALE_SCORE_PRIOR", "ACHIEVEMENT_LEVEL_PRIOR", "CONTENT_AREA_PRIOR", "CONTENT_AREA", "YEAR", "GRADE"))
				
				### Rename SGP_NORM_GROUP_BASELINE for gofSGP - expecting consistent name to establish norm.group.var in that function
				if ("SGP_NORM_GROUP_BASELINE" %in% names(tmp.gof.data)) setnames(tmp.gof.data, "SGP_NORM_GROUP_BASELINE", "SGP_NORM_GROUP")

				for (gof.iter in seq_along(sgps.for.gof)) {
					Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]][['TMP_NAME']] <- gofSGP(
											sgp_object=tmp.gof.data,
											state=goodness.of.fit,
											years=sgp.labels[['my.year']],
											content_areas=sgp.labels[['my.subject']],
											content_areas_prior=tmp.gof.data[['CONTENT_AREA_PRIOR']][1],
											grades=tmp.last,
											use.sgp=sgps.for.gof[gof.iter],
											output.format="GROB")

					tmp.gof.plot.name <- 
						paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression, sep="_"), sep="/"), num.prior+1), collapse="; ")
					tmp.gof.plot.name <- gsub("MATHEMATICS", "MATH", tmp.gof.plot.name)
					names(Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]])[length(Goodness_of_Fit[[tmp.path]])] <- 
						gsub("/", "_", paste(gsub(";", "", rev(unlist(strsplit(tail(tmp.gof.plot.name, 1), " ")))), collapse=";"))
				}
			} else {
				tmp.gof.data <- data.table(
							SCALE_SCORE_PRIOR=quantile.data[['SCALE_SCORE_PRIOR']],
							quantile.data[, sgps.for.gof, with=FALSE],
							VALID_CASE="VALID_CASE", 
							CONTENT_AREA=sgp.labels[['my.subject']], 
							YEAR=sgp.labels[['my.year']], 
							GRADE=tmp.last)

				for (gof.iter in seq_along(sgps.for.gof)) {
					Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]][['TMP_NAME']] <- gofSGP(
											sgp_object=tmp.gof.data,
											years=sgp.labels[['my.year']],
											content_areas=sgp.labels[['my.subject']],
											grades=tmp.last,
											use.sgp=sgps.for.gof[gof.iter],
											output.format="GROB")
					tmp.gof.plot.name <- 
						paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression, sep="_"), sep="/"), num.prior+1), collapse="; ")
					tmp.gof.plot.name <- gsub("MATHEMATICS", "MATH", tmp.gof.plot.name)
					names(Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]])[length(Goodness_of_Fit[[tmp.path]])] <- 
						gsub("/", "_", paste(gsub(";", "", rev(unlist(strsplit(tail(tmp.gof.plot.name, 1), " ")))), collapse=";"))
				}
			}
		}

		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) setnames(quantile.data, "SGP", "SGP_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & "SGP_STANDARD_ERROR" %in% names(quantile.data)) setnames(quantile.data, "SGP_STANDARD_ERROR", "SGP_BASELINE_STANDARD_ERROR")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & identical(return.norm.group.scale.scores, TRUE)) {
			setnames(quantile.data, "SGP_NORM_GROUP_SCALE_SCORES", "SGP_NORM_GROUP_BASELINE_SCALE_SCORES")
		}
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & identical(return.norm.group.identifier, TRUE)) {
			setnames(quantile.data, "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE")
		}
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & print.other.gp) {
			my.tmp.names <- grep("SGP_ORDER", names(quantile.data), value=TRUE)
			setnames(quantile.data, my.tmp.names, gsub("SGP_ORDER", "SGP_BASELINE_ORDER", my.tmp.names))
		}
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & simex.tf) setnames(quantile.data, "SGP_SIMEX", "SGP_SIMEX_BASELINE")

		if (!is.null(additional.vnames.to.return)) {
			quantile.data <- data.table(panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return)), with=FALSE], key="ID")[quantile.data]
			setnames(quantile.data, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
		}

		if (identical(sgp.labels[['my.extra.label']], "BASELINE") & identical(print.sgp.order, TRUE)) setnames(quantile.data, "SGP_ORDER", "SGP_BASELINE_ORDER")

		if (!return.prior.scale.score) {
			quantile.data[,SCALE_SCORE_PRIOR:=NULL]
		}

		SGPercentiles[[tmp.path]] <- rbindlist(list(quantile.data, SGPercentiles[[tmp.path]]), fill=TRUE)

	} ## End if calculate.sgps


	### Start/Finish Message & Return SGP Object

	if (print.time.taken) {
		message(paste("\tStarted studentGrowthPercentiles:", started.date))
		if (calculate.sgps) {
			message(paste("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", 
				paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, " (N=", format(dim(quantile.data)[1], big.mark=","), ")", sep=""))
		} else {
			message(paste("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", 
				paste(tmp.slot.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
		}
		if (verbose.output) message(Verbose_Messages)
		message(c(tmp.messages, "\tFinished SGP Student Growth Percentile Analysis: ", date(), " in ", timetaken(started.at), "\n")) 
	}

	list(Coefficient_Matrices=Coefficient_Matrices,
		Cutscores=Cutscores, 
		Goodness_of_Fit=Goodness_of_Fit, 
		Knots_Boundaries=Knots_Boundaries,
		Panel_Data = if (return.panel.data) Panel_Data else NULL, 
		SGPercentiles=SGPercentiles,
		SGProjections=SGProjections,
		Simulated_SGPs=Simulated_SGPs)

} ### END studentGrowthPercentiles Function
