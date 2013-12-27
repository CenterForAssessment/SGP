`summarizeSGP` <- 
function(sgp_object,
         state=NULL,
         years=NULL,
         content_areas=NULL,
         sgp.summaries=NULL,
         summary.groups=NULL,
         confidence.interval.groups=NULL,
         produce.all.summary.tables=FALSE,
         summarizeSGP.baseline=NULL,
         projection.years.for.target=3,
         save.old.summaries=FALSE,
	 highest.level.summary.grouping="STATE",
         parallel.config=NULL) {

	started.at <- proc.time()
	message(paste("\nStarted summarizeSGP", date()))

	### Set variables to NULL to prevent R CMD check warnings

	tmp.simulation.dt <- variable <- WEIGHT <- ENROLLMENT_STATUS <- names.type <- names.sgp <- names.output <- BY_GROWTH_ONLY <- VALID_CASE <- YEAR_WITHIN <- NULL

	
	### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "summarizeSGP")
        }


	### Create specific variables

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["highest.level.summary.grouping"]])) {
		highest.level.summary.grouping <- SGPstateData[[state]][["SGP_Configuration"]][["highest.level.summary.grouping"]]
	}

	### Create slot.data from sgp_object@Data

	slot.data <- copy(sgp_object@Data)


	### Export/Overwrite old summaries

	if (!is.null(sgp_object@Summary)) {
		if (save.old.summaries) {
			tmp.index <- grep("YEAR", names(sgp_object@Summary[[highest.level.summary.grouping]]))[1]
			tmp.year <- tail(sort(sgp_object@Summary[[highest.level.summary.grouping]][[tmp.index]][['YEAR']]), 1)
			tmp.state.name <- gsub(" ", "_", toupper(getStateAbbreviation(state, type="name")))
			tmp.file.name <- paste(tmp.state.name, "SGP_Summary", tmp.year, sep="_")
			assign(tmp.file.name, sgp_object@Summary)
			message(paste("\tNOTE: Saving @Summary slot to", paste("Data/", tmp.file.name, ".Rdata and then deleting @Summary slot.", sep="")))
			save(list=tmp.file.name, file=file.path("Data", paste(tmp.file.name, "Rdata", sep=".")))
			sgp_object@Summary <- NULL
		} else {
			message("\tNOTE: Deleting @Summary slot")
			sgp_object@Summary <- NULL
		}
	}

	### define summarizeSGP.baseline

	if (is.null(summarizeSGP.baseline)) {
		summarizeSGP.baseline <- FALSE ## Default to cohort referenced is not set by user
		if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort Referenced") summarizeSGP.baseline <- FALSE
		if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Baseline Referenced") summarizeSGP.baseline <- TRUE
		if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced") summarizeSGP.baseline <- TRUE
	}

	if (summarizeSGP.baseline) my.sgp <- intersect(names(slot.data), c("SGP", "SGP_BASELINE", "SGP_SIMEX", "SGP_SIMEX_BASELINE")) else my.sgp <- intersect(names(slot.data), c("SGP", "SGP_SIMEX"))

	my.sgp.target <- paste("SGP_TARGET", projection.years.for.target, "YEAR", sep="_")
	my.sgp.target.musu <- paste("SGP_TARGET_MOVE_UP_STAY_UP", projection.years.for.target, "YEAR", sep="_")
	my.sgp.target.baseline <- paste("SGP_TARGET_BASELINE", projection.years.for.target, "YEAR", sep="_")
	my.sgp.target.musu.baseline <-  paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", projection.years.for.target, "YEAR", sep="_")

	if (!my.sgp.target %in% names(slot.data)) my.sgp.target <- NULL
	if (!my.sgp.target.musu %in% names(slot.data)) my.sgp.target.musu <- NULL
	if (!my.sgp.target.baseline %in% names(slot.data)) my.sgp.target.baseline <- NULL
	if (!my.sgp.target.musu.baseline %in% names(slot.data)) my.sgp.target.musu.baseline <- NULL


	if (missing(sgp_object)) {
		stop("User must supply an SGP object containing a @Data slot with long data. See documentation for details.")
	}

	setkeyv(slot.data, getKey(sgp_object))


	## Set up parallel.config if NULL

	if (is.null(parallel.config)) {
		 parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SUMMARY=1))
	}

	## Utility Functions

	"%w/o%" <- function(x, y) x[!x %in% y]

	getFromNames <- function(x) {
		tmp.names <- sgp_object@Names[!is.na(sgp_object@Names$names.type),]
		return(tmp.names[tmp.names$names.type==x, "names.sgp"])
	}

	group.format <- function(my.group, add.missing=TRUE) {
		if (is.null(my.group) & add.missing) {
			return(c(""))
		}
		if (is.null(my.group) & !add.missing) {
			return(NULL)
		}
		if (!is.null(my.group) & add.missing) {
			return(c("", unlist(lapply(my.group, function(x) paste(", ", x, sep="")))))
		}
		if (!is.null(my.group) & !add.missing) {
			return(unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
		}
	}

	weighted.median <- function(x, probs=0.5, w, na.rm=TRUE) {
		if (is.null(w)) return(as.numeric(quantile(x, probs, na.rm)))
		q <- !is.na(x) & !is.na(w) & w!=0
		if (length(x[q]) < 2) return(as.numeric(quantile(x, probs, na.rm)))
		if (!all(q)) {if (na.rm) {x<-x[q]; w<-w[q]} else stop("NA's")}
		ord <- order(x)
		z <- list(y=x[ord], w=w[ord])
		z$x <- (cumsum(z$w) - z$w[1]) / (sum(z$w) - z$w[1]) # 0 to 1 inclusive
		a <- approx(z$x, z$y, probs)$y
		dec <- if (length(probs)>1) 2-log10(diff(range(probs))) else 2
		return(a)
	}

	median_na <- function(x, weight) {
		if (is.null(weight)) {
			median(as.numeric(x), na.rm=TRUE)
		} else {
			weighted.median(as.numeric(x), w=weight, na.rm=TRUE)
		}
	}
	boot.median <- function(x,i) median(x[i], na.rm=TRUE)
	mean_na <- function(x, weight, result.digits=2) {
		if (is.null(weight)) {
			round(mean(as.numeric(x), na.rm=TRUE), digits=result.digits)
		} else {
			round(weighted.mean(as.numeric(x), w=weight, na.rm=TRUE), digits=result.digits)
		}
	}
	sd_na <- function(x, result.digits=2) round(sd(as.numeric(x), na.rm=TRUE), digits=result.digits)
	num_non_missing <- function(x) sum(!is.na(as.numeric(x)))
	sgp_standard_error <- function(x,y=1) round(y*sd(x, na.rm=TRUE)/sqrt(sum(!is.na(as.numeric(x)))), digits=2)

	percent_in_category <- function(x, in.categories, of.categories, result.digits=1) { ## NOTE: x must be a factor and categories levels
		if (!is.list(in.categories)) in.categories <- list(in.categories)
		if (!is.list(of.categories)) of.categories <- list(of.categories)
		tmp.result <- list()
		tmp <- summary(x[!is.na(x)])
		for (i in seq(length(in.categories))) {
			tmp.result[[i]] <- round(100*sum(tmp[in.categories[[i]]])/sum(tmp[of.categories[[i]]]), digits=result.digits)
		}
		return(unlist(tmp.result))
	}

	percent_at_above_target <- function(sgp, target, result.digits=1) {
		tmp.logical <- sgp >= target
		tmp.pct <- round(sum(tmp.logical, na.rm=TRUE)/sum(!is.na(tmp.logical))*100, digits=result.digits)
		return(tmp.pct)
	}

	boot.sgp <- function(dat, conf.quantiles=NULL, nboot=100) {
		out <- numeric(); CI <- c(NA,NA); SE <- NA
		if (sum(is.na(dat)) != length(dat)) {
			for (j in 1:nboot) {
				foo <- sample(dat,length(dat), replace=TRUE)
				out[j] <- boot.median(foo)
			}
			if (!is.null(conf.quantiles)) {
				CI <- round(as.numeric(quantile(out, conf.quantiles, na.rm=TRUE)), digits=1)
			} else {
				SE <- round(as.numeric(sd(out, na.rm=TRUE)), digits=1)
			}
		}
		if (!is.null(conf.quantiles)) return(CI) else return(SE)
	}

	sgpSummary <- function(data, sgp.groups.to.summarize, produce.confidence.interval, tmp.simulation.dt) {
		SGP_SIM <- V1 <- V2 <- .SD <- MEDIAN_SGP_with_SHRINKAGE <- NULL ## To prevent R CMD check warning
		tmp.sgp.summaries <- sgp.summaries
		sgp.summaries.names <- unlist(strsplit(names(sgp.summaries), "[.]"))
		if (produce.confidence.interval) {
			if ("Bootstrap_CI" %in% confidence.interval.groups$TYPE) {
				tmp.list <- list()
				tmp.quantiles <- paste("c(", paste(confidence.interval.groups$QUANTILES, collapse=", "), ")", sep="")
				for (i in confidence.interval.groups$VARIABLES) {
					tmp.list[[paste("MEDIAN_", i, "_QUANTILES", sep="")]] <- paste("boot.sgp(", i, ", ", tmp.quantiles, ")", sep="")
				}
				tmp.sgp.summaries <- c(tmp.sgp.summaries, tmp.list)
				sgp.summaries.names <- c(sgp.summaries.names, do.call(paste, c(data.table(expand.grid("MEDIAN", my.sgp, confidence.interval.groups$QUANTILES, "CONFIDENCE_BOUND_BOOTSTRAP"), key="Var2"), sep="_"))) 
			} 
			if ("Bootstrap_SE" %in% confidence.interval.groups$TYPE) {
				tmp.list <- list()
				for (i in confidence.interval.groups$VARIABLES) {
					tmp.list[[paste("MEDIAN_", i, "_SE", sep="")]] <- paste("boot.sgp(", i, ")", sep="")
				}
				tmp.sgp.summaries <- c(tmp.sgp.summaries, tmp.list)
				sgp.summaries.names <- c(sgp.summaries.names, do.call(paste, c(data.table(expand.grid("MEDIAN", my.sgp, "STANDARD_ERROR_BOOTSTRAP"), key="Var2"), sep="_"))) 
			}
		}
 
		ListExpr <- parse(text=paste("as.list(c(", paste(unlist(tmp.sgp.summaries), collapse=", "),"))",sep="")) 
		ByExpr <- parse(text=paste("list(", paste(sgp.groups.to.summarize, collapse=", "), ")", sep=""))
		tmp <- data[, eval(ListExpr), keyby=eval(ByExpr)]
		setnames(tmp, (dim(tmp)[2]-length(sgp.summaries.names)+1):dim(tmp)[2], sgp.summaries.names)

		if (produce.confidence.interval & "CSEM" %in% confidence.interval.groups[['TYPE']]) {
			tmp.list.1 <- list() 
			tmp.number.simulated.sgps <- length(grep("SGP_SIM", names(sgp_object@SGP[["Simulated_SGPs"]][[1]])))
			tmp.number.unique.cases <- dim(tmp.simulation.dt)[1]/tmp.number.simulated.sgps
			for (i in seq(tmp.number.simulated.sgps)) {
				tmp.subset.data <- data[,c(key(data), unlist(strsplit(sgp.groups.to.summarize, ", "))), with=FALSE][
					tmp.simulation.dt[seq(i, length=tmp.number.unique.cases, by=tmp.number.simulated.sgps)], allow.cartesian=TRUE]
				tmp.list.1[[i]] <- tmp.subset.data[,list(median_na(SGP_SIM, NULL), mean_na(SGP_SIM, NULL)), keyby=c(unlist(strsplit(sgp.groups.to.summarize, ", ")), "BASELINE")]
			}
			tmp.csem <- data.table(reshape(rbindlist(tmp.list.1)[,list(sd_na(V1), sd_na(V2)), keyby=c(unlist(strsplit(sgp.groups.to.summarize, ", ")), "BASELINE")],
					idvar=c(unlist(strsplit(sgp.groups.to.summarize, ", "))), timevar="BASELINE", direction="wide"), key=key(tmp))
			if (length(grep("BASELINE", names(tmp.csem)))==0) {
				setnames(tmp.csem, c("V1.COHORT", "V2.COHORT"), c("MEDIAN_SGP_STANDARD_ERROR_CSEM", "MEAN_SGP_STANDARD_ERROR_CSEM"))
			} else {
				setnames(tmp.csem, c("V1.COHORT", "V2.COHORT", "V1.BASELINE", "V2.BASELINE"), 
					c("MEDIAN_SGP_STANDARD_ERROR_CSEM", "MEAN_SGP_STANDARD_ERROR_CSEM", "MEDIAN_SGP_BASELINE_STANDARD_ERROR_CSEM", "MEAN_SGP_BASELINE_STANDARD_ERROR_CSEM")) 
			}

			tmp <- tmp.csem[tmp]
			setcolorder(tmp, c(grep("CSEM", names(tmp), invert=TRUE), grep("CSEM", names(tmp))))
		}

		if ('MEDIAN_SGP_STANDARD_ERROR' %in% names(tmp)) {
			constant <- var(tmp[['MEDIAN_SGP']], na.rm=TRUE) - mean(tmp[['MEDIAN_SGP_STANDARD_ERROR']]^2, na.rm=TRUE)
			tmp[,MEDIAN_SGP_with_SHRINKAGE := round(50 + ((tmp[['MEDIAN_SGP']]-50) * (constant/(constant+tmp[['MEDIAN_SGP_STANDARD_ERROR']]^2))))]
		}
		
		message(paste("\tFinished with", sgp.groups.to.summarize))
		return(tmp)
	} ### END sgpSummary function

	combineSims <- function(sgp_object) {
		tmp.list <- list()
		tmp.names <- names(sgp_object@SGP[["Simulated_SGPs"]]) 
		for (i in tmp.names) {
			if (length(grep("BASELINE", i) > 0)) tmp.baseline <- "BASELINE" else tmp.baseline <- "COHORT"
			if ("YEAR_WITHIN" %in% names(sgp_object@SGP[["Simulated_SGPs"]][[i]])) columns.to.omit <- -c(1,2) else columns.to.omit <- -1
			tmp.list[[i]] <- data.table(
				ID=rep(sgp_object@SGP[["Simulated_SGPs"]][[i]][["ID"]], each=length(grep("SGP_SIM", names(sgp_object@SGP[["Simulated_SGPs"]][[i]])))),
				SGP_SIM=as.integer(as.matrix(t(sgp_object@SGP[["Simulated_SGPs"]][[i]][,columns.to.omit]))))[,
				"CONTENT_AREA":=unlist(strsplit(i, "[.]"))[1]][,
				"YEAR":=unlist(strsplit(i, "[.]"))[2]][,
				"BASELINE":=tmp.baseline]
			if ("YEAR_WITHIN" %in% names(sgp_object@SGP[["Simulated_SGPs"]][[i]])) {
				tmp.list[[i]][,YEAR_WITHIN:=rep(sgp_object@SGP[["Simulated_SGPs"]][[i]][["YEAR_WITHIN"]], each=length(grep("SGP_SIM", names(sgp_object@SGP[["Simulated_SGPs"]][[i]]))))]
			}
		}
		return(data.table(rbindlist(tmp.list), VALID_CASE="VALID_CASE", key=key(tmp.dt)))
	}

	summarizeSGP.config <- function(sgp_object, config.type) {

		if (config.type=="sgp.summaries") {
			all.achievement.levels <- SGPstateData[[state]][["Achievement"]][["Levels"]][[1]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][[2]])]
			proficient.achievement.levels <- SGPstateData[[state]][["Achievement"]][["Levels"]][[1]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][[2]]) & 
				SGPstateData[[state]][["Achievement"]][["Levels"]][[2]]=="Proficient"]

			get.expression <- function(character.vector) {
				if (length(character.vector)==0) {
					return(NULL)
				} else {
					paste("list(c(", paste("'", paste(character.vector, collapse="', '"), "'", sep=""), "))", sep="")
				}
			}

			tmp.sgp.summaries <- list(
				MEAN_SGP="mean_na(SGP, WEIGHT)",
				MEDIAN_SGP="median_na(SGP, WEIGHT)",
				MEDIAN_SGP_COUNT="num_non_missing(SGP)",
				PERCENT_AT_ABOVE_PROFICIENT=paste("percent_in_category(ACHIEVEMENT_LEVEL, ", 
					get.expression(proficient.achievement.levels), ", ", get.expression(all.achievement.levels), ")",sep=""),
				PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)",
				MEAN_SGP_STANDARD_ERROR="sgp_standard_error(SGP)",
				MEDIAN_SGP_STANDARD_ERROR="sgp_standard_error(SGP, 1.253)")

				if (summarizeSGP.baseline) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEAN_SGP_BASELINE="mean_na(SGP_BASELINE, WEIGHT)",
						MEDIAN_SGP_BASELINE="median_na(SGP_BASELINE, WEIGHT)",
						MEAN_SGP_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_BASELINE)",
						MEDIAN_SGP_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_BASELINE, 1.253)"
					)
				}

				if ("ACHIEVEMENT_LEVEL_PRIOR" %in% names(slot.data)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						PERCENT_AT_ABOVE_PROFICIENT_PRIOR=paste("percent_in_category(ACHIEVEMENT_LEVEL_PRIOR, ", 
							get.expression(proficient.achievement.levels), ", ", get.expression(all.achievement.levels), ")",sep=""),
						PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT="num_non_missing(ACHIEVEMENT_LEVEL_PRIOR)"
					)
				}

				if ("SCALE_SCORE_PRIOR_STANDARDIZED" %in% names(slot.data)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEAN_SCALE_SCORE_PRIOR_STANDARDIZED="mean_na(SCALE_SCORE_PRIOR_STANDARDIZED, WEIGHT)",
						SD_SCALE_SCORE_PRIOR_STANDARDIZED="sd_na(SCALE_SCORE_PRIOR_STANDARDIZED)"
					)
				}

				if ("SGP_SIMEX" %in% names(slot.data)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEDIAN_SGP_SIMEX="median_na(SGP_SIMEX, WEIGHT)",
						MEAN_SGP_SIMEX="mean_na(SGP_SIMEX, WEIGHT)",
						MEAN_SGP_SIMEX_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX)",
						MEDIAN_SGP_SIMEX_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX, 1.253)"
					)
				}

				if ("SGP_SIMEX_BASELINE" %in% names(slot.data)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEDIAN_SGP_SIMEX_BASELINE="median_na(SGP_SIMEX_BASELINE, WEIGHT)",
						MEAN_SGP_SIMEX_BASELINE="mean_na(SGP_SIMEX_BASELINE, WEIGHT)",
						MEAN_SGP_SIMEX_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_BASELINE)",
						MEDIAN_SGP_SIMEX_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_BASELINE, 1.253)"
					)
				}

				if (!is.null(my.sgp.target)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries, 
						M1=paste("median_na(", my.sgp.target, ", WEIGHT)", sep=""),  
						M2=paste("num_non_missing(", my.sgp.target, ")", sep=""),
						PERCENT_CATCHING_UP_KEEPING_UP="percent_in_category(CATCH_UP_KEEP_UP_STATUS, list(c('Catch Up: Yes', 'Keep Up: Yes')), list(c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No')))"
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <- 
						c(paste("MEDIAN", my.sgp.target, sep="_"), paste("MEDIAN", my.sgp.target, "COUNT", sep="_"))
				}

				if (!is.null(my.sgp.target.baseline)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries, 
						M1=paste("median_na(", my.sgp.target.baseline, ", WEIGHT)", sep=""),  
						M2=paste("num_non_missing(", my.sgp.target.baseline, ")", sep=""),
						PERCENT_CATCHING_UP_KEEPING_UP_BASELINE="percent_in_category(CATCH_UP_KEEP_UP_STATUS_BASELINE, list(c('Catch Up: Yes', 'Keep Up: Yes')), list(c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No')))"
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <- 
						c(paste("MEDIAN", my.sgp.target.baseline, sep="_"), paste("MEDIAN", my.sgp.target.baseline, "COUNT", sep="_"))

				}

				if (!is.null(my.sgp.target.musu)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries, 
						M1=paste("median_na(", my.sgp.target.musu, ", WEIGHT)", sep=""),  
						M2=paste("num_non_missing(", my.sgp.target.musu, ")", sep=""),
						PERCENT_MOVING_UP_STAYING_UP="percent_in_category(MOVE_UP_STAY_UP_STATUS, list(c('Move Up: Yes', 'Stay Up: Yes')), list(c('Move Up: Yes', 'Move Up: No', 'Stay Up: Yes', 'Stay Up: No')))"
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <- 
						c(paste("MEDIAN", my.sgp.target.musu, sep="_"), paste("MEDIAN", my.sgp.target.musu, "COUNT", sep="_"))
				}

				if (!is.null(my.sgp.target.musu.baseline)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries, 
						M1=paste("median_na(", my.sgp.target.musu.baseline, ", WEIGHT)", sep=""),  
						M2=paste("num_non_missing(", my.sgp.target.musu.baseline, ")", sep=""),
						PERCENT_MOVING_UP_STAYING_UP_BASELINE="percent_in_category(MOVE_UP_STAY_UP_STATUS_BASELINE, list(c('Move Up: Yes', 'Stay Up: Yes')), list(c('Move Up: Yes', 'Move Up: No', 'Stay Up: Yes', 'Stay Up: No')))"
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <- 
						c(paste("MEDIAN", my.sgp.target.musu.baseline, sep="_"), paste("MEDIAN", my.sgp.target.musu.baseline, "COUNT", sep="_"))
				}

			return(tmp.sgp.summaries)
		}

		if (config.type=="summary.groups") {
			tmp.summary.groups <- list(
				institution=c(highest.level.summary.grouping, getFromNames("institution")),
				institution_type=getFromNames("institution_type"),
				content=getFromNames("content"),
				time=getFromNames("time"),
				institution_level=getFromNames("institution_level"),
				demographic=intersect(c(getFromNames("demographic"), "CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STAY_UP_STATUS", "ACHIEVEMENT_LEVEL_PRIOR", "HIGH_NEED_STATUS"), names(slot.data)),
				institution_multiple_membership=get.multiple.membership(sgp_object))

			tmp.summary.groups[["institution_inclusion"]] <- vector(mode="list", length=length(tmp.summary.groups[["institution"]]))
			names(tmp.summary.groups[["institution_inclusion"]]) <- tmp.summary.groups[["institution"]]
			for (i in tmp.summary.groups[["institution"]]) {
				tmp.split <- paste(c(unlist(strsplit(i, "_"))[!unlist(strsplit(i, "_"))=="NUMBER"], "ENROLLMENT_STATUS"), collapse="_")
				if (tmp.split %in% getFromNames("institution_inclusion")) {
					tmp.summary.groups[["institution_inclusion"]][[i]] <- tmp.split
				}
				tmp.summary.groups[["growth_only_summary"]][[i]] <- "BY_GROWTH_ONLY"
			}
			tmp.summary.groups[["institution_inclusion"]] <- as.list(tmp.summary.groups[["institution_inclusion"]])
			tmp.summary.groups[["growth_only_summary"]] <- as.list(tmp.summary.groups[["growth_only_summary"]])

			return(tmp.summary.groups)
		}
		
		if (config.type=="confidence.interval.groups") {
			if (is.null(names(sgp_object@SGP[['Simulated_SGPs']]))) tmp.type <- c("Bootstrap_CI", "Bootstrap_SE") else tmp.type <- c("Bootstrap_CI", "Bootstrap_SE", "CSEM")
			tmp.confidence.interval.groups <- list(
				TYPE=tmp.type,
				VARIABLES=my.sgp,
				QUANTILES=c(0.025, 0.975),
				GROUPS=list(
					institution=c("SCHOOL_NUMBER", "STATE, INSTRUCTOR_NUMBER"),
					institution_type="EMH_LEVEL",
					content="CONTENT_AREA",
					time="YEAR",
					institution_level=NULL,
					demographic=NULL,
					institution_multiple_membership=get.multiple.membership(sgp_object)))

			tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]] <- vector(mode="list", length=length(tmp.confidence.interval.groups[["GROUPS"]][["institution"]]))
			names(tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]]) <- tmp.confidence.interval.groups[["GROUPS"]][["institution"]]
			for (i in tmp.confidence.interval.groups[["GROUPS"]][["institution"]]) {
				tmp.split <- paste(c(unlist(strsplit(i, "_"))[!unlist(strsplit(i, "_"))=="NUMBER"], "ENROLLMENT_STATUS"), collapse="_")
				if (tmp.split %in% getFromNames("institution_inclusion")) {
					tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]][[i]] <- tmp.split
				} 
				tmp.confidence.interval.groups[["GROUPS"]][["growth_only_summary"]][[i]] <- "BY_GROWTH_ONLY"
			}
			tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]] <- as.list(tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]])
			tmp.confidence.interval.groups[["GROUPS"]][["growth_only_summary"]] <- as.list(tmp.confidence.interval.groups[["GROUPS"]][["growth_only_summary"]])

			return(tmp.confidence.interval.groups)
		}
	} ### END summarizeSGP.config

	get.multiple.membership <- function(sgp_object) {
		tmp.names <- list()
		if (is.null(sgp_object@Data_Supplementary)) {
			tmp.names <- NULL
		} else {
			for (i in seq_along(sgp_object@Data_Supplementary)) {
				tmp.variable.names <- names(sgp_object@Data_Supplementary)[i]
				tmp.weights <- grep("WEIGHT", names(sgp_object@Data_Supplementary[[i]]), value=TRUE)
				tmp.inclusion <- grep("ENROLLMENT_STATUS", names(sgp_object@Data_Supplementary[[i]]), value=TRUE)

				if (length(tmp.weights) == 0) {
					tmp.weights <- NULL
				}
				if (length(tmp.weights) > 1) {
					stop(paste("\tNOTE: Number of 'WEIGHT' variables in the '@Data_Supplementary' table", tmp.variable.names, "exceeds 1. Only 1 'WEIGHT' variable allowed."))
				}

				if (length(tmp.inclusion) == 0) {
					tmp.inclusion <- NULL
				}
				if (length(tmp.inclusion) > 1) {
					stop(paste("\tNOTE: Number of 'ENROLLMENT_STATUS' variables in the '@Data_Supplementary' table", tmp.variable.names, "exceeds 1. Only 1 'ENROLLMENT_STATUS' variable allowed."))
				}

				tmp.names[[i]] <- list(VARIABLE.NAME=tmp.variable.names, WEIGHTS=tmp.weights, ENROLLMENT_STATUS=tmp.inclusion)
			}
		}
		return(tmp.names)
	} ### END get.multiple.membership

	summarizeSGP_INTERNAL <- function(data, i) {
		
		tmp.summary <- list()

		if (!is.null(confidence.interval.groups) & "CSEM" %in% confidence.interval.groups[['TYPE']])  tmp.simulation.dt <- combineSims(sgp_object) else tmp.simulation.dt <- NULL


		### Create summary tables
		
		sgp.groups <- do.call(paste, c(expand.grid(i,
			group.format(summary.groups[["institution_type"]]),
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["demographic"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["growth_only_summary"]][[i]])), sep=""))

		if (!produce.all.summary.tables) {
			sgp.groups <- intersect(sgp.groups, selected.summary.tables)
			if (length(sgp.groups)==0) return(NULL)
		}

		if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
			ci.groups <- do.call(paste, c(expand.grid(i,
				group.format(confidence.interval.groups[["GROUPS"]][["institution_type"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["content"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["time"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_level"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["demographic"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_inclusion"]][[i]]),
				group.format(confidence.interval.groups[["GROUPS"]][["growth_only_summary"]][[i]])), sep=""))

			if (!produce.all.summary.tables) ci.groups <- intersect(ci.groups, selected.summary.tables)
		}

		if(par.start$par.type=="FOREACH") {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
	  			j <- k <- NULL ## To prevent R CMD check warnings
	  			tmp.summary <- foreach(j=iter(sgp.groups), k=iter(sgp.groups %in% ci.groups), 
	  				.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE) %dopar% {
						return(sgpSummary(data, j, k, tmp.simulation.dt))
				}
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			} else {
				j <- k <- NULL ## To prevent R CMD check warnings
				tmp.summary <- foreach(j=iter(sgp.groups), k=iter(rep(FALSE, length(sgp.groups))), 
					.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE) %dopar% {
						return(sgpSummary(data, j, k, tmp.simulation.dt))
				}
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			}
		} # END FOREACH flavor

		if(par.start$par.type=="SNOW") {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
	  			j <- k <- NULL ## To prevent R CMD check warnings
	  			summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], sgp.groups[x] %in% ci.groups))
	  			tmp.summary <- parLapply(par.start$internal.cl, summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2])), tmp.simulation.dt))
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			} else {
				j <- k <- NULL ## To prevent R CMD check warnings
				summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], FALSE))
	  			tmp.summary <- parLapply(par.start$internal.cl, summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2])), tmp.simulation.dt))
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			}
			# if (is.null(parallel.config[['CLUSTER.OBJECT']]))	 stopCluster(internal.cl)
		} # END 'SNOW' Flavor
 
		if (par.start$par.type=="MULTICORE") {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
	  			j <- k <- NULL ## To prevent R CMD check warnings
	  			summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], sgp.groups[x] %in% ci.groups))
	  			tmp.summary <- mclapply(summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2])), tmp.simulation.dt), mc.cores=par.start$workers, mc.preschedule=FALSE)
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			} else {
				j <- k <- NULL ## To prevent R CMD check warnings
				summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], FALSE))
	  			tmp.summary <- mclapply(summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2])), tmp.simulation.dt), mc.cores=par.start$workers, mc.preschedule=FALSE)	
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			}
		} # END 'MULTICORE' Flavor

		return(tmp.summary)
	} ### END summarizeSGP_INTERNAL


	###################################################################################
	###
	### Setup arguments for call to summarizeSGP_INTERNAL
	###
	###################################################################################

	if (is.null(content_areas)) {
		content_areas <- unique(slot.data["VALID_CASE"]$CONTENT_AREA)
	}

	if (is.null(SGPstateData[[state]][["SGP_Configuration"]][["state.multiple.year.summary"]])) {	
		state.multiple.year.summary <- 3
	} else {
		state.multiple.year.summary <- SGPstateData[[state]][["SGP_Configuration"]][["state.multiple.year.summary"]]
	}
	tmp.years <- list()
	if (is.null(years)) {
		for (i in content_areas) {
			tmp.years[[i]] <- tail(sort(unique(slot.data[SJ("VALID_CASE", i)]$YEAR)), state.multiple.year.summary)
		}
	} else {
		if (!is.list(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- tail(sort(unique(slot.data[SJ("VALID_CASE", i)]$YEAR))[
					seq(which(sort(unique(slot.data[SJ("VALID_CASE", i)]$YEAR))==tail(sort(years), 1)))], state.multiple.year.summary)
			}
		} else {
			if (!all(content_areas %in% names(years))) {
				stop("Supplied list of years does not contain all content areas specified for summarizeSGP.")
			} else {
				tmp.years <- years[content_areas]
			}
		}
	}

	for (i in names(tmp.years)) {
		tmp.years[[i]] <- data.table(CONTENT_AREA=i, YEAR=tmp.years[[i]])
	}
	content_areas.by.years <- as.data.table(rbind.fill(tmp.years))

	if (is.null(sgp.summaries)) sgp.summaries <- summarizeSGP.config(sgp_object, "sgp.summaries")
	if (is.null(summary.groups)) summary.groups <- summarizeSGP.config(sgp_object, "summary.groups")
	if (is.null(confidence.interval.groups)) confidence.interval.groups <- summarizeSGP.config(sgp_object, "confidence.interval.groups")

	if (any(!sapply(summary.groups[["growth_only_summary"]], is.null))) {
		slot.data[,BY_GROWTH_ONLY := factor(is.na(slot.data[[my.sgp[1]]]), levels=c(FALSE, TRUE), labels=c("Students without SGP", "Students with SGP"))]
	}

	variables.for.summaries <- intersect(c(my.sgp, my.sgp.target, my.sgp.target.baseline, my.sgp.target.musu, my.sgp.target.musu.baseline,
						"ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_PRIOR", 
						"CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STAY_UP_STATUS", "CATCH_UP_KEEP_UP_STATUS_BASELINE","MOVE_UP_STAY_UP_STATUS_BASELINE",
						"SCALE_SCORE_PRIOR_STANDARDIZED", "SGP_SIMEX", "SGP_SIMEX_BASELINE",
						unique(as.character(unlist(summary.groups))),
						"YEAR_WITHIN"),
					names(slot.data)) 

	if (!is.null(sgp_object@Data_Supplementary) | "CSEM" %in% confidence.interval.groups[['TYPE']]) variables.for.summaries <- c("VALID_CASE", "ID", variables.for.summaries)

	### Define demographic subgroups and tables that will be calculated from all possible created by expand.grid

	selected.demographic.subgroups <- intersect(
		c(getFromNames("demographic"), "CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STAY_UP_STATUS", "CATCH_UP_KEEP_UP_STATUS_BASELINE", "MOVE_UP_STAY_UP_STATUS_BASELINE", 
			"ACHIEVEMENT_LEVEL_PRIOR", "HIGH_NEED_STATUS"), names(slot.data))
	if (is.null(SGPstateData[[state]][["Variable_Name_Lookup"]])) {
		selected.institution.types <- c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
	} else {
		selected.institution.types <- c(highest.level.summary.grouping, getFromNames("institution"))
	}
	selected.institution.types <- c(selected.institution.types, paste(selected.institution.types[grep("CURRENT", selected.institution.types, invert=TRUE)], "INSTRUCTOR_NUMBER", sep=", "))
	selected.summary.tables <- list()
	for (k in selected.institution.types) {
		if (length(grep("INSTRUCTOR_NUMBER", k)) > 0 | length(grep("CURRENT", k)) > 0) {
			if (length(grep("CURRENT", k)) > 0 | !"INSTRUCTOR_ENROLLMENT_STATUS" %in% names(sgp_object@Data_Supplementary[['INSTRUCTOR_NUMBER']])) {
				ENROLLMENT_STATUS_ARGUMENT <- NULL; ADD_MISSING_ARGUMENT <- TRUE
			} 
			if ("INSTRUCTOR_ENROLLMENT_STATUS" %in% names(sgp_object@Data_Supplementary[['INSTRUCTOR_NUMBER']])) {
				ENROLLMENT_STATUS_ARGUMENT <- "INSTRUCTOR_ENROLLMENT_STATUS"; ADD_MISSING_ARGUMENT <- FALSE
			}

			if (length(grep("SCHOOL", k)) > 0) {
				selected.summary.tables[[k]] <- do.call(paste, c(expand.grid(k,
						group.format("EMH_LEVEL"),
						group.format("CONTENT_AREA"),
						group.format("YEAR"),
						group.format("GRADE"),
						group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)), sep=""))
			} else {
				selected.summary.tables[[k]] <- do.call(paste, c(expand.grid(k,
						group.format("CONTENT_AREA"),
						group.format("YEAR"),
						group.format("GRADE"),
						group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)), sep=""))
			}
		} else {
			if (is.null(summary.groups[["institution_inclusion"]][[k]])) {
				ENROLLMENT_STATUS_ARGUMENT <- NULL
				ADD_MISSING_ARGUMENT <- TRUE
			} else {
				ENROLLMENT_STATUS_ARGUMENT <- summary.groups[["institution_inclusion"]][[k]]
				ADD_MISSING_ARGUMENT <- FALSE
			}

			if (length(grep("SCHOOL", k)) > 0 & !is.null(summary.groups[["institution_inclusion"]][[k]])) {
				selected.summary.tables[[k]] <- do.call(paste, c(expand.grid(k,
					group.format("EMH_LEVEL"),
					group.format("CONTENT_AREA"),
					group.format("YEAR"),
					group.format("GRADE"),
					group.format(selected.demographic.subgroups),
					group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)), sep=""))
			} else {
				selected.summary.tables[[k]] <-  do.call(paste, c(expand.grid(k,
					group.format("CONTENT_AREA"),
					group.format("YEAR"),
					group.format("GRADE"),
					group.format(selected.demographic.subgroups),
					group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)), sep=""))
			}
		}
	} ### End for k
	selected.summary.tables <- unlist(selected.summary.tables, use.names=FALSE)

	##############################################################
	###
	### Data prep and calculation of summary tables
	###
	############################################################## 

	### Loop and send to summarizeSGP_INTERNAL

	tmp.dt <- slot.data[data.table("VALID_CASE", content_areas.by.years), nomatch=0][, variables.for.summaries, with=FALSE][, highest.level.summary.grouping:=state, with=FALSE]
	if (!is.null(summary.groups[["institution_multiple_membership"]]) | "CSEM" %in% confidence.interval.groups[['TYPE']]) setkeyv(tmp.dt, getKey(slot.data))

	par.start <- startParallel(parallel.config, 'SUMMARY')

	for (j in seq(length(summary.groups[["institution_multiple_membership"]])+1)) {
		for (i in summary.groups[["institution"]]) {
			if (j == 1) {
				sgp_object@Summary[[i]] <- summarizeSGP_INTERNAL(data=tmp.dt, i)
			}
			if (j > 1) {

				### Create variable name and set up tmp.inst

				multiple.membership.variable.name <- summary.groups[["institution_multiple_membership"]][[j-1]][["VARIABLE.NAME"]]
				tmp.inst <- paste(i, multiple.membership.variable.name, sep=", ")

				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]])) {
					summary.groups[["institution_inclusion"]][[tmp.inst]] <- summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]]
					if (tmp.inst %in% confidence.interval.groups[['GROUPS']][['institution']]) {
						confidence.interval.groups[['GROUPS']][['institution_inclusion']][[tmp.inst]] <-
							summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]]
					}
				} else {
					summary.groups[["institution_inclusion"]][[tmp.inst]] <- paste(i, "ENROLLMENT_STATUS", sep="_")
					if (tmp.inst %in% confidence.interval.groups[['GROUPS']][['institution']]) {
						confidence.interval.groups[['GROUPS']][['institution_inclusion']][[tmp.inst]] <-
							paste(i, "ENROLLMENT_STATUS", sep="_")	
					}
				}
				
				summary.groups[["growth_only_summary"]][[tmp.inst]] <- "BY_GROWTH_ONLY" # Do we have an option to NOT include "BY_GROWTH_ONLY"? (would we want this?)

				### Create LONGer data and run summarizeSGP_INTERNAL
				
				tmp.dt.long <- tmp.dt[data.table(sgp_object@Data_Supplementary[[j-1]][,VALID_CASE:="VALID_CASE"], key=getKey(tmp.dt)), nomatch=0]

				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]])) {
					setnames(tmp.dt.long, summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]], "WEIGHT")
				}

				sgp_object@Summary[[i]] <- c(sgp_object@Summary[[i]], summarizeSGP_INTERNAL(tmp.dt.long, tmp.inst))
			} 
		} ### End i loop over summary.groups[["institution"]]
	} ### END j loop over multiple membership groups (if they exist)

	stopParallel(parallel.config, par.start)
	message(paste("Finished summarizeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END summarizeSGP Function
