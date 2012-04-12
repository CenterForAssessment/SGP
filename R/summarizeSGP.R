`summarizeSGP` <- 
function(sgp_object,
         state=NULL,
         years=NULL,
         content_areas=NULL,
         sgp.summaries=NULL,
         summary.groups=NULL,
         confidence.interval.groups=NULL,
         parallel.config=NULL) {

	started.at <- proc.time()
	message(paste("\nStarted summarizeSGP", date()))

	### Set variables to NULL to prevent R CMD check warnings

	tmp.simulation.dt <- variable <- WEIGHT <- ENROLLMENT_STATUS <- NULL

	if (missing(sgp_object)) {
		stop("User must supply a list containing a Student slot with long data. See documentation for details.")
	}

	if (!identical(key(sgp_object@Data), c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))) setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))


	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
			state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
		}
	}

	## Set up parallel.config if NULL

	if (is.null(parallel.config)) {
		 parallel.config=list(BACKEND="FOREACH", TYPE=NA, WORKERS=list(SUMMARY=1))
	}

	## Utility Functions

	"%w/o%" <- function(x, y) x[!x %in% y]

	getFromNames <- function(x) {
		tmp.names <- sgp_object@Names[!is.na(sgp_object@Names$names.type),]
		return(tmp.names[tmp.names$names.type==x, "names.sgp"])
	}

	rbind.all <- function(.list, ...){
		if(length(.list)==1) return(.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	group.format <- function(my.group) {
		if (is.null(my.group)) {
			c("")
		} else {
			c("", unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
		}
	}

	median_na <- function(x) median(as.numeric(x), na.rm=TRUE)
	boot.median <- function(x,i) median(x[i], na.rm=TRUE)
	mean_na <- function(x, result.digits=1) round(mean(as.numeric(x), na.rm=TRUE), digits=result.digits)
	num_non_missing <- function(x) sum(!is.na(as.numeric(x)))
	median_sgp_standard_error <- function(x) 1.25*sd(x, na.rm=TRUE)/sqrt(sum(!is.na(as.numeric(x))))

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

	boot.sgp <- function(dat, conf.quantiles=c(0.025, 0.975), nboot=100) {
		out <- numeric(); CI <- c(NA,NA)
		if (sum(is.na(dat)) != length(dat)) {
			for (j in 1:nboot) {
				foo <- sample(dat,length(dat), replace=TRUE)
				out[j] <- boot.median(foo)
			}
			CI <- as.numeric(quantile(out, conf.quantiles, na.rm=TRUE))
		}
		CI
	}

	sgpSummary <- function(data, sgp.groups.to.summarize, produce.confidence.interval) {
		SGP_SIM <- V1 <- .SD <- NULL ## To prevent R CMD check warning
		if (produce.confidence.interval) {
			if ("Bootstrap" %in% confidence.interval.groups$TYPE) {
				require(boot)
				tmp.list <- list()
				tmp.quantiles <- paste("c(", paste(confidence.interval.groups$QUANTILES, collapse=", "), ")", sep="")
				for (i in confidence.interval.groups$VARIABLES) {
					tmp.list[[paste("MEDIAN_", i, "_QUANTILES", sep="")]] <- paste("boot.sgp(", i, ", ", tmp.quantiles, ")", sep="")
				}
				tmp.sgp.summaries <- c(sgp.summaries, tmp.list)
				sgp.summaries.names <- c(unlist(strsplit(names(sgp.summaries), "[.]")), paste("MEDIAN_SGP_", confidence.interval.groups$QUANTILES, "_CONFIDENCE_BOUND", sep="")) 
			} 
			if ("CSEM" %in% confidence.interval.groups$TYPE) {
				tmp.sgp.summaries <- sgp.summaries
				sgp.summaries.names <- c(unlist(strsplit(names(sgp.summaries), "[.]")), paste("MEDIAN_SGP_", confidence.interval.groups$QUANTILES, "_CONFIDENCE_BOUND", sep=""))
			}
		} else {
			tmp.sgp.summaries <- sgp.summaries
			sgp.summaries.names <- unlist(strsplit(names(sgp.summaries), "[.]"))
		}
 
		ListExpr <- parse(text=paste("as.list(c(", paste(unlist(tmp.sgp.summaries), collapse=", "),"))",sep="")) 
		ByExpr <- parse(text=paste("list(", paste(sgp.groups.to.summarize, collapse=", "), ")", sep=""))
		tmp <- data[, eval(ListExpr), by=eval(ByExpr)]
		if (produce.confidence.interval & "CSEM" %in% confidence.interval.groups$TYPE) {
			SIM_ByExpr1 <- parse(text=paste("list(", paste(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))
				[!(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))) %in% key(data)], collapse=", "), 
				", ", paste(names(tmp.simulation.dt)[grep("SGP_SIM_", names(tmp.simulation.dt))], collapse=", "), ")", sep=""))
			SIM_ByExpr2 <- parse(text=paste("list(", paste(sgp.groups.to.summarize, collapse=", "), ")", sep=""))
				tmp.sim <- data[tmp.simulation.dt, eval(SIM_ByExpr1)][, -(1:2), with=FALSE][,
				lapply(.SD, median_na), by=eval(SIM_ByExpr2)][, 
				as.list(round(apply(.SD, 1, quantile, probs=confidence.interval.groups$QUANTILES))), by=eval(SIM_ByExpr2)]
			tmp <- data.table(merge.data.frame(tmp, tmp.sim, by = unlist(strsplit(as.character(sgp.groups.to.summarize), ", ")),all=TRUE))
		}
		setnames(tmp, (dim(tmp)[2]-length(sgp.summaries.names)+1):dim(tmp)[2], sgp.summaries.names)
		message(paste("\tFinished with", sgp.groups.to.summarize))
		return(tmp)
	} ### END sgpSummary function

	combineSims <- function(sgp_object) {
		tmp.list <- list()
		tmp.names <- names(sgp_object@SGP[["Simulated_SGPs"]]) 
		for (i in tmp.names) {
			tmp.list[[i]] <- data.frame(sgp_object@SGP[["Simulated_SGPs"]][[i]],
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]))
		}
 
		data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), key=key(data))
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
				MEDIAN_SGP="median_na(SGP)",
				MEDIAN_SGP_COUNT="num_non_missing(SGP)",
				PERCENT_AT_ABOVE_PROFICIENT=paste("percent_in_category(ACHIEVEMENT_LEVEL, ", 
					get.expression(proficient.achievement.levels), ", ", get.expression(all.achievement.levels), ")",sep=""),
				PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)",
				PERCENT_AT_ABOVE_PROFICIENT_PRIOR=paste("percent_in_category(ACHIEVEMENT_LEVEL_PRIOR, ", 
					get.expression(proficient.achievement.levels), ", ", get.expression(all.achievement.levels), ")",sep=""),
				PERCENT_AT_ABOVE_PROFICIENT_COUNT_PRIOR="num_non_missing(ACHIEVEMENT_LEVEL_PRIOR)",
				MEDIAN_SGP_STANDARD_ERROR="median_sgp_standard_error(SGP)")

				if ("SGP_TARGET" %in% names(sgp_object@Data)) {
					tmp.sgp.summaries <- c(tmp.sgp.summaries, MEDIAN_SGP_TARGET="median_na(SGP_TARGET)",  MEDIAN_SGP_TARGET_COUNT="num_non_missing(SGP_TARGET)")
				}

			return(tmp.sgp.summaries)
		}

		if (config.type=="summary.groups") {
			tmp.summary.groups <- list(
				institution=c("STATE", getFromNames("institution")),
				content=getFromNames("content"),
				time=getFromNames("time"),
				institution_type=getFromNames("institution_type"),
				institution_level=getFromNames("institution_level"),
				institution_multiple_membership=get.multiple.membership(sgp_object@Names[!is.na(sgp_object@Names$names.sgp),]),
				demographic=c(getFromNames("demographic"), "CATCH_UP_KEEP_UP_STATUS", "ACHIEVEMENT_LEVEL_PRIOR"))

				for (i in tmp.summary.groups[["institution"]]) {
					tmp.summary.groups[["institution_inclusion"]][[i]] <- getFromNames("institution_inclusion")[
						grep(strsplit(i, "_")[[1]][1], getFromNames("institution_inclusion"))]
					tmp.summary.groups[["growth_only_summary"]][[i]] <- "BY_GROWTH_ONLY"
				}
				tmp.summary.groups[["institution_inclusion"]] <- as.list(tmp.summary.groups[["institution_inclusion"]])
				tmp.summary.groups[["growth_only_summary"]] <- as.list(tmp.summary.groups[["growth_only_summary"]])

				return(tmp.summary.groups)
		}
		
		if (config.type=="confidence.interval.groups") {
			tmp.confidence.interval.groups <- list(
				TYPE="Bootstrap",
				VARIABLES="SGP",
				QUANTILES=c(0.025, 0.975),
				GROUPS=list(
					institution="SCHOOL_NUMBER",
					content="CONTENT_AREA",
					time="YEAR",
					institution_type="EMH_LEVEL",
					institution_level=NULL,
					demographic=NULL,
					institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS"),
					growth_only_summary=list(STATE="BY_GROWTH_ONLY", DISTRICT_NUMBER="BY_GROWTH_ONLY", SCHOOL_NUMBER="BY_GROWTH_ONLY")))

			return(tmp.confidence.interval.groups)
		}
	} ### END sgpSummarize.config

	get.multiple.membership <- function(names.df) {
		tmp.names <- list()
		tmp.number.variables <- unique(suppressWarnings(as.numeric(sapply(strsplit(
			names.df[["names.type"]][grep("institution_multiple_membership", names.df[["names.type"]])], "_"), 
			function(x) tail(x,1)))) %w/o% NA)
		if (length(tmp.number.variables)==0) {
			tmp.names <- NULL
		} else {
			for (i in seq(tmp.number.variables)) {
				tmp.variable.names <- as.character(names.df[names.df$names.type==paste("institution_multiple_membership_", i, sep=""), "names.sgp"]) %w/o% NA

				tmp.length <- sum(paste("institution_multiple_membership_", i, sep="")==names.df[["names.type"]], na.rm=TRUE)
				tmp.weight.length <- sum(paste("institution_multiple_membership_", i, "_weight", sep="")==names.df[["names.type"]], na.rm=TRUE)
				tmp.inclusion.length <- sum(paste("institution_multiple_membership_", i, "_inclusion", sep="")==names.df[["names.type"]], na.rm=TRUE)

				if ((tmp.weight.length != 0 & tmp.weight.length != tmp.length) | (tmp.inclusion.length != 0 & tmp.inclusion.length != tmp.length)) {
					stop("\tNOTE: The same (non-zero) number of inclusion/weight Multiple Membership variables must exist as the number of multiple Membership variables.")
				}

				if (tmp.weight.length == 0) {
					tmp.weights <- NULL
				} else {
					tmp.weights <- as.character(names.df[names.df$names.type==paste("institution_multiple_membership_", i, "_weight", sep=""), "names.sgp"]) %w/o% NA
				}
				
				if (tmp.inclusion.length != 0 & tmp.inclusion.length != tmp.length) {
					stop("\tNOTE: The same number (or zero) of Multiple membership inclusion variables must exist as the number of multiple membership variables.")
				}
				if (tmp.inclusion.length == 0) {
					tmp.inclusion <- NULL 
				} else {
					tmp.inclusion <- as.character(names.df[names.df$names.type==paste("institution_multiple_membership_", i, "_inclusion", sep=""), "names.sgp"]) %w/o% NA
				}

				tmp.names[[i]] <- list(VARIABLE.NAMES=tmp.variable.names, WEIGHTS=tmp.weights, ENROLLMENT_STATUS=tmp.inclusion)
			}
		}
		return(tmp.names)
	} ### END get.multiple.membership

	summarizeSGP_INTERNAL <- function(data, i) {
		
		tmp.summary <- list()

		if (!is.null(confidence.interval.groups) & "CSEM" %in% confidence.interval.groups$TYPE) {
			tmp.simulation.dt <- combineSims(sgp_object); gc()
		} 

		### Create summary tables
		
		sgp.groups <- do.call(paste, c(expand.grid(i,
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_type"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["demographic"]]),
			group.format(summary.groups[["growth_only_summary"]][[i]])), sep=""))

		if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
			ci.groups <- do.call(paste, c(expand.grid(i,
				group.format(confidence.interval.groups[["GROUPS"]][["content"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["time"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_type"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_level"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_inclusion"]][[i]]),
				group.format(confidence.interval.groups[["GROUPS"]][["demographic"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["growth_only_summary"]][[i]])), sep=""))
		}

		if(par.start$par.type=="FOREACH") {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
	  			j <- k <- NULL ## To prevent R CMD check warnings
	  			tmp.summary <- foreach(j=iter(sgp.groups), k=iter(sgp.groups %in% ci.groups), 
	  				.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE) %dopar% {
						return(sgpSummary(data, j, k))
				}
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			} else {
				j <- k <- NULL ## To prevent R CMD check warnings
				tmp.summary <- foreach(j=iter(sgp.groups), k=iter(rep(FALSE, length(sgp.groups))), 
					.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE) %dopar% {
						return(sgpSummary(data, j, k))
				}
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			}
		} # END FOREACH flavor

		if(par.start$par.type=="SNOW") {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
	  			j <- k <- NULL ## To prevent R CMD check warnings
	  			summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], sgp.groups[x] %in% ci.groups))
	  			tmp.summary <- clusterApplyLB(par.start$internal.cl, summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2]))))
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			} else {
				j <- k <- NULL ## To prevent R CMD check warnings
				summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], FALSE))
	  			tmp.summary <- clusterApplyLB(par.start$internal.cl, summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2]))))
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			}
			# if (is.null(parallel.config[['CLUSTER.OBJECT']]))	 stopCluster(internal.cl)
		} # END 'SNOW' Flavor
 
		if (par.start$par.type=="MULTICORE") {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
	  			j <- k <- NULL ## To prevent R CMD check warnings
	  			summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], sgp.groups[x] %in% ci.groups))
	  			tmp.summary <- mclapply(summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2]))), mc.cores=par.start$workers, mc.preschedule=FALSE)
				names(tmp.summary) <- gsub(", ", "__", sgp.groups)
			} else {
				j <- k <- NULL ## To prevent R CMD check warnings
				summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], FALSE))
	  			tmp.summary <- mclapply(summary.iter, 
	  				function(iter) sgpSummary(data, iter[1], eval(parse(text=iter[2]))), mc.cores=par.start$workers, mc.preschedule=FALSE)	
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
		content_areas <- unique(sgp_object@Data["VALID_CASE"]$CONTENT_AREA)
	}

	if (is.null(SGPstateData[[state]][["SGP_Configuration"]][["state.multiple.year.summary"]])) {	
		state.multiple.year.summary <- 3
	} else {
		state.multiple.year.summary <- SGPstateData[[state]][["SGP_Configuration"]][["state.multiple.year.summary"]]
	}
	tmp.years <- list()
	if (is.null(years)) {
		for (i in content_areas) {
			tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[J("VALID_CASE", i)]$YEAR), state.multiple.year.summary))
		}
	} else {
		if (!is.list(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- years 
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
		tmp.years[[i]] <- data.table(i, tmp.years[[i]])
	}
	content_areas.by.years <- rbind.all(tmp.years)

	if (is.null(sgp.summaries)) sgp.summaries <- summarizeSGP.config(sgp_object, "sgp.summaries")
	if (is.null(summary.groups)) summary.groups <- summarizeSGP.config(sgp_object, "summary.groups")
	if (is.null(confidence.interval.groups)) confidence.interval.groups <- summarizeSGP.config(sgp_object, "confidence.interval.groups")

	if (any(!sapply(summary.groups[["growth_only_summary"]], is.null))) {
		sgp_object@Data[["BY_GROWTH_ONLY"]] <- factor(is.na(sgp_object@Data$SGP), levels=c(FALSE, TRUE), labels=c("Students without SGP", "Students with SGP"))
	}

	variables.for.summaries <- intersect(c("SGP", "SGP_TARGET", "ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_PRIOR", unique(as.character(unlist(summary.groups)))),
					names(sgp_object@Data)) 


	##############################################################
	###
	### Data prep and calculation of summary tables
	###
	############################################################## 

	### Loop and send to summarizeSGP_INTERNAL

	tmp.dt <- data.table(STATE=state, sgp_object@Data[J("VALID_CASE", content_areas.by.years)])[, variables.for.summaries[!is.na(variables.for.summaries)], with=FALSE]

	par.start <- startParallel(parallel.config, 'SUMMARY')

	for (j in seq(length(summary.groups[["institution_multiple_membership"]])+1)) {
		for (i in summary.groups[["institution"]]) {
			if (j == 1) {
				sgp_object@Summary[[i]] <- summarizeSGP_INTERNAL(data=tmp.dt, i)
			}
			if (j > 1) {

				### Create variable name to be used

				multiple.membership.variable.name <- 
					paste(head(unlist(strsplit(summary.groups[["institution_multiple_membership"]][[j-1]][["VARIABLE.NAMES"]][1], "_")), -1), 
						collapse="_")

				### Aggregations will occur by this new institution_level variable

				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]])) {
					tmp.inst <- paste(i, multiple.membership.variable.name, "ENROLLMENT_STATUS", sep=", ")
				} else tmp.inst <- paste(i, multiple.membership.variable.name, sep=", ")
				

				### Reshape data using melt

				tmp.dt.long <- data.table(melt(as.data.frame(tmp.dt), 
					measure.vars=summary.groups[["institution_multiple_membership"]][[j-1]][["VARIABLE.NAMES"]], 
					value.name=multiple.membership.variable.name))
				invisible(tmp.dt.long[, variable := NULL])
				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]])) {
					invisible(tmp.dt.long[, WEIGHT := melt(as.data.frame(tmp.dt[, 
						summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]], with=FALSE]), 
						measure.vars=summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]])[,2]])
				}
				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]])) {
					invisible(tmp.dt.long[, ENROLLMENT_STATUS := melt(as.data.frame(tmp.dt[, 
						summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]], with=FALSE]), 
						measure.vars=summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]])[,2]])
					summary.groups[["institution_inclusion"]][[tmp.inst]] <- "ENROLLMENT_STATUS"
				}
				# if (par.start$par.type=="SNOW") clusterExport(par.start$internal.cl, "tmp.dt.long") # Don't think we need this...
				summary.groups[["growth_only_summary"]][[tmp.inst]] <- "BY_GROWTH_ONLY" # Do we have an option to NOT include "BY_GROWTH_ONLY"? (would we want this?)
				sgp_object@Summary[[i]] <- c(sgp_object@Summary[[i]], summarizeSGP_INTERNAL(tmp.dt.long, tmp.inst))
			} 
		} ### End i loop over summary.groups[["institution"]]
	} ### END j loop over multiple membership groups (if they exist)

	stopParallel(parallel.config, par.start)

	## NULL out BY_GROWTH_ONLY
	if (any(!sapply(summary.groups[["growth_only_summary"]], is.null))) {
		sgp_object@Data[["BY_GROWTH_ONLY"]] <- NULL
	}

	message(paste("Finished summarizeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END summarizeSGP Function
