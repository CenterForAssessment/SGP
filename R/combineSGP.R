`combineSGP` <- 
function(sgp_object,
		 state,
		 years,
		 content_areas,
		 sgp.percentiles=TRUE,
		 sgp.percentiles.baseline=TRUE,
		 sgp.projections.lagged=TRUE,
		 sgp.projections.lagged.baseline=TRUE,
		 max.lagged.sgp.target.years.forward=4
		 ) {

	started.at <- proc.time()
	message(paste("Started combineSGP", date()))

	tmp.messages <- NULL

	### Create state (if missing) from sgp_object (if possible)

	if (missing(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
			state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
		} else {
			tmp.messages <- c(tmp.messages, "\tNOTE: argument 'state' required for target SGP calculation. Target SGPs will not be calculated.\n")
			sgp.projections.lagged <- sgp.projections.lagged.baseline <- FALSE
		}
	}

	## Adjust arguments based upon state being cohort referenced, baseline referenced, or both:

	if (!is.null(SGPstateData[[state]][["Growth"]][["System_Type"]])) {
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort Referenced") {
			sgp.projections.lagged.baseline <- FALSE
			my.sgp <- "SGP"
			my.sgp.target <- "SGP_TARGET"
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Baseline Referenced") {
			sgp.projections.lagged <- FALSE 
			my.sgp <- "SGP_BASELINE"
			my.sgp.target <- "SGP_TARGET_BASELINE"
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort and Baseline Referenced") {
			my.sgp <- "SGP"
			my.sgp.target <- "SGP_TARGET"
		}
	}

	## Utility functions

	"%w/o%" <- function(x,y) x[!x %in% y]

	rbind.all <- function(.list, names.not.equal=FALSE, ...) {
		if(length(.list)==1) return(.list[[1]])
		if (names.not.equal) {
			Recall(c(list(rbind.fill(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), names.not.equal=TRUE, ...)
		} else {
			Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
		}
	}

	get.catch_up_keep_up_status_initial <- function(achievement_level_prior) {
		if (!all(levels(achievement_level_prior) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])) {
			levels(achievement_level_prior)[!levels(achievement_level_prior) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]] <- NA
		}
		levels(achievement_level_prior) <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]] 
		levels(achievement_level_prior) <- c("Catching Up", "Keeping Up")
		factor(achievement_level_prior, ordered=FALSE) ## Drop ordered attribute of factor
	}


	#######################################################
	### Merge Cohort Referenced SGPs with student data
	#######################################################

	 ## Determine years and content_areas if not supplied

		tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGPercentiles))
		if (length(tmp.baseline.names) > 0) {
		 tmp.names <- names(sgp_object@SGP$SGPercentiles)[-tmp.baseline.names]
		} else {
		 tmp.names <- names(sgp_object@SGP$SGPercentiles)
		}
		if (length(tmp.names) > 0 & !missing(years)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[2] %in% years)]
		if (length(tmp.names) > 0 & !missing(content_areas)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[1] %in% content_areas)]

		if (length(tmp.names) == 0 & sgp.percentiles) {
		 tmp.messages <- c(tmp.messages, "\tNOTE: No cohort referenced SGP results available in SGP slot. No cohort referenced SGP results will be merged.\n")
		 sgp.percentiles <- FALSE
		}
	
	if (sgp.percentiles) { 

	## Determine years and content_areas if not supplied

		tmp.list <- list() 
		for (i in tmp.names) {
		tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
									YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
									sgp_object@SGP[["SGPercentiles"]][[i]])
		}

		tmp.list.names <- lapply(tmp.list, names)
		if (length(tmp.list.names) > 1) {
		 tmp.names.not.equal <- !all(sapply(seq(length(tmp.list.names)-1), function(x,i) identical(x[[i]], x[[i+1]]), x=tmp.list.names))
		} else { 
		 tmp.names.not.equal=FALSE
		}

		if (!"SGP" %in% names(sgp_object@Data)) {
			sgp_object@Data <- data.table(rbind.all(tmp.list, tmp.names.not.equal), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
				key=key(sgp_object@Data))[sgp_object@Data]
		} else {
			my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
			sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
				data.table(rbind.all(tmp.list, tmp.names.not.equal), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), key=key(sgp_object@Data))[
				data.table(sgp_object@Data[my.lookup, names(sgp_object@Data) %w/o% ((names(tmp.list[[1]]) %w/o% c("CONTENT_AREA", "YEAR", "ID"))), with=FALSE], 
					key=key(sgp_object@Data))][, names(sgp_object@Data), with=FALSE]
		}
		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
	}


	#######################################################
	### Merge baseline referenced SGPs with student data
	#######################################################

		## Determine years and content_areas if not supplied

		tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGPercentiles), value=TRUE)
		if (length(tmp.baseline.names) > 0) {
			 tmp.names <- tmp.baseline.names
			 if (length(tmp.names) > 0 & !missing(years)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[2] %in% years)]
			 if (length(tmp.names) > 0 & !missing(content_areas)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[1] %in% content_areas)]
		}
		if (length(tmp.baseline.names) == 0 & sgp.percentiles.baseline) {
			 tmp.messages <- c(tmp.messages, "\tNOTE: No baseline referenced SGP results available in SGP slot. No baseline referenced SGP results will be merged.\n")
			 sgp.percentiles.baseline=FALSE
		}

	if (sgp.percentiles.baseline) {

		tmp.list <- list() 
		for (i in tmp.names) {
		tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
									YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
									sgp_object@SGP[["SGPercentiles"]][[i]])
		setnames(tmp.list[[i]], 4:dim(tmp.list[[i]])[2], paste(names(tmp.list[[i]])[4:dim(tmp.list[[i]])[2]], "BASELINE", sep="_"))
		}

		if (!"SGP_BASELINE" %in% names(sgp_object@Data)) {
			sgp_object@Data <- data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
				key=key(sgp_object@Data))[sgp_object@Data]
		} else {
			my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
			sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
				data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), key=key(sgp_object@Data))[
				data.table(sgp_object@Data[my.lookup, names(sgp_object@Data) %w/o% ((names(tmp.list[[1]]) %w/o% c("CONTENT_AREA", "YEAR", "ID"))), with=FALSE],
					key=key(sgp_object@Data))][, names(sgp_object@Data), with=FALSE]
		}
		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
	}


	######################################################################################
	### Create SGP targets (Cohort and Baseline referenced) and merge with student data
	######################################################################################

		### Check that objects exist and correct arguments as necessary

		tmp.lagged.names <- grep("LAGGED", names(sgp_object@SGP$SGProjections))
		tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGProjections))
		tmp.names.lagged <- names(sgp_object@SGP$SGProjections)[tmp.lagged.names %w/o% tmp.baseline.names]
		tmp.names.lagged.baseline <- names(sgp_object@SGP$SGProjections)[intersect(tmp.lagged.names, tmp.baseline.names)]
		if (length(tmp.names.lagged) > 0) {
			 if (length(tmp.names.lagged) > 0 & !missing(years)) tmp.names.lagged <- tmp.names.lagged[sapply(strsplit(tmp.names.lagged, "[.]"), function(x) x[2] %in% years)]
			 if (length(tmp.names.lagged) > 0 & !missing(content_areas)) tmp.names.lagged <- tmp.names.lagged[sapply(strsplit(tmp.names.lagged, "[.]"), function(x) x[1] %in% content_areas)]
		}
		if (length(tmp.names.lagged) == 0 & sgp.projections.lagged) {
			 tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged projection results available in SGP slot. No student growth projection targets will be produced.\n")
			 sgp.projections.lagged <- FALSE
		}
		if (length(tmp.names.lagged.baseline) > 0) {
			 if (length(tmp.names.lagged.baseline) > 0 & !missing(years)) tmp.names.lagged.baseline <- tmp.names.lagged.baseline[sapply(strsplit(tmp.names.lagged.baseline, "[.]"), function(x) x[2] %in% years)]
			 if (length(tmp.names.lagged.baseline) > 0 & !missing(content_areas)) tmp.names.lagged.baseline <- tmp.names.lagged.baseline[sapply(strsplit(tmp.names.lagged.baseline, "[.]"), function(x) x[1] %in% content_areas)]
		}
		if (length(tmp.names.lagged.baseline) == 0 & sgp.projections.lagged.baseline) {
			 tmp.messages <- c(tmp.messages, "\tNOTE: No baseline SGP lagged projection results available in SGP slot. No baseline referenced student growth projection targets will be produced.\n")
			 sgp.projections.lagged.baseline <- FALSE
		}


	### Calculate Targets
 
		if (sgp.projections.lagged | sgp.projections.lagged.baseline) { 

		ID <- CONTENT_AREA <- YEAR <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- NULL ## DONE to AVOID warnings during R CMD check
		level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1

	#################################################################################
	## Cohort referenced lagged SGP targets and Catch Up Keep Up Status Variable
	#################################################################################

	if (sgp.projections.lagged) {

		tmp.list <- list()
		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		for (i in tmp.names.lagged) {
		cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
		num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
		tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
								 YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
								 CATCH_UP_KEEP_UP_STATUS_INITIAL=get.catch_up_keep_up_status_initial(sgp_object@SGP[["SGProjections"]][[i]][["ACHIEVEMENT_LEVEL_PRIOR"]]),
								 sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get[1:num.cols.to.get])])
		}

		tmp_object_1 <- data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), rbind.all(tmp.list))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

		## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

		VALID_CASE <- NULL
		catch_keep_functions <- c(min, max)
		jExpression <- parse(text=paste("{catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
			grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
		tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
		setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET")
		invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["CATCH_UP_KEEP_UP_STATUS_INITIAL"]]), with=FALSE])
		setkeyv(tmp_object_2, key(sgp_object@Data))

		if (!"SGP_TARGET" %in% names(sgp_object@Data)) {
			 sgp_object@Data <- tmp_object_2[sgp_object@Data]
		} else {
			my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
			sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- tmp_object_2[data.table(sgp_object@Data[my.lookup, 
				names(sgp_object@Data) %w/o% ((names(tmp_object_2) %w/o% c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))), with=FALSE],
					key=key(sgp_object@Data))][, names(sgp_object@Data), with=FALSE]
		}
		
		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
 } ## END if (sgp.projections.lagged)


	##############################################################################################
	### Lagged Baseline Student Growth Projection Targets and Catch Up Keep Up Variable
	##############################################################################################

	if (sgp.projections.lagged.baseline) {

		 tmp.list <- list()
		 setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		 for (i in tmp.names.lagged.baseline) {
			 cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
			 num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
			 tmp.list[[i]] <-data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
										YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
										CATCH_UP_KEEP_UP_STATUS_INITIAL=get.catch_up_keep_up_status_initial(sgp_object@SGP[["SGProjections"]][[i]][,2]),
										sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get[1:num.cols.to.get])])
		 }

		tmp_object_1 <- data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), rbind.all(tmp.list))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

		 ## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

		VALID_CASE <- NULL
		catch_keep_functions <- c(min, max)
		jExpression <- parse(text=paste("{catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
		 grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
		tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
		setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET_BASELINE")
		invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["CATCH_UP_KEEP_UP_STATUS_INITIAL"]]), with=FALSE])
		setkeyv(tmp_object_2, key(sgp_object@Data))

		if (!"SGP_TARGET_BASELINE" %in% names(sgp_object@Data)) {
			 sgp_object@Data <- tmp_object_2[sgp_object@Data]
		} else {
			 my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
			 sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
				tmp_object_2[data.table(sgp_object@Data[my.lookup,
				names(sgp_object@Data) %w/o% ((names(tmp_object_2) %w/o% c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))), with=FALSE],
					key=key(sgp_object@Data))][, names(sgp_object@Data), with=FALSE]
		}

		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
	} ## End if (sgp.projections.lagged.baseline)


		#################################################
		## Create CATCH_UP_KEEP_UP_STATUS variable
		#################################################

		if ("CATCH_UP_KEEP_UP_STATUS" %in% names(sgp_object@Data)) sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- NULL 
		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- NA

		### CATCH_UP_KEEP_UP BASED UPON SGP versus SGP_TARGET

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												sgp_object@Data[[my.sgp]] >= sgp_object@Data[[my.sgp.target]]] <- "Keep Up: Yes"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												sgp_object@Data[[my.sgp]] < sgp_object@Data[[my.sgp.target]]] <- "Keep Up: No"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												sgp_object@Data[[my.sgp]] >= sgp_object@Data[[my.sgp.target]]] <- "Catch Up: Yes"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												sgp_object@Data[[my.sgp]] < sgp_object@Data[[my.sgp.target]]] <- "Catch Up: No"

		### CATCH_UP_KEEP_UP clean up based upon reality

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" &
												as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) <= level.to.get] <- "Keep Up: No"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: No" &
												as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) > level.to.get] <- "Catch Up: Yes"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" &
												as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) <= level.to.get &
												sgp_object@Data$GRADE == max(sgp_object@Data$GRADE[!is.na(sgp_object@Data$SGP_TARGET)])] <- "Catch Up: No"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: No" &
												as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) > level.to.get &
												sgp_object@Data$GRADE == max(sgp_object@Data$GRADE[!is.na(sgp_object@Data$SGP_TARGET)])] <- "Keep Up: Yes"

		sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- as.factor(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS)


	} ## END sgp.projections.lagged | sgp.projections.lagged.baseline

	setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

	message(paste(tmp.messages, paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"), sep=""))
	return(sgp_object)

} ## END combineSGP Function
