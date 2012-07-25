`combineSGP` <- 
function(sgp_object,
		 state=NULL,
		 years=NULL,
		 content_areas=NULL,
		 sgp.percentiles=TRUE,
		 sgp.percentiles.baseline=TRUE,
		 sgp.projections.lagged=TRUE,
		 sgp.projections.lagged.baseline=TRUE,
		 max.lagged.sgp.target.years.forward=4,
                 use.cohort.for.baseline.when.missing=NULL
		 ) {

	started.at <- proc.time()
	message(paste("Started combineSGP", date()))

	tmp.messages <- NULL

	### Create slot.data from sgp_object@Data

	slot.data <- sgp_object@Data


	### Create state (if missing) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(x, tmp.name))==1)) {
			state <- c(state.abb, "DEMO", "AOB")[which(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(x, tmp.name))==1)]
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

	if (is.null(use.cohort.for.baseline.when.missing)) {
		if (is.null(SGPstateData[[state]][["SGP_Configuration"]][["use.cohort.for.baseline.when.missing"]])) {
			use.cohort.for.baseline.when.missing <- FALSE
		} else {
			use.cohort.for.baseline.when.missing <- SGPstateData[[state]][["SGP_Configuration"]][["use.cohort.for.baseline.when.missing"]]
		}
	}

	## Utility functions

	"%w/o%" <- function(x,y) x[!x %in% y]

	get.catch_up_keep_up_status_initial <- function(achievement_level_prior) {
		if (!all(levels(achievement_level_prior) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])) {
			levels(achievement_level_prior)[!levels(achievement_level_prior) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]] <- NA
		}
		levels(achievement_level_prior) <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]] 
		levels(achievement_level_prior) <- c("Catching Up", "Keeping Up")
		factor(achievement_level_prior, ordered=FALSE) ## Drop ordered attribute of factor
	}

	get.rbind.all.data <- function(data.pieces, key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")) {
		my.rbind.all <- rbind.fill(data.pieces)
		if (is.integer(sgp_object@Data[["YEAR"]])) my.rbind.all[["YEAR"]] <- as.integer(my.rbind.all[["YEAR"]])
		data.table(my.rbind.all,  VALID_CASE="VALID_CASE", key=key(slot.data))
	}

	get.percentile.names <- function(sgp.type) {
		if (sgp.type %in% c("sgp.percentiles", "sgp.percentiles.baseline")) {
			tmp.sgp.names <- names(sgp_object@SGP$SGPercentiles)
			tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGPercentiles), value=TRUE)
			if (sgp.type=="sgp.percentiles") tmp.names <- setdiff(tmp.sgp.names, tmp.baseline.names)
			if (sgp.type=="sgp.percentiles.baseline") {
				tmp.names <- tmp.baseline.names
				tmp.content_areas.diff <- setdiff(unique(sapply(strsplit(tmp.sgp.names, "[.]"), function(x) paste(x[1:2], collapse="."))), 
					unique(sapply(strsplit(tmp.baseline.names, "[.]"), function(x) paste(x[1:2], collapse="."))))
				if (use.cohort.for.baseline.when.missing & length(tmp.content_areas.diff) > 0) {
					tmp.names <- c(tmp.names, unlist(lapply(tmp.content_areas.diff, function(x) tmp.sgp.names[grep(x, tmp.sgp.names)])))
					message(c("\tNOTE: Cohort referenced SGPs being used for baseline referenced SGPs for content areas and years: ", paste(unlist(lapply(tmp.content_areas.diff, function(x) tmp.sgp.names[grep(x, tmp.sgp.names)])), collapse=", ")))
				}
			}
			if (length(tmp.names) > 0 & !is.null(years)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[2] %in% years)]
			if (length(tmp.names) > 0 & !is.null(content_areas)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[1] %in% content_areas)]
		}
		if (sgp.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) {
			tmp.lagged.names <- grep("LAGGED", names(sgp_object@SGP$SGProjections), value=TRUE)
			tmp.baseline.names <- intersect(tmp.lagged.names, grep("BASELINE", names(sgp_object@SGP$SGProjections), value=TRUE))
			if (sgp.type=="sgp.projections.lagged") tmp.names <- setdiff(tmp.lagged.names, tmp.baseline.names)
			if (sgp.type=="sgp.projections.lagged.baseline") {
				tmp.names <- tmp.baseline.names
				tmp.content_areas.diff <- setdiff(unique(sapply(strsplit(tmp.lagged.names, "[.]"), function(x) paste(x[1:2], collapse="."))), 
					unique(sapply(strsplit(tmp.baseline.names, "[.]"), function(x) paste(x[1:2], collapse="."))))
				if (use.cohort.for.baseline.when.missing & length(tmp.content_areas.diff) > 0) {
					tmp.names <- c(tmp.names, unlist(lapply(tmp.content_areas.diff, function(x) tmp.lagged.names[grep(x, tmp.lagged.names)])))
					message(c("\tNOTE: Cohort referenced lagged.projections being used for baseline referenced lagged projections for content areas and years: ", paste(unlist(lapply(tmp.content_areas.diff, function(x) tmp.lagged.names[grep(x, tmp.lagged.names)])), collapse=", ")))
				}
			}
			if (length(tmp.names) > 0 & !is.null(years)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[2] %in% years)]
			if (length(tmp.names) > 0 & !is.null(content_areas)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[1] %in% content_areas)]
		}
		return(tmp.names)
	} ## END get.percentile.names


	############################################################################
	### sgp.percentiles: Merge Cohort Referenced SGPs with student data
	############################################################################

	## Determine names of Cohort Referenced SGPs

	tmp.names <- get.percentile.names("sgp.percentiles")
	if (length(tmp.names) == 0 & sgp.percentiles) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No cohort referenced SGP results available in SGP slot. No cohort referenced SGP results will be merged.\n")
		sgp.percentiles <- FALSE
	}

	if (sgp.percentiles) { 

		tmp.list <- list()
		for (i in tmp.names) {
		tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=type.convert(unlist(strsplit(i, "[.]"))[2], as.is=TRUE),
					sgp_object@SGP[["SGPercentiles"]][[i]])
		}

		tmp.data <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))


		if (!all(names(tmp.data) %in% names(slot.data))) {
			for (i in names(tmp.data)[!names(tmp.data) %in% names(slot.data)]) suppressWarnings(slot.data[, i := tmp.data[[i]][NA][1], with=FALSE])
		}

		invisible(slot.data[tmp.data[,key(slot.data), with=FALSE], names(tmp.data) := tmp.data, with=FALSE, mult="first"])
		setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
	}


	###################################################################################
	### sgp.percentiles.baseline: Merge baseline referenced SGPs with student data
	###################################################################################

	## Determine names of Baseline Referenced SGPs

	tmp.names <- get.percentile.names("sgp.percentiles.baseline")
	if (length(tmp.names) == 0 & sgp.percentiles.baseline) {
		 tmp.messages <- c(tmp.messages, "\tNOTE: No baseline referenced SGP results available in SGP slot. No baseline referenced SGP results will be merged.\n")
		 sgp.percentiles.baseline=FALSE
	}

	if (sgp.percentiles.baseline) {

		tmp.list <- list() 
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=type.convert(unlist(strsplit(i, "[.]"))[2], as.is=TRUE),
				sgp_object@SGP[["SGPercentiles"]][[i]])

			if (is.na(unlist(strsplit(i, "[.]"))[3])) { ### If cohort referenced SGP are to be included in baseline SGP (e.g., Georgia)
				names(tmp.list[[i]])[names(tmp.list[[i]])=="SGP"] <- "SGP_BASELINE"
				if ("SGP_LEVEL" %in% names(tmp.list[[i]])) names(tmp.list[[i]])[names(tmp.list[[i]])=="SGP_LEVEL"] <- "SGP_LEVEL_BASELINE"
			}
		}

		tmp.data <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))

		if (!all(names(tmp.data) %in% names(slot.data))) {
			for (i in names(tmp.data)[!names(tmp.data) %in% names(slot.data)]) slot.data[, i := tmp.data[[i]][NA][1], with=FALSE]
		}

		invisible(slot.data[tmp.data[,key(slot.data), with=FALSE], names(tmp.data) := tmp.data, with=FALSE, mult="first"])
		setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
	}


	######################################################################################
	### Create SGP targets (Cohort and Baseline referenced) and merge with student data
	######################################################################################

	### Calculate Targets
 
	if (sgp.projections.lagged | sgp.projections.lagged.baseline) { 

		ID <- CONTENT_AREA <- YEAR <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- VALID_CASE <- NULL
		level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1


		#################################################################################
		## Cohort referenced lagged SGP targets and Catch Up Keep Up Status Variable
		#################################################################################

		## Determine names of lagged projections

		tmp.names <- get.percentile.names("sgp.projections.lagged")	
		if (length(tmp.names) == 0 & sgp.projections.lagged) {
			 tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged projection results available in SGP slot. No student growth projection targets will be produced.\n")
			 sgp.projections.lagged <- FALSE
		}

		if (sgp.projections.lagged) {

			tmp.list <- list()
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			for (i in tmp.names) {
				cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
				num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
				tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=type.convert(unlist(strsplit(i, "[.]"))[2], as.is=TRUE),
					CATCH_UP_KEEP_UP_STATUS_INITIAL=get.catch_up_keep_up_status_initial(sgp_object@SGP[["SGProjections"]][[i]][["ACHIEVEMENT_LEVEL_PRIOR"]]),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get[1:num.cols.to.get])])
			}

##			tmp_object_1 <- data.table(VALID_CASE="VALID_CASE", get.rbind.all.data(tmp.list))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]
			tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

			## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

			catch_keep_functions <- c(min, max)
			jExpression <- parse(text=paste("{catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
				grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
			tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
			setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET")
			invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["CATCH_UP_KEEP_UP_STATUS_INITIAL"]]), with=FALSE])
			setkeyv(tmp_object_2, key(slot.data))

			if (!all(names(tmp_object_2) %in% names(slot.data))) {
				for (i in names(tmp_object_2)[!names(tmp_object_2) %in% names(slot.data)]) slot.data[, i := tmp_object_2[[i]][NA][1], with=FALSE]
			}

			invisible(slot.data[tmp_object_2[,key(slot.data), with=FALSE], names(tmp_object_2) := tmp_object_2, with=FALSE, mult="first"])
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			rm(tmp.list); suppressWarnings(gc())

		} ## END if (sgp.projections.lagged)


		##############################################################################################
		### Lagged Baseline Student Growth Projection Targets and Catch Up Keep Up Variable
		##############################################################################################

		tmp.names <- get.percentile.names("sgp.projections.lagged.baseline")	
		if (length(tmp.names) == 0 & sgp.projections.lagged.baseline) {
			tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged baseline projection results available in SGP slot. No baseline referenced student growth projection targets will be produced.\n")
			sgp.projections.lagged.baseline <- FALSE
		}

		if (sgp.projections.lagged.baseline) {

			 tmp.list <- list()
			 setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			 for (i in tmp.names) {
				 cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
				 num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
				 tmp.list[[i]] <-data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=type.convert(unlist(strsplit(i, "[.]"))[2], as.is=TRUE),
					CATCH_UP_KEEP_UP_STATUS_INITIAL=get.catch_up_keep_up_status_initial(sgp_object@SGP[["SGProjections"]][[i]][,2]),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get[1:num.cols.to.get])])
		 }

		tmp_object_1 <- data.table(VALID_CASE="VALID_CASE", get.rbind.all.data(tmp.list))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

		 ## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

		catch_keep_functions <- c(min, max)
		jExpression <- parse(text=paste("{catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
			grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
		tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
		setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET_BASELINE")
		invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["CATCH_UP_KEEP_UP_STATUS_INITIAL"]]), with=FALSE])
		setkeyv(tmp_object_2, key(slot.data))

		if (!all(names(tmp_object_2) %in% names(slot.data))) {
			for (i in names(tmp_object_2)[!names(tmp_object_2) %in% names(slot.data)]) slot.data[, i := tmp_object_2[[i]][NA][1], with=FALSE]
		}

		invisible(slot.data[tmp_object_2[,key(slot.data), with=FALSE], names(tmp_object_2) := tmp_object_2, with=FALSE, mult="first"])
		setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())

	} ## End if (sgp.projections.lagged.baseline)


		#################################################
		## Create CATCH_UP_KEEP_UP_STATUS variable
		#################################################

		if ("CATCH_UP_KEEP_UP_STATUS" %in% names(slot.data)) slot.data$CATCH_UP_KEEP_UP_STATUS <- NULL 
		slot.data$CATCH_UP_KEEP_UP_STATUS <- NA

		### CATCH_UP_KEEP_UP BASED UPON SGP versus SGP_TARGET

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												slot.data[[my.sgp]] >= slot.data[[my.sgp.target]]] <- "Keep Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												slot.data[[my.sgp]] < slot.data[[my.sgp.target]]] <- "Keep Up: No"

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												slot.data[[my.sgp]] >= slot.data[[my.sgp.target]]] <- "Catch Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												slot.data[[my.sgp]] < slot.data[[my.sgp.target]]] <- "Catch Up: No"

		### CATCH_UP_KEEP_UP clean up based upon reality

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												slot.data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" &
												as.numeric(slot.data$ACHIEVEMENT_LEVEL) <= level.to.get] <- "Keep Up: No"

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												slot.data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: No" &
												as.numeric(slot.data$ACHIEVEMENT_LEVEL) > level.to.get] <- "Catch Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
												slot.data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" &
												as.numeric(slot.data$ACHIEVEMENT_LEVEL) <= level.to.get &
												slot.data$GRADE == max(slot.data$GRADE[!is.na(slot.data$SGP_TARGET)])] <- "Catch Up: No"

		slot.data$CATCH_UP_KEEP_UP_STATUS[slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
												slot.data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: No" &
												as.numeric(slot.data$ACHIEVEMENT_LEVEL) > level.to.get &
												slot.data$GRADE == max(slot.data$GRADE[!is.na(slot.data$SGP_TARGET)])] <- "Keep Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS <- as.factor(slot.data$CATCH_UP_KEEP_UP_STATUS)


	} ## END sgp.projections.lagged | sgp.projections.lagged.baseline

	setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
	sgp_object@Data <- slot.data

	message(paste(tmp.messages, paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"), sep=""))

	return(sgp_object)

} ## END combineSGP Function
