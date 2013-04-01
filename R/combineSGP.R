`combineSGP` <- 
function(
	sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	sgp.percentiles=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	max.sgp.target.years.forward=3,
	update.all.years=FALSE) {

	started.at <- proc.time()
	message(paste("Started combineSGP", date()))

	ID <- CONTENT_AREA <- YEAR <- GRADE <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- MOVE_UP_STAY_UP_STATUS_INITIAL <- VALID_CASE <- NULL
	MOVE_UP_STAY_UP_STATUS <- CATCH_UP_KEEP_UP_STATUS <- ACHIEVEMENT_LEVEL_PRIOR <- NULL

	tmp.messages <- NULL

	### Create slot.data from sgp_object@Data

	slot.data <- copy(sgp_object@Data)


	### Create state (if missing) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "combineSGP")
        }

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                tmp.name.position <- sapply(c(state.name, "AOB", "DEMONSTRATION"), function(x) regexpr(toupper(x), tmp.name))
                if (any(tmp.name.position!=-1)) {
                        state <- c(state.abb, "AOB", "DEMO")[which(names(sort(tmp.name.position[tmp.name.position!=-1])[1])==c(state.name, "AOB", "DEMONSTRATION"))]
		} else {
			tmp.messages <- c(tmp.messages, "\tNOTE: argument 'state' required for target SGP calculation. Target SGPs will not be calculated.\n")
			sgp.projections.lagged <- sgp.projections.lagged.baseline <- FALSE
		}
	}

	## Adjust arguments based upon state being cohort referenced, baseline referenced, or both:

	if (!is.null(SGPstateData[[state]][["Growth"]][["System_Type"]])) {
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort Referenced") {
			target.type <- "sgp.projections.lagged"
			my.sgp <- "SGP"
			my.sgp.target <- paste("SGP_TARGET", max.sgp.target.years.forward, "YEAR", sep="_")
			my.sgp.target.move.up.stay.up <- paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, "YEAR", sep="_")
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Baseline Referenced") {
			target.type <- "sgp.projections.lagged.baseline"
			my.sgp <- "SGP_BASELINE"
			my.sgp.target <- paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, "YEAR", sep="_")
			my.sgp.target.move.up.stay.up <- paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", max.sgp.target.years.forward, "YEAR", sep="_")
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort and Baseline Referenced") {
			target.type <- c("sgp.projections.lagged", "sgp.projections.lagged.baseline")
			my.sgp <- "SGP"
			my.sgp.target <- paste("SGP_TARGET", max.sgp.target.years.forward, "YEAR", sep="_")
			my.sgp.target.move.up.stay.up <- paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, "YEAR", sep="_")
		}
	}

	catch_keep_move_functions <- c(min, max)


	## Utility functions

	"%w/o%" <- function(x,y) x[!x %in% y]


	############################################################################
	### Check update.all.years
	############################################################################

	if (update.all.years) {
		variables.to.null.out <- c("SGP", "SGP_SIMEX", "SGP_LEVEL", "SGP_STANDARD_ERROR", "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SGP_BASELINE", "SGP_LEVEL_BASELINE", 
					   "SGP_TARGET", "SGP_TARGET_MU", "SGP_TARGET_MOVE_UP_STAY_UP", "SGP_TARGET_MOVE_UP_STAY_UP_BASELINE", "ACHIEVEMENT_LEVEL_PRIOR", 
					   "CATCH_UP_KEEP_UP_STATUS_INITIAL", "SGP_TARGET_BASELINE", "CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STATUS", "MOVE_UP_STAY_UP_STATUS",
					   "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE", "SGP_BASELINE_STANDARD_ERROR",
					   grep("SGP_ORDER", names(slot.data), value=TRUE), grep("SGP_BASELINE_ORDER", names(slot.data), value=TRUE),
					   grep("PERCENTILE_CUT", names(slot.data), value=TRUE), grep("CONFIDENCE_BOUND", names(slot.data), value=TRUE),
					   paste("SGP_TARGET", max.sgp.target.years.forward, "YEAR", sep="_"), paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, "YEAR", sep="_"), 
					   paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, "YEAR", sep="_"), paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", max.sgp.target.years.forward, "YEAR", sep="_"))

		for (tmp.variables.to.null.out in intersect(names(slot.data), variables.to.null.out)) {
			slot.data[,tmp.variables.to.null.out:=NULL, with=FALSE]
		}
	}


	############################################################################
	### sgp.percentiles: Merge Cohort Referenced SGPs with student data
	############################################################################

	## Determine names of Cohort Referenced SGPs

	tmp.names <- getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.percentiles")
	if (length(tmp.names) == 0 & sgp.percentiles) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No cohort referenced SGP results available in SGP slot. No cohort referenced SGP results will be merged.\n")
		sgp.percentiles <- FALSE
	}

	if (sgp.percentiles) { 

		tmp.list <- list()
		for (i in tmp.names) {
		tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=unlist(strsplit(i, "[.]"))[2],
					sgp_object@SGP[["SGPercentiles"]][[i]])
		}

		tmp.data <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))

		if (any(duplicated(tmp.data))) {
			tmp.data <- getPreferredSGP(tmp.data, state)
		}

		if (length(intersect(names(slot.data), names(tmp.data)) %w/o% key(slot.data))==0) {
			slot.data <- tmp.data[slot.data]
		} else {
			variables.to.merge <- names(tmp.data) %w/o% key(slot.data)
			for (tmp.merge.variable in variables.to.merge) {
				slot.data[tmp.data[,key(slot.data), with=FALSE], tmp.merge.variable := tmp.data[[tmp.merge.variable]], with=FALSE, nomatch=0]
			}
		}

		setkeyv(slot.data, getKey(slot.data))
		rm(tmp.list); suppressMessages(gc())
	}


	###################################################################################
	### sgp.percentiles.baseline: Merge baseline referenced SGPs with student data
	###################################################################################

	## Determine names of Baseline Referenced SGPs

	tmp.names <- getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.percentiles.baseline")
	if (length(tmp.names) == 0 & sgp.percentiles.baseline) {
		 tmp.messages <- c(tmp.messages, "\tNOTE: No baseline referenced SGP results available in SGP slot. No baseline referenced SGP results will be merged.\n")
		 sgp.percentiles.baseline <- FALSE
	}

	if (sgp.percentiles.baseline) {

		tmp.list <- list() 
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=unlist(strsplit(i, "[.]"))[2],
				sgp_object@SGP[["SGPercentiles"]][[i]])

			if (is.na(unlist(strsplit(i, "[.]"))[3])) { ### If cohort referenced SGP are to be included in baseline SGP (e.g., Georgia)
				setnames(tmp.list[[i]], "SGP", "SGP_BASELINE")
				if ("SGP_LEVEL" %in% names(tmp.list[[i]])) setnames(tmp.list[[i]], "SGP_LEVEL", "SGP_LEVEL_BASELINE")
				if ("SGP_NORM_GROUP" %in% names(tmp.list[[i]])) setnames(tmp.list[[i]], "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE")
			}
		}

		tmp.data <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))

		if (any(duplicated(tmp.data))) {
			tmp.data <- getPreferredSGP(tmp.data, state, type="BASELINE")
		}

		if (length(intersect(names(slot.data), names(tmp.data)) %w/o% key(slot.data))==0) {
			slot.data <- tmp.data[slot.data]
		} else {
			variables.to.merge <- names(tmp.data) %w/o% c(key(slot.data), "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED")
			for (tmp.merge.variable in variables.to.merge) {
				slot.data[tmp.data[,key(slot.data), with=FALSE], tmp.merge.variable := tmp.data[, tmp.merge.variable, with=FALSE], with=FALSE, nomatch=0]
			}
		}

		setkeyv(slot.data, getKey(slot.data))
		rm(tmp.list); suppressMessages(gc())
	}


	######################################################################################
	### Create SGP targets (Cohort and Baseline referenced) and merge with student data
	######################################################################################

	if (length(getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.projections.lagged")) == 0) {
		 tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged projections available in SGP slot. No student growth projection targets will be produced.\n")
		 sgp.projections.lagged <- FALSE
	}
	if (length(getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.projections.lagged.baseline")) == 0) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged baseline projections available in SGP slot. No baseline referenced student growth projection targets will be produced.\n")
		sgp.projections.lagged.baseline <- FALSE
	}

	### Calculate Targets
 
	if (sgp.projections.lagged | sgp.projections.lagged.baseline) { 

		if (length(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")) > 1) {
			target.level <- c("CATCH_UP_KEEP_UP", "MOVE_UP_STAY_UP")
		} else {
			target.level <- "CATCH_UP_KEEP_UP"
		}


		for (target.type.iter in target.type) {
			for (target.level.iter in target.level) {
				tmp.data <- getTargetSGP(sgp_object, content_areas, state, years, target.type.iter, target.level.iter, max.sgp.target.years.forward)

				if (length(intersect(names(slot.data), names(tmp.data)) %w/o% key(slot.data))==0) {
					slot.data <- tmp.data[slot.data]
				} else {
					variables.to.merge <- names(tmp.data) %w/o% key(slot.data)
					for (tmp.merge.variable in variables.to.merge) {
						slot.data[tmp.data[,intersect(key(slot.data), names(tmp.data)), with=FALSE],
								tmp.merge.variable := tmp.data[, tmp.merge.variable, with=FALSE], with=FALSE, nomatch=0]
					}
				}
			}
		}

		### CATCH_UP_KEEP_UP_STATUS Calculation

		if ("CATCH_UP_KEEP_UP" %in% target.level) {

			level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1
			slot.data[,CATCH_UP_KEEP_UP_STATUS_INITIAL:=getTargetInitialStatus(ACHIEVEMENT_LEVEL_PRIOR, state, "CATCH_UP_KEEP_UP")]

			if ("CATCH_UP_KEEP_UP_STATUS" %in% names(slot.data)) slot.data[,CATCH_UP_KEEP_UP_STATUS := NULL]
			slot.data[,CATCH_UP_KEEP_UP_STATUS := rep(as.character(NA), dim(slot.data)[1])]

			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & get(my.sgp) >= get(my.sgp.target), CATCH_UP_KEEP_UP_STATUS := "Keep Up: Yes"]
			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & get(my.sgp) < get(my.sgp.target), CATCH_UP_KEEP_UP_STATUS := "Keep Up: No"]
			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & get(my.sgp) >= get(my.sgp.target), CATCH_UP_KEEP_UP_STATUS := "Catch Up: Yes"]
			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & get(my.sgp) < get(my.sgp.target), CATCH_UP_KEEP_UP_STATUS := "Catch Up: No"]

			### CATCH_UP_KEEP_UP clean up based upon reality

			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" & 
				as.numeric(ACHIEVEMENT_LEVEL) <= level.to.get, CATCH_UP_KEEP_UP_STATUS := "Keep Up: No"]
			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & CATCH_UP_KEEP_UP_STATUS == "Catch Up: No" & 
				as.numeric(ACHIEVEMENT_LEVEL) > level.to.get, CATCH_UP_KEEP_UP_STATUS := "Catch Up: Yes"]
			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" & 
				as.numeric(ACHIEVEMENT_LEVEL) <= level.to.get & GRADE == max(as.numeric(GRADE[!is.na(get(my.sgp.target))])), CATCH_UP_KEEP_UP_STATUS := "Catch Up: No"]
			slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & CATCH_UP_KEEP_UP_STATUS == "Keep Up: No" & 
				as.numeric(ACHIEVEMENT_LEVEL) > level.to.get & GRADE == max(as.numeric(GRADE[!is.na(get(my.sgp.target))])), CATCH_UP_KEEP_UP_STATUS := "Keep Up: Yes"]
			slot.data[,CATCH_UP_KEEP_UP_STATUS := as.factor(CATCH_UP_KEEP_UP_STATUS)]
		}


		### MOVE_UP_STAY_UP_STATUS Calculation

		if ("MOVE_UP_STAY_UP" %in% target.level) {

			level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")
			slot.data[,MOVE_UP_STAY_UP_STATUS_INITIAL:=getTargetInitialStatus(ACHIEVEMENT_LEVEL_PRIOR, state, "MOVE_UP_STAY_UP")]

			if ("MOVE_UP_STAY_UP_STATUS" %in% names(slot.data)) slot.data[,MOVE_UP_STAY_UP_STATUS := NULL]
			slot.data[,MOVE_UP_STAY_UP_STATUS := rep(as.character(NA), dim(slot.data)[1])]

			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & get(my.sgp) >= get(my.sgp.target.move.up.stay.up), MOVE_UP_STAY_UP_STATUS := "Stay Up: Yes"]
			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & get(my.sgp) < get(my.sgp.target.move.up.stay.up), MOVE_UP_STAY_UP_STATUS := "Stay Up: No"]
			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & get(my.sgp) >= get(my.sgp.target.move.up.stay.up), MOVE_UP_STAY_UP_STATUS := "Move Up: Yes"]
			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & get(my.sgp) < get(my.sgp.target.move.up.stay.up), MOVE_UP_STAY_UP_STATUS := "Move Up: No"]

			### MOVE_UP_STAY_UP clean up based upon reality

			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & MOVE_UP_STAY_UP_STATUS == "Stay Up: Yes" & 
				as.numeric(ACHIEVEMENT_LEVEL) <= level.to.get, MOVE_UP_STAY_UP_STATUS := "Stay Up: No"]
			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & MOVE_UP_STAY_UP_STATUS == "Move Up: No" & 
				as.numeric(ACHIEVEMENT_LEVEL) > level.to.get, MOVE_UP_STAY_UP_STATUS := "Move Up: Yes"]
			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & MOVE_UP_STAY_UP_STATUS == "Move Up: Yes" & 
				as.numeric(ACHIEVEMENT_LEVEL) <= level.to.get & GRADE == max(as.numeric(GRADE[!is.na(get(my.sgp.target.move.up.stay.up))])), MOVE_UP_STAY_UP_STATUS := "Move Up: No"]
			slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & MOVE_UP_STAY_UP_STATUS == "Stay Up: No" & 
				as.numeric(ACHIEVEMENT_LEVEL) > level.to.get & GRADE == max(as.numeric(GRADE[!is.na(get(my.sgp.target.move.up.stay.up))])), MOVE_UP_STAY_UP_STATUS := "Stay Up: Yes"]
			slot.data[,MOVE_UP_STAY_UP_STATUS := as.factor(MOVE_UP_STAY_UP_STATUS)]

		}

	} ## END sgp.projections.lagged | sgp.projections.lagged.baseline

	for (i in intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL", "MOVE_UP_STAY_UP_STATUS_INITIAL"))) {
		slot.data[[i]] <- NULL
	}

	setkeyv(slot.data, getKey(slot.data))
	sgp_object@Data <- slot.data

	message(c(tmp.messages, paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"), sep=""))

	return(sgp_object)

} ## END combineSGP Function
