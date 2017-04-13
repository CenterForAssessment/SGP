`combineSGP` <-
function(
	sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	sgp.percentiles=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	sgp.target.scale.scores=FALSE,
	sgp.target.scale.scores.only=FALSE,
	sgp.target.content_areas=NULL,
	max.sgp.target.years.forward=3,
	update.all.years=FALSE,
	sgp.config=NULL,
	sgp.percentiles.equated=FALSE,
	SGPt=NULL,
	fix.duplicates=NULL,
	parallel.config=NULL) {

	started.at <- proc.time()
	messageSGP(paste("Started combineSGP", prettyDate()))

	ID <- CONTENT_AREA <- YEAR <- GRADE <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- MOVE_UP_STAY_UP_STATUS_INITIAL <- VALID_CASE <- N <- NULL
	MOVE_UP_STAY_UP_STATUS <- CATCH_UP_KEEP_UP_STATUS <- ACHIEVEMENT_LEVEL_PRIOR <- target.type <- SGP_PROJECTION_GROUP <- DUPS_FLAG <- i.DUPS_FLAG <- SCALE_SCORE <- SGP_NORM_GROUP_SCALE_SCORES <- NULL

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
		tmp.name.position <- sapply(c(datasets::state.name, "AOB", "DEMONSTRATION"), function(x) regexpr(toupper(x), tmp.name))
		if (any(tmp.name.position!=-1)) {
			state <- c(datasets::state.abb, "AOB", "DEMO")[which(names(sort(tmp.name.position[tmp.name.position!=-1])[1])==c(datasets::state.name, "AOB", "DEMONSTRATION"))]
		} else {
			tmp.messages <- c(tmp.messages, "\tNOTE: argument 'state' required for target SGP calculation. Target SGPs will not be calculated.\n")
			sgp.projections.lagged <- sgp.projections.lagged.baseline <- FALSE
		}
	}


	### SGP_Configuration arguments

	### Create SGP_TARGET_CONTENT_AREA in certain cases

	if (is.null(sgp.target.content_areas) & any(sapply(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]], function(x) uniqueN(x)) > 1)) {
		sgp.target.content_areas <- TRUE
		tmp.messages <- c(tmp.messages, "\tNOTE: Multiple content areas detected for student growth targets. 'sgp.target.content_areas set to TRUE.\n")
	}

	### Check to see if max.sgp.target.years.forward if configured in SGPstateData

	if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['max.sgp.target.years.forward']])) {
		max.sgp.target.years.forward <- SGP::SGPstateData[[state]][['SGP_Configuration']][['max.sgp.target.years.forward']]
	}

	if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['sgp.projections.projection.unit.label']])) {
		projection.unit.label <- SGP::SGPstateData[[state]][['SGP_Configuration']][['sgp.projections.projection.unit.label']]
	} else {
		projection.unit.label <- "YEAR"
	}


	### Setup for equated SGPs and scale score targets

	if (!is.null(year.for.equate <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]])) {
		tmp.last.year <- tail(sort(unique(sgp_object@Data, by='YEAR')[['YEAR']]), 1)
		if (year.for.equate!=tmp.last.year) {
			sgp.percentiles.equated <- FALSE
			if (sgp.target.scale.scores) sgp.projections.equated <- NULL
		} else {
			sgp.percentiles.equated <- TRUE
			if (sgp.target.scale.scores) sgp.projections.equated <- list(Year=tmp.last.year, Linkages=sgp_object@SGP[['Linkages']])
		}
	} else {
		if (sgp.percentiles.equated) {
			messageSGP("\tNOTE: 'sgp.percentiles.equated' has been set to TRUE but no meta-data exists in SGPstateData associated with the assessment transition. Equated/linked SGP analyses require meta-data embedded in 'SGPstateData' to correctly work. Contact package administrators on how such data can be added to the package.")
			sgp.percentiles.equated <- FALSE
			sgp.target.scale.scores <- FALSE
		}
		if (sgp.target.scale.scores) sgp.projections.equated <- NULL
	}

	### fix.duplicates

	if (is.null(fix.duplicates) & !is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["fix.duplicates"]])) {
		fix.duplicates <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["fix.duplicates"]]
	}


	### Utility functions

	get.target.arguments <- function(system.type, target.type=NULL, projection.unit.label, year.for.equate) {
		tmp.list <- list()
		if (is.null(system.type)) {
			if (identical(target.type, c("sgp.projections", "sgp.projections.lagged"))) system.type <- "Cohort Referenced"
			if (identical(target.type, c("sgp.projections.baseline", "sgp.projections.lagged.baseline"))) system.type <- "Baseline Referenced"
			if (identical(target.type, c("sgp.projections", "sgp.projections.baseline", "sgp.projections.lagged", "sgp.projections.lagged.baseline"))) {
				system.type <- "Cohort and Baseline Referenced"
			}
		}

		if (!is.null(target.type)) {
			if (identical(target.type, "sgp.projections.lagged")) system.type <- "Cohort Referenced"
			if (identical(target.type, "sgp.projections.lagged.baseline")) system.type <- "Baseline Referenced"
			if (identical(target.type, c("sgp.projections.lagged", "sgp.projections.lagged.baseline"))) system.type <- "Cohort and Baseline Referenced"
		}

		if (identical(system.type, "Cohort Referenced")) {
			tmp.list[['target.type']] <- intersect(target.type, c("sgp.projections", "sgp.projections.lagged"))
			if (!is.null(year.for.equate) && !sgp.percentiles.equated) {
				tmp.variable.name <- paste("SGP_FROM", year.for.equate, sep="_")
				tmp.messages <- c(tmp.messages, paste0("\tNOTE: Due to test transition in ", year.for.equate, " SGP_TARGET will be compared to ", tmp.variable.name, ".\n"))
				tmp.list[['my.sgp']] <- tmp.variable.name
			} else {
				tmp.list[['my.sgp']] <- "SGP"
			}
			tmp.list[['my.sgp.target']] <- paste("SGP_TARGET", max.sgp.target.years.forward, projection.unit.label, sep="_")
			tmp.list[['my.sgp.target.content_area']] <- paste("SGP_TARGET", max.sgp.target.years.forward, projection.unit.label, "CONTENT_AREA", sep="_")
			tmp.list[['my.sgp.target.move.up.stay.up']] <- paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, sep="_")
			if (sgp.target.scale.scores) tmp.list[['sgp.target.scale.scores.types']] <- intersect(target.type, c("sgp.projections", "sgp.projections.lagged"))
		}
		if (identical(system.type, "Baseline Referenced")) {
			tmp.list[['target.type']] <- intersect(target.type, c("sgp.projections.baseline", "sgp.projections.lagged.baseline"))
			tmp.list[['my.sgp']] <- "SGP_BASELINE"
			tmp.list[['my.sgp.target']] <- paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, projection.unit.label, sep="_")
			tmp.list[['my.sgp.target.content_area']] <- paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, projection.unit.label, "CONTENT_AREA", sep="_")
			tmp.list[['my.sgp.target.move.up.stay.up']] <- paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, sep="_")
			if (sgp.target.scale.scores) tmp.list[['sgp.target.scale.scores.types']] <- intersect(target.type, c("sgp.projections.baseline", "sgp.projections.lagged.baseline"))
		}
		if (identical(system.type, "Cohort and Baseline Referenced")) {
			tmp.list[['target.type']] <- intersect(target.type, c("sgp.projections", "sgp.projections.baseline", "sgp.projections.lagged", "sgp.projections.lagged.baseline"))
			if (!is.null(year.for.equate) && !sgp.percentiles.equated) {
				tmp.year.diff <- as.numeric(unlist(strsplit(tail(sort(unique(sgp_object@Data, by='YEAR')[['YEAR']]), 1), "_"))[1]) - as.numeric(unlist(strsplit(year.for.equate, "_"))[1])
				tmp.messages <- c(tmp.messages, paste0("\tNOTE: Due to test transition in ", year.for.equate, " SGP_TARGET will utilize ", paste("SGP_MAX_ORDER", tmp.year.diff, sep="_"), ".\n"))
				tmp.list[['my.sgp']] <- c(paste("SGP_MAX_ORDER", tmp.year.diff, sep="_"), "SGP_BASELINE")[c(sgp.percentiles, sgp.percentiles.baseline)]
			} else {
				tmp.list[['my.sgp']] <- c("SGP", "SGP_BASELINE")[c(sgp.percentiles, sgp.percentiles.baseline)]
			}
			tmp.list[['my.sgp.target']] <- c(paste("SGP_TARGET", max.sgp.target.years.forward, projection.unit.label, sep="_"),
				paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, projection.unit.label, sep="_"))
			tmp.list[['my.sgp.target.content_area']] <- c(paste("SGP_TARGET", max.sgp.target.years.forward, projection.unit.label, "CONTENT_AREA", sep="_"),
				paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, projection.unit.label, "CONTENT_AREA", sep="_"))
			tmp.list[['my.sgp.target.move.up.stay.up']] <- c(paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, sep="_"),
				paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, sep="_"))
			if (sgp.target.scale.scores) tmp.list[['sgp.target.scale.scores.types']] <-
				intersect(target.type, c("sgp.projections", "sgp.projections.baseline", "sgp.projections.lagged", "sgp.projections.lagged.baseline"))
		}

		tmp.list[['target.level']] <- c("CATCH_UP_KEEP_UP", "MOVE_UP_STAY_UP")
		if (!is.null(SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]) &&
			length(which(SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")) <= 1) {
			tmp.list[['target.level']] <- "CATCH_UP_KEEP_UP"
		}
		if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.target.types']]) &&
			!grepl("MUSU", SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.target.types']])) {
				tmp.list[['target.level']] <- "CATCH_UP_KEEP_UP"
		}

		return(tmp.list)
	} ### END get.target.arguments

	catch_keep_move_functions <- c(min, max)

	getTargetData <- function(tmp.target.data, projection_group.iter, tmp.target.level.names) {
		if ("YEAR_WITHIN" %in% names(tmp.target.data)) {
			tmp.var.names <- c("ID", "CONTENT_AREA", "YEAR", "YEAR_WITHIN", intersect(names(tmp.target.data), c("GRADE", "SGP_PROJECTION_GROUP_SCALE_SCORES")))
		} else tmp.var.names <- c("ID", "CONTENT_AREA", "YEAR", intersect(names(tmp.target.data), c("GRADE", "SGP_PROJECTION_GROUP_SCALE_SCORES")))
		tmp.data <- tmp.target.data[SGP_PROJECTION_GROUP==projection_group.iter, c(tmp.var.names, tmp.target.level.names), with=FALSE]
		na.omit(tmp.data, cols=grep("MOVE_UP_STAY_UP", tmp.target.level.names, invert=TRUE, value=TRUE))
	}


	############################################################################
	### Check update.all.years
	############################################################################

	if (update.all.years) {
		variables.to.null.out <- c(
			"SGP", "SGP_SIMEX", "SGP_LEVEL", "SGP_STANDARD_ERROR", "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SGP_BASELINE", "SGP_LEVEL_BASELINE",
			"SGP_TARGET", "SGP_TARGET_MU", "SGP_TARGET_MU_BASELINE", "SGP_TARGET_MOVE_UP_STAY_UP", "SGP_TARGET_MOVE_UP_STAY_UP_BASELINE", "ACHIEVEMENT_LEVEL_PRIOR",
			"CATCH_UP_KEEP_UP_STATUS_INITIAL", "SGP_TARGET_BASELINE", "CATCH_UP_KEEP_UP_STATUS", "CATCH_UP_KEEP_UP_STATUS_BASELINE",
			"MOVE_UP_STATUS", "MOVE_UP_STAY_UP_STATUS", "MOVE_UP_STAY_UP_STATUS_BASELINE",
			"SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE", "SGP_BASELINE_STANDARD_ERROR", "SGP_NORM_GROUP_SCALE_SCORES", "SGP_NORM_GROUP_BASELINE_SCALE_SCORES",
			grep("SGP_ORDER", names(slot.data), value=TRUE), grep("SGP_BASELINE_ORDER", names(slot.data), value=TRUE),
			grep("PERCENTILE_CUT", names(slot.data), value=TRUE), grep("CONFIDENCE_BOUND", names(slot.data), value=TRUE),
			paste("SGP_TARGET", max.sgp.target.years.forward, projection.unit.label, sep="_"),
			paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, sep="_"),
			paste("SGP_TARGET", max.sgp.target.years.forward, projection.unit.label, "CURRENT", sep="_"),
			paste("SGP_TARGET_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, "CURRENT", sep="_"),
			paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, projection.unit.label, sep="_"),
			paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, sep="_"),
			paste("SGP_TARGET_BASELINE", max.sgp.target.years.forward, projection.unit.label, "CURRENT", sep="_"),
			paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", max.sgp.target.years.forward, projection.unit.label, "CURRENT", sep="_"))

		for (tmp.variables.to.null.out in intersect(names(slot.data), variables.to.null.out)) {
			slot.data[,(tmp.variables.to.null.out):=NULL]
		}
	}


	############################################################################
	### sgp.percentiles: Merge Cohort Referenced SGPs with student data
	############################################################################

	## Determine names of Cohort Referenced SGPs

	if (!sgp.target.scale.scores.only && length(tmp.names <- getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.percentiles", sgp.percentiles.equated)) == 0 && sgp.percentiles) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No cohort referenced SGP results available in SGP slot. No cohort referenced SGP results will be merged.\n")
		sgp.percentiles <- FALSE
	}

	if (sgp.percentiles & !sgp.target.scale.scores.only) {

		tmp.list <- list()
		for (i in tmp.names) {
		tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGPercentiles"]][[i]])
		}

		tmp.data <- data.table(rbindlist(tmp.list, fill=TRUE), VALID_CASE="VALID_CASE", key=key(slot.data))

		if (any(duplicated(tmp.data, by=key(tmp.data)))) {
			tmp.data <- getPreferredSGP(tmp.data, state)
		}

		if (!is.null(fix.duplicates) & any(grepl("_DUPS_[0-9]*", tmp.data[["ID"]]))) {
			##  Strip ID of the _DUPS_ Flag, but keep in a seperate variable (used to merge subsequently)
			invisible(tmp.data[, DUPS_FLAG := gsub(".*_DUPS_", "", ID)])
			invisible(tmp.data[!grepl("_DUPS_[0-9]*", ID), DUPS_FLAG := NA])
			invisible(tmp.data[, ID := gsub("_DUPS_[0-9]*", "", ID)])

			##  Extend the slot.data if any new rows are required (e.g. dups in prior years) - if not still merge in DUPS_FLAG.
			slot.data.extension <- tmp.data[!is.na(DUPS_FLAG), c(key(slot.data), "SGP_NORM_GROUP_SCALE_SCORES", "DUPS_FLAG"), with=FALSE]
			tmp.split <- strsplit(as.character(slot.data.extension[["SGP_NORM_GROUP_SCALE_SCORES"]]), "; ")
			invisible(slot.data.extension[, SCALE_SCORE := as.numeric(sapply(tmp.split, function(x) rev(x)[1]))])
			invisible(slot.data.extension[, SGP_NORM_GROUP_SCALE_SCORES := NULL])
			if ("DUPS_FLAG" %in% names(slot.data)) flag.fix <- TRUE else flag.fix <- FALSE
			slot.data <- slot.data.extension[slot.data, on=c(key(slot.data),"SCALE_SCORE"), allow.cartesian=TRUE]
			if (flag.fix) { # Merge together DUPS_FLAG from previous years
				invisible(slot.data[!is.na(i.DUPS_FLAG) & is.na(DUPS_FLAG), DUPS_FLAG := i.DUPS_FLAG])
				invisible(slot.data[, i.DUPS_FLAG := NULL])
			}

			##  Get the row index for variable merge.
			tmp.index <- slot.data[tmp.data[, c(getKey(slot.data), "GRADE", "DUPS_FLAG"), with=FALSE], which=TRUE, on=c(getKey(slot.data), "GRADE", "DUPS_FLAG")]
		} else {
			tmp.index <- slot.data[tmp.data[, key(tmp.data), with=FALSE], which=TRUE, on=key(tmp.data)]
		}

		variables.to.merge <- setdiff(names(tmp.data), c(getKey(slot.data), "GRADE"))
		invisible(slot.data[tmp.index, (variables.to.merge):=tmp.data[, variables.to.merge, with=FALSE]])

		setkeyv(slot.data, getKey(slot.data))
	}


	###################################################################################
	### sgp.percentiles.baseline: Merge baseline referenced SGPs with student data
	###################################################################################

	## Determine names of Baseline Referenced SGPs

	if (!sgp.target.scale.scores.only && length(tmp.names <- getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.percentiles.baseline"))==0 && sgp.percentiles.baseline) {
		 tmp.messages <- c(tmp.messages, "\tNOTE: No baseline referenced SGP results available in SGP slot. No baseline referenced SGP results will be merged.\n")
		 sgp.percentiles.baseline <- FALSE
	}

	if (sgp.percentiles.baseline & !sgp.target.scale.scores.only) {

		tmp.list <- list()
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=getTableNameYear(i),
				sgp_object@SGP[["SGPercentiles"]][[i]])

			if (is.na(unlist(strsplit(i, "[.]"))[3])) { ### If cohort referenced SGP are to be included in baseline SGP (e.g., Georgia)
				setnames(tmp.list[[i]], "SGP", "SGP_BASELINE")
				if ("SGP_LEVEL" %in% names(tmp.list[[i]])) setnames(tmp.list[[i]], "SGP_LEVEL", "SGP_LEVEL_BASELINE")
				if ("SGP_NORM_GROUP" %in% names(tmp.list[[i]])) setnames(tmp.list[[i]], "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE")
				if ("SGP_NORM_GROUP_SCALE_SCORES" %in% names(tmp.list[[i]])) setnames(tmp.list[[i]], "SGP_NORM_GROUP_SCALE_SCORES", "SGP_NORM_GROUP_BASELINE_SCALE_SCORES")
				if ("SGP_SIMEX" %in% names(tmp.list[[i]])) setnames(tmp.list[[i]], "SGP_SIMEX", "SGP_SIMEX_BASELINE")
			}
		}

		tmp.data <- data.table(rbindlist(tmp.list, fill=TRUE), VALID_CASE="VALID_CASE", key=key(slot.data))

		if (any(duplicated(tmp.data, by=key(tmp.data)))) {
			tmp.data <- getPreferredSGP(tmp.data, state, type="BASELINE")
		}

		if (!is.null(fix.duplicates) & any(grepl("_DUPS_[0-9]*", tmp.data[["ID"]]))) {
			##  Strip ID of the _DUPS_ Flag, but keep in a seperate variable (used to merge subsequently)
			invisible(tmp.data[, DUPS_FLAG := gsub(".*_DUPS_", "", ID)])
			invisible(tmp.data[!grepl("_DUPS_[0-9]*", ID), DUPS_FLAG := NA])
			invisible(tmp.data[, ID := gsub("_DUPS_[0-9]*", "", ID)])

			##  Extend the slot.data if any new rows are required (e.g. dups in prior years) - if not still merge in DUPS_FLAG.
			slot.data.extension <- tmp.data[!is.na(DUPS_FLAG), c(key(slot.data), "SGP_NORM_GROUP_SCALE_SCORES", "DUPS_FLAG"), with=FALSE]
			tmp.split <- strsplit(as.character(slot.data.extension[["SGP_NORM_GROUP_SCALE_SCORES"]]), "; ")
			invisible(slot.data.extension[, SCALE_SCORE := as.numeric(sapply(tmp.split, function(x) rev(x)[1]))])
			invisible(slot.data.extension[, SGP_NORM_GROUP_SCALE_SCORES := NULL])
			if ("DUPS_FLAG" %in% names(slot.data)) flag.fix <- TRUE else flag.fix <- FALSE
			slot.data <- slot.data.extension[slot.data, on=c(key(slot.data),"SCALE_SCORE"), allow.cartesian=TRUE]
			if (flag.fix) { # Merge together DUPS_FLAG from previous years
				invisible(slot.data[!is.na(i.DUPS_FLAG) & is.na(DUPS_FLAG), DUPS_FLAG := i.DUPS_FLAG])
				invisible(slot.data[, i.DUPS_FLAG := NULL])
			}

			##  Get the row index for variable merge.
			tmp.index <- slot.data[tmp.data[, c(getKey(slot.data), "GRADE", "DUPS_FLAG"), with=FALSE], which=TRUE, on=c(getKey(slot.data), "GRADE", "DUPS_FLAG")] #
		} else {
			tmp.index <- slot.data[tmp.data[, key(tmp.data), with=FALSE], which=TRUE, on=key(tmp.data)]
		}

		variables.to.merge <- setdiff(names(tmp.data), c(getKey(slot.data), "GRADE"))
		invisible(slot.data[tmp.index, (variables.to.merge):=tmp.data[, variables.to.merge, with=FALSE]])

		setkeyv(slot.data, getKey(slot.data))
	}


	######################################################################################
	### Create SGP targets (Cohort and Baseline referenced) and merge with student data
	######################################################################################


	if (!sgp.target.scale.scores.only && length(getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.projections"))==0 && sgp.projections) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No SGP projections available in SGP slot. No current year student growth projection targets will be produced.\n")
		sgp.projections <- FALSE;
	}
	if (!sgp.target.scale.scores.only && length(getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.projections.baseline"))==0 && sgp.projections.baseline) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No SGP baseline projections available in SGP slot. No current year baseline student growth projection targets will be produced.\n")
		sgp.projections.baseline <- FALSE;
	}
	if (!sgp.target.scale.scores.only && length(getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.projections.lagged"))==0 && sgp.projections.lagged) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged projections available in SGP slot. No student growth projection targets will be produced.\n")
		sgp.projections.lagged <- FALSE;
	}
	if (!sgp.target.scale.scores.only && length(getPercentileTableNames(sgp_object, content_areas, state, years, "sgp.projections.lagged.baseline"))==0 && sgp.projections.lagged.baseline) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged baseline projections available in SGP slot. No baseline referenced student growth projection targets will be produced.\n")
		sgp.projections.lagged.baseline <- FALSE;
	}
	target.type <- c("sgp.projections", "sgp.projections.baseline", "sgp.projections.lagged", "sgp.projections.lagged.baseline")[
				c(sgp.projections, sgp.projections.baseline, sgp.projections.lagged, sgp.projections.lagged.baseline)]

	### Calculate Targets

	if ((sgp.projections | sgp.projections.baseline | sgp.projections.lagged | sgp.projections.lagged.baseline) & !sgp.target.scale.scores.only) {

		target.args <- get.target.arguments(SGP::SGPstateData[[state]][["Growth"]][["System_Type"]], target.type, projection.unit.label, year.for.equate)

		for (target.type.iter in target.args[['target.type']]) {
			for (target.level.iter in target.args[['target.level']]) {

				if (!is.null(fix.duplicates)) {
					if (!sgp.percentiles & !sgp.percentiles.baseline) messageSGP("The fix.duplicates='KEEP.ALL' functionality requires that sgp.percentiles = TRUE or percentiles results have already been merged into @Data.")
					##  Seperate out prior score history of slot.data
					tmp.split <- strsplit(as.character(slot.data[["SGP_NORM_GROUP_SCALE_SCORES"]]), "; ")
					num.scores <- max(sapply(seq_along(tmp.split), function(f) length(tmp.split[[f]])))
					if (num.scores > 2) {
						for (tmp.prior in tail(seq(num.scores), -2)) {
							invisible(slot.data[, paste0("SCALE_SCORE_PRIOR_", tmp.prior-1L) := as.numeric(sapply(tmp.split, function(x) rev(x)[tmp.prior]))])
				}}}

				tmp.data <- getTargetSGP(sgp_object, slot.data, content_areas, state, years, target.type.iter, target.level.iter, max.sgp.target.years.forward, fix.duplicates=fix.duplicates)

				if (!is.null(fix.duplicates)) dup.by <- c(key(tmp.data), grep("SCALE_SCORE$|SCALE_SCORE_PRIOR", names(tmp.data), value=TRUE)) else dup.by <- key(tmp.data)

				if (any(duplicated(tmp.data, by=dup.by))) {
					duplicated.projections.tf <- TRUE
					tmp.data <- getPreferredSGP(tmp.data, state, type="TARGET")
				} else duplicated.projections.tf <- FALSE

				if (!is.null(fix.duplicates) & any(grepl("_DUPS_[0-9]*", tmp.data[["ID"]]))) {
					##  Strip ID of the _DUPS_ Flag, Don't use this as DUPS_FLAG (merge in later from SGPercentiles)
					invisible(tmp.data[, ID := gsub("_DUPS_[0-9]*", "", ID)])

					##  Get the row index for variable merge.
					if (grepl('lagged', target.type.iter)) {
						tmp.index <- slot.data[
							tmp.data[, c(intersect(getKey(slot.data), names(tmp.data)), "DUPS_FLAG", grep("SCALE_SCORE_PRIOR", names(tmp.data), value=TRUE)), with=FALSE, nomatch=NA],
							which=TRUE, on=c(getKey(slot.data), "DUPS_FLAG", grep("SCALE_SCORE_PRIOR", names(tmp.data), value=TRUE))]

						no_match <- tmp.data[which(is.na(tmp.index)),] # usually current year score is NA - still get a lagged projection, but no SGP (& therefore no prior score to merge on)
						if (nrow(no_match) > 0) {
							no_match.index <- slot.data[no_match[, intersect(getKey(slot.data), names(no_match)), with=FALSE, nomatch=NA], which=TRUE, on=getKey(slot.data)]
							if (length(no_match.index) == length(tmp.index[which(is.na(tmp.index))])) {
								tmp.index[which(is.na(tmp.index))] <- no_match.index
							} else stop("Error in matching LAGGED projections with duplicates in data (most likely student records with a current year SCALE_SCORE == NA).")
						}
					} else {
						setnames(tmp.data, "SGP_PROJECTION_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES_CURRENT")

						tmp.index <- slot.data[
							tmp.data[, c(intersect(getKey(slot.data), names(tmp.data)), "DUPS_FLAG", grep("SCALE_SCORE$|SCALE_SCORE_PRIOR", names(tmp.data), value=TRUE)), with=FALSE, nomatch=NA],
							which=TRUE, on=c(getKey(slot.data), "DUPS_FLAG", grep("SCALE_SCORE$|SCALE_SCORE_PRIOR", names(tmp.data), value=TRUE))]

						no_match <- tmp.data[which(is.na(tmp.index)),] # usually current year score is NA - still get a lagged projection, but no SGP (& therefore no prior score to merge on)
						if (nrow(no_match) > 0) {
							no_match.index <- slot.data[no_match[, intersect(getKey(slot.data), names(no_match)), with=FALSE, nomatch=NA], which=TRUE, on=intersect(getKey(slot.data), names(no_match))]
							if (length(no_match.index) == length(tmp.index[which(is.na(tmp.index))])) {
								tmp.index[which(is.na(tmp.index))] <- no_match.index
							} else stop("Error in matching STRAIGHT (CURRENT) projections with duplicates in data (most likely student records with a current year SCALE_SCORE == NA).")
						}
					}
				} else {
					tmp.index <- slot.data[tmp.data[,intersect(getKey(slot.data), names(tmp.data)), with=FALSE], which=TRUE, on=dup.by]
				}
				variables.to.merge <- setdiff(names(tmp.data), c(getKey(slot.data), "DUPS_FLAG", grep("SCALE_SCORE$|SCALE_SCORE_PRIOR", names(tmp.data), value=TRUE)))
				invisible(slot.data[tmp.index, (variables.to.merge):=tmp.data[, variables.to.merge, with=FALSE]])
			}
		}
		if (duplicated.projections.tf) {
			tmp.messages <- c(tmp.messages, paste0(
				"\tNOTE: Multiple Projections exist for individual students. Unique SGP Targets will be created using SGP Progression Preference Table for ", state, ".\n"))
		}

		### SGP_TARGET_CONTENT_AREA calculation

		terminal.content_areas <- unique(na.omit(slot.data, cols=target.args[['my.sgp.target']][1]), by='CONTENT_AREA')[['CONTENT_AREA']]
		if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
			terminal.content_areas <- intersect(terminal.content_areas, sapply(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]], tail, 1))
		}

		if (!is.null(sgp.target.content_areas) && sgp.target.content_areas) {
			for (my.sgp.target.content_area.iter in target.args[['my.sgp.target.content_area']]) {
				slot.data[!is.na(get(target.args[['my.sgp.target']][1])), target.args[['my.sgp.target.content_area']] :=
					getTargetSGPContentArea(GRADE[1], CONTENT_AREA[1], state, max.sgp.target.years.forward, my.sgp.target.content_area.iter),
					by=list(GRADE, CONTENT_AREA)]
			}
		}

		### CATCH_UP_KEEP_UP_STATUS Calculation

		if ("CATCH_UP_KEEP_UP" %in% target.args[['target.level']] & (sgp.projections.lagged | sgp.projections.lagged.baseline)) {

			catch.up.keep.up.levels <- getTargetAchievementLevels(state, "CATCH_UP_KEEP_UP")
			slot.data[,CATCH_UP_KEEP_UP_STATUS_INITIAL:=getTargetInitialStatus(ACHIEVEMENT_LEVEL_PRIOR, state, status.type="CATCH_UP_KEEP_UP")]

			for (i in seq_along(target.args[['my.sgp']])) {
				if (!grepl("BASELINE", target.args[['my.sgp']][i])) my.label <- "CATCH_UP_KEEP_UP_STATUS" else my.label <- "CATCH_UP_KEEP_UP_STATUS_BASELINE"
				if (my.label %in% names(slot.data)) slot.data[,(my.label):=NULL]
				slot.data[,(my.label):=rep(as.character(NA), dim(slot.data)[1])]

				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & get(target.args[['my.sgp']][i]) >= get(target.args[['my.sgp.target']][i]),
					(my.label):="Keep Up: Yes"]
				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & get(target.args[['my.sgp']][i]) < get(target.args[['my.sgp.target']][i]),
					(my.label):="Keep Up: No"]
				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & get(target.args[['my.sgp']][i]) >= get(target.args[['my.sgp.target']][i]),
					(my.label):="Catch Up: Yes"]
				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & get(target.args[['my.sgp']][i]) < get(target.args[['my.sgp.target']][i]),
					(my.label):="Catch Up: No"]

				### CATCH_UP_KEEP_UP clean up based upon reality

				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & get(my.label) == "Keep Up: Yes" &
					ACHIEVEMENT_LEVEL %in% catch.up.keep.up.levels[['NO']], (my.label):="Keep Up: No"]
				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & get(my.label) == "Catch Up: No" &
					ACHIEVEMENT_LEVEL %in% catch.up.keep.up.levels[['YES']], (my.label):="Catch Up: Yes"]
				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" & get(my.label) == "Catch Up: Yes" &
					ACHIEVEMENT_LEVEL %in% catch.up.keep.up.levels[['NO']] &
					GRADE == max(type.convert(GRADE[!is.na(get(target.args[['my.sgp.target']]))], as.is=TRUE)) &
					CONTENT_AREA %in% terminal.content_areas, (my.label):="Catch Up: No"]
				slot.data[CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" & get(my.label) == "Keep Up: No" &
					ACHIEVEMENT_LEVEL %in% catch.up.keep.up.levels[['YES']] &
					GRADE == max(type.convert(GRADE[!is.na(get(target.args[['my.sgp.target']]))], as.is=TRUE)) &
					CONTENT_AREA %in% terminal.content_areas, (my.label):="Keep Up: Yes"]
				slot.data[,(my.label):=as.factor(get(my.label))]
			}
		}


		### MOVE_UP_STAY_UP_STATUS Calculation

		if ("MOVE_UP_STAY_UP" %in% target.args[['target.level']] & (sgp.projections.lagged | sgp.projections.lagged.baseline)) {

			move.up.stay.up.levels <- getTargetAchievementLevels(state, "MOVE_UP_STAY_UP")
			slot.data[,MOVE_UP_STAY_UP_STATUS_INITIAL:=getTargetInitialStatus(ACHIEVEMENT_LEVEL_PRIOR, state, status.type="MOVE_UP_STAY_UP")]

			for (i in seq_along(target.args[['my.sgp']])) {
				if (!grepl("BASELINE", target.args[['my.sgp']][i])) my.label <- "MOVE_UP_STAY_UP_STATUS" else my.label <- "MOVE_UP_STAY_UP_STATUS_BASELINE"
				if (my.label %in% names(slot.data)) slot.data[,(my.label):=NULL]
				slot.data[,(my.label):=rep(as.character(NA), dim(slot.data)[1])]

				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & get(target.args[['my.sgp']][i]) >= get(target.args[['my.sgp.target.move.up.stay.up']][i]),
					(my.label):="Stay Up: Yes"]
				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & get(target.args[['my.sgp']][i]) < get(target.args[['my.sgp.target.move.up.stay.up']][i]),
					(my.label):="Stay Up: No"]
				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & get(target.args[['my.sgp']][i]) >= get(target.args[['my.sgp.target.move.up.stay.up']][i]),
					(my.label):="Move Up: Yes"]
				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & get(target.args[['my.sgp']][i]) < get(target.args[['my.sgp.target.move.up.stay.up']][i]),
					(my.label):="Move Up: No"]

				### MOVE_UP_STAY_UP clean up based upon reality

				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & get(my.label) == "Stay Up: Yes" &
					ACHIEVEMENT_LEVEL %in% move.up.stay.up.levels[['NO']], (my.label):="Stay Up: No"]
				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & get(my.label) == "Move Up: No" &
					ACHIEVEMENT_LEVEL %in% move.up.stay.up.levels[['YES']], (my.label):="Move Up: Yes"]
				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" & get(my.label) == "Move Up: Yes" &
					ACHIEVEMENT_LEVEL %in% move.up.stay.up.levels[['NO']] &
					GRADE == max(type.convert(GRADE[!is.na(get(target.args[['my.sgp.target.move.up.stay.up']]))], as.is=TRUE)) &
					CONTENT_AREA %in% terminal.content_areas, (my.label):="Move Up: No"]
				slot.data[MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" & get(my.label) == "Stay Up: No" &
					ACHIEVEMENT_LEVEL %in% move.up.stay.up.levels[['YES']] &
					GRADE == max(type.convert(GRADE[!is.na(get(target.args[['my.sgp.target.move.up.stay.up']]))], as.is=TRUE)) &
					CONTENT_AREA %in% terminal.content_areas, (my.label):="Stay Up: Yes"]
				slot.data[,(my.label):=as.factor(get(my.label))]
			}
		}

		for (i in intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL", "MOVE_UP_STAY_UP_STATUS_INITIAL"))) {
			slot.data[,(i):=NULL]
		}

	} ## END sgp.projections.lagged | sgp.projections.lagged.baseline


	###################################################################################################
	### Create SGP Scale Score targets (Cohort and Baseline referenced) if requested
	###################################################################################################

	if (sgp.target.scale.scores) {

		if (!exists("target.args")) target.args <- get.target.arguments(SGP::SGPstateData[[state]][["Growth"]][["System_Type"]], target.type, projection.unit.label, year.for.equate)
		tmp.target.list <- list()
		for (target.type.iter in target.args[['sgp.target.scale.scores.types']]) {
			for (target.level.iter in target.args[['target.level']]) {
				tmp.target.list[[paste(target.type.iter, target.level.iter)]] <-
					data.table(getTargetSGP(sgp_object, slot.data, content_areas, state, years, target.type.iter, target.level.iter, max.sgp.target.years.forward, return.lagged.status=FALSE, fix.duplicates=fix.duplicates),
						key=c(getKey(sgp_object), "SGP_PROJECTION_GROUP"))
			}
		}
		tmp.target.data <- data.table(Reduce(function(x, y) merge(x, y, all=TRUE, by=intersect(names(y), names(x))), tmp.target.list[!sapply(tmp.target.list, function(x) dim(x)[1]==0)],
			accumulate=FALSE), key=getKey(slot.data))

		if (!is.null(fix.duplicates)) {
			if (any(grepl("_DUPS_[0-9]*", tmp.target.data[["ID"]]))) {
				invisible(tmp.target.data[, ID := gsub("_DUPS_[0-9]*", "", ID)])
				invisible(tmp.target.data[!is.na(DUPS_FLAG), N := seq.int(.N), by=c(getKey(tmp.target.data))])
			}
		}

		for (projection_group.iter in unique(tmp.target.data[['SGP_PROJECTION_GROUP']])) {
			for (target.type.iter in target.args[['sgp.target.scale.scores.types']]) {
				tmp.target.level.names <-
					as.character(sapply(target.args[['target.level']], function(x) getTargetName(state, target.type.iter, x, max.sgp.target.years.forward, "SGP_TARGET", projection.unit.label, projection_group.iter)))
				if (any(!tmp.target.level.names %in% names(tmp.target.data))) {
					tmp.target.data[,tmp.target.level.names[!tmp.target.level.names %in% names(tmp.target.data)]:=as.integer(NA)]
				}

				sgp_object <- getTargetScaleScore(
					sgp_object,
					state,
					getTargetData(tmp.target.data, projection_group.iter, tmp.target.level.names),
					target.type.iter,
					tmp.target.level.names,
					getYearsContentAreasGrades(state, years=unique(tmp.target.data[SGP_PROJECTION_GROUP==projection_group.iter], by='YEAR')[['YEAR']], content_areas=unique(tmp.target.data[SGP_PROJECTION_GROUP==projection_group.iter], by='CONTENT_AREA')[['CONTENT_AREA']]),
					sgp.config=sgp.config,
					projection_group.identifier=projection_group.iter,
					sgp.projections.equated=if (grepl("baseline", target.type.iter)) NULL else sgp.projections.equated,
					SGPt=SGPt,
					fix.duplicates=fix.duplicates,
					parallel.config=parallel.config)
			}
		}
	} ### END if (sgp.target.scale.scores)

	### Final clean and put slot.data into @Data slot

	if ("DUPS_FLAG" %in% names(slot.data)) invisible(slot.data[, DUPS_FLAG := NULL])
	if (any(grepl("SCALE_SCORE_PRIOR_[0-9]", names(slot.data)))) invisible(slot.data[, grep("SCALE_SCORE_PRIOR_[0-9]", names(slot.data), value=TRUE) := NULL])

	setkeyv(slot.data, getKey(slot.data))
	sgp_object@Data <- slot.data

	messageSGP(c(tmp.messages, paste("Finished combineSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n")))

	return(sgp_object)
} ## END combineSGP Function
