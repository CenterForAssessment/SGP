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
	max.lagged.sgp.target.years.forward=4,
	use.cohort.for.baseline.when.missing=NULL,
	update.all.years=FALSE) {

	started.at <- proc.time()
	message(paste("Started combineSGP", date()))

	tmp.messages <- NULL

	### Create slot.data from sgp_object@Data

	slot.data <- sgp_object@Data


	### Create state (if missing) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		if (any(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(toupper(x), tmp.name))!=-1)) {
			state <- c(state.abb, "DEMO", "AOB")[which(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(toupper(x), tmp.name))!=-1)[1]]
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
			my.sgp.target.move.up.stay.up <- "SGP_TARGET_MOVE_UP_STAY_UP"
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Baseline Referenced") {
			sgp.projections.lagged <- FALSE 
			my.sgp <- "SGP_BASELINE"
			my.sgp.target <- "SGP_TARGET_BASELINE"
			my.sgp.target.move.up.stay.up <- "SGP_TARGET_MOVE_UP_STAY_UP_BASELINE"
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort and Baseline Referenced") {
			my.sgp <- "SGP"
			my.sgp.target <- "SGP_TARGET"
			my.sgp.target.move.up.stay.up <- "SGP_TARGET_MOVE_UP_STAY_UP"
		}
	}

	if (is.null(use.cohort.for.baseline.when.missing)) {
		if (is.null(SGPstateData[[state]][["SGP_Configuration"]][["use.cohort.for.baseline.when.missing"]])) {
			use.cohort.for.baseline.when.missing <- FALSE
		} else {
			use.cohort.for.baseline.when.missing <- SGPstateData[[state]][["SGP_Configuration"]][["use.cohort.for.baseline.when.missing"]]
		}
	}

	catch_keep_move_functions <- c(min, max)


	## Utility functions

	"%w/o%" <- function(x,y) x[!x %in% y]

	get.initial_status <- function(achievement_level_prior, status.type="Catch Up/Keep Up") {

		if (!all(levels(achievement_level_prior) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])) {
			levels(achievement_level_prior)[!levels(achievement_level_prior) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]] <- NA
		}

		if (status.type=="Catch Up/Keep Up") {
			levels(achievement_level_prior) <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]] 
			levels(achievement_level_prior) <- c("Catching Up", "Keeping Up")
			return(factor(achievement_level_prior, ordered=FALSE))
		}

		if (status.type=="Move Up/Stay Up") {
			achievement.level.for.start <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")]
			achievement_level_prior[!achievement_level_prior %in% achievement.level.for.start] <- NA 
			achievement_level_prior <- unclass(factor(achievement_level_prior))
			achievement_level_prior[achievement_level_prior > 2] <- 2
			return(factor(achievement_level_prior, levels=1:2, labels=c("Moving Up", "Staying Up"), ordered=FALSE))
		}
	} ### END get.initial_status

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
	### Check update.all.years
	############################################################################


	if (update.all.years) {
		variables.to.null.out <- c("SGP", "SGP_LEVEL", "SGP_STANDARD_ERROR", "SCALE_SCORE_PRIOR", "SGP_BASELINE", "SGP_LEVEL_BASELINE", "SGP_TARGET", "SGP_TARGET_MU",
			"SGP_TARGET_MOVE_UP_STAY_UP", "SGP_TARGET_MOVE_UP_STAY_UP_BASELINE", "ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL", "SGP_TARGET_BASELINE", 
			"CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STATUS", "MOVE_UP_STAY_UP_STATUS")
		for (tmp.variables.to.null.out in intersect(names(slot.data), variables.to.null.out)) {
			slot.data[,tmp.variables.to.null.out:=NULL, with=FALSE]
		}
	}


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
					YEAR=unlist(strsplit(i, "[.]"))[2],
					sgp_object@SGP[["SGPercentiles"]][[i]])
		}

		tmp.data <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))

		variables.to.merge <- names(tmp.data) %w/o% key(slot.data)
		for (tmp.merge.variable in variables.to.merge) {
			invisible(slot.data[tmp.data[,key(slot.data), with=FALSE], tmp.merge.variable := tmp.data[, tmp.merge.variable, with=FALSE], with=FALSE, mult="first"])
		}
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
				YEAR=unlist(strsplit(i, "[.]"))[2],
				sgp_object@SGP[["SGPercentiles"]][[i]])

			if (is.na(unlist(strsplit(i, "[.]"))[3])) { ### If cohort referenced SGP are to be included in baseline SGP (e.g., Georgia)
				names(tmp.list[[i]])[names(tmp.list[[i]])=="SGP"] <- "SGP_BASELINE"
				if ("SGP_LEVEL" %in% names(tmp.list[[i]])) names(tmp.list[[i]])[names(tmp.list[[i]])=="SGP_LEVEL"] <- "SGP_LEVEL_BASELINE"
			}
		}

		tmp.data <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))

		variables.to.merge <- names(tmp.data) %w/o% key(slot.data)
		for (tmp.merge.variable in variables.to.merge) {
			invisible(slot.data[tmp.data[,key(slot.data), with=FALSE], tmp.merge.variable := tmp.data[, tmp.merge.variable, with=FALSE], with=FALSE, mult="first"])
		}
		setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		rm(tmp.list); suppressWarnings(gc())
	}


	######################################################################################
	### Create SGP targets (Cohort and Baseline referenced) and merge with student data
	######################################################################################

	if (length(get.percentile.names("sgp.projections.lagged")) == 0) {
		 tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged projections available in SGP slot. No student growth projection targets will be produced.\n")
		 sgp.projections.lagged <- FALSE
	}
	if (length(get.percentile.names("sgp.projections.lagged.baseline")) == 0) {
		tmp.messages <- c(tmp.messages, "\tNOTE: No SGP lagged baseline projections available in SGP slot. No baseline referenced student growth projection targets will be produced.\n")
		sgp.projections.lagged.baseline <- FALSE
	}

	### Calculate Targets
 
	if (sgp.projections.lagged | sgp.projections.lagged.baseline) { 

		ID <- CONTENT_AREA <- YEAR <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- MOVE_UP_STAY_UP_STATUS_INITIAL <- VALID_CASE <- NULL
		level.to.get.catch.up.keep.up <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1
		if (length(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")) > 1) {
			level.to.get.move.up.stay.up <- level.to.get.catch.up.keep.up+1
		} else {
			level.to.get.move.up.stay.up <- NULL
		}

		#################################################################################
		## Cohort referenced lagged SGP targets and Catch Up Keep Up Status Variable
		#################################################################################

		if (sgp.projections.lagged) {

			tmp.names <- get.percentile.names("sgp.projections.lagged")	
			tmp.list <- list()
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			for (i in tmp.names) {
				cols.to.get.names <- names(sgp_object@SGP[["SGProjections"]][[i]])[
					grep(paste("LEVEL_", level.to.get.catch.up.keep.up, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))]
				num.years.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get.names))
				cols.to.get.names <- cols.to.get.names[as.integer(sapply(sapply(cols.to.get.names, strsplit, "_"), tail, 1) ) <= num.years.to.get]
				cols.to.get <- sapply(cols.to.get.names, function(x) which(x==names(sgp_object@SGP[["SGProjections"]][[i]])))
				tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=unlist(strsplit(i, "[.]"))[2],
					CATCH_UP_KEEP_UP_STATUS_INITIAL=get.initial_status(sgp_object@SGP[["SGProjections"]][[i]][["ACHIEVEMENT_LEVEL_PRIOR"]]),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get)])
			}

			tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

			## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

			jExpression <- parse(text=paste("{catch_keep_move_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
				grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
			tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
			setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET")
			invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["CATCH_UP_KEEP_UP_STATUS_INITIAL"]]), with=FALSE])
			setkeyv(tmp_object_2, key(slot.data))

			variables.to.merge <- names(tmp_object_2) %w/o% key(slot.data)
			for (tmp.merge.variable in variables.to.merge) {
				invisible(slot.data[tmp_object_2[,key(slot.data), with=FALSE], tmp.merge.variable := tmp_object_2[, tmp.merge.variable, with=FALSE], with=FALSE, mult="first"])
			}
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			rm(tmp.list); suppressWarnings(gc())

			### Move Up/Stay Up Calculations (if possible)

			if (!is.null(level.to.get.move.up.stay.up)) {

			tmp.list <- list()
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			for (i in tmp.names) {
				cols.to.get.names <- names(sgp_object@SGP[["SGProjections"]][[i]])[
					grep(paste("LEVEL_", level.to.get.move.up.stay.up, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))]
				num.years.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get.names))
				cols.to.get.names <- cols.to.get.names[as.integer(sapply(sapply(cols.to.get.names, strsplit, "_"), tail, 1) ) <= num.years.to.get]
				cols.to.get <- sapply(cols.to.get.names, function(x) which(x==names(sgp_object@SGP[["SGProjections"]][[i]])))
				tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=unlist(strsplit(i, "[.]"))[2],
					MOVE_UP_STAY_UP_STATUS_INITIAL=get.initial_status(sgp_object@SGP[["SGProjections"]][[i]][["ACHIEVEMENT_LEVEL_PRIOR"]], "Move Up/Stay Up"),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get)])
			}

			tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))[!is.na(MOVE_UP_STAY_UP_STATUS_INITIAL)]

			## Find min/max of targets based upon MOVE_UP_STAY_UP_STATUS_INITIAL status

			jExpression <- parse(text=paste("{catch_keep_move_functions[[unclass(MOVE_UP_STAY_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
				grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
			tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
			setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET_MOVE_UP_STAY_UP")
			invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "MOVE_UP_STAY_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["MOVE_UP_STAY_UP_STATUS_INITIAL"]]), with=FALSE])
			setkeyv(tmp_object_2, key(slot.data))

			variables.to.merge <- names(tmp_object_2) %w/o% key(slot.data)
			for (tmp.merge.variable in variables.to.merge) {
				invisible(slot.data[tmp_object_2[,key(slot.data), with=FALSE], tmp.merge.variable := tmp_object_2[, tmp.merge.variable, with=FALSE], with=FALSE, mult="first"])
			}
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			rm(tmp.list); suppressWarnings(gc())

			} ### END Move Up/Stay Up calculations

		} ## END if (sgp.projections.lagged)


		##############################################################################################
		### Lagged Baseline Student Growth Projection Targets and Catch Up Keep Up Variable
		##############################################################################################

		if (sgp.projections.lagged.baseline) {

			tmp.names <- get.percentile.names("sgp.projections.lagged.baseline")	
			tmp.list <- list()
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			for (i in tmp.names) {
				 cols.to.get <- grep(paste("LEVEL_", level.to.get.catch.up.keep.up, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
				 num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
				 tmp.list[[i]] <-data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=unlist(strsplit(i, "[.]"))[2],
					CATCH_UP_KEEP_UP_STATUS_INITIAL=get.initial_status(sgp_object@SGP[["SGProjections"]][[i]][,2]),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get[1:num.cols.to.get])])
			 }

			tmp_object_1 <- data.table(VALID_CASE="VALID_CASE", get.rbind.all.data(tmp.list))[!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

			 ## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

			jExpression <- parse(text=paste("{catch_keep_move_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
			grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
			tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
			setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET_BASELINE")
			invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "CATCH_UP_KEEP_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["CATCH_UP_KEEP_UP_STATUS_INITIAL"]]), with=FALSE])
			setkeyv(tmp_object_2, key(slot.data))

			variables.to.merge <- names(tmp_object_2) %w/o% key(slot.data)
			for (tmp.merge.variable in variables.to.merge) {
				invisible(slot.data[tmp_object_2[,key(slot.data), with=FALSE], tmp.merge.variable := tmp_object_2[, tmp.merge.variable, with=FALSE], with=FALSE, mult="first"])
			}
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			rm(tmp.list); suppressWarnings(gc())

			### Move up calculations (if possible)

			if (!is.null(level.to.get.move.up.stay.up)) {

			tmp.list <- list()
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			for (i in tmp.names) {
				cols.to.get.names <- names(sgp_object@SGP[["SGProjections"]][[i]])[
					grep(paste("LEVEL_", level.to.get.move.up.stay.up, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))]
				num.years.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get.names))
				cols.to.get.names <- cols.to.get.names[as.integer(sapply(sapply(cols.to.get.names, strsplit, "_"), tail, 1) ) <= num.years.to.get]
				cols.to.get <- sapply(cols.to.get.names, function(x) which(x==names(sgp_object@SGP[["SGProjections"]][[i]])))
				tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=unlist(strsplit(i, "[.]"))[2],
					MOVE_UP_STAY_UP_STATUS_INITIAL=get.initial_status(sgp_object@SGP[["SGProjections"]][[i]][["ACHIEVEMENT_LEVEL_PRIOR"]], "Move Up/Stay Up"),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,2,cols.to.get)])
			}

			tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=key(slot.data))[!is.na(MOVE_UP_STAY_UP_STATUS_INITIAL)]

			## Find min/max of targets based upon MOVE_UP_STAY_UP_STATUS_INITIAL status

			jExpression <- parse(text=paste("{catch_keep_move_functions[[unclass(MOVE_UP_STAY_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
				grep("ACHIEVEMENT_LEVEL_PRIOR", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
			tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
			setnames(tmp_object_2, dim(tmp_object_2)[2], "SGP_TARGET_MOVE_UP_STAY_UP_BASELINE")
			invisible(tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", "MOVE_UP_STAY_UP_STATUS_INITIAL") := list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[["MOVE_UP_STAY_UP_STATUS_INITIAL"]]), with=FALSE])
			setkeyv(tmp_object_2, key(slot.data))

			variables.to.merge <- names(tmp_object_2) %w/o% key(slot.data)
			for (tmp.merge.variable in variables.to.merge) {
				invisible(slot.data[tmp_object_2[,key(slot.data), with=FALSE], tmp.merge.variable := tmp_object_2[, tmp.merge.variable, with=FALSE], with=FALSE, mult="first"])
			}
			setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			rm(tmp.list); suppressWarnings(gc())


			} ### END Move up calculations


		} ## End if (sgp.projections.lagged.baseline)


		#################################################
		## Create CATCH_UP_KEEP_UP_STATUS variable
		#################################################

		if ("CATCH_UP_KEEP_UP_STATUS" %in% names(slot.data)) slot.data$CATCH_UP_KEEP_UP_STATUS <- NULL 
		slot.data$CATCH_UP_KEEP_UP_STATUS <- NA

		### CATCH_UP_KEEP_UP BASED UPON SGP versus SGP_TARGET

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
			slot.data[[my.sgp]] >= slot.data[[my.sgp.target]]] <- "Keep Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
			slot.data[[my.sgp]] < slot.data[[my.sgp.target]]] <- "Keep Up: No"

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
			slot.data[[my.sgp]] >= slot.data[[my.sgp.target]]] <- "Catch Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
			slot.data[[my.sgp]] < slot.data[[my.sgp.target]]] <- "Catch Up: No"


		### CATCH_UP_KEEP_UP clean up based upon reality

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
			slot.data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" &
			as.numeric(slot.data$ACHIEVEMENT_LEVEL) <= level.to.get.catch.up.keep.up] <- "Keep Up: No"

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
			slot.data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: No" &
			as.numeric(slot.data$ACHIEVEMENT_LEVEL) > level.to.get.catch.up.keep.up] <- "Catch Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
			slot.data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" &
			as.numeric(slot.data$ACHIEVEMENT_LEVEL) <= level.to.get.catch.up.keep.up &
			slot.data$GRADE == max(slot.data$GRADE[!is.na(slot.data[[my.sgp.target]])])] <- "Catch Up: No"

		slot.data$CATCH_UP_KEEP_UP_STATUS[
			slot.data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
			slot.data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: No" &
			as.numeric(slot.data$ACHIEVEMENT_LEVEL) > level.to.get.catch.up.keep.up &
			slot.data$GRADE == max(slot.data$GRADE[!is.na(slot.data[[my.sgp.target]])])] <- "Keep Up: Yes"

		slot.data$CATCH_UP_KEEP_UP_STATUS <- as.factor(slot.data$CATCH_UP_KEEP_UP_STATUS)


		#################################################
		## Create MOVE_UP_STAY_UP_STATUS variable
		#################################################

		if (!is.null(level.to.get.move.up.stay.up)) {

			if ("MOVE_UP_STAY_UP_STATUS" %in% names(slot.data)) slot.data$MOVE_UP_STAY_UP_STATUS <- NULL 
			slot.data$MOVE_UP_STAY_UP_STATUS <- NA

			### MOVE_UP_STAY_UP BASED UPON SGP versus SGP_TARGET

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" &
				slot.data[[my.sgp]] >= slot.data[[my.sgp.target.move.up.stay.up]]] <- "Stay Up: Yes"

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" &
				slot.data[[my.sgp]] < slot.data[[my.sgp.target.move.up.stay.up]]] <- "Stay Up: No"

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" &
				slot.data[[my.sgp]] >= slot.data[[my.sgp.target.move.up.stay.up]]] <- "Move Up: Yes"

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" &
				slot.data[[my.sgp]] < slot.data[[my.sgp.target.move.up.stay.up]]] <- "Move Up: No"


			### MOVE_UP_STAY_UP clean up based upon reality

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" &
				slot.data$MOVE_UP_STAY_UP_STATUS == "Stay Up: Yes" &
				as.numeric(slot.data$ACHIEVEMENT_LEVEL) <= level.to.get.move.up.stay.up] <- "Stay Up: No"

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" &
				slot.data$MOVE_UP_STAY_UP_STATUS == "Move Up: No" &
				as.numeric(slot.data$ACHIEVEMENT_LEVEL) > level.to.get.move.up.stay.up] <- "Move Up: Yes"

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Moving Up" &
				slot.data$MOVE_UP_STAY_UP_STATUS == "Move Up: Yes" &
				as.numeric(slot.data$ACHIEVEMENT_LEVEL) <= level.to.get.move.up.stay.up &
				slot.data$GRADE == max(slot.data$GRADE[!is.na(slot.data[[my.sgp.target.move.up.stay.up]])])] <- "Move Up: No"

			slot.data$MOVE_UP_STAY_UP_STATUS[
				slot.data$MOVE_UP_STAY_UP_STATUS_INITIAL == "Staying Up" &
				slot.data$MOVE_UP_STAY_UP_STATUS == "Stay Up: No" &
				as.numeric(slot.data$ACHIEVEMENT_LEVEL) > level.to.get.move.up.stay.up &
				slot.data$GRADE == max(slot.data$GRADE[!is.na(slot.data[[my.sgp.target.move.up.stay.up]])])] <- "Stay Up: Yes"

			slot.data$MOVE_UP_STAY_UP_STATUS <- as.factor(slot.data$MOVE_UP_STAY_UP_STATUS)

			} ### END Move up calculations

	} ## END sgp.projections.lagged | sgp.projections.lagged.baseline

	for (i in intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL", "MOVE_UP_STAY_UP_STATUS_INITIAL"))) {
		slot.data[[i]] <- NULL
	}

	setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
	sgp_object@Data <- slot.data

	message(c(tmp.messages, paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"), sep=""))

	return(sgp_object)

} ## END combineSGP Function
