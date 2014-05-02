`getTargetSGP` <- 
function(sgp_object,
	content_areas, 
	state, 
	years, 
	target.type, 
	target.level,
	max.sgp.target.years.forward=3,
	subset.ids=NULL,
	return.lagged.status=TRUE) {

	TARGET_STATUS_INITIAL <- VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- FIRST_OBSERVATION <- LAST_OBSERVATION <- STATE <- NULL

	### Utility functions

	"%w/o%" <- function(x, y) x[!x %in% y]

	
	### Define variables

	tmp.sgpTarget.list <- list()

	catch_keep_move_functions <- c(min, max)

	max.sgp.target.years.forward.label <- max.sgp.target.years.forward
	if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) max.sgp.target.years.forward <- max.sgp.target.years.forward+1

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.projection.unit.label"]])) {
		sgp.projections.projection.unit.label <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.projection.unit.label"]]
	} else {
		sgp.projections.projection.unit.label <- "YEAR"
	}

	### Loop over different states (usually just 1 state)

	tmp.names <- getPercentileTableNames(sgp_object, content_areas, state, years, target.type)
	tmp.list <- list()

	if ("STATE" %in% names(sgp_object@Data)) {
		tmp.unique.states <- sort(unique(unlist(sapply(tmp.names, function(x) unique(sgp_object@SGP[['SGProjections']][[x]][['STATE']])))))
	} else {
		tmp.unique.states <- state
	}

	for (state.iter in tmp.unique.states) {
		level.to.get <- getTargetSGPLevel(state, state.iter, target.level)

		### Calculate Targets

		for (i in tmp.names) {
			cols.to.get.names <- names(sgp_object@SGP[["SGProjections"]][[i]])[
				grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))]
			num.years.to.get <- min(max.sgp.target.years.forward, length(cols.to.get.names))
			cols.to.get.names <- cols.to.get.names[as.integer(sapply(strsplit(sapply(sapply(cols.to.get.names, strsplit, paste("_", sgp.projections.projection.unit.label, "_", sep="")), tail, 1), "_"), head, 1)) <= num.years.to.get]
			if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) cols.to.get.names <- c("ACHIEVEMENT_LEVEL_PRIOR", cols.to.get.names)
			if ("STATE" %in% names(sgp_object@Data)) cols.to.get.names <- c("STATE", cols.to.get.names)
			cols.to.get <- sapply(cols.to.get.names, function(x) which(x==names(sgp_object@SGP[["SGProjections"]][[i]])))

			if ("STATE" %in% names(sgp_object@Data)) {
				tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,cols.to.get)])[STATE==state.iter]
			} else {
				tmp.list[[i]] <- data.table(
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]][,c(1,cols.to.get)])
			}
		}

		if (!is.null(subset.ids)) {
			tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=c("ID"))[subset.ids, nomatch=0]
		} else {
			tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE")
		}
	
		if ("YEAR_WITHIN" %in% names(sgp_object@Data)) {
			###  Assumes that any "canonical progression" will use the LAST_OBSERVATION for all (or at least the most recent) prior(s) in straight progressions
			if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) {
				tmp_object_1[, LAST_OBSERVATION := 1L]; year.within.key <- "LAST_OBSERVATION"
			}
			###  lagged progressions would still be based on the FIRST_OBSERVATION score (used to produce SGP)
			if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) {
				tmp_object_1[, FIRST_OBSERVATION := 1L]; year.within.key <- "FIRST_OBSERVATION"
			}
			setkeyv(tmp_object_1, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", year.within.key))
			setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", year.within.key))
			tmp_object_1 <- data.table(sgp_object@Data[,c(key(tmp_object_1), "YEAR_WITHIN"), with=FALSE], key=key(tmp_object_1))[tmp_object_1]
			jExp_Key <- c('ID', 'CONTENT_AREA', 'YEAR', 'VALID_CASE', 'YEAR_WITHIN')
		} else {
			setkeyv(tmp_object_1, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			jExp_Key <- c('ID', 'CONTENT_AREA', 'YEAR', 'VALID_CASE')
		}
		
		if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) {
			if ("YEAR_WITHIN" %in% names(sgp_object@Data)) {
				tmp_object_1 <- data.table(sgp_object@Data[,c(key(tmp_object_1), "ACHIEVEMENT_LEVEL"), with=FALSE], key=key(tmp_object_1))[tmp_object_1]
				setkeyv(sgp_object@Data, getKey(sgp_object))
			} else 	tmp_object_1 <- data.table(sgp_object@Data[,c(key(tmp_object_1), "ACHIEVEMENT_LEVEL"), with=FALSE], key=key(tmp_object_1))[tmp_object_1]
		}

		tmp_object_1[, paste(target.level, "STATUS_INITIAL", sep="_") := 
			getTargetInitialStatus(tmp_object_1[[grep("ACHIEVEMENT", names(tmp_object_1), value=TRUE)]], state, state.iter, target.level), with=FALSE]
		tmp_object_1 <- tmp_object_1[!is.na(get(paste(target.level, "STATUS_INITIAL", sep="_")))]

		## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

		jExpression <- parse(text=paste("{catch_keep_move_functions[[unclass(", target.level, "_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
			grep("ACHIEVEMENT", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
		tmp_object_2 <- tmp_object_1[, eval(jExpression), by = jExp_Key] # list(ID, CONTENT_AREA, YEAR, VALID_CASE)

		if (target.type %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline")) baseline.label <- "_BASELINE" else baseline.label <- NULL
		if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) projection.label <- "_CURRENT" else projection.label <- NULL
		if (target.level=="MOVE_UP_STAY_UP") target.level.label <- "_MOVE_UP_STAY_UP" else target.level.label <- NULL

		setnames(tmp_object_2, "V1", paste("SGP_TARGET", baseline.label, target.level.label, "_",  max.sgp.target.years.forward.label, "_YEAR", projection.label, sep=""))

		if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline") & return.lagged.status) {
			tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", grep("STATUS_INITIAL", names(tmp_object_1), value=TRUE)) := 
				list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[[grep("STATUS_INITIAL", names(tmp_object_1), value=TRUE)]]), with=FALSE]
		}

		tmp.sgpTarget.list[[state.iter]] <- tmp_object_2
	} ### END for state.iter
		return(data.table(rbind.fill(tmp.sgpTarget.list), key=getKey(sgp_object)))
} ### END getTargetSGP
