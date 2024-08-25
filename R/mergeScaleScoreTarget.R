`mergeScaleScoreTarget` <-
function(sgp_object,
		state,
		slot.data,
		years,
		sgp.target.scale.scores.merge
	) {

		### Utility functions
		groupNames <- function(table.names) {
			table.names.groups <- list()
			tmp.lagged.names <- grep("LAGGED", table.names, value=TRUE)
			tmp.current.names <- grep("LAGGED", table.names, value=TRUE, invert=TRUE)
			tmp.lagged.baseline.names <- grep("BASELINE", tmp.lagged.names, value=TRUE)
			tmp.lagged.names <- setdiff(tmp.lagged.names, tmp.lagged.baseline.names)
			tmp.current.baseline.names <- grep("BASELINE", tmp.current.names, value=TRUE)
			tmp.current.names <- setdiff(tmp.current.names, tmp.current.baseline.names)

			table.names.groups[['LAGGED.BASELINE']] <- tmp.lagged.baseline.names
			table.names.groups[['CURRENT.BASELINE']] <- tmp.current.baseline.names
			table.names.groups[['LAGGED']] <- tmp.lagged.names
			table.names.groups[['CURRENT']] <- tmp.current.names

			return(table.names.groups)
		}

		tmp.list <- list()
		if (identical(sgp.target.scale.scores.merge, "1_year_lagged")) {
			tmp.names <- unique(c(grep(paste(years, "LAGGED.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "LAGGED.BASELINE.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE)))
			for (i in tmp.names) {
				tmp.list[[i]] <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1L],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]])
				if (any(duplicated(tmp.list[[i]], by=getKey(tmp.list[[i]])))) {
					tmp.list[[i]] <- getPreferredSGP(tmp.list[[i]], state=state, type="TARGET")
				}
			}

			table.names.groups.list <- groupNames(tmp.names)
			for (names.iter in names(table.names.groups.list)) {
				if (length(table.names.groups.list[[names.iter]]) > 0) {
					tmp.dt <- rbindlist(tmp.list[table.names.groups.list[[names.iter]]], fill=TRUE)
					tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
					tmp.cols <- setdiff(names(tmp.dt), c("ID", "GRADE", "SGP_PROJECTION_GROUP", "SGP_PROJECTION_GROUP_SCALE_SCORES"))
					invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
				}
			}
		}

		if (identical(sgp.target.scale.scores.merge, "1_year_lagged_current")) {
			tmp.names <- unique(c(grep(paste(years, "TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "BASELINE.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "LAGGED.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "LAGGED.BASELINE.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE)))
			for (i in tmp.names) {
				tmp.list[[i]] <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1L],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]])
				if (any(duplicated(tmp.list[[i]], by=getKey(tmp.list[[i]])))) {
					tmp.list[[i]] <- getPreferredSGP(tmp.list[[i]], state=state, type="TARGET")
				}
			}

			table.names.groups.list <- groupNames(tmp.names)
			for (names.iter in names(table.names.groups.list)) {
				if (length(table.names.groups.list[[names.iter]]) > 0) {
					tmp.dt <- rbindlist(tmp.list[table.names.groups.list[[names.iter]]], fill=TRUE)
					tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
					tmp.cols <- setdiff(names(tmp.dt), c("ID", "GRADE", "SGP_PROJECTION_GROUP", "SGP_PROJECTION_GROUP_SCALE_SCORES"))
					invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
				}
			}
		}

		if (identical(sgp.target.scale.scores.merge, "all_years_lagged_current")) {
			tmp.names <- unique(c(grep(paste(years, "TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "BASELINE.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "LAGGED.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE),
							grep(paste(years, "LAGGED.BASELINE.TARGET_SCALE_SCORES", sep=".", collapse="|"), names(sgp_object@SGP$SGProjections), value=TRUE)))
			for (i in tmp.names) {
				tmp.list[[i]] <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1L],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]])
				if (any(duplicated(tmp.list[[i]], by=getKey(tmp.list[[i]])))) {
					tmp.list[[i]] <- getPreferredSGP(tmp.list[[i]], state=state, type="TARGET")
				}
			}

			table.names.groups.list <- groupNames(tmp.names)
			for (names.iter in names(table.names.groups.list)) {
				if (length(table.names.groups.list[[names.iter]]) > 0) {
					tmp.dt <- rbindlist(tmp.list[table.names.groups.list[[names.iter]]], fill=TRUE)
					tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
					tmp.cols <- setdiff(names(tmp.dt), c("ID", "GRADE", "SGP_PROJECTION_GROUP", "SGP_PROJECTION_GROUP_SCALE_SCORES"))
					invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
				}
			}
		}

		return(slot.data)
} ### END mergeScaleScoreTarget
