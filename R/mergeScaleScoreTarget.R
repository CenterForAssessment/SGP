`mergeScaleScoreTarget` <-
function(sgp_object,
		state,
		slot.data,
		years,
		sgp.target.scale.scores.merge
	) {

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
			tmp.dt <- rbindlist(tmp.list, fill=TRUE)
			tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
			tmp.cols <- grep("YEAR_1", names(tmp.dt), value=TRUE)
			invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
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
			tmp.dt <- rbindlist(tmp.list[grep("LAGGED", tmp.names)], fill=TRUE)
			tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
			tmp.cols <- grep("YEAR_1", names(tmp.dt), value=TRUE)
			invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
			tmp.dt <- rbindlist(tmp.list[grep("LAGGED", tmp.names, invert=TRUE)], fill=TRUE)
			tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
			tmp.cols <- grep("YEAR_1_CURRENT", names(tmp.dt), value=TRUE)
			invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
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
			tmp.dt <- rbindlist(tmp.list[grep("LAGGED", tmp.names)], fill=TRUE)
			tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
			tmp.cols <- setdiff(names(tmp.dt), c("ID", "GRADE", "SGP_PROJECTION_GROUP", "SGP_PROJECTION_GROUP_SCALE_SCORES"))
			invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
			tmp.dt <- rbindlist(tmp.list[grep("LAGGED", tmp.names, invert=TRUE)], fill=TRUE)
			tmp.index <- slot.data[tmp.dt[, getKey(slot.data), with=FALSE], which=TRUE, on=getKey(slot.data)]
			tmp.cols <- setdiff(names(tmp.dt), c("ID", "GRADE", "SGP_PROJECTION_GROUP", "SGP_PROJECTION_GROUP_SCALE_SCORES"))
			invisible(slot.data[tmp.index, (tmp.cols):=tmp.dt[,tmp.cols, with=FALSE]])
		}
		return(slot.data)
} ### END mergeScaleScoreTarget
