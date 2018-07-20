`mergeScaleScoreTarget` <-
function(sgp_object,
		slot.data,
		years,
		sgp.target.scale.scores.merge
	) {

		tmp.list <- list()
		if (identical(sgp.target.scale.scores.merge, "1_year_lagged")) {
			tmp.names <- grep(paste(years, "LAGGED.TARGET_SCALE_SCORES", sep="."), names(sgp_object@SGP$SGProjections), value=TRUE)
			for (i in tmp.names) {
				tmp.list[[i]] <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]])
			}
			tmp.dt <- rbindlist(tmp.list, fill=TRUE)
			tmp.cols <- grep("YEAR_1", names(tmp.dt), value=TRUE)
			slot.data[tmp.dt, (tmp.cols):=mget(tmp.cols), on=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE")]
		}
		if (identical(sgp.target.scale.scores.merge, "1_year_lagged_current")) {
			tmp.names <- grep(paste(years, "TARGET_SCALE_SCORES", sep="."), names(sgp_object@SGP$SGProjections), value=TRUE)
			for (i in tmp.names) {
				tmp.list[[i]] <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]])
			}
			tmp.dt <- rbindlist(tmp.list, fill=TRUE)
			tmp.cols <- grep("YEAR_1", names(tmp.dt), value=TRUE)
			slot.data[tmp.dt, (tmp.cols):=mget(tmp.cols), on=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE")]
		}
		if (identical(sgp.target.scale.scores.merge, "all_years_lagged_current")) {
			tmp.names <- grep(paste(years, "TARGET_SCALE_SCORES", sep="."), names(sgp_object@SGP$SGProjections), value=TRUE)
			for (i in tmp.names) {
				tmp.list[[i]] <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					YEAR=getTableNameYear(i),
					sgp_object@SGP[["SGProjections"]][[i]])
			}
			tmp.dt <- rbindlist(tmp.list, fill=TRUE)
			tmp.cols <- setdiff(names(tmp.dt), c("ID", "GRADE", "SGP_PROJECTION_GROUP", "SGP_PROJECTION_GROUP_SCALE_SCORES"))
			slot.data[tmp.dt, (tmp.cols):=mget(tmp.cols), on=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE")]
		}
		return(slot.data)
} ### END mergeScaleScoreTarget
