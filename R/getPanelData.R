`getPanelData` <- 
function(sgp.data,
	sgp.type,
	sgp.iter,
	return.projections.with.targets=FALSE) {

	YEAR <- CONTENT_AREA <- NULL

	if (sgp.type=="sgp.percentiles") {
		return(as.data.frame(reshape(
			sgp.data[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sgp.iter[["sgp.grade.sequences"]][[1]]), nomatch=0][,
				'tmp.timevar' := paste(YEAR, CONTENT_AREA, sep="."), with=FALSE],
		idvar="ID",
		timevar="tmp.timevar",
		drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "tmp.timevar")],
		direction="wide")))
	}

	if (sgp.type=="sgp.projections") {
		if (!return.projections.with.targets) {
			return(as.data.frame(reshape(
				sgp.data[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
					tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
					sgp.iter[["sgp.projection.grade.sequences"]][[1]]), nomatch=0],
			idvar="ID",
			timevar="YEAR",
			drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
			direction="wide")))
		} else {
			##### STILL NEED TO CONSIDER HOW TO ADD IT STRAIGHT PROJECTION TARGETS ######
			return(as.data.frame(reshape(
				sgp.data[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
					tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
					sgp.iter[["sgp.projection.grade.sequences"]][[1]]), nomatch=0],
			idvar="ID",
			timevar="YEAR",
			drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
			direction="wide")))
		}
	}

	if (sgp.type=="sgp.projections.lagged") {
		if (!return.projections.with.targets) {
			return(as.data.frame(reshape(
				data.table(
					data.table(sgp.data, key="ID")[
						sgp.data[SJ("VALID_CASE", 
						tail(sgp.iter[["sgp.content.areas"]], 1), 
						tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.grade.sequences"]][[1]], 1))][,"ID", with=FALSE]], 
				key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[
				SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])-1),
					tail(head(sgp.iter[["sgp.panel.years"]], -1), length(sgp.iter[["sgp.grade.sequences"]][[1]])-1),
					head(sgp.iter[["sgp.grade.sequences"]][[1]], -1)), nomatch=0],
			idvar="ID",
			timevar="YEAR",
			drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR", "ACHIEVEMENT_LEVEL")],
			direction="wide")))
		} else {
			return(as.data.frame(data.table(sgp.data[SJ("VALID_CASE",
					tail(sgp.iter[["sgp.content.areas"]], 1),
					tail(sgp.iter[["sgp.panel.years"]], 1),
					tail(sgp.iter[["sgp.grade.sequences"]][[1]], 1))][, c("ID", grep("TARGET", names(sgp.data), value=TRUE)), with=FALSE], key="ID")[
						data.table(reshape(
							data.table(
								data.table(sgp.data, key="ID")[
									sgp.data[SJ("VALID_CASE", 
									tail(sgp.iter[["sgp.content.areas"]], 1), 
									tail(sgp.iter[["sgp.panel.years"]], 1), 
									tail(sgp.iter[["sgp.grade.sequences"]][[1]], 1))][,"ID", with=FALSE]], 
							key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[
							SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])-1),
								tail(head(sgp.iter[["sgp.panel.years"]], -1), length(sgp.iter[["sgp.grade.sequences"]][[1]])-1),
								head(sgp.iter[["sgp.grade.sequences"]][[1]], -1)), nomatch=0],
						idvar="ID",
						timevar="YEAR",
						drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR", "ACHIEVEMENT_LEVEL")],
						direction="wide"), key="ID")]))
		}
	}

} ## END getPanelData
