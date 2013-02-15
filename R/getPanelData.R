`getPanelData` <- 
function(sgp.data,
	sgp.type,
	sgp.iter) {

	YEAR <- CONTENT_AREA <- V5 <- ID <- GRADE <- SCALE_SCORE <- YEAR_WITHIN <- tmp.timevar <- FIRST_OBSERVATION <- LAST_OBSERVATION <- NULL

	if (sgp.type=="sgp.percentiles") {

		if ("YEAR_WITHIN" %in% names(sgp.data)) {
			tmp.lookup <- data.table("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])),
				tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sgp.iter[["sgp.grade.sequences"]][[1]],
				tail(sgp.iter[["sgp.panel.years.within"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), FIRST_OBSERVATION=as.integer(NA), LAST_OBSERVATION=as.integer(NA))
			tmp.lookup[grep("FIRST", V5, ignore.case=TRUE), FIRST_OBSERVATION:=1L]; tmp.lookup[grep("LAST", V5, ignore.case=TRUE), LAST_OBSERVATION:=1L]; tmp.lookup[,V5:=NULL]

			tmp.lookup.list <- list()
			for (i in c("FIRST_OBSERVATION", "LAST_OBSERVATION")) {
				setkeyv(sgp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
				setkeyv(tmp.lookup, c("VALID_CASE", paste("V", 2:4, sep=""), i))
				suppressWarnings(tmp.lookup.list[[i]] <- data.table(sgp.data[tmp.lookup[get(i)==1], nomatch=0][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, i, sep="."), with=FALSE][,
					list(ID, GRADE, SCALE_SCORE, YEAR_WITHIN, tmp.timevar)], key="ID")) ### Could be NULL and result in a warning
			}
			if (tail(sgp.iter[['sgp.panel.years']], 1)==head(tail(sgp.iter[['sgp.panel.years']], 2), 1)) {
				tmp.ids <- intersect(tmp.lookup.list[[1]][['ID']], tmp.lookup.list[[2]][['ID']])
				tmp.ids <- tmp.ids[tmp.lookup.list[[1]][tmp.ids][['YEAR_WITHIN']] < tmp.lookup.list[[2]][tmp.ids][['YEAR_WITHIN']]]
				tmp.lookup.list <- lapply(tmp.lookup.list, function(x) x[tmp.ids])
			}
			return(as.data.frame(reshape(
				rbindlist(tmp.lookup.list),
					idvar="ID", 
					timevar="tmp.timevar", 
					drop=names(sgp.data)[!names(tmp.lookup.list[[1]]) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR_WITHIN", "tmp.timevar")], 
					direction="wide")))
		} else {
			tmp.lookup <- SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])),
				tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sgp.iter[["sgp.grade.sequences"]][[1]])

			return(as.data.frame(reshape(
				sgp.data[tmp.lookup, nomatch=0][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep="."), with=FALSE],
					idvar="ID",
					timevar="tmp.timevar",
					drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "tmp.timevar")],
					direction="wide")))
		}
	}


	if (sgp.type=="sgp.projections") {
		return(as.data.frame(reshape(
			sgp.data[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
				tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
				sgp.iter[["sgp.projection.grade.sequences"]][[1]]), nomatch=0][,
				'tmp.timevar' := paste(YEAR, CONTENT_AREA, sep="."), with=FALSE],
		idvar="ID",
		timevar="tmp.timevar",
		drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "tmp.timevar")],
		direction="wide")))
	}


	if (sgp.type=="sgp.projections.lagged") {
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
				head(sgp.iter[["sgp.grade.sequences"]][[1]], -1)), nomatch=0][,
				'tmp.timevar' := paste(YEAR, CONTENT_AREA, sep="."), with=FALSE],
		idvar="ID",
		timevar="tmp.timevar",
		drop=names(sgp.data)[!names(sgp.data) %in% c("ID", "GRADE", "SCALE_SCORE", "tmp.timevar", "ACHIEVEMENT_LEVEL")],
		direction="wide")))
	}
} ## END getPanelData
