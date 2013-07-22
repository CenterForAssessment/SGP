`getYearsContentAreasGrades` <- 
function(state,
	tmp.years) {

	CONTENT_AREA <- NULL

	tmp.list <- list()
	for (i in names(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]])) {
		tmp.df <- data.frame(GRADE=as.character(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[i]]), stringsAsFactors=FALSE)
		if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]])) {
			tmp.df <- CJ(tmp.df$GRADE, SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]]:tail(sort(tmp.years), 1))
		} else {
			tmp.df <- CJ(tmp.df$GRADE, tmp.years)
		}
		setnames(tmp.df, c("GRADE", "YEAR")) 
		tmp.list[[i]] <- data.table(CONTENT_AREA=i, tmp.df)
	}
	tmp.dt <- data.table(rbind.fill(tmp.list))
	setkeyv(tmp.dt, c("CONTENT_AREA", "GRADE", "YEAR"))
	return(tmp.dt[!is.na(CONTENT_AREA)])
} ## END getYearsContentAreasGrades
