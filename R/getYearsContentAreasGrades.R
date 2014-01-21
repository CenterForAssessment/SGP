`getYearsContentAreasGrades` <- 
function(state,
	years,
	content_areas=NULL,
	content_areas_domains=NULL) {

	CONTENT_AREA <- NULL

	tmp.list <- list()
	if (is.null(content_areas)) tmp.content_areas <- content_areas_domains else tmp.content_areas <- content_areas
	for (i in intersect(names(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]]), tmp.content_areas)) {
		if (!is.null(content_areas_domains) && !is.null(SGPstateData[[state]][["Student_Report_Information"]][['Content_Areas_Domains']])) {
			tmp.df <- data.frame(GRADE=as.character(unique(unlist(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][grep(i, SGPstateData[[state]][["Student_Report_Information"]][['Content_Areas_Domains']])]))), stringsAsFactors=FALSE)
		} else {
			tmp.df <- data.frame(GRADE=as.character(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[i]]), stringsAsFactors=FALSE)
		}

		if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]])) {
			tmp.df <- CJ(tmp.df$GRADE, intersect(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]]:tail(sort(years), 1), years))
		} else {
			tmp.df <- CJ(tmp.df$GRADE, years)
		}

		setnames(tmp.df, c("GRADE", "YEAR")) 
		tmp.list[[i]] <- data.table(CONTENT_AREA=i, tmp.df)
	}
	tmp.dt <- data.table(rbind.fill(tmp.list))
	setkeyv(tmp.dt, c("CONTENT_AREA", "GRADE", "YEAR"))
	return(tmp.dt[!is.na(CONTENT_AREA)])
} ## END getYearsContentAreasGrades
