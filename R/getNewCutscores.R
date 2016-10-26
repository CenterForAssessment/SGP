`getNewCutscores` <-
function(content_area,
	content_area_label,
	year,
	grade,
	state,
	Cutscores) {


	### Utility function

	get.cutscore.year <- function(year, cutscore.years) {
		if (year %in% cutscore.years) return(year)
		if (year==sort(c(year, cutscore.years))[1]) return(as.character(NA))
		return(sort(cutscore.years)[which(year==sort(c(year, cutscore.years)))-1])
	}


	### Define variables

	if (!is.na(tmp.index <- match(content_area, names(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])))) {
		content_area_domain <- SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]][[tmp.index]]
	} else {
		content_area_domain <- content_area
	}

	cutscore.year <- get.cutscore.year(year, unique(Cutscores[[content_area_domain]][content_area][['YEAR']]))

	if (year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]]) {
		tmp.cutscore.label <- "CUTSCORES_TRANSFORMED"
	} else {
		tmp.cutscore.label <- "CUTSCORES"
	}


	### Return result

	if (is.na(content_area) | is.na(grade)) {
		return(NULL)
	} else {
		return(sort(Cutscores[[content_area_domain]][list(content_area_label, cutscore.year, grade)][[tmp.cutscore.label]]))
	}
} ### END getNewCutscores function
