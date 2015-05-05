`getNewCutscores` <- 
function(content_area,
	content_area_labels,
	year,
	grade,
	Cutscores) {
	if (is.na(content_area) | is.na(grade)) {
		return(NULL)
	} else {
		return(sort(Cutscores[[content_area]][list(content_area_labels, year, grade)][["CUTSCORES"]]))
	}
}
