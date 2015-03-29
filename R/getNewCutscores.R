`getNewCutscores` <- 
function(content_area,
	content_area_labels,
	year,
	grade,
	Cutscores) {
	return(sort(Cutscores[[content_area]][list(content_area_labels, year, grade)][["CUTSCORES"]]))
}
