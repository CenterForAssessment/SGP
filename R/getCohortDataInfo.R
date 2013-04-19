`getCohortDataInfo` <- 
function(sgp.data,
	par.sgp.config) {

	for (sgp.iter in seq(length(par.sgp.config))) {
		tmp.grades <- paste(
				paste(
					paste("GRADE", tail(par.sgp.config[[sgp.iter]][['sgp.panel.years']], 2), tail(par.sgp.config[[sgp.iter]][['sgp.content.areas']], 2), sep="."),
					"=='", tail(par.sgp.config[[sgp.iter]][['sgp.grade.sequences']][[1]], 2), "'", sep=""), collapse=" & ")
		tmp.scale.scores <- paste(
					paste("!is.na(",
						paste("SCALE_SCORE", tail(rev(par.sgp.config[[sgp.iter]][['sgp.panel.years']]), 2), tail(rev(par.sgp.config[[sgp.iter]][['sgp.content.areas']]), 2), sep="."), ")", sep=""), collapse=" & ")
		par.sgp.config[[sgp.iter]][['panel_data_dimension']] <- 
			dim(as.data.table(getPanelData(sgp.data, "sgp.percentiles", par.sgp.config[[sgp.iter]]))[eval(parse(text=paste(tmp.grades, tmp.scale.scores, sep=" & ")))])
	}
	return(par.sgp.config)
} ### END getCohortDataInfo
