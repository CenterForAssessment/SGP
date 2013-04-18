`getCohortDataInfo` <- 
function(sgp.data,
	par.sgp.config) {

	for (sgp.iter in seq(length(par.sgp.config))) {
		tmp.grades <- paste("GRADE", tail(rev(par.sgp.config[[sgp.iter]][['sgp.panel.years']]), 2), tail(rev(par.sgp.config[[sgp.iter]][['sgp.content.areas']]), 2), sep=".")
		tmp.scale.scores <- paste("GRADE", tail(rev(par.sgp.config[[sgp.iter]][['sgp.panel.years']]), 2), tail(rev(par.sgp.config[[sgp.iter]][['sgp.content.areas']]), 2), sep=".")
		par.sgp.config[[sgp.iter]][['panel_data_dimension']] <- dim(getPanelData(sgp.data, "sgp.percentiles", par.sgp.config[[sgp.iter]]))
	}
	return(par.sgp.config)
} ### END getCohortDataInfo
