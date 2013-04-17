`getCohortDataInfo` <- 
function(sgp.data,
	par.sgp.config) {

	for (sgp.iter in seq(length(par.sgp.config))) {
		par.sgp.config[[sgp.iter]][['panel_data_dimension']] <- dim(getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", par.sgp.config[sgp.iter]))
	}
	return(par.sgp.config)
} ### END getCohortDataInfo
