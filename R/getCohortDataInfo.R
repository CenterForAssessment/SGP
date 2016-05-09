`getCohortDataInfo` <-
function(sgp.data,
	par.sgp.config) {

	for (sgp.iter in seq(length(par.sgp.config))) {
		tmp.grades.names <- paste("GRADE", tail(par.sgp.config[[sgp.iter]][['sgp.panel.years']], 2), tail(par.sgp.config[[sgp.iter]][['sgp.content.areas']], 2), sep=".")
		tmp.scale_scores.names <- paste("SCALE_SCORE", tail(par.sgp.config[[sgp.iter]][['sgp.panel.years']], 2), tail(par.sgp.config[[sgp.iter]][['sgp.content.areas']], 2), sep=".")
		tmp.data <- getPanelData(sgp.data, "sgp.percentiles", par.sgp.config[[sgp.iter]])
		if (all(c(tmp.grades.names, tmp.scale_scores.names) %in% names(tmp.data))) {
			par.sgp.config[[sgp.iter]][['number_of_1_prior_cases']] <- dim(na.omit(tmp.data[,c("ID", tmp.grades.names, tmp.scale_scores.names), with=FALSE]))[1]
		} else {
			par.sgp.config[[sgp.iter]][['number_of_1_prior_cases']] <- 0
		}
	}
	return(par.sgp.config)
} ### END getCohortDataInfo
