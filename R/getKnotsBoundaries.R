`getKnotsBoundaries` <-
function(sgp.iter,
	state,
	sgp.iter.type="sgp.percentiles",
	sgp.year.baseline=NULL) {

	kb <- list()


	### Utility functions

	getContent_Areas <- function(sgp.iter, type) {
		if (!is.null(my.tmp <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
			tmp.content_areas <- unique(sgp.iter[[type]])
			tmp.content_areas <- unique(unlist(my.tmp[unlist(lapply(my.tmp, function(x) any(tmp.content_areas %in% x)))]))
			if (is.null(tmp.content_areas)) {
				messageSGP(paste0("\n\tNOTE:  Content Areas ", paste(unique(sgp.iter[[type]]), collapse=", "), " not in SGPstateData[['",state,"']][['SGP_Configuration']][['content_area.projection.sequence']].\n\t\t Knots and bounds for only those Content Areas will be used."))
				tmp.content_areas <- unique(sgp.iter[[type]])
			}
			return(tmp.content_areas)
		} else {
			return(unique(sgp.iter[[type]]))
		}
	}

	### Identify content areas needs for knots/boundaries

	if (sgp.iter.type=="sgp.percentiles") {
		tmp.content_areas <- getContent_Areas(sgp.iter, 'sgp.content.areas')
	}

	if (sgp.iter.type=="sgp.percentiles.baseline") {
		tmp.content_areas <- getContent_Areas(sgp.iter, 'sgp.baseline.content.areas')
	}

	if (sgp.iter.type=="sgp.projections") {
		tmp.content_areas <- getContent_Areas(sgp.iter, 'sgp.projection.content.areas')
	}

	if (sgp.iter.type=="sgp.projections.lagged") {
		tmp.content_areas <- getContent_Areas(sgp.iter, 'sgp.projection.content.areas')
	}

	if (sgp.iter.type=="sgp.projections.baseline") {
		tmp.content_areas <- getContent_Areas(sgp.iter, 'sgp.projection.baseline.content.areas')
	}

	if (sgp.iter.type=="sgp.projections.lagged.baseline") {
		tmp.content_areas <- getContent_Areas(sgp.iter, 'sgp.projection.baseline.content.areas')
	}


	### Create label(s) for Knots_Boundaries

	if (sgp.iter.type=="sgp.percentiles") {
		content_area.label <- tail(sgp.iter[["sgp.content.areas"]], 1)
		if (!is.null(sgp.year.baseline)) tmp.year <- sgp.year.baseline else tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)
	}

	if (sgp.iter.type=="sgp.percentiles.baseline") {
		content_area.label <- tail(sgp.iter[["sgp.baseline.content.areas"]], 1)
		if (!is.null(sgp.year.baseline)) tmp.year <- sgp.year.baseline else tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)
	}

	if (sgp.iter.type=="sgp.projections") {
		content_area.label <- tail(sgp.iter[["sgp.projection.content.areas"]], 1)
		if (!is.null(sgp.year.baseline)) tmp.year <- sgp.year.baseline else tmp.year <- tail(sgp.iter[["sgp.projection.panel.years"]], 1)
	}

	if (sgp.iter.type=="sgp.projections.baseline") {
		content_area.label <- tail(sgp.iter[["sgp.projection.baseline.content.areas"]], 1)
		if (!is.null(sgp.year.baseline)) tmp.year <- sgp.year.baseline else tmp.year <- tail(sgp.iter[["sgp.projection.baseline.panel.years"]], 1)
	}

	if (sgp.iter.type=="sgp.projections.lagged") {
		content_area.label <- tail(sgp.iter[["sgp.content.areas"]], 1)
		if (!is.null(sgp.year.baseline)) tmp.year <- sgp.year.baseline else tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)
	}

	if (sgp.iter.type=="sgp.projections.lagged.baseline") {
		content_area.label <- tail(sgp.iter[["sgp.baseline.content.areas"]], 1)
		if (!is.null(sgp.year.baseline)) tmp.year <- sgp.year.baseline else tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)
	}

	### Create Knots_Boundaries list

	for (j in tmp.content_areas) {
		for (i in grep(j, names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
			kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
		}
	}

	return(kb[["Knots_Boundaries"]])
} ## END getKnotsBoundaries
