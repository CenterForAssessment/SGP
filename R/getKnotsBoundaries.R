`getKnotsBoundaries` <- 
function(sgp.iter,
	state,
	sgp.iter.type=c("Standard", "sgp.percentiles")) {

	if (sgp.iter.type[1]=="Standard") {
		if (sgp.iter.type[2]=="sgp.percentiles") my.content.areas <- "sgp.content.areas"
		if (sgp.iter.type[2] %in% c("sgp.projections", "sgp.projections.lagged")) {
			if (!is.null(sgp.iter[['sgp.projection.sequence']])) my.content.areas <- "sgp.projection.sequence" else my.content.areas <- "sgp.projection.content.areas"
		}
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[[my.content.areas]])
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]]) & sgp.iter.type[2] %in% c("sgp.projections", "sgp.projections.lagged")) {
			tmp.content_areas <- unique(unlist(sapply(tmp.content_areas, function(x) unique(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[x]]))))
		}
		if (sgp.iter.type[2]=="sgp.projections") {
			content_area.label <- tail(sgp.iter[["sgp.projection.content.areas"]], 1)
		} else {
			content_area.label <- tail(sgp.iter[["sgp.content.areas"]], 1)
		}
		if (sgp.iter.type[2]=="sgp.projections") {
			tmp.year <- tail(sgp.iter[["sgp.projection.panel.years"]], 1)
		} else {
			tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)
		}

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}

	if (sgp.iter.type[1]=="Baseline") {
		if (sgp.iter.type[2]=="sgp.percentiles") my.content.areas <- "sgp.baseline.content.areas"
		if (sgp.iter.type[2] %in% c("sgp.projections", "sgp.projections.lagged")) {
			if (!is.null(sgp.iter[['sgp.projection.sequence']])) my.content.areas <- "sgp.projection.sequence" else my.content.areas <- "sgp.projection.content.areas"
		}
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[[my.content.areas]])
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]]) & sgp.iter.type[2] %in% c("sgp.projections", "sgp.projections.lagged")) {
			tmp.content_areas <- unique(unlist(sapply(tmp.content_areas, function(x) unique(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[x]]))))
		}
		if (sgp.iter.type[2]=="sgp.projections") {
			content_area.label <- tail(sgp.iter[["sgp.projection.content.areas"]], 1)
		} else {
			content_area.label <- tail(sgp.iter[["sgp.baseline.content.areas"]], 1)
		}
		tmp.year <- "BASELINE"

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}
		
	return(kb[["Knots_Boundaries"]])
} ## END getKnotsBoundaries
