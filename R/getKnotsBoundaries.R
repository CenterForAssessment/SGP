`getKnotsBoundaries` <- 
function(sgp.iter,
	state,
	sgp.iter.type=c("Standard", "sgp.percentiles")) {

	if (sgp.iter.type[1]=="Standard") {
		if (sgp.iter.type[2] %in% c("sgp.percentiles", "sgp.projections.lagged")) my.content.areas <- "sgp.content.areas"
		if (sgp.iter.type[2]=="sgp.projections") my.content.areas <- "sgp.projection.content.areas"
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[[my.content.areas]])
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
			tmp.content_areas <- sapply(tmp.content_areas, function(x) unique(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[x]]))
		}
		content_area.label <- tail(sgp.iter[[my.content.areas]], 1)
		tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}

	if (sgp.iter.type[1]=="Baseline") {
		if (sgp.iter.type[2] %in% c("sgp.percentiles", "sgp.projections.lagged")) my.content.areas <- "sgp.baseline.content.areas"
		if (sgp.iter.type[2]=="sgp.projections") my.content.areas <- "sgp.projection.content.areas"
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[[my.content.areas]])
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
			tmp.content_areas <- sapply(tmp.content_areas, function(x) unique(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[x]]))
		}
		content_area.label <- tail(sgp.iter[[my.content.areas]], 1)
		tmp.year <- "BASELINE"

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}
		
	return(kb[["Knots_Boundaries"]])
} ## END getKnotsBoundaries
