`getKnotsBoundaries` <- 
function(sgp.iter,
	state,
	sgp.iter.type="Standard") {

	if (sgp.iter.type=="Standard") {
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[["sgp.content.areas"]])
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
			tmp.content_areas <- sapply(tmp.content_areas, function(x) unique(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[x]]))
		}
		content_area.label <- tail(sgp.iter[["sgp.content.areas"]], 1)
		tmp.year <- tail(sgp.iter[["sgp.panel.years"]], 1)

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}

	if (sgp.iter.type=="Baseline") {
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[["sgp.baseline.content.areas"]])
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
			tmp.content_areas <- sapply(tmp.content_areas, function(x) unique(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[x]]))
		}
		content_area.label <- tail(sgp.iter[["sgp.baseline.content.areas"]], 1)
		tmp.year <- "BASELINE"

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}
		
	return(kb[["Knots_Boundaries"]])
} ## END getKnotsBoundaries
