`getKnotsBoundaries` <- 
function(sgp.iter,
	state,
	sgp.iter.type="Standard") {

	if (sgp.iter.type=="Standard") {
		kb <- list()
		tmp.content_areas <- unique(sgp.iter[["sgp.content.areas"]])
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
		tmp.content_areas <- unique(sgp.iter[["baseline.content.areas"]])
		content_area.label <- tail(sgp.iter[["baseline.content.areas"]], 1)
		tmp.year <- "BASELINE"

		for (j in tmp.content_areas) {
			for (i in grep(j, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
				kb[["Knots_Boundaries"]][[paste(content_area.label, tmp.year, sep=".")]][[i]] <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
			}
		}
	}
		
	return(kb[["Knots_Boundaries"]])

} ## END getKnotsBoundaries
