`getPanelDataVnames` <- 
function(sgp.type,
	sgp.iter,
	sgp.data.names) {

	if (sgp.type=="sgp.percentiles") {
		if ("YEAR_WITHIN" %in% sgp.data.names) {
			return(c("ID", paste("GRADE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.panel.years.within"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep="."), 
				paste("SCALE_SCORE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])),
				tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.panel.years.within"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep=".")))
		} else {
			return(c("ID", paste("GRADE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep="."), 
				paste("SCALE_SCORE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])),
				tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep=".")))
		}
	}

	if (sgp.type=="sgp.percentiles.to.return") {
		special.names.to.return <- ("YEAR_WITHIN")
		if (any(special.names.to.return %in% sgp.data.names)) {
			tmp.list <- list()
			for (i in intersect(special.names.to.return, sgp.data.names)) {
				tmp.list <- c(tmp.list, list(TMP=i))
				names(tmp.list)[[length(tmp.list)]] <- 
					paste(i, tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), tail(sgp.iter[['sgp.panel.years.within']], 1), sep=".")
			}
			return(tmp.list)
		} else {
			return(NULL)
		}
	}

	if (sgp.type=="sgp.projections") {
		return(c("ID", paste("GRADE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])), 
			tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep="."), 
			paste("SCALE_SCORE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
			tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])), sep=".")))
	}

	if (sgp.type=="sgp.projections.lagged") {
		return(c("ID", paste("GRADE", head(tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), 
			head(tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), sep="."), 
			paste("SCALE_SCORE", head(tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), 
			head(tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), sep=".")))
	}
} ## END getPanelDataVnames
