`getPanelDataVnames` <- 
function(sgp.type,
	sgp.iter) {

	if (sgp.type=="sgp.percentiles") {
		return(c("ID", paste("GRADE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
			tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep="."), 
			paste("SCALE_SCORE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])),
			tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep=".")))
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
	suppressMessages(gc())
} ## END getPanelDataVnames

