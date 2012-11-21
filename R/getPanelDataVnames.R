`getPanelDataVnames` <- 
function(sgp.type,
	sgp.iter) {

	## year.increment function

	year.increment <- function(year, increment, lag) {
		paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment-lag, collapse="_")
	}

	if (sgp.type=="sgp.percentiles") {
		return(c("ID", paste("GRADE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
			tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep="."), 
			paste("SCALE_SCORE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
			tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep=".")))
	}

	if (sgp.type=="sgp.projections") {
		tmp.years <- sapply(rev(head(sgp.iter[["sgp.projection.grade.sequences"]][[1]], 1)-sgp.iter[["sgp.projection.grade.sequences"]][[1]]),
			year.increment, year=tail(sgp.iter[["sgp.panel.years"]], 1), lag=0)
		return(c("ID", paste("GRADE", tmp.years, sep="."), paste("SCALE_SCORE", tmp.years, sep=".")))
	}

	if (sgp.type=="sgp.projections.lagged") {
		return(c("ID", paste("GRADE", head(tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), sep="."), 
			paste("SCALE_SCORE", head(tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), sep=".")))
	}
} ## END getPanelDataVnames

