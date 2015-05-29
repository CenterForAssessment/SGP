`getSGPtNames` <- 
function(sgp.iter,
	SGPt=NULL) {

	if (is.null(SGPt)) {
		return(NULL)
	} else {
		tmp.list <- as.list(paste(c("TIME", "TIME_LAG"), tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), sep="."))
		names(tmp.list) <- c("TIME", "TIME_LAG")
		return(tmp.list)
	}
} ## END getSGPtNames
