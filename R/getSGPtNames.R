`getSGPtNames` <- 
function(panel.data.vnames,
	SGPt=NULL) {

	if (is.null(SGPt)) {
		return(NULL)
	} else {
		tmp.list <- as.list(tail(panel.data.vnames, 2))
		names(tmp.list) <- c("TIME", "TIME_LAG")
		return(tmp.list)
	}
} ## END getSGPtNames
