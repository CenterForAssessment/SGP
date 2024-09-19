`getTargetScaleScoreTableNames` <- 
function(
	table_names, 
	years) {
	if (is.null(years)) {
		return(grep("TARGET_SCALE_SCORES", table_names, value=TRUE))
	} else {
		return(grep(paste(years, collapse="|"), grep("TARGET_SCALE_SCORES", table_names, value=TRUE), value=TRUE))
	}
} ### END getTargetScaleScoreTableNames
