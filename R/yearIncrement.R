`yearIncrement` <-
function(year, 
	increment, 
	lag=0) {

	if (identical(year, "BASELINE")) {
		return(rep("BASELINE", length(increment)))
	} else {
		sapply(seq_along(increment), function(i) paste(as.numeric(unlist(strsplit(as.character(year), "_"))) + increment[i] - lag, collapse="_"))
	}

} ### End yearIncrement
