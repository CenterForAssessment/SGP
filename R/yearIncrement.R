`yearIncrement` <-
function(year, 
	increment, 
	lag=0) {

	if (identical(year, "BASELINE")) {
		return(rep("BASELINE", length(increment)))
	} else {
		tmp <- sapply(seq_along(increment), function(i) paste(as.numeric(unlist(strsplit(as.character(year), "_"))) + increment[i] - lag, collapse="_"))
		if (length(grep("_", tmp[1]) > 0)) tmp <- sapply(strsplit(tmp, "_"), function(i) paste(floor(as.numeric(i[1])), i[2], sep="_"))
		return(tmp)
	}
} ### End yearIncrement
