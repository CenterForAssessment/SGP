`yearIncrement` <-
function(year,
	increment,
	lag=0) {

	if (is.null(year)) return(NULL)

	if (identical(year, "BASELINE")) {
		return(rep("BASELINE", length(increment)))
	} else {
		if (grepl("_", year[1L])) {
			tmp1 <- sapply(seq_along(increment), function(i) as.numeric(sapply(strsplit(as.character(year), "_"), '[', 2L)) + increment[i] - lag)
			paste(floor(tmp1)-1L, tmp1, sep="_")
		} else {
			as.character(sapply(seq_along(increment), function(i) as.numeric(year) + increment[i] - lag))
		}
	}
} ### End yearIncrement
