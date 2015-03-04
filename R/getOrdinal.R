`getOrdinal` <- 
function(cardinal_number) {

	strtail <- function(s, n=1) {
		if(n < 0) substring(s, 1-n)
		else substring(s, nchar(s)-n+1)
	}

	tmp <- strtail(as.character(cardinal_number), 2)
	if (tmp %in% c('1', paste(c(0, 2:9), 1, sep=""))) tmp.suffix <- "st"
	if (tmp %in% c('2', paste(c(0, 2:9), 2, sep=""))) tmp.suffix <- "nd"
	if (tmp %in% c('3', paste(c(0, 2:9), 3, sep=""))) tmp.suffix <- "rd"
	if (tmp %in% c('11', '12', '13')) tmp.suffix <- "th"
	if (tmp %in% c('4', paste(0:9, 4, sep=""))) tmp.suffix <- "th"
	if (tmp %in% c('5', paste(0:9, 5, sep=""))) tmp.suffix <- "th"
	if (tmp %in% c('6', paste(0:9, 6, sep=""))) tmp.suffix <- "th"
	if (tmp %in% c('7', paste(0:9, 7, sep=""))) tmp.suffix <- "th"
	if (tmp %in% c('8', paste(0:9, 8, sep=""))) tmp.suffix <- "th"
	if (tmp %in% c('9', paste(0:9, 9, sep=""))) tmp.suffix <- "th"
	if (tmp %in% c('0', paste(0:9, 0, sep=""))) tmp.suffix <- "th"
	
	return(paste(cardinal_number, tmp.suffix, sep=""))
} ### END getOrdinal
