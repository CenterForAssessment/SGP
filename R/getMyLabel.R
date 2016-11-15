`getMyLabel` <-
function(state,
	content_area,
	year,
	label="Cutscores") {

	tmp.cutscore.years <- unlist(lapply(sapply(strsplit(grep(content_area, names(SGP::SGPstateData[[state]][["Achievement"]][[label]]), value=TRUE), "[.]"), tail, -1), paste, collapse="."))
	tmp.cutscore.years[tmp.cutscore.years==""] <- NA
	if (any(!is.na(tmp.cutscore.years))) {
		if (year %in% tmp.cutscore.years) {
			return(paste(content_area, year, sep="."))
		} else {
			if (year==sort(c(year, tmp.cutscore.years))[1]) {
				return(content_area)
			} else {
				return(paste(content_area, sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1], sep="."))
			}
		}
	} else {
		return(content_area)
	}
} ## END getMyLabel Function
