`getHighNeedStatus` <-
function(sgp_object) {

	ID <- CONTENT_AREA <- YEAR_INT <- PRIOR_GRADE <- GRADE <- SCALE_SCORE <- PRIOR_SCALE_SCORE  <- HIGH_NEED_STATUS <- VALID_CASE <- SCHOOL_NUMBER <- YEAR <- NULL

	# my.quantile.function producing HIGH_NEED_STATUS variable

	my.quantile.function <- function(x, invalid_cases, quantiles=c(0.25, 0.75)) {
		high.needs.status.labels <- c(paste0("High Needs Status: Prior Achievement Below ", 100*quantiles[1], "th Percentile"),
			NA, paste0("High Needs Status: Prior Achievement Above ", 100*quantiles[2], "th Percentiles"))
		if (invalid_cases) {
			return(rep(NA_character_, length(x)))
		}
		if (all(is.na(x))) {
			return(rep(NA_character_, length(x)))
		} else {
			if (any(diff(quantile(x, probs=c(0, quantiles, 1), na.rm=TRUE))==0)) {
				return(rep(NA_character_, length(x)))
			} else {
				return(high.needs.status.labels[cut(x, quantile(x, probs=c(0, quantiles, 1), na.rm=TRUE), include.lowest=TRUE, labels=FALSE)])
			}
		}
	} ### END my.quantile.function


	slot.data <- copy(sgp_object@Data)
	slot.data[,YEAR_INT:=as.integer(factor(YEAR))]
	setkeyv(slot.data, c("ID", "CONTENT_AREA", "YEAR_INT", "VALID_CASE"))
	slot.data[,c("PRIOR_SCALE_SCORE", "PRIOR_GRADE"):=slot.data[SJ(ID, CONTENT_AREA, YEAR_INT-1L), mult="last"][,list(SCALE_SCORE, GRADE)]]
	setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR_INT", "SCHOOL_NUMBER", "PRIOR_GRADE", "ID"))
	slot.data[,HIGH_NEED_STATUS:=slot.data[,my.quantile.function(PRIOR_SCALE_SCORE, !VALID_CASE[1]=="VALID_CASE"),
		keyby=list(VALID_CASE, CONTENT_AREA, YEAR_INT, SCHOOL_NUMBER, PRIOR_GRADE)][['V1']]]
	slot.data[,HIGH_NEED_STATUS:=as.factor(HIGH_NEED_STATUS)]
	slot.data[,c("PRIOR_SCALE_SCORE", "PRIOR_GRADE", "YEAR_INT"):=NULL]
	setkey(slot.data, VALID_CASE, CONTENT_AREA, YEAR, ID)
	sgp_object@Data <- slot.data
	if (!is.null(sgp_object@Names) && !"HIGH_NEED_STATUS" %in% sgp_object@Names[['names.sgp']]) {
		sgp_object@Names <- rbind(sgp_object@Names, c("HIGH_NEED_STATUS", "HIGH_NEED_STATUS", "demographic", "High need status flag", TRUE))
	}
	messageSGP("\tNOTE: Added variable HIGH_NEED_STATUS to @Data.")
	return(sgp_object)
} ### END getHighNeedStatus
