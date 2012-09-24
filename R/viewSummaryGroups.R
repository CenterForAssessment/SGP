`viewSummaryGroups` <- 
function(
	summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS"),
		institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
	confidence.interval.groups=list(institution="SCHOOL_NUMBER",
		content="CONTENT_AREA",
		time="YEAR",
		institution_level=NULL,
		demographic=NULL,
		institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
	confidence.interval.groups.only=FALSE) {

	## Functions
	group.format <- function(my.group) {
		if (is.null(my.group)) c("")
		else {
			c("", unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
		}
	}

	## Summary tables to be produced and corresponding confidence intervals
	groups.to.summarize <- data.frame(Summary_Groups = NULL, Confidence_Interval_Calculated = NULL)
	for (i in summary.groups$institution) {
		sgp.groups <- do.call(paste, c(expand.grid(i,
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["demographic"]])), sep=""))

		if (!is.null(confidence.interval.groups) & i %in% confidence.interval.groups$institution) {
			ci.groups <- do.call(paste, c(expand.grid(i,
				group.format(confidence.interval.groups[["content"]]),
				group.format(confidence.interval.groups[["time"]]),
				group.format(confidence.interval.groups[["institution_level"]]),
				group.format(confidence.interval.groups[["institution_inclusion"]][[i]]),
				group.format(confidence.interval.groups[["demographic"]])), sep=""))
      tmp <- data.frame(Summary_Groups = sgp.groups, Confidence_Interval_Calculated = ci.groups %in% sgp.groups)
		} else tmp <- data.frame(Summary_Groups = sgp.groups, Confidence_Interval_Calculated = FALSE)
		groups.to.summarize <- rbind(groups.to.summarize, tmp)
	} ## END summary.groups$institution
	if(confidence.interval.groups.only) groups.to.summarize <- groups.to.summarize[groups.to.summarize$Confidence_Interval_Calculated==TRUE,]
return(groups.to.summarize)
} ## END summarizeSGP Function
