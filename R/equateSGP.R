`equateSGP` <- 
function(sgp_object,
	state) {

	VALID_CASE <- YEAR <- CONTENT_AREA <- GRADE <- NULL

	tmp.list <- list()
	current.year <- SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
	prior.year <- yearIncrement(current.year, -1)

	current.year.data <- sgp_object@Data[VALID_CASE=="VALID_CASE" & YEAR==current.year]
	prior.year.data <- sgp_object@Data[VALID_CASE=="VALID_CASE" & YEAR==prior.year]
	setkey(current.year.data, CONTENT_AREA, GRADE)
	setkey(prior.year.data, CONTENT_AREA, GRADE)


	### Utility function

	equateSGP_INTERNAL <- function(prior.year.data, current.year.data) {
		return(equate(
			freqtab(current.year.data[['SCALE_SCORE']], scales=seq(min(current.year.data[['SCALE_SCORE']], na.rm=TRUE), max(current.year.data[['SCALE_SCORE']], na.rm=TRUE))),
			freqtab(prior.year.data[['SCALE_SCORE']], scales=seq(min(prior.year.data[['SCALE_SCORE']], na.rm=TRUE), max(prior.year.data[['SCALE_SCORE']], na.rm=TRUE))), type="equip"))
	}

	### Loop over GRADE and CONTENT_AREA

	for (content_area.iter in unique(current.year.data$CONTENT_AREA)) {
		for (grade.iter in unique(current.year.data$GRADE)) {
			tmp.list[[paste(content_area.iter, grade.iter, sep=".")]] <- 
				equateSGP_INTERNAL(prior.year.data[list(content_area.iter, grade.iter)], current.year.data[list(content_area.iter, grade.iter)])
		}
	}

	return(tmp.list)
} ## END equateSGP Function
