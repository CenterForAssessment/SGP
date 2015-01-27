`equateSGP` <- 
function(sgp_object,
	state,
	equating.method="equip") {

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
		return(list(
			NEW_TO_OLD=equate(
			freqtab(current.year.data[['SCALE_SCORE']], scales=seq(min(current.year.data[['SCALE_SCORE']], na.rm=TRUE), max(current.year.data[['SCALE_SCORE']], na.rm=TRUE))),
			freqtab(prior.year.data[['SCALE_SCORE']], scales=seq(min(prior.year.data[['SCALE_SCORE']], na.rm=TRUE), max(prior.year.data[['SCALE_SCORE']], na.rm=TRUE))), type=equating.method),
			OLD_TO_NEW=equate(
			freqtab(prior.year.data[['SCALE_SCORE']], scales=seq(min(prior.year.data[['SCALE_SCORE']], na.rm=TRUE), max(prior.year.data[['SCALE_SCORE']], na.rm=TRUE))),
			freqtab(current.year.data[['SCALE_SCORE']], scales=seq(min(current.year.data[['SCALE_SCORE']], na.rm=TRUE), max(current.year.data[['SCALE_SCORE']], na.rm=TRUE))), type=equating.method)
			))
	}

	### Loop over GRADE and CONTENT_AREA

	for (content_area.iter in unique(current.year.data$CONTENT_AREA)) {
		for (grade.iter in unique(current.year.data$GRADE)) {
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]] <- 
				equateSGP_INTERNAL(prior.year.data[list(content_area.iter, grade.iter)], current.year.data[list(content_area.iter, grade.iter)])
		}
	}

	return(tmp.list)
} ## END equateSGP Function
