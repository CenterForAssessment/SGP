`transformScaleScore` <-
function(tmp.data,
	state,
	content_areas,
	linkages) {

	TRANSFORMED_SCALE_SCORE <- SCALE_SCORE <- TEMP_SCALE_SCORE <- SCALE_SCORE_EQUATED <- CONTENT_AREA_LABELS <- YEAR <- GRADE <- NULL

	SGPstateData <- SGPstateData

	if (!is.null(linkages)) {

		scale.transition.scenario <- c(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][['Vertical_Scale_OLD']], 
			SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][['Vertical_Scale_NEW']])


		### Vertical-to-Vertical scale transition

		if (identical(scale.transition.scenario, c("Yes", "Yes"))) {
			tmp.data[, TRANSFORMED_SCALE_SCORE := SCALE_SCORE_EQUATED]
			return(tmp.data)
		}
		

		### Non-Vertical-to-Vertical scale transition

		if (identical(scale.transition.scenario, c("No", "Yes"))) {
			year.for.equate <- tail(sort(sapply(strsplit(names(linkages), "[.]"), ']', 2)), 1)
			years.for.reporting <- sort(unique(tmp.data$YEAR))
			years.for.equate.OLD <- years.for.reporting[seq(match(year.for.equate, years.for.reporting)-1)]
			years.for.equate.NEW <- years.for.reporting[seq(match(year.for.equate, years.for.reporting), length(years.for.reporting))]
			tmp.data[, TEMP_SCALE_SCORE := piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[YEAR %in% years.for.equate.NEW, TEMP_SCALE_SCORE:=NA]
			tmp.data[, TEMP_SCALE_SCORE:=piecewiseTransform(TEMP_SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[YEAR %in% years.for.equate.NEW, TEMP_SCALE_SCORE:=SCALE_SCORE] 
			tmp.linkages <- equateSGP(tmp.data, state, year.for.equate)
			setkeyv(tmp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))
			tmp.data <- convertScaleScore(tmp.data, year.for.equate, tmp.linkages, conversion.type="NEW_TO_OLD", state)
			tmp.data[,TRANSFORMED_SCALE_SCORE := SCALE_SCORE_EQUATED]
			return(tmp.data)
		}


		### Non-Vertical-to-Non-Vertical scale transition

		if (identical(scale.transition.scenario, c("No", "No"))) {


		}


	} else {
		return(tmp.data[, TRANSFORMED_SCALE_SCORE := piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)])
	}

} ### END transformScaleScore function

