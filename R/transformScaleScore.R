`transformScaleScore` <-
function(tmp.data,
	state,
	linkages) {

	TRANSFORMED_SCALE_SCORE <- SCALE_SCORE <- state <- CONTENT_AREA_LABELS <- YEAR <- GRADE <- NULL

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
			tmp.data[, 
		}


		### Non-Vertical-to-Non-Vertical scale transition

		if (identical(scale.transition.scenario, c("No", "No"))) {


		}


	} else {
		return(tmp.data[, TRANSFORMED_SCALE_SCORE := piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)])
	}

} ### END transformScaleScore function

