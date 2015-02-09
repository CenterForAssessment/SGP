`transformScaleScore` <-
function(tmp.data,
	linkages) {

	TRANSFORMED_SCALE_SCORE <- SCALE_SCORE <- state <- CONTENT_AREA_LABELS <- YEAR <- GRADE <- NULL

	if (!is.null(linkages)) {

	} else {
		return(tmp.data[, TRANSFORMED_SCALE_SCORE := piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)])
	}

} ### END transformScaleScore function

