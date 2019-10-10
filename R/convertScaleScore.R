`convertScaleScore` <-
function(tmp.data.for.equate,
	tmp.year.for.equate,
	equate.list,
	conversion.type,
	equating.method,
	state) {

	SCALE_SCORE <- TEMP_SCALE_SCORE_EQUATED <- GRADE <- CONTENT_AREA <- YEAR <- NULL

	### Define variables

	tmp.unique.years <- unique(tmp.data.for.equate[['YEAR']])

	if (conversion.type=="NEW_TO_OLD") {
		tmp.years.for.equate <- sort(tmp.unique.years[tmp.unique.years >= tmp.year.for.equate])
	} else {
		tmp.years.for.equate <- sort(tmp.unique.years[tmp.unique.years < tmp.year.for.equate])
	}
	if (paste("SCALE_SCORE_EQUATED", equating.method, conversion.type, sep="_") %in% names(tmp.data.for.equate)) tmp.data.for.equate[,paste("SCALE_SCORE_EQUATED", equating.method, conversion.type, sep="_"):=NULL]

	if (!is.null(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Equated_Content_Areas_and_Grades"]])) {
		equating.content_areas.grades <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Equated_Content_Areas_and_Grades"]]
	} else {
		equating.content_areas.grades <- lapply(equate.list, names)
		names(equating.content_areas.grades) <- sapply(strsplit(names(equating.content_areas.grades), '[.]'), '[', 1)
		for (tmp.iter in seq_along(equating.content_areas.grades)) {
			equating.content_areas.grades[[tmp.iter]] <- gsub("GRADE_", "", equating.content_areas.grades[[tmp.iter]])
		}
	}


	### Create scale.score.concordance lookup

	for (content_area.iter in names(equating.content_areas.grades)) {
		for (grade.iter in equating.content_areas.grades[[content_area.iter]]) {
			tmp.data.for.equate[YEAR %in% tmp.years.for.equate & CONTENT_AREA==content_area.iter & GRADE==grade.iter,
				TEMP_SCALE_SCORE_EQUATED:=equate.list[[paste(content_area.iter, tmp.year.for.equate, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][[toupper(equating.method)]][[conversion.type]][['interpolated_function']](SCALE_SCORE)]
		}
	}

	tmp.data.for.equate[!YEAR %in% tmp.years.for.equate, TEMP_SCALE_SCORE_EQUATED:=SCALE_SCORE]
	tmp.data.for.equate[is.na(TEMP_SCALE_SCORE_EQUATED) & !is.na(SCALE_SCORE), TEMP_SCALE_SCORE_EQUATED:=SCALE_SCORE]
	setnames(tmp.data.for.equate, "TEMP_SCALE_SCORE_EQUATED", paste("SCALE_SCORE_EQUATED", toupper(equating.method), conversion.type, sep="_"))

	return(tmp.data.for.equate)
} ### END convertScaleScore function
