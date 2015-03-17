`transformScaleScore` <-
function(tmp.data,
	state,
	content_areas,
	linkages) {

	TRANSFORMED_SCALE_SCORE <- SCALE_SCORE <- TEMP_SCALE_SCORE <- SCALE_SCORE_EQUATED <- CONTENT_AREA <- CONTENT_AREA_LABELS <- YEAR <- GRADE <- GRADE_NUMERIC <- NULL
	CUTSCORES <- CUTSCORES_ORIGINAL <- NULL
	SGPstateData <- SGPstateData

	### Create LONG cutscores

	Cutscores <- list()
	for (i in content_areas) {
		Cutscores[[i]] <- createLongCutscores(state, i)
		Cutscores[[i]][,CUTSCORES_ORIGINAL:=CUTSCORES]
	}

	### Utility functions

	get.min.max.grade <- function(Cutscores) {

		if ("GRADE_NUMERIC" %in% names(Cutscores)) {
			tmp.grades.numeric <- range(sort(type.convert(subset(Cutscores, !GRADE %in% c("GRADE_LOWER", "GRADE_UPPER"))[['GRADE_NUMERIC']])))
			tmp.grades <- sort(subset(Cutscores, GRADE_NUMERIC %in% tmp.grades.numeric)[['GRADE']])
		} else {
			tmp.grades <- sort(type.convert(subset(Cutscores, !GRADE %in% c("GRADE_LOWER", "GRADE_UPPER"))[['GRADE']]))
		}
		return(c(tmp.grades[1], rev(tmp.grades)[1]))
	}


	### Return Data and Cutscores based upon whether scale score transition

	if (!is.null(linkages)) {
		year.for.equate <- tail(sort(sapply(strsplit(names(linkages), "[.]"), '[', 2)), 1)
		scale.transition.scenario <- c(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][['Vertical_Scale']], 
			SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste('Vertical_Scale', year.for.equate, sep=".")]])


		### Vertical-to-Vertical scale transition

		if (identical(scale.transition.scenario, c("Yes", "Yes"))) {

			### Scale Score Transformation

			year.for.equate <- tail(sort(sapply(strsplit(names(linkages), "[.]"), '[', 2)), 1)
			tmp.data[, TRANSFORMED_SCALE_SCORE:=SCALE_SCORE_EQUATED]

			### Cutscore Transformation

			for (content_area.iter in content_areas) {  
				for (grade.iter in unique(Cutscores[[content_area.iter]][['GRADE']])) {
					Cutscores[[content_area.iter]][CONTENT_AREA==content_area.iter & GRADE==grade.iter & YEAR==year.for.equate,
						CUTSCORES:=linkages[[paste(content_area.iter, year.for.equate, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][["interpolated_function"]](CUTSCORES)]
					tmp.min.max <- get.min.max.grade(Cutscores[[content_area.iter]])
					if (grade.iter=="GRADE_UPPER") {
						Cutscores[[content_area.iter]][CONTENT_AREA=="PLACEHOLDER" & GRADE=="GRADE_UPPER" & YEAR==year.for.equate,
							CUTSCORES:=linkages[[paste(content_area.iter, year.for.equate, sep=".")]][[paste("GRADE", tmp.min.max[2], sep="_")]][['NEW_TO_OLD']][["interpolated_function"]](CUTSCORES)]
					}
					if (grade.iter=="GRADE_LOWER") {
						Cutscores[[content_area.iter]][CONTENT_AREA=="PLACEHOLDER" & GRADE=="GRADE_LOWER" & YEAR==year.for.equate,
							CUTSCORES:=linkages[[paste(content_area.iter, year.for.equate, sep=".")]][[paste("GRADE", tmp.min.max[1], sep="_")]][['NEW_TO_OLD']][["interpolated_function"]](CUTSCORES)]
					}
				}
			}
			
			### Return data

			return(list(Data=tmp.data, Cutscores=Cutscores, sgp.projections.equated=list(Year=year.for.equate, Linkages=linkages)))
		}
		

		### Non-Vertical-to-Vertical scale transition

		if (identical(scale.transition.scenario, c("No", "Yes"))) {

			### Scale Score Transformation

			year.for.equate <- tail(sort(sapply(strsplit(names(linkages), "[.]"), '[', 2)), 1)
			years.for.reporting <- sort(unique(tmp.data$YEAR))
			years.for.equate.OLD <- years.for.reporting[seq(match(year.for.equate, years.for.reporting)-1)]
			years.for.equate.NEW <- years.for.reporting[seq(match(year.for.equate, years.for.reporting), length(years.for.reporting))]
			tmp.data[,TEMP_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[YEAR %in% years.for.equate.NEW, TEMP_SCALE_SCORE:=SCALE_SCORE]
			setnames(tmp.data, c("TEMP_SCALE_SCORE", "SCALE_SCORE"), c("SCALE_SCORE", "SCALE_SCORE_ORIGINAL"))
			tmp.linkages <- equateSGP(tmp.data, state, year.for.equate)
			setkeyv(tmp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))
			tmp.data <- convertScaleScore(tmp.data, year.for.equate, tmp.linkages, conversion.type="NEW_TO_OLD", state)
			tmp.data[,TRANSFORMED_SCALE_SCORE := SCALE_SCORE_EQUATED]
			setnames(tmp.data, c("SCALE_SCORE", "SCALE_SCORE_ORIGINAL"), c("SCALE_SCORE", "TEMP_SCALE_SCORE"))
			tmp.data[,TEMP_SCALE_SCORE:=NULL]

			### Cutscore Transformation

			for (content_area.iter in content_areas) {  
				for (grade.iter in unique(Cutscores[[content_area.iter]][['GRADE']])) {
					Cutscores[[content_area.iter]][CONTENT_AREA==content_area.iter & GRADE==grade.iter & YEAR==year.for.equate,
						CUTSCORES:=tmp.linkages[[paste(content_area.iter, year.for.equate, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][["interpolated_function"]](CUTSCORES)]
					tmp.min.max <- get.min.max.grade(Cutscores[[content_area.iter]])
					if (grade.iter=="GRADE_UPPER") {
						Cutscores[[content_area.iter]][CONTENT_AREA=="PLACEHOLDER" & GRADE=="GRADE_UPPER" & YEAR==year.for.equate,
							CUTSCORES:=tmp.linkages[[paste(content_area.iter, year.for.equate, sep=".")]][[paste("GRADE", tmp.min.max[2], sep="_")]][['NEW_TO_OLD']][["interpolated_function"]](CUTSCORES)]
					}
					if (grade.iter=="GRADE_LOWER") {
						Cutscores[[content_area.iter]][CONTENT_AREA=="PLACEHOLDER" & GRADE=="GRADE_LOWER" & YEAR==year.for.equate,
							CUTSCORES:=tmp.linkages[[paste(content_area.iter, year.for.equate, sep=".")]][[paste("GRADE", tmp.min.max[1], sep="_")]][['NEW_TO_OLD']][["interpolated_function"]](CUTSCORES)]
					}
				}
			}

			### Return data

			return(list(Data=tmp.data, Cutscores=Cutscores, sgp.projections.equated=list(Year=year.for.equate, Linkages=tmp.linkages)))
		}


		### Non-Vertical-to-Non-Vertical scale transition

		if (identical(scale.transition.scenario, c("No", "No"))) {

			### Scale Score Transformation

			year.for.equate <- tail(sort(sapply(strsplit(names(linkages), "[.]"), '[', 2)), 1)
			years.for.reporting <- sort(unique(tmp.data$YEAR))
			years.for.equate.OLD <- years.for.reporting[seq(match(year.for.equate, years.for.reporting)-1)]
			years.for.equate.NEW <- years.for.reporting[seq(match(year.for.equate, years.for.reporting), length(years.for.reporting))]
			tmp.data[, TEMP_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[YEAR %in% years.for.equate.NEW, TEMP_SCALE_SCORE:=NA]
			tmp.data[, TEMP_SCALE_SCORE:=piecewiseTransform(TEMP_SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[YEAR %in% years.for.equate.NEW, TEMP_SCALE_SCORE:=SCALE_SCORE] 
			tmp.linkages <- equateSGP(tmp.data, state, year.for.equate)
			setkeyv(tmp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))
			tmp.data <- convertScaleScore(tmp.data, year.for.equate, tmp.linkages, conversion.type="NEW_TO_OLD", state)
			tmp.data[,TRANSFORMED_SCALE_SCORE := SCALE_SCORE_EQUATED]


			### Cutscore Transformation

			### Return data

			return(list(Data=tmp.data, Cutscores=Cutscores, sgp.projections.equated=list(Year=year.for.equate, Linkages=tmp.linkages)))

		}


	} else {
		tmp.data[, TRANSFORMED_SCALE_SCORE := piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
		return(list(Data=tmp.data, Cutscores=Cutscores, Linkages=NULL))
	}

} ### END transformScaleScore function
