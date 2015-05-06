`transformScaleScore` <-
function(tmp.data,
	state,
	content_areas,
	linkages,
	slot.data) {

	TRANSFORMED_SCALE_SCORE <- SCALE_SCORE <- TEMP_SCALE_SCORE <- SCALE_SCORE_EQUATED <- CONTENT_AREA <- CONTENT_AREA_LABELS <- YEAR <- GRADE <- GRADE_NUMERIC <- NULL
	CUTSCORES <- CUTSCORES_ORIGINAL <- NULL

	### Create relevant variables

	Cutscores <- list()


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
		assessment.transition.type <- c(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][['Vertical_Scale']], 
			SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste('Vertical_Scale', year.for.equate, sep=".")]])

		for (i in content_areas) {
			Cutscores[[i]] <- createLongCutscores(state=state, content_area=i, assessment.transition.type=assessment.transition.type)
			Cutscores[[i]][, CUTSCORES_ORIGINAL:=CUTSCORES]
		}


		#############################################################
		### Vertical-to-Vertical scale transition
		#############################################################

		if (identical(assessment.transition.type, c("Yes", "Yes"))) {

			### Scale Score Transformation

			tmp.data[, TRANSFORMED_SCALE_SCORE:=SCALE_SCORE_EQUATED]

			### Transform Cutscores

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
		

		#######################################################
		### Non-Vertical-to-Vertical scale transition
		#######################################################

		if (identical(assessment.transition.type, c("No", "Yes"))) {

			### Scale Score Transformation of OLD test data

			tmp.data[,SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			slot.data[,SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.linkages <- equateSGP(slot.data, state, year.for.equate, "linear", loss.hoss.correction=FALSE)
			setkeyv(tmp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))
			tmp.data <- convertScaleScore(tmp.data, year.for.equate, tmp.linkages, conversion.type="NEW_TO_OLD", state)
			tmp.data[,TRANSFORMED_SCALE_SCORE:=SCALE_SCORE_EQUATED]

			### Transform Cutscores

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


		###############################################################
		### Non-Vertical-to-Non-Vertical scale transition
		###############################################################

		if (identical(assessment.transition.type, c("No", "No"))) {

			### Scale Score Transformation of OLD & NEW test data

			slot.data[,SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, YEAR, GRADE), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.linkages <- equateSGP(slot.data, state, year.for.equate, "linear", loss.hoss.correction=FALSE)

			### Transform Cutscores

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

			tmp.data[,TEMP_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[!is.na(CONTENT_AREA_LABELS) & YEAR >= year.for.equate, 
				TEMP_SCALE_SCORE:=piecewiseTransform(
					SCALE_SCORE, 
					state, 
					CONTENT_AREA_LABELS, 
					YEAR, 
					GRADE,
					new.cutscores=getNewCutscores(CONTENT_AREA[1], CONTENT_AREA_LABELS[1], year.for.equate, GRADE[1], Cutscores)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
			tmp.data[,TRANSFORMED_SCALE_SCORE:=TEMP_SCALE_SCORE]
			tmp.data[,TEMP_SCALE_SCORE:=NULL]

			### Return data

			return(list(Data=tmp.data, Cutscores=Cutscores, sgp.projections.equated=list(Year=year.for.equate, Linkages=tmp.linkages, Assessment_Transition_Type=assessment.transition.type)))
		}
	} else {
		for (i in content_areas) {
			Cutscores[[i]] <- createLongCutscores(state, i)
			Cutscores[[i]][, CUTSCORES_ORIGINAL:=CUTSCORES]
		}
		tmp.data[, TRANSFORMED_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]
		return(list(Data=tmp.data, Cutscores=Cutscores, sgp.projections.equated=NULL))
	}
} ### END transformScaleScore function
