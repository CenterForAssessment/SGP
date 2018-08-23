`piecewiseTransform` <-
function(scale_score,
	state,
	content_area,
	year,
	grade,
	output.digits=1,
	sgp.projections.equated=NULL,
	new.cutscores=NULL,
	equating.method="equipercentile",
	vertical_scale_for_projections=NULL) {

	if (all(is.na(scale_score))) return(scale_score)
	if (is.null(vertical_scale_for_projections)) vertical_scale_for_projections <- TRUE

	### Test to deal with assessment transition scenario

	if (!is.null(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]])) {
		equate.year <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
		if (is.null(equate.year)) equate.year <- 0
		if (year < equate.year)  {
			tmp.test <- "Transformed_Achievement_Level_Cutscores"
		} else {
			if (!is.null(new.cutscores) && length(new.cutscores) > 0) {
				tmp.test <- "NOT_NULL"
			} else {
				tmp.test <- NULL
			}
		}
	} else {
		tmp.test <- NULL
	}


	if (is.null(sgp.projections.equated) | !is.null(tmp.test)) {
		if (((year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]] | !vertical_scale_for_projections) &&
			grade %in% unlist(lapply(strsplit(names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[getMyLabel(state, content_area, year, "Knots_Boundaries")]]), "_"), '[', 2))) || !is.null(tmp.test)) {

			my.knots_boundaries.label <- getMyLabel(state, content_area, year, "Knots_Boundaries")
			tmp.loss.hoss <- SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[my.knots_boundaries.label]][[paste0("loss.hoss_", grade)]]
			scale_score[scale_score < tmp.loss.hoss[1]] <- tmp.loss.hoss[1]; scale_score[scale_score > tmp.loss.hoss[2]] <- tmp.loss.hoss[2]
			my.cutscores.label <- getMyLabel(state, content_area, year)
			old.cutscores <- c(tmp.loss.hoss[1], SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.cutscores.label]][[paste0("GRADE_", grade)]],
				tmp.loss.hoss[2])
			if (is.null(new.cutscores)) new.cutscores <- seq(0, by=100, length.out=length(old.cutscores))
			tmp.index <- findInterval(scale_score, old.cutscores, rightmost.closed=TRUE)
			tmp.diff <- diff(new.cutscores)/diff(old.cutscores)
			round(new.cutscores[tmp.index] + (scale_score - old.cutscores[tmp.index]) * (diff(new.cutscores)/diff(old.cutscores))[tmp.index], digits=output.digits)
		} else {
			as.numeric(scale_score)
		}
	} else {
		if (!is.na(content_area) & !is.na(grade)) {
			sgp.projections.equated[['Linkages']][[paste(content_area, sgp.projections.equated[['Year']], sep=".")]][[paste("GRADE", grade, sep="_")]][[toupper(equating.method)]][['OLD_TO_NEW']][['interpolated_function']](scale_score)
		} else {
			as.numeric(scale_score)
		}
	}
} ## END piecewiseTransform Function
