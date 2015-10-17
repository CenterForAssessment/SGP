`equateSGP` <-
function(tmp.data,
	state,
	current.year,
	equating.method="equip") {

	VALID_CASE <- YEAR <- CONTENT_AREA <- GRADE <- NULL

	tmp.list <- list()
	current.year <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
	equate.interval.digits <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Equate_Interval_Digits"]]
	if (is.null(equate.interval.digits)) equate.interval.digits <- 0
	prior.year <- tail(head(sort(unique(tmp.data$YEAR)), -1), 1)
	grades.for.equate <- as.character(intersect(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][['Grades_Tested']],
                        SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste('Grades_Tested', current.year, sep=".")]]))
	current.year.data <- tmp.data[VALID_CASE=="VALID_CASE" & YEAR==current.year]
	content_areas.for.equate <- intersect(unique(current.year.data$CONTENT_AREA),
		names(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Content_Areas_Labels"]]))
	prior.year.data <- tmp.data[VALID_CASE=="VALID_CASE" & YEAR==prior.year]
	setkey(current.year.data, CONTENT_AREA, GRADE)
	setkey(prior.year.data, CONTENT_AREA, GRADE)


	### Utility functions

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.path.knots.boundaries <- paste(content_area, year, sep=".")
		tmp.knots.boundaries.names <- names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])
		tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), function(x) x[2])
		if (any(!is.na(tmp.knots.boundaries.years))) {
			if (year %in% tmp.knots.boundaries.years) {
				return(paste("[['", content_area, ".", year, "']]", sep=""))
			} else {
				if (year==sort(c(year, tmp.knots.boundaries.years))[1]) {
					return(paste("[['", content_area, "']]", sep=""))
				} else {
					return(paste("[['", content_area, ".", rev(sort(tmp.knots.boundaries.years))[1], "']]", sep=""))
				}
			}
		} else {
			return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, "']]", sep=""))
		}
	}

	equateSGP_INTERNAL <- function(prior.year.data, current.year.data) {
		current.year.data.range <- round(range(current.year.data[['SCALE_SCORE']], na.rm=TRUE), digits=equate.interval.digits)
		prior.year.data.range <- round(range(prior.year.data[['SCALE_SCORE']], na.rm=TRUE), digits=equate.interval.digits)
		return(list(
			NEW_TO_OLD=equate(
				freqtab(as.numeric(as.character(round(current.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
					scales=as.numeric(as.character(round(seq(current.year.data.range[1], current.year.data.range[2], by=0.1^equate.interval.digits), digits=equate.interval.digits)))),
				freqtab(as.numeric(as.character(round(prior.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
					scales=as.numeric(as.character(round(seq(prior.year.data.range[1], prior.year.data.range[2], by=0.1^equate.interval.digits), digits=equate.interval.digits)))), type=equating.method),
			OLD_TO_NEW=equate(
				freqtab(as.numeric(as.character(round(prior.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
					scales=as.numeric(as.character(round(seq(prior.year.data.range[1], prior.year.data.range[2], by=0.1^equate.interval.digits), digits=equate.interval.digits)))),
				freqtab(as.numeric(as.character(round(current.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
					scales=as.numeric(as.character(round(seq(current.year.data.range[1], current.year.data.range[2], by=0.1^equate.interval.digits), digits=equate.interval.digits)))), type=equating.method)
		))
	}

	### Loop over GRADE and CONTENT_AREA

	for (content_area.iter in content_areas.for.equate) {
		for (grade.iter in grades.for.equate) {
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]] <-
				equateSGP_INTERNAL(prior.year.data[list(content_area.iter, grade.iter)], current.year.data[list(content_area.iter, grade.iter)])

			approxfun.scale <- tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']][['scale']]
			approxfun.yx <- tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']][['yx']]
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']]$interpolated_function <- approxfun(
				approxfun.scale, approxfun.yx, rule=2)
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['OLD_TO_NEW']]$interpolated_function <- approxfun(
				approxfun.yx, approxfun.scale, rule=2)
		}
	}
	return(tmp.list)
} ## END equateSGP Function
