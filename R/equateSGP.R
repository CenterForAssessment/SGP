`equateSGP` <- 
function(tmp.data,
	state,
	current.year,
	equating.method="equip") {

	VALID_CASE <- YEAR <- CONTENT_AREA <- GRADE <- NULL

	tmp.list <- list()
	current.year <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
	prior.year <- tail(head(sort(unique(tmp.data$YEAR)), -1), 1)

	current.year.data <- tmp.data[VALID_CASE=="VALID_CASE" & YEAR==current.year]
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
