`equateSGP` <- 
function(tmp.data,
	state,
	current.year,
	equating.method="equip",
	loss.hoss.correction=TRUE) {

	VALID_CASE <- YEAR <- CONTENT_AREA <- GRADE <- NULL

	tmp.list <- list()
	current.year <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
	prior.year <- yearIncrement(current.year, -1)

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

			if (loss.hoss.correction) {
				tmp.boundaries.new.to.old <- 
					eval(parse(text=paste("SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']]", get.my.knots.boundaries.path(content_area.iter, prior.year), "[['loss.hoss_", grade.iter, "']]", sep="")))
				tmp.boundaries.old.to.new <- 
					eval(parse(text=paste("SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']]", get.my.knots.boundaries.path(content_area.iter, current.year), "[['loss.hoss_", grade.iter, "']]", sep="")))
				tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']]$yx[
					tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']]$yx < tmp.boundaries.new.to.old[1]] <-
					tmp.boundaries.new.to.old[1]	
				tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']]$yx[
					tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']]$yx > tmp.boundaries.new.to.old[2]] <-
					tmp.boundaries.new.to.old[2]	
				tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['OLD_TO_NEW']][['concordance']]$yx[
					tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['OLD_TO_NEW']][['concordance']]$yx < tmp.boundaries.old.to.new[1]] <-
					tmp.boundaries.old.to.new[1]	
				tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['OLD_TO_NEW']][['concordance']]$yx[
					tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['OLD_TO_NEW']][['concordance']]$yx > tmp.boundaries.old.to.new[2]] <-
					tmp.boundaries.old.to.new[2]
			}

			splinefun.scale <- tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']][['scale']]
			splinefun.scale <- c(extendrange(splinefun.scale, f=0.2)[1], splinefun.scale, extendrange(splinefun.scale, f=0.2)[2])
			splinefun.yx <- tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']][['concordance']][['yx']]
			splinefun.yx <- c(extendrange(splinefun.yx, f=0.2)[1], splinefun.yx, extendrange(splinefun.yx, f=0.2)[2])
 
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['NEW_TO_OLD']]$interpolated_function <- splinefun(
				splinefun.scale, splinefun.yx, method="hyman")
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][['OLD_TO_NEW']]$interpolated_function <- splinefun(
				splinefun.yx, splinefun.scale, method="hyman")
		}
	}
	return(tmp.list)
} ## END equateSGP Function
