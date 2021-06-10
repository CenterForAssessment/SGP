`equateSGP` <-
function(tmp.data,
	state,
	current.year=NULL,
	equating.method) {

	VALID_CASE <- YEAR <- CONTENT_AREA <- GRADE <- V1 <- V2 <- NULL

	tmp.list <- equate.list <- list()
	equate.interval.digits <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Equate_Interval_Digits"]]
	if (is.null(equate.interval.digits)) equate.interval.digits <- 0

	if (is.null(current.year)) current.year <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
	prior.year <- tail(head(sort(unique(tmp.data[['YEAR']])), -1L), 1L)
	current.year.data <- tmp.data[VALID_CASE=="VALID_CASE" & YEAR==current.year]
	prior.year.data <- tmp.data[VALID_CASE=="VALID_CASE" & YEAR==prior.year]
	setkey(current.year.data, CONTENT_AREA, GRADE)
	setkey(prior.year.data, CONTENT_AREA, GRADE)
	current.year.uniques <- unique(current.year.data[VALID_CASE=="VALID_CASE"], by=key(current.year.data))[,c("CONTENT_AREA", "GRADE"), with=FALSE]
	prior.year.uniques <- unique(prior.year.data[VALID_CASE=="VALID_CASE"], by=key(prior.year.data))[,c("CONTENT_AREA", "GRADE"), with=FALSE]
	content_areas.for.equate <- intersect(unique(current.year.uniques[['CONTENT_AREA']]), unique(prior.year.uniques[['CONTENT_AREA']]))
	unique.content.by.grade <- lapply(content_areas.for.equate, function(x) intersect(current.year.uniques[x]$GRADE, prior.year.uniques[x]$GRADE))
	names(unique.content.by.grade) <- content_areas.for.equate


	### Utility functions

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.path.knots.boundaries <- paste(content_area, year, sep=".")
		tmp.knots.boundaries.names <- names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])
		tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), function(x) x[2])
		if (any(!is.na(tmp.knots.boundaries.years))) {
			if (year %in% tmp.knots.boundaries.years) {
				return(paste0("[['", content_area, ".", year, "']]"))
			} else {
				if (year==sort(c(year, tmp.knots.boundaries.years))[1L]) {
					return(paste0("[['", content_area, "']]"))
				} else {
					return(paste0("[['", content_area, ".", rev(sort(tmp.knots.boundaries.years))[1L], "']]"))
				}
			}
		} else {
			return(paste0("[['", tmp.path.knots.boundaries, "']][['", content_area, "']]"))
		}
	}

	equateSGP_INTERNAL <- function(prior.year.data, current.year.data) {
		current.year.data.range <- round(range(current.year.data[['SCALE_SCORE']], na.rm=TRUE), digits=equate.interval.digits)
		prior.year.data.range <- round(range(prior.year.data[['SCALE_SCORE']], na.rm=TRUE), digits=equate.interval.digits)
		for (equate.type.iter in equating.method) {
			equate.list[[toupper(equate.type.iter)]] <- list(
				NEW_TO_OLD=equate(
					freqtab(as.numeric(as.character(round(current.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
						scales=as.numeric(as.character(round(seq(current.year.data.range[1L], current.year.data.range[2L], by=0.1^equate.interval.digits), digits=equate.interval.digits)))),
					freqtab(as.numeric(as.character(round(prior.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
						scales=as.numeric(as.character(round(seq(prior.year.data.range[1L], prior.year.data.range[2L], by=0.1^equate.interval.digits), digits=equate.interval.digits)))), type=equate.type.iter, name=paste(equate.type.iter, "NEW_TO_OLD", sep="_")),
				OLD_TO_NEW=equate(
					freqtab(as.numeric(as.character(round(prior.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
						scales=as.numeric(as.character(round(seq(prior.year.data.range[1L], prior.year.data.range[2L], by=0.1^equate.interval.digits), digits=equate.interval.digits)))),
					freqtab(as.numeric(as.character(round(current.year.data[['SCALE_SCORE']], digits=equate.interval.digits))),
						scales=as.numeric(as.character(round(seq(current.year.data.range[1L], current.year.data.range[2L], by=0.1^equate.interval.digits), digits=equate.interval.digits)))), type=equate.type.iter, name=paste(equate.type.iter, "OLD_TO_NEW", sep="_"))
			)
		}
		return(equate.list)
	}

	### Loop over GRADE and CONTENT_AREA

	for (content_area.iter in names(unique.content.by.grade)) {
		for (grade.iter in unique.content.by.grade[[content_area.iter]]) {
			tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]] <-
				equateSGP_INTERNAL(prior.year.data[list(content_area.iter, grade.iter)], current.year.data[list(content_area.iter, grade.iter)])
			for (equate.type.iter in equating.method) {
				approxfun.scale <- tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][[toupper(equate.type.iter)]][['NEW_TO_OLD']][['concordance']][['scale']]
				approxfun.yx <- tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][[toupper(equate.type.iter)]][['NEW_TO_OLD']][['concordance']][['yx']]
				equate.dt <- data.table(V1=approxfun.scale, V2=approxfun.yx)
				equate.dt.newtoold <- equate.dt[,list(V2=mean(V2, na.rm=TRUE)), by=V1]
				tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][[toupper(equate.type.iter)]][['NEW_TO_OLD']]$interpolated_function <- approxfun(
					equate.dt.newtoold[['V1']], equate.dt.newtoold[['V2']], rule=2)
				equate.dt.oldtonew <- equate.dt[,list(V1=mean(V1, na.rm=TRUE)), by=V2]
				tmp.list[[paste(content_area.iter, current.year, sep=".")]][[paste("GRADE", grade.iter, sep="_")]][[toupper(equate.type.iter)]][['OLD_TO_NEW']]$interpolated_function <- approxfun(
					equate.dt.oldtonew[['V2']], equate.dt.oldtonew[['V1']], rule=2)
			}
		}
	}
	return(tmp.list)
} ## END equateSGP Function
