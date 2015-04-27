`convertScaleScore` <- 
function(tmp.data.for.equate, 
	tmp.year.for.equate,
	equate.list,
	conversion.type="NEW_TO_OLD",
	state,
	interpolate.scores=FALSE) {

	SCALE_SCORE <- SCALE_SCORE_EQUATED <- GRADE <- CONTENT_AREA <- NULL

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


	### Define variables

	scale.score.concordance.table <- scale.score.concordance.functions <- list()
	if (conversion.type=="NEW_TO_OLD") tmp.year.for.knots.boundaries <- yearIncrement(tmp.year.for.equate, -1) else tmp.year.for.knots.boundaries <- tmp.year.for.equate


	### Create scale.score.concordance lookup

	for (i.iter in names(equate.list)) {
		i <- unlist(strsplit(i.iter, "[.]"))[1]
		for (j.iter in names(equate.list[[i.iter]])) {
			j <- unlist(strsplit(j.iter, "_"))[2]
			scale.score.concordance.table[[paste(i, j, sep=".")]] <- data.table(
							VALID_CASE="VALID_CASE", 
							CONTENT_AREA=i, 
							YEAR=tmp.year.for.equate, 
							GRADE=j, 
							SCALE_SCORE=equate.list[[i.iter]][[j.iter]][[conversion.type]][['concordance']][[1]],
							SCALE_SCORE_EQUATED=equate.list[[i.iter]][[j.iter]][[conversion.type]][['concordance']][[2]], 
							key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))
			tmp.boundaries <- eval(parse(text=paste("SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']]", get.my.knots.boundaries.path(unlist(strsplit(i, "[.]"))[1], tmp.year.for.knots.boundaries), "[['loss.hoss_", unlist(strsplit(j, "_"))[2], "']]", sep="")))
			scale.score.concordance.table[[paste(i, j, sep=".")]][SCALE_SCORE_EQUATED < tmp.boundaries[1], SCALE_SCORE_EQUATED:=tmp.boundaries[1]]
			scale.score.concordance.table[[paste(i, j, sep=".")]][SCALE_SCORE_EQUATED > tmp.boundaries[2], SCALE_SCORE_EQUATED:=tmp.boundaries[2]]
			scale.score.concordance.functions[[paste(i, j, sep=".")]] <- splinefun(scale.score.concordance.table[[paste(i, j, sep=".")]]$SCALE_SCORE, scale.score.concordance.table[[paste(i, j, sep=".")]]$SCALE_SCORE_EQUATED, method="hyman")
		}
	}

	if (!interpolate.scores) {
		if ("SCALE_SCORE_EQUATED" %in% names(tmp.data.for.equate)) tmp.data.for.equate[,SCALE_SCORE_EQUATED:=NULL]
		return(data.table(rbindlist(scale.score.concordance.table), key=key(tmp.data.for.equate))[tmp.data.for.equate][is.na(SCALE_SCORE_EQUATED), SCALE_SCORE_EQUATED:=SCALE_SCORE])
	} else {
		return(scale.score.concordance.functions)
	}
} ### END convertScaleScore function
