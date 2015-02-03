`convertScaleScore` <- 
function(tmp.data.for.equate, 
	tmp.year.for.equate,
	equate.list,
	conversion.type="NEW_TO_OLD",
	state) {

	SCALE_SCORE <- SCALE_SCORE_EQUATED <- NULL

	### Utility functions

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.path.knots.boundaries <- paste(content_area, year, sep=".")
		tmp.knots.boundaries.names <- names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])
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

	scale.score.concordance.list <- list()
	if (conversion.type=="NEW_TO_OLD") tmp.year.for.knots.boundaries <- yearIncrement(tmp.year.for.equate, -1) else tmp.year.for.knots.boundaries <- tmp.year.for.equate


	### Create scale.score.concordance lookup

	for (i in names(equate.list)) {
		for (j in names(equate.list[[i]])) {
			scale.score.concordance.list[[paste(i, j, sep=".")]] <- data.table(
							VALID_CASE="VALID_CASE", 
							CONTENT_AREA=unlist(strsplit(i, "[.]"))[1], 
							YEAR=tmp.year.for.equate, 
							GRADE=unlist(strsplit(j, "_"))[2], 
							SCALE_SCORE=equate.list[[i]][[j]][[conversion.type]][['concordance']][[1]],
							SCALE_SCORE_EQUATED=equate.list[[i]][[j]][[conversion.type]][['concordance']][[2]], 
							key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"))
			tmp.boundaries <- eval(parse(text=paste("SGPstateData[[state]][['Achievement']][['Knots_Boundaries']]", get.my.knots.boundaries.path(unlist(strsplit(i, "[.]"))[1], tmp.year.for.knots.boundaries), "[['loss.hoss_", unlist(strsplit(j, "_"))[2], "']]", sep="")))
			scale.score.concordance.list[[paste(i, j, sep=".")]][SCALE_SCORE_EQUATED < tmp.boundaries[1], SCALE_SCORE_EQUATED:=tmp.boundaries[1]]
			scale.score.concordance.list[[paste(i, j, sep=".")]][SCALE_SCORE_EQUATED > tmp.boundaries[2], SCALE_SCORE_EQUATED:=tmp.boundaries[2]]
		}
	}

	return(data.table(rbindlist(scale.score.concordance.list), key=key(tmp.data.for.equate))[tmp.data.for.equate][is.na(SCALE_SCORE_EQUATED), SCALE_SCORE_EQUATED:=SCALE_SCORE])
} ### END convertScaleScore function
