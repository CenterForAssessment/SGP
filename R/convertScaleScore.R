`convertScaleScore` <- 
function(tmp.data.for.equate, 
	tmp.year.for.equate,
	equate.list,
	conversion.type="NEW_TO_OLD") {

	.EACHI <- SCALE_SCORE_EQUATED <- NULL

	scale.score.concordance.list <- list()

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
		}
	}

	return(data.table(rbindlist(scale.score.concordance.list), key=key(tmp.data.for.equate))[tmp.data.for.equate])
} ### END convertScaleScore function
