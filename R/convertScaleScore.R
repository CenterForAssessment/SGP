`convertScaleScore` <- 
function(tmp.data.for.equate, 
	tmp.year.for.equate,
	equate.list,
	conversion.type="NEW_TO_OLD",
	state) {

	SCALE_SCORE <- SCALE_SCORE_EQUATED <- GRADE <- CONTENT_AREA <- YEAR <- NULL

	### Define variables

	scale.score.concordance.table <- scale.score.concordance.functions <- list()
	tmp.unique.years <- unique(tmp.data.for.equate$YEAR)

	if (conversion.type=="NEW_TO_OLD") {
		tmp.years.for.equate <- tmp.unique.years[tmp.unique.years >= tmp.year.for.equate]
	} else {
		tmp.years.for.equate <- tmp.unique.years[tmp.unique.years < tmp.year.for.equate]
	}
	if ("SCALE_SCORE_EQUATED" %in% names(tmp.data.for.equate)) tmp.data.for.equate[,SCALE_SCORE_EQUATED:=NULL]


	### Create scale.score.concordance lookup

	for (i.iter in names(equate.list)) {
		i <- unlist(strsplit(i.iter, "[.]"))[1]
		for (j.iter in names(equate.list[[i.iter]])) {
			j <- unlist(strsplit(j.iter, "_"))[2]
			tmp.data.for.equate[YEAR %in% tmp.years.for.equate & CONTENT_AREA==i & GRADE==j, 
				SCALE_SCORE_EQUATED:=equate.list[[paste(i, tmp.year.for.equate, sep=".")]][[paste("GRADE", j, sep="_")]][[conversion.type]][['interpolated_function']](SCALE_SCORE)]
		}
	}
	tmp.data.for.equate[!YEAR %in% tmp.years.for.equate, SCALE_SCORE_EQUATED:=SCALE_SCORE]

	return(tmp.data.for.equate)
} ### END convertScaleScore function
