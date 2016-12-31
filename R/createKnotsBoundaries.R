`createKnotsBoundaries` <-
function(tmp.data,
	knot.cut.percentiles=c(0.2,0.4,0.6,0.8)) {

	CONTENT_AREA <- GRADE <- VALID_CASE <- NULL

	tmp.grade.list <- tmp.list <- list()

	setkeyv(tmp.data, c("VALID_CASE", "CONTENT_AREA", "GRADE"))

	for (my.list.label in unique(tmp.data["VALID_CASE"], by="CONTENT_AREA")[["CONTENT_AREA"]]) {
		tmp.grade.list[[my.list.label]] <- sort(unique(tmp.data[SJ("VALID_CASE", my.list.label)], by="GRADE")[["GRADE"]])
		for (j in seq_along(tmp.grade.list[[my.list.label]])) {
			tmp.list[[my.list.label]][[3*j-2]] <-
				round(quantile(tmp.data[list("VALID_CASE", my.list.label, tmp.grade.list[[my.list.label]][j])][["SCALE_SCORE"]],
					probs=knot.cut.percentiles, na.rm=TRUE, names=FALSE), digits=3)
			tmp.list[[my.list.label]][[3*j-1]] <-
				round(extendrange(tmp.data[list("VALID_CASE", my.list.label, tmp.grade.list[[my.list.label]][j])][["SCALE_SCORE"]], f=0.1), digits=3)
			tmp.list[[my.list.label]][[3*j]] <-
				round(extendrange(tmp.data[list("VALID_CASE", my.list.label, tmp.grade.list[[my.list.label]][j])][["SCALE_SCORE"]], f=0.0), digits=3)
		}
		setattr(tmp.list[[my.list.label]], "names", paste0(rep(c("knots_", "boundaries_", "loss.hoss_"), length(tmp.grade.list[[my.list.label]])),
			rep(tmp.grade.list[[my.list.label]], each=3)))
	}

	return(tmp.list)
} ## END createKnotsBoundaries
