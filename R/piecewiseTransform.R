`piecewiseTransform` <- 
function(scale_score,
	state,
	content_area,
	year,
	grade,
	output.digits=1) {

	if (content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &&
		grade %in% as.numeric(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]]), "_")), 
			ncol=2, byrow=TRUE)[,2])) {
		my.knots_boundaries.label <- getMyLabel(state, content_area, year, "Knots_Boundaries")
		tmp.loss.hoss <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[my.knots_boundaries.label]][[paste("loss.hoss_", grade, sep="")]]
		scale_score[scale_score < tmp.loss.hoss[1]] <- tmp.loss.hoss[1]; scale_score[scale_score > tmp.loss.hoss[2]] <- tmp.loss.hoss[2]
		my.content_area <- getMyLabel(state, content_area, year)
		tmp.old.cuts <- c(tmp.loss.hoss[1], SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE_", grade, sep="")]], 
			tmp.loss.hoss[2])
		tmp.new.cuts <- SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]]
		tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
		tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
		round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
	} else {
		as.numeric(scale_score)
	}
} ## END piecewiseTransform Function
