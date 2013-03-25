csemScoreSimulator <- 
function(
	scale_scores, 
	grade, 
	content_area, 
	year, 
	state, 
	variable, 
	distribution, 
	round) {

	GRADE <- CONTENT_AREA <- YEAR <- NULL
	if (is.null(round)) round <- 1
	if (is.null(distribution)) distribution <- "Normal"
	if (!is.null(state)) min.max <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", grade, sep="")]]
	if (!is.null(variable)) min.max <- range(scale_scores, na.rm=TRUE)
	if (!is.null(state)) {
		if ("YEAR" %in% names(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			CSEM_Data <- subset(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==grade & CONTENT_AREA==content_area & YEAR==year)
		} else {
			CSEM_Data <- subset(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==grade & CONTENT_AREA==content_area)
		}
		CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
		tmp.scale <- CSEM_Function(scale_scores)
	} 
	if (!is.null(variable)) {
		tmp.scale <- variable
	}
	if (distribution=="Skew-Normal") {
		tmp.shape <- tan((pi/2)*((min.max[1]+min.max[2]) - 2*scale_scores)/(min.max[2]-min.max[1]))
		tmp.score <- round_any(as.numeric(rsn(length(scale_scores), location=scale_scores, scale=tmp.scale, shape=tmp.shape)), round)
	}
	if (distribution=="Normal") {
		tmp.score <- round_any(as.numeric(rnorm(length(scale_scores), mean=scale_scores, sd=tmp.scale)), round)
	}
	tmp.score[tmp.score < min.max[1]] <- min.max[1]
	tmp.score[tmp.score > min.max[2]] <- min.max[2]
	return(tmp.score)
} ### END csemScoreSimulator
