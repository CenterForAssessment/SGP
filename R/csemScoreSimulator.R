csemScoreSimulator <-
function(
	scale_scores,
	grade,
	content_area,
	year,
	state,
	variable=NULL,
	distribution=NULL,
	round.digits=NULL) {

	GRADE <- CONTENT_AREA <- YEAR <- SIM <- V1 <- NULL

	get.my.knots.boundaries <- function(content_area, year) {
		tmp.knots.boundaries.names <- grep(content_area, names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)
		tmp.knots.boundaries  <- tmp.knots.boundaries.names[grep(paste0("^", content_area, "$"), sapply(strsplit(tmp.knots.boundaries.names, "[.]"), '[', 1L))]
		if (length(tmp.knots.boundaries)==1L) {
			return(content_area)
		} else {
			tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries, "[.]"), '[', 2L)
			tmp.index <- sum(year >= tmp.knots.boundaries.years, na.rm=TRUE)
			return(paste(c(content_area, sort(tmp.knots.boundaries.years)[tmp.index]), collapse="."))
		}
	}

	### Define relevant variables

	if (is.null(round.digits)) round.digits <- 1L
	if (is.null(distribution)) distribution <- "Normal"
	if (!is.null(state)) {
		min.max <- SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[get.my.knots.boundaries(content_area, year)]][[paste0("loss.hoss_", grade)]]
	} else {
		min.max <- range(scale_scores, na.rm=TRUE)
	}
	Interpolation_Function <- function(scale_score, variance, round.digits) return(splinefun(scale_score, variance/sqrt(round.digits), method="natural"))

	### Create scale score dependent CSEMs

	if (is.null(variable) && !is.null(state)) {
		if ("YEAR" %in% names(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			Interpolation_Data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]][GRADE==grade & CONTENT_AREA==content_area & YEAR==year]
		} else {
			Interpolation_Data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]][GRADE==grade & CONTENT_AREA==content_area]
		}
		tmp.omega <- Interpolation_Function(Interpolation_Data[['SCALE_SCORE']], Interpolation_Data[['SCALE_SCORE_CSEM']], round.digits)(scale_scores)
	}
	if (!is.null(variable)) {
		tmp.dt <- setkey(unique(data.table(V1=scale_scores, V2=variable), by="V1"), V1)
		tmp.omega <- Interpolation_Function(tmp.dt[['V1']], tmp.dt[['V2']], round.digits)(scale_scores)
	}
	if (distribution=="Skew-Normal") {
		tmp.scores <- data.table(SIM=round(rsn(length(scale_scores), xi=scale_scores, omega=tmp.omega,
			alpha=tan((pi/2)*((min.max[1]+min.max[2]) - 2*scale_scores)/(min.max[2]-min.max[1]))), digits=round.digits))
	} else {
		tmp.scores <- data.table(SIM=round(rnorm(length(scale_scores), scale_scores, tmp.omega), digits=round.digits))
	}
	tmp.scores[SIM < min.max[1L], SIM:=min.max[1L]]
	tmp.scores[SIM > min.max[2L], SIM:=min.max[2L]]
	return(tmp.scores[['SIM']])
} ### END csemScoreSimulator
