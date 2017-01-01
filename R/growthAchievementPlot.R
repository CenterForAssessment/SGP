`growthAchievementPlot` <-
	function(
	gaPlot.sgp_object,
	gaPlot.students=NULL,
	gaPlot.percentile_trajectories,
	gaPlot.achievement_percentiles=c(.01, seq(.05, .95, by=.05), .99),
	gaPlot.show.scale.transformations=TRUE,
	gaPlot.grade_range,
	gaPlot.max.order.for.progression=NULL,
	gaPlot.start.points="Achievement Level Cuts",
	gaPlot.back.extrapolated.cuts=NULL,
	gaPlot.subtitle=TRUE,
	gaPlot.SGPt=NULL,
	state,
	content_area,
	year,
	format="print",
	baseline=FALSE,
	equated=NULL,
	output.format="PDF",
	output.folder,
	assessment.name) {

	CUTLEVEL <- GRADE <- YEAR <- ID <- SCALE_SCORE <- level_1_curve <- V1 <- VALID_CASE <- NULL
	TRANSFORMED_SCALE_SCORE <- PERCENTILE <- GRADE_NUMERIC <- CONTENT_AREA <- LEVEL <- SGP <- EXTRAPOLATED_P50_CUT <- DATE <- NULL ## To prevent R CMD check warnings
	SCALE_SCORE_PERCENTILES <- SCALE_SCORE_PERCENTILES_TRANSFORMED <- CUTSCORES <- CUTSCORES_TRANSFORMED <- College_Readiness_Cutscores <- NULL

	content_area <- toupper(content_area)
	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area]])) {
		content_area.all <- unique(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area]])
	} else {
		content_area.all <- content_area
	}
	number.achievement.level.regions <- length(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])

	cohort <- TRUE
	if (baseline) {cohort <- FALSE; equated <- NULL}
	if (!is.null(equated)) cohort <- baseline <- FALSE
	if (!is.null(gaPlot.students)) gaPlot.start.points <- "Individual Student"
	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.projection.unit.label"]])) {
		tmp.unit.label <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.projection.unit.label"]]
	} else {
		tmp.unit.label <- "YEAR"
	}


	## State stuff

	if (is.null(state.name.label <- suppressMessages(getStateAbbreviation(state, type="long")))) state.name.label <- test.abbreviation.label <- state
	state.name.file.label <- gsub(" ", "_", state.name.label)

	### Test if scale change has occured in the requested year

	if (is.null(equated) &&
			!identical(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Baseline_Projections_in_Transition_Year"]], TRUE) &&
			year %in% SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]) {
		message(paste0("\tNOTE: Based upon state scale changes in ", capwords(year), " student growth projections are not possible. No ",
			capwords(year), " ", content_area, " growth and achievement plot will be generated.\n"))
		return("DONE")
    }

	### Create folder for plots

	dir.create(output.folder, recursive=TRUE, showWarnings=FALSE)

	### Create LONG cutscores

	long_cutscores <- createLongCutscores(state, content_area, add.GRADE_NUMERIC=TRUE)
	long_cutscores <- long_cutscores[YEAR %in% sort(c(year, unique(long_cutscores$YEAR)), na.last=FALSE)[max(which(sort(c(year, unique(long_cutscores$YEAR)), na.last=FALSE)==year))-1]]
	if (!year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]][[content_area]]) {
		long_cutscores[,CUTSCORES_TRANSFORMED:=CUTSCORES]
	}

	### Create default values

	if (missing(gaPlot.grade_range)) {
		if (is.null(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area]])) {
			stop("\tNOTE: No grade range is available from supplied argument or SGP::SGPstateData to construct growth and achievement plots.\n")
		}
		gaPlot.grade_range <- range(long_cutscores$GRADE_NUMERIC, na.rm=TRUE)
	}

	if (!missing(state) & missing(gaPlot.percentile_trajectories)) {
		gaPlot.percentile_trajectories <- round(sort(unique(c(10, 50, 90, SGP::SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]]))/5)) * 5
	}
	if (missing(state) & missing(gaPlot.percentile_trajectories)) {
		gaPlot.percentile_trajectories <- c(10, 35, 50, 65, 90)
	}

	tmp.smooth.grades <- seq(gaPlot.grade_range[1], gaPlot.grade_range[2], by=0.01)
	tmp.unique.grades.numeric <- sort(unique(long_cutscores[['GRADE_NUMERIC']]))
	tmp.unique.grades.character <- data.table(long_cutscores, key="GRADE_NUMERIC")[list(tmp.unique.grades.numeric), mult="first"][["GRADE"]]
	tmp.unique.content_areas <- data.table(long_cutscores, key="GRADE_NUMERIC")[list(tmp.unique.grades.numeric), mult="first"][["CONTENT_AREA"]]
	tmp.unique.grades.current.year <- sort(unique(gaPlot.sgp_object@Data[VALID_CASE=="VALID_CASE" & YEAR==year][['GRADE']]))
	setkeyv(gaPlot.sgp_object@Data, c("VALID_CASE", "CONTENT_AREA"))
	growthAchievementPlot.data <- gaPlot.sgp_object@Data[CJ("VALID_CASE", content_area.all)][, list(ID, CONTENT_AREA, YEAR, GRADE, SCALE_SCORE, SGP)][
		GRADE %in% tmp.unique.grades.character & !is.na(SCALE_SCORE)]

	if (length(unique(tmp.unique.content_areas)) > 1) display.content_areas <- TRUE else display.content_areas <- FALSE

	if (missing(assessment.name) & missing(state)) {
		assessment.name <- NULL
	} else {
		assessment.name <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]
	}

	if (nchar(content_area) > 12)  {
		content_area.label <- capwords(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Labels"]][[content_area]])
	} else {
		content_area.label <- capwords(content_area)
	}

	cutscore.year <- sort(c(year, unique(long_cutscores$YEAR)), na.last=FALSE)[max(which(sort(c(year, unique(long_cutscores$YEAR)), na.last=FALSE)==year))-1]
	temp_cutscores <- long_cutscores[GRADE %in% tmp.unique.grades.character & !CUTLEVEL %in% c("LOSS", "HOSS") & YEAR %in% cutscore.year & CONTENT_AREA %in% tmp.unique.content_areas][,CUTLEVEL:=as.numeric(CUTLEVEL)]
	setkeyv(temp_cutscores, c("GRADE_NUMERIC", "CONTENT_AREA"))

	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["gaPlot.back.extrapolated.cuts"]])) {
		gaPlot.back.extrapolated.cuts <-
			SGP::SGPstateData[[state]][["SGP_Configuration"]][["gaPlot.back.extrapolated.cuts"]][[content_area]]
	}
	if (identical(gaPlot.back.extrapolated.cuts, TRUE)) {
		gaPlot.back.extrapolated.cuts <-
			temp_cutscores[CONTENT_AREA==tail(content_area.all, 1) & GRADE_NUMERIC==gaPlot.grade_range[2] & CUTLEVEL==which.max(SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1][['CUTSCORES']]
	}
	if (!is.null(gaPlot.back.extrapolated.cuts)) {
		College_Readiness_Cutscores <- gaPlot.back.extrapolated.cuts
	}

	if (!is.null(gaPlot.SGPt)) {
		if (identical(gaPlot.SGPt, TRUE)) gaPlot.SGPt <- "DATE"
		if (!all(gaPlot.SGPt %in% names(gaPlot.sgp_object@Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(gaPlot.SGPt, collapse=", "), "are not all contained in the supplied 'gaPlot.sgp_object@Data'. 'SGPt' is set to NULL.\n")
			gaPlot.SGPt <- NULL
		}
	}

	if (!is.null(SGP::SGPstateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]][[content_area]])) {
			College_Readiness_Cutscores <- SGP::SGPstateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]][[content_area]]
	}


	## Utility functions

	# Functions to create good endpoints for scale score axis

	pretty_year <- function(x) sub("_", "-", x)

	myround <- function(low.and.high) {
		base <- 5*10^(floor(log(diff(low.and.high), 10))-1)
		return(c(base*ceiling(low.and.high[1]/base), base*floor(low.and.high[2]/base)))
	}

	my_min <- function(x) {
		if (is.null(x)) return(NULL) else return(min(x))
	}

	gaPlot.percentile_trajectories_Internal <- function(tmp.dt, percentile, content_area, year, state) {

		.create.path <- function(labels, pieces=c("my.subject", "my.year", "my.extra.label")) {
			sub(' ', '_', toupper(sub('\\.+$', '', paste(unlist(sapply(labels[pieces], as.character)), collapse="."))))
		}

		gaPlot.sgp_object@SGP$Panel_Data <- tmp.dt
		gaPlot.sgp_object@SGP$SGProjections <- NULL
		if (is.null(gaPlot.SGPt)) {
			tmp.grades <- as.character(tmp.dt[1,2:((dim(tmp.dt)[2]+1)/2), with=FALSE])
		} else {
			tmp.grades <- as.character(tmp.dt[1,2:((dim(tmp.dt)[2]-2+1)/2), with=FALSE])
		}
		if (baseline) my.extra.label <- "BASELINE"
		if (!is.null(equated)) my.extra.label <- "EQUATED"
		if (cohort) my.extra.label <- NULL
		if (content_area %in% names(SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]])) {
			tmp.content_area <- content_area
		} else {
			tmp.content_area <- grep(content_area, names(SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]]), value=TRUE)[1]
		}

		studentGrowthProjections(
			panel.data=gaPlot.sgp_object@SGP,
			sgp.labels=list(my.year=year, my.subject=content_area, my.extra.label=my.extra.label),
			projcuts.digits=2,
			projection.unit="GRADE",
			percentile.trajectory.values=percentile,
			grade.progression=tmp.grades,
			grade.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tmp.content_area]],
			content_area.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tmp.content_area]],
			year_lags.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tmp.content_area]],
			max.order.for.progression=my_min(c(gaPlot.max.order.for.progression, getMaxOrderForProgression(year, content_area, state, equated))),
			sgp.projections.equated=equated,
			panel.data.vnames=c("ID", grep("GRADE|SCALE_SCORE", names(tmp.dt), value=TRUE)),
			sgp.projections.use.only.complete.matrices=SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']],
			SGPt=gaPlot.SGPt,
			print.time.taken=FALSE)[["SGProjections"]][[.create.path(list(my.subject=content_area, my.year=year, my.extra.label=my.extra.label))]][,-1, with=FALSE]
	}

	smoothPercentileTrajectory <- function(tmp.dt, grade.projection.sequence.numeric, grade.projection.sequence, content_area.projection.sequence, percentile, content_area, year, state) {
		if (is.null(gaPlot.SGPt)) {
			tmp.trajectories <- gaPlot.percentile_trajectories_Internal(tmp.dt, percentile, content_area, year, state)
		} else {
			tmp.trajectories <- gaPlot.percentile_trajectories_Internal(tmp.dt, percentile, content_area, year, state)[,-1,with=FALSE]
		}
		trajectories <- c(as.numeric(tail(tmp.dt[,grep("SCALE_SCORE", names(tmp.dt), value=TRUE), with=FALSE], 1)), as.numeric(tmp.trajectories))

		if (year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]][[content_area]]) {
			tmp.function <- function(tmp.iter) {
				sapply(tmp.iter, function(x) piecewiseTransform(trajectories[x], state, content_area.projection.sequence[x], as.character(year), grade.projection.sequence[x]))
			}
			return(splinefun(grade.projection.sequence.numeric, tmp.function(seq_along(grade.projection.sequence))))
		} else {
			return(splinefun(grade.projection.sequence.numeric, trajectories))
		}
	}

	getSGPtDate <- function(year) {
		tmp.split <- unlist(strsplit(year, "[.]"))
		tmp.year <- unlist(strsplit(tmp.split[1], "_"))
		if (length(tmp.split)==2) tmp.period <- tmp.split[2] else tmp.period <- NULL
		if (is.null(tmp.period)) tmp.date <- "05-01"
		if (tmp.period==1) {tmp.year <- head(tmp.year, 1); tmp.date <- "08-01"}
		if (tmp.period==2) {tmp.year <- tail(tmp.year, 1); tmp.date <- "01-15"}
		if (tmp.period==3) {tmp.year <- tail(tmp.year, 1); tmp.date <- "07-31"}
		return(as.Date(paste(tmp.year, tmp.date, sep="-")))
	}

	stextGrob <- function (label, r=0.1, x = x, y = y,
		just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
		default.units = "native", name = NULL, gp = gpar(), vp = NULL){
		# http://stackoverflow.com/questions/7734535/control-font-thickness-without-changing-font-size

		let <- textGrob("a", gp=gp, vp=vp)
		wlet <- grobWidth(let)
		hlet <- grobHeight(let)

		tg <- textGrob(label=label, x=x, y=y, gp=gpar(col="white"), just = just, hjust = hjust, vjust = vjust, rot = rot,
				check.overlap = check.overlap, default.units = default.units)

		tgl <- c(lapply(seq(0, 2*pi, length=36), function(theta){
		  textGrob(label=label,x=x+cos(theta)*r*wlet, y=y+sin(theta)*r*hlet, gp=gpar(col="black"),
				just = just, hjust = hjust, vjust = vjust, rot = rot, check.overlap = check.overlap, default.units = default.units)
		  }), list(tg))

		g <- gTree(children=do.call(gList, tgl), vp=vp, name=name, gp=gp)
	}

	grid.stext <- function(...){
		g <- stextGrob(...)
		grid.draw(g)
		invisible(g)
	}


	### Calculate Scale Transformations (if required)

	growthAchievementPlot.data[, TRANSFORMED_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA, YEAR, GRADE), by=list(CONTENT_AREA, YEAR, GRADE)]
	if (year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]][[content_area]]) {
		gaPlot.show.scale.transformations <- FALSE
	}


	### Calculate ACHIEVEMENT percentiles

	if (!is.null(gaPlot.achievement_percentiles)) {

		## Creating the points used by lines to construct unconditional percentile curves

		setkey(growthAchievementPlot.data, GRADE, CONTENT_AREA)
		setkey(long_cutscores, GRADE, CONTENT_AREA)
		growthAchievementPlot.data <- unique(long_cutscores[!is.na(GRADE_NUMERIC), c("GRADE", "CONTENT_AREA", "GRADE_NUMERIC"), with=FALSE], by=c("GRADE", "CONTENT_AREA"))[growthAchievementPlot.data]
		setnames(growthAchievementPlot.data, c("GRADE", "GRADE_NUMERIC"), c("GRADE_CHARACTER", "GRADE"))
		setkey(growthAchievementPlot.data, YEAR)
		tmp.percentiles <- growthAchievementPlot.data[list(year)][,
				list(SCALE_SCORE_PERCENTILES=quantile(SCALE_SCORE, probs=gaPlot.achievement_percentiles, na.rm=TRUE),
				SCALE_SCORE_PERCENTILES_TRANSFORMED=quantile(TRANSFORMED_SCALE_SCORE, probs=gaPlot.achievement_percentiles, na.rm=TRUE)), keyby=list(GRADE, CONTENT_AREA)][
				list(tmp.unique.grades.numeric)][,PERCENTILE:=rep(gaPlot.achievement_percentiles, length(tmp.unique.grades.numeric))]
		temp_uncond_frame <- matrix(tmp.percentiles[,splinefun(GRADE, SCALE_SCORE_PERCENTILES_TRANSFORMED)(tmp.smooth.grades), by=PERCENTILE][['V1']], nrow=length(gaPlot.achievement_percentiles), byrow=TRUE)
		temp_uncond_frame_UNTRANSFORMED <- matrix(tmp.percentiles[,splinefun(GRADE, SCALE_SCORE_PERCENTILES)(tmp.smooth.grades), by=PERCENTILE][['V1']], nrow=length(gaPlot.achievement_percentiles), byrow=TRUE)
		rownames(temp_uncond_frame) <- rownames(temp_uncond_frame_UNTRANSFORMED) <- gaPlot.achievement_percentiles
		colnames(temp_uncond_frame) <- colnames(temp_uncond_frame_UNTRANSFORMED) <- tmp.smooth.grades
	}


	### Calculate Extrapolated Cuts based upon SGP growth of 50, 60, 70, 80, and 90 (if requested)

	if (!is.null(gaPlot.back.extrapolated.cuts)) {
		tmp.extrapolated.cuts.list <- list()
		tmp.inf.sup.functions <- c(function(x) quantile(x, prob=0.975), function(x) quantile(x, prob=0.5))
		setkey(growthAchievementPlot.data, CONTENT_AREA, YEAR, ID)
		if (is.null(tmp.proj.name <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area]])) tmp.proj.name <- content_area
		if (baseline) tmp.proj.name <- unique(paste(tmp.proj.name, year, "BASELINE", sep=".")) else tmp.proj.name <- unique(paste(tmp.proj.name, year, sep="."))
		for (tmp.proj.name.iter in intersect(tmp.proj.name, names(gaPlot.sgp_object@SGP$SGProjections))) {
			tmp.extrapolated.cuts.list[[tmp.proj.name.iter]] <- gaPlot.sgp_object@SGP$SGProjections[[tmp.proj.name.iter]][,c("ID", grep("P50|P60|P70|P80|P90", names(gaPlot.sgp_object@SGP$SGProjections[[tmp.proj.name.iter]]), value=TRUE)), with=FALSE]
			tmp.extrapolated.cuts.list[[tmp.proj.name.iter]][,c("CONTENT_AREA", "YEAR"):=list(unlist(strsplit(tmp.proj.name.iter, "[.]"))[1], sub(".BASELINE", "", paste(tail(unlist(strsplit(tmp.proj.name.iter, "[.]")), -1), collapse=".")))]
		}
		tmp.projections <- rbindlist(tmp.extrapolated.cuts.list, fill=TRUE)
		setkey(tmp.projections, CONTENT_AREA, YEAR, ID)
		tmp.projections <- growthAchievementPlot.data[tmp.projections]
		extrapolated.cuts.dt <- data.table(long_cutscores, key="GRADE_NUMERIC")[list(head(seq(gaPlot.grade_range[1], gaPlot.grade_range[2]), -1)), mult="first"][,c("GRADE", "GRADE_NUMERIC", "CONTENT_AREA"), with=FALSE]
		for (percentile.iter in c(50, 60, 70, 80, 90)) {
			for (i in seq(dim(extrapolated.cuts.dt)[1])) {
				tmp.projection.label <- grep(paste(paste0("P", percentile.iter), "PROJ", tmp.unit.label, i, "", sep="_"), names(tmp.projections), value=TRUE)
				tmp.inf.sup <- list(tmp.projections[GRADE==rev(extrapolated.cuts.dt$GRADE_NUMERIC)[i] & get(tmp.projection.label) < gaPlot.back.extrapolated.cuts][['SCALE_SCORE']],
									tmp.projections[GRADE==rev(extrapolated.cuts.dt$GRADE_NUMERIC)[i] & get(tmp.projection.label) >= gaPlot.back.extrapolated.cuts][['SCALE_SCORE']])
				for (j in 1:2) if (length(tmp.inf.sup[[j]]) > 0) tmp.inf.sup[[j]] <- tmp.inf.sup.functions[[j]](tmp.inf.sup[[j]]) else tmp.inf.sup[[j]] <- NaN
				tmp.inf.sup <- unlist(tmp.inf.sup)
				if (year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]][[content_area]]) {
					extrapolated.cuts.dt[GRADE_NUMERIC==rev(extrapolated.cuts.dt$GRADE_NUMERIC)[i],
						paste0("EXTRAPOLATED_P", percentile.iter, "_CUT"):=
							piecewiseTransform(tail(tmp.inf.sup[is.finite(tmp.inf.sup)], 1),
												state,
												CONTENT_AREA,
												year,
												GRADE)]
				} else {
					extrapolated.cuts.dt[GRADE_NUMERIC==rev(extrapolated.cuts.dt$GRADE_NUMERIC)[i], paste0("EXTRAPOLATED_P", percentile.iter, "_CUT"):=tail(tmp.inf.sup[is.finite(tmp.inf.sup)], 1)]
				}
			}
		}
		extrapolated.cuts.dt[,(4:8):=as.data.table(t(apply(as.matrix(extrapolated.cuts.dt[,(4:8), with=FALSE]), 1, sort, decreasing=TRUE)))]

		if (year %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]][[content_area]]) {
			tmp.dt <- data.table(long_cutscores, key="GRADE_NUMERIC")[list(tail(seq(gaPlot.grade_range[1], gaPlot.grade_range[2]), 1)), mult="first"][,c("GRADE", "GRADE_NUMERIC", "CONTENT_AREA"), with=FALSE]
			tmp.dt <- data.table(matrix(c(tmp.dt[['GRADE_NUMERIC']],
								rep(piecewiseTransform(gaPlot.back.extrapolated.cuts,
														state,
														tmp.dt[['CONTENT_AREA']],
														year,
														tmp.dt[['GRADE']]), 5)), nrow=1))
			College_Readiness_Cutscores <- tmp.dt[[6]]
		} else {
			tmp.dt <- data.table(matrix(c(gaPlot.grade_range[2], rep(gaPlot.back.extrapolated.cuts, 5)), nrow=1))
		}

		extrapolated.cuts.list <- as.list(rbindlist(list(extrapolated.cuts.dt[,!c("GRADE", "CONTENT_AREA"), with=FALSE], tmp.dt)))
		for (col.iter in 2:6) extrapolated.cuts.list[[col.iter]] <- spline(extrapolated.cuts.list[[1]], extrapolated.cuts.list[[col.iter]], n=40, method="natural")[['y']]
		extrapolated.cuts.list[[1]] <- spline(extrapolated.cuts.list[[1]], n=40, method="natural")[['y']]
		extrapolated.cuts.dt <- as.data.table(extrapolated.cuts.list)
	}


	################################################
	###
	### Code for producing the chart
	###
	################################################

	if (format=="print") {
		format.colors.background <- rgb(0.985, 0.985, 1.0)
		format.colors.region <- paste0("grey", round(seq(62, 91, length=number.achievement.level.regions)))
		format.colors.font <- "grey20"
		format.colors.growth.trajectories <- "black"
	} else {
		format.colors.background <- rgb(0.48, 0.48, 0.52)
		format.colors.region <- tail(c("#2868a4", "#3282cd", "#5b9bd7", "#84b4e1", "#adcdeb", "#d6e6f5"), number.achievement.level.regions)
		format.colors.font <- rgb(0.985, 0.985, 1.0)
		format.colors.growth.trajectories <- rgb(0.985, 0.985, 1.0)
	}

	xscale.range <- c(gaPlot.grade_range[1]-0.5, gaPlot.grade_range[2]+0.5)

	tmp.content_area.starting.points <- unique(unlist(lapply(strsplit(names(gaPlot.sgp_object@SGP[['Coefficient_Matrices']]), "[.]"), '[', 1)))


	## Create data sets to be used for plot production

	if (is.null(gaPlot.students)) {
		tmp.cutscores <- data.table(long_cutscores[!CUTLEVEL %in% c("HOSS", "LOSS") &
													!GRADE %in% c("GRADE_LOWER", "GRADE_UPPER") &
													GRADE %in% tmp.unique.grades.current.year &
													GRADE_NUMERIC!=max(GRADE_NUMERIC, na.rm=TRUE) &
													YEAR %in% tail(sort(unique(YEAR), na.last=FALSE), 1)], key=c("GRADE_NUMERIC", "CONTENT_AREA"))
		if (gaPlot.start.points=="Achievement Level Cuts") {
			tmp1.dt <- data.table(
					ID=seq(dim(tmp.cutscores)[1]),
					GRADE=tmp.cutscores[['GRADE']],
					GRADE_NUMERIC=tmp.cutscores[['GRADE_NUMERIC']],
					CONTENT_AREA=tmp.cutscores[['CONTENT_AREA']],
					SCALE_SCORE=tmp.cutscores[['CUTSCORES']],
					TRANSFORMED_SCALE_SCORE=tmp.cutscores[['CUTSCORES_TRANSFORMED']],
					LEVEL=as.numeric(tmp.cutscores[['CUTLEVEL']]),
					key=c("GRADE", "SCALE_SCORE"))
		}
		if (gaPlot.start.points=="Achievement Percentiles") {
			tmp1.dt <- data.table(
					ID="1",
					GRADE=rep(unique(tmp.cutscores, by=key(tmp.cutscores))[['GRADE']], each=dim(temp_uncond_frame)[1]),
					GRADE_NUMERIC=rep(unique(tmp.cutscores, by=key(tmp.cutscores))[['GRADE_NUMERIC']], each=dim(temp_uncond_frame)[1]),
					CONTENT_AREA=rep(unique(tmp.cutscores, by=key(tmp.cutscores))[['CONTENT_AREA']], each=dim(temp_uncond_frame)[1]),
					SCALE_SCORE=as.vector(temp_uncond_frame_UNTRANSFORMED[,as.character(unique(tmp.cutscores, by=key(tmp.cutscores))[['GRADE_NUMERIC']])]),
					TRANSFORMED_SCALE_SCORE=as.vector(temp_uncond_frame[,as.character(unique(tmp.cutscores, by=key(tmp.cutscores))[['GRADE_NUMERIC']])]),
					LEVEL=as.numeric(rownames(temp_uncond_frame)),
					key=c("GRADE", "SCALE_SCORE"))[,ID:=as.character(seq(.N))]
		}
		tmp1.dt <- tmp1.dt[CONTENT_AREA %in% tmp.content_area.starting.points & GRADE %in% tmp.unique.grades.current.year]
	} else {
		setkey(growthAchievementPlot.data, ID)
		tmp1.dt <- growthAchievementPlot.data[gaPlot.students]
		setnames(tmp1.dt, c("GRADE_CHARACTER", "GRADE"), c("GRADE", "GRADE_NUMERIC"))
		tmp1.dt <- tmp1.dt[ID %in% unique(tmp1.dt[YEAR==year & CONTENT_AREA][['ID']])]
	}


	## Start loop over students or starting scores

	for (j in unique(tmp1.dt[['ID']])) {

		started.at <- proc.time()
		started.date <- prettyDate()

		tmp2.dt <- tmp1.dt[ID==j]
		tmp.dt <- data.table(ID=j, data.frame(lapply(tmp2.dt[,c("GRADE", "SCALE_SCORE"), with=FALSE], function(x) t(data.frame(x))), stringsAsFactors=FALSE))
		if (!is.null(gaPlot.SGPt)) tmp.dt[,DATE:=getSGPtDate(year)]
		tmp.smooth.grades.trajectories <- tmp.smooth.grades[tmp.smooth.grades >= tail(tmp2.dt[['GRADE_NUMERIC']], 1)]
		grade.projection.sequence.numeric <- intersect(tmp.smooth.grades.trajectories, tmp.unique.grades.numeric)
		content_area.projection.sequence <- temp_cutscores[list(grade.projection.sequence.numeric), mult="first"][['CONTENT_AREA']]
		grade.projection.sequence <- temp_cutscores[list(grade.projection.sequence.numeric), mult="first"][['GRADE']]

		## Create Percentile Trajectory functions

		smoothPercentileTrajectory_Functions <- lapply(sort(gaPlot.percentile_trajectories), function(i) smoothPercentileTrajectory(tmp.dt, grade.projection.sequence.numeric, grade.projection.sequence, content_area.projection.sequence, i, tail(tmp2.dt[['CONTENT_AREA']], 1), year, state))
		names(smoothPercentileTrajectory_Functions) <- as.character(sort(gaPlot.percentile_trajectories))

		## Define axis ranges based (ranges contingent upon starting score)

		setkey(growthAchievementPlot.data, YEAR)
		gp.axis.range <- c(smoothPercentileTrajectory_Functions[[1]](gaPlot.grade_range[[2]]),
			smoothPercentileTrajectory_Functions[[length(gaPlot.percentile_trajectories)]](gaPlot.grade_range[[2]]))
		yscale.range <- extendrange(c(min(gp.axis.range[1], quantile(growthAchievementPlot.data[year][['TRANSFORMED_SCALE_SCORE']], prob=.005, na.rm=TRUE), tmp2.dt[['TRANSFORMED_SCALE_SCORE']], na.rm=TRUE),
			max(gp.axis.range[2], quantile(growthAchievementPlot.data[list(year)]$TRANSFORMED_SCALE_SCORE, prob=.995, na.rm=TRUE), tmp2.dt[['TRANSFORMED_SCALE_SCORE']], na.rm=TRUE)), f=0.075)
		ach.per.axis.range <- (temp_uncond_frame[,1])[temp_uncond_frame[,1] >= yscale.range[1] & temp_uncond_frame[,1] <= yscale.range[2]]
		ach.per.axis.labels <- formatC(100*as.numeric(rownames(temp_uncond_frame)[temp_uncond_frame[,1] >= yscale.range[1] & temp_uncond_frame[,1] <= yscale.range[2]]),
			digits=0, format="f")


		##
		## Create viewports
		##

		if (is.null(College_Readiness_Cutscores)) {
			growth.achievement.vp <- viewport(layout = grid.layout(6, 3, widths = unit(c(1.5, 6.5, 0.5), rep("inches", 3)),
				heights = unit(c(0.25, 1.15, 0.35, 8.25, 0.5, 0.35), rep("inches", 6))))
		} else {
			growth.achievement.vp <- viewport(layout = grid.layout(6, 3, widths = unit(c(1.4, 6.5, 0.6), rep("inches", 3)),
				heights = unit(c(0.25, 1.15, 0.35, 8.25, 0.5, 0.35), rep("inches", 6))))
		}

		chart.vp <- viewport(name="chart.vp",
			layout.pos.row=4, layout.pos.col=2,
			xscale=xscale.range,
			yscale=yscale.range,
			clip="on",
			gp=gpar(fill="transparent"))

		left.axis.vp <- viewport(name="left.axis.vp",
			layout.pos.row=4, layout.pos.col=1,
			xscale=c(0,1),
			yscale=yscale.range,
			gp=gpar(fill="transparent"))

		right.axis.vp <- viewport(name="right.axis.vp",
			layout.pos.row=4, layout.pos.col=3,
			xscale=c(0,1),
			yscale=yscale.range,
			gp=gpar(fill="transparent"))

		bottom.axis.vp <- viewport(name="bottom.axis.vp",
			layout.pos.row=5, layout.pos.col=2,
			xscale=xscale.range,
			yscale=c(0,1),
			gp=gpar(fill="transparent"))

		title.vp <- viewport(name="title.vp",
			layout.pos.row=2, layout.pos.col=1:3,
			xscale=c(0,1),
			yscale=c(0,1),
			gp=gpar(fill="transparent"))

		##
		## Loop over output.format
		##

		for (k in output.format) {

			if (baseline) my.label <- "_State_Baseline_Growth_and_Achievement_Plot_"
			if (!is.null(equated)) my.label <- "_State_Equated_Growth_and_Achievement_Plot_"
			if (cohort)	my.label <- "_State_Growth_and_Achievement_Plot_"

			if (k=="PDF") tmp.suffix <- ".pdf" else tmp.suffix <- ".png"
			if (gaPlot.start.points=="Achievement Level Cuts") {
				tmp.file.name <- paste0(output.folder, "/", state.name.file.label, my.label, gsub(" ", "_", capwords(tail(tmp2.dt[['CONTENT_AREA']], 1))), "_", year, "_Level_", tmp2.dt[['LEVEL']], "_Grade_", tmp2.dt[['GRADE']], tmp.suffix)
			}
			if (gaPlot.start.points=="Achievement Percentiles") {
				tmp.file.name <- paste0(output.folder, "/", state.name.file.label, my.label, gsub(" ", "_", capwords(tail(tmp2.dt[['CONTENT_AREA']], 1))), "_", year, "_Percentile_", as.integer(100*tmp2.dt[['LEVEL']]), "_Grade_", tmp2.dt[['GRADE']], tmp.suffix)
			}
			if (gaPlot.start.points=="Individual Student") {
				tmp.file.name <- paste0(output.folder, "/", state.name.file.label, my.label, gsub(" ", "_", capwords(tail(tmp2.dt[['CONTENT_AREA']], 1))), "_", year, "_Student_Number_", tmp2.dt[['ID']][1], tmp.suffix)
			}

			if (k=="PDF") pdf(file=tmp.file.name, width=8.5, height=11, bg=format.colors.background)
			if (k=="PNG") Cairo(file=tmp.file.name, width=8.5, height=11, units="in", dpi=144, pointsize=10.5, bg=format.colors.background)


			##
			## Push growth.achievement.vp
			##

			pushViewport(growth.achievement.vp)


			##
			## Push chart.vp
			##

			pushViewport(chart.vp)


			##
			## Code for coloring performance level areas
			##

			## Create spline functions to calculate boundary values for each cutlevel

			for (i in 1:max(temp_cutscores$CUTLEVEL)){
				assign(paste0("level_", i, "_curve"), splinefun(tmp.unique.grades.numeric, subset(temp_cutscores, CUTLEVEL==i)[['CUTSCORES_TRANSFORMED']]))
			}


			## Create variables for boundaries and plotting

			x.boundary.values.1 <- c(gaPlot.grade_range[1], seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), gaPlot.grade_range[2])
			if (max(temp_cutscores$CUTLEVEL) > 1) {
				for (i in 2:max(temp_cutscores$CUTLEVEL)){
					assign(paste0("x.boundary.values.", i), c(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), seq(gaPlot.grade_range[2], gaPlot.grade_range[1], length=40)))
				}
				assign(paste0("x.boundary.values.", max(temp_cutscores$CUTLEVEL)+1), c(gaPlot.grade_range[1], seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), gaPlot.grade_range[2]))
			}


			y.boundary.values.1 <- c(yscale.range[1], level_1_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40)), yscale.range[1])
			for (i in 2:max(temp_cutscores$CUTLEVEL)) {
				assign(paste0("y.boundary.values.", i), c(eval(parse(text=paste0("level_", i-1, "_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40))"))),
					eval(parse(text=paste0("level_", i, "_curve(seq(gaPlot.grade_range[2], gaPlot.grade_range[1], length=40))")))))
			}
			assign(paste0("y.boundary.values.", max(temp_cutscores$CUTLEVEL)+1), c(yscale.range[2],
				eval(parse(text=paste0("level_", max(temp_cutscores$CUTLEVEL) , "_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40))"))), yscale.range[2]))


			## Create colored (grey-scale) regions

			for (i in 1:(1+max(temp_cutscores$CUTLEVEL))){
				grid.polygon(x=get(paste0("x.boundary.values.", i)),
					y=get(paste0("y.boundary.values.", i)),
					default.units="native",
					gp=gpar(fill=format.colors.region[i], lwd=0.1, col="grey85"))
			}

			## Code for producing extrapolated typical growth region (if requested)

			if (!is.null(gaPlot.back.extrapolated.cuts)) {
				tmp.cuts <- c(50,60,70,80,90)
				tmp.region.colors <- c("#abd9e9", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027")
				for (cut.iter in seq(length(tmp.cuts)+1)) {
					if (cut.iter==1) {
						grid.polygon(x=c(extrapolated.cuts.dt[['GRADE_NUMERIC']][1], extrapolated.cuts.dt[['GRADE_NUMERIC']], rev(extrapolated.cuts.dt[['GRADE_NUMERIC']])[1]),
						y=c(get(paste0("y.boundary.values.", 1+max(temp_cutscores[['CUTLEVEL']])))[1], extrapolated.cuts.dt[[paste0("EXTRAPOLATED_P", tmp.cuts[1], "_CUT")]], rev(get(paste0("y.boundary.values.", 1+max(temp_cutscores[['CUTLEVEL']]))))[1]),
						gp=gpar(fill=tmp.region.colors[cut.iter], lwd=0.1, lty=2, col="grey85", alpha=0.5), default.units="native")
					}

					if (cut.iter > 1 & cut.iter < length(tmp.cuts)+1) {
						grid.polygon(x=c(extrapolated.cuts.dt[['GRADE_NUMERIC']], rev(extrapolated.cuts.dt[['GRADE_NUMERIC']])),
						y=c(extrapolated.cuts.dt[[paste0("EXTRAPOLATED_P", tmp.cuts[cut.iter-1], "_CUT")]], rev(extrapolated.cuts.dt[[paste0("EXTRAPOLATED_P", tmp.cuts[cut.iter], "_CUT")]])),
						gp=gpar(fill=tmp.region.colors[cut.iter], lwd=0.1, lty=2, col="grey85", alpha=0.5), default.units="native")
					}

					if (cut.iter==length(tmp.cuts)+1) {
						grid.polygon(x=c(extrapolated.cuts.dt[['GRADE_NUMERIC']][1], extrapolated.cuts.dt[['GRADE_NUMERIC']], rev(extrapolated.cuts.dt[['GRADE_NUMERIC']])[1]),
						y=c(y.boundary.values.1[1], extrapolated.cuts.dt[[paste0("EXTRAPOLATED_P", rev(tmp.cuts)[1], "_CUT")]], rev(y.boundary.values.1[1])),
						gp=gpar(fill=tmp.region.colors[cut.iter], lwd=0.1, lty=2, col="grey85", alpha=0.5), default.units="native")
					}
				}
			}

			## Code for producing the achievement percentile curves

			if (!is.null(gaPlot.achievement_percentiles)) {

				for (i in rownames(temp_uncond_frame)) {
					grid.lines(tmp.smooth.grades, temp_uncond_frame[i,], gp=gpar(lwd=0.3, col="white"), default.units="native")
				}
			}

			## Code for producing percentile growth trajectories

			if (!is.null(gaPlot.percentile_trajectories)){
				for (i in gaPlot.percentile_trajectories) {
					grid.lines(tmp.smooth.grades.trajectories, smoothPercentileTrajectory_Functions[[as.character(i)]](tmp.smooth.grades.trajectories),
						gp=gpar(lwd=1.7, col="black"), default.units="native")
					grid.lines(tmp.smooth.grades.trajectories, smoothPercentileTrajectory_Functions[[as.character(i)]](tmp.smooth.grades.trajectories),
						gp=gpar(lwd=1.5, lty=5, col=getSGPColor(i, format)), default.units="native")
				}
				grid.circle(x=tmp.smooth.grades.trajectories[1], y=smoothPercentileTrajectory_Functions[[as.character(i)]](tmp.smooth.grades.trajectories[1]),
					r=unit(0.04, "inches"), gp=gpar(col="black", lwd=0.6, fill="white"), default.units="native")
			}

			## Code for producing historical student scores

			if (!is.null(gaPlot.students)) {
				grid.lines(tmp2.dt[['GRADE_NUMERIC']], tmp2.dt[['TRANSFORMED_SCALE_SCORE']], gp=gpar(lwd=1.7, col="black"), default.units="native")
				for (i in seq(dim(tmp2.dt)[1]-1)) {
					grid.lines(tmp2.dt[['GRADE_NUMERIC']][c(i,i+1)], tmp2.dt[['TRANSFORMED_SCALE_SCORE']][c(i,i+1)], gp=gpar(lwd=1.5, col=getSGPColor(tmp2.dt[['SGP']][i+1], format)), default.units="native")
				}
				grid.circle(x=tmp2.dt[['GRADE']], y=tmp2.dt[['TRANSFORMED_SCALE_SCORE']], r=unit(0.04, "inches"), gp=gpar(col="black", lwd=0.6, fill="white"), default.units="native")
			}

			## Code for producing skipped grade region

			skipped.grades <- setdiff(min(tmp.unique.grades.numeric, na.rm=TRUE):max(tmp.unique.grades.numeric, na.rm=TRUE), tmp.unique.grades.numeric)
			if (length(skipped.grades) > 0) {
				for (i in skipped.grades) {
					grid.polygon(x=c(i-0.4, i-0.4, i+0.4, i+0.4), y=c(yscale.range, rev(yscale.range)), default.units="native",
						gp=gpar(fill=rgb(1,1,1,0.4), lwd=1, col=rgb(1,1,1,0.4)))
					grid.text(x=unit(i, "native"), y=0.5, paste("No Grade", i, "Assessment"), gp=gpar(col="grey20", cex=2.0), rot=90)
				}
			}

			## Code for producing subtitle

			if (gaPlot.subtitle) {
				if (gaPlot.start.points=="Achievement Level Cuts") {
					tmp.text <- paste0("SGP trajectories for a ", toOrdinal(trunc(tmp2.dt[['GRADE_NUMERIC']])), " grade student starting from the Level ", tmp2.dt[['LEVEL']], "/Level ", tmp2.dt[['LEVEL']]+1, " cut.")
				}
				if (gaPlot.start.points=="Achievement Percentiles") {
					tmp.text <- paste0("SGP trajectories for a ", toOrdinal(trunc(tmp2.dt[['GRADE_NUMERIC']])), " grade student starting from the ", toOrdinal(as.integer(100*tmp2.dt[['LEVEL']])), " achievement percentile.")
				}
				if (gaPlot.start.points=="Individual Student") {
					tmp.text <- paste0("SGP trajectories for student ", tmp2.dt[['ID']][1], " starting from their ", toOrdinal(trunc(tail(tmp2.dt[['GRADE_NUMERIC']], 1))), " grade result.")
				}
#				grid.text(x=0.5, y=0.035, tmp.text, gp=gpar(col="white", cex=0.9))
				grid.stext(tmp.text, x=unit(0.5, "npc"), y=unit(0.035, "npc"), gp=gpar(cex=0.9))
			}


			popViewport() ## pop chart.vp


			##
			## Left Axis Viewport
			##

			pushViewport(left.axis.vp)

			if (gaPlot.show.scale.transformations) {
				ss.axis.range <- myround(yscale.range)
				grid.lines(0.6, ss.axis.range, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
				tmp.diff <- ss.axis.range[2]-ss.axis.range[1]
				if (tmp.diff < 2.5) my.by <- 0.25
				if (tmp.diff >= 2.5 & tmp.diff < 5) my.by <- 0.5
				if (tmp.diff >= 5 & tmp.diff < 25) my.by <- 2.5
				if (tmp.diff >= 25 & tmp.diff < 50) my.by <- 5
				if (tmp.diff >= 50 & tmp.diff < 250) my.by <- 25
				if (tmp.diff >= 250 & tmp.diff < 1000) my.by <- 50
				if (tmp.diff >= 1000 & tmp.diff < 5000) my.by <- 250
				for (i in seq(ss.axis.range[1], ss.axis.range[2], by=my.by)) {
					grid.lines(c(0.5, 0.6), i, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
					grid.text(x=0.45, y=i, formatC(i, digits=0, format="f"), gp=gpar(col=format.colors.font, cex=0.8), just="right", default.units="native")
				}
				grid.text(x=unit(0.15, "native"), y=0.5, "Scale Score", gp=gpar(col=format.colors.font, cex=1.0), rot=90)
			}

			grid.lines(0.95, ach.per.axis.range, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")

			for (i in 1:length(ach.per.axis.range)) {
				grid.lines(c(0.95, 1.05), ach.per.axis.range[i], gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
				grid.text(x=1.15, y=ach.per.axis.range[i], ach.per.axis.labels[i], gp=gpar(col=format.colors.font, cex=0.65), just="right", default.units="native")
			}

			setkey(growthAchievementPlot.data, YEAR, GRADE)
			grid.text(x=unit(0.8, "native"), y=unit(median(growthAchievementPlot.data[list(year, tmp.unique.grades.numeric[1])]$TRANSFORMED_SCALE_SCORE), "native"),
				paste(pretty_year(year), "Achievement Percentile"), gp=gpar(col=format.colors.font, cex=0.9), rot=90)

			popViewport() ## pop left.axis.vp


			##
			## Right Axis Viewport
			##

			pushViewport(right.axis.vp)

			if (is.null(College_Readiness_Cutscores)) {
				grid.lines(0.1, gp.axis.range, gp=gpar(lwd=1.5, col=format.colors.growth.trajectories), default.units="native")

				for (i in gaPlot.percentile_trajectories){
					grid.lines(c(-0.1, 0.1), smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]),
						gp=gpar(lwd=1.5, col=format.colors.growth.trajectories), default.units="native")
					grid.text(x=unit(-0.475, "native"), y=smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]), i,
						gp=gpar(col=getSGPColor(i, format), cex=0.8), just="left", default.units="native")
				}

				if (baseline) {
					grid.text(x=0.5, y=(gp.axis.range[1]+gp.axis.range[2])/2, "Baseline Referenced Percentile Growth Trajectory",
						gp=gpar(col=format.colors.growth.trajectories, cex=1.0), rot=90, default.units="native")
				}
				if (!is.null(equated)) {
					grid.text(x=0.5, y=(gp.axis.range[1]+gp.axis.range[2])/2, "Equated Percentile Growth Trajectory",
						gp=gpar(col=format.colors.growth.trajectories, cex=1.0), rot=90, default.units="native")
				}
				if (cohort) {
					grid.text(x=0.5, y=(gp.axis.range[1]+gp.axis.range[2])/2, "Percentile Growth Trajectory",
						gp=gpar(col=format.colors.growth.trajectories, cex=1.0), rot=90, default.units="native")
				}
			} else {
				tmp.cut <- College_Readiness_Cutscores
				low.cut <- min(gp.axis.range[1], temp_uncond_frame[,ncol(temp_uncond_frame)][1], smoothPercentileTrajectory_Functions[[as.character(head(gaPlot.percentile_trajectories, 1))]](gaPlot.grade_range[2]), na.rm=TRUE)
				high.cut <- max(gp.axis.range[1], rev(temp_uncond_frame[,ncol(temp_uncond_frame)])[1], smoothPercentileTrajectory_Functions[[as.character(tail(gaPlot.percentile_trajectories, 1))]](gaPlot.grade_range[2]), na.rm=TRUE)
				grid.polygon(x=c(0.05, 0.05, 0.35, 0.35), y=c(low.cut, tmp.cut, tmp.cut, low.cut), default.units="native",
					gp=gpar(col=format.colors.font, fill="red", lwd=1.5))
				grid.text(x=0.2, y=(low.cut+tmp.cut)/2, "Not College Ready", gp=gpar(col=format.colors.font, cex=0.5), rot=90, default.units="native")
				grid.polygon(x=c(0.05, 0.05, 0.35, 0.35), y=c(high.cut, tmp.cut, tmp.cut, high.cut), default.units="native",
					gp=gpar(col=format.colors.font, fill="green3", lwd=1.5))
				grid.text(x=0.2, y=(high.cut+tmp.cut)/2, "College Ready", gp=gpar(col=format.colors.font, cex=0.5), rot=90, default.units="native")

				for (i in gaPlot.percentile_trajectories){
					grid.lines(c(-0.15, 0.05), smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]),
						gp=gpar(lwd=1.5, col=format.colors.growth.trajectories), default.units="native")
					grid.text(x=unit(-0.5, "native"), y=smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]), i,
						gp=gpar(col=format.colors.growth.trajectories, cex=0.8), just="left", default.units="native")
				}

				grid.text(x=0.65, y=(low.cut+high.cut)/2, "Percentile Growth Trajectory to College Readiness",
					gp=gpar(col=format.colors.growth.trajectories, cex=1.0), rot=90, default.units="native")
			}
			popViewport() ## pop right.axis.vp


			##
			## Bottom Axis Viewport
			##

			pushViewport(bottom.axis.vp)

			grid.lines(gaPlot.grade_range, 0.8, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
			grade.label.size <- c(rep(1, 8), rep(0.9, 2), rep(0.8, 2), rep(0.7, 2))[length(tmp.unique.grades.numeric)]
			for (i in seq_along(tmp.unique.grades.numeric)){
				grid.lines(tmp.unique.grades.numeric[i], c(0.5, 0.8), gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
				if (tmp.unique.grades.character[i]=="EOCT") grade.label <- "EOCT" else grade.label <- paste("Grade", tmp.unique.grades.numeric[i])
				grid.text(x=tmp.unique.grades.numeric[i], y=0.25, grade.label, gp=gpar(col=format.colors.font, cex=grade.label.size), default.units="native")
			}

			if (display.content_areas) {
				for (i in seq_along(tmp.unique.grades.numeric)){
					grid.text(x=tmp.unique.grades.numeric[i], y=0.0, capwords(tmp.unique.content_areas[i]), gp=gpar(col=format.colors.font, cex=0.5), default.units="native")
				}
			}

			popViewport() ## pop bottom.axis.vp


			##
			## Top Viewport
			##

			pushViewport(title.vp)

			tmp.title <- paste0(state.name.label, ": ", pretty_year(year), " ", content_area.label)
			grid.roundrect(width=unit(0.95, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=format.colors.font, lwd=1.6))
			grid.text(x=0.5, y=0.675, tmp.title, gp=gpar(col=format.colors.font, cex=2.65-max(0, -30+nchar(tmp.title))*0.06), default.units="native")
			if (is.null(SGP::SGPstateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]])) {
				grid.text(x=0.5, y=0.275, "Norm & Criterion Referenced Growth & Achievement",
					gp=gpar(col=format.colors.font, cex=2.0), default.units="native")
			} else {
				grid.text(x=0.5, y=0.275, "Norm & Criterion Referenced Growth to College Readiness",
					gp=gpar(col=format.colors.font, cex=1.75), default.units="native")
			}

			popViewport() ## pop title.vp


			##
			## End Viewport Creation and provide completion message
			##

			popViewport()

			dev.off()
		} ### END Loop over output.format

		if (baseline) tmp.baseline.message <- "Baseline Referenced"
		if (!is.null(equated)) tmp.baseline.message <- "Equated Cohort Referenced"
		if (cohort) tmp.baseline.message <- "Cohort Referenced"
		if (gaPlot.start.points=="Achievement Level Cuts") {
			message(paste("\tStarted", year, state.name.label, tail(tmp2.dt[['CONTENT_AREA']], 1), "Grade", tail(tmp2.dt[['GRADE']], 1), "Level", tail(tmp2.dt[['LEVEL']], 1), tmp.baseline.message, "growthAchievementPlot:",  started.date))
			message(paste("\tFinished", year, state.name.label, tail(tmp2.dt[['CONTENT_AREA']], 1), "Grade", tail(tmp2.dt[['GRADE']], 1), "Level", tail(tmp2.dt[['LEVEL']], 1), tmp.baseline.message, "growthAchievementPlot:",  prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))
		}
		if (gaPlot.start.points=="Achievement Percentiles") {
			message(paste("\tStarted", year, state.name.label, tail(tmp2.dt[['CONTENT_AREA']], 1), "Grade", tail(tmp2.dt[['GRADE']], 1), "Percentile", as.integer(100*tmp2.dt[['LEVEL']]), tmp.baseline.message, "growthAchievementPlot:",  started.date))
			message(paste("\tFinished", year, state.name.label, tail(tmp2.dt[['CONTENT_AREA']], 1), "Grade", tail(tmp2.dt[['GRADE']], 1), "Percentile", as.integer(100*tmp2.dt[['LEVEL']]), tmp.baseline.message, "growthAchievementPlot:",  prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))
		}
		if (gaPlot.start.points=="Individual Student") {
			message(paste("\tStarted", year, state.name.label, tail(tmp2.dt[['CONTENT_AREA']], 1), "Grade", tail(tmp2.dt[['GRADE']], 1), "Student Number", tmp2.dt[['ID']][1], tmp.baseline.message, "growthAchievementPlot:",  started.date))
			message(paste("\tFinished", year, state.name.label, tail(tmp2.dt[['CONTENT_AREA']], 1), "Grade", tail(tmp2.dt[['GRADE']], 1), "Student Number", tmp2.dt[['ID']][1], tmp.baseline.message, "growthAchievementPlot:",  prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))
		}


	} ## End loop over starting scores or students (j)

	return("DONE")

} ## End growthAchievementPlot function
