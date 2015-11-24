`growthAchievementPlot` <-
	function(
	gaPlot.sgp_object,
	gaPlot.students,
	gaPlot.percentile_trajectories,
	gaPlot.achievement_percentiles=c(.01, seq(.05, .95, by=.05), .99),
	gaPlot.show.scale.transformations=TRUE,
	gaPlot.grade_range,
	gaPlot.max.order.for.progression=NULL,
	gaPlot.start.points="Achievement Level Cuts",
	gaPlot.subtitle=TRUE,
	state,
	content_area,
	year,
	format="print",
	baseline=FALSE,
	equated=NULL,
	output.format="PDF",
	output.folder,
	assessment.name) {

	CUTLEVEL <- GRADE <- YEAR <- ID <- SCALE_SCORE <- level_1_curve <- V1 <- TRANSFORMED_SCALE_SCORE <- PERCENTILE <- GRADE_NUMERIC <- CONTENT_AREA <- NULL ## To prevent R CMD check warnings

	content_area <- toupper(content_area)
	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]])) {
		content_area.all <- unique(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area]])
	} else {
		content_area.all <- content_area
	}
	number.achievement.level.regions <- length(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])

	cohort <- TRUE
	if (baseline) {cohort <- FALSE; equated <- NULL}
	if (!is.null(equated)) cohort <- baseline <- FALSE


	## State stuff

	if (state %in% c(datasets::state.abb, "DEMO")) {
		state.name.label <- c(datasets::state.name, "Demonstration")[state==c(datasets::state.abb, "DEMO")]
	} else {
		state.name.label <- test.abbreviation.label <- state
	}
	state.name.file.label <- gsub(" ", "_", state.name.label)

	### Test if scale change has occured in the requested year

	if (is.null(equated) && year %in% SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]) {
		message(paste("\tNOTE: Based upon state scale changes in ", capwords(year), ". student growth projections are not possible. No ",
			capwords(year), " ", content_area, " growth and achievement plot will be generated.\n", sep=""))
		return("DONE")
    }

	### Create folder for plots

	dir.create(output.folder, recursive=TRUE, showWarnings=FALSE)

	### Create LONG cutscores

	long_cutscores <- createLongCutscores(state, content_area, add.GRADE_NUMERIC=TRUE)

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
	tmp.unique.grades.character <- unique(long_cutscores[!is.na(GRADE_NUMERIC)])[['GRADE']]
	setkeyv(gaPlot.sgp_object@Data, c("VALID_CASE", "CONTENT_AREA"))
	growthAchievementPlot.data <- gaPlot.sgp_object@Data[CJ("VALID_CASE", content_area.all)][, list(ID, CONTENT_AREA, YEAR, GRADE, SCALE_SCORE)][
		GRADE %in% tmp.unique.grades.character & !is.na(SCALE_SCORE)]

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

	temp_cutscores <- long_cutscores[GRADE %in% tmp.unique.grades.character & !CUTLEVEL %in% c("LOSS", "HOSS") & YEAR %in% tail(sort(unique(long_cutscores$YEAR), na.last=FALSE), 1)][,CUTLEVEL:=as.numeric(CUTLEVEL)]
	setkeyv(temp_cutscores, c("GRADE_NUMERIC", "CONTENT_AREA"))


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

	gaPlot.percentile_trajectories_Internal <- function(tmp.df, percentile, content_area, year, state) {

		.create.path <- function(labels, pieces=c("my.subject", "my.year", "my.extra.label")) {
			sub(' ', '_', toupper(sub('\\.+$', '', paste(unlist(sapply(labels[pieces], as.character)), collapse="."))))
		}

		gaPlot.sgp_object@SGP$Panel_Data <- tmp.df
		gaPlot.sgp_object@SGP$SGProjections <- NULL
		tmp.grades <- as.numeric(tmp.df[1,2:((dim(tmp.df)[2]+1)/2)])
		if (baseline) my.extra.label <- "BASELINE"
		if (!is.null(equated)) my.extra.label <- "EQUATED"
		if (cohort) my.extra.label <- NULL


		studentGrowthProjections(
			panel.data=gaPlot.sgp_object@SGP,
			sgp.labels=list(my.year=year, my.subject=content_area, my.extra.label=my.extra.label),
			projcuts.digits=2,
			projection.unit="GRADE",
			percentile.trajectory.values=percentile,
			grade.progression=tmp.grades,
			grade.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[content_area]],
			content_area.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area]],
			year_lags.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[content_area]],
			max.order.for.progression=my_min(c(gaPlot.max.order.for.progression, getMaxOrderForProgression(year, content_area, state, equated))),
			sgp.projections.equated=equated,
			print.time.taken=FALSE)[["SGProjections"]][[.create.path(list(my.subject=content_area, my.year=year, my.extra.label=my.extra.label))]][,-1, with=FALSE]
	}

	smoothPercentileTrajectory <- function(tmp.df, percentile, content_area, year, state) {
		tmp.trajectories <- gaPlot.percentile_trajectories_Internal(tmp.df, percentile, content_area, year, state)
		trajectories <- c(tail(as.numeric(tmp.df), (dim(tmp.df)[2]-1)/2), as.numeric(tmp.trajectories))
		grade.sequence <- c(tmp.df[1,2:((dim(tmp.df)[2]+1)/2)], head(setdiff(tmp.unique.grades.numeric, tmp.df[1,2:((dim(tmp.df)[2]+1)/2)]), length(tmp.trajectories)))

		if (content_area %in% names(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]])) {
			tmp.spline.fun <- splinefun(grade.sequence, trajectories)
			tmp.function <- function(grades) {
				sapply(grades, function(x) piecewiseTransform(tmp.spline.fun(x), state, as.character(content_area), as.character(year), as.character(x)))
			}
			return(splinefun(grade.sequence, tmp.function(grade.sequence)))
		} else {
			return(splinefun(grade.sequence, trajectories))
		}
	}


	### Calculate Scale Transformations (if required)

	setkey(growthAchievementPlot.data, GRADE)
		growthAchievementPlot.data[, TRANSFORMED_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, content_area, YEAR, GRADE), by=list(CONTENT_AREA, YEAR, GRADE)]
	if (content_area %in% names(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores_gaPlot"]])) {
		gaPlot.show.scale.transformations <- FALSE
	}

	### Calculate ACHIEVEMENT percentiles

	if (!is.null(gaPlot.achievement_percentiles)) {

		## Creating the points used by lines to construct unconditional percentile curves

		setkey(growthAchievementPlot.data, GRADE, CONTENT_AREA)
		setkey(long_cutscores, GRADE, CONTENT_AREA)
		growthAchievementPlot.data <- unique(long_cutscores[!is.na(GRADE_NUMERIC),c("GRADE", "CONTENT_AREA", "GRADE_NUMERIC"), with=FALSE])[growthAchievementPlot.data]
		setnames(growthAchievementPlot.data, c("GRADE", "GRADE_NUMERIC"), c("GRADE_CHARACTER", "GRADE"))
		setkeyv(growthAchievementPlot.data, "YEAR")
		my.tmp <- growthAchievementPlot.data[list(year)][,quantile(TRANSFORMED_SCALE_SCORE, probs=gaPlot.achievement_percentiles, na.rm=TRUE), keyby=list(GRADE, CONTENT_AREA)][
			list(tmp.unique.grades.numeric)][,PERCENTILE:=rep(gaPlot.achievement_percentiles, length(tmp.unique.grades.numeric))]
		temp_uncond_frame <- matrix(my.tmp[,splinefun(GRADE, V1)(tmp.smooth.grades), by=PERCENTILE][['V1']], nrow=length(gaPlot.achievement_percentiles), byrow=TRUE)
		rownames(temp_uncond_frame) <- gaPlot.achievement_percentiles
		colnames(temp_uncond_frame) <- tmp.smooth.grades
	}


	################################################
	###
	### Code for producing the chart
	###
	################################################

	if (format=="print") {
		format.colors.background <- rgb(0.985, 0.985, 1.0)
		format.colors.region <- paste("grey", round(seq(62, 91, length=number.achievement.level.regions)), sep="")
		format.colors.font <- "grey20"
		format.colors.growth.trajectories <- "black"
	} else {
		format.colors.background <- rgb(0.48, 0.48, 0.52)
		format.colors.region <- c("#4D98C1", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7")[seq(number.achievement.level.regions)]
		format.colors.font <- rgb(0.985, 0.985, 1.0)
		format.colors.growth.trajectories <- "khaki1"
	}

	xscale.range <- c(gaPlot.grade_range[1]-0.5, gaPlot.grade_range[2]+0.5)


	## Create data sets to be used for plot production

	if (is.null(gaPlot.students)) {
		my.cutscore.label <- getMyLabel(state, content_area, as.character(year))
		start.cuts <- SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.cutscore.label]]
		if (gaPlot.start.points=="Achievement Level Cuts") {
			start.cuts.values <- start.cuts[[1]]
		}
		if (gaPlot.start.points=="Achievement Percentiles") {
			start.cuts.values <- as.numeric(sort(temp_uncond_frame[,1]))
		}
	 	tmp1.df <- data.frame(
			ID=seq_along(start.cuts.values),
			GRADE=as.numeric(as.character(tail(unlist(strsplit(names(start.cuts)[1], "_")), 1))),
			SCALE_SCORE=start.cuts.values)
	} else {
		setkey(growthAchievementPlot.data, ID)
		tmp1.df <- growthAchievementPlot.data[gaPlot.students]
	}

	## Start loop over students or starting scores

	for (j in unique(tmp1.df$ID)) {

		started.at <- proc.time()
		started.date <- date()

		tmp2.df <- subset(tmp1.df, ID==j)
		tmp.df <- data.frame(matrix(c(as.numeric(as.character(tmp2.df$ID[1])), tmp2.df$GRADE, tmp2.df$SCALE_SCORE), nrow=1))

	## Create Percentile Trajectory functions

	smoothPercentileTrajectory_Functions <- list()
	for (i in sort(gaPlot.percentile_trajectories)) {
		smoothPercentileTrajectory_Functions[[as.character(i)]] <- smoothPercentileTrajectory(tmp.df, i, content_area, year, state)
	}

	## Define axis ranges based (ranges contingent upon starting score)

	setkey(growthAchievementPlot.data, YEAR)
	gp.axis.range <- c(smoothPercentileTrajectory_Functions[[1]](gaPlot.grade_range[[2]]),
		smoothPercentileTrajectory_Functions[[length(gaPlot.percentile_trajectories)]](gaPlot.grade_range[[2]]))
	yscale.range <- c(min(gp.axis.range[1], quantile(growthAchievementPlot.data[list(year)]$TRANSFORMED_SCALE_SCORE, prob=.005, na.rm=TRUE)),
		max(gp.axis.range[2], quantile(growthAchievementPlot.data[list(year)]$TRANSFORMED_SCALE_SCORE, prob=.995, na.rm=TRUE)))
	ach.per.axis.range <- (temp_uncond_frame[,1])[temp_uncond_frame[,1] >= yscale.range[1] & temp_uncond_frame[,1] <= yscale.range[2]]
	ach.per.axis.labels <- formatC(100*as.numeric(rownames(temp_uncond_frame)[temp_uncond_frame[,1] >= yscale.range[1] & temp_uncond_frame[,1] <= yscale.range[2]]),
		digits=0, format="f")


##
## Create viewports
##

	if (is.null(SGP::SGPstateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]])) {
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
			tmp.file.name <- paste(output.folder, "/", state.name.file.label, my.label, capwords(content_area), "_", year, "_Level_", j, tmp.suffix, sep="")
		}
		if (gaPlot.start.points=="Achievement Percentiles") {
			tmp.file.name <- paste(output.folder, "/", state.name.file.label, my.label, capwords(content_area), "_", year, "_Percentile_", as.integer(100*gaPlot.achievement_percentiles[j]), tmp.suffix, sep="")
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

##
## Create spline functions to calculate boundary values for each cutlevel
##

	for (i in 1:max(temp_cutscores$CUTLEVEL)){
		assign(paste("level_", i, "_curve", sep=""), splinefun(tmp.unique.grades.numeric, subset(temp_cutscores, CUTLEVEL==i)$CUTSCORES))
	}


##
## Create variables for boundaries and plotting
##

	x.boundary.values.1 <- c(gaPlot.grade_range[1], seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), gaPlot.grade_range[2])
	if (max(temp_cutscores$CUTLEVEL) > 1) {
		for (i in 2:max(temp_cutscores$CUTLEVEL)){
			assign(paste("x.boundary.values.", i, sep=""), c(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), seq(gaPlot.grade_range[2], gaPlot.grade_range[1], length=40)))
		}
		assign(paste("x.boundary.values.", max(temp_cutscores$CUTLEVEL)+1, sep=""), c(gaPlot.grade_range[1], seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), gaPlot.grade_range[2]))
	}


	y.boundary.values.1 <- c(yscale.range[1], level_1_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40)), yscale.range[1])
	for (i in 2:max(temp_cutscores$CUTLEVEL)) {
	assign(paste("y.boundary.values.", i, sep=""), c(eval(parse(text=paste("level_", i-1, "_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40))", sep=""))),
							 eval(parse(text=paste("level_", i, "_curve(seq(gaPlot.grade_range[2], gaPlot.grade_range[1], length=40))", sep="")))))
	}
	assign(paste("y.boundary.values.", max(temp_cutscores$CUTLEVEL)+1, sep=""), c(yscale.range[2],
			eval(parse(text=paste("level_", max(temp_cutscores$CUTLEVEL) , "_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40))", sep=""))), yscale.range[2]))


##
## Create colored (grey-scale) regions
##

	for (i in 1:(1+max(temp_cutscores$CUTLEVEL))){
		grid.polygon(x=get(paste("x.boundary.values.", i, sep="")),
			y=get(paste("y.boundary.values.", i, sep="")),
			default.units="native",
			gp=gpar(fill=format.colors.region[i], lwd=0.1, col="grey85"))
	}

	## Code for producing the achievement percentile curves

	if (!is.null(gaPlot.achievement_percentiles)){

		for (i in rownames(temp_uncond_frame)) {
			grid.lines(tmp.smooth.grades, temp_uncond_frame[i,], gp=gpar(lwd=0.3, col="white"), default.units="native")
		}
	}


	## Code for producing percentile growth trajectories

	if (!is.null(gaPlot.percentile_trajectories)){

		for (i in gaPlot.percentile_trajectories) {
			grid.lines(tmp.smooth.grades, smoothPercentileTrajectory_Functions[[as.character(i)]](tmp.smooth.grades),
				gp=gpar(lwd=1.2, col=format.colors.growth.trajectories), default.units="native")
		}
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
			tmp.text <- paste("Student starting Grade ", gaPlot.grade_range[1], " from Level ", j, "/Level ", j+1, " cut", sep="")
		}
		if (gaPlot.start.points=="Achievement Percentiles") {
			tmp.text <- paste("Student starting Grade ", gaPlot.grade_range[1], " from ", toOrdinal(as.integer(100*gaPlot.achievement_percentiles[j])), " achievement percentile", sep="")
		}
		grid.text(x=0.5, y=0.05, tmp.text, gp=gpar(col="white", cex=1.2))

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

	setkey(growthAchievementPlot.data, GRADE)
	grid.text(x=unit(0.8, "native"), y=unit(median(growthAchievementPlot.data[list(tmp.unique.grades.numeric[1])]$TRANSFORMED_SCALE_SCORE), "native"),
		paste(pretty_year(year), "Achievement Percentile"), gp=gpar(col=format.colors.font, cex=0.9), rot=90)

	popViewport() ## pop left.axis.vp


##
## Right Axis Viewport
##

	pushViewport(right.axis.vp)

	if (is.null(SGP::SGPstateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]])) {
		grid.lines(0.1, gp.axis.range, gp=gpar(lwd=1.5, col=format.colors.growth.trajectories), default.units="native")

		for (i in gaPlot.percentile_trajectories){
			grid.lines(c(-0.1, 0.1), smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]),
				gp=gpar(lwd=1.5, col=format.colors.growth.trajectories), default.units="native")
			grid.text(x=unit(-0.475, "native"), y=smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]), i,
				gp=gpar(col=format.colors.growth.trajectories, cex=0.8), just="left", default.units="native")
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
		tmp.cut <- as.numeric(SGP::SGPstateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]][[content_area]])
		grid.polygon(x=c(0.05, 0.05, 0.35, 0.35), y=c(gp.axis.range[1], tmp.cut, tmp.cut, gp.axis.range[1]), default.units="native",
			gp=gpar(col=format.colors.font, fill="red", lwd=1.5))
		grid.text(x=0.2, y=(gp.axis.range[1]+tmp.cut)/2, "Not College Ready", gp=gpar(col=format.colors.font, cex=0.5), rot=90, default.units="native")
		grid.polygon(x=c(0.05, 0.05, 0.35, 0.35), y=c(gp.axis.range[2], tmp.cut, tmp.cut, gp.axis.range[2]), default.units="native",
			gp=gpar(col=format.colors.font, fill="green3", lwd=1.5))
		grid.text(x=0.2, y=(gp.axis.range[2]+tmp.cut)/2, "College Ready", gp=gpar(col=format.colors.font, cex=0.5), rot=90, default.units="native")

		for (i in gaPlot.percentile_trajectories){
			grid.lines(c(-0.15, 0.05), smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]),
				gp=gpar(lwd=1.5, col=format.colors.growth.trajectories), default.units="native")
			grid.text(x=unit(-0.5, "native"), y=smoothPercentileTrajectory_Functions[[as.character(i)]](gaPlot.grade_range[2]), i,
				gp=gpar(col=format.colors.growth.trajectories, cex=0.8), just="left", default.units="native")
		}

		grid.text(x=0.65, y=(gp.axis.range[1]+gp.axis.range[2])/2, "Percentile Growth Trajectory to College Readiness",
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
		if (tmp.unique.grades.character[i]=="EOCT") tmp.label <- "EOCT" else tmp.label <- paste("Grade", tmp.unique.grades.numeric[i])
		grid.text(x=tmp.unique.grades.numeric[i], y=0.25, tmp.label, gp=gpar(col=format.colors.font, cex=grade.label.size), default.units="native")
	}

	popViewport() ## pop bottom.axis.vp


##
## Top Viewport
##

	pushViewport(title.vp)

	grid.roundrect(width=unit(0.95, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=format.colors.font, lwd=1.6))
	grid.text(x=0.5, y=0.675, paste(state.name.label, ": ", pretty_year(year), " ", content_area.label, sep=""),
		gp=gpar(col=format.colors.font, cex=2.65), default.units="native")
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
	message(paste("\tStarted", year, state.name.label, content_area, tmp.baseline.message, "growthAchievementPlot:",  started.date))
	message(paste("\tFinished", year, state.name.label, content_area, tmp.baseline.message, "growthAchievementPlot:",  date(), "in", convertTime(timetaken(started.at)), "\n"))


	} ## End loop over starting scores or students (j)

	return("DONE")

} ## End growthAchievementPlot function
