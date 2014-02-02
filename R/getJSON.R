`getJSON` <-
function(tmp.data,
	state,
	content_area,
	data.type)

	### Utility functions

	interpolate.grades <- function(grades, content_areas, years, data.year.span, grades.content_areas.reported.in.state) {

		last.number <- function (x) {
			if (sum(!is.na(x)) > 0) return(max(which(!is.na(x)))) else return (0)
		}

		first.number <- function (x) {
			if (sum(!is.na(x)) > 0 ) return(min(which(!is.na(x)))) else return (0)
		}

		convert.grades <- function(grades, content_areas, to="GRADE_NUMERIC", lookup=grades.content_areas.reported.in.state) {
			if (to=="GRADE_NUMERIC") {
				return(as.numeric(lookup$GRADE_NUMERIC[match(paste(grades, content_areas), paste(lookup$GRADE, lookup$CONTENT_AREA))]))
			}
			if (to=="GRADE") {
				return(as.character(lookup$GRADE[match(grades, lookup$GRADE_NUMERIC)]))
			}
		}

		first.scale.score <- first.number(head(grades, data.year.span-1))
		last.scale.score <- last.number(grades)
		tmp.grades <- grades[seq(first.scale.score, last.scale.score)]
		tmp.content_areas <- content_areas[seq(first.scale.score, last.scale.score)]
		tmp.years <- years[seq(first.scale.score, last.scale.score)]
		tmp.grades <- convert.grades(tmp.grades, tmp.content_areas)

		tmp.index <- match(tmp.grades[1], grades.content_areas.reported.in.state$GRADE_NUMERIC)

		if (tmp.index < dim(grades.content_areas.reported.in.state)[1]) {
			tmp.additional <- seq(tmp.index+1, dim(grades.content_areas.reported.in.state)[1])
			tmp.grades <- c(rev(grades.content_areas.reported.in.state$GRADE_NUMERIC[tmp.additional]), tmp.grades)
			tmp.content_areas <- c(rev(grades.content_areas.reported.in.state$CONTENT_AREA[tmp.additional]), tmp.content_areas)
			tmp.years <- rev(year.function(Report_Parameters$Current_Year, 1, 5, "character")
		}



	} ### END interpolate.grades

	
	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area]])) {
		grades.content_areas.reported.in.state <- data.frame(
					GRADE=SGPstateData[[Report_Parameters$State]][["SGP_Configuration"]][["grade.projection.sequence"]][[Report_Parameters$Content_Area]],
					YEAR_LAG=c(1, SGPstateData[[Report_Parameters$State]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[Report_Parameters$Content_Area]]),
					CONTENT_AREA=SGPstateData[[Report_Parameters$State]][["SGP_Configuration"]][["content_area.projection.sequence"]][[Report_Parameters$Content_Area]], 
					stringsAsFactors=FALSE
					)
	} else {
		grades.content_areas.reported.in.state <- data.frame(
				GRADE=SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Grades_Reported"]][[content_area]],
				YEAR_LAG=c(1, diff(as.numeric(SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Grades_Reported"]][[Report_Parameters$Content_Area]]))),
				CONTENT_AREA=Report_Parameters$Content_Area, 
				stringsAsFactors=FALSE
				)
	}
	grades.content_areas.reported.in.state$GRADE_NUMERIC <- as.numeric(grades.content_areas.reported.in.state$GRADE[1])+c(0, cumsum(tail(grades.content_areas.reported.in.state$YEAR_LAG, -1)))
	grades.content_areas.reported.in.state$GRADE_NUMERIC <- (as.numeric(grades.content_areas.reported.in.state$GRADE[2])-1)+c(0, cumsum(tail(grades.content_areas.reported.in.state$YEAR_LAG, -1)))


	### Create lists to be converted to JSON

	if (data.type=="studentGrowthPlot") {

		tmp.list <- list(
			Scale_Scores=as.numeric(subset(tmp.data, select=paste("SCALE_SCORE", rev(sgPlot.years), sep="."))),
			Plotting_Scale_Scores=as.numeric(subset(tmp.data, select=paste("TRANSFORMED_SCALE_SCORE", rev(sgPlot.years), sep="."))),
			Achievement_Levels=as.character(unlist(subset(tmp.data, select=paste("ACHIEVEMENT_LEVEL", rev(sgPlot.years), sep=".")))),
			SGP=as.numeric(subset(tmp.data, select=paste(my.sgp, rev(sgPlot.years), sep="."))),
			SGP_Levels=as.character(unlist(subset(tmp.data, select=paste(my.sgp.level, rev(sgPlot.years), sep=".")))),
			Grades=as.character(subset(tmp.data, select=paste("GRADE", rev(sgPlot.years), sep="."))),
			Content_Areas=as.character(subset(tmp.data, select=paste("CONTENT_AREA_LABELS", rev(sgPlot.years), sep="."))),
			Cuts=list(NY1=as.numeric(subset(tmp.data, select=intersect(grep(trajectory.cuts, names(tmp.data)), grep("YEAR_1", names(tmp.data))))),
				NY2=as.numeric(subset(tmp.data, select=intersect(grep(trajectory.cuts, names(tmp.data)), grep("YEAR_2", names(tmp.data))))),
				NY3=as.numeric(subset(tmp.data, select=intersect(grep(trajectory.cuts, names(tmp.data)), grep("YEAR_3", names(tmp.data)))))),
			SGP_Targets=list(CUKU=tmp.data[[paste(paste(my.sgp.target.label[1], my.sgp.target.label[2], sep="_"), last.year, sep=".")]], 
				CUKU_Current=tmp.data[[paste(paste(my.sgp.target.label[1], my.sgp.target.label[2], "CURRENT", sep="_"), last.year, sep=".")]], 
				MUSU=tmp.data[[paste(paste(my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], sep="_"), last.year, sep=".")]], 
				MUSU_Current=tmp.data[[paste(paste(my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "CURRENT", sep="_"), last.year, sep=".")]]),
			SGP_Scale_Score_Targets=list(CUKU=list(NY1=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1", sep="_")]]),
				NY2=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2", sep="_")]]),
				NY3=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1", sep="_")]])),
				MUSU=list(NY1=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1", sep="_")]]),
					NY2=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2", sep="_")]]),
					NY3=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3", sep="_")]])),
				CUKU_Current=list(NY1=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT", sep="_")]]),
					NY2=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT", sep="_")]]),
					NY3=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT", sep="_")]])),
				MUSU_Current=list(NY1=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT", sep="_")]]),
					NY2=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT", sep="_")]]),
					NY3=as.numeric(tmp.data[[paste('SCALE_SCORE', my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT", sep="_")]]))),
			Cutscores=Cutscores[[content_areas[vp]]],
			Report_Parameters=list(Current_Year=last.year, Content_Area=content_areas[vp], Content_Area_Title=tmp.data[[paste("CONTENT_AREA_LABELS", last.year, sep=".")]], 
				State=state))
} ### END getJSON
