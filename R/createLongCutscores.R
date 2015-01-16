`createLongCutscores` <-
function(state,
	content_area,
	add.GRADE_NUMERIC=FALSE
   ) {

	GRADE <- GRADE_NUMERIC <- CUTSCORES <- YEAR <- CUTLEVEL <- YEAR_LAG <- NULL

	content_area.argument <- content_area
	if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])) {
		content_area <- unique(names(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])[
			SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]]==content_area])
	}

	if (!any(content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]))) {
		tmp.list <- list()
		for (content_area.iter in content_area) {
			for (i in grep(content_area.iter, sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]]), '[.]'), '[', 1))) {
				tmp.content_area <- unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], '[.]'))[1]
				tmp.grades <- as.character(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]), "_")), ncol=2, byrow=TRUE)[,2])

				if (names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i] %in% names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])) {
					tmp.loss.hoss.label <- names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i]
				} else {
					tmp.loss.hoss.label <- tmp.content_area
				}
				tmp.loss <- sapply(SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[tmp.loss.hoss.label]][
							grep("loss.hoss", names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[tmp.loss.hoss.label]]))], '[', 1)
				tmp.loss <- as.numeric(tmp.loss[sapply(strsplit(names(tmp.loss), "_"), '[', 2) %in% tmp.grades])
				tmp.hoss <- sapply(SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[tmp.loss.hoss.label]][
							grep("loss.hoss", names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[tmp.loss.hoss.label]]))], '[', 2)
				tmp.hoss <- as.numeric(tmp.hoss[sapply(strsplit(names(tmp.hoss), "_"), '[', 2) %in% tmp.grades])
				tmp.cutscores <- matrix(unlist(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]),
					ncol=length(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]][[1]]), byrow=TRUE)
				tmp.year <- as.character(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], "[.]"))[2])

				for (j in seq(length(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]][[1]]))) {
					tmp.list[[paste(i, j, sep="_")]] <- data.table(
						GRADE=tmp.grades,
						CONTENT_AREA=tmp.content_area,
						CUTLEVEL=as.character(j),
						CUTSCORES=tmp.cutscores[,j],
						YEAR=tmp.year)
					tmp.list[[paste(i, j, sep="_")]] <- subset(tmp.list[[paste(i, j, sep="_")]],
						GRADE %in% SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[tmp.content_area]])
				}

				tmp.list[[paste(i, "LOSS", sep="_")]] <- data.table(
					GRADE=tmp.grades,
					CONTENT_AREA=tmp.content_area,
					CUTLEVEL="LOSS",
					CUTSCORES=tmp.loss,
					YEAR=tmp.year)
				tmp.list[[paste(i, "LOSS", sep="_")]] <- subset(tmp.list[[paste(i, "LOSS", sep="_")]],
					GRADE %in% SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[tmp.content_area]])
				
				tmp.list[[paste(i, "HOSS", sep="_")]] <- data.table(
					GRADE=tmp.grades,
					CONTENT_AREA=tmp.content_area,
					CUTLEVEL="HOSS",
					CUTSCORES=tmp.hoss,
					YEAR=tmp.year)
				tmp.list[[paste(i, "HOSS", sep="_")]] <- subset(tmp.list[[paste(i, "HOSS", sep="_")]],
					GRADE %in% SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[tmp.content_area]])
			}
		}
	} else {
		tmp.list <- list()
		if(!all(content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]))) {
			stop("Not all content areas have Transformed Achievement Level Cutscores available in SGPstateData.
				Please augment the SGPstateData set with your data or contact the SGP package maintainer to have your data added to the SGP package.")
		}
		for (content_area.iter in content_area) {
			for (i in grep(content_area.iter, sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]]), '[.]'), '[', 1))) {
				tmp.content_area <- unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], '[.]'))[1]
				tmp.grades <- as.character(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]), "_")), ncol=2, byrow=TRUE)[,2])
				tmp.year <- as.character(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], "[.]"))[2])

				for (j in seq(length(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area.iter]])-1)) {
					tmp.list[[paste(i, j, sep="_")]] <- data.table(
						GRADE=tmp.grades,
						CONTENT_AREA=tmp.content_area,
						CUTLEVEL=as.character(j),
						CUTSCORES=SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area.iter]][j+1], 
						YEAR=tmp.year)
	
					tmp.list[[paste(i, j, sep="_")]] <- subset(tmp.list[[paste(i, j, sep="_")]], GRADE %in% SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.iter]])
				}
				tmp.list[[paste(i, "LOSS", sep="_")]] <- data.table(
					GRADE=tmp.grades,
					CONTENT_AREA=tmp.content_area,
					CUTLEVEL="LOSS",
					CUTSCORES=SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area.iter]][1],
					YEAR=tmp.year)
				tmp.list[[paste(i, "LOSS", sep="_")]] <- subset(tmp.list[[paste(i, "LOSS", sep="_")]], GRADE %in% SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.iter]])
						
				tmp.list[[paste(i, "HOSS", sep="_")]] <- data.table(
					GRADE=tmp.grades,
					CONTENT_AREA=tmp.content_area,
					CUTLEVEL="HOSS",
					CUTSCORES=tail(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area.iter]], 1),
					YEAR=tmp.year)
				tmp.list[[paste(i, "HOSS", sep="_")]] <- subset(tmp.list[[paste(i, "HOSS", sep="_")]], GRADE %in% SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.iter]])
			}
		}
	}

	tmp.long.cutscores <- data.table(rbindlist(tmp.list), key=c("YEAR", "CUTLEVEL"))
	tmp.extension <- data.table(CONTENT_AREA="PLACEHOLDER", GRADE=c("GRADE_LOWER", "GRADE_UPPER"), tmp.long.cutscores[,extendrange(CUTSCORES, f=0.15), by=list(YEAR, CUTLEVEL)])
	setnames(tmp.extension, "V1", "CUTSCORES")
	tmp.long.cutscores <- rbind(tmp.long.cutscores, setcolorder(tmp.extension,names(tmp.long.cutscores)))
	setkeyv(tmp.long.cutscores, c("GRADE", "CONTENT_AREA"))

	if (length(sort(tmp.long.cutscores$YEAR)) > 0 & !is.null(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[content_area]])) {
		tmp.long.cutscores <- subset(tmp.long.cutscores, as.numeric(unlist(sapply(strsplit(as.character(tmp.long.cutscores$YEAR), "_"), function(x) x[1]))) >= 
			as.numeric(sapply(strsplit(as.character(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[content_area]]), "_"), function(x) x[1])))
	}

	if (add.GRADE_NUMERIC) {
		if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area.argument]])) {
			grades.content_areas.reported.in.state <- data.table(
					GRADE=as.character(SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[content_area.argument]]),
					YEAR_LAG=c(1, SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[content_area.argument]]),
					CONTENT_AREA=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area.argument]],
					key=c("GRADE", "CONTENT_AREA")
					)
		} else {
			grades.content_areas.reported.in.state <- data.table(
					GRADE=as.numeric(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.argument]]),
					YEAR_LAG=c(1, diff(as.numeric(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.argument]]))),
					CONTENT_AREA=content_area.argument,
					key=c("GRADE", "CONTENT_AREA")
					)
		}
		grades.content_areas.reported.in.state[,GRADE_NUMERIC:=
			(as.numeric(grades.content_areas.reported.in.state$GRADE[2])-1)+c(0, cumsum(tail(grades.content_areas.reported.in.state$YEAR_LAG, -1)))]
		grades.content_areas.reported.in.state[,GRADE:=as.character(grades.content_areas.reported.in.state$GRADE)]
		setkeyv(grades.content_areas.reported.in.state, c("GRADE", "CONTENT_AREA"))
		tmp.long.cutscores <- grades.content_areas.reported.in.state[tmp.long.cutscores]
		tmp.long.cutscores[,YEAR_LAG:=NULL]
	}

	return(tmp.long.cutscores)
} ## END createLongCutscores Function
