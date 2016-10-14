`createLongCutscores` <-
function(state,
	content_area,
	add.GRADE_NUMERIC=FALSE,
	assessment.transition.type=NULL) {

	GRADE <- GRADE_NUMERIC <- CUTSCORES <- CUTSCORES_TRANSFORMED <- YEAR <- CUTLEVEL <- YEAR_LAG <- CONTENT_AREA <- SCALE_SCORE <- NULL

	### Create relevant variables

	content_area.argument <- content_area
	if (!is.null(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])) {
		content_area <- unique(names(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])[
			SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]]==content_area])
	}


	### Utility functions

	get.long.cutscores <- function(content_area) {

		cutscore.list <- list()
		for (i in sort(unlist(lapply(content_area, function(x) which(x==sapply(strsplit(names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]]), '[.]'), '[', 1)))))) {
			cutscores.content_area <- unlist(strsplit(names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], '[.]'))[1]
			grades <- as.character(matrix(unlist(strsplit(names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]), "_")), ncol=2, byrow=TRUE)[,2])
			cutscores.year <- as.character(unlist(strsplit(names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], "[.]"))[2])

			cutscores.iter <- seq(length(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]][[1]]))
			cutscores <- as.data.table(matrix(unlist(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]),
				ncol=length(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]][[1]]), byrow=TRUE))
			if (names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i] %in% names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])) {
				loss.hoss.label <- names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i]
			} else {
				tmp.names <- sort(c(names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i],
									names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])))
				tmp.idx <- match(names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], tmp.names)-1
				loss.hoss.label <- tmp.names[tmp.idx]
			}
			loss <- sapply(SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[loss.hoss.label]][
					grep("loss.hoss", names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[loss.hoss.label]]))], '[', 1)
			loss <- as.numeric(loss[sapply(strsplit(names(loss), "_"), '[', 2) %in% grades])
			hoss <- sapply(SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[loss.hoss.label]][
					grep("loss.hoss", names(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[loss.hoss.label]]))], '[', 2)
			hoss <- as.numeric(hoss[sapply(strsplit(names(hoss), "_"), '[', 2) %in% grades])

			for (j in cutscores.iter) {
				cutscore.list[[paste(i, j, sep="_")]] <- data.table(
					GRADE=grades,
					CONTENT_AREA=cutscores.content_area,
					CUTLEVEL=as.character(j),
					CUTSCORES=cutscores[[j]],
					CUTSCORES_TRANSFORMED=100*cutscores.iter[j],
					YEAR=cutscores.year)
				cutscore.list[[paste(i, j, sep="_")]] <-
					cutscore.list[[paste(i, j, sep="_")]][
						GRADE %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[cutscores.content_area]]]
			}

			### Add LOSS/HOSS

			cutscore.list[[paste(i, "LOSS", sep="_")]] <- data.table(
				GRADE=grades,
				CONTENT_AREA=cutscores.content_area,
				CUTLEVEL="LOSS",
				CUTSCORES=loss,
				CUTSCORES_TRANSFORMED=0,
				YEAR=cutscores.year)
			cutscore.list[[paste(i, "LOSS", sep="_")]] <-
				cutscore.list[[paste(i, "LOSS", sep="_")]][
					GRADE %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[cutscores.content_area]]]

			cutscore.list[[paste(i, "HOSS", sep="_")]] <- data.table(
				GRADE=grades,
				CONTENT_AREA=cutscores.content_area,
				CUTLEVEL="HOSS",
				CUTSCORES=hoss,
				CUTSCORES_TRANSFORMED=100*(tail(cutscores.iter, 1)+1),
				YEAR=cutscores.year)
			cutscore.list[[paste(i, "HOSS", sep="_")]] <-
				cutscore.list[[paste(i, "HOSS", sep="_")]][
					GRADE %in% SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[cutscores.content_area]]]
		} ### END for (i in ...)

		### Add GRADE_LOWER/GRADE_UPPER

		long.cutscores <- rbindlist(cutscore.list)
		extension.cutscores <-
			data.table(
				CONTENT_AREA="PLACEHOLDER",
				GRADE=c("GRADE_LOWER", "GRADE_UPPER"),
				long.cutscores[,list(CUTSCORES=extendrange(CUTSCORES, f=0.15),
									CUTSCORES_TRANSFORMED=extendrange(CUTSCORES_TRANSFORMED, f=0.15)), by=list(YEAR, CUTLEVEL)])

		long.cutscores <- rbindlist(list(long.cutscores, extension.cutscores), fill=TRUE)
		setkey(long.cutscores, GRADE, CONTENT_AREA)

		### Trim cutscores if SGPstateData indicates `Earliest_Year_Reported`

		if (length(sort(long.cutscores[['YEAR']])) > 0 & !is.null(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[cutscores.content_area]])) {
			long.cutscores <- long.cutscores[YEAR >= SGP::SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[cutscores.content_area]]]
		}

		return(data.table(long.cutscores, key=c("GRADE", "CONTENT_AREA")))
	} ### END get.long.cutscores


	### Create LONG cutscores

	long.cutscores <- get.long.cutscores(content_area)


	### Add GRADE_NUMERIC

	if (add.GRADE_NUMERIC) {
		if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area.argument]])) {
			grades.content_areas.reported.in.state <- data.table(
					GRADE=as.character(SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[content_area.argument]]),
					YEAR_LAG=c(1, SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[content_area.argument]]),
					CONTENT_AREA=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[content_area.argument]])
		} else {
			grades.content_areas.reported.in.state <- data.table(
					GRADE=as.numeric(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.argument]]),
					YEAR_LAG=c(1, diff(as.numeric(SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area.argument]]))),
					CONTENT_AREA=content_area.argument)
		}
		grades.content_areas.reported.in.state[,GRADE_NUMERIC:=
			as.numeric(as.character((as.numeric(grades.content_areas.reported.in.state$GRADE[2])-1)+c(0, cumsum(tail(grades.content_areas.reported.in.state$YEAR_LAG, -1)))))]
		grades.content_areas.reported.in.state[,GRADE:=as.character(grades.content_areas.reported.in.state$GRADE)]
		setkeyv(grades.content_areas.reported.in.state, c("GRADE", "CONTENT_AREA"))
		long.cutscores <- grades.content_areas.reported.in.state[long.cutscores]
		long.cutscores[,YEAR_LAG:=NULL]
	}

	return(unique(data.table(long.cutscores, key=c("CONTENT_AREA", "YEAR", "GRADE", "CUTLEVEL")), by=c("CONTENT_AREA", "YEAR", "GRADE", "CUTLEVEL")))
} ## END createLongCutscores Function
