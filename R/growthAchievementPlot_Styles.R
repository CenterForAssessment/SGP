`growthAchievementPlot_Styles` <- 
   function(
	gaPlot.sgp_object,
	gaPlot.students,
	gaPlot.percentile_trajectories,
	gaPlot.achievement_percentiles,
	gaPlot.show.scale.transformations,
	gaPlot.grade_range,
	gaPlot.max.order.for.progression,
	state,
	content_area,
	year, 
	format,
	baseline, 
	pdf.folder,
	assessment.name) { 

	capwords <- function(x) {
		special.words <- c("ELA", "EMH", "II", "III", "IV")
		if (x %in% special.words) return(x)
		s <- sub("_", " ", x)
		s <- strsplit(s, split=" ")[[1]]
		s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
		s <- strsplit(s, split="-")[[1]]
		s <- paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
		s <- strsplit(s, split="'")[[1]]
		paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="'")
	} ### END capwords

	if (year %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]) {

		message(paste("\tNOTE: Based upon state scale changes in ", capwords(year), ". student growth projections are not possible. No ", 
			capwords(year), " ", content_area, " growth and achievement plot will be generated.\n", sep=""))

	} else {

		setkeyv(gaPlot.sgp_object@Data, c("VALID_CASE", "CONTENT_AREA"))

		growthAchievementPlot(
			gaPlot.sgp_object=gaPlot.sgp_object,
			gaPlot.students=gaPlot.students,
			gaPlot.max.order.for.progression=gaPlot.max.order.for.progression,
			state=state,
			content_area=content_area,
			year=year, 
			format=format,
			baseline=baseline,
			pdf.folder=pdf.folder)
	}

} ## End growthAchievementPlot_Styles function
