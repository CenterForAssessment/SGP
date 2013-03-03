`abcSGP` <- 
function(sgp_object,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
	years=NULL,
	content_areas=NULL,
	grades=NULL,
	prepareSGP.var.names=NULL,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	sgp.use.my.coefficient.matrices=NULL,
	simulate.sgps=TRUE,
	calculate.simex=NULL,
	parallel.config=NULL,
	save.intermediate.results=FALSE,
	save.old.summaries=FALSE,
	sgPlot.demo.report=FALSE,
	sgp.summaries=NULL,
	summary.groups=NULL,
	data_supplementary=NULL,
	confidence.interval.groups=NULL,
	plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
	verbose.output=FALSE) {

        started.at <- proc.time()
	message(paste("\nStarted abcSGP", date()), "\n")

	names.type <- names.provided <- names.output <- NULL

	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- getStateAbbreviation(tmp.name, "abcSGP")
	}


	### prepareSGP ###

	if ("prepareSGP" %in% steps) {
		sgp_object <- prepareSGP(sgp_object, data_supplementary=data_supplementary, state=state, var.names=prepareSGP.var.names)
	        if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### analyzeSGP ###

	if ("analyzeSGP" %in% steps) {

        ### Check for consistency between simulate.sgps and existence of CSEMs ###

		if (simulate.sgps & is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
        	        message("\tCSEMs are required in SGPstateData to simulate SGPs for confidence interval calculations. Confidence intervals will not be calculated.")
			simulate.sgps <- FALSE
		}

		sgp_object <- analyzeSGP(
			sgp_object=sgp_object,
			state=state,
			content_areas=content_areas,
			years=years,
			grades=grades,
			sgp.percentiles=sgp.percentiles,
			sgp.projections=sgp.projections,
			sgp.projections.lagged=sgp.projections.lagged,
			sgp.percentiles.baseline=sgp.percentiles.baseline,
			sgp.projections.baseline=sgp.projections.baseline,
			sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
			sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
			simulate.sgps=simulate.sgps,
			calculate.simex=calculate.simex,
			parallel.config=parallel.config,
			verbose.output=verbose.output)

                if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### combineSGP ###

	if ("combineSGP" %in% steps) {
		sgp_object <- combineSGP(
			sgp_object=sgp_object,
			state=state,
			years=years,
			content_areas=content_areas,
			sgp.percentiles=sgp.percentiles,
			sgp.projections.lagged=sgp.projections.lagged,
			sgp.projections.lagged.baseline=sgp.projections.lagged.baseline)

                if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### summarizeSGP ###

	if ("summarizeSGP" %in% steps) {
		sgp_object <- summarizeSGP(
			sgp_object=sgp_object,
			state=state,
			years=years, 
			content_areas=content_areas, 
			sgp.summaries=sgp.summaries, 
			summary.groups=summary.groups, 
			confidence.interval.groups=confidence.interval.groups,
			parallel.config=parallel.config,
			save.old.summaries=save.old.summaries)

                if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### visualizeSGP ###

	if ("visualizeSGP" %in% steps) {

		visualizeSGP(
			sgp_object=sgp_object,
			plot.types=plot.types,
			state=state,
			bPlot.years=years,
			sgPlot.years=years,
			sgPlot.demo.report=sgPlot.demo.report,
			gaPlot.years=years,
			bPlot.content_areas=content_areas,
			gaPlot.content_areas=content_areas,
			parallel.config=parallel.config)
	}


	### outputSGP ###

	if ("outputSGP" %in% steps) {
		outputSGP(
			sgp_object=sgp_object,
			state=state,
			outputSGP_SUMMARY.years=years,
			outputSGP_SUMMARY.content_areas=content_areas,
			outputSGP_INDIVIDUAL.years=years,
			outputSGP_INDIVIDUAL.content_areas=content_areas,
			outputSGP.student.groups=intersect(names(sgp_object@Data), subset(sgp_object@Names, names.type=="demographic" & names.output==TRUE, select=names.provided, drop=TRUE)))
	}


	### Print finish and return SGP object

        message(paste("Finished abcSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END abcSGP Function
