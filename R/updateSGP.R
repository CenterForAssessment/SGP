`updateSGP` <- 
function(what_sgp_object=NULL,
	with_sgp_data_LONG=NULL,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
	years=NULL,
	content_areas=NULL,
	grades=NULL,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	simulate.sgps = FALSE,
	save.old.summaries=TRUE,
	save.intermediate.results=TRUE,
	calculate.simex = NULL,
	calculate.simex.baseline = NULL,
	sgp.use.my.coefficient.matrices=NULL,
	sgp.target.scale.scores=FALSE,
	overwrite.existing.data=FALSE,
	sgPlot.demo.report=TRUE,
	sgp.config=NULL,
	parallel.config=NULL,
	...) {

		started.at <- proc.time()
		message(paste("\nStarted updateSGP", date()), "\n")


	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(what_sgp_object))))
		state <- getStateAbbreviation(tmp.name, "updateSGP")
	}

	if (identical(calculate.simex, TRUE)) {
		##  Enforce that simex.use.my.coefficient.matrices must be TRUE for updating COHORT SIMEX SGPs
		calculate.simex <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=50, simex.sample.size=25000, extrapolation="linear", save.matrices=TRUE, simex.use.my.coefficient.matrices = TRUE)
	}

	if (identical(calculate.simex.baseline, TRUE)) {
		##  Enforce that simex.use.my.coefficient.matrices must be TRUE for updating BASELINE SIMEX SGPs
		calculate.simex.baseline <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=50, simex.sample.size=25000, extrapolation="linear", save.matrices=TRUE, simex.use.my.coefficient.matrices = TRUE)
	}

	### Utility functions

	"%w/o%" <- function(x,y) x[!x %in% y]
	
	### Argument checks

	if (is.null(what_sgp_object)) {
		stop("\tNOTE: Argument 'what_sgp_object' must be supplied to updateSGP (at a minimum). See man page for 'updateSGP' for details.")
	}

	if (is.null(with_sgp_data_LONG)) {
		sgp.use.my.coefficient.matrices <- TRUE
	}
	if (identical(sgp.use.my.coefficient.matrices, FALSE)) {
		sgp.use.my.coefficient.matrices <- NULL
	}

	matrix.names <- names(what_sgp_object@SGP[['Coefficient_Matrices']])


	##############################################################################
	### DOESN'T supply 'with_sgp_data_LONG'
	##############################################################################

	if (is.null(with_sgp_data_LONG)) {
		
		tmp.years <- tmp.content_areas.years <- list()

		if (is.null(content_areas)) {
			content_areas <- sort(unique(sapply(strsplit(matrix.names, "[.]"), '[', 1)))
		}

		for (i in content_areas) {
			tmp.content_area.matrix.names <- grep(i, matrix.names, value=TRUE)
			tmp.years[[i]] <- sort(unique(sapply(strsplit(tmp.content_area.matrix.names, "[.]"), '[', 2))) %w/o% "BASELINE"
		}

		if (!is.null(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- intersect(tmp.years[[i]], years)
			}
		}

		for (i in names(tmp.years)) {
			tmp.content_areas.years[[i]] <- paste(i, tmp.years[[i]], sep=".")
		}
		tmp.content_areas.years <- as.character(unlist(tmp.content_areas.years))

		### NULL out existing results to be re-calculated

		what_sgp_object@SGP[['Goodness_of_Fit']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['Goodness_of_Fit']]))] <- NULL
		what_sgp_object@SGP[['SGPercentiles']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['SGPercentiles']]))] <- NULL
		what_sgp_object@SGP[['SGProjections']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['SGProjections']]))] <- NULL
		what_sgp_object@SGP[['Simulated_SGPs']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['Simulated_SGPs']]))] <- NULL
		

		### Update results

		sgp_object <- abcSGP(
					sgp_object=what_sgp_object,
					state=state,
					steps=steps,
					years=years,
					content_areas=content_areas,
					grades=grades,
					sgp.percentiles=sgp.percentiles,
					sgp.projections=sgp.projections,
					sgp.projections.lagged=sgp.projections.lagged,
					sgp.percentiles.baseline=sgp.percentiles.baseline,
					sgp.projections.baseline=sgp.projections.baseline,
					sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
					sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
					sgp.target.scale.scores=sgp.target.scale.scores,
					save.intermediate.results=save.intermediate.results,
					calculate.simex = calculate.simex,
					calculate.simex.baseline=calculate.simex.baseline,
					simulate.sgps = simulate.sgps,
					sgp.config=sgp.config,
					parallel.config=parallel.config,
					...
					)

		### Print finish and return SGP object

		message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
		return(sgp_object)
	} ### END is.null(with_sgp_data_LONG)


	#############################################################################################
	### DOES supply 'with_sgp_data_LONG'
	#############################################################################################

	if (!is.null(with_sgp_data_LONG)) {

		HIGH_NEED_STATUS <- YEAR <- ID <- VALID_CASE <- CONTENT_AREA <- NULL
		tmp_sgp_object <- prepareSGP(with_sgp_data_LONG, state=state, create.additional.variables=FALSE)
		if(is.null(years)) update.years <- sort(unique(tmp_sgp_object@Data$YEAR)) else update.years <- years

		if (overwrite.existing.data) {
				what_sgp_object@Data <- as.data.table(rbind.fill(what_sgp_object@Data[YEAR!=update.years], tmp_sgp_object@Data))
				what_sgp_object@SGP[['Goodness_of_Fit']][grep(update.years, names(what_sgp_object@SGP[['Goodness_of_Fit']]))] <- NULL
				what_sgp_object@SGP[['SGPercentiles']][grep(update.years, names(what_sgp_object@SGP[['SGPercentiles']]))] <- NULL
				what_sgp_object@SGP[['SGProjections']][grep(update.years, names(what_sgp_object@SGP[['SGProjections']]))] <- NULL
				what_sgp_object@SGP[['Simulated_SGPs']][grep(update.years, names(what_sgp_object@SGP[['Simulated_SGPs']]))] <- NULL
				if (is.null(sgp.use.my.coefficient.matrices)) {
					what_sgp_object@SGP[['Coefficient_Matrices']][grep(update.years, names(what_sgp_object@SGP[['Coefficient_Matrices']]))] <- NULL
				}

			if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
				what_sgp_object@Data[['HIGH_NEED_STATUS']] <- NULL
				what_sgp_object <- suppressMessages(prepareSGP(what_sgp_object, state=state))
			}

			what_sgp_object <- abcSGP(
						what_sgp_object, 
						steps=steps, 
						years=update.years, 
						content_areas=content_areas,
						grades=grades,
						state=state, 
						sgp.percentiles=sgp.percentiles,
						sgp.projections=sgp.projections,
						sgp.projections.lagged=sgp.projections.lagged,
						sgp.percentiles.baseline=sgp.percentiles.baseline,
						sgp.projections.baseline=sgp.projections.baseline,
						sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
						save.intermediate.results=save.intermediate.results, 
						save.old.summaries=save.old.summaries, 
						sgPlot.demo.report=sgPlot.demo.report,
						sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						calculate.simex = calculate.simex,
						calculate.simex.baseline=calculate.simex.baseline,
						simulate.sgps = simulate.sgps,
						sgp.target.scale.scores=sgp.target.scale.scores,
						sgp.config=sgp.config,
						parallel.config=parallel.config,
						...)

			### Print finish and return SGP object

			message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
			return(what_sgp_object)

		} else {
			if (!is.null(sgp.use.my.coefficient.matrices)) {
				tmp.long.data <- rbind.fill(data.table(what_sgp_object@Data, key=c("VALID_CASE", "CONTENT_AREA", "ID"))[
					data.table(tmp_sgp_object@Data, key=c("VALID_CASE", "CONTENT_AREA", "ID"))[,list(VALID_CASE, CONTENT_AREA, ID)], nomatch='0'], tmp_sgp_object@Data)
				if ("YEAR_WITHIN" %in% names(tmp.long.data)) {
					tmp.long.data$FIRST_OBSERVATION <- NULL
					tmp.long.data$LAST_OBSERVATION <- NULL
				}
				tmp.sgp_object.update <- prepareSGP(tmp.long.data, state=state, create.additional.variables=FALSE)
				tmp.sgp_object.update@SGP$Coefficient_Matrices <- what_sgp_object@SGP$Coefficient_Matrices
				
				if (is.null(SGPstateData[[state]][["SGP_Configuration"]])) {
					SGPstateData[[state]][["SGP_Configuration"]] <- list(return.prior.scale.score.standardized = FALSE)
				} else SGPstateData[[state]][["SGP_Configuration"]][["return.prior.scale.score.standardized"]] <- FALSE

				tmp.sgp_object.update <- analyzeSGP(
							tmp.sgp_object.update,
							years=update.years, 
							content_areas=content_areas,
							grades=grades,
							state=state, 
							sgp.percentiles=sgp.percentiles,
							sgp.projections=sgp.projections,
							sgp.projections.lagged=sgp.projections.lagged,
							sgp.percentiles.baseline=sgp.percentiles.baseline,
							sgp.projections.baseline=sgp.projections.baseline,
							sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
							sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							calculate.simex = calculate.simex,
							calculate.simex.baseline=calculate.simex.baseline,
							simulate.sgps = simulate.sgps,
							sgp.config=sgp.config,
							parallel.config=parallel.config,
							goodness.of.fit.print=FALSE,
							...)
							
				tmp.sgp_object.update <- suppressMessages(combineSGP(tmp.sgp_object.update, state=state))

				### Save SGP object with updated data and full student history
				### Create Data/Updated_Data directory if it doesn't already exist:
				dir.create(file.path("Data", "Updated_Data"), recursive=TRUE, showWarnings=FALSE)

				tmp.file.name <- paste(gsub(" ", "_", toupper(getStateAbbreviation(state, type="name"))), "SGP_Update", paste(update.years, collapse=","), sep="_")
				assign(tmp.file.name, tmp.sgp_object.update)
				save(list=tmp.file.name, file=file.path("Data", "Updated_Data", paste(tmp.file.name, "Rdata", sep=".")))

				### Merge update with original SGP object

				what_sgp_object@Data <- data.table(rbind.fill(what_sgp_object@Data, tmp_sgp_object@Data), key=getKey(what_sgp_object@Data))
				if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
					what_sgp_object@Data[, HIGH_NEED_STATUS := NULL]
					what_sgp_object <- suppressMessages(prepareSGP(what_sgp_object, state=state))
				}
				tmp_sgp_list <- mergeSGP(what_sgp_object@SGP, tmp.sgp_object.update@SGP)

				what_sgp_object@SGP <- tmp_sgp_list

				if ("combineSGP" %in% steps) {
					what_sgp_object <- combineSGP(
						what_sgp_object, 
						years=update.years, 
						state=state,
						sgp.percentiles= sgp.percentiles, 
						sgp.projections= sgp.projections,
						sgp.projections.lagged= sgp.projections.lagged,
						sgp.percentiles.baseline= sgp.percentiles.baseline,
						sgp.projections.baseline=sgp.projections.baseline,
						sgp.projections.lagged.baseline= sgp.projections.lagged.baseline)
				}

				if ("summarizeSGP" %in% steps) what_sgp_object <- summarizeSGP(what_sgp_object, state=state, parallel.config=parallel.config)
				if ("visualizeSGP" %in% steps) visualizeSGP(what_sgp_object)
				if ("outputSGP" %in% steps) outputSGP(what_sgp_object)

				###  Output just additional update data
				###  Do this AFTER rbind.fill @Data, mergeSGP, combineSGP, etc.				
				if (update.years  %in% unique(tmp_sgp_object@Data$YEAR)) {
					tmp_sgp_object@SGP <- tmp.sgp_object.update@SGP
					tmp_sgp_object <- suppressMessages(combineSGP(tmp_sgp_object, state=state))
					outputSGP(tmp_sgp_object, state = state, output.type = "LONG_Data", outputSGP.directory = file.path("Data", "Updated_Data"))
				} else message("NOTE: with_sgp_data_LONG appears to only contain priors.  Only results containing the entire student history have been saved.")

				### Print finish and return SGP object

				message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
				return(what_sgp_object)
			} else {
				what_sgp_object@Data <- data.table(rbind.fill(what_sgp_object@Data, tmp_sgp_object@Data), key=getKey(what_sgp_object@Data))

				if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
					what_sgp_object@Data[['HIGH_NEED_STATUS']] <- NULL
					what_sgp_object <- suppressMessages(prepareSGP(what_sgp_object, state=state))
				}

				### abcSGP

				what_sgp_object <- abcSGP(
							what_sgp_object, 
							steps=steps, 
							years=update.years, 
							content_areas=content_areas,
							grades=grades,
							state=state, 
							sgp.percentiles=sgp.percentiles,
							sgp.projections=sgp.projections,
							sgp.projections.lagged=sgp.projections.lagged,
							sgp.percentiles.baseline=sgp.percentiles.baseline,
							sgp.projections.baseline=sgp.projections.baseline,
							sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
							save.intermediate.results=save.intermediate.results, 
							save.old.summaries=save.old.summaries, 
							sgPlot.demo.report=sgPlot.demo.report,
							sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							calculate.simex = calculate.simex,
							calculate.simex.baseline=calculate.simex.baseline,
							simulate.sgps = simulate.sgps,
							sgp.target.scale.scores=sgp.target.scale.scores,
							sgp.config=sgp.config,
							parallel.config=parallel.config,
							...)

				### Print finish and return SGP object

				message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
				return(what_sgp_object)
			} ### END if else (!is.null(sgp.use.my.coefficient.matrices))
		} ### END if (overwrite.existing.data)
	} ### END !is.null(with_sgp_data_LONG)
} ## END updateSGP Function
