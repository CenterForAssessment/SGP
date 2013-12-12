`updateSGP` <- 
function(what_sgp_object=NULL,
	with_sgp_data_LONG=NULL,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
	years=NULL,
	content_areas=NULL,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	calculate.simex = NULL,
	simulate.sgps = FALSE,
	save.old.summaries=TRUE,
	save.intermediate.results=TRUE,
	sgp.use.my.coefficient.matrices=NULL,
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
					sgp_object=sgp_object,
					state=state,
					steps=steps,
					years=years,
					content_areas=content_areas,
					sgp.percentiles=sgp.percentiles,
					sgp.projections=sgp.projections,
					sgp.projections.lagged=sgp.projections.lagged,
					sgp.percentiles.baseline=sgp.percentiles.baseline,
					sgp.projections.baseline=sgp.projections.baseline,
					sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
					sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
					save.intermediate.results=save.intermediate.results,
					calculate.simex = calculate.simex,
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

		YEAR <- ID <- NULL
		tmp_sgp_object <- prepareSGP(with_sgp_data_LONG, state=state, create.additional.variables=FALSE)
		update.years <- sort(unique(tmp_sgp_object@Data$YEAR))

		if (overwrite.existing.data) {
				what_sgp_object@Data <- as.data.table(rbind.fill(what_sgp_object@Data[YEAR!=update.years], tmp_sgp_object@Data))
				what_sgp_object@SGP[['Goodness_of_Fit']][grep(update.years, names(what_sgp_object@SGP[['Goodness_of_Fit']]))] <- NULL
				what_sgp_object@SGP[['SGPercentiles']][grep(update.years, names(what_sgp_object@SGP[['SGPercentiles']]))] <- NULL
				what_sgp_object@SGP[['SGProjections']][grep(update.years, names(what_sgp_object@SGP[['SGProjections']]))] <- NULL
				what_sgp_object@SGP[['Simulated_SGPs']][grep(update.years, names(what_sgp_object@SGP[['Simulated_SGPs']]))] <- NULL
				sgp.use.my.coefficient.matrices <- sgp.use.my.coefficient.matrices

			if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
				what_sgp_object@Data[['HIGH_NEED_STATUS']] <- NULL
				what_sgp_object <- suppressMessages(prepareSGP(what_sgp_object, state=state))
			}

			what_sgp_object <- abcSGP(
						what_sgp_object, 
						steps=steps, 
						years=update.years, 
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
						simulate.sgps = simulate.sgps,
						sgp.config=sgp.config,
						parallel.config=parallel.config,
						...)

			### Print finish and return SGP object

			message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
			return(what_sgp_object)

		} else {
			if (!is.null(sgp.use.my.coefficient.matrices)) {
				tmp.long.data <- rbind.fill(subset(what_sgp_object@Data, ID %in% unique(tmp_sgp_object@Data[['ID']])), tmp_sgp_object@Data)
				if ("YEAR_WITHIN" %in% names(tmp.long.data)) {
					tmp.long.data$FIRST_OBSERVATION <- NULL
					tmp.long.data$LAST_OBSERVATION <- NULL
				}
				tmp.sgp_object.update <- prepareSGP(tmp.long.data, state=state, create.additional.variables=FALSE)
				tmp.sgp_object.update@SGP$Coefficient_Matrices <- what_sgp_object@SGP$Coefficient_Matrices

				tmp.sgp_object.update <- analyzeSGP(
							tmp.sgp_object.update,
							years=update.years, 
							state=state, 
							sgp.percentiles=sgp.percentiles,
							sgp.projections=sgp.projections,
							sgp.projections.lagged=sgp.projections.lagged,
							sgp.percentiles.baseline=sgp.percentiles.baseline,
							sgp.projections.baseline=sgp.projections.baseline,
							sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
							sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							calculate.simex = calculate.simex,
							simulate.sgps = simulate.sgps,
							sgp.config=sgp.config,
							parallel.config=parallel.config,
							goodness.of.fit.print=FALSE,
							...)
				tmp.sgp_object.update <- combineSGP(tmp.sgp_object.update, state=state)

				### Save analyses with just update

				tmp.file.name <- paste(gsub(" ", "_", toupper(getStateAbbreviation(state, type="name"))), "SGP_Update", paste(update.years, collapse=","), sep="_")
				assign(tmp.file.name, tmp.sgp_object.update)
				save(list=tmp.file.name, file=file.path("Data", paste(tmp.file.name, "Rdata", sep=".")))

				### Merge update with original SGP object

				what_sgp_object@Data <- data.table(rbind.fill(what_sgp_object@Data, tmp_sgp_object@Data), key=getKey(what_sgp_object@Data))
				if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
					what_sgp_object@Data[['HIGH_NEED_STATUS']] <- NULL
					what_sgp_object <- suppressMessages(prepareSGP(what_sgp_object, state=state))
				}
				what_sgp_object@SGP <- mergeSGP(what_sgp_object@SGP, tmp.sgp_object.update@SGP)
				what_sgp_object <- combineSGP(what_sgp_object, years=update.years, state=state, parallel.config=parallel.config)

				if ("summarizeSGP" %in% steps) what_sgp_object <- summarizeSGP(what_sgp_object, state=state, parallel.config=parallel.config)
				if ("visualizeSGP" %in% steps) visualizeSGP(what_sgp_object)
				if ("outputSGP" %in% steps) outputSGP(what_sgp_object)

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
							simulate.sgps = simulate.sgps,
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
