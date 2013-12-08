`updateSGP` <- 
function(what_sgp_object=NULL,
	with_sgp_data_LONG=NULL,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
	years=NULL,
	content_areas=NULL,
	save.old.summaries=TRUE,
	save.intermediate.results=TRUE,
	sgp.use.my.coefficient.matrices=NULL,
	overwrite.previous.data=FALSE,
	sgPlot.demo.report=TRUE,
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
	if (length(grep("BASELINE", matrix.names)) > 0) {
		tf.sgp.baseline <- TRUE	
	} else {
		tf.sgp.baseline <- FALSE
	}


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

		### NULL out previous results to be re-calculated

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
					sgp.percentiles=TRUE,
					sgp.projections=TRUE,
					sgp.projections.lagged=TRUE,
					sgp.percentiles.baseline=tf.sgp.baseline,
					sgp.projections.baseline=tf.sgp.baseline,
					sgp.projections.lagged.baseline=tf.sgp.baseline,
					sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
					save.intermediate.results=save.intermediate.results,
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

		if (overwrite.previous.data) {
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
						sgp.percentiles=TRUE,
						sgp.projections=TRUE,
						sgp.projections.lagged=TRUE,
						sgp.percentiles.baseline=tf.sgp.baseline,
						sgp.projections.baseline=tf.sgp.baseline,
						sgp.projections.lagged.baseline=tf.sgp.baseline,
						save.intermediate.results=save.intermediate.results, 
						save.old.summaries=save.old.summaries, 
						sgPlot.demo.report=sgPlot.demo.report,
						sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						parallel.config=parallel.config,
						...)

			### Print finish and return SGP object

			message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
			return(what_sgp_object)

		} else {
			if (!is.null(sgp.use.my.coefficient.matrices)) {
				tmp.long.data <- rbind.fill(subset(what_sgp_object@Data, ID %in% unique(tmp_sgp_object@Data[['ID']])), tmp_sgp_object@Data)
				tmp.sgp_object <- prepareSGP(tmp.long.data, state=state, create.additional.variables=FALSE)
				tmp.sgp_object <- analyzeSGP(
							tmp.sgp_object,
							years=update.years, 
							state=state, 
							sgp.percentiles=TRUE,
							sgp.projections=TRUE,
							sgp.projections.lagged=TRUE,
							sgp.percentiles.baseline=tf.sgp.baseline,
							sgp.projections.baseline=tf.sgp.baseline,
							sgp.projections.lagged.baseline=tf.sgp.baseline,
							save.intermediate.results=save.intermediate.results, 
							save.old.summaries=save.old.summaries, 
							sgPlot.demo.report=sgPlot.demo.report,
							sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							goodness.of.fit.print=FALSE,
							...)

				what_sgp_object <- mergeSGP(what_sgp_object, tmp.sgp_object)
				what_sgp_object <- combineSGP(what_sgp_object, years=update.years, state=state, parallel.config=parallel.config)
				what_sgp_object <- summarizeSGP(what_sgp_object, state=state, parallel.config=parallel.config)
				if ("visualizeSGP" %in% steps) visualizeSGP(what_sgp_object)
				if ("outputSGP" %in% steps) outputSGP(what_sgp_object)

				### Print finish and return SGP object

				message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
				return(what_sgp_object)
			} else {
				what_sgp_object@Data <- as.data.table(rbind.fill(what_sgp_object@Data, tmp_sgp_object@Data))

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
							sgp.percentiles=TRUE,
							sgp.projections=TRUE,
							sgp.projections.lagged=TRUE,
							sgp.percentiles.baseline=tf.sgp.baseline,
							sgp.projections.baseline=tf.sgp.baseline,
							sgp.projections.lagged.baseline=tf.sgp.baseline,
							save.intermediate.results=save.intermediate.results, 
							save.old.summaries=save.old.summaries, 
							sgPlot.demo.report=sgPlot.demo.report,
							sgp.use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							parallel.config=parallel.config,
							...)

				### Print finish and return SGP object

				message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
				return(what_sgp_object)
			} ### END if else (!is.null(sgp.use.my.coefficient.matrices))
		} ### END if (overwrite.previous.data)
	} ### END !is.null(with_sgp_data_LONG)
} ## END updateSGP Function
