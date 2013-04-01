`updateSGP` <- 
function(what_sgp_object=NULL,
	with_sgp_data_LONG=NULL,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	save.old.summaries=TRUE,
	save.intermediate.results=FALSE,
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

	### What updateSGP

	if (!is.null(what_sgp_object) & is.null(with_sgp_data_LONG)) {
		
		tmp.years <- tmp.content_areas.years <- list()

		matrix.names <- names(what_sgp_object@SGP[['Coefficient_Matrices']])

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

		if (length(grep("BASELINE", matrix.names)) > 0) {
			tf.sgp.baseline <- TRUE	
		} else {
			tf.sgp.baseline <- FALSE
		}

		### NULL out previous results to be re-calculated

		what_sgp_object@SGP[['Goodness_of_Fit']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['Goodness_of_Fit']]))] <- NULL
		what_sgp_object@SGP[['SGPercentiles']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['SGPercentiles']]))] <- NULL
		what_sgp_object@SGP[['SGProjections']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['SGProjections']]))] <- NULL
		what_sgp_object@SGP[['Simulated_SGPs']][grep(paste(tmp.content_areas.years, collapse="|"), names(what_sgp_object@SGP[['Simulated_SGPs']]))] <- NULL
		

		### Re-calculate

		sgp_object <- prepareSGP(
				what_sgp_object,
				state=state)

		if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")

		sgp_object <- analyzeSGP(
					sgp_object=sgp_object,
					state=state,
					years=years,
					content_areas=content_areas,
					sgp.percentiles=FALSE,
					sgp.projections=FALSE,
					sgp.projections.lagged=FALSE,
					sgp.percentiles.baseline=tf.sgp.baseline,
					sgp.projections.baseline=tf.sgp.baseline,
					sgp.projections.lagged.baseline=tf.sgp.baseline,
					sgp.use.my.coefficient.matrices=TRUE,
					...
					)

		if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")

		sgp_object <- combineSGP(sgp_object, state=state, years=years, content_areas=content_areas) 

		if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")

		if (!is.null(sgp_object@Summary)) {
			sgp_object <- summarizeSGP(sgp_object, state=state, ...)
			if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
		}

		### Print finish and return SGP object

		message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
		return(sgp_object)

	} ### END What updateSGP


	### What/With updateSGP

	if (!is.null(what_sgp_object) & !is.null(with_sgp_data_LONG)) {

		### Use prepareSGP on supplied 'with_sgp_data_LONG'

		tmp_sgp_object <- prepareSGP(with_sgp_data_LONG, state=state, create.additional.variables=FALSE)
		what_sgp_object@Data <- as.data.table(rbind.fill(what_sgp_object@Data, tmp_sgp_object@Data))
		if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
			what_sgp_object@Data[['HIGH_NEED_STATUS']] <- NULL
			what_sgp_object <- suppressMessages(prepareSGP(what_sgp_object, state=state))
		}


		### abcSGP

		new.year <- sort(unique(with_sgp_data_LONG$YEAR))
		what_sgp_object <- abcSGP(what_sgp_object, years=new.year, state=state, save.intermediate.results=save.intermediate.results, save.old.summaries=save.old.summaries,...)


		### Print finish and return SGP object

		message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
		return(what_sgp_object)

	} ### END what/with updateSGP

} ## END updateSGP Function
