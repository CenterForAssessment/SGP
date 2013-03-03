`updateSGP` <- 
function(what_sgp_object=NULL,
	with_sgp_data_LONG=NULL,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	save.old.summaries=TRUE,
	...) {

        started.at <- proc.time()
	message(paste("\nStarted updateSGP", date()), "\n")


	### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(what_sgp_object))))
		state <- getStateAbbreviation(tmp.name, "updateSGP")
        }

	
	### Argument checks

	if (is.null(what_sgp_object)) {
		stop("\tNOTE: Argument 'what_sgp_object' must be supplied to updateSGP (at a minimum). See man page for 'updateSGP' for details.")
	}

	### What updateSGP

	if (!is.null(what_sgp_object) & is.null(with_sgp_data_LONG)) {

		matrix.names <- names(what_sgp_object@SGP[['Coefficient_Matrices']])

		if (is.null(content_areas)) {
			content_areas <- sort(unique(sapply(strsplit(matrix.names, "[.]"), '[', 1)))
		}
		if (is.null(years)) {
			for (i in content_areas) {
				tmp.content_area.matrix.names <- grep(i, matrix.names, value=TRUE)
			}
		} else {
			for (i in content_areas) {
				tmp.years[[i]] <- years
			}
		}

		what_sgp_object <- prepareSGP(what_sgp_object)


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
		what_sgp_object <- abcSGP(what_sgp_object, years=new.year, state=state, save.old.summaries=save.old.summaries,...)


		### Print finish and return SGP object

		message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
		return(what_sgp_object)

	} ### END what/with updateSGP

} ## END updateSGP Function
