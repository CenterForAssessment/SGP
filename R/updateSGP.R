`updateSGP` <- 
function(what_sgp_object,
	with_sgp_data_LONG,
	state=NULL,
	save.old.summaries=TRUE,
	...) {

        started.at <- proc.time()
	message(paste("\nStarted updateSGP", date()), "\n")


	### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(what_sgp_object))))
		state <- getStateAbbreviation(tmp.name, "updateSGP")
        }


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
} ## END updateSGP Function
