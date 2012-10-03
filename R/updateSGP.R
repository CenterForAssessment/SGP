`updateSGP` <- 
function(what_sgp_object,
	with_sgp_data_LONG,
	...) {

        started.at <- proc.time()
	message(paste("\nStarted updateSGP", date()), "\n")

	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(what_sgp_object))))
		if (any(sapply(c(state.name, "Demonstration", "sgpData LONG", "AOB"), function(x) regexpr(toupper(x), tmp.name)))!=-1) {
			state <- c(state.abb, rep("DEMO", 2), "AOB")[which(sapply(c(state.name, "Demonstration", "sgpData LONG", "AOB"), function(x) regexpr(toupper(x), tmp.name))!=1)[1]]
		} else {
			message("\tNOTE: Use of the higher level 'updateSGP' function requires extensive metadata embedded in the 'SGPstateData' list object. Please add your state's data to 'SGPstateData' by examining a state that is currently embedded. For example, SGPstateData[['DEMO']]. Please contact the package administrator with further questions.")
		}
	}


	### Use prepareSGP on supplied 'with_sgp_data_LONG' ###

		tmp_sgp_object <- prepareSGP(with_sgp_data_LONG, state=state, create.additional.variables=FALSE)
		what_sgp_object@Data <- rbind.fill(what_sgp_object@Data, tmp_sgp_object@Data)
		if ("HIGH_NEED_STATUS" %in% names(what_sgp_object@Data)) {
			what_sgp_object@Data[['HIGH_NEED_STATUS']] <- NULL
			what_sgp_object <- prepareSGP(what_sgp_object, state=state)
		}


	### abcSGP ###

		new.year <- unique(tmp_sgp_object@Data$YEAR)
		what_sgp_object <- abcSGP(what_sgp_object, years=new.year, ...)


	### Print finish and return SGP object

        message(paste("Finished updateSGP", date(), "in", timetaken(started.at), "\n"))
	return(what_sgp_object)

} ## END updateSGP Function
