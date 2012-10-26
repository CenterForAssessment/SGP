`updateSGP` <- 
function(what_sgp_object,
	with_sgp_data_LONG,
	...) {

        started.at <- proc.time()
	message(paste("\nStarted updateSGP", date()), "\n")

	### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                tmp.name.position <- sapply(c(state.name, "AOB", "DEMONSTRATION"), function(x) regexpr(toupper(x), tmp.name))
                if (any(tmp.name.position!=-1)) {
                        state <- c(state.abb, "AOB", "DEMO")[which(names(sort(tmp.name.position[tmp.name.position!=-1])[1])==c(state.name, "AOB", "DEMONSTRATION"))]
                }
        }

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(what_sgp_object))))
                tmp.name.position <- sapply(c(state.name, "AOB", "DEMONSTRATION"), function(x) regexpr(toupper(x), tmp.name))
                if (any(tmp.name.position!=-1)) {
                        state <- c(state.abb, "AOB", rep("DEMO", 2))[which(names(sort(tmp.name.position[tmp.name.position!=-1])[1])==c(state.name, "AOB", rep("DEMONSTRATION",2)))]
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
