`getStateAbbreviation` <-
function(
	supplied.name,
	SGPfunction=NULL,
	type="Abbreviation") {

	my.state.abbreviations <- c(setdiff(datasets::state.abb, c("MA", "WA")), "ABQ", "AOB", "ATI", "CO_ORIGINAL", rep("DEMO", 2), "GUA", "MA_MCAS", "MA_PARCC", "MA", "MA_ORIGINAL", "NCSC_SD", "NJ_ORIGINAL", "PARCC", "RI_ORIGINAL", "RLI_UK", "RLI", "WIDA_CO", "WIDA_GA", "WIDA_MA", "WIDA_MI", "WIDA_NV", "WIDA", "DC", "WA") ### NOTE: Add abbreviations ALPHABETICALLY
	my.state.names <- c(setdiff(datasets::state.name, c("Massachusetts", "Washington")), "Albuquerque", "AOB", "ATI", "Colorado", "DEMONSTRATION", "SGPDATA LONG", "Guatemala", "Massachusetts MCAS", "Massachusetts PARCC", "Massachusetts", "Massachusetts", "NCSC SD", "New Jersey", "PARCC", "Rhode Island", "RLI UK", "RLI", "WIDA CO", "WIDA GA", "WIDA MA", "WIDA MI", "WIDA NV", "WIDA", "WASHINGTON DC", "WASHINGTON") ### NOTE: Add state names ALPHABETICALLY - need compound abbreviations/names first (e.g. WIDA_CO before WIDA)
	if (type=="Abbreviation") {
		tmp.name.position <- sapply(my.state.names, function(x) regexpr(toupper(x), supplied.name))
	} else {
		tmp.name.position <- sapply(lapply(my.state.abbreviations, function(x) regexpr(toupper(x), supplied.name)), function(x) attributes(x)[['match.length']])
	}
		if (any(tmp.name.position!=-1)) {
			if (type=="Abbreviation") {
				my.state.abbreviations[which(names(sort(tmp.name.position[tmp.name.position!=-1])[1])==my.state.names)[1]]
			} else {
				my.state.names[which(tmp.name.position==nchar(supplied.name))[1]]
			}
		} else {
			if (!is.null(SGPfunction)) {
				message(paste0("\tNOTE: Use of the higher level '", SGPfunction, "' function requires extensive metadata embedded in the 'SGPstateData' list object.\n\tEither supply the two letter state acronymn as an argument or name the object supplied as 'sgp_object' using the entire state name (e.g., 'Colorado_SGP').\n\tIf your state's meta-data is not a part of the package, please add your state's data to 'SGPstateData' by examining a state that is currently embedded in https://github.com/CenterForAssessment/SGPstateData/blob/master/SGPstateData.R.\n\tPlease contact the package administrator with further questions."))
			} else {
				message(paste0("\tNOTE: Either supply a state/organization acronymn as an argument or name the object supplied as 'sgp_object' using the entire state name (e.g., 'Colorado_SGP').\n\tIf your state's meta-data is not a part of the package, please add your state's data to 'SGPstateData' by examining state that is currently embedded in https://github.com/CenterForAssessment/SGPstateData/blob/master/SGPstateData.R.\n\tPlease contact the package administrator with further questions."))
			}
		}
} ### END getStateAbbreviation
