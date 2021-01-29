`getStateAbbreviation` <-
function(
	supplied.name,
	SGPfunction=NULL,
	type="Abbreviation") {

	my.state.abbreviations <- c(setdiff(datasets::state.abb, c("MA", "WA")), "ABQ", "AOB", "ATI", "BI", "CO_ORIGINAL", "DC", "DD", rep("DEMO", 2), "DEMO_COVID", "DEMO_COVID", "DEMO_EOCT", "GCPS", "GUA", "MA_MCAS", "MA_PARCC", "MA", "MA_ORIGINAL", "NCSC_SD", "NJ_ORIGINAL", "PARCC", "RI_ORIGINAL", "RLI_UK", "RLI", "SBAC", "VI", "WIDA_CO", "WIDA_DPS", "WIDA_GA", "WIDA_IN", "WIDA_MA", "WIDA_MI", "WIDA_NH", "WIDA_NV", "WIDA_RI", "WIDA_WI", "WIDA", "WA") ### NOTE: Add abbreviations ALPHABETICALLY
	my.state.names <- c(setdiff(datasets::state.name, c("Massachusetts", "Washington")), "Albuquerque", "AOB", "ATI", "Bureau Indian Affairs", "Colorado", "Washington DC", "Department of Defense", "Demonstration", "SGPDATA LONG", "Demonstration COVID", "SGPDATA LONG COVID", "Demonstration", "Gwinnett", "Guatemala", "Massachusetts MCAS", "Massachusetts PARCC", "Massachusetts", "Massachusetts", "NCSC SD", "New Jersey", "PARCC", "Rhode Island", "RLI UK", "RLI", "SBAC", "US Virgin Islands", "WIDA CO", "WIDA DPS", "WIDA GA", "WIDA IN", "WIDA MA", "WIDA MI", "WIDA NH", "WIDA NV", "WIDA RI", "WIDA WI", "WIDA", "Washington") ### NOTE: Add abbreviations (not states) names ALPHABETICALLY - need compound abbreviations first (e.g. WIDA_CO before WIDA)

	if (type=="Abbreviation") {
		tmp.name.position <- sapply(my.state.names, function(x) regexpr(toupper(x), toupper(supplied.name)))
	} else {
		tmp.name.position <- sapply(lapply(my.state.abbreviations, function(x) regexpr(toupper(x), toupper(supplied.name))), function(x) attributes(x)[['match.length']])
	}

	if (any(tmp.name.position!=-1)) {
		if (type=="Abbreviation") {
			if (length(sub.name.position <- names(sort(tmp.name.position[tmp.name.position!=-1]))) > 1) {
				if (any(grepl("COVID", sub.name.position))) {
					sub.name.position <- grep("COVID", sub.name.position, value = "TRUE")
				}
				# Didn't work with all cases (both longer and shorter supplied.name at the same time)
				# sub.name.position <- sub.name.position[agrep(supplied.name, sub.name.position, ignore.case=TRUE, max.distance=0.5)] # For longer supplied.name (e.g. 'DEMONSTRATION COVID DATA LONG')
				# if (length(sub.name.position > 1)) {
				# 	if (!is.na(match(supplied.name, sub.name.position))) {
				# 		sub.name.position <- sub.name.position[match(supplied.name, sub.name.position)] # For shorter supplied.name (e.g. 'SGPDATA LONG COVID')
				# 	} else sub.name.position <- sub.name.position[1] # Admit defeat and use old selection method
				# }
			}
			my.state.abbreviations[which(sub.name.position==my.state.names)[1]]
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
