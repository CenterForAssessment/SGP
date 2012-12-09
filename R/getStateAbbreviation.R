`getStateAbbreviation` <- 
function(
	supplied.name,
	SGPfunction=NULL) {

	my.state.abbreviations <- c(state.abb, "AOB", rep("DEMO", 2)) ### NOTE: Add abbreviations ALPHABETICALLY
	my.state.names <- c(state.name, "AOB", "DEMONSTRATION", "SGPDATA LONG") ### NOTE: Add state names ALPHABETICAALY
	tmp.name.position <- sapply(my.state.names, function(x) regexpr(toupper(x), supplied.name))
		if (any(tmp.name.position!=-1)) {
			my.state.abbreviations[which(names(sort(tmp.name.position[tmp.name.position!=-1])[1])==my.state.names)[1]]
		} else {
			if (!is.null(SGPfunction)) {
				message(paste("\tNOTE: Use of the higher level '", SGPfunction, "' function requires extensive metadata embedded in the 'SGPstateData' list object.\n\tEither supply the two letter state acronymn as an argument or name the object supplied as 'sgp_object' using the entire state name (e.g., 'Colorado_SGP').\n\tIf your state's meta data is not a part of the package, please add your state's data to 'SGPstateData' by examining a state that is currently embedded in https://github.com/SchoolView/SGPstateData/blob/master/SGPstateData.R.\n\tPlease contact the package administrator with further questions.", sep=""))
			} else {
				message(paste("\tNOTE: Either supply the two letter state acronymn as an argument or name the object supplied as 'sgp_object' using the entire state name (e.g., 'Colorado_SGP').\n\tIf your state's meta data is not a part of the package, please add your state's data to 'SGPstateData' by examining state that is currently embedded in https://github.com/SchoolView/SGPstateData/blob/master/SGPstateData.R.\n\tPlease contact the package administrator with further questions.", sep=""))
			}
		}
} ### END getStateAbbreviation

