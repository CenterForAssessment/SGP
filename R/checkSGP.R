`checkSGP` <-
function(sgp_object, state=NULL) {

        ### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration", "sgpData LONG", "AOB"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, rep("DEMO", 2), "AOB")[which(sapply(c(state.name, "Demonstration", "sgpData LONG", "AOB"), function(x) regexpr(x, tmp.name))==1)]
                }
        }

	### Utility functions

	## checkVariableClass

	checkVariableClass <- function(my.data) {
		for (my.variable in c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")) {
			if (is.SGP(my.data)) {
				if (is.factor(my.data@Data[[my.variable]])) {
					my.data@Data[[my.variable]] <- as.character(my.data@Data[[my.variable]])
					message(paste("\tNOTE:", my.variable, "converted from class factor to class character to accomodate data.table 1.8.0 changes."))
				}
			}
			if (is.data.frame(my.data)) {
				if (is.factor(my.data[[my.variable]])) {
					my.data[[my.variable]] <- as.character(my.data[[my.variable]])
					message(paste("\tNOTE:", my.variable, "converted from class factor to class character to accomodate data.table 1.8.0 changes."))
				}
			}
		}
		return(my.data)
	}


	###########################################
	###
	### Perform checks
	###
	###########################################

	### Check class of variables used with data.table keys

	sgp_object <- checkVariableClass(sgp_object)


	### Check is ACHIEVEMENT_LEVEL levels are in SGPstateData

	if (!is.null(state)) {
		if (is.SGP(sgp_object)) {
			if (!all(levels(sgp_object@Data$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']])) {
				missing.achievement.levels <- 
					levels(sgp_object@Data$ACHIEVEMENT_LEVEL)[!levels(sgp_object@Data$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']]]
				message(paste("\tNOTE: Achievement level(s):", missing.achievement.levels, "in supplied data are not contained in 'SGPstateData'."))
			}
		}
		if (is.data.frame(sgp_object)) {
			if (!all(levels(sgp_object$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']])) {
				missing.achievement.levels <- 
					levels(sgp_object$ACHIEVEMENT_LEVEL)[!levels(sgp_object$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']]]
				message(paste("\tNOTE: Achievement level(s):", missing.achievement.levels, "in supplied data are not contained in 'SGPstateData'."))
			}
		}
	}


	### Return sgp_object

	return(sgp_object)

} ### END sgp_object
