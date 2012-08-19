`checkSGP` <-
function(sgp_object, state=NULL) {

	### Check if sgp_object is of class SGP

	if (!is.SGP(sgp_object)) stop("NOTE: Check SGP accepts only objects of class SGP. See manual pages for details.")


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

		for (my.variable in c("ID")) {
			if (my.variable %in% names(my.data) && !is.character(my.data[[my.variable]])) {
				my.data[[my.variable]] <- as.character(my.data[[my.variable]])
				message(paste("\tNOTE:", my.variable, "converted to class character to take advantage of data.table 1.8.0 improvement."))
			}
		}

		for (my.variable in c("VALID_CASE", "CONTENT_AREA", "YEAR")) {
			if (my.variable %in% names(my.data) && is.factor(my.data[[my.variable]])) {
				my.data[[my.variable]] <- as.character(my.data[[my.variable]])
				message(paste("\tNOTE:", my.variable, "converted from class factor to class character to accomodate data.table 1.8.0 changes."))
			}
		}
		return(my.data)
	}


	###########################################
	###
	### Perform checks
	###
	###########################################

	### Check class of variables in @Data

	sgp_object@Data <- checkVariableClass(sgp_object@Data)

	### Check class of variables in @SGP$SGPercentiles and @SGP$SGProjections

	for (i in names(sgp_object@SGP$SGPercentiles)) {
		sgp_object@SGP[['SGPercentiles']][[i]] <- checkVariableClass(sgp_object@SGP[['SGPercentiles']][[i]])
	}

	for (i in names(sgp_object@SGP$SGProjections)) {
		sgp_object@SGP[['SGProjections']][[i]] <- checkVariableClass(sgp_object@SGP[['SGProjections']][[i]])
	}


	### Check if ACHIEVEMENT_LEVEL levels are in SGPstateData

	if (!is.null(state)) {
		if (!all(levels(sgp_object@Data$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']])) {
			missing.achievement.levels <- 
				levels(sgp_object@Data$ACHIEVEMENT_LEVEL)[!levels(sgp_object@Data$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']]]
			message(paste("\tNOTE: Achievement level(s):", missing.achievement.levels, "in supplied data are not contained in 'SGPstateData'."))
		}
	}


	### Return sgp_object

	return(sgp_object)

} ### END sgp_object
