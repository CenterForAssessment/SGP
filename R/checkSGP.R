`checkSGP` <-
function(sgp_object) {

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


	### Perform checks

	sgp_object <- checkVariableClass(sgp_object)


	### Return sgp_object

	return(sgp_object)

} ### END sgp_object
