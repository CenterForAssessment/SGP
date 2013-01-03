`checksplineMatrix` <-
function(list.of.splineMatrix, sgp_object=NULL) {
	if (is.splineMatrix(list.of.splineMatrix)) {
		list.of.splineMatrix <- list(list.of.splineMatrix)
	}
	for (i in names(list.of.splineMatrix)) {
		splineMatrix.tf <- sapply(list.of.splineMatrix[[i]], validObject, test=TRUE)==TRUE
		if (!any(splineMatrix.tf)) {
			tmp.changes <- TRUE
			message(paste("Updating Existing Coefficient Matrix", i, "to new splineMatrix class."))
			for (j in names(list.of.splineMatrix[[i]])[!splineMatrix.tf]) {
				list.of.splineMatrix[[i]][[j]] <- as.splineMatrix(matrix_argument=list.of.splineMatrix[[i]][[j]], matrix_argument_name=j, sgp_object=sgp_object)
			}
		}
	}
	return(list.of.splineMatrix)
} ### END checksplineMatrix
