`checksplineMatrix` <-
function(list.of.splineMatrix, sgp_object=NULL) {
	if (is.splineMatrix(list.of.splineMatrix)) {
		list.of.splineMatrix <- list(list.of.splineMatrix)
	}
	for (i in names(list.of.splineMatrix)) {
		splineMatrix.tf <- sapply(list.of.splineMatrix[[i]], validObject, test=TRUE)==TRUE
		if (!all(splineMatrix.tf)) {
			for (j in names(list.of.splineMatrix[[i]])[!splineMatrix.tf]) {
				message(paste("\tNOTE: Updating Coefficient Matrix", i, j, "to new splineMatrix class."))
				list.of.splineMatrix[[i]][[j]] <- as.splineMatrix(matrix_argument=list.of.splineMatrix[[i]][[j]], matrix_argument_name=j, sgp_object=sgp_object)
			}
		}
		list.of.splineMatrix[[i]] <- unique.splineMatrix(list.of.splineMatrix[[i]])
	}
	return(list.of.splineMatrix)
} ### END checksplineMatrix
