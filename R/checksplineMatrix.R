`checksplineMatrix` <-
function(list.of.splineMatrix,
	content_area=NULL,
	year=NULL,
	sgp_object=NULL) {
	if (is.splineMatrix(list.of.splineMatrix)) {
		list.of.splineMatrix <- list(list.of.splineMatrix)
	}
	for (i in names(list.of.splineMatrix)) {
		splineMatrix.tf <- sapply(list.of.splineMatrix[[i]], validObject, test=TRUE)==TRUE
		if (!all(splineMatrix.tf)) {
			content_area <- unlist(strsplit(i, "[.]"))[1]; year <- unlist(strsplit(i, "[.]"))[2]
			for (j in names(list.of.splineMatrix[[i]])[!splineMatrix.tf]) {
				message(paste("\tNOTE: Updating Coefficient Matrix", i, j, "to new splineMatrix class."))
				list.of.splineMatrix[[i]][[j]] <- 
					as.splineMatrix(matrix_argument=list.of.splineMatrix[[i]][[j]], matrix_argument_name=j, content_area=content_area, year=year, sgp_object=sgp_object)
			}
		}
		list.of.splineMatrix[[i]] <- unique.splineMatrix(list.of.splineMatrix[[i]])
	}
	return(list.of.splineMatrix)
} ### END checksplineMatrix
