`getsplineMatrix` <- 
function(
	my.matrices,
	my.matrix.content.area.progression,
	my.matrix.grade.progression,
	my.matrix.time.progression,
	my.matrix.time.progression.lags,
	my.matrix.order=NULL,
	return.only.orders=FALSE) {

	Matrix_TF <- Order <- NULL

	splineMatrix_equality <- function(my.matrices, my.order=NULL) {
		tmp.df <- data.frame()
		if (is.null(my.order)) my.order <- (2:length(my.matrix.time.progression))-1
		for (i in seq_along(my.order)) {
			tmp.df[i,1] <- identical(my.matrices@Content_Areas[[1]], tail(my.matrix.content.area.progression, my.order[i]+1)) & 
					identical(my.matrices@Grade_Progression[[1]], as.character(tail(my.matrix.grade.progression, my.order[i]+1))) & 
					identical(my.matrices@Time[[1]], as.character(tail(my.matrix.time.progression, my.order[i]+1))) &
					identical(my.matrices@Time_Lags[[1]], tail(my.matrix.time.progression.lags, my.order[i]+1))
			tmp.df[i,2] <-	my.order[i]
		}
		names(tmp.df) <- c("Matrix_TF", "Order")
		return(tmp.df)
	}

	if (return.only.orders) {
		tmp.list <- lapply(my.matrices, splineMatrix_equality)
		tmp.orders <- as.numeric(subset(tmp.list[sapply(tmp.list, function(x) any(x[['Matrix_TF']]))][1][[1]], Matrix_TF==TRUE, select=Order))
		return(tmp.orders)
	} else {
		if (is.null(my.matrix.order)) my.matrix.order <- length(my.matrix.time.progression.lags)
		my.tmp.index <- which(sapply(lapply(my.matrices, splineMatrix_equality, my.order=my.matrix.order), function(x) x[['Matrix_TF']]))
		if (length(my.tmp.index)==0) {
			stop(paste("\tNOTE: No splineMatrix exists with designated content.area.progression:", my.matrix.content.area.progression, "year.progression:", 
				my.matrix.time.progression, "and grade.progression", my.matrix.grade.progression))
		} else {
			return(my.matrices[[my.tmp.index]])
		}
	}
} ### END getsplineMatrix
