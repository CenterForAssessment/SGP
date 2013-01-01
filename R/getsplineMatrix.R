`getsplineMatrix` <- 
function(
	my.matrices,
	my.matrix.content.area.progression,
	my.matrix.grade.progression,
	my.matrix.time.progression,
	my.matrix.time.progression.lags,
	my.matrix.order=NULL,
	what.to.return="MATRICES") {

	Matrix_TF <- Order <- Grade <- NULL

	splineMatrix_equality <- function(my.matrix, my.order=NULL) {
		tmp.df <- data.frame()
		if (is.null(my.order)) my.order <- (2:length(my.matrix.time.progression))-1
		for (i in seq_along(my.order)) {
			tmp.df[i,1] <- identical(my.matrix@Content_Areas[[1]], tail(my.matrix.content.area.progression, my.order[i]+1)) & 
					identical(my.matrix@Grade_Progression[[1]], as.character(tail(my.matrix.grade.progression, my.order[i]+1))) & 
					identical(my.matrix@Time[[1]], as.character(tail(my.matrix.time.progression, my.order[i]+1))) &
					identical(my.matrix@Time_Lags[[1]], as.integer(tail(my.matrix.time.progression.lags, my.order[i])))
			tmp.df[i,2] <-	my.order[i]
			tmp.df[i,3] <- tail(my.matrix@Grade_Progression[[1]], 1)
		}
		names(tmp.df) <- c("Matrix_TF", "Order", "Grade")
		return(tmp.df)
	}

	if (what.to.return=="ORDERS") {
		tmp.list <- lapply(my.matrices, splineMatrix_equality)
		tmp.orders <-  as.numeric(unlist(sapply(tmp.list[sapply(tmp.list, function(x) any(x[['Matrix_TF']]))], subset, Matrix_TF==TRUE, select=Order)))
		return(tmp.orders)
	}
	if (what.to.return=="GRADES") {
		tmp.list <- lapply(my.matrices, splineMatrix_equality)
		tmp.grades <-  as.numeric(unlist(sapply(tmp.list[sapply(tmp.list, function(x) any(x[['Matrix_TF']]))], subset, Matrix_TF==TRUE, select=Grade)))
		return(tmp.grades)
	}
	if (what.to.return=="MATRICES") {
		if (is.null(my.matrix.order)) my.matrix.order <- length(my.matrix.time.progression.lags)
		my.tmp.index <- which(sapply(lapply(my.matrices, splineMatrix_equality, my.order=my.matrix.order), function(x) x[['Matrix_TF']]))
		if (length(my.tmp.index)==0) {
			stop(paste("\tNOTE: No splineMatrix exists with designated content.area.progression:", my.matrix.content.area.progression, "year.progression:", 
				my.matrix.time.progression, "and grade.progression", my.matrix.grade.progression))
		}
		if (length(my.tmp.index)>1) {
			stop(paste("\tNOTE: Multiple splineMatrix objects exists with designated content.area.progression:", my.matrix.content.area.progression, "year.progression:", 
				my.matrix.time.progression, "grade.progression:", my.matrix.grade.progression, "time.progression.lags:", my.matrix.time.progression.lags))
		} else {
			return(my.matrices[[my.tmp.index]])
		}
	}
} ### END getsplineMatrix
