`unique.splineMatrix` <- 
function(list.of.splineMatrices) {
	tmp.list.1 <- lapply(list.of.splineMatrices, 
		function(x) list(Data = x@.Data, Content_Areas=x@Content_Areas, Grade_Progression=x@Grade_Progression, Time=x@Time, Time_Lags=x@Time_Lags))
	tmp.list.2 <- lapply(list.of.splineMatrices, 
		function(x) list(Data = x@.Data, Content_Areas=x@Content_Areas, Grade_Progression=x@Grade_Progression, Time=x@Time, Time_Lags=x@Time_Lags, Version=x@Version))
	if (any(duplicated(tmp.list.1))) {
		list.of.splineMatrices[!duplicated(tmp.list.1[order(as.character(unlist(sapply(tmp.list.2, function(x) x$Version[1]))), decreasing=TRUE)])]
	} else {
		list.of.splineMatrices
	}
} ### END unique.splineMatrix
