`getPreferredSGP` <- function(tmp.data, state) {
	if (!is.null(SGPstateData[[state]][['SGP_Norm_Group_Preference']]])) {
		message("\tNOTE: Multiple SGPs exist for individual students. Unique SGPs will be created using SGP Norm Group Preference Table.")
	} else {
		stop("\tNOTE: Multiple SGPs exist for individual students. Please examine results in @SGP[['SGPercentiles']].")
	}
	setkey(tmp.data, YEAR, SGP_NORM_GROUP)
	tmp.data <- data.table(tmp.data[SGPstateData[[state]][['SGP_Norm_Group_Preference']]], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "PREFERENCE"))
	setkey(tmp.data, VALID_CASE, CONTENT_AREA, YEAR, ID)
	tmp.data[!duplicated(tmp.data), -dim(tmp.data)[2], with=FALSE]
} ### END getPreferredSGP
