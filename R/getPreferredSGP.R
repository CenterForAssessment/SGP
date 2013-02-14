`getPreferredSGP` <- 
function(tmp.data,
	state,
	type="COHORT_REFERENCED") {

	YEAR <- SGP_NORM_GROUP <- VALID_CASE <- CONTENT_AREA <- ID <- PREFERENCE <- NULL

	if (type=="BASELINE") {
		tmp.sgp.norm.group.variables <- c("YEAR", "SGP_NORM_GROUP_BASELINE", "PREFERENCE")
	} else {
		tmp.sgp.norm.group.variables <- c("YEAR", "SGP_NORM_GROUP", "PREFERENCE")
	}

	if (!is.null(SGPstateData[[state]][['SGP_Norm_Group_Preference']])) {
		message(paste("\tNOTE: Multiple SGPs exist for individual students. Unique SGPs will be created using SGP Norm Group Preference Table for ", state, ".", sep=""))
		setkeyv(SGPstateData[[state]][['SGP_Norm_Group_Preference']], tmp.sgp.norm.group.variables[1:2])
		setkeyv(tmp.data, tmp.sgp.norm.group.variables[1:2])
	} else {
		stop("\tNOTE: Multiple SGPs exist for individual students. Please examine results in @SGP[['SGPercentiles']].")
	}
	tmp.data <- data.table(SGPstateData[[state]][['SGP_Norm_Group_Preference']][,tmp.sgp.norm.group.variables,with=FALSE][tmp.data], 
		key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "PREFERENCE"))
	setkey(tmp.data, VALID_CASE, CONTENT_AREA, YEAR, ID)
	tmp.data[!duplicated(tmp.data)][,PREFERENCE:=NULL]
	setkeyv(tmp.data, getKey(tmp.data))
	return(tmp.data)
} ### END getPreferredSGP
