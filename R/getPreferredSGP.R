`getPreferredSGP` <-
function(tmp.data,
	state,
	type="COHORT_REFERENCED") {

	YEAR <- SGP_NORM_GROUP <- VALID_CASE <- CONTENT_AREA <- ID <- PREFERENCE <- NULL

	if (type=="TARGET") {
		tmp.sgp.norm.group.variables <- c("SGP_PROJECTION_GROUP", "PREFERENCE")
	}
	if (type=="BASELINE") {
		tmp.sgp.norm.group.variables <- c("YEAR", "SGP_NORM_GROUP_BASELINE", "PREFERENCE")
		tmp.message <- "\tNOTE: Multiple Baseline SGPs exist for individual students. Unique Baseline SGPs will be created using SGP Norm Group Preference Table for "
	}
	if (type=="COHORT_REFERENCED") {
		tmp.sgp.norm.group.variables <- c("YEAR", "SGP_NORM_GROUP", "PREFERENCE")
		tmp.message <- "\tNOTE: Multiple SGPs exist for individual students. Unique SGPs will be created using SGP Norm Group Preference Table for "
	}

	if (type=="TARGET") {
		if (!is.null(SGP::SGPstateData[[state]][['SGP_Progression_Preference']])) {
			setkeyv(SGP::SGPstateData[[state]][['SGP_Progression_Preference']], tmp.sgp.norm.group.variables)
			setkeyv(tmp.data, tmp.sgp.norm.group.variables[1])
		} else {
			stop("\tNOTE: Multiple Projections exist for individual students. Please examine results in @SGP[['SGProjections']].")
		}
		tmp.data <- data.table(SGP::SGPstateData[[state]][['SGP_Progression_Preference']][,tmp.sgp.norm.group.variables,with=FALSE][tmp.data],
			key=c(getKey(tmp.data), "PREFERENCE"))
	} else {
		if (!is.null(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']])) {
			message(paste(tmp.message, state, ".", sep=""))
			data.norm.groups <- unique(as.character(tmp.data[which(duplicated(tmp.data, by=key(tmp.data))), SGP_NORM_GROUP]))
			metadata.norm.groups <- unique(as.character(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']][[tmp.sgp.norm.group.variables[2]]]))
			if(!all(data.norm.groups %in% metadata.norm.groups)) {
				missing.norm.groups <- data.norm.groups[which(!(data.norm.groups %in% metadata.norm.groups))]
				stop(paste("\n\n\tERROR: Not all", tmp.sgp.norm.group.variables[2], "values present in SGPstateData SGP_Norm_Group_Preference object. Missing Norm Group(s):\n\t\t"), paste(missing.norm.groups, collapse="\n\t\t"))
			}
			setkeyv(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']], tmp.sgp.norm.group.variables[1:2])
			setkeyv(tmp.data, tmp.sgp.norm.group.variables[1:2])
		} else {
			stop("\tNOTE: Multiple SGPs exist for individual students. Please examine results in @SGP[['SGPercentiles']].  A SGP_Norm_Group_Preference entry in SGPstateData may be required.")
		}
		tmp.data <- data.table(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']][,tmp.sgp.norm.group.variables,with=FALSE][tmp.data],
			key=c(getKey(tmp.data), "PREFERENCE"))
	}

#	if (length(sgp_order_variables <- sort(grep("SGP_ORDER_[123456789]", names(tmp.data), value=TRUE))) > 1) {
#		setkeyv(tmp.data, getKey(tmp.data))
#		my.fill.table <- tmp.data[tmp.data[duplicated(tmp.data, by=key(tmp.data))][,key(tmp.data), with=FALSE]][,lapply(.SD, mean, na.rm=TRUE), keyby=key(tmp.data), .SDcols=sgp_order_variables]
#		for (order.iter in seq(length(sgp_order_variables)-1)) {
#			which.to.fill <- tmp.data[is.na(get(sgp_order_variables[order.iter])) & !is.na(SGP)][,key(tmp.data), with=FALSE]
#			tmp.data[which.to.fill, sgp_order_variables[order.iter]:=tmp.data[my.fill.table[which.to.fill], c(key(tmp.data), sgp_order_variables), with=FALSE][[sgp_order_variables[order.iter]]]]
#		}
#	}

	setkeyv(tmp.data, getKey(tmp.data))
	tmp.data <- tmp.data[!duplicated(tmp.data, by=key(tmp.data))][,PREFERENCE:=NULL]
	return(tmp.data)
} ### END getPreferredSGP
