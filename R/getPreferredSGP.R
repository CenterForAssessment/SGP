`getPreferredSGP` <-
function(tmp.data,
	state,
	type="COHORT_REFERENCED",
	dup.key=NULL) {

	YEAR <- SGP_NORM_GROUP <- VALID_CASE <- CONTENT_AREA <- ID <- PREFERENCE <- SGP_NOTE <- SGP_NOTE_TF <- NULL

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
	if (type=="EQUATED") {
		tmp.sgp.norm.group.variables <- c("YEAR", "SGP_NORM_GROUP_EQUATED", "PREFERENCE")
		tmp.message <- "\tNOTE: Multiple Equated SGPs exist for individual students. Unique Equated SGPs will be created using SGP Norm Group Preference Table for "
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
		if ("SGP_NOTE" %in% names(tmp.data)) { # Key data on SGP_NOTE to put existing SGPs at the top, regardless of preference of SGP_NOTE's norm group.
			tmp.key.vars <- c("SGP_NOTE_TF", "PREFERENCE")
			invisible(tmp.data[, SGP_NOTE_TF := !is.na(SGP_NOTE)])
		} else tmp.key.vars <- "PREFERENCE"

		if (any(grepl("^PREFERENCE$", names(tmp.data)))) {
			#  Order data using key
		  setkeyv(tmp.data, c(getKey(tmp.data), tmp.key.vars))
		  if (is.null(dup.key)) dup.key <- getKey(tmp.data)
		  setkeyv(tmp.data, dup.key)
			if (!is.null(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']])) {
				if (any(unique(tmp.data$YEAR) %in% unique(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']]$YEAR))) {
					new.dups <- tmp.data[!is.na(PREFERENCE),]
					new.dups <- new.dups[!duplicated(new.dups, by=key(tmp.data))]
					tmp.data <- rbindlist(list(tmp.data[is.na(PREFERENCE),], new.dups))[, PREFERENCE:=NULL]
					setkeyv(tmp.data, dup.key)
					ng.pref.obj.tf <- TRUE
				} else ng.pref.obj.tf <- FALSE
			} else ng.pref.obj.tf <- FALSE
			if (!ng.pref.obj.tf) {
			  tmp.data <- tmp.data[!duplicated(tmp.data, by=key(tmp.data))][, PREFERENCE:=NULL]
		  	if ("SGP_NOTE_TF" %in% names(tmp.data)) invisible(tmp.data[, SGP_NOTE_TF := NULL])
		  	return(tmp.data)
			}
		}
		if (!is.null(SGP::SGPstateData[[state]][['SGP_Norm_Group_Preference']])) {
			messageSGP(paste0(tmp.message, state, "."))
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
			key=c(getKey(tmp.data), tmp.key.vars))
	}

	if (is.null(dup.key)) dup.key <- getKey(tmp.data)

	setkeyv(tmp.data, dup.key)
	tmp.data <- tmp.data[!duplicated(tmp.data, by=key(tmp.data))][,PREFERENCE:=NULL]
	if ("SGP_NOTE" %in% names(tmp.data)) invisible(tmp.data[, SGP_NOTE_TF := NULL])
	return(tmp.data)
} ### END getPreferredSGP
