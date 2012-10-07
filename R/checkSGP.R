`checkSGP` <-
function(sgp_object, state=NULL) {

	### Check if sgp_object is of class SGP

	if (!is.SGP(sgp_object)) stop("NOTE: Check SGP accepts only objects of class SGP. See manual pages for details.")


	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		if (any(sapply(c(state.name, "Demonstration", "sgpData LONG", "AOB"), function(x) regexpr(toupper(x), tmp.name)))!=1) {
			state <- c(state.abb, rep("DEMO", 2), "AOB")[which(sapply(c(state.name, "Demonstration", "sgpData LONG", "AOB"), function(x) regexpr(toupper(x), tmp.name))!=1)[1]]
		}
	}

	###
	### Utility functions
	###

	## checkVariableClass

	checkVariableClass <- function(my.data, id.only=TRUE) {
		if (id.only){
			return("ID" %in% names(my.data) && !is.character(my.data[["ID"]]))
		} else {
			tmp.vars <- c("ID", "VALID_CASE", "CONTENT_AREA", "YEAR")
			return(sapply(tmp.vars, function(x) x %in% names(my.data) && !is.character(my.data[[x]]), USE.NAMES=FALSE))
		}
	}

	## checkchangeVariableClassVariableClass

	changeVariableClass <- function(my.data, convert.tf, data.slot) {
		if (!data.slot=="@Data") {
			message(paste("\tNOTE: ID in", data.slot, "converted from class factor to character to accomodate data.table >= 1.8.0 changes."))
			my.data[["ID"]] <- as.character(my.data[["ID"]])			
		} else {
			for (my.variable in c("ID", "VALID_CASE", "CONTENT_AREA", "YEAR")[convert.tf]) {
				message(paste("\tNOTE:", my.variable, "in", data.slot, "converted from class factor to character to accomodate data.table >= 1.8.0 changes."))
				my.data[[my.variable]] <- as.character(my.data[[my.variable]])
			}
		}
		return(my.data)
	}

	###########################################
	###
	### Perform checks
	###
	###########################################

	### Check class of variables in @Data

	if (any(tmp.check <- checkVariableClass(sgp_object@Data, id.only=FALSE))) {
		sgp_object@Data <- changeVariableClass(sgp_object@Data, convert.tf=tmp.check, data.slot="@Data")
	}

	### Check class of variables in @SGP$SGPercentiles and @SGP$SGProjections

	if (any(SGPctls.tf <- sapply(sgp_object@SGP[['SGPercentiles']], checkVariableClass))) {
		tmp.data <- sgp_object@SGP[['SGPercentiles']]
		for (i in which(SGPctls.tf)) {
			tmp.data[[i]] <- changeVariableClass(tmp.data[[i]], data.slot=paste('SGPercentiles', names(sgp_object@SGP[['SGPercentiles']])[i]))
		}
		tmp.data -> sgp_object@SGP[['SGPercentiles']]
	}

	if (any(SGPrjns.tf <- sapply(sgp_object@SGP[['SGProjections']], checkVariableClass))) {
		tmp.data <- sgp_object@SGP[['SGProjections']]
		for (i in which(SGPrjns.tf)) {
			tmp.data[[i]] <- changeVariableClass(tmp.data[[i]], data.slot=paste('SGProjections', names(sgp_object@SGP[['SGProjections']])[i]))
		}
		tmp.data -> sgp_object@SGP[['SGProjections']]
	}

	### Check if ACHIEVEMENT_LEVEL levels are in SGPstateData

	if (!is.null(state)) {
		if (!all(levels(sgp_object@Data$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']])) {
			missing.achievement.levels <- 
				levels(sgp_object@Data$ACHIEVEMENT_LEVEL)[!levels(sgp_object@Data$ACHIEVEMENT_LEVEL) %in% SGPstateData[[state]][['Achievement']][['Levels']][['Labels']]]
			message(paste("\tNOTE: Achievement level(s):", missing.achievement.levels, "in supplied data are not contained in 'SGPstateData'."))
		}
	}

	### Correct SCALE_SCORE_PRIOR/PRIOR_SCALE_SCORE mixup

	if ("PRIOR_SCALE_SCORE" %in% names(sgp_object@Data)) {
		message("\tNOTE: Changing name 'PRIOR_SCALE_SCORE' to 'SCALE_SCORE_PRIOR' in @Data")
		setnames(sgp_object@Data, "PRIOR_SCALE_SCORE", "SCALE_SCORE_PRIOR")
	}

	for (i in names(sgp_object@SGP[['SGPercentiles']])) {
		if ("PRIOR_SCALE_SCORE" %in% names(sgp_object@SGP[['SGPercentiles']][[i]])) {
			names(sgp_object@SGP[['SGPercentiles']][[i]])[which(names(sgp_object@SGP[['SGPercentiles']][[i]])=="PRIOR_SCALE_SCORE")] <- "SCALE_SCORE_PRIOR"
		}
	}

	### Return sgp_object	

	return(sgp_object)

} ### END sgp_object
