`checkSGP` <-
function(sgp_object, 
	state=NULL) {

	ID <- NULL

	### Check if sgp_object is of class SGP

	if (!is.SGP(sgp_object)) stop("NOTE: Check SGP accepts only objects of class SGP. See manual pages for details.")


	### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "checkSGP")
        }

	my.character.variables <- c("ID", "VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ACHIEVEMENT_LEVEL")
	my.numeric.variables <- c("SCALE_SCORE", "SCALE_SCORE_PRIOR")
	my.Date.variables <- c("DATE")


	###
	### Utility functions
	###

	## checkVariableClass

	checkVariableClass <- function(my.data, check.class, my.variables.to.check, id.only=TRUE) {
		if (id.only){
			return("ID" %in% names(my.data) && !is.character(my.data[["ID"]]))
		} else {
			if (check.class=="character") {
				return(sapply(my.variables.to.check, function(x) x %in% names(my.data) && !is.character(my.data[[x]]), USE.NAMES=FALSE))
			}
			if (check.class=="numeric") {
				return(sapply(my.variables.to.check, function(x) x %in% names(my.data) && !is.double(my.data[[x]]), USE.NAMES=FALSE))
			}
			if (check.class=="Date") {
				return(sapply(my.variables.to.check, function(x) x %in% names(my.data) && !inherits(my.data[[x]], "Date"), USE.NAMES=FALSE))
			}
		}
	}

	## changeVariableClass

	changeVariableClass <- function(my.data, my.variables.to.change, data.slot, convert.to.class) {
		if (!data.slot=="@Data" & !data.slot=="@Data_Supplementary") {
			message(paste("\tNOTE: ID in", data.slot, "converted from factor to character."))
			my.data[,ID:=as.character(my.data[["ID"]])]			
		} else {
			if (convert.to.class=="character") {
				for (my.variable in my.variables.to.change) {
					message(paste("\tNOTE:", my.variable, "in", data.slot, "converted from", paste(class(my.data[[my.variable]]), collapse=" "), "to character."))
					my.data[,my.variable:=as.character(my.data[[my.variable]]), with=FALSE]
				}
			}
			if (convert.to.class=="numeric") {
				for (my.variable in my.variables.to.change) {
					message(paste("\tNOTE:", my.variable, "in", data.slot, "converted from", class(my.data[[my.variable]]), "to numeric."))
					my.data[,my.variable:=as.numeric(my.data[[my.variable]]), with=FALSE]
				}
			}
			if (convert.to.class=="Date") {
				for (my.variable in my.variables.to.change) {
					message(paste("\tNOTE:", my.variable, "in", data.slot, "converted from", class(my.data[[my.variable]]), "to Date assuming XXXX-XX-XX format."))
					my.data[,my.variable:=as.Date(my.data[[my.variable]]), with=FALSE]
				}
			}
		}
		return(my.data)
	}


	###########################################
	###
	### Perform checks
	###
	###########################################

	## Check class of variables in @Data

	if (any(tmp.check <- checkVariableClass(sgp_object@Data, "character", my.character.variables, id.only=FALSE))) {
		sgp_object@Data <- changeVariableClass(sgp_object@Data, my.character.variables[tmp.check], data.slot="@Data", convert.to.class="character")
	}

	if (any(tmp.check <- checkVariableClass(sgp_object@Data, "numeric", my.numeric.variables, id.only=FALSE))) {
		sgp_object@Data <- changeVariableClass(sgp_object@Data, my.numeric.variables[tmp.check], data.slot="@Data", convert.to.class="numeric")
	}

	if (any(tmp.check <- checkVariableClass(sgp_object@Data, "Date", my.Date.variables, id.only=FALSE))) {
		sgp_object@Data <- changeVariableClass(sgp_object@Data, my.Date.variables[tmp.check], data.slot="@Data", convert.to.class="Date")
	}

	## Check class of variables in @Data_Supplementary
	
	if (!is.null(sgp_object@Data_Supplementary)) {
		for(j in 1:length(sgp_object@Data_Supplementary)) {
			if (any(tmp.check <- checkVariableClass(sgp_object@Data_Supplementary[[j]], "character", my.character.variables, id.only=FALSE))) {
				sgp_object@Data_Supplementary[[j]] <- 
					changeVariableClass(sgp_object@Data_Supplementary[[j]], my.character.variables[tmp.check], data.slot="@Data_Supplementary", convert.to.class="character")
			}
			if (any(tmp.check <- checkVariableClass(sgp_object@Data_Supplementary[[j]], "numeric", my.numeric.variables, id.only=FALSE))) {
				sgp_object@Data_Supplementary[[j]] <- 
					changeVariableClass(sgp_object@Data_Supplementary[[j]], my.numeric.variables[tmp.check], data.slot="@Data_Supplementary", convert.to.class="numeric")
			}
		}
	}

	## Check class and construction of coefficient matrices

	if (!is.null(sgp_object@SGP[["Coefficient_Matrices"]])) {
		sgp_object@SGP[["Coefficient_Matrices"]] <- checksplineMatrix(sgp_object@SGP[["Coefficient_Matrices"]], sgp_object, state)
	}

	## Check class of variables in @SGP$SGPercentiles and @SGP$SGProjections

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

	## Check if ACHIEVEMENT_LEVEL levels are in SGPstateData

	if (!is.null(state) && "ACHIEVEMENT_LEVEL" %in% names(sgp_object@Data)) {
		if (!is.null(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]])) {
			tmp.index <- grep("Achievement_Levels", names(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]]))
			achievement.levels <- sort(unique(unlist(sapply(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][tmp.index], function(x) x[['Labels']]))))
		} else {
			achievement.levels <- SGP::SGPstateData[[state]][['Achievement']][['Levels']][['Labels']]
		}
		if (!all(sort(unique(sgp_object@Data$ACHIEVEMENT_LEVEL)) %in% achievement.levels)) {
			missing.achievement.levels <- 
				sort(unique(sgp_object@Data$ACHIEVEMENT_LEVEL))[!sort(unique(sgp_object@Data$ACHIEVEMENT_LEVEL)) %in% achievement.levels]
			message(paste("\tNOTE: Achievement level(s):", paste(missing.achievement.levels, collapse=", "), "in supplied data are not contained in 'SGPstateData'.", collapse=" "))
		}
	}

	## Correct SCALE_SCORE_PRIOR/PRIOR_SCALE_SCORE mixup

	if ("PRIOR_SCALE_SCORE" %in% names(sgp_object@Data)) {
		message("\tNOTE: Changing name 'PRIOR_SCALE_SCORE' to 'SCALE_SCORE_PRIOR' in @Data")
		setnames(sgp_object@Data, "PRIOR_SCALE_SCORE", "SCALE_SCORE_PRIOR")
	}

	for (i in names(sgp_object@SGP[['SGPercentiles']])) {
		if ("PRIOR_SCALE_SCORE" %in% names(sgp_object@SGP[['SGPercentiles']][[i]])) {
			message(paste("\tNOTE: Changing name 'PRIOR_SCALE_SCORE' to 'SCALE_SCORE_PRIOR' in", i, "table of '@SGP$SGPercentiles'"))
			names(sgp_object@SGP[['SGPercentiles']][[i]])[which(names(sgp_object@SGP[['SGPercentiles']][[i]])=="PRIOR_SCALE_SCORE")] <- "SCALE_SCORE_PRIOR"
		}
	}

	## Change SGP_TARGET names to indicate number of years

	names.to.change <- c("SGP_TARGET", "SGP_TARGET_BASELINE", "SGP_TARGET_MOVE_UP_STAY_UP", "SGP_TARGET_BASELINE_MOVE_UP_STAY_UP")
	for (i in intersect(names(sgp_object@Data), names.to.change)) {
		message(paste("\tNOTE: Changing name '", i, "' to '", paste(i, "3_YEAR", sep="_"), "' in @Data", sep=""))
		setnames(sgp_object@Data, i, paste(i, "3_YEAR", sep="_"))
	}

	## Add CURRENT to names of straight projection targets

	for (i in grep("LAGGED", names(sgp_object@SGP[['SGProjections']]), value=TRUE, invert=TRUE)) {
		tmp.names <- grep("YEAR", names(sgp_object@SGP[['SGProjections']][[i]]), value=TRUE)
		if (length(grep("CURRENT", tmp.names))!=length(tmp.names)) {
			setnames(sgp_object@SGP[['SGProjections']][[i]], tmp.names, paste(tmp.names, "CURRENT", sep="_"))
			message(paste("\tNOTE: Adding '_CURRENT' to non-lagged variable names in @SGP[['SGProjections']][['", i, "']]", sep=""))
		}
	}

	## Change table in SGPercentiles and SGProjections from data.frame to data.table

	for (i in names(sgp_object@SGP[['SGPercentiles']])) {
		if (!is.data.table(sgp_object@SGP[['SGPercentiles']][[i]])) sgp_object@SGP[['SGPercentiles']][[i]] <- as.data.table(sgp_object@SGP[['SGPercentiles']][[i]])
	}

	for (i in names(sgp_object@SGP[['SGProjections']])) {
		if (!is.data.table(sgp_object@SGP[['SGProjections']][[i]])) sgp_object@SGP[['SGProjections']][[i]] <- as.data.table(sgp_object@SGP[['SGProjections']][[i]])
	}

	## Return sgp_object	

	return(sgp_object)
} ### END sgp_object
