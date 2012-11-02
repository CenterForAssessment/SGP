`prepareSGP` <- 
	function(data,
		state=NULL,
		var.names=NULL,
		create.additional.variables=TRUE,
		fix.duplicates="keep.all") {

	## Print start time

	started.at <- proc.time()
	message(paste("\nStarted prepareSGP", date()))

	VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- ID <- GRADE <- SCALE_SCORE <- DUPLICATED_CASES <- PRIOR_SCALE_SCORE <- PRIOR_GRADE <- SCHOOL_NUMBER <- YEAR_INT <- NULL

	## Get state (if possible)

		if (is.null(state)) {
			tmp.name <- toupper(gsub("_", " ", deparse(substitute(data))))
			state <- getStateAbbreviation(tmp.name, "prepareSGP")
		}


	### Utility functions

	# achievement_level_recode

	achievement_level_recode <- function(sgp_object, state=NULL, year=NULL, content_area=NULL, grade=NULL) {
		if (!"ACHIEVEMENT_LEVEL" %in% names(sgp_object@Data)) {
			sgp_object@Data[["ACHIEVEMENT_LEVEL"]] <- 
				factor(1, levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])]),
				labels=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])], ordered=TRUE)
		}

		if (is.null(year)) year <- sort(unique(sgp_object@Data$YEAR))
		if (is.null(content_area)) content_area <- sort(unique(sgp_object@Data$CONTENT_AREA[sgp_object@Data$YEAR %in% year]))
		if (is.null(grade)) grade <- sort(unique(sgp_object@Data$GRADE[sgp_object@Data$YEAR %in% year & sgp_object@Data$CONTENT_AREA %in% content_area]))

		get.cutscore.label <- function(state, year, content_area) {
			tmp.cutscore.names <- names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])
			tmp.cutscore.years <- sapply(strsplit(tmp.cutscore.names[grep(content_area, tmp.cutscore.names)], "[.]"), function(x) x[2])
			if (any(!is.na(tmp.cutscore.years))) {
				if (year %in% tmp.cutscore.years) {
					return(paste(content_area, year, sep="."))
				} else {
					if (year==sort(c(year, tmp.cutscore.years))[1]) {
						return(content_area)
					} else {
						return(paste(content_area, sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1], sep="."))
					}
				}
			} else {
				return(content_area)
			}
		}

		achievement_level_recode_INTERNAL <- function(state, content_area, year, grade, scale_score) {
			factor(findInterval(scale_score, SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, year, content_area)]][[paste("GRADE_", grade, sep="")]])+1,
				levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])]),
				labels=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])], ordered=TRUE)
		}

		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
			sgp_object@Data[["ACHIEVEMENT_LEVEL"]][sgp_object@Data[CJ("VALID_CASE", content_area, year, grade), which=TRUE, nomatch=0]] <- 
			sgp_object@Data[CJ("VALID_CASE", content_area, year, grade), nomatch=0][, achievement_level_recode_INTERNAL(state, CONTENT_AREA, YEAR, GRADE, SCALE_SCORE), 
				by=list(CONTENT_AREA, YEAR, GRADE)][["V1"]]
		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

		return(sgp_object)
	}

	# Function producing HIGH_NEED_STATUS variable (not yet in use)

	my.quantile.function <- function(x, invalid_cases, quantiles=c(0.25, 0.75)) {
		high.needs.status.labels <- c(paste("High Needs Status: Prior Achievement Below ", 100*quantiles[1], "th Percentile", sep=""),
			NA, paste("High Needs Status: Prior Achievement Above ", 100*quantiles[2], "th Percentiles", sep=""))
		if (invalid_cases) {
			return(factor(rep(NA, length(x)), levels=1:2, labels=high.needs.status.labels[c(1,3)]))
		}
		if (all(is.na(x))) {
			return(factor(rep(NA, length(x)), levels=1:2, labels=high.needs.status.labels[c(1,3)]))
		} else {
			my.quantiles <- quantile(x, probs=c(0, quantiles, 1), na.rm=TRUE)
			if (any(diff(quantile(x, probs=c(0, quantiles, 1), na.rm=TRUE))==0)) {
				return(factor(rep(NA, length(x)), levels=1:2, labels=high.needs.status.labels[c(1,3)]))
			} else {
				return(droplevels(cut(x, quantile(x, probs=c(0, quantiles, 1), na.rm=TRUE), include.lowest=TRUE, labels=high.needs.status.labels)))
			}
		}
	} ### END my.quantile.function

	## getNames

	getNames <- function(data, var.names) {

		## Get the names of the provided variables and create data frame

		names.provided <- names(data)
		variable.names.provided <- data.frame(
			column.provided=seq_along(names.provided), 
			names.provided=names.provided,
			stringsAsFactors=FALSE)

		## Required variables and default data frame

		required.names <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "VALID_CASE")

		default.variable.names <- data.frame(
			names.provided=required.names, 
			names.sgp=required.names, 
			names.type=SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp.type[match(required.names, SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp)], 
			names.info=SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp.info[match(required.names, SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp)], 
			names.output=SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp.output[match(required.names, SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp)], 
			stringsAsFactors=FALSE)

		## Check names.provided

		if (!is.null(var.names)) {
			if (!class(var.names) %in% c("list", "data.frame")) {
				stop("Supplied argument to 'var.names' must be of class list or data.frame. Please supply argument of appropriate class.")
			}
			if (identical("data.frame", class(var.names))) {
				if (!identical(names(var.names), c("names.provided", "names.sgp", "names.type", "names.info", "names.output"))) {
					stop("Supplied data.frame to arugment 'var.names' does not include all required columns: 'names.provided', 'names.sgp', 'names.type', 'names.info', 'names.output'")
				}
			}
			if (identical("list", class(var.names))) {
				var.names <- data.frame(
					names.provided=unlist(var.names), 
					names.sgp=toupper(names(var.names)), 
					names.type=SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp.type[
						match(toupper(names(var.names)), SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp)], 
					names.info=SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp.info[
						match(toupper(names(var.names)), SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp)], 
					names.output=SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp.output[
						match(toupper(names(var.names)), SGPstateData[["DEMO"]][["Variable_Name_Lookup"]]$names.sgp)], 
					stringsAsFactors=FALSE, row.names=NULL)
			}

			## Include default variable names (as needed)

			tmp.names <- default.variable.names[!(default.variable.names[["names.sgp"]] %in% var.names[["names.sgp"]]),]
			variable.names <- rbind(var.names, tmp.names)
		} else {
			if (state=="DEMO" | is.null(SGPstateData[[state]][["Variable_Name_Lookup"]])) {
				variable.names <- default.variable.names
			} else {
				variable.names <- SGPstateData[[state]][["Variable_Name_Lookup"]]
			}
		}

		## Compile the provided variable names and the corresponding (capitalized) variable names used in SGP

		variable.names <- merge(variable.names.provided, variable.names, by.x="names.provided", by.y="names.provided", all.x=TRUE)
		variable.names <- subset(variable.names, select=c("names.provided", "names.sgp", "names.type", "names.info", "names.output", "column.provided"))
		variable.names <- merge(variable.names, SGPstateData[["DEMO"]][["Variable_Name_Lookup"]], by.x="names.provided", by.y="names.sgp", all.x=TRUE)
		variable.names[is.na(variable.names$names.type) & !is.na(variable.names$names.sgp.type),][,c("names.sgp", "names.type", "names.info", "names.output")] <- 
			variable.names[is.na(variable.names$names.type) & !is.na(variable.names$names.sgp.type),][,c("names.provided", "names.sgp.type", "names.sgp.info", "names.sgp.output")]
		variable.names$names.sgp.type <- variable.names$names.sgp.info <- variable.names$names.sgp.output <- NULL
	
		## Check see if any of the required variables are missing

		if (!all(required.names %in% variable.names$names.sgp)) {
			stop(paste("\tThe {data} object is missing the following column name: ", required.names[(required.names %in% variable.names$names.sgp)==FALSE],
			". Please identify the variable using the {var.names} argument.", sep=""))
		}
		return(data.frame(variable.names[order(variable.names$column.provided),][,c("names.provided", "names.sgp", "names.type", "names.info", "names.output")], row.names=NULL, stringsAsFactors=FALSE))
	} ## END getNames

	## create.knots.boundaries

	create.knots.boundaries <- function(tmp.data) {
		tmp.grade.list <- tmp.list <- list()
		for (my.list.label in unique(tmp.data["VALID_CASE"][["CONTENT_AREA"]])) {
			tmp.grade.list[[my.list.label]] <- unique(tmp.data[SJ("VALID_CASE", my.list.label)][["GRADE"]])
			for (j in seq_along(tmp.grade.list[[my.list.label]])) {
				tmp.list[[my.list.label]][[3*j-2]] <-
					round(as.vector(quantile(subset(tmp.data, VALID_CASE=="VALID_CASE" & CONTENT_AREA==my.list.label & GRADE==tmp.grade.list[[my.list.label]][j], select="SCALE_SCORE"), 
						probs=c(0.2,0.4,0.6,0.8), na.rm=TRUE)), digits=3)
				tmp.list[[my.list.label]][[3*j-1]] <-
					round(as.vector(extendrange(subset(tmp.data, VALID_CASE=="VALID_CASE" & CONTENT_AREA==my.list.label & GRADE==tmp.grade.list[[my.list.label]][j], select="SCALE_SCORE"), f=0.1)), digits=3)
				tmp.list[[my.list.label]][[3*j]] <-
					round(as.vector(extendrange(subset(tmp.data, VALID_CASE=="VALID_CASE" & CONTENT_AREA==my.list.label & GRADE==tmp.grade.list[[my.list.label]][j], select="SCALE_SCORE"), f=0.0)), digits=3)
			}
			names(tmp.list[[my.list.label]]) <- paste(rep(c("knots_", "boundaries_", "loss.hoss_"), length(tmp.grade.list[[my.list.label]])), 
				rep(tmp.grade.list[[my.list.label]], each=3), sep="")
		}
		return(tmp.list)
	} ## END create.knots.boundaries

	## getVersion
	
	getVersion <- function(data) {
		if (is.SGP(data) & .hasSlot(data, "Version")) {
			return(list(SGP_Package_Version=c(data@Version[["SGP_Package_Version"]], as.character(packageVersion("SGP"))), 
				Date_Prepared=c(data@Version[["Date_Prepared"]], date())))
		} else {
			return(list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date()))
		}
	} ## END getVersion


	###################################################################
	###
	### prepare object depending upon whether it is of class SGP
	###
	###################################################################

	if (is.SGP(data)) {

		## Fix matrices if they aren't of splineMatrix class
		tmp.changes <- FALSE

		# Convert to splineMatrix class if not already
		if (!is.null(data@SGP[["Coefficient_Matrices"]])) {
			tmp.matrices <- data@SGP[["Coefficient_Matrices"]]
			for (i in names(tmp.matrices)) {
				splineMatrix.tf <- sapply(tmp.matrices[[i]], is.splineMatrix)
				if (!any(splineMatrix.tf)) {
					tmp.changes <- TRUE
					message("Updating Existing Coefficient Matrices to new splineMatrix class.")
					tmp.matrices[[i]][!splineMatrix.tf] <- 
						lapply(tmp.matrices[[i]][!splineMatrix.tf], function(x) as.splineMatrix(matrix=x, sgp_object=data))
				}
			}
			
			#  Attempt to populat Content_Areas and Grade_Progression slots if NULL according to @SGP$Coefficient_Matrices naming conventions
			for (j in names(tmp.matrices)) {
				j.txt <- paste("tmp.matrices[['", j, "']]", sep="")
				tmp.ca <- strsplit(j, "[.]")[[1]][1]
				official.gp <- SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[tmp.ca]]
				for (q in seq_along(names(eval(parse(text=j.txt))))) {
					qmat <- names(eval(parse(text=j.txt)))[q]
					grd <- as.numeric(strsplit(qmat, "_")[[1]][2]); priors <- as.numeric(strsplit(qmat, "_")[[1]][3])
					if (!is.null(official.gp) & grd %in% official.gp) {
						gp <- official.gp[(match(grd, official.gp)-priors):match(grd, official.gp)]
					} else {
						gp <- c(as.numeric(strsplit(strsplit(qmat, "_")[[1]][4], "[.]")[[1]]))
						if (length(gp)==0) {
							gp <- (grd-priors):grd 
						}
					}

					#  For splineMatrices created without Content_Areas, Grade_Progression or Version
					if (identical(class(try(eval(parse(text=paste(j.txt, "$", qmat, "@Content_Areas"))), silent=TRUE)), "try-error")) {
						eval(parse(text=paste(j.txt, "$", qmat, "@Content_Areas <- list(rep(tmp.ca, length(gp)))")))
						message("Updating new splineMatrix class Coefficient Matrices to include @Content_Areas slot."); tmp.changes <- TRUE
					} 
					if (identical(class(try(eval(parse(text=paste(j.txt, "$", qmat, "@Grade_Progression"))), silent=TRUE)), "try-error")) {
						eval(parse(text=paste(j.txt, "$", qmat, "@Grade_Progression <- list(gp)")))
						message("Updating new splineMatrix class Coefficient Matrices to include @Grade_Progression slot."); tmp.changes <- TRUE
					} 
					if (identical(class(try(eval(parse(text=paste(j.txt, "$", qmat, "@Version"))), silent=TRUE)), "try-error")) {
						eval(parse(text=paste(j.txt, "$", qmat, "@Version <- list(SGP_Package_Version=", paste("'Unknown - Converted with", 
							as.character(packageVersion("SGP")), "', ", sep=""), "Date_Prepared=", paste("'Unknown - Coverted,", date(), "')", sep=""), sep="")))
						tmp.changes <- TRUE
					}

					#  For recently converted (using as.splineMatrix above) splineMatrices
					if (is.null(eval(parse(text=paste(j.txt, "$", qmat, "@Content_Areas", sep=""))))) {
						eval(parse(text=paste(j.txt, "$", qmat, "@Content_Areas <- list(rep(tmp.ca, length(gp)))")))
						message("Updating new splineMatrix class Coefficient Matrices to include @Content_Areas slot."); tmp.changes <- TRUE
					} 
					if (is.null(eval(parse(text=paste(j.txt, "$", qmat, "@Grade_Progression"))))) {
						eval(parse(text=paste(j.txt, "$", qmat, "@Grade_Progression <- list(gp)")))
						message("Updating new splineMatrix class Coefficient Matrices to include @Grade_Progression slot."); tmp.changes <- TRUE
					} 
					
					#  For splineMatrices created with grade progression lables
					if (!is.na(strsplit(qmat, "_")[[1]][4])) {
						eval(parse(text=paste("names(", j.txt, ")[", q, "] <- paste(strsplit(qmat, '_')[[1]][-4], collapse='_')", sep="")))
						message("Renaming splineMatrix class Coefficient Matrices to EXCLUDE grade progression labels."); tmp.changes <- TRUE
					}
				}
			}
			if (tmp.changes) {
				data@SGP[["Coefficient_Matrices"]] <- tmp.matrices
			}
		}

		if (!is.null(state) & is.null(var.names)) {
			if (!identical(state, "DEMO") & !identical(data@Names, SGPstateData[[state]][["Variable_Name_Lookup"]])) {
				data@Names <- SGPstateData[[state]][["Variable_Name_Lookup"]]
			} 
			if (identical(state, "DEMO") & !identical(data@Names, SGPstateData[[state]][["Variable_Name_Lookup"]])) {
				data@Names <- getNames(sgpData_LONG, var.names)
			} 
		}

		if (!is.null(var.names)) {
			data@Names <- getNames(data@Data, var.names)
		}

		## Check class values of fields

		data <- checkSGP(data, state=state)

		if (!identical(key(data@Data), c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))) {
			setkeyv(data@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			if (any(duplicated(data@Data["VALID_CASE"]))) {
				message("\tWARNING: @Data keyed by 'VALID_CASE', 'CONTENT_AREA', 'YEAR', 'ID' has duplicate cases. Subsequent merges will likely be corrupt.")
				message("\tDuplicate cases are saved and available in current working environment as 'DUPLICATED_CASES'.")
				assign("DUPLICATED_CASES", data@Data["VALID_CASE"][duplicated(data@Data["VALID_CASE"])][,list(VALID_CASE, CONTENT_AREA, YEAR, ID)], envir=globalenv())
				assign("DUPLICATED_CASES", data@Data["VALID_CASE"][duplicated(data@Data["VALID_CASE"])][,list(VALID_CASE, CONTENT_AREA, YEAR, ID)])
				save(DUPLICATED_CASES, file="DUPLICATED_CASES.Rdata")
			}
		}

		if (is.null(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])) {
			SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]] <- create.knots.boundaries(data@Data)
			assign("SGPstateData", SGPstateData, envir=globalenv())
			assign(paste(state, "Knots_Boundaries", sep="_"), SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])
			save(list=paste(state, "Knots_Boundaries", sep="_"), file=paste(state, "Knots_Boundaries.Rdata", sep="_"))
			message(paste("\tKnots and Boundaries do not exist for state provided but have been produced, embedded in a working copy of SGPstateData (using state=", state, ") for subsequent analyses and saved to your working directory '", getwd(), "'.", sep=""))
		}

		data@Version <- getVersion(data)
		sgp_object <- data
	} else {
		variable.names <- getNames(data, var.names)

	
		##  Create keyed data.table and check for duplicate cases

		data <- as.data.table(data)
		setnames(data, which(!is.na(variable.names$names.sgp)), variable.names$names.sgp[!is.na(variable.names$names.sgp)])
		setkeyv(data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		if (any(duplicated(data["VALID_CASE"]))) {
			message("\tWARNING: Data keyed by 'VALID_CASE', 'CONTENT_AREA', 'YEAR', 'ID' has duplicate cases. Subsequent merges will be corrupted.")
			message("\tDuplicate cases are saved and available in current working environment as 'DUPLICATED_CASES'.")
			assign("DUPLICATED_CASES", data["VALID_CASE"][duplicated(data["VALID_CASE"])][,list(VALID_CASE, CONTENT_AREA, YEAR, ID)], envir=globalenv())
			assign("DUPLICATED_CASES", data["VALID_CASE"][duplicated(data["VALID_CASE"])][,list(VALID_CASE, CONTENT_AREA, YEAR, ID)])
			save(DUPLICATED_CASES, file="DUPLICATED_CASES.Rdata")
		}

		if (is.null(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])) {
			SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]] <- create.knots.boundaries(data)
			assign("SGPstateData", SGPstateData, envir=globalenv())
			assign(paste(state, "Knots_Boundaries", sep="_"), SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])
			save(list=paste(state, "Knots_Boundaries", sep="_"), file=paste(state, "Knots_Boundaries.Rdata", sep="_"))
			message(paste("\tKnots and Boundaries do not exist for state provided but have been produced, embedded in a working copy of SGPstateData (using state=", state, ") for subsequent analyses and saved to your working directory '", getwd(), "'.", sep=""))
		}


		################################################################	
		## INCLUDE CODE HERE TO HANDLE DUPLICATE CASES
		################################################################	

		##  Create the SGP object

		sgp_object <- new("SGP", Data=data, Names=variable.names, Version=getVersion(data))
		sgp_object <- checkSGP(sgp_object, state=state)

	} ## END else

	#################################################################
	###
	### Tidy up variables (could be validity checks)
	###
	#################################################################



	#################################################################
	###
	### Add additional variables
	###
	#################################################################

	## Create ACHIEVEMENT_LEVEL is it doesn't exist
	
	if (!"ACHIEVEMENT_LEVEL" %in% names(sgp_object@Data) & !is.null(SGPstateData[[state]][["Achievement"]][["Cutscores"]])) {
		sgp_object <- achievement_level_recode(sgp_object, state=state)
		message(paste("\tNOTE: Added variable ACHIEVEMENT_LEVEL to @Data using", state, "cutscores embedded in SGPstateData."))
	}

	if (create.additional.variables) {

		### HIGH_NEED_STATUS

		if (!"HIGH_NEED_STATUS" %in% names(sgp_object@Data) & "SCHOOL_NUMBER" %in% names(sgp_object@Data)) {
			sgp_object@Data[["YEAR_INT"]] <- as.integer(factor(sgp_object@Data[["YEAR"]]))
			setkeyv(sgp_object@Data, c("ID", "CONTENT_AREA", "YEAR_INT", "VALID_CASE")) ## CRITICAL that VALID_CASE is last in group
			sgp_object@Data$PRIOR_SCALE_SCORE <- sgp_object@Data[SJ(ID, CONTENT_AREA, YEAR_INT-1L), mult="last"][,SCALE_SCORE]
			sgp_object@Data$PRIOR_GRADE <- sgp_object@Data[SJ(ID, CONTENT_AREA, YEAR_INT-1L), mult="last"][,GRADE]

			setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR_INT", "SCHOOL_NUMBER", "PRIOR_GRADE", "ID"))
			sgp_object@Data[["HIGH_NEED_STATUS"]] <- sgp_object@Data[,my.quantile.function(PRIOR_SCALE_SCORE, !VALID_CASE[1]=="VALID_CASE"), 
				by=list(VALID_CASE, CONTENT_AREA, YEAR_INT, SCHOOL_NUMBER, PRIOR_GRADE)]$V1
			sgp_object@Data[["PRIOR_SCALE_SCORE"]] <- sgp_object@Data[["PRIOR_GRADE"]] <- sgp_object@Data[["YEAR_INT"]] <- NULL
			message("\tNOTE: Added variable HIGH_NEED_STATUS to @Data.")
			sgp_object@Names <- rbind(sgp_object@Names, c("HIGH_NEED_STATUS", "HIGH_NEED_STATUS", "demographic", "High need status flag", TRUE))
			setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
		}

	}

	##  Print finish time
	message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))

	return(sgp_object)
} ## END prepareSGP function
