`prepareSGP` <- 
	function(data,
		state=NULL,
		var.names=NULL, 
		fix.duplicates="keep.all") {

	## Print start time

	started.at <- proc.time()
	message(paste("\nStarted prepareSGP", date()))

	VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- ID <- GRADE <- SCALE_SCORE <- DUPLICATED_CASES <- NULL ## To prevent R CMD check warnings

	## Get state (if possible)

		if (is.null(state)) {
			tmp.name <- gsub("_", " ", deparse(substitute(data)))
			if (any(sapply(c(state.name, "Demonstration", "sgpData LONG"), function(x) regexpr(x, tmp.name)))==1) {
				state <- c(state.abb, rep("DEMO", 2))[which(sapply(c(state.name, "Demonstration", "sgpData LONG"), function(x) regexpr(x, tmp.name))==1)]
			} else {
				state <- "TEMP"
			}
		}


	### Utility functions

	# achievement_level_recode (NOT YET IN USE)

	achievement_level_recode <- function(sgp_object, state=NULL, year=NULL, content_area=NULL, grade=NULL) {
		if (!"ACHIEVEMENT_LEVEL" %in% names(sgp_object@Data)) {
			sgp_object@Data[["ACHIEVEMENT_LEVEL"]]<- factor(1, levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]),
				labels=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])
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
						return(paste(content_area, rev(sort(tmp.cutscore.years))[1], sep="."))
					}
				}
			} else {
				return(content_area)
			}
		}

		achievement_level_recode_INTERNAL <- function(state, content_area, year, grade, scale_score) {
			factor(findInterval(scale_score, SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, year, content_area)]][[paste("GRADE_", grade, sep="")]])+1,
				levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]),
				labels=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])
		}

		setkeyv(data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
			sgp_object@Data[["ACHIEVEMENT_LEVEL"]][sgp_object@Data[CJ("VALID_CASE", content_area, year, grade), which=TRUE, nomatch=0]] <- 
			sgp_object@Data[CJ("VALID_CASE", content_area, year, grade), nomatch=0][, achievement_level_recode_INTERNAL(state, as.character(CONTENT_AREA), as.character(YEAR), GRADE, SCALE_SCORE), 
				by=list(CONTENT_AREA, YEAR, GRADE)][["V1"]]
		setkeyv(data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

		return(sgp_object)
	}

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
		for (my.list.label in unique(tmp.data[J("VALID_CASE")][["CONTENT_AREA"]])) {
			tmp.grade.list[[my.list.label]] <- unique(tmp.data[J("VALID_CASE", as.character(my.list.label))][["GRADE"]])
			for (j in seq_along(tmp.grade.list[[my.list.label]])) {
				tmp.list[[my.list.label]][[3*j-2]] <-
					round(as.vector(quantile(subset(tmp.data, VALID_CASE=="VALID_CASE" & GRADE==tmp.grade.list[[my.list.label]][j], select="SCALE_SCORE"), 
						probs=c(0.2,0.4,0.6,0.8), na.rm=TRUE)), digits=3)
				tmp.list[[my.list.label]][[3*j-1]] <-
					round(as.vector(extendrange(subset(tmp.data, VALID_CASE=="VALID_CASE" & GRADE==tmp.grade.list[[my.list.label]][j], select="SCALE_SCORE"), f=0.1)), digits=3)
				tmp.list[[my.list.label]][[3*j]] <-
					round(as.vector(extendrange(subset(tmp.data, VALID_CASE=="VALID_CASE" & GRADE==tmp.grade.list[[my.list.label]][j], select="SCALE_SCORE"), f=0.0)), digits=3)
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

		if (!is.null(data@SGP[["Coefficient_Matrices"]])) {
			for (i in names(data@SGP[["Coefficient_Matrices"]])) {
				splineMatrix.tf <- sapply(data@SGP[["Coefficient_Matrices"]][[i]], is.splineMatrix)
				if (!any(splineMatrix.tf)) {
					data@SGP[["Coefficient_Matrices"]][[i]][!splineMatrix.tf] <- 
						lapply(data@SGP[["Coefficient_Matrices"]][[i]][!splineMatrix.tf], function(x) as.splineMatrix(matrix=x, sgp_object=data))
				}
			}
		}

		if (!is.null(state) & is.null(var.names)) {
			if (!identical(data@Names, SGPstateData[[state]][["Variable_Name_Lookup"]])) data@Names <- SGPstateData[[state]][["Variable_Name_Lookup"]]
		}
		if (!is.null(var.names)) {
			data@Names <- getNames(data@Data, var.names)
		}

		if (!identical(key(data@Data), c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))) {
			setkeyv(data@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			if (any(duplicated(data@Data[J("VALID_CASE")]))) {
				message("\tWARNING: @Data keyed by 'VALID_CASE', 'CONTENT_AREA', 'YEAR', 'ID' has duplicate cases. Subsequent merges will likely be corrupt.")
				message("\tDuplicate cases are saved and available in current working environment as 'DUPLICATED_CASES'.")
				assign("DUPLICATED_CASES", data[data[J("VALID_CASE")][duplicated(data[J("VALID_CASE")])][,list(VALID_CASE, CONTENT_AREA, YEAR, ID)]], envir=globalenv())
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

		message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))
		return(data)
	} else {
		variable.names <- getNames(data, var.names)
	
		##  Create keyed data.table and check for duplicate cases

		data <- as.data.table(data)
		setnames(data, which(!is.na(variable.names$names.sgp)), variable.names$names.sgp[!is.na(variable.names$names.sgp)])
		setkeyv(data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		if (any(duplicated(data[J("VALID_CASE")]))) {
			message("\tWARNING: Data keyed by 'VALID_CASE', 'CONTENT_AREA', 'YEAR', 'ID' has duplicate cases. Subsequent merges will be corrupted.")
			message("\tDuplicate cases are saved and available in current working environment as 'DUPLICATED_CASES'.")
			assign("DUPLICATED_CASES", data[data[J("VALID_CASE")][duplicated(data[J("VALID_CASE")])][,list(VALID_CASE, CONTENT_AREA, YEAR, ID)]], envir=globalenv())
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

	
		##  Print finish time
		message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))

		return(sgp_object)
	} ## END else
} ## END prepareSGP function
