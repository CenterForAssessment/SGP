`prepareSGP` <- 
	function(data, 
		var.names=NULL, 
		fix.duplicates="keep.all") {

	## Print start time

	started.at <- proc.time()
	message(paste("\nStarted prepareSGP", date()))

	VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- ID <- GRADE <- SCALE_SCORE <- NULL  ## To prevent R CMD check warnings

	### Utility functions

	achievement_level_recode <- function(sgp_object, state=NULL, year=NULL, content_area=NULL, grade=NULL) {

	        if (is.null(state)) {
	                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
	                if (any(sapply(c(state.name, "Demonstration", "sgpData LONG"), function(x) regexpr(x, tmp.name)))==1) {
	                        state <- c(state.abb, rep("DEMO", 2))[which(sapply(c(state.name, "Demonstration", "sgpData LONG"), function(x) regexpr(x, tmp.name))==1)]
	                }
	        }

                if (!"ACHIEVEMENT_LEVEL" %in% names(sgp_object@Data)) {
                        sgp_object@Data$ACHIEVEMENT_LEVEL <- factor(1, levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]),
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
	        sgp_object@Data$ACHIEVEMENT_LEVEL[sgp_object@Data[CJ("VALID_CASE", content_area, year, grade), which=TRUE, nomatch=0]] <- 
	        sgp_object@Data[CJ("VALID_CASE", content_area, year, grade), nomatch=0][, achievement_level_recode_INTERNAL(state, as.character(CONTENT_AREA), as.character(YEAR), GRADE, SCALE_SCORE), 
			by=list(CONTENT_AREA, YEAR, GRADE)]$V1
		setkeyv(data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

	        return(sgp_object)
	}


	if (is.SGP(data)) {
		if(!identical(key(data@Data), c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))) {
			setkeyv(data@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
		}
		if (.hasSlot(data, "Version")) {
			data@Version <- list(SGP_Package_Version=c(data@Version[["SGP_Package_Version"]], as.character(packageVersion("SGP"))), 
				Date_Prepared=c(data@Version[["Date_Prepared"]], date()))
		} else {
			data@Version <- list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date())
		}
		if (!is.null(data@SGP[["Coefficient_Matrices"]])) {
			for (i in names(data@SGP[["Coefficient_Matrices"]])) {
				splineMatrix.tf <- sapply(data@SGP[["Coefficient_Matrices"]][[i]], is.splineMatrix)
				if (!any(splineMatrix.tf)) {
					data@SGP[["Coefficient_Matrices"]][[i]][!splineMatrix.tf] <- 
						lapply(data@SGP[["Coefficient_Matrices"]][[i]][!splineMatrix.tf], function(x) as.splineMatrix(matrix=x, sgp_object=data))
				}
			}
		} 

		message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))
		return(data)
	} else {
	
	## Required variables

	req.nms <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "VALID_CASE")
	
	## Get the names of the original variables
	## These values will be reused in the output

	nms.original <- names(data)

	
	##  Create an object with default variable names

	default.var.names <- data.frame(nms.orig=req.nms, nms.sgp=req.nms)

	
	##  Check variable names

	if (!missing(var.names)) {
		var.names <- data.frame(nms.orig=toupper(unlist(var.names)), nms.sgp=toupper(names(var.names)))
		
		## Include default variable names (as needed)

		tmp <- default.var.names[(default.var.names$nms.sgp %in% var.names$nms.sgp)==FALSE,]
		var.names <- rbind(var.names, tmp)
	} else {
		var.names <- default.var.names
	}

	
	## Compile the original variable names and the corresponding (capitalized) variable names used in SGP

	var.names.original <- data.frame(column=seq_along(nms.original), nms.original=nms.original, nms.orig=toupper(nms.original))
	var.names$flag <- 1
	tmp.var.names <- merge(var.names.original, var.names, all.x=TRUE)
	tmp.var.names$nms.sgp[is.na(tmp.var.names$flag)] <- NA
	tmp.var.names$flag <- NULL
	tmp.var.names <- subset(tmp.var.names, select=c("column", "nms.original", "nms.sgp"))
	tmp.var.names <- tmp.var.names[order(tmp.var.names$column),]
	tmp.var.names$nms.original  <- as.character(tmp.var.names$nms.original)
	tmp.var.names$nms.sgp <- as.character(tmp.var.names$nms.sgp)

	
	## Check to see if any of the required variables are missing

	if (!all(req.nms %in% tmp.var.names$nms.sgp)) {
		stop(paste("The {data} object is missing the following column name: ", req.nms[(req.nms %in% tmp.var.names$nms.sgp)==FALSE],
		". You may need to identify the variable using the {var.names} argument.", sep=""))
	}

	
	##  Update variable names in the dataset

	names(data)[!is.na(tmp.var.names$nms.sgp)] <- tmp.var.names$nms.sgp[!is.na(tmp.var.names$nms.sgp)]

	
	##  Create keyed data.table and check for duplicate cases

	data <- data.table(data)
	setkeyv(data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

	## Create list identifying date and SGP Package version:

	version <- list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date())

	################################################################	
	## INCLUDE CODE HERE TO HANDLE DUPLICATE CASES
	################################################################	
	
	
	##  Create the SGP object

	sgp_object <- new("SGP", Data=data, Names=tmp.var.names, Version=version)

	
	##  Print finish time
	message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))

	return(sgp_object)
	} ## END else
} ## END prepareSGP function
