`combineSGP` <- 
  function(sgp_object,
           state,
           years,
           content_areas,
           sgp.percentiles=TRUE,
           sgp.percentiles.baseline=TRUE,
           sgp.projections.lagged=TRUE,
           sgp.projections.lagged.baseline=TRUE,
           max.lagged.sgp.target.years.forward=4,
           sgp.target.to.NA=TRUE
           ) {

    started.at <- proc.time()
    message(paste("Started combineSGP", date()))

    ### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                } else {
			message("\tNOTE: argument 'state' required for target SGP calculation. Target SGPs will not be calculated.")
			sgp.projections.lagged <- sgp.projections.lagged.baseline <- FALSE
		}
        }

    ## Adjust arguments based upon state being cohort referenced, baseline referenced, or both:

	if (!is.null(SGPstateData[[state]][["Growth"]][["System_Type"]])) {
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort Referenced") {
			sgp.projections.lagged <- TRUE
			sgp.projections.lagged.baseline <- FALSE
			my.sgp <- "SGP"
			my.sgp.target <- "SGP_TARGET"
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Baseline Referenced") {
			sgp.projections.lagged <- FALSE 
			sgp.projections.lagged.baseline <- TRUE
			my.sgp <- "SGP_BASELINE"
			my.sgp.target <- "SGP_TARGET_BASELINE"
		}
		if (SGPstateData[[state]][["Growth"]][["System_Type"]]=="Cohort and Baseline Referenced") {
			sgp.projections.lagged <- TRUE
			sgp.projections.lagged.baseline <- TRUE
			my.sgp <- "SGP"
			my.sgp.target <- "SGP_TARGET"
		}
	}

    ## Utility functions

    "%w/o%" <- function(x,y) x[!x %in% y]

    rbind.all <- function(.list, names.not.equal=FALSE, ...) {
      if(length(.list)==1) return(.list[[1]])
      if (names.not.equal) {
        Recall(c(list(rbind.fill(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), names.not.equal=TRUE, ...)
      } else {
        Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
      }
    }


    #######################################################
    ### Merge Cohort Referenced SGPs with student data
    #######################################################

     ## Determine years and content_areas if not supplied

      tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGPercentiles))
      if (length(tmp.baseline.names) > 0) {
             tmp.names <- names(sgp_object@SGP$SGPercentiles)[-tmp.baseline.names]
      } else {
             tmp.names <- names(sgp_object@SGP$SGPercentiles)
      }
      if (!missing(years)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[2] %in% years)]
      if (!missing(content_areas)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[1] %in% content_areas)]

      if (length(tmp.names) == 0 & sgp.percentiles) {
             message("\tNOTE: No cohort referenced SGP results available in SGP slot. No cohort referenced SGP results will be merged.")
             sgp.percentiles <- FALSE
      }
    
    if (sgp.percentiles) { 

      ## Determine years and content_areas if not supplied

      tmp.list <- list() 
      for (i in tmp.names) {
        tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                    YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
                                    sgp_object@SGP[["SGPercentiles"]][[i]])
      }

      tmp.list.names <- lapply(tmp.list, names)
      if (length(tmp.list.names) > 1) {
         tmp.names.not.equal <- !all(sapply(seq(length(tmp.list.names)-1), function(x,i) identical(x[[i]], x[[i+1]]), x=tmp.list.names))
      } else { 
         tmp.names.not.equal=FALSE
      }

      if (!"SGP" %in% names(sgp_object@Data)) {
          sgp_object@Data <- data.table(rbind.all(tmp.list, tmp.names.not.equal), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
              key=key(sgp_object@Data))[sgp_object@Data]
      } else {
          my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
          sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
              data.table(rbind.all(tmp.list, tmp.names.not.equal), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), key=key(sgp_object@Data))[
              sgp_object@Data[my.lookup, names(sgp_object@Data) %w/o% ((names(tmp.list[[1]]) %w/o% c("CONTENT_AREA", "YEAR", "ID"))), with=FALSE]][, names(sgp_object@Data), with=FALSE]
      }
      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
      rm(tmp.list); suppressWarnings(gc())
    }


    #######################################################
    ### Merge baseline referenced SGPs with student data
    #######################################################

      ## Determine years and content_areas if not supplied

      tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGPercentiles), value=TRUE)
      if (length(tmp.baseline.names) > 0) {
             tmp.names <- tmp.baseline.names
             if (!missing(years)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[2] %in% years)]
             if (!missing(content_areas)) tmp.names <- tmp.names[sapply(strsplit(tmp.names, "[.]"), function(x) x[1] %in% content_areas)]
      }
      if (length(tmp.baseline.names) == 0 & sgp.percentiles.baseline) {
             message("\tNOTE: No baseline referenced SGP results available in SGP slot. No baseline referenced SGP results will be merged.")
             sgp.percentiles.baseline=FALSE
      }

    if (sgp.percentiles.baseline) {

      tmp.list <- list() 
      for (i in tmp.names) {
        tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                    YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
                                    sgp_object@SGP[["SGPercentiles"]][[i]])
        names(tmp.list[[i]])[-(1:3)] <- paste(names(tmp.list[[i]])[-(1:3)], "BASELINE", sep="_")
      }

      if (!"SGP_BASELINE" %in% names(sgp_object@Data)) {
          sgp_object@Data <- data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
              key=key(sgp_object@Data))[sgp_object@Data]
      } else {
          my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
          sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
              data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), key=key(sgp_object@Data))[
              sgp_object@Data[my.lookup,  names(sgp_object@Data) %w/o% ((names(tmp.list[[1]]) %w/o% c("CONTENT_AREA", "YEAR", "ID"))), with=FALSE]][, names(sgp_object@Data), with=FALSE]
      }
      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
      rm(tmp.list); suppressWarnings(gc())
    }


    ######################################################################################
    ### Create SGP targets (Cohort and  Baseline referenced) and merge with student data
    ######################################################################################

      ### Check that objects exist and correct arguments as necessary

      tmp.lagged.names <- grep("LAGGED", names(sgp_object@SGP$SGProjections))
      tmp.baseline.names <- grep("BASELINE", names(sgp_object@SGP$SGProjections))
      tmp.names.lagged <- names(sgp_object@SGP$SGProjections)[tmp.lagged.names %w/o% tmp.baseline.names]
      tmp.names.lagged.baseline <- names(sgp_object@SGP$SGProjections)[intersect(tmp.lagged.names, tmp.baseline.names)]
      if (length(tmp.names.lagged) > 0) {
             if (!missing(years)) tmp.names.lagged <- tmp.names.lagged[sapply(strsplit(tmp.names.lagged, "[.]"), function(x) x[2] %in% years)]
             if (!missing(content_areas)) tmp.names.lagged <- tmp.names.lagged[sapply(strsplit(tmp.names.lagged, "[.]"), function(x) x[1] %in% content_areas)]
      }
      if (length(tmp.names.lagged) == 0 & sgp.projections.lagged) {
             message("\tNOTE: No SGP lagged projection results available in SGP slot. No student growth projection targets will be produced.")
             sgp.projections.lagged <- FALSE
      }
      if (length(tmp.names.lagged.baseline) > 0) {
             if (!missing(years)) tmp.names.lagged.baseline <- tmp.names.lagged.baseline[sapply(strsplit(tmp.names.lagged.baseline, "[.]"), function(x) x[2] %in% years)]
             if (!missing(content_areas)) tmp.names.lagged.baseline <- tmp.names.lagged.baseline[sapply(strsplit(tmp.names.lagged.baseline, "[.]"), function(x) x[1] %in% content_areas)]
      }
      if (length(tmp.names.lagged.baseline) == 0 & sgp.projections.lagged.baseline) {
             message("\tNOTE: No baseline SGP lagged projection results available in SGP slot. No baseline referenced student growth projection targets will be produced.")
             sgp.projections.lagged.baseline <- FALSE
      }


      ### Calculate Targets
 
      if (sgp.projections.lagged | sgp.projections.lagged.baseline) { 

      level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1

      ## Create variable CATCH_UP_KEEP_UP_STATUS_INITIAL indicating catch up keep up status.
      
      ID <- CONTENT_AREA <- YEAR <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- NULL  ## DONE to AVOID warnings during R CMD check
      sgp_object@Data$YEAR_INTEGER_TMP <- as.integer(sgp_object@Data$YEAR) ## To convert YEAR, when factor, to integer
      key(sgp_object@Data) <- c("ID", "CONTENT_AREA", "YEAR_INTEGER_TMP", "VALID_CASE") ## CRITICAL that Valid_Case is last in group
      sgp_object@Data$ACHIEVEMENT_LEVEL_PRIOR <- sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL <- 
             sgp_object@Data[SJ(ID, CONTENT_AREA, YEAR_INTEGER_TMP-1, "VALID_CASE"), mult="last"][, ACHIEVEMENT_LEVEL]
      sgp_object@Data$YEAR_INTEGER_TMP <- NULL
      if (!all(levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])) {
             levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL)[!levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]] <- NA
      }
      levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL) <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]   
      levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL) <- c("Catching Up", "Keeping Up")
      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL <- factor(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL, order=FALSE) ## Drop ordered attribute of factor


    #################################################################################
    ## Cohort referenced lagged SGP targets and Catch Up Keep Up Status Variable
    #################################################################################

    if (sgp.projections.lagged) {


      tmp.list <- list()
      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
      for (i in tmp.names.lagged) {
        cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
        num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
        tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                 YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
                                 sgp_object@SGP[["SGProjections"]][[i]][,c(1,cols.to.get[1:num.cols.to.get])])
      }

      tmp_object_1 <- sgp_object@Data[, c(key(sgp_object@Data), "CATCH_UP_KEEP_UP_STATUS_INITIAL"), with=FALSE][
        data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), rbind.all(tmp.list),key=key(sgp_object@Data))][
        !is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

      ## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

      VALID_CASE <- NULL
      catch_keep_functions <- c(min, max)
      jExpression <- parse(text=paste("{catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
      tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
      names(tmp_object_2)[dim(tmp_object_2)[2]] <- "SGP_TARGET"
      key(tmp_object_2) <- key(sgp_object@Data)

      if (!"SGP_TARGET" %in% names(sgp_object@Data)) {
           sgp_object@Data <- tmp_object_2[sgp_object@Data]
      } else {
           my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
           sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
              tmp_object_2[sgp_object@Data[my.lookup, 
                 names(sgp_object@Data) %w/o% ((names(tmp_object_2) %w/o% c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))), with=FALSE]][, names(sgp_object@Data), with=FALSE]
      }
      
      if (sgp.target.to.NA) sgp_object@Data$SGP_TARGET[is.na(sgp_object@Data$SGP)] <- NA 

      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
      rm(tmp.list); suppressWarnings(gc())
 } ## END if (sgp.projections.lagged)


    ##############################################################################################
    ### Lagged Baseline Student Growth Projection Targets and Catch Up Keep Up Variable
    ##############################################################################################

    if (sgp.projections.lagged.baseline) {

         tmp.list <- list()
         key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
         for (i in tmp.names.lagged.baseline) {
           cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
           num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
           tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                    YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
                                    sgp_object@SGP[["SGProjections"]][[i]][,c(1,cols.to.get[1:num.cols.to.get])])
         }

         tmp_object_1 <- sgp_object@Data[, c(key(sgp_object@Data), "CATCH_UP_KEEP_UP_STATUS_INITIAL"), with=FALSE][
           data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), rbind.all(tmp.list),key=key(sgp_object@Data))][
           !is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

         ## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

      VALID_CASE <- NULL
      catch_keep_functions <- c(min, max)
      jExpression <- parse(text=paste("{catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
      tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
      names(tmp_object_2)[dim(tmp_object_2)[2]] <- "SGP_TARGET_BASELINE"
      key(tmp_object_2) <- key(sgp_object@Data)

      if (!"SGP_TARGET_BASELINE" %in% names(sgp_object@Data)) {
           sgp_object@Data <- tmp_object_2[sgp_object@Data]
      } else {
           my.lookup <- J("VALID_CASE", sapply(strsplit(tmp.names, "[.]"), function(x) x[1]), type.convert(sapply(strsplit(tmp.names, "[.]"), function(x) x[2])))
           sgp_object@Data[sgp_object@Data[my.lookup, which=TRUE]] <- 
              tmp_object_2[sgp_object@Data[my.lookup,  
              names(sgp_object@Data) %w/o% ((names(tmp_object_2) %w/o% c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))), with=FALSE]][, names(sgp_object@Data), with=FALSE]
      }

      if (sgp.target.to.NA) sgp_object@Data$SGP_TARGET_BASELINE[is.na(sgp_object@Data$SGP)] <- NA 

      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
      rm(tmp.list); suppressWarnings(gc())
    } ## End if (sgp.projections.lagged.baseline)


      #################################################
      ## Create CATCH_UP_KEEP_UP_STATUS variable
      #################################################

      if ("CATCH_UP_KEEP_UP_STATUS" %in% names(sgp_object@Data)) sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- NULL 
      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- NA

      ### CATCH_UP_KEEP_UP BASED UPON SGP versus SGP_TARGET

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
                                                      sgp_object@Data[[my.sgp]] >= sgp_object@Data[[my.sgp.target]]] <- "Keep Up: Yes"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
                                                      sgp_object@Data[[my.sgp]] < sgp_object@Data[[my.sgp.target]]] <- "Keep Up: No"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
                                                      sgp_object@Data[[my.sgp]] >= sgp_object@Data[[my.sgp.target]]] <- "Catch Up: Yes"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
                                                      sgp_object@Data[[my.sgp]] < sgp_object@Data[[my.sgp.target]]] <- "Catch Up: No"

      ### CATCH_UP_KEEP_UP clean up based upon reality

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
                                                      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" &
                                                      as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) <= level.to.get] <- "Keep Up: No"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
                                                      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: No" &
                                                      as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) > level.to.get] <- "Catch Up: Yes"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Catching Up" &
                                                      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" &
                                                      as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) <= level.to.get &
                                                      sgp_object@Data$GRADE == max(sgp_object@Data$GRADE[!is.na(sgp_object@Data$SGP_TARGET)])] <- "Catch Up: No"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL == "Keeping Up" &
                                                      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: No" &
                                                      as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) > level.to.get &
                                                      sgp_object@Data$GRADE == max(sgp_object@Data$GRADE[!is.na(sgp_object@Data$SGP_TARGET)])] <- "Keep Up: Yes"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- as.factor(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS)


   } ## END sgp.projections.lagged | sgp.projections.lagged.baseline

    key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")

    message(paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"))
    return(sgp_object)

  } ## END combineSGP Function
