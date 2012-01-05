`summarizeSGP` <- 
  function(sgp_object,
           state,
           years,
           content_areas,
           sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
             MEDIAN_SGP_COUNT="num_non_missing(SGP)",
             PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
             PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)",
             PERCENT_AT_ABOVE_PROFICIENT_PRIOR="percent_in_category(ACHIEVEMENT_LEVEL_PRIOR, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
             PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT="num_non_missing(ACHIEVEMENT_LEVEL_PRIOR)"),
           summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
             content="CONTENT_AREA",
             time="YEAR",
             institution_level="GRADE",
             demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS"),
             institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
           confidence.interval.groups=list(TYPE="Bootstrap",
             VARIABLES=c("SGP"),
             QUANTILES=c(0.025, 0.975),
             GROUPS=list(institution="SCHOOL_NUMBER",
               content="CONTENT_AREA",
               time="YEAR",
               institution_level= NULL,
               demographic=NULL,
               institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")))) {

    started.at <- proc.time()
    message(paste("\nStarted summarizeSGP", date()))

    if (missing(sgp_object)) {
      stop("User must supply a list containing a Student slot with long data. See documentation for details.")
    }

    ### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }


    ## If missing years and content_areas then determine year(s), and content_area(s) for summaries

    if (missing(content_areas)) {
      content_areas <- unique(sgp_object@Data["VALID_CASE"]$CONTENT_AREA)
    }
    if (missing(years)) {
      for (i in content_areas) {
        years <- sort(tail(unique(sgp_object@Data[J("VALID_CASE", content_areas)]$YEAR), 3))
      }
    }

    ## Functions

    rbind.all <- function(.list, ...){
      if(length(.list)==1) return(.list[[1]])
      Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
    }

    group.format <- function(my.group) {
      if (is.null(my.group)) {
        c("")
      } else {
        c("", unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
      }
    }

    median_na <- function(x) median(as.numeric(x), na.rm=TRUE)
    boot.median <- function(x,i) median(x[i], na.rm=TRUE)
    mean_na <- function(x, result.digits=1) round(mean(as.numeric(x), na.rm=TRUE), digits=result.digits)
    num_non_missing <- function(x) sum(!is.na(as.numeric(x)))

    percent_in_category <- function(x, in.categories, of.categories, result.digits=1) { ## NOTE: x must be a factor and categories levels
      if (!is.list(in.categories)) in.categories <- list(in.categories)
      if (!is.list(of.categories)) of.categories <- list(of.categories)
      tmp.result <- list()
      tmp <- summary(x[!is.na(x)])
      for (i in seq(length(in.categories))) {
        tmp.result[[i]] <-  round(100*sum(tmp[in.categories[[i]]])/sum(tmp[of.categories[[i]]]), digits=result.digits)
      }
      return(unlist(tmp.result))
    }

    percent_at_above_target <- function(sgp, target, result.digits=1) {
      tmp.logical <- sgp >= target
      tmp.pct <- round(sum(tmp.logical, na.rm=TRUE)/sum(!is.na(tmp.logical))*100, digits=result.digits)
      return(tmp.pct)
    }

    ## boot.sgp <- function(data, conf.quantiles=c(0.025, 0.975)) {
    ## 	as.list(quantile(boot(data, boot.median, 100)$t, probs=conf.quantiles, na.rm=TRUE))
    ## }
    boot.sgp <- function(dat, conf.quantiles=c(0.025, 0.975),nboot=100) {
      rep(NA,2)->CI
      out<-numeric()
      if (sum(is.na(dat))!=length(dat))  {
        for (j in 1:nboot) {
          sample(dat,length(dat),replace=TRUE)->foo
          boot.median(foo)->out[j]
        }
        as.numeric(quantile(out,conf.quantiles,na.rm=TRUE))->CI
      }
      #as.list(CI)->to.ret
      #names(to.ret)<-names(CI)
      CI
    }

    sgpSummary <- function(sgp.groups.to.summarize, produce.confidence.interval) {
      SGP_SIM <- V1 <- .SD <- NULL  ## To prevent R CMD check warning
      if (produce.confidence.interval) {
        if ("Bootstrap" %in% confidence.interval.groups$TYPE) {
          require(boot)
          tmp.list <- list()
          tmp.quantiles <- paste("c(", paste(confidence.interval.groups$QUANTILES, collapse=", "), ")", sep="")
          for (i in confidence.interval.groups$VARIABLES) {
            tmp.list[[paste("MEDIAN_", i, "_QUANTILES", sep="")]] <- paste("boot.sgp(", i, ", ", tmp.quantiles, ")", sep="")
          }
          tmp.sgp.summaries <- c(sgp.summaries, tmp.list)
          sgp.summaries.names <- c(unlist(strsplit(names(sgp.summaries), "[.]")), paste("MEDIAN_SGP_", confidence.interval.groups$QUANTILES, "_CONFIDENCE_BOUND", sep="")) 
        } 
        if ("CSEM" %in% confidence.interval.groups$TYPE) {
          tmp.sgp.summaries <- sgp.summaries
          sgp.summaries.names <- c(unlist(strsplit(names(sgp.summaries), "[.]")), paste("MEDIAN_SGP_", confidence.interval.groups$QUANTILES, "_CONFIDENCE_BOUND", sep=""))
        }
      } else {
        tmp.sgp.summaries <- sgp.summaries
        sgp.summaries.names <- unlist(strsplit(names(sgp.summaries), "[.]"))
      } 
      ListExpr <- parse(text=paste("as.list(c(", paste(unlist(tmp.sgp.summaries), collapse=", "),"))",sep="")) 
      ByExpr <- parse(text=paste("list(", paste(sgp.groups.to.summarize, collapse=", "), ")", sep=""))
      tmp <- tmp.dt[, eval(ListExpr), by=eval(ByExpr)]
      if (produce.confidence.interval & "CSEM" %in% confidence.interval.groups$TYPE) {
        SIM_ByExpr1 <- parse(text=paste("list(", paste(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))
                               [!(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))) %in% key(tmp.dt)], collapse=", "), 
                               ", ", paste(names(tmp.simulation.dt)[grep("SGP_SIM_", names(tmp.simulation.dt))], collapse=", "), ")", sep=""))
        SIM_ByExpr2 <- parse(text=paste("list(", paste(sgp.groups.to.summarize, collapse=", "), ")", sep=""))
        tmp.sim <- tmp.dt[tmp.simulation.dt, eval(SIM_ByExpr1)][, -(1:2), with=FALSE][,
                                                                                  lapply(.SD, median_na), by=eval(SIM_ByExpr2)][, 
                                                                                                            as.list(round(apply(.SD, 1, quantile, probs=confidence.interval.groups$QUANTILES))), by=eval(SIM_ByExpr2)]
        tmp <- data.table(merge.data.frame(tmp, tmp.sim, by = unlist(strsplit(as.character(sgp.groups.to.summarize), ", ")),all=TRUE))
      }
      names(tmp)[-seq(length(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))))] <- sgp.summaries.names
      message(paste("\tFinished with", sgp.groups.to.summarize))
      return(tmp)
    }

    combineSims <- function(sgp_object) {
      tmp.list <- list()
      tmp.names <- names(sgp_object@SGP[["Simulated_SGPs"]]) 
      for (i in tmp.names) {
        tmp.list[[i]] <- data.frame(sgp_object@SGP[["Simulated_SGPs"]][[i]],
                                    CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                    YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]))
      }
      
      data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
                 key=paste(key(tmp.dt), collapse=","))
    }
    
    ## Prepare data

    tmp.dt <- data.table(STATE=state, sgp_object@Data[CJ("VALID_CASE", content_areas, years)], key="VALID_CASE, ID, CONTENT_AREA, YEAR")

    if (!is.null(confidence.interval.groups) & "CSEM" %in% confidence.interval.groups$TYPE) {
      tmp.simulation.dt <- combineSims(sgp_object); gc()
    } 

    ## Create summary tables

    for (i in summary.groups$institution) {
      sgp.groups <- do.call(paste, c(expand.grid(i,
                                                 group.format(summary.groups[["content"]]),
                                                 group.format(summary.groups[["time"]]),
                                                 group.format(summary.groups[["institution_level"]]),
                                                 group.format(summary.groups[["institution_inclusion"]][[i]]),
                                                 group.format(summary.groups[["demographic"]])), sep=""))

      if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
        ci.groups <- do.call(paste, c(expand.grid(i,
                                                  group.format(confidence.interval.groups[["GROUPS"]][["content"]]),
                                                  group.format(confidence.interval.groups[["GROUPS"]][["time"]]),
                                                  group.format(confidence.interval.groups[["GROUPS"]][["institution_level"]]),
                                                  group.format(confidence.interval.groups[["GROUPS"]][["institution_inclusion"]][[i]]),
                                                  group.format(confidence.interval.groups[["GROUPS"]][["demographic"]])), sep=""))
      }

      if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
        j <- k <- NULL ## To prevent R CMD check warnings
        sgp_object@Summary[[i]] <- foreach(j=iter(sgp.groups), k=iter(sgp.groups %in% ci.groups), 
                                           .options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE) %dopar% {
                                             return(sgpSummary(j, k))
                                           }
        names(sgp_object@Summary[[i]]) <- gsub(", ", "__", sgp.groups)
      } else {
        j <- k <- NULL ## To prevent R CMD check warnings
        sgp_object@Summary[[i]] <- foreach(j=iter(sgp.groups), k=iter(rep(FALSE, length(sgp.groups))), 
                                           .options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE) %dopar% {
                                             return(sgpSummary(j, k))
                                           }
        names(sgp_object@Summary[[i]]) <- gsub(", ", "__", sgp.groups)
      }
    } ## END summary.groups$institution summary loop

    message(paste("Finished summarizeSGP", date(), "in", timetaken(started.at), "\n"))
    return(sgp_object)
  } ## END summarizeSGP Function
