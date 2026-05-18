`analyzeSGP` <-
function(sgp_object,
         state=NULL,
         years=NULL,
         content_areas=NULL,
         grades=NULL,
         sgp.percentiles=TRUE,
         sgp.projections=TRUE,
         sgp.projections.lagged=TRUE,
         sgp.percentiles.baseline=TRUE,
         sgp.projections.baseline=TRUE,
         sgp.projections.lagged.baseline=TRUE,
         sgp.percentiles.baseline.max.order=3,
         sgp.percentiles.srs.baseline.max.order=3,
         sgp.projections.baseline.max.order=3,
         sgp.projections.lagged.baseline.max.order=3,
         sgp.projections.max.forward.progression.years=3,
         sgp.projections.max.forward.progression.grade=NULL,
         sgp.projections.use.only.complete.matrices=NULL,
         sgp.minimum.default.panel.years=NULL,
         sgp.use.my.coefficient.matrices=NULL,
         sgp.use.my.sgp_object.baseline.coefficient.matrices=NULL,
         sgp.test.cohort.size=NULL,
         return.sgp.test.results=FALSE,
         simulate.sgps=TRUE,
         calculate.simex=NULL,
         calculate.simex.baseline=NULL,
         calculate.simex.srs.baseline=NULL,
         calculate.srs=NULL,
         calculate.srs.baseline=NULL,
         goodness.of.fit.print=TRUE,
         sgp.config=NULL,
         sgp.config.drop.nonsequential.grade.progression.variables=TRUE,
         sgp.baseline.panel.years=NULL,
         sgp.baseline.config=NULL,
         trim.sgp.config=TRUE,
         parallel.config=NULL,
         verbose.output=FALSE,
         print.other.gp=NULL,
         sgp.projections.projection.unit="YEAR",
         get.cohort.data.info=FALSE,
         sgp.sqlite=FALSE,
         sgp.percentiles.equated=NULL,
         sgp.percentiles.equating.method=NULL,
         sgp.percentiles.calculate.sgps=TRUE,
         SGPt=NULL,
         fix.duplicates=NULL,
         ...) {

	started.at <- proc.time()
	messageSGP(paste("\nStarted analyzeSGP", prettyDate()), "\n")
	messageSGP(match.call())

	VALID_CASE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- YEAR_WITHIN <- SCALE_SCORE <- SCALE_SCORE_EQUATED <- NULL

	#######################################################
	### Create relevant analyzeSGP variables
	#######################################################

	### Create state (if NULL) from sgp_object (if possible)
	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- getStateAbbreviation(tmp.name, "analyzeSGP")
	}

    SGPstateData <- list(SGP::SGPstateData[[state]]) ### Needed due to possible assignment of values to SGPstateData
    names(SGPstateData) <- state

	###############################################################
	### Tests associated with supplied arguments
	###############################################################

	if (!(sgp.percentiles | sgp.percentiles.baseline)) {
		simulate.sgps <- FALSE
	}

	if (simulate.sgps) {
    csem.variable <- NULL
    calculate.confidence.intervals.list <- list(state=state)
		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			messageSGP("\tNOTE: CSEMs are required in 'SGPstateData' (either as a data.frame of CSEMs or as a variable name of CSEMsin @Data) to simulate SGPs for confidence interval calculations. SGP standard errors will not be calculated.")
			calculate.confidence.intervals.list <- NULL
		} else {
      if (is.character(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
        csem.variable <- SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]]
      }
      if (is.numeric(SGPstateData[[state]][["SGP_Configuration"]][["calculate.confidence.intervals"]][["confidence.quantiles"]])) {
        calculate.confidence.intervals.list[['confidence.quantiles']] <-
          SGPstateData[[state]][["SGP_Configuration"]][["calculate.confidence.intervals"]][["confidence.quantiles"]]
      }
		}
	} else {
		calculate.confidence.intervals.list <- csem.variable <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.config.drop.nonsequential.grade.progression.variables"]])) {
		sgp.config.drop.nonsequential.grade.progression.variables <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.config.drop.nonsequential.grade.progression.variables"]]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]])) {
		sgp.loss.hoss.adjustment <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]]
	} else {
		sgp.loss.hoss.adjustment <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["return.norm.group.scale.scores"]])) {
		return.norm.group.scale.scores <- SGPstateData[[state]][["SGP_Configuration"]][["return.norm.group.scale.scores"]]
	} else {
		return.norm.group.scale.scores <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["return.norm.group.dates"]])) {
		return.norm.group.dates <- SGPstateData[[state]][["SGP_Configuration"]][["return.norm.group.dates"]]
	} else {
		return.norm.group.dates <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["return.projection.group.scale.scores"]])) {
		return.projection.group.scale.scores <- SGPstateData[[state]][["SGP_Configuration"]][["return.projection.group.scale.scores"]]
	} else {
		return.projection.group.scale.scores <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["return.projection.group.dates"]])) {
		return.projection.group.dates <- SGPstateData[[state]][["SGP_Configuration"]][["return.projection.group.dates"]]
	} else {
		return.projection.group.dates <- NULL
	}

	if (!is.null(SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]])) {
		percentile.trajectory.values <- sort(unique(c(SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 50)))
	} else {
		percentile.trajectory.values <- c(35, 50, 65)
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["percentile.trajectory.values"]])) {
		percentile.trajectory.values <- sort(unique(c(percentile.trajectory.values, SGPstateData[[state]][["SGP_Configuration"]][["percentile.trajectory.values"]])))
	}

	if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Projection_Fan_Limits"]])) {
		percentile.trajectory.values <- sort(c(SGPstateData[[state]][["Student_Report_Information"]][["Projection_Fan_Limits"]], percentile.trajectory.values))
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["gaPlot.back.extrapolated.cuts"]])) {
		percentile.trajectory.values <- sort(unique(c(percentile.trajectory.values, 1:9*10)))
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.baseline.max.order"]])) {
		sgp.projections.baseline.max.order <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.baseline.max.order"]]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.lagged.baseline.max.order"]])) {
		sgp.projections.lagged.baseline.max.order <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.lagged.baseline.max.order"]]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["return.prior.scale.score.standardized"]])) {
		return.prior.scale.score.standardized <- SGPstateData[[state]][["SGP_Configuration"]][["return.prior.scale.score.standardized"]]
	} else {
		return.prior.scale.score.standardized <- TRUE
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["max.n.for.coefficient.matrices"]])) {
		max.n.for.coefficient.matrices <- SGPstateData[[state]][["SGP_Configuration"]][["max.n.for.coefficient.matrices"]]
	} else {
		max.n.for.coefficient.matrices <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.cohort.size"]]) & is.null(sgp.use.my.coefficient.matrices)) {
		tmp.cohort.size <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.cohort.size"]]
	} else tmp.cohort.size <- NULL

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.less.than.sgp.cohort.size.return"]])) {
		sgp.less.than.sgp.cohort.size.return <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.less.than.sgp.cohort.size.return"]]
	} else sgp.less.than.sgp.cohort.size.return <- NULL

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["rq.method"]])) {
		tmp.rq.method <- SGPstateData[[state]][["SGP_Configuration"]][["rq.method"]]
	} else tmp.rq.method <- "br"

	if (!is.null(sgp.config) && sgp.config.drop.nonsequential.grade.progression.variables) {
		sgp.config.drop.nonsequential.grade.progression.variables <- FALSE
	}

	if (!any(
		grepl("PERCENTILES|BASELINE_PERCENTILES|TAUS|SIMEX|BASELINE_MATRICES|PROJECTIONS|LAGGED_PROJECTIONS",
		      names(parallel.config[["WORKERS"]])
			)
	)) {
		parallel.config <- NULL # NULL out parallel.config when passed from abcSGP, etc.
	}

    if (!is.null(parallel.config)) {
        par.warn <-
            c("\n\tCAUTION:  Running higher- and lower-level processes (i.e., 'PERCENTILES and TAUS') in parallel",
              "\n\t\t  at the same time. Make sure you have enough CPU cores and memory to support this!\n")

        if (all(c("PERCENTILES", "TAUS") %in% names(parallel.config[["WORKERS"]]))) {
            messageSGP(par.warn)
        }
        if (all(c("PERCENTILES", "SIMEX") %in% names(parallel.config[["WORKERS"]]))) {
            messageSGP(gsub("'PERCENTILES and TAUS'", "'PERCENTILES and SIMEX'", par.warn))
        }
        if (all(c("BASELINE_PERCENTILES", "TAUS") %in% names(parallel.config[["WORKERS"]]))) {
            messageSGP(gsub("'PERCENTILES and TAUS'", "'BASELINE_PERCENTILES and TAUS'", par.warn))
        }
        if (all(c("BASELINE_PERCENTILES", "SIMEX") %in% names(parallel.config[["WORKERS"]]))) {
            messageSGP(gsub("'PERCENTILES and TAUS'", "'BASELINE_PERCENTILES and SIMEX'", par.warn))
        }
    }

    if (any(c("SIMEX", "TAUS") %in% names(parallel.config[["WORKERS"]]))) {
        lower.level.parallel.config <- parallel.config
        if (length(
               grep("SUMMARY|GA_PLOTS|SG_PLOTS|SGP_SCALE_SCORE_TARGETS",
                    names(parallel.config[["WORKERS"]]),
                    value = TRUE, invert = TRUE)
            ) <= 2
        ) parallel.config <- NULL # NULL out parallel.config when passed from abcSGP, etc
    } else lower.level.parallel.config <- NULL

	if (!is.null(calculate.simex) | !is.null(calculate.simex.baseline)) {
		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
            messageSGP(c("\tNOTE: CSEMs are required in 'SGPstateData' (either as a data.frame of CSEMs or",
                    "\n\t      as a variable name of CSEMs in @Data) to produce SIMEX corrected SGPs.",
                    "\n\t      SIMEX corrected SGPs will NOT be calculated."))
			calculate.simex <- calculate.simex.baseline <- NULL
		}
	}

	if (is.list(calculate.simex) && "csem.data.vnames" %in% names(calculate.simex)) {
		csem.variable <- calculate.simex[["csem.data.vnames"]]
	}

	if (is.list(calculate.simex.baseline) && "csem.data.vnames" %in% names(calculate.simex.baseline)) {
		csem.variable <- calculate.simex.baseline[["csem.data.vnames"]]
	}

	if (identical(calculate.simex, TRUE)) {
		if (is.character(csem.variable <- SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			calculate.simex <- list(csem.data.vnames=csem.variable, lambda=seq(0,2,0.5), simulation.iterations=75, simex.sample.size=5000, extrapolation="linear", save.matrices=TRUE)
		} else 	{
			calculate.simex <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=75, simex.sample.size=5000, extrapolation="linear", save.matrices=TRUE)
			csem.variable <- NULL
		}
		if (identical(sgp.use.my.coefficient.matrices, TRUE)) calculate.simex[['simex.use.my.coefficient.matrices']] <- TRUE
	}

	if (identical(calculate.simex.baseline, TRUE)) {
		if (is.character(csem.variable <- SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			calculate.simex.baseline <- list(csem.data.vnames=csem.variable, lambda=seq(0,2,0.5), simulation.iterations=75, simex.sample.size=5000, extrapolation="linear", save.matrices=TRUE, use.cohort.for.ranking=TRUE)
		} else {
			calculate.simex.baseline <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=75, simex.sample.size=5000, extrapolation="linear", save.matrices=TRUE, use.cohort.for.ranking=TRUE)
			csem.variable <- NULL
		}
	}

	if (is.null(sgp.minimum.default.panel.years) & !is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.minimum.default.panel.years']])) {
		sgp.minimum.default.panel.years <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.minimum.default.panel.years']]
	}

	if (is.null(sgp.minimum.default.panel.years) & is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.minimum.default.panel.years']])) {
		if (uniqueN(sgp_object@Data[['YEAR']])==2) {
			sgp.minimum.default.panel.years <- 2
			messageSGP("\tNOTE: Only two years of data present. Minimum default of 3 years of panel data for SGP analyses changed to 2. Please confirm this is consistent with analyses you wish to perform.")
		} else {
			sgp.minimum.default.panel.years <- 3
		}
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.grade']])) {
		sgp.projections.max.forward.progression.grade <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.grade']]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['print.other.gp']])) {
		print.other.gp <- SGPstateData[[state]][["SGP_Configuration"]][['print.other.gp']]
	}

	if (is.null(print.other.gp)) print.other.gp <- FALSE

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.years']])) {
		if (identical(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.years']], FALSE)) {
			sgp.projections.max.forward.progression.years <- NULL
		} else {
			sgp.projections.max.forward.progression.years <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.years']]
		}
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.projection.unit']])) {
		sgp.projections.projection.unit <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.projection.unit']]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.projection.unit.label']])) {
		sgp.projections.projection.unit.label <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.projection.unit.label']]
	} else {
		sgp.projections.projection.unit.label <- sgp.projections.projection.unit
	}

	if (is.character(goodness.of.fit.print)) {
		if (goodness.of.fit.print =="GROB") {
			goodness.of.fit.print <- FALSE
			goodness.of.fit.print.arg <- state
		} else goodness.of.fit.print <- as.logical(goodness.of.fit.print)
	} else {
		if (!goodness.of.fit.print) {
			goodness.of.fit.print.arg <- FALSE
		} else {
    		if (identical(SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.achievement.level.prior"]], FALSE)) {   ### For RLI and RLI_UK
        		goodness.of.fit.print.arg <- TRUE
      		} else {
        		goodness.of.fit.print.arg <- state
      		}
		}
	}

	if (!is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]])) {
		if (SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]!={tmp.last.year <- tail(sort(unique(sgp_object@Data, by='YEAR')[['YEAR']]), 1)}) {
			sgp.percentiles.equated <- FALSE
      if ("SCALE_SCORE_EQUATED" %in% names(sgp_object@Data)) sgp_object@Data[["SCALE_SCORE_EQUATED"]][sgp_object@Data[["YEAR"]] >= tmp.last.year] <- sgp_object@Data[["SCALE_SCORE"]][sgp_object@Data[["YEAR"]] >= tmp.last.year]
		} else {
			if (!identical(sgp.percentiles.equated, FALSE)) sgp.percentiles.equated <- TRUE
		}
	} else {
		if (identical(sgp.percentiles.equated, TRUE)) {
			messageSGP("\t\tNOTE: 'sgp.percentiles.equated' has been set to TRUE but no meta-data exists in 'SGPstateData' associated with that assessment transition. Equated/linked SGP analyses require meta-data embedded in 'SGPstateData' to correctly work. Contact package administrators on how such data can be added to the package.")
		}
		sgp.percentiles.equated <- FALSE
	}

	if (!is.null(SGPt)) {
		if (identical(SGPt, TRUE)) SGPt <- "DATE"
		if (!all(SGPt %in% names(sgp_object@Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(SGPt, collapse=", "), "are not all contained in the supplied 'sgp_object@Data'. 'SGPt' is set to NULL.\n")
			SGPt <- NULL
		}
    	SGPt.max.time <- SGPstateData[[state]][['SGP_Configuration']][['SGPt.max.time']]
	} else {
    	SGPt.max.time <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.use.my.sgp_object.baseline.coefficient.matrices']]) && is.null(sgp.use.my.sgp_object.baseline.coefficient.matrices)) {
		sgp.use.my.sgp_object.baseline.coefficient.matrices <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.use.my.sgp_object.baseline.coefficient.matrices']]
	}

	if (identical(sgp.use.my.sgp_object.baseline.coefficient.matrices, FALSE)) sgp.use.my.sgp_object.baseline.coefficient.matrices <- NULL

	if (!is.null(sgp.use.my.sgp_object.baseline.coefficient.matrices) && length(grep("BASELINE", names(sgp_object@SGP$Coefficient_Matrices)))==0) {
		sgp.use.my.sgp_object.baseline.coefficient.matrices <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['lagged.percentile.trajectory.values']])) {
		lagged.percentile.trajectory.values <- sort(SGPstateData[[state]][["SGP_Configuration"]][['lagged.percentile.trajectory.values']])
	} else {
	    lagged.percentile.trajectory.values <- NULL
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']])) {
	    sgp.projections.use.only.complete.matrices <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']]
	}

	if (is.null(fix.duplicates) & !is.null(SGPstateData[[state]][["SGP_Configuration"]][["fix.duplicates"]])) {
		fix.duplicates <- SGPstateData[[state]][["SGP_Configuration"]][["fix.duplicates"]]
		return.norm.group.scale.scores <- TRUE
		return.projection.group.scale.scores <- TRUE
	}

	if (sgp.percentiles.calculate.sgps==FALSE) {
		goodness.of.fit.print <- FALSE
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["max.forward.projection.sequence"]]) && 
			!is.null(SGPstateData[[state]][["SGP_Configuration"]][["max.sgp.target.years.forward"]]) &&
			max(SGPstateData[[state]][["SGP_Configuration"]][["max.sgp.target.years.forward"]]) > min(unlist(SGPstateData[[state]][["SGP_Configuration"]][["max.forward.projection.sequence"]]))
		) {
			stop(paste("SGPstateData configuration value for 'max.forward.projection.sequence' is less than the largest specified value for 'max.sgp.target.years.forward'. Reconcile these values in", state, "meta-data."))
	}

	###########################################################################################################
	### Utility functions
	###########################################################################################################

	## Function to merge coefficient matrices from coefficient matrix productions
	merge.coefficient.matrices <- function(list.of.matrices, simex=FALSE) {
		tmp.list <- list()
		tmp.coefficient.matrices <- unlist(list.of.matrices, recursive=FALSE)
		if (simex) {
			for (tmp.names in unique(names(tmp.coefficient.matrices))) {
				tmp1 <- unlist(tmp.coefficient.matrices[grep(tmp.names, names(tmp.coefficient.matrices))], recursive=FALSE)
				names(tmp1) <- sapply(strsplit(names(tmp1), "[.]"), function(x) x[4])
				tmp.list[[tmp.names]] <- tmp1
			}
		} else {
			for (tmp.names in unique(names(tmp.coefficient.matrices))) {
				tmp1 <- unlist(tmp.coefficient.matrices[grep(tmp.names, names(tmp.coefficient.matrices))], recursive=FALSE)
				names(tmp1) <- sapply(strsplit(names(tmp1), "[.]"), function(x) x[3])
				tmp.list[[tmp.names]] <- tmp1
			}
		}
	tmp.list
	}

    get.simulate.sgps.arg <- function(calculate.confidence.intervals.list, sgp.iter) {
		if (!is.null(calculate.confidence.intervals.list) && is.character(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
            calculate.confidence.intervals.list[['variable']] <-
                gsub("[.]+$", "", paste(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], tail(sgp.iter[['sgp.panel.years']], 1), tail(sgp.iter[['sgp.content.areas']], 1),  tail(sgp.iter[['sgp.panel.years.within']], 1), sep="."))
		}
    return(calculate.confidence.intervals.list)
	}

	get.calculate.simex.arg <- function(calculate.simex, sgp.iter) {
		if (is.null(calculate.simex)) return(NULL) # If not NULL, must be a list
		if (is.null(calculate.simex$csem.data.vnames)) return(calculate.simex)
		calculate.simex[['csem.data.vnames']] <- gsub("[.]+$", "", paste(calculate.simex$csem.data.vnames, sgp.iter[['sgp.panel.years']], sgp.iter[['sgp.content.areas']],  sgp.iter[['sgp.panel.years.within']], sep="."))
		return(calculate.simex)
	}

    .filter_matrices_for_projections <- function(matrices, coefficient.matrix.type) {
        if (identical(coefficient.matrix.type, "BASELINE")) {
            return(matrices[grep("BASELINE", names(matrices), value = TRUE)])
        }
        if (identical(coefficient.matrix.type, "EQUATED")) {
            return(matrices[grep("EQUATED", names(matrices), value = TRUE)])
        }
        return(matrices[
            grep("BASELINE|EQUATED|SIMEX", x = names(matrices), value = TRUE, invert = TRUE)
        ])
    }


	#######################################################################################################################
	##   Set up the temporary sgp list object.  Fill with necessary old results if they exist first.
	##   Create subset of @Data containing essential data elements for analyses
	#######################################################################################################################

	if (sgp.percentiles.equated) {
		year.for.equate <- tail(sort(unique(sgp_object@Data, by='YEAR')[['YEAR']]), 1)

        if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][['Baseline_Projections_in_Transition_Year']]) &
            (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline)) {
                messageSGP("\tNOTE: Baseline SGP related analyses are not possible across an assessment transition with equating. Arguments related to baseline analyses are set to FALSE.")
                sgp.percentiles.baseline <- sgp.projections.baseline <- sgp.projections.lagged.baseline <- FALSE
        }

        if (is.null(sgp.use.my.coefficient.matrices)) {
            Scale_Score_Linkages <- list()
            dir.create(file.path("Data", paste("Linkages", year.for.equate, sep="_"), "Figures"), recursive=TRUE, showWarnings=FALSE)
            content_areas.for.equate <- unique(sgp_object@Data[YEAR==year.for.equate], by="CONTENT_AREA")[['CONTENT_AREA']]

            if (is.null(sgp.percentiles.equating.method)) {
                messageSGP("\tNOTE: Analyses involving equating will be performed using each of: 'identity', 'mean', 'linear', and 'equipercentile' methods.\n\t\tSee documentation associated with the 'sgp.percentiles.equating.method' argument in 'analyzeSGP'.")
                sgp.percentiles.equating.method <- c("identity", "mean", "linear", "equipercentile")
            }

            if (!identical(years, year.for.equate)) {
                messageSGP(paste0("\tNOTE: Analyses involving equating only occur in most recent year. 'years' argument changed to ", year.for.equate, "."))
                years <- year.for.equate
            }

            if (!all(paste(content_areas.for.equate, year.for.equate, sep=".") %in% names(SGPstateData[[state]][['Achievement']][['Knots_Boundaries']]))) {
                tmp.knots.boundaries <- createKnotsBoundaries(sgp_object@Data[YEAR==year.for.equate])
                names(tmp.knots.boundaries) <- paste(names(tmp.knots.boundaries), year.for.equate, sep=".")
                SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]] <- c(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]], tmp.knots.boundaries)
                assign(paste(state, "Knots_Boundaries", sep="_"), SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]])
                save(list=paste(state, "Knots_Boundaries", sep="_"), file=paste(state, "Knots_Boundaries.Rdata", sep="_"))
                messageSGP(paste0("\tNOTE: Knots and Boundaries do not exist for ", year.for.equate, " in state provided.\n\tThey have been produced, embedded in SGPstateData, and are available using state=", state, " for subsequent analyses and saved to your working directory '", getwd(), "'."))
            }

            data.for.equate <- copy(sgp_object@Data)

            if ("SCALE_SCORE_EQUATED" %in% names(data.for.equate)) {
                old.scale.score.equated.year <- tail(data.for.equate[!is.na(SCALE_SCORE_EQUATED),.N,keyby="YEAR"]$YEAR, 1)
                if (paste("SCALE_SCORE_EQUATED_FROM", old.scale.score.equated.year, sep="_") %in% names(data.for.equate)) data.for.equate[,paste("SCALE_SCORE_EQUATED_FROM", old.scale.score.equated.year, sep="_"):=NULL]
                setnames(data.for.equate, "SCALE_SCORE_EQUATED", paste("SCALE_SCORE_EQUATED_FROM", old.scale.score.equated.year, sep="_"))
                messageSGP(paste0("\tNOTE: Variable `SCALE_SCORE_EQUATED` exists in @Data and is being renamed as SCALE_SCORE_EQUATED_", old.scale.score.equated.year, " to accommodate an additional assessment transition variable."))
            }

            sgp_object@SGP[['Linkages']] <- Linkages <- equateSGP(data.for.equate, state, year.for.equate, sgp.percentiles.equating.method)
            setkey(data.for.equate, VALID_CASE, CONTENT_AREA, YEAR, GRADE, SCALE_SCORE)
            for (conversion.type.iter in c("OLD_TO_NEW", "NEW_TO_OLD")) {
              for (sgp.percentiles.equating.method.iter in sgp.percentiles.equating.method) {
                data.for.equate <- convertScaleScore(data.for.equate, year.for.equate, sgp_object@SGP[['Linkages']],
                  conversion.type=conversion.type.iter, sgp.percentiles.equating.method.iter, state)
                Scale_Score_Linkages[[conversion.type.iter]][[toupper(sgp.percentiles.equating.method.iter)]] <-
                  unique(data.for.equate, by=key(data.for.equate))[!is.na(SCALE_SCORE) & VALID_CASE=="VALID_CASE", intersect(names(data.for.equate),
                    c("CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "SCALE_SCORE_ACTUAL", paste("SCALE_SCORE_EQUATED", toupper(sgp.percentiles.equating.method.iter), conversion.type.iter, sep="_"))), with=FALSE]
                fwrite(Scale_Score_Linkages[[conversion.type.iter]][[toupper(sgp.percentiles.equating.method.iter)]],
                  file=paste0(paste0("Data/", paste("Linkages", year.for.equate, sep="_"), "/"), paste(gsub(" ", "_", getStateAbbreviation(state, type="LONG")), "Scale_Score_Linkages", toupper(sgp.percentiles.equating.method.iter), conversion.type.iter, sep="_"), ".txt"), quote=FALSE, sep="|")
                linkagePlot(Scale_Score_Linkages[[conversion.type.iter]][[toupper(sgp.percentiles.equating.method.iter)]], conversion.type.iter, sgp.percentiles.equating.method.iter, year.for.equate, state)
              }
            }
            save(Linkages, file=paste0(paste0("Data/", paste("Linkages", year.for.equate, sep="_"), "/"), "Linkages.Rdata"))
            assign(paste(gsub(" ", "_", getStateAbbreviation(state, type="LONG")), "Scale_Score_Linkages", sep="_"), Scale_Score_Linkages)
            save(list=paste(gsub(" ", "_", getStateAbbreviation(state, type="LONG")), "Scale_Score_Linkages", sep="_"),
              file=paste0(paste0("Data/", paste("Linkages", year.for.equate, sep="_"), "/"), paste(gsub(" ", "_", getStateAbbreviation(state, type="LONG")), "Scale_Score_Linkages", sep="_"), ".Rdata"))
            setkey(data.for.equate, VALID_CASE, CONTENT_AREA, YEAR, ID)
            data.for.equate[,setdiff(names(data.for.equate), c(names(sgp_object@Data), 'SCALE_SCORE_EQUATED_EQUIPERCENTILE_OLD_TO_NEW')):=NULL]
            setnames(data.for.equate, 'SCALE_SCORE_EQUATED_EQUIPERCENTILE_OLD_TO_NEW', 'SCALE_SCORE_EQUATED')
            sgp_object@Data <- data.for.equate
        } ### END if (is.null(sgp.use.my.coefficient.matrices))

		equate.variable <- "SCALE_SCORE_EQUATED"
		equate.label <- coefficient.matrix.type <- "EQUATED"
		sgp.percentiles.equated <- TRUE
		sgp.projections.equated <- list(State=state, Year=year.for.equate, Linkages=sgp_object@SGP[['Linkages']])
		tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]], Linkages=sgp_object@SGP[['Linkages']])
	} else {
		sgp.percentiles.equated <- FALSE
		equate.variable <- equate.label <- year.for.equate <- sgp.projections.equated <- coefficient.matrix.type <- NULL
		tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])
	} ### END if (sgp.percentiles.equated)

    variables.to.get <-
        c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE",
          "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", "LAST_OBSERVATION",
          "STATE", csem.variable, equate.variable, SGPt)

		tmp_sgp_data_for_analysis <- sgp_object@Data[,intersect(names(sgp_object@Data), variables.to.get), with=FALSE]["VALID_CASE"]
		sgp.data.names <- names(tmp_sgp_data_for_analysis)
		if ("YEAR_WITHIN" %in% sgp.data.names) {
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEAR_WITHIN)
		} else {
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
		}


	##############################################################################################################################
	###   Baseline SGP - compute matrices first if they are not in SGPstateData or merge them into sgp_object if in SGPstateData
	##############################################################################################################################

	if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {

		if (is.null(sgp.use.my.sgp_object.baseline.coefficient.matrices) && is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
			if (is.null(sgp.baseline.config)) {
				sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years, sgp.percentiles.baseline.max.order)
			} else {
				sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
			}

			messageSGP("\n\tStarted Baseline Coefficient Matrix Calculation:\n")

            ##  SEQUENTIAL BASELINE COEFFICIENT MATRIX CONSTRUCTION
            ##  Or, run TAUS in parallel in studentGrowthPercentiles using lower.level.parallel.config
            ##  Useful if many more cores/workers available than configs to iterate over.
            tmp <- list()
            for (sgp.iter in seq_along(sgp.baseline.config)) {
                tmp[[sgp.iter]] <- baselineSGP(
                    sgp_object,
                    state=state,
                    sgp.baseline.config=sgp.baseline.config[sgp.iter], ## NOTE: must pass list, [...], not vector, [[...]].
                    return.matrices.only=TRUE,
                    calculate.baseline.sgps=FALSE,
                    parallel.config=lower.level.parallel.config)
            }
            tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
			rm(tmp)

			assign(paste0(state, "_Baseline_Matrices"), list())
			for (tmp.matrix.label in grep("BASELINE", names(tmp_sgp_object$Coefficient_Matrices), value=TRUE)) {
				eval(parse(text=paste0(state, "_Baseline_Matrices[['", tmp.matrix.label, "']] <- tmp_sgp_object[['Coefficient_Matrices']][['", tmp.matrix.label, "']]")))
			}
			save(list=paste0(state, "_Baseline_Matrices"), file=paste0(state, "_Baseline_Matrices.Rdata"))
			messageSGP("\n\tFinished Calculating Baseline Coefficient Matrices\n")
		} else {
			if (is.null(sgp.use.my.sgp_object.baseline.coefficient.matrices)) tmp_sgp_object <- mergeSGP(tmp_sgp_object, SGPstateData[[state]][["Baseline_splineMatrix"]])
		}
	} # END Get/Compute baseline coefficient matrices


	#######################################################################################################################
	##   SIMEX Baseline SGP - compute matrices first if they are not in sgp_object
	#######################################################################################################################

	if (sgp.percentiles.baseline & !is.null(calculate.simex.baseline)) {

		if (!is.null(sgp.config)) {
			tmp.subjects <- unique(sapply(sgp.config, function(x) tail(x[["sgp.content.areas"]],1)))
		} else {
			if (!is.null(content_areas)) tmp.subjects <- content_areas else tmp.subjects <- unique(sgp_object@Data["VALID_CASE"], by="CONTENT_AREA")[["CONTENT_AREA"]]
		}

		###  Calculate BASELINE SIMEX matrices if they are not present
		if (!all(find.matrices <- paste0(tmp.subjects, ".BASELINE.SIMEX") %in% names(tmp_sgp_object[["Coefficient_Matrices"]]))) {
      if (length(grep("BASELINE", names(sgp_object@SGP[["Coefficient_Matrices"]])))==0){
        sgp_object@SGP[["Coefficient_Matrices"]] <- c(sgp_object@SGP[["Coefficient_Matrices"]], tmp_sgp_object[["Coefficient_Matrices"]][grep("BASELINE", names(tmp_sgp_object[["Coefficient_Matrices"]]))])
      }

      if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {# Put in SGPstateData to bypass re-running baseline matrices (either already exist or calculated above)
        SGPstateData[[state]][["Baseline_splineMatrix"]] <- tmp_sgp_object[["Coefficient_Matrices"]][grep("BASELINE", names(tmp_sgp_object[["Coefficient_Matrices"]]))]
      }

			if (is.null(sgp.baseline.config)) {
				sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas=tmp.subjects, grades, sgp.baseline.panel.years, sgp.percentiles.baseline.max.order, calculate.simex.baseline)
			} else {
				sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
			}

			sgp.baseline.config <- sgp.baseline.config[which(sapply(sgp.baseline.config, function(x) tail(x[["sgp.baseline.content.areas"]],1)) %in% tmp.subjects[!find.matrices])]

			messageSGP("\n\tStarted SIMEX Baseline Coefficient Matrix Calculation:\n")

			##  Enforce that simex.use.my.coefficient.matrices must be FALSE for BASELINE SIMEX matrix production
			calculate.simex.baseline[['simex.use.my.coefficient.matrices']] <- NULL

            ## SEQUENTIAL BASELINE COEFFICIENT MATRIX CONSTRUCTION
            ##  Or, run SIMEX simulation iterations in parallel in studentGrowthPercentiles using lower.level.parallel.config
            ##  Useful if many more cores/workers available than configs to iterate over.
            tmp <- list()
            for (sgp.iter in seq_along(sgp.baseline.config)) {
                tmp[[sgp.iter]] <- baselineSGP(
                    sgp_object,
                    state=state,
                    sgp.baseline.config=sgp.baseline.config[sgp.iter], ## NOTE: must pass list, [...], not vector, [[...]].
                    return.matrices.only=TRUE,
                    calculate.baseline.sgps=FALSE,
                    calculate.simex.baseline=calculate.simex.baseline,
                    parallel.config=lower.level.parallel.config,
                    panel.data.vnames=getPanelDataVnames("baseline.sgp", sgp.baseline.config[[sgp.iter]], sgp.data.names))
            }
            tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp, simex=TRUE)))

			###  Save SIMEX BASELINE matrices
			assign(paste0(state, "_SIMEX_Baseline_Matrices"), list())
			for (tmp.matrix.label in grep("BASELINE.SIMEX", names(tmp_sgp_object$Coefficient_Matrices), value=TRUE)) {
				eval(parse(text=paste0(state, "_SIMEX_Baseline_Matrices[['", tmp.matrix.label, "']] <- tmp_sgp_object[['Coefficient_Matrices']][['", tmp.matrix.label, "']]")))
			}
			save(list=paste0(state, "_SIMEX_Baseline_Matrices"), file=paste0(state, "_SIMEX_Baseline_Matrices.Rdata"), compress="xz")

			messageSGP("\n\tFinished Calculating SIMEX Baseline Coefficient Matrices\n")
		} # END Compute SIMEX baseline coefficient matrices

		##  Enforce that simex.use.my.coefficient.matrices must be TRUE and save.matrices is FALSE for BASELINE SIMEX calculations below
		calculate.simex.baseline[['simex.use.my.coefficient.matrices']] <- TRUE
		calculate.simex.baseline[['save.matrices']] <- FALSE
	} # END check for SIMEX baseline matrices presence


    #################################################################################
    ###   Create par.sgp.config (for both parallel and sequential implementations)
    #################################################################################

	setkeyv(sgp_object@Data, getKey(sgp_object))
	par.sgp.config <- getSGPConfig(sgp_object, state, tmp_sgp_object, content_areas, years, grades, sgp.config, trim.sgp.config, sgp.percentiles, sgp.projections, sgp.projections.lagged,
		sgp.percentiles.baseline, sgp.projections.baseline, sgp.projections.lagged.baseline, sgp.config.drop.nonsequential.grade.progression.variables,
		sgp.minimum.default.panel.years, sgp.projections.max.forward.progression.years, sgp.use.my.coefficient.matrices, calculate.simex, calculate.simex.baseline, year.for.equate,
        sgp.percentiles.equated, SGPt)

	if (sgp.projections & length(par.sgp.config[['sgp.projections']])==0) {
		messageSGP("\tNOTE: No configurations are present for cohort referenced projections. No cohort referenced projections will be calculated.\n")
		sgp.projections <- FALSE
	}

	if (sgp.projections.lagged & length(par.sgp.config[['sgp.projections.lagged']])==0) {
		messageSGP("\tNOTE: No configurations are present for cohort referenced lagged projections. No lagged cohort referenced projections will be calculated.\n")
		sgp.projections.lagged <- FALSE
	}

	if (sgp.projections.baseline & length(par.sgp.config[['sgp.projections.baseline']])==0) {
		messageSGP("\tNOTE: No configurations are present for baseline projections. No baseline projections will be calculated.\n")
		sgp.projections.baseline <- sgp.projections.lagged.baseline <- FALSE
	}

	if (sgp.projections.lagged.baseline & length(par.sgp.config[['sgp.projections.lagged.baseline']])==0) {
		messageSGP("\tNOTE: No configurations are present for lagged baseline projections. No lagged baseline projections will be calculated.\n")
		sgp.projections.baseline <- sgp.projections.lagged.baseline <- FALSE
	}

    if (sgp.percentiles) {
        if (!is.null(tmp.transition.year <- SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]) &&
            sort(unique(unlist(sapply(par.sgp.config[['sgp.percentiles']], function(x) x[['sgp.panel.years']]))))[1L] < tmp.transition.year) {
                messageSGP(paste0("\tNOTE: Configurations include years prior to assessment transition (", tmp.transition.year, ").\n\t\tOutput will include SGPs of all orders to accommodate investigations.\n"))
                print.other.gp <- TRUE
        }
    }

	### Produce cohort data information

	if (get.cohort.data.info) {
    if (!sgp.percentiles & sgp.percentiles.baseline) tmp.label <- 'sgp.percentiles.baseline' else tmp.label <- 'sgp.percentiles'
		cohort_data_info <- getCohortDataInfo(tmp_sgp_data_for_analysis, par.sgp.config[[tmp.label]])
		save(cohort_data_info, file=file.path("Logs", "cohort_data_info.Rdata"))
        messageSGP("\tNOTE: Cohort data information saved to 'Logs/cohort_data_info.Rdata'.")
	}

###############################################################################
###############################################################################
###          Percentiles  (Cohort, Baseline and Equated)
###############################################################################
###############################################################################

    ###   Cohort-referenced growth percentiles
    call_function <- define_compute(parallel.config, "PERCENTILES")

    if (sgp.percentiles) {

        results <- lapply(
            rev(par.sgp.config[["sgp.percentiles"]]),
            \(sgp.iter) {
                grade.prog = sgp.iter[["sgp.grade.sequences"]]

                list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                          tmp_sgp_data_for_analysis, "sgp.percentiles",
                          sgp.iter, csem.variable, SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Knots_Boundaries = getKnotsBoundaries(sgp.iter, state, "sgp.percentiles"),
                        Coefficient_Matrices = sgp.iter[["sgp.matrices"]]),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    panel.data.vnames = getPanelDataVnames("sgp.percentiles", sgp.iter, sgp.data.names),
                    additional.vnames.to.return = getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, sgp.data.names),
                    grade.progression = grade.prog,
                    content_area.progression = sgp.iter[["sgp.content.areas"]],
                    year.progression = sgp.iter[["sgp.panel.years"]],
                    max.order.for.percentile = SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
                    return.additional.max.order.sgp = sgp.iter[['return.additional.max.order.sgp']],
                    percentile.cuts = SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
                    growth.levels = state,
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    use.my.coefficient.matrices = sgp.use.my.coefficient.matrices,
                    calculate.confidence.intervals = get.simulate.sgps.arg(calculate.confidence.intervals.list, sgp.iter),
                    print.other.gp = print.other.gp,
                    print.sgp.order = !is.null(SGPstateData[[state]][["SGP_Configuration"]][["print.sgp.order"]]),
                    calculate.sgps = sgp.percentiles.calculate.sgps,
                    rq.method = tmp.rq.method,
                    max.n.for.coefficient.matrices = max.n.for.coefficient.matrices,
                    exact.grade.progression.sequence = sgp.iter[["sgp.exact.grade.progression"]],
                    drop.nonsequential.grade.progression.variables = FALSE,
                    sgp.loss.hoss.adjustment = sgp.loss.hoss.adjustment,
                    sgp.cohort.size = tmp.cohort.size,
                    sgp.less.than.sgp.cohort.size.return = sgp.less.than.sgp.cohort.size.return,
                    sgp.test.cohort.size = sgp.test.cohort.size,
                    percuts.digits.internal = SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts.round.digits"]],
                    goodness.of.fit = goodness.of.fit.print.arg,
                    goodness.of.fit.minimum.n = SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
                    return.prior.scale.score.standardized = return.prior.scale.score.standardized,
                    return.norm.group.scale.scores = return.norm.group.scale.scores,
                    return.norm.group.dates = return.norm.group.dates,
                    return.norm.group.preference = sgp.iter[["sgp.norm.group.preference"]],
                    return.panel.data = FALSE,
                    parallel.config = lower.level.parallel.config,
                    calculate.simex = get.calculate.simex.arg(sgp.iter[["sgp.calculate.simex"]], sgp.iter),
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.percentiles"),
                    SGPt.max.time = SGPt.max.time,
                    verbose.output = verbose.output,
                    ## Add in previously unspecified defaults too:
                    year_lags.progression = NULL,
                    num.prior = length(grade.prog[!is.na(grade.prog)])-1L, # Internal default.
                    subset.grade = NA_integer_,
                    rq.method.for.large.n = "fn",
                    knot.cut.percentiles = c(0.2,0.4,0.6,0.8),
                    knots.boundaries.by.panel = FALSE,
                    convert.0and100 = TRUE,
                    sgp.quantiles = "Percentiles",
                    sgp.quantiles.labels = NULL,
                    percuts.digits = 0L,
                    isotonize = TRUE,
                    convert.using.loss.hoss = TRUE,
                    goodness.of.fit.output.format = "GROB",
                    return.prior.scale.score = TRUE,
                    return.norm.group.identifier = TRUE,
                    print.time.taken = TRUE,
                    sgp.percentiles.set.seed = 314159,
                    sgp.percentiles.equated = NULL
                )}
            ) |>
                call_function(studentGrowthPercentiles)

        if (any(
            tmp.tf <- sapply(results, \(x)
                any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
          tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
            sgp.percentiles =
                getErrorReports(results, tmp.tf, rev(par.sgp.config[['sgp.percentiles']])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)

        if (!is.null(sgp.test.cohort.size)) {
            test.ids <- unique(rbindlist(tmp_sgp_object[["SGPercentiles"]], fill=TRUE), by='ID')[["ID"]]
            tmp_sgp_data_for_analysis <- tmp_sgp_data_for_analysis[ID %in% test.ids]
            if (sgp.projections|sgp.projections.baseline) {
                tmp.proj.lookup <-
                    SJ( sapply( seq(length(par.sgp.config[["sgp.projections"]])),
                                function(f) tail(par.sgp.config[["sgp.projections"]][[f]][["sgp.projection.content.areas"]], 1)),
                        sapply( seq(length(par.sgp.config[["sgp.projections"]])),
                                function(f) tail(par.sgp.config[["sgp.projections"]][[f]][["sgp.panel.years"]], 1)),
                        sapply( seq(length(par.sgp.config[["sgp.projections"]])),
                                function(f) tail(par.sgp.config[["sgp.projections"]][[f]][["sgp.projection.grade.sequences"]], 1))
                    ) |> unique()
                setnames(tmp.proj.lookup, c("CONTENT_AREA", "YEAR", "GRADE"))
                setkey(tmp.proj.lookup)
                setkeyv(tmp_sgp_data_for_analysis, key(tmp.proj.lookup))
                missing.lookup <- tmp.proj.lookup[!tmp_sgp_data_for_analysis] # data.table anti join
                if (nrow(missing.lookup) > 0){
                setkeyv(sgp_object@Data, key(missing.lookup))
                tmp_data_to_add <-
                    sgp_object@Data[missing.lookup][
                        VALID_CASE  == "VALID_CASE",
                        intersect(names(sgp_object@Data), variables.to.get), with=FALSE
                    ][,
                     .SD[sample(.N, sgp.test.cohort.size)], by=key(missing.lookup)
                    ]
                tmp_sgp_data_for_analysis <- rbindlist(list(tmp_sgp_data_for_analysis, tmp_data_to_add), use.names = TRUE)
                setkeyv(tmp_sgp_data_for_analysis, getKey(tmp_sgp_data_for_analysis))
                }
            }
        }
    } ## END if sgp.percentiles

    ###   EQUATED, cohort-referenced growth percentiles
    if (sgp.percentiles.equated) {

        results <- lapply(
            rev(par.sgp.config[["sgp.percentiles.equated"]]),
            \(sgp.iter) {
                grade.prog = sgp.iter[["sgp.grade.sequences"]]

                 list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                          tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter, csem.variable,
                          equate.variable, SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Knots_Boundaries = getKnotsBoundaries(sgp.iter, state, "sgp.percentiles"),
                        Coefficient_Matrices = sgp.iter[["sgp.equated.matrices"]]),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1),
                        my.extra.label = equate.label),
                    panel.data.vnames = getPanelDataVnames("sgp.percentiles", sgp.iter, sgp.data.names, equate.variable),
                    additional.vnames.to.return = getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, sgp.data.names),
                    grade.progression = grade.prog,
                    content_area.progression = sgp.iter[["sgp.content.areas"]],
                    year.progression = sgp.iter[["sgp.panel.years"]],
                    max.order.for.percentile = SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
                    return.additional.max.order.sgp = sgp.iter[['return.additional.max.order.sgp']],
                    percentile.cuts = SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
                    growth.levels = state,
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    use.my.coefficient.matrices = sgp.use.my.coefficient.matrices,
                    calculate.confidence.intervals = get.simulate.sgps.arg(calculate.confidence.intervals.list, sgp.iter),
                    print.other.gp = print.other.gp,
                    print.sgp.order = !is.null(SGPstateData[[state]][["SGP_Configuration"]][["print.sgp.order"]]),
                    calculate.sgps = sgp.percentiles.calculate.sgps,
                    rq.method = tmp.rq.method,
                    max.n.for.coefficient.matrices = max.n.for.coefficient.matrices,
                    exact.grade.progression.sequence = sgp.iter[["sgp.exact.grade.progression"]],
                    drop.nonsequential.grade.progression.variables = FALSE,
                    sgp.loss.hoss.adjustment = sgp.loss.hoss.adjustment,
                    sgp.cohort.size = tmp.cohort.size,
                    sgp.less.than.sgp.cohort.size.return = sgp.less.than.sgp.cohort.size.return,
                    sgp.test.cohort.size = sgp.test.cohort.size,
                    percuts.digits.internal = SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts.round.digits"]],
                    goodness.of.fit = goodness.of.fit.print.arg,
                    goodness.of.fit.minimum.n = SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
                    return.prior.scale.score.standardized = return.prior.scale.score.standardized,
                    return.norm.group.scale.scores = return.norm.group.scale.scores,
                    return.norm.group.dates = return.norm.group.dates,
                    return.norm.group.preference = sgp.iter[["sgp.norm.group.preference"]],
                    return.panel.data = FALSE,
                    parallel.config = lower.level.parallel.config,
                    calculate.simex = get.calculate.simex.arg(sgp.iter[["sgp.calculate.simex"]], sgp.iter),
                    sgp.percentiles.equated = sgp.projections.equated,
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.percentiles.equated"),
                    SGPt.max.time = SGPt.max.time,
                    verbose.output = verbose.output,
                    ## Add in previously unspecified defaults too:
                    year_lags.progression = NULL,
                    num.prior = length(grade.prog[!is.na(grade.prog)])-1L, # Internal default.
                    subset.grade = NA_integer_,
                    rq.method.for.large.n = "fn",
                    knot.cut.percentiles = c(0.2,0.4,0.6,0.8),
                    knots.boundaries.by.panel = FALSE,
                    convert.0and100 = TRUE,
                    sgp.quantiles = "Percentiles",
                    sgp.quantiles.labels = NULL,
                    percuts.digits = 0L,
                    isotonize = TRUE,
                    convert.using.loss.hoss = TRUE,
                    goodness.of.fit.output.format = "GROB",
                    return.prior.scale.score = TRUE,
                    return.norm.group.identifier = TRUE,
                    print.time.taken = TRUE,
                    sgp.percentiles.set.seed = 314159
                )}
            ) |>
                call_function(studentGrowthPercentiles)

        if (any(
            tmp.tf <- sapply(results, \(x)
                any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
          tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
            sgp.percentiles.equated =
                getErrorReports(results, tmp.tf, rev(par.sgp.config[["sgp.percentiles.equated"]])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)
    } ## END if sgp.percentiles.equated

    if (sgp.percentiles.baseline) {
        if ("BASELINE_PERCENTILES" %in% names(parallel.config[["WORKERS"]])) {
            tmp.proc <- "BASELINE_PERCENTILES"
        } else {
            tmp.proc <- "PERCENTILES"
        }
        call_function <- define_compute(parallel.config, tmp.proc)

        results <- lapply(
            rev(par.sgp.config[["sgp.percentiles.baseline"]]),
            \(sgp.iter) {
                list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                          tmp_sgp_data_for_analysis, "sgp.percentiles",
                          sgp.iter, csem.variable, SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Knots_Boundaries = getKnotsBoundaries(sgp.iter, state, "sgp.percentiles"),
                        Coefficient_Matrices = sgp.iter[["sgp.baseline.matrices"]]),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1),
                        my.extra.label = "BASELINE"),
                    panel.data.vnames = getPanelDataVnames("sgp.percentiles.baseline", sgp.iter, sgp.data.names),
                    additional.vnames.to.return = getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, sgp.data.names),
                    grade.progression = sgp.iter[["sgp.baseline.grade.sequences"]],
                    content_area.progression = sgp.iter[["sgp.baseline.content.areas"]],
                    year_lags.progression = sgp.iter[["sgp.baseline.panel.years.lags"]],
                    num.prior = min(sgp.iter[["sgp.baseline.max.order"]], sgp.percentiles.baseline.max.order),
                    max.order.for.percentile = SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
                    return.additional.max.order.sgp = sgp.iter[['return.additional.max.order.sgp']],
                    percentile.cuts = SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
                    growth.levels = state,
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    use.my.coefficient.matrices = list(
                        my.year = "BASELINE",
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    calculate.confidence.intervals = get.simulate.sgps.arg(calculate.confidence.intervals.list, sgp.iter),
                    print.other.gp = print.other.gp,
                    print.sgp.order = !is.null(SGPstateData[[state]][["SGP_Configuration"]][["print.sgp.order"]]),
                    rq.method = tmp.rq.method,
                    max.n.for.coefficient.matrices = max.n.for.coefficient.matrices,
                    exact.grade.progression.sequence = sgp.iter[["sgp.exact.grade.progression"]],
                    drop.nonsequential.grade.progression.variables = FALSE,
                    sgp.loss.hoss.adjustment = sgp.loss.hoss.adjustment,
                    sgp.cohort.size = tmp.cohort.size,
                    sgp.less.than.sgp.cohort.size.return = sgp.less.than.sgp.cohort.size.return,
                    sgp.test.cohort.size = sgp.test.cohort.size,
                    percuts.digits.internal = SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts.round.digits"]],
                    goodness.of.fit = goodness.of.fit.print.arg,
                    goodness.of.fit.minimum.n = SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
                    return.prior.scale.score.standardized = return.prior.scale.score.standardized,
                    return.norm.group.scale.scores = return.norm.group.scale.scores,
                    return.norm.group.dates = return.norm.group.dates,
                    return.norm.group.preference = sgp.iter[["sgp.norm.group.preference"]],
                    return.panel.data = FALSE,
                    parallel.config = lower.level.parallel.config,
                    calculate.simex = get.calculate.simex.arg(sgp.iter[["sgp.calculate.simex.baseline"]], sgp.iter),
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.calculate.simex.baseline"),
                    SGPt.max.time = SGPt.max.time,
                    verbose.output = verbose.output,
                    ## Add in previously unspecified defaults too:
                    year.progression = NULL,
                    subset.grade = NA_integer_,
                    calculate.sgps = TRUE,
                    rq.method.for.large.n = "fn",
                    knot.cut.percentiles = c(0.2,0.4,0.6,0.8),
                    knots.boundaries.by.panel = FALSE,
                    convert.0and100 = TRUE,
                    sgp.quantiles = "Percentiles",
                    sgp.quantiles.labels = NULL,
                    percuts.digits = 0L,
                    isotonize = TRUE,
                    convert.using.loss.hoss = TRUE,
                    goodness.of.fit.output.format = "GROB",
                    return.prior.scale.score = TRUE,
                    return.norm.group.identifier = TRUE,
                    print.time.taken = TRUE,
                    sgp.percentiles.set.seed = 314159,
                    sgp.percentiles.equated = NULL
                )}
            ) |>
                call_function(studentGrowthPercentiles)

        if (any(
            tmp.tf <- sapply(results, \(x)
                      any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.percentiles.baseline =
                    getErrorReports(results, tmp.tf, rev(par.sgp.config[["sgp.percentiles.baseline"]])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)
    } ## END if sgp.percentiles.baseline

###############################################################################
###############################################################################
###      Straight and Lagged Projections (Cohort and Baseline Referenced)
###############################################################################
###############################################################################

    if (sgp.projections||sgp.projections.lagged) {
        cohort_ref_matrices <-
            .filter_matrices_for_projections(tmp_sgp_object[["Coefficient_Matrices"]], coefficient.matrix.type)
    }
    if (sgp.projections.baseline||sgp.projections.lagged.baseline) {
        bline_ref_matrices <-
            .filter_matrices_for_projections(tmp_sgp_object[["Coefficient_Matrices"]], "BASELINE")
    }

    call_function <- define_compute(parallel.config, "PROJECTIONS")

    ###   Straight, Cohort-Referenced Projections
    if (sgp.projections) {

        results <- lapply(
            par.sgp.config[["sgp.projections"]],
            \(sgp.iter) {
                list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                            tmp_sgp_data_for_analysis, "sgp.projections",
                            sgp.iter, sgp.scale.score.equated = equate.variable,
                            SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Knots_Boundaries = getKnotsBoundaries(sgp.iter, state, "sgp.projections"),
                        Coefficient_Matrices = cohort_ref_matrices),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.projection.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.projection.content.areas"]], 1),
                        my.grade = tail(sgp.iter[["sgp.projection.grade.sequences"]], 1)),
                    grade.progression = sgp.iter[["sgp.projection.grade.sequences"]],
                    content_area.progression = sgp.iter[["sgp.projection.content.areas"]],
                    year_lags.progression = sgp.iter[["sgp.projection.panel.years.lags"]],
                    grade.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    content_area.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    year_lags.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    max.forward.progression.years = sgp.iter[['sgp.projections.max.forward.progression.years']],
                    max.forward.progression.grade = sgp.projections.max.forward.progression.grade,
                    max.order.for.progression = getMaxOrderForProgression(
                        tail(sgp.iter[["sgp.projection.panel.years"]], 1),
                        tail(sgp.iter[["sgp.projection.content.areas"]], 1),
                        state, sgp.projections.equated),
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.projection.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.projection.content.areas"]], 1)),
                    use.my.coefficient.matrices = list(
                        my.year = tail(sgp.iter[["sgp.projection.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.projection.content.areas"]], 1),
                        my.extra.label = equate.label),
                    panel.data.vnames = getPanelDataVnames("sgp.projections", sgp.iter, sgp.data.names, equate.variable),
                    performance.level.cutscores = state,
                    calculate.sgps = !(tail(sgp.iter[["sgp.projection.panel.years"]], 1) %in%
                        SGPstateData[[state]][["Assessment_Program_Information"]][[
                            "Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]] &
                        is.null(sgp.projections.equated)),
                    sgp.projections.equated = sgp.projections.equated,
                    projection.unit = sgp.projections.projection.unit,
                    projection.unit.label = sgp.projections.projection.unit.label,
                    percentile.trajectory.values = unique(c(1, percentile.trajectory.values, 99)),
                    return.projection.group.identifier = sgp.iter[["sgp.projection.sequence"]],
                    return.projection.group.scale.scores = return.projection.group.scale.scores,
                    return.projection.group.dates = return.projection.group.dates,
                    sgp.exact.grade.progression = sgp.iter[["sgp.exact.grade.progression"]],
                    projcuts.digits = SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
                    sgp.projections.use.only.complete.matrices = sgp.projections.use.only.complete.matrices,
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.projections"),
                    ## Add in previously unspecified defaults too:
                    achievement.level.prior.vname = NULL,
                    convert.0and100 = TRUE,
                    trajectories.chunk.size = 50000L,
                    percentile.trajectory.values.max.forward.progression.years = NULL,
                    return.percentile.trajectory.values = NULL,
                    isotonize = TRUE,
                    lag.increment = 0L,
                    lag.increment.label = NULL
                )}
            ) |>
                call_function(studentGrowthProjections)

        if (any(
            tmp.tf <- sapply(results, \(x)
                any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.projections =
                    getErrorReports(results, tmp.tf, rev(par.sgp.config[["sgp.projections"]])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)
    } ## END if sgp.projections

        ###   Straight, Baseline-Referenced Projections
    if (sgp.projections.baseline) {

        results <- lapply(
            par.sgp.config[["sgp.projections.baseline"]],
            \(sgp.iter) {
                list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                            tmp_sgp_data_for_analysis, "sgp.projections.baseline",
                            sgp.iter, SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Knots_Boundaries = getKnotsBoundaries(sgp.iter, state, "sgp.projections.baseline"),
                        Coefficient_Matrices = bline_ref_matrices),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.projection.baseline.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.projection.baseline.content.areas"]], 1),
                        my.grade = tail(sgp.iter[["sgp.projection.baseline.grade.sequences"]], 1),
                        my.extra.label = "BASELINE"),
                    grade.progression = sgp.iter[["sgp.projection.baseline.grade.sequences"]],
                    content_area.progression = sgp.iter[["sgp.projection.baseline.content.areas"]],
                    year_lags.progression = sgp.iter[["sgp.projection.baseline.panel.years.lags"]],
                    grade.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    content_area.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    year_lags.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    max.forward.progression.years = sgp.iter[['sgp.projections.max.forward.progression.years']],
                    max.forward.progression.grade = sgp.projections.max.forward.progression.grade,
                    max.order.for.progression = sgp.projections.baseline.max.order,
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.projection.baseline.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.projection.baseline.content.areas"]], 1)),
                    use.my.coefficient.matrices = list(
                        my.year = "BASELINE",
                        my.subject = tail(sgp.iter[["sgp.projection.baseline.content.areas"]], 1)),
                    panel.data.vnames = getPanelDataVnames("sgp.projections.baseline", sgp.iter, sgp.data.names),
                    performance.level.cutscores = state,
                    calculate.sgps = !(tail(
                        sgp.iter[["sgp.projection.baseline.panel.years"]], 1) %in%
                        SGPstateData[[state]][["Assessment_Program_Information"]][[
                            "Scale_Change"]][[tail(sgp.iter[["sgp.projection.baseline.content.areas"]], 1)]]
                        & is.null(sgp.projections.equated)),
                    projection.unit = sgp.projections.projection.unit,
                    projection.unit.label = sgp.projections.projection.unit.label,
                    percentile.trajectory.values = unique(c(1, percentile.trajectory.values, 99)),
                    return.projection.group.identifier = sgp.iter[["sgp.projection.sequence"]],
                    return.projection.group.scale.scores = return.projection.group.scale.scores,
                    return.projection.group.dates = return.projection.group.dates,
                    sgp.exact.grade.progression = sgp.iter[["sgp.exact.grade.progression"]],
                    projcuts.digits = SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
                    sgp.projections.use.only.complete.matrices = sgp.projections.use.only.complete.matrices,
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.projections.baseline"),
                    ## Add in previously unspecified defaults too:
                    achievement.level.prior.vname = NULL,
                    convert.0and100 = TRUE,
                    trajectories.chunk.size = 50000L,
                    sgp.projections.equated = NULL,
                    percentile.trajectory.values.max.forward.progression.years = NULL,
                    return.percentile.trajectory.values = NULL,
                    isotonize = TRUE,
                    lag.increment = 0L,
                    lag.increment.label = NULL
                )}
            ) |>
                call_function(studentGrowthProjections)

        if (any(
            tmp.tf <- sapply(results, \(x)
                any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.projections.baseline =
                    getErrorReports(results, tmp.tf, rev(par.sgp.config[["sgp.projections.baseline"]])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)
    } ## END if sgp.projections.baseline

    call_function <- define_compute(parallel.config, "LAGGED_PROJECTIONS")

    ###   Lagged, Cohort-Referenced Projections
    if (sgp.projections.lagged) {

        results <- lapply(
            par.sgp.config[["sgp.projections.lagged"]],
            \(sgp.iter) {
                list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                            tmp_sgp_data_for_analysis, "sgp.projections.lagged",
                            sgp.iter, sgp.scale.score.equated = equate.variable,
                            SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Coefficient_Matrices = cohort_ref_matrices,
                        Knots_Boundaries =
                            getKnotsBoundaries(sgp.iter, state, "sgp.projections.lagged")),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1),
                        my.grade = tail(sgp.iter[["sgp.grade.sequences"]], 1),
                        my.extra.label = "LAGGED"),
                    grade.progression = sgp.iter[["sgp.projection.grade.sequences"]],
                    content_area.progression = sgp.iter[["sgp.projection.content.areas"]],
                    year_lags.progression = sgp.iter[["sgp.projection.panel.years.lags"]],
                    grade.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    content_area.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    year_lags.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    max.forward.progression.grade = sgp.projections.max.forward.progression.grade,
                    max.order.for.progression = getMaxOrderForProgression(
                        tail(sgp.iter[["sgp.panel.years"]], 1),
                        tail(sgp.iter[["sgp.content.areas"]], 1),
                        state, sgp.projections.equated),
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    use.my.coefficient.matrices = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1),
                        my.extra.label = equate.label),
                    panel.data.vnames =
                        getPanelDataVnames("sgp.projections.lagged", sgp.iter, sgp.data.names, equate.variable),
                    achievement.level.prior.vname =
                        paste("ACHIEVEMENT_LEVEL",
                            tail(head(sgp.iter[["sgp.panel.years"]], -1), 1),
                            tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep = "."),
                    performance.level.cutscores = state,
                    calculate.sgps = !(tail(sgp.iter[["sgp.panel.years"]], 1) %in%
                        SGPstateData[[state]][["Assessment_Program_Information"]][[
                            "Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]] &
                        is.null(sgp.projections.equated)),
                    sgp.projections.equated = sgp.projections.equated,
                    projection.unit = sgp.projections.projection.unit,
                    projection.unit.label = sgp.projections.projection.unit.label,
                    percentile.trajectory.values = lagged.percentile.trajectory.values,
                    return.projection.group.identifier = sgp.iter[["sgp.projection.sequence"]],
                    return.projection.group.scale.scores = return.projection.group.scale.scores,
                    return.projection.group.dates = return.projection.group.dates,
                    lag.increment = 1L,
                    sgp.exact.grade.progression = sgp.iter[["sgp.exact.grade.progression"]],
                    projcuts.digits = SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
                    sgp.projections.use.only.complete.matrices = sgp.projections.use.only.complete.matrices,
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.projections.lagged"),
                    ## Add in previously unspecified defaults too:
                    max.forward.progression.years = NULL,
                    convert.0and100 = TRUE,
                    trajectories.chunk.size = 50000L,
                    percentile.trajectory.values.max.forward.progression.years = NULL,
                    return.percentile.trajectory.values = NULL,
                    isotonize = TRUE,
                    lag.increment.label = NULL
                )}
            ) |>
                call_function(studentGrowthProjections)

        if (any(
            tmp.tf <- sapply(results, \(x)
                any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.projections.lagged =
                    getErrorReports(results, tmp.tf, rev(par.sgp.config[["sgp.projections.lagged"]])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)
    } ## END sgp.projections.lagged

        ###   Lagged, Baseline-Referenced Projections
    if (sgp.projections.lagged.baseline) {

        results <- lapply(
            par.sgp.config[["sgp.projections.lagged.baseline"]],
            \(sgp.iter) {
                list(
                    panel.data = list(
                        Panel_Data = getPanelData(
                            tmp_sgp_data_for_analysis, "sgp.projections.lagged.baseline",
                            sgp.iter, SGPt = SGPt, fix.duplicates = fix.duplicates),
                        Coefficient_Matrices = bline_ref_matrices,
                        Knots_Boundaries =
                            getKnotsBoundaries(sgp.iter, state, "sgp.projections.lagged.baseline")),
                    sgp.labels = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1),
                        my.grade = tail(sgp.iter[["sgp.grade.sequences"]], 1),
                        my.extra.label = "LAGGED.BASELINE"),
                    grade.progression = sgp.iter[["sgp.projection.baseline.grade.sequences"]],
                    content_area.progression = sgp.iter[["sgp.projection.baseline.content.areas"]],
                    year_lags.progression = sgp.iter[["sgp.projection.baseline.panel.years.lags"]],
                    grade.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    content_area.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    year_lags.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][[
                        "year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                    max.forward.progression.grade = sgp.projections.max.forward.progression.grade,
                    max.order.for.progression = sgp.projections.lagged.baseline.max.order,
                    use.my.knots.boundaries = list(
                        my.year = tail(sgp.iter[["sgp.panel.years"]], 1),
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    use.my.coefficient.matrices = list(
                        my.year = "BASELINE",
                        my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
                    panel.data.vnames =
                        getPanelDataVnames("sgp.projections.lagged.baseline", sgp.iter, sgp.data.names),
                    achievement.level.prior.vname =
                        paste("ACHIEVEMENT_LEVEL",
                            tail(head(sgp.iter[["sgp.panel.years"]], -1), 1),
                            tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep = "."),
                    performance.level.cutscores = state,
                    calculate.sgps = !(tail(sgp.iter[["sgp.panel.years"]], 1) %in%
                        SGPstateData[[state]][["Assessment_Program_Information"]][[
                            "Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]] &
                        is.null(sgp.projections.equated)),
                    projection.unit = sgp.projections.projection.unit,
                    projection.unit.label = sgp.projections.projection.unit.label,
                    percentile.trajectory.values = lagged.percentile.trajectory.values,
                    return.projection.group.identifier = sgp.iter[["sgp.projection.sequence"]],
                    return.projection.group.scale.scores = return.projection.group.scale.scores,
                    return.projection.group.dates = return.projection.group.dates,
                    lag.increment = 1L,
                    sgp.exact.grade.progression = sgp.iter[["sgp.exact.grade.progression"]],
                    projcuts.digits = SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
                    sgp.projections.use.only.complete.matrices = sgp.projections.use.only.complete.matrices,
                    SGPt = getSGPtNames(sgp.iter, SGPt, "sgp.projections.lagged"),
                    ## Add in previously unspecified defaults too:
                    max.forward.progression.years = NULL,
                    convert.0and100 = TRUE,
                    trajectories.chunk.size = 50000L,
                    sgp.projections.equated = NULL,
                    percentile.trajectory.values.max.forward.progression.years = NULL,
                    return.percentile.trajectory.values = NULL,
                    isotonize = TRUE,
                    lag.increment.label = NULL
                )}
            ) |>
                call_function(studentGrowthProjections)

        if (any(
            tmp.tf <- sapply(results, \(x)
                any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
        )) {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.projections.lagged.baseline =
                    getErrorReports(results, tmp.tf, rev(par.sgp.config[["sgp.projections.lagged.baseline"]])))
        }
        tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
        rm(results)
    } ## END sgp.projections.lagged.baseline

    tmp_sgp_object[["Panel_Data"]] <- NULL

	if (!is.null(sgp.test.cohort.size) & toupper(return.sgp.test.results) != "ALL_DATA") {
		if (!return.sgp.test.results) {
			messageSGP(paste("Finished analyzeSGP", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))
			return(sgp_object)
		} else {
			setkeyv(tmp_sgp_data_for_analysis, getKey(sgp_object@Data))
			sgp_object@Data <- tmp_sgp_data_for_analysis
		}
	}

	sgp_object@SGP <- mergeSGP(tmp_sgp_object, sgp_object@SGP)

    if (goodness.of.fit.print) {
      if (!is.null(sgp.config)) {
        years <- content_areas <- NULL
        for (cfig in seq(length(sgp.config))) {
          years <-
            unique(c(years, tail(sgp.config[[cfig]][["sgp.panel.years"]], 1)))
          content_areas <-
            unique(c(content_areas, tail(sgp.config[[cfig]][["sgp.content.areas"]], 1)))
        }
      }
      gofPrint(sgp_object = sgp_object,
               years = years,
               content_areas = content_areas,
               grades = grades)
    }
	setkeyv(sgp_object@Data, getKey(sgp_object)) # re-key data for combineSGP, etc.
	sgp_object@Version[["analyzeSGP"]][[as.character(gsub("-", "_", Sys.Date()))]] <- as.character(packageVersion("SGP"))
	messageSGP(paste("Finished analyzeSGP", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
