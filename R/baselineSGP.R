`baselineSGP` <-
function(sgp_object,
		state=NULL,
		years=NULL,
		content_areas=NULL,
		grades=NULL,
		exclude.years=NULL,
		sgp.config=NULL,
		sgp.baseline.config=NULL,
		sgp.baseline.panel.years=NULL,
		sgp.percentiles.baseline.max.order=3,
		return.matrices.only=FALSE,
		calculate.baseline.sgps=TRUE,
		calculate.simex.baseline=NULL,
		goodness.of.fit.print=TRUE,
		parallel.config=NULL,
		SGPt=NULL,
		...) {


	started.at <- proc.time()
	messageSGP(paste("\tStarted baselineSGP", prettyDate(), "\n"))

	VALID_CASE <- YEAR <- GRADE <- CONTENT_AREA <- YEAR_WITHIN <- COHORT_YEAR <- panel.data.vnames <- NULL ### To prevent R CMD check warnings

	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- getStateAbbreviation(tmp.name, "baselineSGP")
	}

	if (identical(calculate.simex.baseline, TRUE)) {
		calculate.simex.baseline <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=75, simex.sample.size=5000, extrapolation="linear", save.matrices=TRUE)
	}

	### Syncronize "return.matrices.only" and "calculate.baseline.sgps" arguments

	if (return.matrices.only & calculate.baseline.sgps) {
		messageSGP("\tArgument 'return.matrices.only=TRUE' obviates need to calculate baseline student growth percentiles. Argument 'calculate.baseline.sgps' will be set to FALSE")
		calculate.baseline.sgps <- FALSE
	}

	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]])) {
		sgp.loss.hoss.adjustment <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]]
	} else {
		sgp.loss.hoss.adjustment <- NULL
	}

	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["sgp.minimum.default.panel.years"]])) {
		sgp.minimum.default.panel.years <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["sgp.minimum.default.panel.years"]]
	} else sgp.minimum.default.panel.years <- 3

	if (!is.null(SGPt)) {
		if (identical(SGPt, TRUE)) SGPt <- "DATE"
		if (!all(SGPt %in% names(sgp_object@Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(SGPt, collapse=", "), "are not all contained in the supplied 'sgp_object@Data'. 'SGPt' is set to NULL.\n")
			SGPt <- NULL
		}
	}


	############################################
	###
	### Utility Functions
	###
	############################################

	baselineSGP_Internal <- function(sgp_object, state, years, content_areas, grade.sequences, baseline.grade.sequences.lags,
		knots.boundaries.iter, parallel.config, use.my.coefficient.matrices, simex.baseline.config, baseline.iter=NULL, vnames.passed.down, exclude.years) {

		started.at <- proc.time()
		started.date <- prettyDate()

		### Utility functions

		test.year.sequence <- function(content_areas, years, grades, baseline.grade.sequences.lags=NULL) {
			grades <- type.convert(as.character(grades), as.is=TRUE)
			if (is.null(baseline.grade.sequences.lags)) baseline.grade.sequences.lags <- rep(1L, length(grades)-1L)

			tmp.years.sequence <- list()
			tmp.years.sequence <- lapply(years, function(x) yearIncrement(year=x, increment=c(0,cumsum(baseline.grade.sequences.lags))))
			return(tmp.years.sequence[sapply(tmp.years.sequence, function(x) all(x %in% years))])
		} ### END test.year.sequence

		get.calculate.simex.arg <- function(simex.baseline.args, iter) {
			if (is.null(simex.baseline.args$csem.data.vnames)) return(simex.baseline.args)
			simex.baseline.args[['csem.data.vnames']] <- paste(simex.baseline.args$csem.data.vnames, rev(seq(iter[['sgp.baseline.grade.sequences']])), sep="_")
			return(simex.baseline.args)
		} ### END get.calculate.simex.arg

		### Get multiple-cohort data and stack up into 'Super-cohort'

#		tmp.dt <- createSuperCohortData(
#								dataForSuperCohort=sgp_object@Data,
#							 	content_areas=content_areas,
#								years=years,
#								grades=grades,
#								baseline.grade.sequence.lags=baseline.grade.sequence.lags,
#								exclude.years=exclude.years)


		variables.to.get <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", "LAST_OBSERVATION", simex.baseline.config$csem.data.vnames)
		tmp_sgp_data_for_analysis <- sgp_object@Data[,intersect(names(sgp_object@Data), variables.to.get), with=FALSE]["VALID_CASE"]
		if ("YEAR_WITHIN" %in% names(tmp_sgp_data_for_analysis)) {
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEAR_WITHIN)
			year_within.tf <- TRUE
		} else {
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
			year_within.tf <- FALSE
		}


		tmp.year.sequence <- test.year.sequence(content_areas, years, grade.sequences, baseline.grade.sequences.lags)
		if (!is.null(exclude.years)) {
			tmp.year.sequence <- tmp.year.sequence[sapply(tmp.year.sequence, function(x) !tail(x, 1) %in% exclude.years)]
		}
		tmp.list <- list()
		for (k in seq_along(tmp.year.sequence)) {
			tmp.sgp.iter <- sgp.baseline.config[[iter]] # Convert sgp.baseline.config into a valid sgp.iter for getPanelData
			names(tmp.sgp.iter) <- gsub('sgp.baseline.', 'sgp.', names(tmp.sgp.iter))
			tmp.sgp.iter$sgp.panel.years <- tmp.year.sequence[[k]]
			tmp.sgp.iter$sgp.grade.sequences <- tmp.sgp.iter$sgp.grade.sequences
			if (!is.null(tmp.sgp.iter$sgp.exclude.sequences)) tmp.sgp.iter$sgp.exclude.sequences <- tmp.sgp.iter$sgp.exclude.sequences[COHORT_YEAR %in% tail(tmp.sgp.iter$sgp.panel.years, 1L)]
			if (!is.null(simex.baseline.config))  sgp.csem <- simex.baseline.config$csem.data.vnames else sgp.csem <- NULL
			tmp.list[[k]] <- getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter = tmp.sgp.iter, sgp.csem=sgp.csem)
			setnames(tmp.list[[k]], c("ID",
				paste("GRADE", rev(seq_along(tmp.year.sequence[[k]])), sep="_"),
				paste("SCALE_SCORE", rev(seq_along(tmp.year.sequence[[k]])), sep="_"),
				if(year_within.tf) paste("YEAR_WITHIN", rev(seq_along(tmp.year.sequence[[k]])), sep="_"),
				if(!is.null(sgp.csem)) paste(sgp.csem, rev(seq_along(tmp.year.sequence[[k]])), sep="_")))
		}
		tmp.dt <- rbindlist(tmp.list, fill=TRUE)
		if (year_within.tf) tmp.dt[, grep("YEAR_WITHIN", names(tmp.dt)) := NULL] # remove YEAR_WITHIN from Data where relevant
		setkey(tmp.dt)
		tmp.panel.vnames <- c("ID",
			paste("GRADE", rev(seq_along(tmp.year.sequence[[k]])), sep="_"),
			paste("SCALE_SCORE", rev(seq_along(tmp.year.sequence[[k]])), sep="_"))


		### Calculate Coefficient Matrices and return list containing coefficient matrices

		if (!is.null(simex.baseline.config)) {
			TMP_Coefficient_Matrices = sgp_object@SGP[["Coefficient_Matrices"]]
			tmp.simex.args <- get.calculate.simex.arg(simex.baseline.config, baseline.iter)
		} else {
			TMP_Coefficient_Matrices <- list()
			tmp.simex.args <- NULL
		}

		tmp_sgp_list <- list(Coefficient_Matrices =
			studentGrowthPercentiles(
				panel.data=list(Panel_Data=tmp.dt, Coefficient_Matrices=TMP_Coefficient_Matrices, # Add Coef Matrices for SIMEX
					Knots_Boundaries=getKnotsBoundaries(knots.boundaries.iter, state, "sgp.percentiles.baseline", "BASELINE")),
				sgp.labels=list(my.year="BASELINE", my.subject=tail(content_areas, 1L)),
				if (!vnames.passed.down & !is.null(sgp.csem)) panel.data.vnames=grep(sgp.csem, names(tmp.dt), invert=TRUE, value=TRUE),
				use.my.knots.boundaries=list(my.year="BASELINE", my.subject=tail(content_areas, 1L)),
				use.my.coefficient.matrices= use.my.coefficient.matrices,
				calculate.sgps=FALSE,
				goodness.of.fit=FALSE,
				drop.nonsequential.grade.progression.variables=FALSE,
				grade.progression=grade.sequences,
				content_area.progression=content_areas,
				year.progression=rep("BASELINE", length(content_areas)),
				year_lags.progression=baseline.grade.sequences.lags,
				exact.grade.progression.sequence=TRUE,
				print.time.taken=FALSE,
				parallel.config=parallel.config,
				calculate.simex=tmp.simex.args,
				SGPt=SGPt,
				...)[["Coefficient_Matrices"]])

		messageSGP(paste("\tStarted baselineSGP Coefficient Matrix Calculation:", started.date))
		messageSGP(paste0("\tContent Area: ", tail(content_areas, 1L), ", Grade Progression: ", paste(grade.sequences, collapse=", "), ". "))
		messageSGP(paste0("\tFinished baselineSGP Coefficient Matrix Calculation ", prettyDate(), " in ", convertTime(timetakenSGP(started.at)), ".\n"))

		return(tmp_sgp_list)
	} ### END baselineSGP_Internal Function


	gof.print <- function(sgp_object) {
		if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
				dir.create(paste0("Goodness_of_Fit/", i), recursive=TRUE, showWarnings=FALSE)
				for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
					pdf(file=paste0("Goodness_of_Fit/", i, "/", j, ".pdf"), width=8.5, height=4.5)
					grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
					dev.off()
				}
			}
		} else {
			messageSGP("\tNOTE: No Goodness of Fit tables available to print. No tables will be produced.")
		}
	}


	#################################################################################
	###
	### Calculate/Retrieve baseline coefficient matrices if requested
	###
	#################################################################################

	vnames.arg.tf <- hasArg(panel.data.vnames)

	if (is.null(SGP::SGPstateData[[state]][["Baseline_splineMatrix"]])) {

		if (is.null(sgp.baseline.config)) {
			sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years,
				sgp.percentiles.baseline.max.order = sgp.percentiles.baseline.max.order, calculate.simex.baseline = calculate.simex.baseline)
		} else {
			sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
		}

		tmp.list <- list()

		for (iter in seq_along(sgp.baseline.config)) {
			tmp.list[[iter]] <- baselineSGP_Internal(
							sgp_object,
							state=state,
							years=sgp.baseline.config[[iter]][["sgp.baseline.panel.years"]],
							content_areas=sgp.baseline.config[[iter]][["sgp.baseline.content.areas"]],
							grade.sequences=sgp.baseline.config[[iter]][["sgp.baseline.grade.sequences"]],
							baseline.grade.sequences.lags=sgp.baseline.config[[iter]][["sgp.baseline.grade.sequences.lags"]],
							knots.boundaries.iter=sgp.baseline.config[[iter]],
							use.my.coefficient.matrices=NULL,
							parallel.config=parallel.config,
							simex.baseline.config=NULL,
							vnames.passed.down=vnames.arg.tf,
							exclude.years=exclude.years)
		}

		sgp_object@SGP <- mergeSGP(Reduce(mergeSGP, tmp.list), sgp_object@SGP)
	} else {
		sgp_object@SGP <- mergeSGP(sgp_object@SGP, SGP::SGPstateData[[state]][["Baseline_splineMatrix"]])
	}


	#################################################################################
	###
	###    Calculate SIMEX baseline referenced coefficient matrices if requested
	###
	#################################################################################

	if (!is.null(calculate.simex.baseline)) {
		if (is.null(sgp.baseline.config)) {
			sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years,
				sgp.percentiles.baseline.max.order = sgp.percentiles.baseline.max.order, calculate.simex.baseline = calculate.simex.baseline)
		} else {
			sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
		}

		tmp.list <- list()

		for (iter in seq_along(sgp.baseline.config)) {
			tmp.list[[iter]] <- baselineSGP_Internal(
					sgp_object,
					state=state,
					years=sgp.baseline.config[[iter]][["sgp.baseline.panel.years"]],
					content_areas=sgp.baseline.config[[iter]][["sgp.baseline.content.areas"]],
					grade.sequences=sgp.baseline.config[[iter]][["sgp.baseline.grade.sequences"]],
					baseline.grade.sequences.lags=sgp.baseline.config[[iter]][["sgp.baseline.grade.sequences.lags"]],
					knots.boundaries.iter=sgp.baseline.config[[iter]],
					parallel.config=parallel.config,
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.baseline.config[[iter]][["sgp.baseline.content.areas"]], 1L)),
					simex.baseline.config=calculate.simex.baseline,
					baseline.iter = sgp.baseline.config[[iter]],
					vnames.passed.down=vnames.arg.tf,
					exclude.years=exclude.years)
		}

		sgp_object@SGP <- mergeSGP(Reduce(mergeSGP, tmp.list), sgp_object@SGP)

	}


	################################################################
	###
	### Calculate baseline referenced student growth percentiles
	###
	################################################################

	if (calculate.baseline.sgps) {

		tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

		variables.to.get <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", "LAST_OBSERVATION")
		tmp_sgp_data_for_analysis <- sgp_object@Data[,intersect(names(sgp_object@Data), variables.to.get), with=FALSE]
		if ("YEAR_WITHIN" %in% names(tmp_sgp_data_for_analysis)) {
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEAR_WITHIN)
		} else {
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
		}

		par.sgp.config <- getSGPConfig(sgp_object, state, tmp_sgp_object, content_areas, years, grades, sgp.config, trim.sgp.config=TRUE,
			sgp.percentiles=FALSE, sgp.projections=FALSE, sgp.projections.lagged=FALSE,
			sgp.percentiles.baseline=TRUE, sgp.projections.baseline=FALSE, sgp.projections.lagged.baseline=FALSE,
			sgp.config.drop.nonsequential.grade.progression.variables=TRUE, sgp.minimum.default.panel.years=sgp.minimum.default.panel.years,
			sgp.projections.max.forward.progression.years=NULL, sgp.use.my.coefficient.matrices=NULL, calculate.simex.baseline=calculate.simex.baseline,
			SGPt=SGPt)

		for (sgp.iter in par.sgp.config[['sgp.percentiles.baseline']]) {

			panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter)))
			tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, "sgp.percentiles") # Get specific knots and boundaries in case course sequence is different
			panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

			tmp_sgp_object <- studentGrowthPercentiles(
				panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1L),
						my.subject=tail(sgp.iter[['sgp.content.areas']], 1L), my.extra.label="BASELINE"),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1L), my.subject=tail(sgp.iter[['sgp.content.areas']], 1L)),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[['sgp.content.areas']], 1L)),
					growth.levels=state,
					panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter, names(tmp_sgp_data_for_analysis)),
					grade.progression=sgp.iter[['sgp.grade.sequences']],
					content_area.progression=tail(sgp.iter[['sgp.content.areas']], min(sgp.iter[['sgp.baseline.max.order']], sgp.percentiles.baseline.max.order)+1),
					year.progression=rep("BASELINE", length(sgp.iter[['sgp.content.areas']])),
					num.prior =min(sgp.iter[['sgp.baseline.max.order']], sgp.percentiles.baseline.max.order),
					percentile.cuts=SGP::SGPstateData[[state]][['SGP_Configuration']][['percentile.cuts']],
					drop.nonsequential.grade.progression.variables=FALSE,
					exact.grade.progression.sequence=sgp.iter[['sgp.exact.grade.progression']],
					sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
					SGPt=SGPt,
					...)

		} ### END sgp.iter loop

		sgp_object@SGP <- mergeSGP(sgp_object@SGP, tmp_sgp_object)

		if (goodness.of.fit.print) gof.print(sgp_object)

	} ### END if (calculate.baseline.sgps)


	############################################################
	###
	### Return results
	###
	############################################################

	messageSGP(paste("\tFinished baselineSGP", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))

	if (return.matrices.only) {
		tmp.list <- list()
		if (is.null(SGP::SGPstateData[[state]][["Baseline_splineMatrix"]])) {
			for (ca in unique(sapply(sgp.baseline.config, function(x) tail(x[["sgp.baseline.content.areas"]],1L)))) {
				tmp.list[[paste0(ca, ".BASELINE")]] <- sgp_object@SGP[["Coefficient_Matrices"]][[paste0(ca, ".BASELINE")]]
			}
		}
		if (!is.null(calculate.simex.baseline)) {
			for (ca in unique(sapply(sgp.baseline.config, function(x) tail(x[["sgp.baseline.content.areas"]],1L)))) {
				tmp.list[[paste0(ca, ".BASELINE.SIMEX")]] <- sgp_object@SGP[["Coefficient_Matrices"]][[paste0(ca, ".BASELINE.SIMEX")]]
			}
		}
		return(tmp.list)
	} else {
		return(sgp_object)
	}
} ### END baselineSGP Function
