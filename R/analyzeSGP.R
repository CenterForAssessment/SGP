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
         sgp.projections.baseline.max.order=3,
         sgp.projections.lagged.baseline.max.order=3,
	 sgp.projections.max.forward.progression.years=3,
	 sgp.projections.max.forward.progression.grade=NULL,
         sgp.minimum.default.panel.years=NULL,
         sgp.use.my.coefficient.matrices=NULL,
         simulate.sgps=TRUE,
         calculate.simex=NULL,
         calculate.simex.baseline=NULL,
         goodness.of.fit.print=TRUE,
         sgp.config=NULL,
         sgp.config.drop.nonsequential.grade.progression.variables=TRUE,
         sgp.baseline.panel.years=NULL,
         sgp.baseline.config=NULL, 
         parallel.config=NULL,
         verbose.output=FALSE,
         print.other.gp=FALSE,
         get.cohort.data.info=FALSE,
         ...) {

	started.at <- proc.time()
	message(paste("\nStarted analyzeSGP", date()))

	VALID_CASE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- YEAR_WITHIN <- NULL


	###
	### Create relevant analyzeSGP variables
	###

	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- getStateAbbreviation(tmp.name, "analyzeSGP")
	}

	
	###
	### Tests associated with provided arguments
	###

	if (simulate.sgps==TRUE) {
		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			message("\tNOTE: CSEMs are required in SGPstateData to simulate SGPs for confidence interval calculations. Confidence intervals will not be calculated.")
			calculate.confidence.intervals <- NULL
		} else {
			calculate.confidence.intervals <- state
		}
	} else {
		calculate.confidence.intervals <- NULL
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

	if (!is.null(SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]])) {
		percentile.trajectory.values <- SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]]
	} else {
		percentile.trajectory.values <- c(35, 50, 65)
	}

	if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Projection_Fan_Limits"]])) {
		percentile.trajectory.values <- sort(c(SGPstateData[[state]][["Student_Report_Information"]][["Projection_Fan_Limits"]], percentile.trajectory.values))
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

	if (!is.null(sgp.config) && sgp.config.drop.nonsequential.grade.progression.variables) {
		sgp.config.drop.nonsequential.grade.progression.variables <- FALSE
	}

	if ((sgp.projections | sgp.projections.lagged | sgp.projections.baseline | sgp.projections.lagged.baseline) & is.null(SGPstateData[[state]][["Achievement"]][["Cutscores"]])) {
		message(paste("\tNOTE: Achievement Level cutscores for state, ", state, ", are not in embedded SGPstateData. Projections and Lagged Projections will not be calculated"))
		sgp.projections <- sgp.projections.lagged <- sgp.projections.baseline <- sgp.projections.lagged.baseline <- FALSE
	}
	
	if (all(c("PERCENTILES", "TAUS") %in% names(parallel.config[['WORKERS']]))) stop("Both TAUS and PERCENTILES can not be executed in Parallel at the same time.")
	if (all(c("PERCENTILES", "SIMEX") %in% names(parallel.config[['WORKERS']]))) stop("Both SIMEX and PERCENTILES can not be executed in Parallel at the same time.")
	
	if (any(c("SIMEX", "TAUS") %in% names(parallel.config[['WORKERS']]))) {
		lower.level.parallel.config <- parallel.config
		parallel.config <- NULL
	} else lower.level.parallel.config <- NULL

	if (identical(calculate.simex, TRUE)) {
		calculate.simex <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=50, simex.sample.size=25000, extrapolation="linear", save.matrices=TRUE)
	}

	if (identical(calculate.simex.baseline, TRUE)) {
		calculate.simex.baseline <- list(state=state, lambda=seq(0,2,0.5), simulation.iterations=50, simex.sample.size=25000, extrapolation="linear", save.matrices=TRUE)
	}

	if (is.null(sgp.minimum.default.panel.years) & !is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.minimum.default.panel.years']])) {
		sgp.minimum.default.panel.years <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.minimum.default.panel.years']]
	}

	if (is.null(sgp.minimum.default.panel.years) & is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.minimum.default.panel.years']])) {
		sgp.minimum.default.panel.years <- 3
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.grade']])) {
		sgp.projections.max.forward.progression.grade <- SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.max.forward.progression.grade']]
	}
	
	### 
	### Utility functions
	###

	## Function to export/print goodness of fit results as pdf files to directory Goodness_of_Fit

	gof.print <- function(sgp_object) {
		if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
				dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE, showWarnings=FALSE)
					for (output.format in c("PDF", "PNG")) {
						for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
							if (output.format=="PDF") {
								pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=11)
							}
							if (output.format=="PNG") {
								Cairo(file=paste("Goodness_of_Fit/", i, "/", j, ".png", sep=""), 
								      width=8.5, height=11, units="in", dpi=144, pointsize=24, bg="transparent")
							}
							grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
							dev.off()
						}
					}
				}
		} else {
			message("\tNOTE: No Goodness of Fit tables available to print. No tables will be produced.")
		}
	}

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


	#######################################################################################################################
	##   Set up the temporary sgp list object.  Fill with necessary old results if they exist first.
	##   Create subset of @Data containing essential data elements for analyses
	#######################################################################################################################

	tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

	variables.to.get <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", "LAST_OBSERVATION")
	tmp_sgp_data_for_analysis <- sgp_object@Data[,intersect(names(sgp_object@Data), variables.to.get), with=FALSE]
	if ("YEAR_WITHIN" %in% names(tmp_sgp_data_for_analysis)) {
		setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEAR_WITHIN)
	} else {
		setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
	}


	#######################################################################################################################
	##   Baseline SGP - compute matrices first if they are not in SGPstateData or merge them into sgp_object if in SGPstateData
	#######################################################################################################################

	if (sgp.percentiles.baseline) {

		if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
			if (is.null(sgp.baseline.config)) {
				sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years, sgp.percentiles.baseline.max.order)
			} else {
				sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
			}
		
			message("\n\tStarted Baseline Coefficient Matrix Calculation:\n")
			
			if (!is.null(parallel.config)) { ### PARALLEL BASELINE COEFFICIENT MATRIX CONSTRUCTION
				par.start <- startParallel(parallel.config, 'BASELINE_MATRICES')

				###  FOREACH flavor
				if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
					tmp <- foreach(sgp.iter=iter(sgp.baseline.config), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE))
					}
					tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
					rm(tmp)
				} else {
					if (par.start$par.type=="SNOW") {
						tmp <- clusterApplyLB(par.start$internal.cl, sgp.baseline.config, function(sgp.iter) baselineSGP(
								sgp_object,
								state=state,
								sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
								return.matrices.only=TRUE,
								calculate.baseline.sgps=FALSE))
					
						tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
						rm(tmp)
					} # END if (SNOW)
						
					if (par.start$par.type=="MULTICORE") {
						tmp <- mclapply(sgp.baseline.config, function(sgp.iter) baselineSGP(
									sgp_object,
									state=state,
									sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
									return.matrices.only=TRUE,
									calculate.baseline.sgps=FALSE),
								mc.cores=par.start$workers, mc.preschedule=FALSE)
						tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
						rm(tmp)
					} # END if (MULTICORE)
					stopParallel(parallel.config, par.start)
				} #  END  if parallel
			} else { 
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
			}

			assign(paste(state, "_Baseline_Matrices", sep=""), list())
			for (tmp.matrix.label in grep("BASELINE", names(tmp_sgp_object$Coefficient_Matrices), value=TRUE)) {
				eval(parse(text=paste(state, "_Baseline_Matrices[['", tmp.matrix.label, "']] <- tmp_sgp_object[['Coefficient_Matrices']][['", tmp.matrix.label, "']]", sep="")))
			}
			save(list=paste(state, "_Baseline_Matrices", sep=""), file=paste(state, "_Baseline_Matrices.Rdata", sep=""))
			message("\n\tFinished Calculating Baseline Coefficient Matrices\n")
		} else {
			tmp_sgp_object <- mergeSGP(tmp_sgp_object, SGPstateData[[state]][["Baseline_splineMatrix"]])
		}
		suppressMessages(gc()) # clean up
	} # END Get/Compute baseline coefficient matrices


	#######################################################################################################################
	##   SIMEX Baseline SGP - compute matrices first if they are not in sgp_object (NOTE: Not stored in SGPstateData)
	#######################################################################################################################

	if (sgp.percentiles.baseline & !is.null(calculate.simex.baseline)) {

		if (is.null(sgp.baseline.config)) {
			sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years, sgp.percentiles.baseline.max.order)
		} else {
			sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
		}

		tmp.subjects <- unique(sapply(sgp.baseline.config, function(x) tail(x[["sgp.baseline.content.areas"]],1)))

		###  Calculate BASELINE SIMEX matrices if they are not present
		if (!all(find.matrices <- paste(tmp.subjects, ".BASELINE.SIMEX", sep="") %in% names(tmp_sgp_object[["Coefficient_Matrices"]]))) {
			tmp.subjects <- tmp.subjects[!find.matrices]
			sgp.baseline.config <- sgp.baseline.config[which(sapply(sgp.baseline.config, function(x) tail(x[["sgp.baseline.content.areas"]],1)) %in% tmp.subjects)]

			message("\n\tStarted SIMEX Baseline Coefficient Matrix Calculation:\n")
			
			##  Enforce that simex.use.my.coefficient.matrices must be FALSE for BASELINE SIMEX matrix production
			calculate.simex.baseline$simex.use.my.coefficient.matrices <- NULL
			
			if (!is.null(parallel.config)) { ### PARALLEL BASELINE COEFFICIENT MATRIX CONSTRUCTION
				
				par.start <- startParallel(parallel.config, 'BASELINE_MATRICES')

				##  FOREACH flavor
				if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
					tmp <- foreach(sgp.iter=iter(sgp.baseline.config), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE,
							calculate.baseline.simex=calculate.simex.baseline,
							parallel.config=parallel.config))
					}
					tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp, simex=TRUE)))
					rm(tmp)
				} else {  ## SNOW and MULTICORE flavors
					if (par.start$par.type=="SNOW") {
						tmp <- clusterApplyLB(par.start$internal.cl, sgp.baseline.config, function(sgp.iter) baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE,
							calculate.baseline.simex=calculate.simex.baseline,
							parallel.config=parallel.config))
					
						tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp, simex=TRUE)))
						rm(tmp)
					} # END if (SNOW)
						
					if (par.start$par.type=="MULTICORE") {
						tmp <- mclapply(sgp.baseline.config, function(sgp.iter) baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE,
							calculate.baseline.simex=calculate.simex.baseline,
							parallel.config=parallel.config),
							mc.cores=par.start$workers, mc.preschedule=FALSE)
							
						tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp, simex=TRUE)))
						rm(tmp)
					} # END if (MULTICORE)
					stopParallel(parallel.config, par.start)
				} #  END FOREACH, SNOW and MULTICORE
			} else { 
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
						calculate.baseline.simex=calculate.simex.baseline,
						parallel.config=lower.level.parallel.config)
				}
				
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp, simex=TRUE)))
				rm(tmp)
			}
			
			###  Save SIMEX BASELINE matrices
			assign(paste(state, "_SIMEX_Baseline_Matrices", sep=""), list())
			for (tmp.matrix.label in grep("BASELINE.SIMEX", names(tmp_sgp_object$Coefficient_Matrices), value=TRUE)) {
				eval(parse(text=paste(state, "_SIMEX_Baseline_Matrices[['", tmp.matrix.label, "']] <- tmp_sgp_object[['Coefficient_Matrices']][['", tmp.matrix.label, "']]", sep="")))
			}
			save(list=paste(state, "_SIMEX_Baseline_Matrices", sep=""), file=paste(state, "_SIMEX_Baseline_Matrices.Rdata", sep=""), compress="xz")
			
			message("\n\tFinished Calculating SIMEX Baseline Coefficient Matrices\n")
			suppressMessages(gc()) # clean up
		} # END Compute SIMEX baseline coefficient matrices
		
		##  Enforce that simex.use.my.coefficient.matrices must be TRUE for BASELINE SIMEX calculations below
		calculate.simex.baseline$simex.use.my.coefficient.matrices <- TRUE
		
	} # END check for SIMEX baseline matrices presence


	### Create par.sgp.config (for both parallel and sequential implementations)

	setkeyv(sgp_object@Data, getKey(sgp_object))
	par.sgp.config <- getSGPConfig(sgp_object, tmp_sgp_object, content_areas, years, grades, sgp.config, sgp.percentiles, sgp.projections, sgp.projections.lagged,
		sgp.percentiles.baseline, sgp.projections.baseline, sgp.projections.lagged.baseline, sgp.config.drop.nonsequential.grade.progression.variables, sgp.minimum.default.panel.years)

	if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {
		if (any(sapply(par.sgp.config, function(x) identical(x[['sgp.baseline.grade.sequences']], "NO_BASELINE_COEFFICIENT_MATRICES")))) {
			baseline.missings <- which(sapply(par.sgp.config, function(x) identical(x[['sgp.baseline.grade.sequences']], "NO_BASELINE_COEFFICIENT_MATRICES")))
			baseline.missings <- paste(unlist(sapply(baseline.missings, function(x) 
				paste(tail(par.sgp.config[[x]]$sgp.content.areas, 1), paste(par.sgp.config[[x]]$sgp.grade.sequences, collapse=", "), sep=": "))), collapse=";\n\t\t")
			message("\tNOTE: Baseline coefficient matrices are not available for:\n\t\t", baseline.missings, ".", sep="")
		}
		par.sgp.config.baseline <- par.sgp.config[which(sapply(par.sgp.config, function(x) !identical(x[['sgp.baseline.grade.sequences']], "NO_BASELINE_COEFFICIENT_MATRICES")))]
	}


	### Produce cohort data information

	if (get.cohort.data.info) {
		cohort_data_info <- getCohortDataInfo(tmp_sgp_data_for_analysis, par.sgp.config)
		save(cohort_data_info, file="cohort_data_info.Rdata")
	}


	#######################################################################################################################
	#######################################################################################################################
	##   Percentiles, Baseline Percentiles, Projections, Lagged Projections -  PARALLEL FLAVORS FIRST
	#######################################################################################################################
	#######################################################################################################################

	if (!is.null(parallel.config)) {

	##################################		
	###  PERCENTILES
	##################################

		if (sgp.percentiles) {
			par.start <- startParallel(parallel.config, 'PERCENTILES')
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(rev(par.sgp.config)), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthPercentiles(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.content.areas"]],
						year.progression=sgp.iter[["sgp.panel.years"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						sgp.cohort.size=SGPstateData[[state]][["SGP_Configuration"]][["sgp.cohort.size"]],
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						calculate.simex=calculate.simex,
						...))
					}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else { # END FOREACH
				###    SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, rev(par.sgp.config), 	function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.content.areas"]],
						year.progression=sgp.iter[["sgp.panel.years"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						sgp.cohort.size=SGPstateData[[state]][["SGP_Configuration"]][["sgp.cohort.size"]],
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						calculate.simex=calculate.simex,
						...))

					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.=getErrorReports(tmp, tmp.tf, rev(par.sgp.config)))
					}
					rm(tmp)
				} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(rev(par.sgp.config), function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.content.areas"]],
						year.progression=sgp.iter[["sgp.panel.years"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						sgp.cohort.size=SGPstateData[[state]][["SGP_Configuration"]][["sgp.cohort.size"]],
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						calculate.simex=calculate.simex,
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)

					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.=getErrorReports(tmp, tmp.tf, rev(par.sgp.config)))
					}
					rm(tmp)
				} # End MULTICORE
			} # #END not FOREACH
			stopParallel(parallel.config, par.start)
			suppressMessages(gc()) # clean up
		} #END if (sgp.percentiles)


	####################################
	###  BASELINE PERCENTILES
	####################################

		if (sgp.percentiles.baseline) {

			par.start <- startParallel(parallel.config, 'BASELINE_PERCENTILES')

			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(rev(par.sgp.config.baseline)), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthPercentiles(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles.baseline", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.baseline.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.baseline.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.baseline.panel.years.lags"]],
						num.prior=min(sgp.iter[["sgp.baseline.max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						calculate.simex=calculate.simex.baseline,
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else { # END FOREACH	
				###    SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, rev(par.sgp.config.baseline), 	function(sgp.iter)	studentGrowthPercentiles(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles.baseline", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.baseline.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.baseline.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.baseline.panel.years.lags"]],
						num.prior=min(sgp.iter[["sgp.baseline.max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						calculate.simex=calculate.simex.baseline,
						...))
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.baseline.=getErrorReports(tmp, tmp.tf, rev(par.sgp.config.baseline)))
					}
					rm(tmp)
				} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(rev(par.sgp.config.baseline), function(sgp.iter)	studentGrowthPercentiles(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles.baseline", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.baseline.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.baseline.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.baseline.panel.years.lags"]],
						num.prior=min(sgp.iter[["sgp.baseline.max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						calculate.simex=calculate.simex.baseline,
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.baseline.=getErrorReports(tmp, tmp.tf, rev(par.sgp.config.baseline)))
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.percentiles.baseline


	#######################################################
	###  PROJECTIONS (COHORT referenced)
	#######################################################

		if (sgp.projections) {
		
			par.start <- startParallel(parallel.config, 'PROJECTIONS')
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.projection.content.areas"]], 1), state),
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter),
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.projection.content.areas"]], 1), state),
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.=getErrorReports(tmp, tmp.tf, par.sgp.config))
					}
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.projection.content.areas"]], 1), state),
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.=getErrorReports(tmp, tmp.tf, par.sgp.config))
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections


	#######################################################
	###  PROJECTIONS (BASELINE referenced)
	#######################################################

		if (sgp.projections.baseline) {
			par.start <- startParallel(parallel.config, 'PROJECTIONS')

			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config.baseline), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.baseline.=getErrorReports(tmp, tmp.tf, par.sgp.config.baseline))
					}
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config.baseline, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.baseline.=getErrorReports(tmp, tmp.tf, par.sgp.config.baseline))
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.baseline


	#################################################
	###  LAGGED PROJECTIONS (COHORT Referenced)
	#################################################

		if (sgp.projections.lagged) {
			par.start <- startParallel(parallel.config, 'LAGGED_PROJECTIONS')
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						lag.increment=1,
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						lag.increment=1,
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.=getErrorReports(tmp, tmp.tf, par.sgp.config))
					}
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						lag.increment=1,
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.=getErrorReports(tmp, tmp.tf, par.sgp.config))
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged


	#################################################
	###  LAGGED PROJECTIONS (BASELINE Referenced)
	#################################################

		if (sgp.projections.lagged.baseline) {
			par.start <- startParallel(parallel.config, 'LAGGED_PROJECTIONS')

			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config.baseline), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=sgp.projections.lagged.baseline.max.order,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						lag.increment=1,
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH

			###  SNOW flavor
			if (par.start$par.type == 'SNOW') {
				tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(
						Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
						Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
						Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
					grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
					content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
					year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
					lag.increment=1,
					grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
					content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
					year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...))

				tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
				if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
					tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.baseline.=getErrorReports(tmp, tmp.tf, par.sgp.config.baseline))
				}
				rm(tmp)
			} # END SNOW
			
			###  MULTICORE flavor
			if (par.start$par.type == 'MULTICORE') {
				tmp <- mclapply(par.sgp.config.baseline, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(
						Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
						Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], 
						Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
					grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
					content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
					year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
					lag.increment=1,
					grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
					content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
					year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...), mc.cores=par.start$workers, mc.preschedule=FALSE)

				tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
				if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
					tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.baseline.=getErrorReports(tmp, tmp.tf, par.sgp.config.baseline))
				}
				rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged.baseline
	}  ## END if (!is.null(parallel.config))


	################################################################
	################################################################
	###	SEQUENTIAL OPTION (NON-Parallel Option)
	################################################################
	################################################################

	if (is.null(parallel.config)) {

		### sgp.percentiles
			
		if (sgp.percentiles) {
			for (sgp.iter in rev(par.sgp.config)) {

				panel.data <- within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (dim(panel.data$Panel_Data)[1] > 0 & dim(panel.data$Panel_Data)[2] > 3) {
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.content.areas"]],
						year.progression=sgp.iter[["sgp.panel.years"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						calculate.confidence.intervals=calculate.confidence.intervals,
						drop.nonsequential.grade.progression.variables=FALSE,
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						sgp.cohort.size=SGPstateData[[state]][["SGP_Configuration"]][["sgp.cohort.size"]],
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						parallel.config=lower.level.parallel.config,
						calculate.simex=calculate.simex,
						...)
				} else {
					message(paste("\n\t\tNOTE: No student records &/or no prior data for student growth percentiles:", tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.content.areas"]], 1), "with", paste(head(sgp.iter[["sgp.content.areas"]], -1), collapse=", "), "priors.\n"))
				}
			}
			suppressMessages(gc())
		} ## END if sgp.percentiles


		## sgp.percentiles.baseline

		if (sgp.percentiles.baseline) {
			for (sgp.iter in rev(par.sgp.config.baseline)) {
				
				panel.data <- within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.percentiles"))
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (dim(panel.data$Panel_Data)[1] > 0 & dim(panel.data$Panel_Data)[2] > 3) {
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles.baseline", sgp.iter, names(tmp_sgp_data_for_analysis)),
						additional.vnames.to.return=getPanelDataVnames("sgp.percentiles.to.return", sgp.iter, names(tmp_sgp_data_for_analysis)),
						grade.progression=sgp.iter[["sgp.baseline.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.baseline.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.baseline.panel.years.lags"]],
						num.prior=min(sgp.iter[["sgp.baseline.max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						calculate.confidence.intervals=calculate.confidence.intervals,
						drop.nonsequential.grade.progression.variables=FALSE,
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						return.norm.group.scale.scores=return.norm.group.scale.scores,
						return.prior.scale.score.standardized=return.prior.scale.score.standardized,
						goodness.of.fit=state,
						goodness.of.fit.minimum.n=SGPstateData[[state]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]],
						verbose.output=verbose.output,
						print.other.gp=print.other.gp,
						parallel.config=lower.level.parallel.config,
						calculate.simex=calculate.simex.baseline,
						...)
				} else {
					message(paste("\n\t\tNOTE: No student records &/or no prior data for baseline student growth percentiles:", tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.content.areas"]], 1), "with", paste(head(sgp.iter[["sgp.content.areas"]], -1), collapse=", "), "priors.\n"))
				}
			suppressMessages(gc())
			}
		} ## END if sgp.percentiles.baseline

	
		## sgp.projections
	
		if (sgp.projections) {
			for (sgp.iter in par.sgp.config) {
	
				panel.data <- within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (dim(panel.data$Panel_Data)[1] > 0) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.projection.content.areas"]], 1), state),
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...)
				} else {
					message(paste("\n\t\tNOTE: No student records &/or no prior data for student growth projections:", tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.projection.content.areas"]], 1), "with", paste(head(sgp.iter[["sgp.projection.content.areas"]], -1), collapse=", "), "priors.\n"))
				}
			suppressMessages(gc())
			}
		} ## END if sgp.projections


		## sgp.projections.baseline
	
		if (sgp.projections.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {

				panel.data <- within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections"))
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (dim(panel.data$Panel_Data)[1] > 0) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1),
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.projection.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=sgp.projections.max.forward.progression.years,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, percentile.trajectory.values, 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.projection.`content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.projection.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...)
				} else {
					message(paste("\n\t\tNOTE: No student records &/or no prior data for baseline student growth projections:", tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.projection.content.areas"]], 1), "with", paste(head(sgp.iter[["sgp.projection.content.areas"]], -1), collapse=", "), "priors.\n"))
				}
			suppressMessages(gc())
			}
		} ## END if sgp.projections.baseline
	
	
		## sgp.projections.lagged
	
		if (sgp.projections.lagged) {
			for (sgp.iter in par.sgp.config) {

				panel.data <- within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (dim(panel.data$Panel_Data)[1] > 0) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						lag.increment=1,
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...)
				} else {
					message(paste("\n\t\tNOTE: No student records &/or no prior data for lagged student growth projections:", tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.content.areas"]], 1), "with", paste(head(sgp.iter[["sgp.content.areas"]], -1), collapse=", "), "priors.\n"))
				}
			suppressMessages(gc())
			}
		} ## END sgp.projections.lagged


		## sgp.projections.lagged.baseline
	
		if (sgp.projections.lagged.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state, c("Standard", "sgp.projections.lagged"))
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (dim(panel.data$Panel_Data)[1] > 0) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=sgp.projections.lagged.baseline.max.order,
						max.forward.progression.grade=sgp.projections.max.forward.progression.grade,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]],
						content_area.progression=sgp.iter[["sgp.projection.content.areas"]],
						year_lags.progression=sgp.iter[["sgp.projection.panel.years.lags"]],
						lag.increment=1,
						grade.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						content_area.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						year_lags.projection.sequence=SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						sgp.exact.grade.progression=sgp.iter[["sgp.exact.grade.progression"]],
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...)
				} else {
					message(paste("\n\t\tNOTE: No student records &/or no prior data for lagged baseline student growth projections:", tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.content.areas"]], 1), "with", paste(head(sgp.iter[["sgp.content.areas"]], -1), collapse=", "), "priors.\n"))
				}
			suppressMessages(gc())
			}
		} ## END sgp.projections.lagged.baseline

		tmp_sgp_object[['Panel_Data']] <- NULL

	} ## END sequential analyzeSGP


	sgp_object@SGP <- mergeSGP(tmp_sgp_object, sgp_object@SGP)

	if (goodness.of.fit.print) gof.print(sgp_object)
	setkeyv(sgp_object@Data, getKey(sgp_object)) # re-key data for combineSGP, etc.
	sgp_object@Version[["analyzeSGP"]][[as.character(gsub("-", "_", Sys.Date()))]] <- as.character(packageVersion("SGP"))
	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
