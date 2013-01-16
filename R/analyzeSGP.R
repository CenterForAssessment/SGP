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
         sgp.use.my.coefficient.matrices=NULL,
         simulate.sgps=TRUE,
         goodness.of.fit.print=TRUE,
         sgp.config=NULL,
         sgp.config.drop.nonsequential.grade.progression.variables=TRUE,
         sgp.baseline.panel.years=NULL,
         sgp.baseline.config=NULL, 
         parallel.config=NULL,
         ...) {

	started.at <- proc.time()
	message(paste("\nStarted analyzeSGP", date()))

	VALID_CASE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- NULL


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
			simulate.sgps <- FALSE
		}
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.config.drop.nonsequential.grade.progression.variables"]])) {
		sgp.config.drop.nonsequential.grade.progression.variables <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.config.drop.nonsequential.grade.progression.variables"]]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]])) {
		sgp.loss.hoss.adjustment <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]]
	} else {
		sgp.loss.hoss.adjustment <- NULL
	}

	if ((sgp.projections | sgp.projections.lagged | sgp.projections.baseline | sgp.projections.lagged.baseline) & is.null(SGPstateData[[state]][["Achievement"]][["Cutscores"]])) {
		message(paste("\tNOTE: Achievement Level cutscores for state, ", state, ", are not in embedded SGPstateData. Projections and Lagged Projections will not be calculated"))
		sgp.projections <- sgp.projections.lagged <- sgp.projections.baseline <- sgp.projections.lagged.baseline <- FALSE
	}

	### 
	### Utility functions
	###

	## Function to export/print goodness of fit results as pdf files to directory Goodness_of_Fit

	gof.print <- function(sgp_object) {
		if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
				dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE, showWarnings=FALSE)
					for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
						pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=11)
						grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
						dev.off()
					}
				}
		} else {
			message("\tNOTE: No Goodness of Fit tables available to print. No tables will be produced.")
		}
	}

	## Function to calculate the maximum order for a progression based upon any scale changes for the assessment system

	get.max.order.for.progression <- function(year, content_area) {
		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]])) {
			return(SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]]) ## Returns NULL if it doesn't exist
		} else {
			tmp <- as.numeric(tail(unlist(strsplit(as.character(year), "_")), 1)) - as.numeric(tail(unlist(strsplit(as.character(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]), "_")), 1))
			if (tmp < 0) return(SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]])
			if (tmp > 0) return(min(c(as.numeric(tmp), SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]])))
			if (tmp==0) message(paste("\tNOTE: Based upon state scale changes in ", year, ". student growth projections are not possible. No student growth projections will be generated", sep=""))
		}
	}

	## Function to merge coefficient matrices from coefficient matrix productions

	merge.coefficient.matrices <- function(list.of.matrices) {
		tmp.list <- list()
		tmp.coefficient.matrices <- unlist(list.of.matrices, recursive=FALSE)
		for (tmp.names in unique(names(tmp.coefficient.matrices))) {
			tmp1 <- unlist(tmp.coefficient.matrices[grep(tmp.names, names(tmp.coefficient.matrices))], recursive=FALSE)
			names(tmp1) <- sapply(strsplit(names(tmp1), "[.]"), function(x) x[3])
			tmp.list[[tmp.names]] <- tmp1
		}
	tmp.list
	}


	#######################################################################################################################
	##   Set up the temporary sgp list object.  Fill with necessary old results if they exist first.
	##   Create subset of @Data containing essential data elements for analyses
	#######################################################################################################################

	tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

	tmp_sgp_data_for_analysis <- sgp_object@Data[,c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"), with=FALSE]
	setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)


	#######################################################################################################################
	##   Baseline SGP - compute matrices first if they are not in SGPstateData or merge them into sgp_object if in SGPstateData
	#######################################################################################################################

	if (sgp.percentiles.baseline) {

		if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
			if (is.null(sgp.baseline.config)) {
				sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years)
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
			} else { ### SEQUENTIAL BASELINE COEFFICIENT MATRIX CONSTRUCTION 
				tmp <- list()
				for (sgp.iter in seq_along(sgp.baseline.config)) {
					tmp[[sgp.iter]] <- baselineSGP(
						sgp_object,
						state=state,
						sgp.baseline.config=sgp.baseline.config[sgp.iter], ## NOTE: must pass list, [...], not vector, [[...]].
						return.matrices.only=TRUE,
						calculate.baseline.sgps=FALSE)
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
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


	### Create par.sgp.config (for both parallel and sequential implementations)

	setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
	par.sgp.config <- getSGPConfig(sgp_object, tmp_sgp_object, content_areas, years, grades, sgp.config, sgp.percentiles.baseline, sgp.projections.baseline, sgp.projections.lagged.baseline,
		sgp.config.drop.nonsequential.grade.progression.variables)

	if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {
		if (any(sapply(par.sgp.config, function(x) identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))) {
			baseline.missings <- which(sapply(par.sgp.config, function(x) identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))
			baseline.missings <- paste(unlist(sapply(par.sgp.config[baseline.missings], function(x) paste(tail(x$sgp.content.areas, 1), x$sgp.grade.sequences))), collapse="; ")
			message("\tNOTE: Baseline coefficient matrices are not available for ", baseline.missings, ".", sep="")
		}
		par.sgp.config.baseline <- par.sgp.config[which(sapply(par.sgp.config, function(x) !identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))]
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
				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- foreach(sgp.iter=iter(rev(par.sgp.config)), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
								Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							year.progression=sgp.iter[["sgp.panel.years"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							goodness.of.fit=state,
							...))
					}
				} else {
					tmp <- foreach(sgp.iter=iter(rev(par.sgp.config)), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
								Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							year.progression=sgp.iter[["sgp.panel.years"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							goodness.of.fit=state,
							...))
					}
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else { # END FOREACH
				###    SNOW flavor
				if (par.start$par.type == 'SNOW') {
					if (simulate.sgps) {
						if (!exists("calculate.confidence.intervals")) {
							calculate.confidence.intervals <- state
						}
						tmp <- clusterApplyLB(par.start$internal.cl, rev(par.sgp.config), 	function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
								Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							year.progression=sgp.iter[["sgp.panel.years"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							goodness.of.fit=state,
							...))
					} else {
						tmp <- clusterApplyLB(par.start$internal.cl, rev(par.sgp.config), 	function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
								Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							year.progression=sgp.iter[["sgp.panel.years"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							goodness.of.fit=state,
							...))
					}
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.=getErrorReports(tmp, tmp.tf, rev(par.sgp.config)))
					}
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					if (simulate.sgps) {
						if (!exists("calculate.confidence.intervals")) {
							calculate.confidence.intervals <- state
						}
						tmp <- mclapply(rev(par.sgp.config), function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
								Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							year.progression=sgp.iter[["sgp.panel.years"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							goodness.of.fit=state,
							...), mc.cores=par.start$workers, mc.preschedule=FALSE)
					} else {
						tmp <- mclapply(rev(par.sgp.config), function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
								Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							year.progression=sgp.iter[["sgp.panel.years"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							goodness.of.fit=state,
							...), mc.cores=par.start$workers, mc.preschedule=FALSE)
					}
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["base.gp"]],
						content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
						year.progression.lags=sgp.iter[["time.lags"]],
						num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						goodness.of.fit=state,
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else { # END FOREACH	
				###    SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, rev(par.sgp.config.baseline), 	function(sgp.iter)	studentGrowthPercentiles(
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["base.gp"]],
						content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
						year.progression.lags=sgp.iter[["time.lags"]],
						num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						goodness.of.fit=state,
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["base.gp"]],
						content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
						year.progression.lags=sgp.iter[["time.lags"]],
						num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						goodness.of.fit=state,
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter),
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=sgp.projections.lagged.baseline.max.order,
						panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH

			###  SNOW flavor
			if (par.start$par.type == 'SNOW') {
				tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
						Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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
					panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter), 
						Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
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

				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
						content.area.progression=sgp.iter[["sgp.content.areas"]],
						year.progression=sgp.iter[["sgp.panel.years"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						calculate.confidence.intervals=calculate.confidence.intervals,
						drop.nonsequential.grade.progression.variables=FALSE,
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...)
				} else {
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
						content.area.progression=sgp.iter[["sgp.content.areas"]],
						year.progression=sgp.iter[["sgp.panel.years"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
						drop.nonsequential.grade.progression.variables=FALSE,
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...)
				}
				suppressMessages(gc())
			}
		} ## END if sgp.percentiles


		## sgp.percentiles.baseline

		if (sgp.percentiles.baseline) {
			for (sgp.iter in rev(par.sgp.config.baseline)) {
				
				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthPercentiles(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
						my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					growth.levels=state,
					panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
					grade.progression=sgp.iter[["base.gp"]],
					content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
					year.progression.lags=sgp.iter[["time.lags"]],
					num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
					percentile.cuts=SGPstateData[[state]][["SGP_Configuration"]][["percentile.cuts"]],
					drop.nonsequential.grade.progression.variables=FALSE,
					exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
					sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
					...)
				suppressMessages(gc())
			}
		} ## END if sgp.percentiles.baseline

	
		## sgp.projections
	
		if (sgp.projections) {
			for (sgp.iter in par.sgp.config) {
	
				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
					grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
				suppressMessages(gc())
			}
		} ## END if sgp.projections


		## sgp.projections.baseline
	
		if (sgp.projections.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1),
						my.extra.label="BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=sgp.projections.baseline.max.order,
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=getPanelDataVnames("sgp.projections", sgp.iter),
					grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
				suppressMessages(gc())
			}
		} ## END if sgp.projections.baseline
	
	
		## sgp.projections.lagged
	
		if (sgp.projections.lagged) {
			for (sgp.iter in par.sgp.config) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
				suppressMessages(gc())
			}
		} ## END sgp.projections.lagged


		## sgp.projections.lagged.baseline
	
		if (sgp.projections.lagged.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.projections.lagged", sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), tail(head(sgp.iter[["sgp.content.areas"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
				suppressMessages(gc())
			}
		} ## END sgp.projections.lagged.baseline

		tmp_sgp_object[['Panel_Data']] <- NULL

	} ## END sequential analyzeSGP


	sgp_object@SGP <- mergeSGP(tmp_sgp_object, sgp_object@SGP)

	if (goodness.of.fit.print) gof.print(sgp_object)
	setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, ID) # re-key data for combineSGP, etc.
	sgp_object@Version[["analyzeSGP"]][[as.character(gsub("-", "_", Sys.Date()))]] <- as.character(packageVersion("SGP"))
	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
