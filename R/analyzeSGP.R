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
         simulate.sgps=TRUE,
         goodness.of.fit.print=TRUE,
         sgp.config=NULL,
         sgp.baseline.config=NULL, 
         parallel.config=NULL,
         ...) {

	started.at <- proc.time()
	message(paste("\nStarted analyzeSGP", date()))


	### 
	### Utility functions
	###

	## Function to merge results from assorted multiple SGP function calls

	.mergeSGP <- function(list_1, list_2) {
		for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			list_1[[j]] <- c(list_1[[j]], list_2[[j]])[!duplicated(names(c(list_1[[j]], list_2[[j]])))]
		}
		for (j in c("SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			if (all(names(list_2[[j]]) %in% names(list_1[[j]]))) {
				for (k in names(list_2[[j]])) { # merging list_2 in with list_1, so use it here
					if (!identical(list_1[[j]][[k]], list_2[[j]][[k]])) { # keeps it from copying first set of results
						if (dim(list_1[[j]][[k]])[2] != dim(list_2[[j]][[k]])[2]) {
							list_1[[j]][[k]] <- rbind.fill(list_1[[j]][[k]], list_2[[j]][[k]])
						}	else list_1[[j]][[k]] <- rbind(list_1[[j]][[k]], list_2[[j]][[k]])
					}
				}
			}
		}
		for (j in c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries")) {
			for (k in names(list_1[[j]])) {
				list_1[[j]][[k]] <- c(list_1[[j]][[k]], list_2[[j]][[k]])[!duplicated(names(c(list_1[[j]][[k]], list_2[[j]][[k]])))]
			}
		}
	list_1
	}

	## Function to export/print goodness of fit results as pdf files to directory Goodness_of_Fit

	gof.print <- function(sgp_object) {
		if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
				dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE, showWarnings=FALSE)
					for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
						pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=4.5)
						grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
						dev.off()
					}
				}
		} else {
			message("No Goodness of Fit tables available to print. No tables will be produced.")
		}
	}

	## Funtion the calculate the maximum order for a progression based upon any scale changes for the assessment system

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

	## Function to create sgp.config based upon supplied content_areas, years, and grades

	get.sgp.config <- function(content_areas, years, grades) {

	        .get.config <- function(content_area, year, grades) {
        	        tmp.unique.data <- lapply(sgp_object@Data[J("VALID_CASE", content_area), c("YEAR", "GRADE"), with=FALSE], function(x) sort(unique(x)))
	                .sgp.panel.years <- tmp.unique.data$YEAR[1:which(tmp.unique.data$YEAR==year)]
	                .sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
	                tmp.sgp.grade.sequences <- lapply(tmp.unique.data$GRADE[-1], function(x) tail(tmp.unique.data$GRADE[tmp.unique.data$GRADE <= x], length(tmp.unique.data$YEAR)))
	                if (!is.null(grades)) tmp.sgp.grade.sequences <- tmp.sgp.grade.sequences[sapply(tmp.sgp.grade.sequences, function(x) tail(x,1)) %in% grades]
	                .sgp.grade.sequences <- lapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1])
	                list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences)
	        }

                tmp.sgp.config <- tmp.years <- list()
                if (is.null(content_areas)) {
                        content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
                }
                if (is.null(years)) {
                        for (i in content_areas) {
                                tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[J("VALID_CASE", i)][["YEAR"]]), -2), decreasing=TRUE)
                        }
                } else {
                        for (i in content_areas) {
                                tmp.years[[i]] <- years
                        }
                }
                for (i in content_areas) {
                        for (j in tmp.years[[i]]) {
                                tmp.sgp.config[[paste(i,j,sep=".")]] <- .get.config(i,j,grades)
                        }
                }
	tmp.sgp.config
	}


	###
	### Create relevant analyzeSGP variables
	###

        ### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }


	### Create sgp.config (if NULL) 

	if (is.null(sgp.config)) {
		sgp.config <- get.sgp.config(content_areas, years, grades)
	}


	### Create parallel.config (if NULL)
		
	if (!is.null(parallel.config)) {
		if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
			require(foreach)
			eval(parse(text=paste("require(", parallel.config[["BACKEND"]][["FOREACH_TYPE"]], ")"))) # require(do*)   #  ONLY NEED ONCE
			if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC" & 
				is.null(parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]][["preschedule"]])) {
					if (is.list(parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]])) {
						parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]][["preschedule"]]=FALSE # won't override other options
					}	else parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]] = list(preschedule=FALSE)
			}  #  ONLY NEED ONCE
			foreach.options <- parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]] # works fine if NULL
		}

		if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") require(multicore)

		if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") require(snow)
	}

	
	#######################################################################################################################
	##   Baseline SGP - compute matrices first if they are not in SGPstateData or merge them into sgp_object if in SGPstateData
	#######################################################################################################################

	if (sgp.percentiles.baseline) {
		if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
		        if (is.null(sgp.baseline.config)) {
 		               sgp.baseline.config <- tmp.sgp.baseline.config <- .content_areas <- .years <- .grades <- .sgp.grade.sequences <- list()
	        	       .content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
                               .years <- sort(unique(sgp_object@Data[J("VALID_CASE", .content_areas)][["YEAR"]]))
	                       .grades <- sort(unique(sgp_object@Data[J("VALID_CASE", .content_areas)][["GRADE"]]))
	                       .baseline.max.order <- length(.years)-2
	                       tmp.sgp.grade.sequences <- lapply(.grades[-1], function(x) tail(.grades[.grades <= x], (.baseline.max.order+1)))
	                       tmp.sgp.baseline.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= .baseline.max.order])
	                       sgp.baseline.grade.sequences <- list()
	                       for (a in seq_along(tmp.sgp.baseline.grade.sequences)) {
	                               sgp.baseline.grade.sequences[[a]] <-
	                                        eval(parse(text=paste("list(", paste("tail(tmp.sgp.baseline.grade.sequences[[", a, "]],", length(tmp.sgp.baseline.grade.sequences[[a]]):2, ")", collapse=", "), ")")))
                               }
	                       sgp.baseline.grade.sequences <- unlist(sgp.baseline.grade.sequences, recursive=FALSE)

	                       for (i in .content_areas) {
	                                tmp.sgp.baseline.config[[as.character(i)]] <- list(baseline.content.areas=i, baseline.panel.years=.years,
	                                        baseline.grade.sequences=sgp.baseline.grade.sequences)
	                       }

	                       for (a in seq_along(tmp.sgp.baseline.config)) {
					tmp.length <- length(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]])
		                               for (b in seq(tmp.length)) {
		                                   sgp.baseline.config[[b+(a-1)*tmp.length]] <- tmp.sgp.baseline.config[[a]]
		                                   sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.grade.sequences"]] <- unlist(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]][b])
		                        }
		               }
	                } else {
				for (i in names(sgp.baseline.config)) {
					if (!identical(names(sgp.baseline.config[[i]]), c("baseline.content.areas", "baseline.panel.years", "baseline.grade.sequences"))) {
						stop("Please specify an appropriate list of SGP function labels (sgp.baseline.config).	See help page for details.")
					}
				}
			}

			message("\n\tStarted Baseline Coefficient Matrix Calculation:\n")
			
			if (!is.null(parallel.config)) {
				workers <- NULL
				if (!is.null(parallel.config[["ANALYSES"]][["BASELINE_MATRICES_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["BASELINE_MATRICES_WORKERS"]]
				if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
				
				if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
						doMPI.cl <- startMPIcluster(count = workers)
						registerDoMPI(doMPI.cl)
					}
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
						registerDoRedis('jobs')
						startLocalWorkers(n = workers, queue='jobs')
					}
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
						doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
						registerDoSNOW(doSNOW.cl)
					}
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
						doSMP.workers <- startWorkers(workerCount = workers)
						registerDoSMP(doSMP.workers)
					}
	
#				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP" | !is.null(sessionInfo()$otherPkgs$doSMP)) xports <- "sgp_object" else xports <- NULL
	
					tmp <- foreach(sgp.iter=iter(sgp.baseline.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
						.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
						return(baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE))
					}
					sgp_object@SGP <- .mergeSGP(sgp_object@SGP, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
					rm(tmp)
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
					if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
				} # END FOREACH
			
				if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
					if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
						cluster.object <- "internal.cl"
					}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#					clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state")) #, "baselineSGP"
					clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

					#  Add check for having both WORKERS and [["CLUSTER.OBJECT"]] - shouldn't have both - warn and use [["CLUSTER.OBJECT"]]
					
					tmp <- parLapply(eval(parse(text=cluster.object)), sgp.baseline.config, function(sgp.iter) baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE))
					
					sgp_object@SGP <- .mergeSGP(sgp_object@SGP, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
					if (exists("internal.cl")) stopCluster(internal.cl)
					rm(tmp)
				} # END if (SNOW)
				
				if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
					if (is.null(workers)) workers = getOption("cores")
					tmp <- mclapply(sgp.baseline.config, function(sgp.iter) baselineSGP(
								sgp_object,
								state=state,
								sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
								return.matrices.only=TRUE,
								calculate.baseline.sgps=FALSE),
							mc.cores = workers, mc.preschedule = FALSE)
					sgp_object@SGP <- .mergeSGP(sgp_object@SGP, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
					rm(tmp)
				} # END if (MULTICORE)
			} else { # process sequentially
				tmp <- list()
				for (sgp.iter in seq_along(sgp.baseline.config)) {
					tmp[[sgp.iter]] <- baselineSGP(
						sgp_object,
						state=state,
						sgp.baseline.config=sgp.baseline.config[sgp.iter], ## NOTE: must pass list, [...], not vector, [[...]].
						return.matrices.only=TRUE,
						calculate.baseline.sgps=FALSE)
				}

				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
			}

			assign(paste(state, "_Baseline_Matrices", sep=""), list())
			for (tmp.matrix.label in grep("BASELINE", names(sgp_object@SGP$Coefficient_Matrices), value=TRUE)) {
				eval(parse(text=paste(state, "_Baseline_Matrices[['", tmp.matrix.label, "']] <- sgp_object@SGP[['Coefficient_Matrices']][['", tmp.matrix.label, "']]", sep="")))
			}
			save(list=paste(state, "_Baseline_Matrices", sep=""), file=paste(state, "_Baseline_Matrices.Rdata", sep="")) 
			message("\n\tFinished Calculating Baseline Coefficient Matrices\n")
		} else {
			sgp_object@SGP <- .mergeSGP(sgp_object@SGP, SGPstateData[[state]][["Baseline_splineMatrix"]])
		}
	suppressMessages(gc()) # clean up
	} # END Get/Compute baseline coefficient matrices


	#######################################################################################################################
	##   Percentiles, Baseline Percentiles, Projections, Lagged Projections -  PARALLEL FLAVORS FIRST
	#######################################################################################################################

	if (!is.null(parallel.config)) {
		###  INIITIAL SET UP FOR ALL ANALYSES AND BACKEND TYPES
		# RE-Key data to select only grades in grade progression
		key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")
		
		# Stretch out sgp.config and add info for baseline sgps
		par.sgp.config <- list(); cnt <- 1
		for (a in names(sgp.config)) {
			for (b in seq_along(sgp.config[[a]][["sgp.grade.sequences"]])) {
				par.sgp.config[[cnt]] <- sgp.config[[a]]
				par.sgp.config[[cnt]][["sgp.grade.sequences"]] <- sgp.config[[a]][["sgp.grade.sequences"]][b]
				if (any(diff(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]])>1)) {
					grade.span <- seq(min(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]),max(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]))
					index <- match(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], grade.span)
					par.sgp.config[[cnt]][["sgp.panel.years"]] <- par.sgp.config[[cnt]][["sgp.panel.years"]][index]
					par.sgp.config[[cnt]][["sgp.content.areas"]] <- par.sgp.config[[cnt]][["sgp.content.areas"]][index]
				}
				
				if (sgp.percentiles.baseline) {
					mtx.names <- names(sgp_object@SGP[["Coefficient_Matrices"]][[paste(strsplit(a, "\\.")[[1]][1], ".BASELINE", sep="")]])
					max.order <- max(as.numeric(sapply(strsplit(mtx.names[grep(paste("qrmatrix_", tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1), sep=""), mtx.names)], "_"), function(x) x[3])))
					if (length(par.sgp.config[[cnt]][["sgp.panel.years"]])-1 < max.order) max.order <- length(par.sgp.config[[cnt]][["sgp.panel.years"]])-1			
					if (sum(diff(tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order))) > length(par.sgp.config[[cnt]][["sgp.panel.years"]])) {
						base.gp <- par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]][tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1) - 
							par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]] <= max.order]
						max.order <- length(base.gp) - 1
					}	else base.gp <- tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order)  # not sure we need this if statement since years are corrected 9/27/11
					par.sgp.config[[cnt]][["base.gp"]] <- base.gp
					par.sgp.config[[cnt]][["max.order"]] <- max.order
				}
				cnt <- cnt + 1
			}
		}

		# create text objects to eval(parse()) in functions
		pctls.data <- "as.data.frame(reshape(sgp_object@Data[J('VALID_CASE', tail(sgp.iter[['sgp.content.areas']], length(sgp.iter[['sgp.grade.sequences']][[1]])), 
			tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), sgp.iter[['sgp.grade.sequences']][[1]])],
			idvar='ID',
			timevar='YEAR',
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'GRADE', 'SCALE_SCORE', 'YEAR')],
			direction='wide'))"
					
		pctls.vnames <- "c('ID', paste('GRADE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), sep='.'), 
			paste('SCALE_SCORE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), sep='.'))"

		projs.data <- "as.data.frame(reshape(sgp_object@Data[J('VALID_CASE', tail(sgp.iter[['sgp.content.areas']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), 
			tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), head(sgp.iter[['sgp.grade.sequences']][[1]], -1))],
			idvar='ID',
			timevar='YEAR',
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'GRADE', 'SCALE_SCORE', 'YEAR')],
			direction='wide'))"

		projs.vnames <- "c('ID', paste('GRADE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), sep='.'), 
			paste('SCALE_SCORE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), sep='.'))"
		
		lagged.projs.data <- "as.data.frame(reshape(sgp_object@Data[J('VALID_CASE', tail(sgp.iter[['sgp.content.areas']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), 
			head(tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), -1), head(sgp.iter[['sgp.grade.sequences']][[1]], -1))],
			idvar='ID',
			timevar='YEAR',
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'GRADE', 'SCALE_SCORE', 'YEAR')],
			direction='wide'))"

		lagged.projs.vnames <- "c('ID', paste('GRADE', head(tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), -1), sep='.'), 
			paste('SCALE_SCORE', head(tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), -1), sep='.'))"


	##################################		
	###  PERCENTILES
	##################################

		if (sgp.percentiles) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["PERCENTILE_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["PERCENTILE_WORKERS"]]
#			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
					doMPI.cl <- startMPIcluster(count = workers)
					registerDoMPI(doMPI.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
					registerDoRedis('jobs')
					startLocalWorkers(n = workers, queue='jobs')
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
					doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					registerDoSNOW(doSNOW.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
					doSMP.workers <- startWorkers(workerCount = workers)
					registerDoSMP(doSMP.workers)
				}

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
						.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
								Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=eval(parse(text=pctls.vnames)),
							grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							...))
					}
				} else {
					tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
						.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
								Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=eval(parse(text=pctls.vnames)),
							grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							...))
					}
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
			} # END FOREACH
			
			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...))
				} else {
					tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...))
				}
				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...), mc.cores=workers, mc.preschedule = FALSE)
				} else {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...), mc.cores=workers, mc.preschedule = FALSE)
				}
				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} #END if (sgp.percentiles)


	####################################
	###  BASELINE PERCENTILES
	####################################

		if (sgp.percentiles.baseline) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["BASELINE_SGP_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["BASELINE_SGP_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
					doMPI.cl <- startMPIcluster(count = workers)
					registerDoMPI(doMPI.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
					registerDoRedis('jobs')
					startLocalWorkers(n = workers, queue='jobs')
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
					doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					registerDoSNOW(doSNOW.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
					doSMP.workers <- startWorkers(workerCount = workers)
					registerDoSMP(doSMP.workers)
				}

#				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP" | !is.null(sessionInfo()$otherPkgs$doSMP)) xports <- "sgp_object" else xports <- NULL
				
				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthPercentiles(
						panel.data = list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries = state,
						use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels = state,
						panel.data.vnames = eval(parse(text=pctls.vnames)),
						grade.progression = sgp.iter[["base.gp"]],
						num.prior = min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						goodness.of.fit=TRUE,
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
			} # END FOREACH
			
			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles(
					panel.data = list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
						my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries = state,
					use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
					growth.levels = state,
					panel.data.vnames = eval(parse(text=pctls.vnames)),
					grade.progression = sgp.iter[["base.gp"]],
					num.prior = min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
					goodness.of.fit=TRUE,
					drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
					...))

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles(
					panel.data = list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
						my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries = state,
					use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
					growth.levels = state,
					panel.data.vnames = eval(parse(text=pctls.vnames)),
					grade.progression = sgp.iter[["base.gp"]],
					num.prior = min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
					goodness.of.fit=TRUE,
					drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
					...), mc.cores=workers, mc.preschedule = FALSE)

#					sgp_object@SGP <- mclapply(tmp, .mergeSGP, sgp_object, mc.cores=workers, mc.preschedule = FALSE)
				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.percentiles.baseline


	#######################################################
	###  PROJECTIONS (COHORT referenced)
	#######################################################

		if (sgp.projections) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
					doMPI.cl <- startMPIcluster(count = workers)
					registerDoMPI(doMPI.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
					registerDoRedis('jobs')
					startLocalWorkers(n = workers, queue='jobs')
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
					doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					registerDoSNOW(doSNOW.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
					doSMP.workers <- startWorkers(workerCount = workers)
					registerDoSMP(doSMP.workers)
				}

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=eval(parse(text=projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...))

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections


	#######################################################
	###  PROJECTIONS (BASELINE referenced)
	#######################################################

		if (sgp.projections.baseline) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
					doMPI.cl <- startMPIcluster(count = workers)
					registerDoMPI(doMPI.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
					registerDoRedis('jobs')
					startLocalWorkers(n = workers, queue='jobs')
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
					doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					registerDoSNOW(doSNOW.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
					doSMP.workers <- startWorkers(workerCount = workers)
					registerDoSMP(doSMP.workers)
				}

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=eval(parse(text=projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=sgp.projections.baseline.max.order,
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...))

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=sgp.projections.baseline.max.order,
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.baseline


	#################################################
	###  LAGGED PROJECTIONS (COHORT Referenced)
	#################################################

		if (sgp.projections.lagged) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
					doMPI.cl <- startMPIcluster(count = workers)
					registerDoMPI(doMPI.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
					registerDoRedis('jobs')
					startLocalWorkers(n = workers, queue='jobs')
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
					doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					registerDoSNOW(doSNOW.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
					doSMP.workers <- startWorkers(workerCount = workers)
					registerDoSMP(doSMP.workers)
				}

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...))

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged


	#################################################
	###  LAGGED PROJECTIONS (BASELINE Referenced)
	#################################################

		if (sgp.projections.lagged.baseline) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC") registerDoMC(workers)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI") {
					doMPI.cl <- startMPIcluster(count = workers)
					registerDoMPI(doMPI.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis") {
					registerDoRedis('jobs')
					startLocalWorkers(n = workers, queue='jobs')
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW") {
					doSNOW.cl = makeCluster(workers, type = parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					registerDoSNOW(doSNOW.cl)
				}
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP") {
					doSMP.workers <- startWorkers(workerCount = workers)
					registerDoSMP(doSMP.workers)
				}

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=sgp.projections.lagged.baseline.max.order,
						panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMPI")	closeCluster(doMPI.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doRedis")	removeQueue('jobs')
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSNOW")	stopCluster(doSNOW.cl)
				if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doSMP")	stopWorkers(doSMP.workers)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...))

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in seq_along(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged.baseline
	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID") # re-key data for combineSGP, etc.
	}  ## END if (!is.null(parallel.config))


#############################################################
###	SEQUENTIAL OPTION (NON-Parallel Option)
#############################################################

	if (is.null(parallel.config)) {

		tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

		for (sgp.iter in sgp.config) {
			tmp_sgp_object[["Panel_Data"]] <- 
			as.data.frame(reshape(sgp_object@Data[J("VALID_CASE", sgp.iter[["sgp.content.areas"]], sgp.iter[["sgp.panel.years"]])],
				idvar="ID",
				timevar="YEAR",
				drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
				direction="wide"))
			suppressMessages(gc()) 
			
			if (sgp.percentiles) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					for (k in sgp.iter[["sgp.grade.sequences"]]) {
						tmp_sgp_object <- studentGrowthPercentiles(
							panel.data=tmp_sgp_object,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k,
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							calculate.confidence.intervals=calculate.confidence.intervals,  
							...)
					} ## END k loop
				} else {
					for (k in sgp.iter[["sgp.grade.sequences"]]) {
						tmp_sgp_object <- studentGrowthPercentiles(
							panel.data=tmp_sgp_object,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k,
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							...)
					} ## END k loop
				} 
			} ## END if sgp.percentiles

			if (sgp.percentiles.baseline) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
					
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, SGPstateData[[state]][["Baseline_splineMatrix"]])
				
				mtx.names <- names(tmp_sgp_object[["Coefficient_Matrices"]][[paste(toupper(tail(sgp.iter[["sgp.content.areas"]], 1)), ".BASELINE", sep="")]])
	
				for (k in sgp.iter[["sgp.grade.sequences"]]) {
					max.order <- max(as.numeric(sapply(strsplit(mtx.names[grep(paste("qrmatrix_", tail(k, 1), sep=""), mtx.names)], "_"), function(x) x[3])))
					
					if (length(sgp.iter[["sgp.panel.years"]])-1 < max.order) max.order <- length(sgp.iter[["sgp.panel.years"]])-1			
	
					if (any(diff(tail(k, 1+max.order)) > 1)) {	# deals with 'holes'
						base.gp <- k[tail(k, 1)-k <= max.order]
						max.order <- length(base.gp) - 1
					}	else base.gp <- tail(k, 1+max.order)
	
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data = tmp_sgp_object,
						sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries = state,
						use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels = state,
						panel.data.vnames = sgp.vnames,
						grade.progression = base.gp,
						num.prior = min(max.order, sgp.percentiles.baseline.max.order),
						goodness.of.fit=TRUE,
						...)
				} ## END k loop
			} ## END if sgp.percentiles.baseline

	
			## sgp.projections
	
			if (sgp.projections) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...)
				}
			} ## END if sgp.projections


			## sgp.projections.baseline
	
			if (sgp.projections.baseline) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1),
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...)
				}
			} ## END if sgp.projections.baseline
	
	
			## sgp.projections.lagged
	
			if (sgp.projections.lagged) {
				sgp.vnames <- c("ID", paste("GRADE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."), 
					paste("SCALE_SCORE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...)
				}
			} ## END sgp.projections.lagged

			## sgp.projections.lagged.baseline
	
			if (sgp.projections.lagged.baseline) {
				sgp.vnames <- c("ID", paste("GRADE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."), 
					paste("SCALE_SCORE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=sgp.projections.lagged.baseline.max.order,
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						...)
				}
			} ## END sgp.projections.lagged.baseline
		
			## sgp.percentiles.baseline

		} ## END for (sgp.iter in sgp.config)) -- SEQUENTIAL
		sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp_sgp_object)
	} ## END sequential analyzeSGP

	if (goodness.of.fit.print) gof.print(sgp_object)

	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
