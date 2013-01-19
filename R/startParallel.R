`startParallel` <- 
function(
	parallel.config, 
	process,
	qr.taus) {
	
	if (any(toupper(parallel.config[['BACKEND']]) == 'MULTICORE' | toupper(parallel.config[['BACKEND']]) == 'SNOW')) {
		stop(paste('\n\t', parallel.config[['BACKEND']], "no longer supported.  Please use the 'PARALLEL' package backend and R > 2.12 for parallel computation.\n"))
	}
	
	if (toupper(parallel.config[['BACKEND']]) == 'FOREACH' && (parallel.config[['TYPE']] != "doParallel" & !is.na(parallel.config[['TYPE']]))) {
			stop(paste('\n\t', parallel.config[['TYPE']], "no longer supported.  Please use doParallel and R > 2.12 for parallel computation.\n"))
	}
	
	workers <- NULL; par.type <- 'OTHER'; TAUS.LIST <- NULL

	if (!is.null(parallel.config[['CLUSTER.OBJECT']])) {
		if (!missing(qr.taus)) {
			workers <- length(eval(parse(text=parallel.config[['CLUSTER.OBJECT']])))
			chunk.size <- ceiling(length(qr.taus) / workers)
			TAUS.LIST <- vector("list", workers)
			for (chunk in 0:(workers-1)) {
				lower.index <- chunk*chunk.size+1
				upper.index <- min((chunk+1)*chunk.size, length(qr.taus))
				TAUS.LIST[[chunk+1]] <- qr.taus[lower.index:upper.index]
			}
		}
		clusterEvalQ(eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), library(SGP))
		par.start <- list(internal.cl=eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), par.type='SNOW')
		clusterExport(eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), "par.start", envir=2)
		return(list(internal.cl=eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), 
			par.type='SNOW', TAUS.LIST=TAUS.LIST))
	}
	
	###  Basic checks - default to ANY percentiles or projections WORKERS.
	
	if (is.numeric(parallel.config[['WORKERS']])) {
		message(paste("\t", process, " workers not specified.  Numeric value from WORKERS (", parallel.config[['WORKERS']], ") will be used for all processes.\n", sep=""))
		parallel.config[['WORKERS']][[process]] <- parallel.config[['WORKERS']]
	}
	if (is.null(parallel.config[['WORKERS']][[process]])) {
		if (!is.null(parallel.config[['WORKERS']])) {
			 tmp.indx <- grep(strsplit(process, "_")[[1]][2], names(parallel.config[['WORKERS']]))
			 if (any(!is.na(tmp.indx))) {
				 parallel.config[['WORKERS']][[process]] <- parallel.config[['WORKERS']][[tmp.indx]]
				 message(paste(process, "workers not defined specifically.", names(parallel.config[['WORKERS']][tmp.indx]), 
				 	"WORKERS will be used  (", parallel.config[['WORKERS']][tmp.indx], "worker processors)."))
			 }
		} # See if still NULL and stop:
		if (is.null(parallel.config[['WORKERS']][[process]])) stop(paste(process, "workers must be specified."))
	}
	
	if (all(c("PERCENTILES", "TAUS") %in% names(parallel.config[['WORKERS']]))) stop("Both TAUS and PERCENTILES can not be executed in Parallel at the same time.")

	###  Basic configuration
	
	if (toupper(parallel.config[['BACKEND']]) == 'FOREACH') {
		if (!is.na(parallel.config[['TYPE']]) & !identical(parallel.config[['TYPE']], "NA")) {
			eval(parse(text=paste("suppressPackageStartupMessages(require(", parallel.config[['TYPE']], "))")))
		} else parallel.config[['TYPE']] <- "doParallel"

		# if (parallel.config[['TYPE']]=="doMC" & is.null(parallel.config[['OPTIONS']][["preschedule"]])) {
			# if (is.list(parallel.config[['OPTIONS']])) {
				# parallel.config[['OPTIONS']][["preschedule"]]=FALSE
			# }	else parallel.config[['OPTIONS']]=list(preschedule=FALSE)
		# }

		if (parallel.config[['TYPE']]=="doParallel") { 
			if (.Platform$OS.type == "unix" & is.null(par.type)) par.type <- 'MULTICORE' 
			if (.Platform$OS.type != "unix" & is.null(par.type)) par.type <- 'SNOW'
			if (par.type == 'MULTICORE' & is.null(parallel.config[['OPTIONS']][["preschedule"]])) {
				if (is.list(parallel.config[['OPTIONS']])) {
					parallel.config[['OPTIONS']][["preschedule"]]=FALSE
				}	else parallel.config[['OPTIONS']]=list(preschedule=FALSE)
			}
		} # END doParallel
		
		foreach.options <- parallel.config[['OPTIONS']] # works fine if NULL
	} #  END FOREACH

	# if (toupper(parallel.config[['BACKEND']]) == 'MULTICORE') {
		# par.type <- 'MULTICORE'
	# }

	# if (toupper(parallel.config[['BACKEND']]) == 'SNOW') {
		# par.type <- 'SNOW'
	# }

	if (toupper(parallel.config[['BACKEND']]) == 'PARALLEL') {
		# Weird error for MPI stopCluster(...) 'Error in NextMethod() : 'NextMethod' called from an anonymous function'  load snow first removes it.
		# if (!is.null(parallel.config[['TYPE']]) && parallel.config[['TYPE']] == 'MPI') require(snow)  #  Don't think this is a problem any more... 08/03/12
		suppressPackageStartupMessages(require(parallel))
		if (!is.null(parallel.config[['TYPE']])) {
			if (!parallel.config[['TYPE']] %in% c('SOCK', 'MPI')) {
				stop("The 'snow' package will be used when 'parallel.config$TYPE' is specified and BACKEND=='PARALLEL'.  List element must be 'SOCK' or 'MPI'.")
			}
			par.type <- 'SNOW'
		} else {
			if (.Platform$OS.type == "unix") par.type <- 'MULTICORE' 
			if (.Platform$OS.type != "unix") par.type <- 'SNOW'; parallel.config[['TYPE']] <- 'SOCK'
		}
	}
	
	if (par.type == 'SNOW') {
		if (is.null(parallel.config[['TYPE']])) stop("The 'parallel.config$TYPE' must be specified ('SOCK' or 'MPI')")
		if (!parallel.config[['TYPE']] %in% c('SOCK','MPI')) stop("The 'parallel.config$TYPE' must be 'SOCK' or 'MPI'")
	}


	###  Set up workers and spin up clusters / register workers
	
	if (!is.null(parallel.config[['WORKERS']][[process]])) {
		workers <- parallel.config[['WORKERS']][[process]]
	} else workers <- parallel.config[['WORKERS']]
	if (is.null(workers)) workers <- getOption("cores")
	if (is.null(workers)) stop("parallel.config$WORKERS must, at a minimum, contain the number of parallel workers for all processes, 
		or getOption('cores') must be specified to use MULTICORE parallel processing.")

	###
	###  Need this for all flavors - move to startParallel
	###

	if (process=='TAUS') {
		chunk.size <- ceiling(length(qr.taus) / workers)
		TAUS.LIST <- vector("list", workers)
		for (chunk in 0:(workers-1)) {
			lower.index <- chunk*chunk.size+1
			upper.index <- min((chunk+1)*chunk.size, length(qr.taus))
			TAUS.LIST[[chunk+1]] <- qr.taus[lower.index:upper.index]
		}
		if ((chunk.size*workers) > length(qr.taus))  TAUS.LIST <- TAUS.LIST[sapply(TAUS.LIST, function(x) !is.na(x)[[1]][1])]
	}
	
	###
	### END to startParallel
	###
	
	if (toupper(parallel.config[['BACKEND']]) == 'FOREACH') {
		par.type='FOREACH'
		if (parallel.config[['TYPE']]=="NA") {
			registerDoSEQ() # prevents warning message
			return(list(foreach.options=foreach.options, par.type=par.type))
		}
		# if (parallel.config[['TYPE']]=="doMC") {
			# registerDoMC(workers)
			# return(list(foreach.options=foreach.options, par.type=par.type))
		# }
		# if (parallel.config[['TYPE']]=='doMPI') {
			# doPar.cl <- startMPIcluster(count=workers)
			# registerDoMPI(doPar.cl)
			# return(list(doPar.cl=doPar.cl, foreach.options=foreach.options, par.type=par.type))
		# }
		# if (parallel.config[['TYPE']]=='doRedis') {
			# redisWorker('jobs', port=10187) #  Doesn't seem to work.  Maybe get rid of this option/flavor?
			# registerDoRedis('jobs')
			# startLocalWorkers(n=workers, queue='jobs')
			# return(list(jobs='jobs', foreach.options=foreach.options, par.type=par.type))
		# }
		# if (parallel.config[['TYPE']]=='doSNOW') {
			# doPar.cl=makeCluster(workers, type='SOCK')
			# registerDoSNOW(doPar.cl)
			# return(list(doPar.cl=doPar.cl, foreach.options=foreach.options, par.type=par.type))
		# }
		if (parallel.config[['TYPE']]=="doParallel") {
			if (par.type == 'SNOW') {
				doPar.cl <- makeCluster(workers, type='SOCK')
				registerDoParallel(doPar.cl)
				clusterEvalQ(doPar.cl, library(SGP))
				return(list(doPar.cl=doPar.cl, foreach.options=foreach.options, par.type=par.type))
			} else {
				registerDoParallel(workers)
				return(list(foreach.options=foreach.options, par.type=par.type, TAUS.LIST=TAUS.LIST))
			}
		}
	} # END if (FOREACH)

	if (par.type=='SNOW') {
		# if (parallel.config[['TYPE']]=='MPI') {
			# if (exists('par.start')) return() #don't try to restart a new config
		# }
		internal.cl <- makeCluster(eval(parse(text=workers)), type=parallel.config[['TYPE']]) # eval workers in case 'names' used
		clusterEvalQ(internal.cl, library(SGP))
		return(list(internal.cl=internal.cl, par.type=par.type, TAUS.LIST=TAUS.LIST)) #  workers=workers,
	}

	if (par.type=='MULTICORE') {
		return(list(workers=workers, par.type=par.type, TAUS.LIST=TAUS.LIST))
	}
}
