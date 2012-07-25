`startParallel` <- 
function(
	parallel.config, 
	process
	) {
	
	workers <- NULL; par.type <- 'OTHER'

	if (!is.null(parallel.config[['CLUSTER.OBJECT']])) {
		clusterEvalQ(eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), library(SGP))
		par.start <- list(internal.cl=eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), par.type='SNOW')
		clusterExport(eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), "par.start", envir=2)
		return(list(internal.cl=eval(parse(text=parallel.config[['CLUSTER.OBJECT']])), par.type='SNOW'))
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

	
	###  Basic configuration
	
	if (toupper(parallel.config[['BACKEND']]) == 'FOREACH') {
		require(foreach); require(iterators) # Have to load iterators for sequential uses
		if (!is.na(parallel.config[['TYPE']]) & !identical(parallel.config[['TYPE']], "NA")) {
			eval(parse(text=paste("require(", parallel.config[['TYPE']], ")")))
		} else parallel.config[['TYPE']] <- "NA"

		if (parallel.config[['TYPE']]=="doMC" & is.null(parallel.config[['OPTIONS']][["preschedule"]])) {
			if (is.list(parallel.config[['OPTIONS']])) {
				parallel.config[['OPTIONS']][["preschedule"]]=FALSE
			}	else parallel.config[['OPTIONS']]=list(preschedule=FALSE)
		}

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

	if (toupper(parallel.config[['BACKEND']]) == 'MULTICORE') {
		require(multicore)
		par.type <- 'MULTICORE'
	}

	if (toupper(parallel.config[['BACKEND']]) == 'SNOW') {
		require(snow)
		par.type <- 'SNOW'
	}

	if (toupper(parallel.config[['BACKEND']]) == 'PARALLEL') {
		# Weird error for MPI stopCluster(...) 'Error in NextMethod() : 'NextMethod' called from an anonymous function'  load snow first removes it.
		# if (!is.null(parallel.config[['TYPE']]) && parallel.config[['TYPE']] == 'MPI') require(snow) 
		require(parallel)
		if (!is.null(parallel.config[['TYPE']])) {
			if (!parallel.config[['TYPE']] %in% c('SOCK', 'MPI')) {
				stop("The 'snow' package will be used when 'parallel.config$TYPE' is specified and BACKEND=='PARALLEL'.  List element must be 'SOCK' or 'MPI'.")
			}
			par.type <- 'SNOW'
		} else {
			if (.Platform$OS.type == "unix") par.type <- 'MULTICORE' 
			if (.Platform$OS.type != "unix") par.type <- 'SNOW'
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
	
	if (toupper(parallel.config[['BACKEND']]) == 'FOREACH') {
		par.type='FOREACH'
		if (parallel.config[['TYPE']]=="NA") {
			registerDoSEQ() # prevents warning message
			return(list(foreach.options=foreach.options, par.type=par.type))
		}
		if (parallel.config[['TYPE']]=="doMC") {
			registerDoMC(workers)
			return(list(foreach.options=foreach.options, par.type=par.type))
		}
		if (parallel.config[['TYPE']]=='doMPI') {
			doPar.cl <- startMPIcluster(count=workers)
			registerDoMPI(doPar.cl)
			return(list(doPar.cl=doPar.cl, foreach.options=foreach.options, par.type=par.type))
		}
		if (parallel.config[['TYPE']]=='doRedis') {
			redisWorker('jobs', port=10187) #  Doesn't seem to work.  Maybe get rid of this option/flavor?
			registerDoRedis('jobs')
			startLocalWorkers(n=workers, queue='jobs')
			return(list(jobs='jobs', foreach.options=foreach.options, par.type=par.type))
		}
		if (parallel.config[['TYPE']]=='doSNOW') {
			doPar.cl=makeCluster(workers, type='SOCK')
			registerDoSNOW(doPar.cl)
			return(list(doPar.cl=doPar.cl, foreach.options=foreach.options, par.type=par.type))
		}
		if (parallel.config[['TYPE']]=="doParallel") {
			if (par.type == 'SNOW') {
				doPar.cl <- makeCluster(workers, type='SOCK')
				registerDoParallel(doPar.cl)
				clusterEvalQ(doPar.cl, library(SGP))
				return(list(doPar.cl=doPar.cl, foreach.options=foreach.options, par.type=par.type))
			} else {
				registerDoParallel(workers)
				return(list(foreach.options=foreach.options, par.type=par.type))
			}
		}
	} # END if (FOREACH)

	if (par.type=='SNOW') {
		# if (parallel.config[['TYPE']]=='MPI') {
			# if (exists('par.start')) return() #don't try to restart a new config
		# }
		internal.cl <- makeCluster(eval(parse(text=workers)), type=parallel.config[['TYPE']]) # eval workers in case 'names' used
		clusterEvalQ(internal.cl, library(SGP))
		return(list(internal.cl=internal.cl, par.type=par.type)) #  workers=workers,
	}

	if (par.type=='MULTICORE') {
		return(list(workers=workers, par.type=par.type))
	}
}
