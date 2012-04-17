`stopParallel` <- 
function(
		parallel.config,
		par.start
		) {
	
	
	if (identical(toupper(parallel.config[['BACKEND']]), 'FOREACH')) {

		if (identical(parallel.config[['TYPE']], 'doMPI')) {
			closeCluster(par.start$doPar.cl)
			return()
		}

		if (identical(parallel.config[['TYPE']], 'doParallel') & identical(par.start[['par.type']], 'SNOW')) {
			stopCluster(par.start$doPar.cl)
			return()
		}
		
		if (identical(parallel.config[['TYPE']], 'doSNOW')) {
			stopCluster(par.start$doPar.cl)
			return()
		}
		
		if (identical(parallel.config[['TYPE']], 'doRedis')) {
			removeQueue(par.start$jobs)
			return()
		}
	} #  END FOREACH

	# Nothing required for MULTICORE (or doMC)
	# if (identical(toupper(parallel.config[['BACKEND']]), 'MULTICORE')) {
	# }

	if (identical(par.start[['par.type']], 'SNOW')) {
		if (is.null(parallel.config[['CLUSTER.OBJECT']]))	 {
			stopCluster(par.start$internal.cl)
		}
	}
}
