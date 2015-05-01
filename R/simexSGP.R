`simexSGP` <- 
function(
	state,
	variable=NULL,
	csem.data.vnames=NULL,
	csem.loss.hoss=NULL, 
	lambda,
	B,
	simex.sample.size,
	extrapolation,
	save.matrices,
	simex.use.my.coefficient.matrices=NULL,
	calculate.simex.sgps,
	verbose=FALSE) {
	
	if (is.null(verbose)) verbose <- FALSE
	if (verbose) message("\n\tStarted SIMEX SGP calculation ", rev(content_area.progression)[1], " Grade ", rev(tmp.gp)[1], " ", date())
	
	### To avoid R CMD check warnings & NULL for data.table and objects established later

	GRADE <- CONTENT_AREA <- YEAR <- V1 <- Lambda <- tau <- b <- .SD <- TEMP <- NULL

	###  Set objects passed from studentGrowthPercentiles' environment to themselves (in order of R CMD check warnings issued)

	content_area.progression <- content_area.progression
	tmp.gp <- tmp.gp
	get.my.knots.boundaries.path <- get.my.knots.boundaries.path
	sgp.labels <- sgp.labels
	use.my.coefficient.matrices <- use.my.coefficient.matrices
	exact.grade.progression.sequence <- exact.grade.progression.sequence
	num.prior <- num.prior
	if (is.null(simex.use.my.coefficient.matrices)) coefficient.matrix.priors <- coefficient.matrix.priors else coefficient.matrix.priors <- NULL
	.get.panel.data <- .get.panel.data
	ss.data <- ss.data
	by.grade <- by.grade
	year.progression <- year.progression
	Panel_Data <- Panel_Data
	taus <- taus
	Coefficient_Matrices <- Coefficient_Matrices
	tmp.path.coefficient.matrices <- tmp.path.coefficient.matrices
	grade.progression <- grade.progression
	year_lags.progression <- year_lags.progression
	knot.cut.percentiles <- knot.cut.percentiles
	ID <- ID
	parallel.config <- parallel.config
	isotonize <- isotonize
	sgp.loss.hoss.adjustment <- sgp.loss.hoss.adjustment
	.get.quantiles <- .get.quantiles
	print.other.gp <- print.other.gp
	print.sgp.order <- print.sgp.order
	return.norm.group.identifier <- return.norm.group.identifier
	
	my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
	if (is.logical(simex.use.my.coefficient.matrices)) if (! simex.use.my.coefficient.matrices) simex.use.my.coefficient.matrices <- NULL
	if (!is.null(state) & !is.null(variable)) stop("SIMEX config can not use both 'state' and 'variable' elements.")
	if (!is.null(state) & !is.null(csem.data.vnames)) stop("SIMEX config can not use both 'state' and 'csem.data.vnames' elements.")
	if (!is.null(csem.data.vnames) & !is.null(variable)) stop("SIMEX config can not use both 'csem.data.vnames' and 'variable' elements.")

	### Utility functions
	
	rq.mtx <- function(tmp.gp.iter, lam, rqdata) {
		mod <- character()
		s4Ks <- "Knots=list("
		s4Bs <- "Boundaries=list("
		for (i in seq_along(tmp.gp.iter)) {
			knt <- paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", lam, "']][['knots_", tmp.gp.iter[i], "']]", sep="")
			bnd <- paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", lam, "']][['boundaries_", tmp.gp.iter[i], "']]", sep="")
			mod <- paste(mod, " + bs(prior_", i, ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
			s4Ks <- paste(s4Ks, "knots_", tmp.gp.iter[i], "=", knt, ",", sep="")
			s4Bs <- paste(s4Bs, "boundaries_", tmp.gp.iter[i], "=", bnd, ",", sep="")
		}
		tmp.mtx <-eval(parse(text=paste("rq(final_yr ~", substring(mod,4), ", tau=taus, data = rqdata, method=rq.method)[['coefficients']]", sep="")))
		
		tmp.version <- list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date(), Matrix_Information=list(N=dim(rqdata)[1]))
		
		eval(parse(text=paste("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1, nchar(s4Ks)-1), "), ", substring(s4Bs, 1, nchar(s4Bs)-1), "), ",
													"Content_Areas=list(as.character(tail(content_area.progression, k+1))), ",
													"Grade_Progression=list(as.character(tail(tmp.slot.gp, k+1))), ",
													"Time=list(as.character(tail(year.progression, k+1))), ",
													"Time_Lags=list(as.numeric(tail(year_lags.progression, k))), ",
													"Version=tmp.version)", sep="")))
	}
	
	fitted <- extrap <- tmp.quantiles.simex <- simex.coef.matrices <- list()
	loss.hoss <- matrix(nrow=2,ncol=length(tmp.gp)-1)
	if (!is.null(csem.loss.hoss)) {
		if (!is.list(csem.loss.hoss)) stop("SIMEX config element 'csem.loss.hoss' must be a 2 level nested list with LOSS/HOSS data for each subject (level 1) by grade (level 2).")
		for (g in 1:ncol(loss.hoss)) {
			loss.hoss[,g] <- csem.loss.hoss[[rev(content_area.progression)[-1][g]]][[paste("loss.hoss_", rev(tmp.gp)[-1][g], sep="")]]
		}}
	if (!is.null(state)) {
		for (g in 1:ncol(loss.hoss)) {
			loss.hoss[,g] <- SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[rev(content_area.progression)[-1][g]]][[paste("loss.hoss_", rev(tmp.gp)[-1][g], sep="")]]
		}}
	if (!is.null(variable)) {
		for (g in 1:ncol(loss.hoss)) {
			loss.hoss[,g] <- variable[[paste("loss.hoss_", rev(tmp.gp)[-1][g], sep="")]]
		}}

	if (!is.null(use.my.coefficient.matrices)) { # Passed implicitly from studentGrowthPercentiles arguments
		if (exact.grade.progression.sequence) {
			simex.matrix.priors <- num.prior
		} else {
			simex.matrix.priors <- seq(num.prior)
		}
	} else simex.matrix.priors <- coefficient.matrix.priors
	
	for (k in simex.matrix.priors) {
		tmp.data <- .get.panel.data(ss.data, k, by.grade)
		tmp.num.variables <- dim(tmp.data)[2]
		tmp.gp.iter <- rev(tmp.gp)[2:(k+1)]
		tmp.ca.iter <- rev(content_area.progression)[2:(k+1)]
		tmp.yr.iter <- rev(year.progression)[2:(k+1)]
		if (is.null(csem.data.vnames)) {
			csem.int <- matrix(nrow=dim(tmp.data)[1], ncol=length(tmp.gp.iter)) # build matrix to store interpolated csem
			colnames(csem.int) <- paste("icsem", tmp.gp.iter, tmp.ca.iter, tmp.yr.iter, sep="")
		} else {
			csem.int <- data.table(Panel_Data[,c("ID", intersect(csem.data.vnames, names(Panel_Data))),with=FALSE], key="ID")
			setnames(csem.int, csem.data.vnames, paste("icsem", head(tmp.gp, -1), head(content_area.progression, -1), head(year.progression, -1), sep=""))
		}
		
		# interpolate csem for all scale scores except that of the last grade
		if (!is.null(state)) {
			for (g in seq_along(tmp.gp.iter)) {
				if ("YEAR" %in% names(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
					CSEM_Data <- subset(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], 
															GRADE==tmp.gp.iter[g] & CONTENT_AREA== tmp.ca.iter[g] & YEAR==tmp.yr.iter[g])
				} else {
					CSEM_Data <- subset(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]], 
															GRADE==tmp.gp.iter[g] & CONTENT_AREA== tmp.ca.iter[g])
				}
				if (dim(CSEM_Data)[1] == 0) stop(paste('CSEM data for', tmp.ca.iter[g], 'Grade', tmp.gp.iter[g], 'is required to use SIMEX functionality, but is not available in SGPstateData.  Please contact package administrators to add CSEM data.'))
				CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
				csem.int[, paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="")] <- CSEM_Function(tmp.data[[tmp.num.variables-g]])
			}
		}
		
		if (!is.null(variable)){
			# the following is added by YS on 021915 to accommodate missing data when using "variable"
			csem.tmp <- vector()
			for (g in seq_along(tmp.gp.iter)) {
				csem.tmp <- cbind(csem.tmp, variable[[paste("CSEM.grade", tmp.gp.iter[g], ".", tmp.ca.iter[g], sep="")]])
			}
			csem.tmp <- na.omit(csem.tmp)
			
			for (g in seq_along(tmp.gp.iter)) {
				csem.int[, paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="")] <- csem.tmp[,g]
			}
		}
		
		## naive model
		if (calculate.simex.sgps) {
			fitted[[paste("order_", k, sep="")]] <- matrix(0, nrow=length(lambda), ncol=dim(tmp.data)[1]*length(taus))
			tmp.matrix <- getsplineMatrices(
				Coefficient_Matrices[[tmp.path.coefficient.matrices]], 
				tail(content_area.progression, k+1), 
				tail(grade.progression, k+1),
				tail(year.progression, k+1),
				tail(year_lags.progression, k),
				my.matrix.order=k)[[1]]
			
			fitted[[paste("order_", k, sep="")]][1,] <- as.vector(.get.percentile.predictions(tmp.data, tmp.matrix))
		}
		
		if (verbose) message("\t\t", rev(content_area.progression)[1], " Grade ", rev(tmp.gp)[1], " Order ", k, " Started simulation process ", date())

		## perturb data
		if (!is.null(csem.data.vnames)) {
			tmp.data <- merge(tmp.data, csem.int, by="ID")
		}
		for (L in lambda[-1]) {
			big.data <- rbindlist(replicate(B, tmp.data, simplify = FALSE))
			big.data[, Lambda := rep(L, each=dim(tmp.data)[1]*B)]
			big.data[, b := rep(1:B, each=dim(tmp.data)[1])]
			setnames(big.data, tmp.num.variables, "final_yr")
			for (g in seq_along(tmp.gp.iter)) {
				col.index <- tmp.num.variables-g
				if (is.null(csem.data.vnames)) {
					setkeyv(big.data, c(names(big.data)[col.index], "final_yr", "b"))
					big.data.uniques <- unique(big.data)
					big.data.uniques.indices <- which(!duplicated(big.data))
					big.data.uniques[, paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="") := 
													 	rep(csem.int[, paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="")], B)[big.data.uniques.indices]]
				} else {
					setkeyv(big.data, c(names(big.data)[col.index], "final_yr", "b", paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="")))
					# big.data.uniques <- merge(big.data, csem.int[,c("ID", paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="")), with=F], by="ID")
					# setkeyv(big.data.uniques, c(names(big.data)[col.index], "final_yr", "b", paste("icsem", tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], sep="")))
					big.data.uniques <- unique(big.data)
				}
				big.data.uniques[, TEMP := 
												 	eval(parse(text=paste("big.data.uniques[[", tmp.num.variables-g, "]]+sqrt(big.data.uniques[['Lambda']])*big.data.uniques[['icsem",
												 												tmp.gp.iter[g], tmp.ca.iter[g], tmp.yr.iter[g], "']] * rnorm(dim(big.data.uniques)[1])", sep="")))]
				big.data.uniques[big.data.uniques[[col.index]] < loss.hoss[1,g], col.index := loss.hoss[1,g], with=FALSE]
				big.data.uniques[big.data.uniques[[col.index]] > loss.hoss[2,g], col.index := loss.hoss[2,g], with=FALSE]
				if (is.null(key(big.data.uniques))) setkeyv(big.data.uniques, key(big.data)) 
				big.data[, tmp.num.variables-g := big.data.uniques[,c(key(big.data), "TEMP"), with=FALSE][big.data][['TEMP']]]
				
				if (is.null(simex.use.my.coefficient.matrices)) {
					ks <- big.data[, as.list(as.vector(unlist(round(quantile(big.data[[col.index]], probs=knot.cut.percentiles, na.rm=TRUE), digits=3))))] # Knots
					bs <- big.data[, as.list(as.vector(round(extendrange(big.data[[col.index]], f=0.1), digits=3)))] # Boundaries
					lh <- big.data[, as.list(as.vector(round(extendrange(big.data[[col.index]], f=0.0), digits=3)))] # LOSS/HOSS
					
					eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['knots_", tmp.gp.iter[g], 
																"']] <- c(ks[,V1], ks[,V2], ks[,V3], ks[,V4])", sep="")))
					eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['boundaries_", tmp.gp.iter[g], 
																"']] <- c(bs[,V1], bs[,V2])", sep="")))
					eval(parse(text=paste("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['loss.hoss_", tmp.gp.iter[g], 
																"']] <- c(lh[,V1], lh[,V2])", sep="")))
				}
				
				setnames(big.data, tmp.num.variables-g, paste("prior_",g,sep=""))
				setkey(big.data, b, ID)
			}
			
			## Write big.data to disk and remove from memory
			dir.create("tmp_data", recursive=TRUE, showWarnings=FALSE)
			if (!exists('year.progression.for.norm.group')) year.progression.for.norm.group <- year.progression # Needed during Baseline Matrix construction
			tmp.dbname <- paste("tmp_data/", paste(tail(paste(year.progression.for.norm.group, 
																												paste(content_area.progression, grade.progression, sep="_"), sep="_"), num.prior+1), collapse="-"), ".sqlite", sep="")
			con <- dbConnect(SQLite(), dbname = tmp.dbname)
			dbWriteTable(con, name = "simex_data", value=big.data, overwrite=TRUE, row.names=0)
			dbDisconnect(con)
			rm(big.data);suppressMessages(gc())
			
			## Establish the simulation iterations - either 1) 1:B, or 2) a sample of either B or the number of previously computed matrices
			sim.iters <- 1:B
			
			if (!is.null(simex.use.my.coefficient.matrices)) { # Element from the 'calculate.simex' argument list.
				available.matrices <- unlist(getsplineMatrices(
					Coefficient_Matrices[[paste(tmp.path.coefficient.matrices, '.SIMEX', sep="")]][[
						paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]], 
					tail(content_area.progression, k+1), 
					tail(grade.progression, k+1),
					tail(year.progression, k+1),
					tail(year_lags.progression, k),
					my.exact.grade.progression.sequence=TRUE,
					return.multiple.matrices=TRUE,
					my.matrix.order=k), recursive=FALSE)
				
				if (length(available.matrices) > B) sim.iters <- sample(1:length(available.matrices), B) # Stays as 1:B when length(available.matrices) == B
				if (length(available.matrices) < B) sim.iters <- sample(1:length(available.matrices), B, replace=TRUE)
			}
			
			if (is.null(parallel.config)) { # Sequential
				if (verbose) message("\t\t\tStarted coefficient matrix calculation, Lambda ", L, ": ", date())
				if (is.null(simex.use.my.coefficient.matrices)) {
					for (z in seq_along(sim.iters)) {
						if (is.null(simex.sample.size) || dim(tmp.data)[1] <= simex.sample.size) {
							simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]][[z]] <-
								rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname), 
																																	paste("select * from simex_data where b in ('", z, "')", sep="")))
						} else {
							simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]][[z]] <-
								rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname), 
																																	paste("select * from simex_data where b in ('", z, "')", sep=""))[sample(seq(dim(tmp.data)[1]), simex.sample.size),])
						}
					}
				} else simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <- available.matrices[sim.iters]
				
				if (calculate.simex.sgps) {
					if (verbose) message("\t\t\tStarted percentile prediction calculation, Lambda ", L, ": ", date())
					for (z in seq_along(sim.iters)) {
						fitted[[paste("order_", k, sep="")]][which(lambda==L),] <- fitted[[paste("order_", k, sep="")]][which(lambda==L),] + 
							as.vector(.get.percentile.predictions(dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname), 
																															 paste("select ", paste(c("ID", paste('prior_', k:1, sep=""), "final_yr"), collapse=", "), " from simex_data where b in ('", z, "')", sep="")), 
																										simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]][[z]])/B)
					}
				}
			} else {	# Parallel over sim.iters
				
				par.start <- startParallel(parallel.config, 'SIMEX')
				
				## Note, that if you use the parallel.config for SIMEX here, you can also use it for TAUS in the naive analysis
				## Example parallel.config argument: '... parallel.config=list(BACKEND="PARALLEL", TYPE="PSOCK", WORKERS=list(SIMEX = 4, TAUS = 4))'
				
				## Calculate coefficient matricies (if needed/requested)
				if (is.null(simex.use.my.coefficient.matrices)) {
					if (verbose) message("\t\t\tStarted coefficient matrix calculation, Lambda ", L, ": ", date())
					if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
						if (is.null(simex.sample.size) || dim(tmp.data)[1] <= simex.sample.size) {
							simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <- 
								foreach(z=iter(sim.iters), .packages=c("quantreg", "data.table"), 
												.export=c("Knots_Boundaries", "rq.method", "taus", "content_area.progression", "tmp.slot.gp", "year.progression", "year_lags.progression"),
												.options.mpi=par.start$foreach.options, .options.multicore=par.start$foreach.options, .options.snow=par.start$foreach.options) %dopar% {
													rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																						paste("select * from simex_data where b in ('", z, "')", sep="")))
												}
						} else {
							simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <-
								foreach(z=iter(sim.iters), .packages=c("quantreg", "data.table"), 
												.export=c("Knots_Boundaries", "rq.method", "taus", "content_area.progression", "tmp.slot.gp", "year.progression", "year_lags.progression"),
												.options.mpi=par.start$foreach.options, .options.multicore=par.start$foreach.options, .options.snow=par.start$foreach.options) %dopar% {
													rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																						paste("select * from simex_data where b in ('", z, "')", sep=""))[sample(seq(dim(tmp.data)[1]), simex.sample.size),])
												}
						}
					} else {
						if (par.start$par.type == 'MULTICORE') {
							if (is.null(simex.sample.size) || dim(tmp.data)[1] <= simex.sample.size) {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <- 
									mclapply(sim.iters, function(z) rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																																		paste("select * from simex_data where b in ('", z, "')", sep=""))), mc.cores=par.start$workers)
							} else {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <- 
									mclapply(sim.iters, function(z) rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																																		paste("select * from simex_data where b in ('", z, "')", sep=""))[sample(seq(dim(tmp.data)[1]), simex.sample.size),]), 
													 mc.cores=par.start$workers)
							}
						}
						if (par.start$par.type == 'SNOW') {
							if (is.null(simex.sample.size) || dim(tmp.data)[1] <= simex.sample.size) {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <- 
									parLapply(par.start$internal.cl, sim.iters, function(z) rq.mtx(tmp.gp.iter[1:k], lam=L, 
																																								 rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname), paste("select * from simex_data where b in ('", z, "')", sep=""))))
							} else {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <-
									parLapply(par.start$internal.cl, sim.iters, function(z) rq.mtx(tmp.gp.iter[1:k], lam=L, rqdata=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																																														paste("select * from simex_data where b in ('", z, "')", sep=""))[sample(seq(dim(tmp.data)[1]), simex.sample.size),]))
							}
						}
					}
				} else {
					simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] <- available.matrices[sim.iters]
				}
				
				## get percentile predictions from coefficient matricies
				if (calculate.simex.sgps) {
					if (verbose) message("\t\t\tStarted percentile prediction calculation, Lambda ", L, ": ", date())
					if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
						mtx.subset <- simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]] # Save on memory copying to R SNOW workers
						environment(.get.percentile.predictions) <- environment()
						environment(.smooth.isotonize.row) <- environment()
						fitted[[paste("order_", k, sep="")]][which(lambda==L),] <- 
							foreach(z=iter(sim.iters), .combine="+", .export=c('tmp.gp', 'k', 'taus', 'sgp.loss.hoss.adjustment', 'isotonize'),
								.options.multicore=par.start$foreach.options) %dopar% { # .options.snow=par.start$foreach.options
									as.vector(.get.percentile.predictions(my.matrix=mtx.subset[[z]], my.data=dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
										paste("select ", paste(c("ID", paste('prior_', k:1, sep=""), "final_yr"), collapse=", "), " from simex_data where b in ('",z,"')", sep="")))/B)
							}
					} else {
						if (par.start$par.type == 'MULTICORE') {
							tmp.fitted <- mclapply(seq_along(sim.iters), function(z) {
								as.vector(.get.percentile.predictions(dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																 paste("select ", paste(c("ID", paste('prior_', k:1, sep=""), "final_yr"), collapse=", ")," from simex_data where b in ('",z,"')", sep="")), 
																											simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]][[z]])/B)
							}, mc.cores=par.start$workers)
							
							fitted[[paste("order_", k, sep="")]][which(lambda==L),] <- tmp.fitted[[1]]
							for (s in seq_along(sim.iters[-1])) {
								fitted[[paste("order_", k, sep="")]][which(lambda==L),] <- fitted[[paste("order_", k, sep="")]][which(lambda==L),] + tmp.fitted[[s]]
							}
						}
						if (par.start$par.type == 'SNOW') {
							tmp.fitted <- parLapply(par.start$internal.cl, seq_along(sim.iters), function(z) { 
								as.vector(.get.percentile.predictions(dbGetQuery(dbConnect(SQLite(), dbname = tmp.dbname),
																																 paste("select ", paste(c("ID", paste('prior_', k:1, sep=""), "final_yr"), collapse=", ")," from simex_data where b in ('",z,"')", sep="")), 
																											simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp,1), k, sep="_")]][[paste("lambda_", L, sep="")]][[z]])/B)
							})
							
							fitted[[paste("order_", k, sep="")]][which(lambda==L),] <- tmp.fitted[[1]]
							for (s in seq_along(sim.iters[-1])) {
								fitted[[paste("order_", k, sep="")]][which(lambda==L),] <- fitted[[paste("order_", k, sep="")]][which(lambda==L),] + tmp.fitted[[s]]
							}
						}
					}
				}
				stopParallel(parallel.config, par.start)
			}
		} ### END for (L in lambda[-1])
		unlink(tmp.dbname)
		if (verbose) message("\t\t", rev(content_area.progression)[1], " Grade ", rev(tmp.gp)[1], " Order ", k, " Simulation process complete ", date())
		
		if (calculate.simex.sgps) {
			switch(extrapolation,
						 LINEAR = fit <- lm(fitted[[paste("order_", k, sep="")]] ~ lambda),
						 QUADRATIC = fit <- lm(fitted[[paste("order_", k, sep="")]] ~ lambda + I(lambda^2)))
			extrap[[paste("order_", k, sep="")]] <- t(apply(matrix(predict(fit, newdata=data.frame(lambda=-1)), nrow=dim(tmp.data)[1]), 1, .smooth.isotonize.row, isotonize, sgp.loss.hoss.adjustment))
			tmp.quantiles.simex[[k]] <- data.table(ID=tmp.data[["ID"]], SIMEX_ORDER=k, 
																						 SGP_SIMEX=.get.quantiles(extrap[[paste("order_", k, sep="")]], tmp.data[[tmp.num.variables]]))
		}
	} ### END for (k in simex.matrix.priors)
	
	if (verbose) message("\tFinished SIMEX SGP calculation ", rev(content_area.progression)[1], " Grade ", rev(tmp.gp)[1], " ", date())

	if (is.null(save.matrices)) simex.coef.matrices <- NULL
	if (calculate.simex.sgps) {
		quantile.data.simex <- data.table(rbindlist(tmp.quantiles.simex), key=c("ID", "SIMEX_ORDER"))
		setkey(quantile.data.simex, ID) # first key on ID and SIMEX_ORDER, then re-key on ID only to insure sorted order. Don't rely on rbindlist/k ordering...
	} else quantile.data.simex <- data.table("ID"=NA, "SIMEX_ORDER"=NA, "SGP_SIMEX"=NA) # set up empty data.table for reshapes and subsets below.
	if (print.other.gp) {
		return(list(
			DT = data.table(reshape(quantile.data.simex, idvar="ID", timevar="SIMEX_ORDER", direction="wide"),
											SGP_SIMEX=quantile.data.simex[c(which(!duplicated(quantile.data.simex))[-1]-1L, nrow(quantile.data.simex))][["SGP_SIMEX"]]),
			MATRICES = simex.coef.matrices))
	} else {
		if (print.sgp.order | return.norm.group.identifier) {
			return(list(
				DT = quantile.data.simex[c(which(!duplicated(quantile.data.simex))[-1]-1L, nrow(quantile.data.simex))],
				MATRICES = simex.coef.matrices))
		} else {
			return(list(
				DT = quantile.data.simex[c(which(!duplicated(quantile.data.simex))[-1]-1L, nrow(quantile.data.simex)), c("ID", "SGP_SIMEX"), with=FALSE],
				MATRICES = simex.coef.matrices))
		}
	}
	if (verbose) message("\tFinished SIMEX SGP calculation ", rev(content_area.progression)[1], " Grade ", rev(tmp.gp)[1], " ", date(), "\n")
} ### END simexSGP function
