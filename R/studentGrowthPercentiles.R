`studentGrowthPercentiles` <-
function(panel.data,         ## REQUIRED
         sgp.labels,         ## REQUIRED
         panel.data.vnames=NULL,
         additional.vnames.to.return=NULL,
         grade.progression,
         content_area.progression=NULL,
         year.progression=NULL,
         year_lags.progression=NULL,
         num.prior,
         max.order.for.percentile=NULL,
         return.additional.max.order.sgp=NULL,
         subset.grade,
         percentile.cuts=NULL,
         growth.levels,
         use.my.knots.boundaries,
         use.my.coefficient.matrices=NULL,
         calculate.confidence.intervals=NULL,
         print.other.gp=FALSE,
         print.sgp.order=FALSE,
         calculate.sgps=TRUE,
         rq.method="br",
         rq.method.for.large.n="fn",
         max.n.for.coefficient.matrices=NULL,
         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),
         knots.boundaries.by.panel=FALSE,
         exact.grade.progression.sequence=FALSE,
         drop.nonsequential.grade.progression.variables=TRUE,
         convert.0and100=TRUE,
         sgp.quantiles="Percentiles",
         sgp.quantiles.labels=NULL,
         sgp.loss.hoss.adjustment=NULL,
         sgp.cohort.size=NULL,
         sgp.less.than.sgp.cohort.size.return=NULL,
         sgp.test.cohort.size=NULL,
         percuts.digits=0L,
         isotonize=TRUE,
         convert.using.loss.hoss=TRUE,
         goodness.of.fit=TRUE,
         goodness.of.fit.minimum.n=NULL,
         goodness.of.fit.output.format="GROB",
         return.prior.scale.score=TRUE,
         return.prior.scale.score.standardized=TRUE,
         return.norm.group.identifier=TRUE,
         return.norm.group.scale.scores=NULL,
         return.norm.group.dates=NULL,
         return.norm.group.preference=NULL,
         return.panel.data=identical(parent.frame(), .GlobalEnv),
         print.time.taken=TRUE,
         parallel.config=NULL,
         calculate.simex=NULL,
         sgp.percentiles.set.seed=314159,
         sgp.percentiles.equated=NULL,
         SGPt=NULL,
         SGPt.max.time=NULL,
         verbose.output=FALSE) {

	started.at <- proc.time()
	started.date <- prettyDate()

	##########################################################
	###
	### Internal utility functions
	###
	##########################################################

	.smooth.bound.iso.row <- function(tmp.dt, iso=isotonize, sgp.loss.hoss.adjustment) {
		X <- NULL
		if (!is.null(sgp.loss.hoss.adjustment)) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels[['my.subject']], as.character(sgp.labels[['my.year']]))
			bnd <- eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']]")))
			tmp.dt[X < bnd[1L], X:=bnd[1L]]
			tmp.dt[X > bnd[2L], X:=bnd[2L]]
		}
		if (iso) setkey(tmp.dt, ID, X)
		return(tmp.dt[['X']])
	}

	.create.path <- function(labels, pieces=c("my.subject", "my.year", "my.extra.label")) {
		sub(' ', '_', toupper(sub('\\.+$', '', paste(unlist(lapply(labels[pieces], as.character)), collapse="."))))
	}

	.get.knots.boundaries <- function(data, by.grade) {
		num.panels <- (dim(data)[2L]-1L)/2L

		if (knots.boundaries.by.panel) {
			tmp.years <- rep(yearIncrement(sgp.labels$my.year, (-num.panels+1L):-1L), each=dim(data)[1L])
		} else {
			tmp.years <- rep(sgp.labels$my.year, dim(data)[1L]*(num.panels-1L))
		}

		if (by.grade) {
			tmp.grades <- unlist(data[,2L:(2L+num.panels-2L), with=FALSE], use.names=FALSE)
		} else {
			tmp.grades <- rep(head(tmp.gp, -1L), each=dim(data)[1L])
		}

		tmp.stack <- data.table(
			VALID_CASE="VALID_CASE",
			CONTENT_AREA=rep(head(content_area.progression, -1L), each=dim(data)[1L]),
			GRADE=tmp.grades,
			SCALE_SCORE=unlist(data[,(2L+num.panels):(2L+2*num.panels-2L), with=FALSE], use.names=FALSE),
			YEAR=tmp.years, key=c("VALID_CASE", "CONTENT_AREA", "GRADE"))

		createKnotsBoundaries(tmp.stack, knot.cut.percentiles)
	}

	.get.panel.data <- function(tmp.data, k, by.grade, tmp.gp) {
		if (by.grade) {
            if (is.character(tmp.gp)) tmp.gp.gpd <- shQuote(rev(tmp.gp)[seq(k+1L)]) else tmp.gp.gpd <- rev(tmp.gp)[seq(k+1L)]
            eval(parse(text=paste0("na.omit(tmp.data[.(", paste(tmp.gp.gpd, collapse=", "), "), on=names(tmp.data)[c(", paste(1L+num.panels-(0:k), collapse=", ") , ")]], cols=names(tmp.data)[c(",paste(1L+2*num.panels-0:k, collapse=", "), ")])[,c(1, ", paste(rev(1+2*num.panels-0:k), collapse=", "),  ")]")))
		} else {
            eval(parse(text=paste0("na.omit(tmp.data, cols=names(tmp.data)[c(",paste(1+2*num.panels-0:k, collapse=", "), ")])[,c(1, ", paste(rev(1L+2*num.panels-0:k), collapse=", "),  ")]")))
		}
	}

	get.my.knots.boundaries.path <- function(content_area, year) {
		if (is.null(sgp.percentiles.equated)) {
			tmp.knots.boundaries.names <-
				names(Knots_Boundaries[[tmp.path.knots.boundaries]])[content_area==sapply(strsplit(names(Knots_Boundaries[[tmp.path.knots.boundaries]]), "[.]"), '[', 1L)]
			if (length(tmp.knots.boundaries.names)==0L) {
				return(paste0("[['", tmp.path.knots.boundaries, "']]"))
			} else {
				tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), '[', 2L)
				tmp.index <- sum(year >= tmp.knots.boundaries.years, na.rm=TRUE)
				return(paste0("[['", tmp.path.knots.boundaries, "']][['", paste(c(content_area, sort(tmp.knots.boundaries.years)[tmp.index]), collapse="."), "']]"))
			}
		} else {
			return(paste0("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", sgp.percentiles.equated[['Year']], "']]"))
		}
	}

	get.prior.cutscore.path <- function(content_area, year) {
		if (is.null(sgp.percentiles.equated)) {
			tmp.cutscores <- grep(content_area, names(SGP::SGPstateData[[goodness.of.fit]][['Achievement']][['Cutscores']]), value=TRUE)
			if (length(tmp.cutscores) > 0L) {
				tmp.cutscores.names <- tmp.cutscores[content_area==sapply(strsplit(tmp.cutscores, "[.]"), '[', 1L)]
				tmp.cutscores.years <- sapply(strsplit(tmp.cutscores.names[grep(content_area, tmp.cutscores.names)], "[.]"), function(x) paste(x[-1L], collapse="."))
				tmp.cutscores.years[tmp.cutscores.years==""]<-NA
				tmp.sum <- sum(year >= sort(tmp.cutscores.years), na.rm=TRUE)
				return(paste(c(content_area, sort(tmp.cutscores.years)[tmp.sum]), collapse="."))
			} else return(content_area)
		} else {
			return(paste(content_area, sgp.percentiles.equated[['Year']], sep="."))
		}
	}

	.create.coefficient.matrices <- function(data, k, by.grade, max.n.for.coefficient.matrices) {

		rq.sgp <- function(..., my.taus) { # Function needs to be nested within the .create.coefficient.matrices function to avoid data copying with SNOW
			if (rq.method == "br") {
				tmp.res <- rq(method="br", ...)[['coefficients']]
			} else {
				tmp.res <- try(rq(method=rq.method, ...)[['coefficients']], silent=TRUE)
				if (inherits(tmp.res, "try-error")) {
					tmp.res <- rq(method="br", ...)[['coefficients']]
				}
			}
			if (!is.matrix(tmp.res)) return(matrix(tmp.res, dimnames=list(names(tmp.res), paste("tau=", my.taus))))
			return(tmp.res)
		}
		tmp.data <- .get.panel.data(data, k, by.grade, tmp.gp)
		if (dim(tmp.data)[1L]==0L) return(NULL)
		if (dim(tmp.data)[1L] < sgp.cohort.size) return("Insufficient N")
		if (!is.null(max.n.for.coefficient.matrices) && dim(tmp.data)[1L] > max.n.for.coefficient.matrices) tmp.data <- tmp.data[sample(seq.int(dim(tmp.data)[1L]), max.n.for.coefficient.matrices)]
        if (is.null(max.n.for.coefficient.matrices) && dim(tmp.data)[1L] >= 300000L) rq.method <- rq.method.for.large.n
		tmp.num.variables <- dim(tmp.data)[2L]
		mod <- character()
		s4Ks <- "Knots=list("
		s4Bs <- "Boundaries=list("
		tmp.gp.iter <- rev(tmp.gp)[2L:(k+1L)]
		for (i in seq_along(tmp.gp.iter)) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(rev(content_area.progression)[i+1L], yearIncrement(rev(year.progression)[i+1L], 0L))
			.check.knots.boundaries(names(eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries)))), tmp.gp.iter[i])
			knt <- paste0("Knots_Boundaries", my.path.knots.boundaries, "[['knots_", tmp.gp.iter[i], "']]")
			bnd <- paste0("Knots_Boundaries", my.path.knots.boundaries, "[['boundaries_", tmp.gp.iter[i], "']]")
			mod <- paste0(mod, " + bs(tmp.data[[", tmp.num.variables-i, "]], knots=", knt, ", Boundary.knots=", bnd, ")")
			s4Ks <- paste0(s4Ks, "knots_", tmp.gp.iter[i], "=", knt, ",")
			s4Bs <- paste0(s4Bs, "boundaries_", tmp.gp.iter[i], "=", bnd, ",")
		}
		if (!is.null(SGPt)) {
			tmp.data <- Panel_Data[,c("ID", "TIME", "TIME_LAG"), with=FALSE][tmp.data, on="ID"][,c(names(tmp.data), "TIME", "TIME_LAG"), with=FALSE]
			mod <- paste0(mod, " + I(tmp.data[['TIME']]) + I(tmp.data[['TIME_LAG']])")
		}

		if (!is.null(tmp.par.config <- parallel.config)) if (is.null(parallel.config[["WORKERS"]][["TAUS"]])) tmp.par.config <- NULL

		if (is.null(tmp.par.config)) {
			tmp.mtx <- eval(parse(text=paste0("rq.sgp(tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=taus, data=tmp.data, my.taus=taus)")))
		} else {
			par.start <- startParallel(tmp.par.config, 'TAUS', qr.taus=taus)

			if (toupper(tmp.par.config[["BACKEND"]]) == "FOREACH") {
				tmp.mtx <- foreach(x = iter(par.start$TAUS.LIST), .export=c("tmp.data", "Knots_Boundaries", "rq.method", "rq.sgp", "get.my.knots.boundaries.path"), .combine = "cbind", .errorhandling = "pass",
				.inorder=TRUE, .options.mpi = par.start$foreach.options, .options.multicore = par.start$foreach.options, .options.snow = par.start$foreach.options) %dopar% {
					eval(parse(text=paste0("rq.sgp(formula=tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=x, data=tmp.data, my.taus=x)")))
				}
				if (any(!grepl("tau=", colnames(tmp.mtx)))) return(list(RQ_ERROR=unlist(tmp.mtx[,grep("tau=", colnames(tmp.mtx), invert=TRUE)][1L], use.names=FALSE)))
			} else {
				if (par.start$par.type == 'MULTICORE') {
					tmp.mtx <- mclapply(par.start$TAUS.LIST, function(x) eval(parse(text=paste0("rq.sgp(tmp.data[[", tmp.num.variables, "]] ~ ",
						substring(mod,4), ", tau=x, data=tmp.data, my.taus=x)"))), mc.cores=par.start$workers, mc.preschedule = FALSE)
					if (any(tmp.tf <- sapply(tmp.mtx, function(x) inherits(x, "try-error")))) return(list(RQ_ERROR=sapply(which(tmp.tf), function(f) tmp.mtx[[f]][1L])))
					tmp.mtx <- do.call(cbind, tmp.mtx)
				}

				if (par.start$par.type == 'SNOW') {
					tmp.mtx <- parLapplyLB(par.start$internal.cl, par.start$TAUS.LIST, function(x) eval(parse(text=paste0("rq.sgp(tmp.data[[",
						tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=x, data=tmp.data, my.taus=x)"))))
					if (any(tmp.tf <- sapply(tmp.mtx, function(x) inherits(x, "try-error")))) return(list(RQ_ERROR=sapply(which(tmp.tf), function(f) tmp.mtx[[f]][1L])))
					tmp.mtx <- do.call(cbind, tmp.mtx)
				}
			}
			stopParallel(tmp.par.config, par.start)
		}

		tmp.version <- list(
			SGP_Package_Version=as.character(packageVersion("SGP")),
			Date_Prepared=prettyDate(),
			Matrix_Information=list(
				N=dim(tmp.data)[1L],
				Model=paste0("rq.sgp(tmp.data[[", tmp.num.variables, "]] ~ ", substring(mod,4), ", tau=taus, data=tmp.data, method=", rq.method, ")"),
				SGPt=if (is.null(SGPt)) NULL else list(VARIABLES=unlist(SGPt), MAX_TIME=max(tmp.data$TIME, na.rm=TRUE), MAX_TIME_PRIOR=max(tmp.data$TIME-tmp.data$TIME_LAG, na.rm=TRUE), RANGE_TIME_LAG=range(tmp.data$TIME_LAG))))

		eval(parse(text=paste0("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1L, nchar(s4Ks)-1L), "), ", substring(s4Bs, 1L, nchar(s4Bs)-1L), "), ",
			"Content_Areas=list(as.character(tail(content_area.progression, k+1L))), ",
			"Grade_Progression=list(as.character(tail(tmp.slot.gp, k+1L))), ",
			"Time=list(as.character(tail(year.progression, k+1L))), ",
			"Time_Lags=list(as.numeric(tail(year_lags.progression, k))), ",
			"Version=tmp.version)")))

	} ### END .create.coefficient.matrices

	.check.knots.boundaries <- function(names, grade) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		if (!grade %in% tmp[tmp[,1L]=="knots", 2L]) stop(paste0("knots_", grade, " not found in Knots_Boundaries."))
		if (!grade %in% tmp[tmp[,1L]=="boundaries", 2L]) stop(paste0("boundaries_", grade, " not found in Knots_Boundaries."))
	}

	.create_taus <- function(sgp.quantiles) {
		if (is.character(sgp.quantiles)) {
			taus <- switch(sgp.quantiles,
				PERCENTILES = (seq.int(100)-0.5)/100)
		}
		if (is.numeric(sgp.quantiles)) {
			taus <- sgp.quantiles
		}
		return(taus)
	}

	get.coefficient.matrix.name <- function(tmp.last, k) {
		return(paste0("qrmatrix_", tmp.last, "_", k))
	}

	.get.percentile.predictions <- function(my.data, my.matrix, SGPt.max.time=NULL) {
		SCORE <- TIME <- TIME_LAG <- NULL
		mod <- character()
		for (k in seq_along(my.matrix@Time_Lags[[1L]])) {
			knt <- paste0("my.matrix@Knots[[", k, "]]")
			bnd <- paste0("my.matrix@Boundaries[[", k, "]]")
			mod <- paste0(mod, ", bs(my.data[[", dim(my.data)[2L]-k, "]], knots=", knt, ", Boundary.knots=", bnd, ")")
		}
		if (!is.null(SGPt)) {
			my.data <- Panel_Data[,c("ID", "TIME", "TIME_LAG"), with=FALSE][my.data, on="ID"][,c(names(my.data), "TIME", "TIME_LAG"), with=FALSE]
            tmp.time.shift.index <- getTimeShiftIndex(max(as.numeric(my.data[['TIME']])), my.matrix)
            if (max(my.data$TIME+365*-tmp.time.shift.index, na.rm=TRUE) > my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]+30) stop("Matrix Misfit with TIME data!!!")
            if (is.null(SGPt.max.time) && tmp.time.shift.index != 0) my.data[,TIME:=TIME+365*-tmp.time.shift.index]
            if (!is.null(SGPt.max.time)) {
                my.data[,TIME_LAG:=TIME_LAG+my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]-(TIME+365*-tmp.time.shift.index)]
                my.data[,TIME:=my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]]
            }
			mod <- paste0(mod, ", my.data[['TIME']], my.data[['TIME_LAG']]")
		}
        tmp <- eval(parse(text=paste0("cbind(1L, ", substring(mod, 2L), ") %*% my.matrix")))
        return(round(matrix(.smooth.bound.iso.row(data.table(ID=rep(seq.int(dim(tmp)[1L]), each=length(taus)), X=c(t(tmp))), isotonize, sgp.loss.hoss.adjustment), ncol=length(taus), byrow=TRUE), digits=5L))
	}

	.get.quantiles <- function(data1, data2, ranked.simex=FALSE) {
        if (is.character(ranked.simex)) {
            reproduce.old.values <- TRUE; ranked.simex <- TRUE
        } else reproduce.old.values <- FALSE

        if (ranked.simex) {
          for (p in seq.int(3)) { # Additional values between the tau predicted values - 1/8th percentiles for ranking
            dataX <- data1[,(seq.int(ncol(data1))-1L)] + t(apply(data1, 1, diff))/2
            data1 <- cbind(data1, dataX)[, order(c(seq.int(ncol(data1)), seq.int(ncol(dataX))))]
            if (!is.matrix(data1) & is.vector(data1)) {  #  Account for edge cases where a single student is fed in (e.g., baseline)
              data1 <- matrix(data1, nrow=1, byrow=TRUE)
            }
          }
          tmp.zero <- 794L
        } else tmp.zero <- 101L

        V1 <- NULL
        tmp <- as.data.table(max.col(cbind(data1 < data2, FALSE), "last"))[V1==tmp.zero, V1 := 0L]
        if (ranked.simex) tmp[, V1 := V1/8]

        if (!is.null(sgp.quantiles.labels)) {
            setattr(tmp[['V1']] <- as.factor(tmp[['V1']]), "levels", sgp.quantiles.labels)
            return(as.integer(levels(tmp[['V1']]))[tmp[['V1']]])
        } else {
            if (!is.null(sgp.loss.hoss.adjustment)) {
                my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
                tmp.hoss <- eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']][2L]")))
                if (length(tmp.index <- which(data2>=tmp.hoss)) > 0L) {
                    if (ranked.simex) {
                        tmp[tmp.index, V1:=as.double(apply(data.table(data1 > data2, TRUE)[tmp.index], 1, function(x) which.max(x)-1L))]
                        if (!reproduce.old.values) tmp[tmp.index, V1 := V1/8]
                    } else tmp[tmp.index, V1:=apply(data.table(data1 > data2, TRUE)[tmp.index], 1, function(x) which.max(x)-1L)]
                }
            }
            if (convert.0and100) {
                tmp[V1==0L, V1:=1L]
                tmp[V1==100L, V1:=99L]
            }
            return(tmp[['V1']])
        }
    }

	.get.percentile.cuts <- function(data1) {
		tmp <- round(data1[ , percentile.cuts+1L, drop=FALSE], digits=percuts.digits)
		if (convert.using.loss.hoss) {
			my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels[['my.subject']], as.character(sgp.labels[['my.year']]))
			bnd <- eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']]")))
			tmp[tmp < bnd[1L]] <- bnd[1L]
			tmp[tmp > bnd[2L]] <- bnd[2L]
		}
		colnames(tmp) <- paste0("PERCENTILE_CUT_", percentile.cuts)
		return(tmp)
	}

    .get.best.cuts <- function(list.of.cuts, label.suffix=NULL) {
        cuts.best <- data.table(rbindlist(list.of.cuts), key="ID")
        cuts.best <- cuts.best[c(which(!duplicated(cuts.best, by=key(cuts.best)))[-1L]-1L, dim(cuts.best)[1L])][,-1L, with=FALSE]
        if (!is.null(label.suffix)) setnames(cuts.best, names(cuts.best), paste(names(cuts.best), label.suffix, sep="_"))
        return(cuts.best)
    }

	split.location <- function(years) sapply(strsplit(years, '_'), length)[1L]

	###
	### SIMEX function
	###
  simex.sgp <- function(
		state,
    csem.data.vnames=NULL,
    lambda,
    B,
    simex.sample.size,
    extrapolation,
    save.matrices,
    simex.use.my.coefficient.matrices=NULL,
    calculate.simex.sgps,
    dependent.var.error=FALSE,
    use.cohort.for.ranking=FALSE,
    reproduce.old.values=FALSE,
    verbose=FALSE) {

      GRADE <- CONTENT_AREA <- YEAR <- V1 <- Lambda <- tau <- b <- .SD <- TEMP <- CSEM <- VARIABLE <- NULL ## To avoid R CMD check warnings

      ### simex.sgp internal utility functions

      getSIMEXdata <- function(dbase, z, k=NULL, predictions=FALSE) {
        if (predictions) {
            data.table::fread(file.path(dbase, paste0("simex_data_", z, ".csv")), header=TRUE, showProgress = FALSE,
            select = paste(c("ID", paste0('prior_', k:1L), "final_yr")), verbose = FALSE)
        } else {
            data.table::fread(file.path(dbase, paste0("simex_data_", z, ".csv")), header=TRUE, showProgress = FALSE, verbose = FALSE)
        }
      } ### END getSIMEXdata function

      rq.sgp <- function(...) { # Function needs to be nested within the simex.sgp function to avoid data copying with SNOW
        if (rq.method == "br") {
          tmp.res <- quantreg::rq(method="br", ...)[['coefficients']]
        } else {
          tmp.res <- try(quantreg::rq(method=rq.method, ...)[['coefficients']], silent=TRUE)
          if (inherits(tmp.res, "try-error")) {
              tmp.res <- quantreg::rq(method="br", ...)[['coefficients']]
          }
        }
        return(tmp.res)
      } ### END rq.sgp function

      rq.mtx <- function(tmp.gp.iter, lam, rqdata) {
        mod <- character()
        s4Ks <- "Knots=list("
        s4Bs <- "Boundaries=list("
        for (i in seq_along(tmp.gp.iter)) {
          knt <- paste0("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", lam, "']][['knots_", tmp.gp.iter[i], "']]")
          bnd <- paste0("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", lam, "']][['boundaries_", tmp.gp.iter[i], "']]")
          mod <- paste0(mod, " + bs(prior_", i, ", knots=", knt, ", Boundary.knots=", bnd, ")")
          s4Ks <- paste0(s4Ks, "knots_", tmp.gp.iter[i], "=", knt, ",")
          s4Bs <- paste0(s4Bs, "boundaries_", tmp.gp.iter[i], "=", bnd, ",")
        }
        tmp.mtx <- eval(parse(text=paste0("rq.sgp(final_yr ~", substring(mod,4), ", tau=taus, data = rqdata)")))

        tmp.version <- list(
          SGP_Package_Version=as.character(packageVersion("SGP")),
          Date_Prepared=prettyDate(),
          Matrix_Information=list(
            N=dim(rqdata)[1L],
            Model=paste0("rq.sgp(final_yr ~", substring(mod,4), ", tau=taus, data = rqdata)"),
            SGPt=if (is.null(SGPt)) NULL else list(VARIABLES=unlist(SGPt), MAX_TIME=max(rqdata$TIME, na.rm=TRUE), MAX_TIME_PRIOR=max(rqdata$TIME-rqdata$TIME_LAG, na.rm=TRUE), RANGE_TIME_LAG=range(rqdata$TIME_LAG))))

        eval(parse(text= paste0("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1L, nchar(s4Ks)-1L), "), ", substring(s4Bs, 1L, nchar(s4Bs)-1L), "), ",
                                "Content_Areas=list(as.character(tail(content_area.progression, k+1L))), ",
                                "Grade_Progression=list(as.character(tail(tmp.slot.gp, k+1L))), ",
                                "Time=list(as.character(tail(year.progression, k+1L))), ",
                                "Time_Lags=list(as.numeric(tail(year_lags.progression, k))), ",
                                "Version=tmp.version)")))
      } ### END rq.mtx function

      createBigData <- function(tmp.data, perturb.var, L, dependent.var.error) { # Function that creates big.data object from which SIMEX SGPs are calculated
        big.data <- rbindlist(replicate(B, tmp.data, simplify = FALSE))[, b:=rep(seq.int(B), each=n.records)]
        if (dependent.var.error) csem.col.offset <- (ncol(big.data)-2)/2 else csem.col.offset <- (ncol(big.data)-1)/2
        for (perturb.var.iter in rev(seq_along(perturb.var))) {
          setnames(big.data, c(1L+perturb.var.iter, 1L+perturb.var.iter+csem.col.offset), c("VARIABLE", "CSEM"))
          unique.key <- c("VARIABLE", names(big.data)[1L+csem.col.offset], "b", "CSEM")
          setkeyv(big.data, unique.key)
          big.data[,(1L+perturb.var.iter) := unique(big.data, by=unique.key)[,"TEMP" := VARIABLE+sqrt(L)*CSEM*rnorm(.N)][big.data[,unique.key, with=FALSE], on=unique.key][["TEMP"]]]
          setnames(big.data, c("VARIABLE", "CSEM"), paste0(c("VARIABLE", "DONE_CSEM"), perturb.var.iter))
        }
        big.data[,grep("DONE", names(big.data), value=TRUE):=NULL]
        setnames(big.data, c("ID", paste0("prior_", (csem.col.offset-1L):1L), "final_yr", "b"))
        setkey(big.data, b, ID)
        return(big.data)
      } ### END createBigData function

      get.simex.ranking.info <- function(table_list, cap, gp, yp, ylp) {
        table.index <- which(sapply(table_list, function(f) {
          attr(f, "content_area_progression") == cap &&
          attr(f, "grade_progression") == gp &&
          attr(f, "year_progression") == yp &&
          attr(f, "year_lags_progression") == ylp}))
        return(table_list[[table.index]])
      }

      ### Check arguments/define variables
      if (is.null(dependent.var.error)) dependent.var.error <- FALSE
      if (is.null(use.cohort.for.ranking)) use.cohort.for.ranking <- FALSE
      if (is.null(reproduce.old.values)) reproduce.old.values <- FALSE
      if (is.null(verbose)) verbose <- FALSE
      if (verbose) messageSGP(c("\n\tStarted SIMEX SGP calculation ", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " ", prettyDate()))
      if (is.logical(simex.use.my.coefficient.matrices) && !simex.use.my.coefficient.matrices) simex.use.my.coefficient.matrices <- NULL
      if (!is.null(state) && !is.null(csem.data.vnames)) stop("SIMEX config can not use both 'state' and 'csem.data.vnames' elements.")
      if (!is.null(parallel.config)) {
        if (is.null(parallel.config[["WORKERS"]][["SIMEX"]])) tmp.par.config <- NULL else tmp.par.config <- parallel.config
      } else tmp.par.config <- NULL

		fitted <- extrap <- tmp.quantiles.simex <- simex.coef.matrices <- list()
		my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))

    if (!is.null(csem.data.vnames)) {
      if (length(content_area.progression)==length(csem.data.vnames)) csem.data.vnames <- head(csem.data.vnames, -1L)
      if (length(content_area.progression) < length(csem.data.vnames)) csem.data.vnames <- tail(head(csem.data.vnames, -1L), (length(csem.data.vnames)-(length(csem.data.vnames)-length(content_area.progression)+1L))) # grep(paste(head(content_area.progression, -1L), collapse="|"), csem.data.vnames, value=TRUE)
    }
		if (!is.null(use.my.coefficient.matrices)) { # Passed implicitly from studentGrowthPercentiles arguments
			if (exact.grade.progression.sequence) {
				simex.matrix.priors <- num.prior
			} else {
				simex.matrix.priors <- seq(num.prior)
			}
		} else simex.matrix.priors <- coefficient.matrix.priors

        ### Loop over priors
		for (k in simex.matrix.priors) {
			tmp.data <- .get.panel.data(ss.data, k, by.grade, tmp.gp)
			n.records <- nrow(tmp.data)
			tmp.num.variables <- dim(tmp.data)[2L]
			tmp.gp.iter <- rev(tmp.gp)[2L:(k+1L)]
			if (dependent.var.error) {
				perturb.var <- rev(tmp.gp)[seq.int((k+1L))]
				start.index <- 1L
				num.perturb.vars <- tmp.num.variables+1L
			} else {
				perturb.var <- tmp.gp.iter
				start.index <- 2L
				num.perturb.vars <- tmp.num.variables
			}
			tmp.ca.iter <- rev(content_area.progression)[start.index:(k+1L)]
			tmp.yr.iter <- rev(year.progression)[start.index:(k+1L)]

			## naive model
      # if (calculate.simex.sgps) { # Always calculate SIMEX SGPs (for ranked SIMEX table)
      fitted[[paste0("order_", k)]] <- matrix(0, nrow=length(lambda), ncol=n.records*length(taus))
      tmp.matrix <- getsplineMatrices(
        Coefficient_Matrices[[tmp.path.coefficient.matrices]],
        tail(content_area.progression, k+1L),
        tail(grade.progression, k+1L),
        tail(year.progression, k+1L),
        tail(year_lags.progression, k),
        my.matrix.order=k,
        my.matrix.time.dependency=SGPt)[[1L]]

      fitted[[paste0("order_", k)]][1L,] <- c(.get.percentile.predictions(tmp.data, tmp.matrix))
      # }

			# add csems to tmp.data
			if (!is.null(state)) {
				for (g in rev(seq_along(perturb.var))) {
					if ("YEAR" %in% names(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
						CSEM_Data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]][
							GRADE==perturb.var[g] & CONTENT_AREA==tmp.ca.iter[g] & YEAR==tmp.yr.iter[g]]
					} else {
						CSEM_Data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]][
							GRADE==perturb.var[g] & CONTENT_AREA==tmp.ca.iter[g]]
					}
					if (dim(CSEM_Data)[1L] == 0L) stop(paste('CSEM data for', tmp.ca.iter[g], 'Grade', perturb.var[g], 'is required to use SIMEX functionality, but is not available in SGPstateData.  Please contact package administrators to add CSEM data.'))

					CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
					tmp.data[, paste0("icsem", perturb.var[g], tmp.ca.iter[g], tmp.yr.iter[g]) := CSEM_Function(tmp.data[[num.perturb.vars-g]])]
				}
			}

      if (!is.null(csem.data.vnames)) {
        for (g in rev(seq_along(perturb.var))) {
          tmp.data[, paste0("icsem", perturb.var[g], tmp.ca.iter[g], tmp.yr.iter[g]) := Panel_Data[list(tmp.data$ID)][[rev(csem.data.vnames)[g]]]]
        }
      }

			if (verbose) messageSGP(c("\t\t", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " Order ", k, " Started simulation process ", prettyDate()))

			for (L in lambda[-1L]) {
        big.data <- createBigData(tmp.data, perturb.var, L, dependent.var.error)
				if (is.null(simex.use.my.coefficient.matrices) & !identical(sgp.labels[['my.extra.label']], "BASELINE")) {
          ifelse(dependent.var.error, knots_boundaries.iter <- tail(perturb.var, -1), knots_boundaries.iter <- perturb.var)
          for (g in seq_along(knots_boundaries.iter)) {
            ks <- big.data[, as.list(as.vector(unlist(round(quantile(big.data[[g+1L]], probs=knot.cut.percentiles, na.rm=TRUE), digits=3L))))] # Knots
            bs <- big.data[, as.list(as.vector(round(extendrange(big.data[[g+1L]], f=0.1), digits=3L)))] # Boundaries
            lh <- big.data[, as.list(as.vector(round(extendrange(big.data[[g+1L]], f=0.0), digits=3L)))] # LOSS/HOSS

            eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['knots_", rev(knots_boundaries.iter)[g],
  									"']] <- c(ks[,V1], ks[,V2], ks[,V3], ks[,V4])")))
            eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['boundaries_", rev(knots_boundaries.iter)[g],
  									"']] <- c(bs[,V1], bs[,V2])")))
            eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['Lambda_", L, "']][['loss.hoss_", rev(knots_boundaries.iter)[g],
  									"']] <- c(lh[,V1], lh[,V2])")))
          }
				}

				## Establish the simulation iterations - either 1) 1:B, or 2) a sample of either B or the number of previously computed matrices
				sim.iters <- seq.int(B)

				if (!is.null(tmp.par.config)) { # Not Sequential
          ## Write big.data to disk and remove from memory
          if (!exists('year.progression.for.norm.group')) year.progression.for.norm.group <- year.progression # Needed during Baseline Matrix construction
          tmp.dbname <- tempdir()
          sapply(sim.iters, function(z) data.table::fwrite(big.data[list(z)], file=file.path(tmp.dbname, paste0("simex_data_", z, ".csv")), showProgress = FALSE, verbose = FALSE))
				}

				if (!is.null(simex.use.my.coefficient.matrices)) { # Element from the 'calculate.simex' argument list.
					available.matrices <- unlist(getsplineMatrices(
						Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]][[
							paste("qrmatrices", tail(tmp.gp,1L), k, sep="_")]][[paste0("lambda_", L)]],
  						tail(content_area.progression, k+1L),
  						tail(grade.progression, k+1L),
  						tail(year.progression, k+1L),
  						tail(year_lags.progression, k),
  						my.exact.grade.progression.sequence=TRUE,
  						return.multiple.matrices=TRUE,
  						my.matrix.order=k,
  						my.matrix.time.dependency=SGPt), recursive=FALSE)

					if (length(available.matrices) > B) sim.iters <- sample.int(length(available.matrices), B) # Stays as 1:B when length(available.matrices) == B
					if (length(available.matrices) < B) sim.iters <- sample.int(length(available.matrices), B, replace=TRUE)
				}

				if (is.null(tmp.par.config)) { # Sequential
					if (verbose) messageSGP(c("\t\t\tStarted coefficient matrix calculation, Lambda ", L, ": ", prettyDate()))
					if (is.null(simex.use.my.coefficient.matrices)) {
						if (!is.null(simex.sample.size) && n.records > simex.sample.size) {
              foreach::registerDoSEQ()
              tmp.random <- foreach(z=iter(sim.iters)) %dorng% sample(seq.int(n.records), simex.sample.size)
						}
						for (z in seq_along(sim.iters)) {
							if (is.null(simex.sample.size) || n.records <= simex.sample.size) {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]][[z]] <-
									rq.mtx(tmp.gp.iter[seq.int(k)], lam=L, rqdata=big.data[list(z)][, b:=NULL])
							} else {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]][[z]] <-
									rq.mtx(tmp.gp.iter[seq.int(k)], lam=L, rqdata=big.data[list(z)][, b:=NULL][tmp.random[[z]]])
							}
						}
					} else simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]] <- available.matrices[sim.iters]

					# if (calculate.simex.sgps) { # Always calculate SIMEX SGPs (for ranked SIMEX table)
					if (verbose) messageSGP(c("\t\t\tStarted percentile prediction calculation, Lambda ", L, ": ", prettyDate()))
					for (z in seq_along(sim.iters)) {
						fitted[[paste0("order_", k)]][which(lambda==L),] <- fitted[[paste0("order_", k)]][which(lambda==L),] +
							c(.get.percentile.predictions(big.data[list(z)][, paste(c("ID", paste0('prior_', k:1L), "final_yr")), with=FALSE],
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]][[z]])/B)
					}
					# }
				} else {### Parallel over sim.iters
					###  Always use FOREACH for coefficient matrix production -- need %dorng% to guarantee reproducibility across plateforms (also MUCH more efficient with SNOW/Windows).
					if (toupper(tmp.par.config[["BACKEND"]]) != "FOREACH") tmp.par.config[["BACKEND"]] <- "FOREACH"; tmp.par.config[["TYPE"]] <- "doParallel"

					par.start <- startParallel(tmp.par.config, 'SIMEX')

					## Note, that if you use the parallel.config for SIMEX here, you can also use it for TAUS in the naive analysis
					## Example parallel.config argument: '... parallel.config=list(BACKEND="FOREACH", TYPE="doParallel", WORKERS=list(SIMEX = 4, TAUS = 4))'

					## Calculate coefficient matricies (if needed/requested)
					if (is.null(simex.use.my.coefficient.matrices)) {
						if (verbose) messageSGP(c("\t\t\tStarted coefficient matrix calculation, Lambda ", L, ": ", prettyDate()))
						if (is.null(simex.sample.size) || n.records <= simex.sample.size) {
							simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]] <-
								# foreach(z=iter(sim.iters), .packages=c("quantreg", "data.table"),
								foreach(z=iter(sim.iters),
									.export=c("Knots_Boundaries", "rq.method", "taus", "content_area.progression", "tmp.slot.gp", "year.progression", "year_lags.progression", "SGPt", "rq.sgp", "get.my.knots.boundaries.path"),
									.options.mpi=par.start$foreach.options, .options.multicore=par.start$foreach.options, .options.snow=par.start$foreach.options) %dopar% {
										rq.mtx(tmp.gp.iter[seq.int(k)], lam=L, rqdata=getSIMEXdata(tmp.dbname, z))
                                }
						} else {
							simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]] <-
								foreach(z=iter(sim.iters), .packages=c("quantreg", "data.table"),
									.export=c("Knots_Boundaries", "rq.method", "taus", "content_area.progression", "tmp.slot.gp", "year.progression", "year_lags.progression", "SGPt", "rq.sgp", "get.my.knots.boundaries.path"),
									.options.mpi=par.start$foreach.options, .options.multicore=par.start$foreach.options, .options.snow=par.start$foreach.options) %dorng% {
										rq.mtx(tmp.gp.iter[seq.int(k)], lam=L, rqdata=getSIMEXdata(tmp.dbname, z)[sample(seq.int(n.records), simex.sample.size)])
                                }
						}
					} else {
						simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]] <- available.matrices[sim.iters]

            ###   Re-set the random seed to match when coef matrices are produced.  Otherwise seed is off when data is simulated in subsequent L loops.
            if (is.null(simex.sample.size)) {
              ###   Use the N from the matrices rather than `simex.sample.size` - since that element may not be specified in the `calculate.simex` argument/list.
              simex.mtx.size <- unique(sapply(sim.iters, function(f) available.matrices[[f]]@Version[["Matrix_Information"]][["N"]]))
              if (all(n.records > simex.mtx.size)) tmp.random.reset <- foreach(z=iter(sim.iters)) %dorng% sample(seq.int(n.records), simex.mtx.size)
            }
					}

					if (!all(sapply(simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]], is.splineMatrix))) {
						recalc.index <- which(!sapply(simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]], is.splineMatrix))
						messageSGP(c("\n\t\t", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " Order ", k, " Coefficient Matrix process(es) ", recalc.index, "FAILED!  Attempting to recalculate sequentially..."))
						for (z in recalc.index) {
							if (is.null(simex.sample.size) || n.records <= simex.sample.size) {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]][[z]] <-
									rq.mtx(tmp.gp.iter[seq.int(k)], lam=L, rqdata=as.data.table(getSIMEXdata(tmp.dbname, z)))
							} else {
								simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]][[z]] <-
									rq.mtx(tmp.gp.iter[seq.int(k)], lam=L, rqdata=as.data.table(getSIMEXdata(tmp.dbname, z))[sample(seq.int(n.records), simex.sample.size),])
							}
						}
					}

					## get percentile predictions from coefficient matricies
					# if (calculate.simex.sgps) {
					if (verbose) messageSGP(c("\t\t\tStarted percentile prediction calculation, Lambda ", L, ": ", prettyDate()))
					mtx.subset <- simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste0("lambda_", L)]] # Save on memory copying to R SNOW workers
					environment(.get.percentile.predictions) <- environment(.smooth.bound.iso.row) <- environment()
					fitted[[paste0("order_", k)]][which(lambda==L),] <-
						foreach(z=iter(seq_along(sim.iters)), .combine="+", .export=c('tmp.gp', 'taus', 'sgp.loss.hoss.adjustment', 'isotonize', 'SGPt', 'get.my.knots.boundaries.path'),
							.options.multicore=par.start$foreach.options) %dopar% { # .options.snow=par.start$foreach.options
								c(.get.percentile.predictions(my.matrix=mtx.subset[[z]], my.data=getSIMEXdata(tmp.dbname, z, k, predictions=TRUE))/B)
							}
					# }
					stopParallel(tmp.par.config, par.start)
				} ### END Parallel over sim.iters
        if (!is.null(tmp.par.config)) unlink(tmp.dbname)
			} ### END for (L in lambda[-1L])
			if (verbose) messageSGP(c("\t\t", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " Order ", k, " Simulation process complete ", prettyDate()))

      # if (calculate.simex.sgps) { # Always calculate SIMEX SGPs (for ranked SIMEX table)
      switch(extrapolation,
        LINEAR = fit <- lm(fitted[[paste0("order_", k)]] ~ lambda),
        QUADRATIC = fit <- lm(fitted[[paste0("order_", k)]] ~ lambda + I(lambda^2)))

      extrap[[paste0("order_", k)]] <-
        matrix(.smooth.bound.iso.row(data.table(ID=seq.int(n.records), X=predict(fit, newdata=data.frame(lambda=-1L))[1L,]), isotonize, sgp.loss.hoss.adjustment),
               ncol=length(taus), byrow=TRUE)

      if (is.null(simex.use.my.coefficient.matrices)) {
        ranked.simex.quantile.values <- .get.quantiles(extrap[[paste0("order_", k)]], tmp.data[[tmp.num.variables]], ranked.simex=ifelse(reproduce.old.values, "reproduce.old.values", TRUE))
        # simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste("ranked_simex_table", tail(tmp.gp, 1L), k, sep="_")]] <- table(ranked.simex.quantile.values)
        # simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][[paste("n_records", tail(tmp.gp, 1L), k, sep="_")]] <- n.records
        ranked_simex_table <- table(ranked.simex.quantile.values)
        attr(ranked_simex_table, 'content_area_progression') <- tail(content_area.progression, k+1L)
        attr(ranked_simex_table, 'grade_progression') <- tail(grade.progression, k+1L)
        attr(ranked_simex_table, 'year_progression') <- tail(year.progression, k+1L)
        attr(ranked_simex_table, 'year_lags_progression') <- tail(year_lags.progression, k)
        attr(ranked_simex_table, 'n_records') <- n.records
        simex.coef.matrices[[paste("qrmatrices", tail(tmp.gp, 1L), k, sep="_")]][["ranked_simex_table"]][[1]] <- ranked_simex_table

        tmp.quantiles.simex[[k]] <- data.table(ID=tmp.data[["ID"]], SIMEX_ORDER=k,
            SGP_SIMEX=.get.quantiles(extrap[[paste0("order_", k)]], tmp.data[[tmp.num.variables]]),
            SGP_SIMEX_RANKED=as.integer(round(100*(data.table::frank(ties.method = "average", x = ranked.simex.quantile.values)/n.records), 0)))
      } else {
        if (any(grepl("ranked_simex_table", names(Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]][[paste("qrmatrices", tail(tmp.gp,1L), k, sep="_")]])))) {
        # if (length(Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]][[paste("qrmatrices", tail(tmp.gp,1L), k, sep="_")]]) > (length(lambda)-1)) {
          # ranked.simex.info <- Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]][[paste("qrmatrices", tail(tmp.gp,1L), k, sep="_")]][length(lambda):(length(lambda)+1)]
          ranked.simex.info <- get.simex.ranking.info(
            Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]][[paste("qrmatrices", tail(tmp.gp,1L), k, sep="_")]][["ranked_simex_table"]],
            tail(content_area.progression, k+1L), tail(grade.progression, k+1L), tail(year.progression, k+1L), tail(year_lags.progression, k))
          ranked.simex.tf <- TRUE
        } else {
          if (use.cohort.for.ranking) {
              ranked.simex.tf <- TRUE
          } else {
              messageSGP("\tRanked SIMEX SGP calculation with pre-calculated SGPs is only available with info embedded as of SGP version 1.9-4.0\n\tor setting parameter use.cohort.for.ranking = TRUE in the calculate.simex configuration.\n\tNAs will be returned for SGP_SIMEX_RANKED.")
              ranked.simex.tf <- FALSE
          }
        }
        if (ranked.simex.tf) {
          if (use.cohort.for.ranking) { # Use `use.cohort.for.ranking=TRUE` if reproducing values with original data OR to rank against the updated/new cohort data ONLY
            tmp.quantiles.simex[[k]] <- data.table(ID=tmp.data[["ID"]], SIMEX_ORDER=k,
            SGP_SIMEX = .get.quantiles(extrap[[paste0("order_", k)]], tmp.data[[tmp.num.variables]]),
            SGP_SIMEX_RANKED = as.integer(round(100*(data.table::frank(ties.method = "average", x =
            .get.quantiles(extrap[[paste0("order_", k)]], tmp.data[[tmp.num.variables]], ranked.simex=ifelse(reproduce.old.values, "reproduce.old.values", TRUE)))/n.records), 0)))
          } else { # creates a new "average" rank of the original data and the updated/new cohort
            tmp.quantiles.simex[[k]] <- data.table(ID=tmp.data[["ID"]], SIMEX_ORDER=k,
            SGP_SIMEX = .get.quantiles(extrap[[paste0("order_", k)]], tmp.data[[tmp.num.variables]]),
            SGP_SIMEX_RANKED = head(as.integer(round(100*(data.table::frank(ties.method = "average", x =
                c(.get.quantiles(extrap[[paste0("order_", k)]], tmp.data[[tmp.num.variables]], ranked.simex=ifelse(reproduce.old.values, "reproduce.old.values", TRUE)),
                as.numeric(rep(names(ranked.simex.info), ranked.simex.info))))/(n.records+attr(ranked.simex.info, "n_records"))), 0)), n.records))
          }
        }
      }
      # }
    }### END for (k in simex.matrix.priors)

		if (verbose) messageSGP(c("\tFinished SIMEX SGP calculation ", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " ", prettyDate()))

		if (is.null(save.matrices)) simex.coef.matrices <- NULL
		if (calculate.simex.sgps) {
			quantile.data.simex <- data.table(rbindlist(tmp.quantiles.simex), key=c("ID", "SIMEX_ORDER"))
      # invisible(quantile.data.simex[, SGP_SIMEX_RANKED := as.integer(round(100*(rank(SGP_SIMEX, ties.method = "average")/length(SGP_SIMEX)), 0)), by = "SIMEX_ORDER"])
      if (convert.0and100) {
        invisible(quantile.data.simex[SGP_SIMEX_RANKED==0L, SGP_SIMEX_RANKED := 1L])
        invisible(quantile.data.simex[SGP_SIMEX_RANKED==100L, SGP_SIMEX_RANKED := 99L])
      }
			setkey(quantile.data.simex, ID) # first key on ID and SIMEX_ORDER, then re-key on ID only to insure sorted order. Don't rely on rbindlist/k ordering...
		} else quantile.data.simex <- data.table("ID"=NA, "SIMEX_ORDER"=NA, "SGP_SIMEX"=NA, "SGP_SIMEX_RANKED"=NA) # set up empty data.table for ddcast and subsets below.
		if (print.other.gp) {
			tmp.quantile.data.simex <- ddcast(quantile.data.simex, ID ~ SIMEX_ORDER, value.var=setdiff(names(quantile.data.simex), c("ID", "SIMEX_ORDER")), sep="_ORDER_")
			quantile.data.simex <- data.table(tmp.quantile.data.simex,
        SGP_SIMEX=quantile.data.simex[c(which(!duplicated(quantile.data.simex, by=key(quantile.data.simex)))[-1L]-1L, dim(quantile.data.simex)[1L])][["SGP_SIMEX"]],
        SGP_SIMEX_RANKED=quantile.data.simex[c(which(!duplicated(quantile.data.simex, by=key(quantile.data.simex)))[-1L]-1L, dim(quantile.data.simex)[1L])][["SGP_SIMEX_RANKED"]])
			return(list(
				DT=quantile.data.simex,
				MATRICES=simex.coef.matrices))
		} else {
			if (print.sgp.order | return.norm.group.identifier) {
				return(list(
					DT=quantile.data.simex[c(which(!duplicated(quantile.data.simex, by=key(quantile.data.simex)))[-1L]-1L, dim(quantile.data.simex)[1L])],
					MATRICES=simex.coef.matrices))
			} else {
				return(list(
					DT=quantile.data.simex[c(which(!duplicated(quantile.data.simex, by=key(quantile.data.simex)))[-1L]-1L, dim(quantile.data.simex)[1L]), c("ID", "SGP_SIMEX", "SGP_SIMEX_RANKED"), with=FALSE],
					MATRICES=simex.coef.matrices))
			}
		}
		if (verbose) messageSGP(c("\tFinished SIMEX SGP calculation ", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " ", prettyDate(), "\n"))
	} ### END simex.sgp function

	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	ID <- tmp.messages <- ORDER <- SCALE_SCORE_PRIOR <- SGP <- PREFERENCE <- NULL

	if (missing(panel.data)) {
		stop("User must supply student achievement data for student growth percentile calculations. NOTE: data is now supplied to function using panel.data argument. See help page for details.")
	}
	if (!(is.matrix(panel.data) || is.list(panel.data))) {
		stop("Supplied panel.data not of a supported class. See help for details of supported classes")
	}
	if (inherits(panel.data, "list") && !"Panel_Data" %in% names(panel.data)) {
			stop("Supplied panel.data missing Panel_Data")
	}
	if (inherits(panel.data, "list") && !is.data.frame(panel.data[["Panel_Data"]])) {
			stop("Supplied panel.data$Panel_Data is not a data.frame or a data.table")
	}
	if (inherits(panel.data, "list") && !is.null(panel.data[['Coefficient_Matrices']])) {
		panel.data[['Coefficient_Matrices']] <- checksplineMatrix(panel.data[['Coefficient_Matrices']])
	}

	if (!missing(sgp.labels) && !is.list(sgp.labels)) {
			stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
	}
	if (!identical(names(sgp.labels), c("my.year", "my.subject")) &&
		!identical(names(sgp.labels), c("my.year", "my.subject", "my.extra.label"))) {
		stop("Please specify an appropriate list for sgp.labels. See help page for details.")
	}
	sgp.labels <- lapply(sgp.labels, toupper)
	tmp.path <- .create.path(sgp.labels)

	if (!missing(growth.levels)) {
		tmp.growth.levels <- list()
		if (!is.list(growth.levels) && !is.character(growth.levels)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: growth.levels must be supplied as a list or character abbreviation. See help page for details. studentGrowthPercentiles will be calculated without augmented growth.levels\n")
			tf.growth.levels <- FALSE
		}
		if (is.list(growth.levels)) {
			if (!identical(names(growth.levels), c("my.cuts", "my.levels"))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for growth.levels. See help page for details. Student growth percentiles will be calculated without augmented growth.levels\n")
				tf.growth.levels <- FALSE
			} else {
				tmp.growth.levels <- growth.levels
				tf.growth.levels <- TRUE
			}
		}
		if (is.character(growth.levels)) {
			if (is.null(SGP::SGPstateData[[growth.levels]][["Growth"]][["Levels"]])) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Growth Levels are currently not specified for the indicated state. \n\tPlease contact the SGP package administrator to have your state's data included in the package. Student growth percentiles will be calculated without augmented growth levels\n")
				tf.growth.levels <- FALSE
			} else {
				tmp.growth.levels[["my.cuts"]] <- SGP::SGPstateData[[growth.levels]][["Growth"]][["Cutscores"]][["Cuts"]]
				tmp.growth.levels[["my.levels"]] <- SGP::SGPstateData[[growth.levels]][["Growth"]][["Levels"]]
				tf.growth.levels <- TRUE
			}
		}
	} else {
		tf.growth.levels <- FALSE
	}

	if (!missing(use.my.knots.boundaries)) {
		if (!is.list(use.my.knots.boundaries) && !is.character(use.my.knots.boundaries)) {
			stop("use.my.knots.boundaries must be supplied as a list or character abbreviation. See help page for details.")
		}
		if (is.list(use.my.knots.boundaries)) {
			if (!inherits(panel.data, "list")) {
				stop("use.my.knots.boundaries is only appropriate when panel data is of class list. See help page for details.")
			}
			if (!identical(names(use.my.knots.boundaries), c("my.year", "my.subject")) &
				!identical(names(use.my.knots.boundaries), c("my.year", "my.subject", "my.extra.label"))) {
					stop("Please specify an appropriate list for use.my.knots.boundaries. See help page for details.")
			}
			tmp.path.knots.boundaries <- .create.path(use.my.knots.boundaries, pieces=c("my.subject", "my.year"))
			if (is.null(panel.data[["Knots_Boundaries"]]) | is.null(panel.data[["Knots_Boundaries"]][[tmp.path.knots.boundaries]])) {
				stop("Knots and Boundaries indicated by use.my.knots.boundaries are not included.")
			}
		}
		if (is.character(use.my.knots.boundaries)) {
			if (is.null(SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) {
				tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Knots and Boundaries are currently not implemented for the state indicated (",
				use.my.knots.boundaries, "). Knots and boundaries will be calculated from the data.", "
				Please contact the SGP package administrator to have your Knots and Boundaries included in the package\n"))
			}
			tmp.path.knots.boundaries <- .create.path(sgp.labels, pieces=c("my.subject", "my.year"))
		}
	} else {
		tmp.path.knots.boundaries <- .create.path(sgp.labels, pieces=c("my.subject", "my.year"))
	}

	if (!is.null(use.my.coefficient.matrices) && !identical(use.my.coefficient.matrices, TRUE)) {
		if (!inherits(panel.data, "list")) {
			stop("use.my.coefficient.matrices is only appropriate when panel data is of class list. See help page for details.")
		}
		if (!is.list(use.my.coefficient.matrices)) {
			stop("Please specify an appropriate list for argument 'use.my.coefficient.matrices'. See help page for details.")
		}
		if (!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject")) &&
			!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject", "my.extra.label"))) {
				stop("Please specify an appropriate list for argument 'use.my.coefficient.matrices'. See help page for details.")
		}
		tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices, pieces=c("my.subject", "my.year"))
		if (is.null(panel.data[["Coefficient_Matrices"]]) || is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
			stop("Coefficient matrices indicated by argument 'use.my.coefficient.matrices' are not included.")
		}
	} else {
		tmp.path.coefficient.matrices <- tmp.path
	}

	if (is.character(sgp.quantiles)) {
		sgp.quantiles <- toupper(sgp.quantiles)
		if (sgp.quantiles != "PERCENTILES") {
			stop("Character options for sgp.quantiles include only Percentiles at this time. Other options available by specifying a numeric quantity. See help page for details.")
		}
		taus <- .create_taus(sgp.quantiles)
		sgp.quantiles.labels <- NULL
	}
	if (is.numeric(sgp.quantiles)) {
		if (!(all(sgp.quantiles > 0 && sgp.quantiles < 1))) {
			stop("Specify sgp.quantiles as as a vector of probabilities between 0 and 1.")
		}
		taus <- .create_taus(sgp.quantiles)
		if (!is.null(sgp.quantiles.labels)) {
			if (length(sgp.quantiles.labels)!=length(sgp.quantiles)+1L) stop("Supplied 'sgp.quantiles.labels' must be 1 longer than supplied 'sgp.quantiles'.")
			if (any(is.na(as.integer(sgp.quantiles.labels)))) stop("Supplied 'sgp.quantiles.labels' must be integer values.")
			sgp.quantiles.labels <- as.integer(sgp.quantiles.labels)
		} else {
			sgp.quantiles.labels <- as.integer(c(100*taus, 100))
		}
	}
	if (!is.null(percentile.cuts)) {
		if (sgp.quantiles != "PERCENTILES") {
			stop("percentile.cuts only appropriate for growth percentiles. Set sgp.quantiles to Percentiles to produce requested percentile.cuts.")
		}
		if (!all(percentile.cuts %in% 0:100)) {
			stop("Specified percentile.cuts must be integers between 0 and 100.")
	}}
	if (!calculate.sgps && (is.character(goodness.of.fit) || goodness.of.fit==TRUE)) {
		tmp.messages <- c(tmp.messages, "\t\tNOTE: Goodness-of-Fit tables only produced when calculating SGPs.\n")
	}
	if (!is.null(calculate.confidence.intervals)) {
		csem.tf <- TRUE
		if (!is.character(calculate.confidence.intervals) && !is.list(calculate.confidence.intervals)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply an appropriate state acronym, variable or list containing details to calculate.confidence.intervals. See help page for details. SGPs will be calculated without confidence intervals.\n")
			csem.tf <- FALSE
		}
		if (is.list(calculate.confidence.intervals)) {
			if (!(("state" %in% names(calculate.confidence.intervals)) || ("variable" %in% names(calculate.confidence.intervals)))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for calculate.confidence.intervals including state/csem variable, confidence.quantiles, simulation.iterations, distribution and round. See help page for details. SGPs will be calculated without confidence intervals.\n")
				csem.tf <- FALSE
			}
			if ("variable" %in% names(calculate.confidence.intervals) && is.null(panel.data.vnames)) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: To utilize a supplied CSEM variable for confidence interval calculations you must specify the variables to be used for student growth percentile calculations with the panel.data.vnames argument. SGPs will be calculated without confidence intervals. See help page for details.\n")
                csem.tf <- FALSE
			}
		}
		if (is.character(calculate.confidence.intervals)) {
			if (!calculate.confidence.intervals %in% c(objects(SGP::SGPstateData), names(panel.data[['Panel_Data']]))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without confidence intervals.\n")
				csem.tf <- FALSE
			}
			if (calculate.confidence.intervals %in% objects(SGP::SGPstateData)) {
				if ("YEAR" %in% names(SGP::SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGP::SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\t\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. Simulated SGPs and confidence intervals will not be calculated.\n")
						csem.tf <- FALSE
					}
				}
                if (dim(SGP::SGPstateData[[calculate.confidence.intervals]][["Assessment_Program_Information"]][["CSEM"]][CONTENT_AREA==sgp.labels$my.subject & GRADE==rev(grade.progression)[1]])[1]==0) {
					tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '", sgp.labels$my.subject, "'. Simulated SGPs and confidence intervals will not be calculated.\n"))
					csem.tf <- FALSE
				}
				calculate.confidence.intervals <- list(state=calculate.confidence.intervals)
			}
            if (calculate.confidence.intervals %in% names(panel.data[['Panel_Data']])) {
                calculate.confidence.intervals <- list(variable=calculate.confidence.intervals)
            }
        }
		if (is.list(calculate.confidence.intervals) &&
            "variable" %in% names(calculate.confidence.intervals) &&
            calculate.confidence.intervals$variable %in% names(panel.data[['Panel_Data']]) &&
            all(is.na(panel.data[['Panel_Data']][[calculate.confidence.intervals$variable]]))) {
                tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: CSEM variable values in supplied panel data contain only missing values for requested content area '", sgp.labels$my.subject, "' and grade '", rev(grade.progression)[1], "'.\n\t\t\tSimulation based standard errors/confidences intervals for SGPs wil not be calculated.\n"))
                csem.tf <- FALSE
        }
		if (is.list(calculate.confidence.intervals) &&
            "state" %in% names(calculate.confidence.intervals) &&
            !"variable" %in% names(calculate.confidence.intervals) &&
            is.data.frame(SGP::SGPstateData[[calculate.confidence.intervals$state]][["Assessment_Program_Information"]][["CSEM"]]) &&
            dim(SGP::SGPstateData[[calculate.confidence.intervals$state]][["Assessment_Program_Information"]][["CSEM"]][CONTENT_AREA==sgp.labels$my.subject & GRADE==rev(grade.progression)[1]])[1]==0) {
                tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '", sgp.labels$my.subject, "'. Simulated SGPs and confidence intervals will not be calculated.\n"))
                csem.tf <- FALSE
		}
        if (is.list(calculate.confidence.intervals) &&
            "round.digits" %in% names(SGP::SGPstateData[[calculate.confidence.intervals$state]][["SGP_Configuration"]])) {
                calculate.confidence.intervals$round.digits <- SGP::SGPstateData[[calculate.confidence.intervals$state]][["SGP_Configuration"]][["round.digits"]]
        }
        if (is.list(calculate.confidence.intervals) &&
            "simulation.iterations" %in% names(SGP::SGPstateData[[calculate.confidence.intervals$state]][["SGP_Configuration"]])) {
                calculate.confidence.intervals$simulation.iterations <- SGP::SGPstateData[[calculate.confidence.intervals$state]][["SGP_Configuration"]][["simulation.iterations"]]
        }
		if (sgp.quantiles != "PERCENTILES") {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: When 'sgp.quantiles' is supplied and not equal to PERCENTILES, simulation based standard errors/confidences intervals for SGPs are not available.\n")
			csem.tf <- FALSE
		}
	} else {
		csem.tf <- FALSE
	}

	if (is.logical(calculate.simex) && !calculate.simex) calculate.simex <- NULL # check for calculate.simex=FALSE - same as calculate.simex=NULL
	if (!is.null(calculate.simex)) {
		simex.tf <- TRUE
		if (!is.character(calculate.simex) && !is.list(calculate.simex)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply an appropriate state acronym, variable or list containing details to calculate.simex. See help page for details. SGPs will be calculated without measurement error correction.\n")
			simex.tf <- FALSE
		}
		if (is.list(calculate.simex)) {
			if (!("state" %in% names(calculate.simex)) && !("variable" %in% names(calculate.simex)) & !("csem.data.vnames" %in% names(calculate.simex))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for calculate.simex including state/csem variable, simulation.iterations, lambda and extrapolation. See help page for details. SGPs will be calculated without measurement error correction.\n")
				simex.tf <- FALSE
			}
			if (all(c("state", "variable") %in% names(calculate.simex))) {
				stop("Please specify EITHER a state OR a CSEM variable for SGP measurement error correction. See help page for details.")
			}
			if (!is.null(calculate.simex$lambda)) {
				if (!is.numeric(calculate.simex$lambda)) {
					tmp.messages <- c(tmp.messages, "\t\tNOTE: Please supply numeric values to lambda. See help page for details. SGPs will be calculated without measurement error correction.\n")
					simex.tf <- FALSE
				}
				if (any(calculate.simex$lambda < 0)) {
					messageSGP("lambda should not contain negative values. Negative values will be ignored.")
					lambda <- calculate.simex$lambda[calculate.simex$lambda >= 0]
				} else lambda=calculate.simex$lambda
				if (is.null(panel.data.vnames) && !is.null(calculate.simex$csem.data.vnames)) stop("Use of csem.data.vnames in SIMEX requires panel.data.vnames be provided.")
			}
		}
		if (is.character(calculate.simex)) {
			if (!calculate.simex %in% c(objects(SGP::SGPstateData), names(panel.data))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please provide an appropriate state acronym or variable name in supplied data corresponding to CSEMs. See help page for details. SGPs will be calculated without measurement error correction.\n")
				simex.tf <- FALSE
			}
			if (calculate.simex %in% objects(SGP::SGPstateData)) {
				if ("YEAR" %in% names(SGP::SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]])) {
					if (!sgp.labels$my.year %in% unique(SGP::SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]][["YEAR"]])) {
						tmp.messages <- c(tmp.messages, "\t\tNOTE: SGPstateData contains year specific CSEMs but year requested is not available. SGPs will be calculated without measurement error correction.\n")
						simex.tf <- FALSE
					}
				}
				if (!sgp.labels$my.subject %in% unique(SGP::SGPstateData[[calculate.simex]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]])) {
					tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: SGPstateData does not contain content area CSEMs for requested content area '",
						sgp.labels$my.subject, "'. SGPs will be calculated without measurement error correction.\n"))
					simex.tf <- FALSE
				}
				calculate.simex <- list(state=calculate.simex)
			}
			if (calculate.simex %in% names(panel.data)) {
				calculate.simex <- list(variable=calculate.simex)
			}
		}
		if (is.null(calculate.simex$simulation.iterations)) calculate.simex$simulation.iterations <- 20L
		if (!is.null(calculate.simex$simex.sample.size) && !is.numeric(calculate.simex$simex.sample.size)) calculate.simex$simulation.sample.size <- NULL
		if (is.null(calculate.simex$lambda)) calculate.simex$lambda <- seq(0,2,0.5)
		if (is.null(calculate.simex$extrapolation)) {
			calculate.simex$extrapolation <- "LINEAR"
		} else {
			calculate.simex$extrapolation <- toupper(calculate.simex$extrapolation)
		}
		if (!any(calculate.simex$extrapolation == c("QUADRATIC", "LINEAR"))) {
			messageSGP("\t\tNOTE: Extrapolation not implemented. Using: linear")
			calculate.simex$extrapolation <- "LINEAR"
		}

	} else {
		simex.tf <- FALSE
	}

	if (!is.null(additional.vnames.to.return)) {
		if (!all(names(additional.vnames.to.return) %in% names(panel.data[["Panel_Data"]]))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'additional.vnames.to.return' are not all contained in supplied panel.data. No additional variables will be returned.\n")
			additional.vnames.to.return <- NULL
		}
	}

	if (is.null(sgp.cohort.size)) sgp.cohort.size <- 0L

	if (is.null(goodness.of.fit.minimum.n)) goodness.of.fit.minimum.n <- 250L

	if (!is.null(sgp.percentiles.set.seed)) set.seed(as.integer(sgp.percentiles.set.seed))

	if (!is.null(SGPt)) {
		if (identical(SGPt, TRUE)) SGPt <- list(TIME="TIME", TIME_LAG="TIME_LAG")
		if (is.list(SGPt) && !all(c("TIME", "TIME_LAG") %in% names(SGPt))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: 'TIME' and 'TIME_LAG' are not contained in list supplied to 'SGPt' argument. SGPt is set to NULL")
			SGPt <- NULL
		} else {
			if (!((all(unlist(SGPt) %in% names(panel.data))) | (all(unlist(SGPt) %in% names(panel.data$Panel_Data))))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(unlist(SGPt), collapse=", "), "are not all contained in the supplied 'panel.data'. 'SGPt' is set to NULL.\n")
				SGPt <- NULL
			}
		}
	}

	if (is.null(SGPt) && !is.null(return.norm.group.dates)) {
		return.norm.group.dates <- NULL
	}

    if (!is.null(SGPt.max.time) && is.null(SGPt)) {
        SGPt.max.time <- NULL
    }

	if (identical(return.norm.group.dates, TRUE)) {
		return.norm.group.dates <- "TIME[!_]"
	}

	if (identical(return.norm.group.scale.scores, FALSE)) {
		return.norm.group.scale.scores <- NULL
	}

    if (!is.null(sgp.less.than.sgp.cohort.size.return) && is.null(sgp.cohort.size)) {
        stop("NOTE: When 'sgp.less.than.sgp.cohort.size.return' is not NULL, 'sgp.cohort.size' must also be provided.")
    }

    if (!is.null(sgp.less.than.sgp.cohort.size.return)) {
        if (identical(sgp.less.than.sgp.cohort.size.return, TRUE)) sgp.less.than.sgp.cohort.size.return <- paste("Less than", sgp.cohort.size, "Students in Cohort. No SGP Calculated")
        sgp.less.than.sgp.cohort.size.return <- as.character(sgp.less.than.sgp.cohort.size.return)
    }

    sgp.message.label <- sgp.labels[['my.extra.label']]
    if (!is.null(calculate.simex)) sgp.message.label <- paste("SIMEX", sgp.message.label)


	### Create object to store the studentGrowthPercentiles objects

	tmp.objects <- c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "Panel_Data", "SGPercentiles", "SGProjections", "Simulated_SGPs")
	Coefficient_Matrices <- Cutscores <- Goodness_of_Fit <- Knots_Boundaries <- Panel_Data <- SGPercentiles <- SGProjections <- Simulated_SGPs <- SGP_STANDARD_ERROR <- Verbose_Messages <- NULL
	SGP_SIMEX <- SGP_SIMEX_RANKED <- SGP_NORM_GROUP_SCALE_SCORES <- SGP_NORM_GROUP_DATES <- SGP_NORM_GROUP <- NULL

	if (inherits(panel.data, "list")) {
		for (i in tmp.objects) {
			if (!is.null(panel.data[[i]])) {
				assign(i, panel.data[[i]])
			}
		}

		## Check class and construction of coefficient matrices

		if (!is.null(panel.data[['Coefficient_Matrices']])) {
			tmp.matrices <- Coefficient_Matrices; tmp.changes <- FALSE
			for (i in names(tmp.matrices)) {
				splineMatrix.tf <- sapply(tmp.matrices[[i]], validObject, test=TRUE)==TRUE
				if (!any(splineMatrix.tf)) {
					tmp.changes <- TRUE
					tmp.content_area <- unlist(strsplit(i, "[.]"))[1L]; tmp.year <- unlist(strsplit(i, "[.]"))[2L]
					for (j in names(tmp.matrices[[i]])[!splineMatrix.tf]) {
						messageSGP(paste("\t\tUpdating Existing Coefficient Matrix", i, j, "to new splineMatrix class."))
						tmp.matrices[[i]][[j]] <- as.splineMatrix(matrix_argument=tmp.matrices[[i]][[j]],
							matrix_argument_name=j, content_area=tmp.content_area, year=tmp.year, sgp_object=panel.data)
					}
				}
			}
			if (tmp.changes) {
				Coefficient_Matrices <- tmp.matrices
			}
		}
	} ### if (identical(class(panel.data), "list"))


	### Create Panel_Data based upon class of input data

	if (is.matrix(panel.data)) {
		Panel_Data <- panel.data <- as.data.table(panel.data)
	}
	if (is.data.frame(panel.data)) {
		Panel_Data <- as.data.table(panel.data)
	}
	if (inherits(panel.data, "list") && !is.data.table(panel.data[["Panel_Data"]])) {
        Panel_Data <- as.data.table(panel.data[["Panel_Data"]])
	}

	### Create ss.data from Panel_Data

	if (dim(Panel_Data)[1L]==0L || dim(Panel_Data)[2L]<3L) {
		tmp.messages <- paste0("\t\tNOTE: Supplied data together with grade progression contains no data (dim = ", paste(dim(Panel_Data), collapse=", "), "). Check data, function arguments and see help page for details.\n")
		messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
			paste(grade.progression, collapse=", "), " ", sgp.message.label))
		messageSGP(paste(tmp.messages, "\tFinished SGP Student Growth Percentile Analysis", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))

		return(
			list(Coefficient_Matrices=Coefficient_Matrices,
				Cutscores=Cutscores,
				Goodness_of_Fit=Goodness_of_Fit,
				Knots_Boundaries=Knots_Boundaries,
				Panel_Data=Panel_Data,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}

	if (!is.null(SGPt)) {
		setnames(Panel_Data, unlist(SGPt), c("TIME", "TIME_LAG"))
	}

	if (!is.null(panel.data.vnames)) {
		if (!all(panel.data.vnames %in% names(Panel_Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'panel.data.vnames' are not all in the supplied Panel_Data.\n\t\t\tAnalyses will continue with the variables contained in both Panel_Data and the 'panel.data.vnames' argument.\n")
		}
		ss.data <- Panel_Data[,intersect(panel.data.vnames, names(Panel_Data)), with=FALSE]
	} else {
		ss.data <- Panel_Data
	}

	if (dim(ss.data)[2L] %% 2 != 1L) {
		stop(paste("Number of columns of supplied panel data (", dim(ss.data)[2L], ") does not conform to data requirements. See help page for details."))
	}

	num.panels <- (dim(ss.data)[2L]-1L)/2L

	### Rename variables in ss.data based upon grade progression

	if (!missing(grade.progression)) {
		tmp.gp <- grade.progression
		by.grade <- TRUE

		if (length(tmp.gp[!is.na(tmp.gp)]) > num.panels) {
			tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Supplied 'grade progression', grade.progression=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.\n\t\t\tAnalyses will utilize maximum number of priors supplied by the data.\n"))
		tmp.gp <- tail(grade.progression, num.panels)
	}}
	if (!missing(subset.grade) && missing(grade.progression)) {
		tmp.gp <- (subset.grade-num.panels+1L):subset.grade
		by.grade <- TRUE
	}
	if (missing(subset.grade) && missing(grade.progression)) {
		tmp.gp <- seq.int(num.panels)
		by.grade <- FALSE
	}
	if (!missing(num.prior) && !exact.grade.progression.sequence) {
		if (length(num.prior) > 1 || !((num.prior-round(num.prior)) < .Machine$double.eps^0.5) || num.prior <= 0L) {
			stop("Specified num.prior not positive integer(s)")
		}
		if (num.prior > length(tmp.gp[!is.na(tmp.gp)])-1L) {
			tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Specified argument num.prior (", num.prior, ") exceeds number of panels of data supplied.\n\t\t\tAnalyses will utilize maximum number of priors possible.\n"))
			num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1L
		} else {
			tmp.gp <- grade.progression <- tail(tmp.gp[!is.na(tmp.gp)], num.prior+1L)
			if (!is.null(content_area.progression) && length(content_area.progression > num.prior+1L)) content_area.progression <- tail(content_area.progression, num.prior+1L)

	}} else {
		num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1L
	}

	if (exact.grade.progression.sequence) {
		tmp.gp <- grade.progression
		by.grade <- TRUE
		num.prior <- length(tmp.gp[!is.na(tmp.gp)])-1L
	}

	if (!is.null(max.order.for.percentile)) {
		tmp.gp <- tail(tmp.gp, max.order.for.percentile+1L)
		num.prior <- min(num.prior, max.order.for.percentile)
		if (!is.null(content_area.progression)) content_area.progression <- tail(content_area.progression, length(tmp.gp))
		if (!is.null(year.progression)) year.progression <- year.progression.for.norm.group <- tail(year.progression, length(tmp.gp))
	}

	if (is.character(tmp.gp)) {
		tmp.slot.gp <- tmp.gp
		tmp.gp <- tmp.gp[!is.na(tmp.gp)]
	} else {
		tmp.slot.gp <- grade.progression
	}

	if (is.numeric(tmp.gp) & drop.nonsequential.grade.progression.variables && any(diff(tmp.gp) > 1)) {
		ss.data <- ss.data[,c(1L, (num.panels+1L)-rev(c(1L, cumsum(rev(diff(tmp.gp)))+1L)-1L), (2*num.panels+1L)-rev(c(1L, cumsum(rev(diff(tmp.gp)))+1L)-1L)), with=FALSE]
		num.panels <- (dim(ss.data)[2L]-1L)/2L
	}

	## Run this check before the setup of ss.data - otherwise function chokes on negative subscripts
	if (exact.grade.progression.sequence && num.prior > num.panels) {
		tmp.messages <- paste("\t\tNOTE: Supplied data together with EXACT grade progression contains fewer panel years than required. \n\t\t
			Check data, function arguments and see help page for details.\n")
		messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
			paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label))
		messageSGP(paste(tmp.messages, "\tStudent Growth Percentile Analysis NOT RUN", prettyDate(), "\n"))

		return(
			list(Coefficient_Matrices=Coefficient_Matrices,
				Cutscores=Cutscores,
				Goodness_of_Fit=Goodness_of_Fit,
				Knots_Boundaries=Knots_Boundaries,
				Panel_Data=Panel_Data,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}

	### Create ss.data

	tmp.last <- tail(tmp.gp, 1L)
	ss.data <- data.table(ss.data[,c(1L, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels)), with=FALSE], key=names(ss.data)[1L])
	num.panels <- (dim(ss.data)[2L]-1L)/2L
	if (is.factor(ss.data[[1L]])) ss.data[[1L]] <- as.character(ss.data[[1L]])
	if (exact.grade.progression.sequence) tmp.num.prior <- num.prior else tmp.num.prior <- 1L

	if (!is.null(sgp.test.cohort.size)) {
		cohort.ids <- .get.panel.data(ss.data, num.prior, by.grade, tmp.gp)[[1L]]
		max.cohort.size <- min(length(cohort.ids), as.numeric(sgp.test.cohort.size))
    if (any(grepl("_DUPS_[0-9]*", ss.data$ID))) {
      dup.ids <- grep("_DUPS_[0-9]*", ss.data$ID, value=TRUE)
    } else dup.ids <- NULL
		ss.data <- ss.data[ss.data[[1L]] %in% unique(c(sample(cohort.ids, max.cohort.size), dup.ids))]
	} else max.cohort.size <- dim(.get.panel.data(ss.data, tmp.num.prior, by.grade, tmp.gp))[1L]

	if (max.cohort.size == 0L) {
		tmp.messages <- "\t\tNOTE: Supplied data together with grade progression contains no data. Check data, function arguments and see help page for details.\n"
		messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
			paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label))
		messageSGP(paste(tmp.messages, "\tFinished SGP Student Growth Percentile Analysis", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))

		return(
			list(Coefficient_Matrices=Coefficient_Matrices,
				Cutscores=Cutscores,
				Goodness_of_Fit=Goodness_of_Fit,
				Knots_Boundaries=Knots_Boundaries,
				Panel_Data=Panel_Data,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}

	if (is.null(sgp.less.than.sgp.cohort.size.return) && max.cohort.size < sgp.cohort.size) {
		tmp.messages <- paste("\t\tNOTE: Supplied data together with grade progression contains fewer than the minimum cohort size.\n\t\tOnly", max.cohort.size,
			"valid cases provided with", sgp.cohort.size, "indicated as minimum cohort N size. Check data, function arguments and see help page for details.\n")
		messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
			paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label))
		messageSGP(paste(tmp.messages, "\tStudent Growth Percentile Analysis NOT RUN", prettyDate(), "\n"))

		return(
			list(Coefficient_Matrices=Coefficient_Matrices,
				Cutscores=Cutscores,
				Goodness_of_Fit=Goodness_of_Fit,
				Knots_Boundaries=Knots_Boundaries,
				Panel_Data=Panel_Data,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}


	### PROGRESSION variable creation:

	grade.progression <- grade.progression.for.norm.group <- tmp.gp
	if (is.null(content_area.progression)) {
		content_area.progression <- rep(sgp.labels$my.subject, length(tmp.gp))
	} else {
		if (!inherits(content_area.progression, "character")) {
			stop("The 'content_area.progression' vector/argument should be a character vector. See help page for details.")
		}
		if (!identical(tail(content_area.progression, 1L), sgp.labels[['my.subject']])) {
			stop("The last element in the 'content_area.progression' vector/argument must be identical to 'my.subject' of the sgp.labels. See help page for details.")
		}
		if (length(content_area.progression) != length(tmp.gp)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: The 'content_area.progression' vector/argument does not have the same number of elements as the 'grade.progression' vector/argument.\n\t\t\t'content_area.progression' will be trimmed based upon the length of 'grade.progression'.\n")
			content_area.progression <- tail(content_area.progression, length(tmp.gp))
		}
	}

	if (is.null(year.progression) && is.null(year_lags.progression)) {
		if (is.character(type.convert(as.character(grade.progression), as.is=TRUE))) {
			stop("\tNOTE: Non-numeric grade progressions must be accompanied by arguments 'year.progression' and 'year_lags.progression'")
		} else {
			year.progression <- year.progression.for.norm.group <-
				tail(rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(diff(type.convert(as.character(grade.progression), as.is=FALSE))))))), length(tmp.gp))
		}
	}

	if (is.null(year.progression) && !is.null(year_lags.progression)) {
		if (!identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- year.progression.for.norm.group <- tail(rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year_lags.progression))))), length(tmp.gp))
		}
		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) {
			year.progression <- rep("BASELINE", length(tmp.gp))
			year.progression.for.norm.group <- tail(rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year_lags.progression))))), length(tmp.gp))
		}
		if (!inherits(year.progression, "character")) {
			stop("year.area.progression should be a character vector. See help page for details.")
		}
		if (!identical(sgp.labels[['my.extra.label']], "BASELINE") && !identical(tail(year.progression, 1L), sgp.labels[['my.year']])) {
			stop("The last element in the year.progression must be identical to 'my.year' of the sgp.labels. See help page for details.")
		}
		if (length(year.progression) != length(tmp.gp)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: The year.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (!is.null(year.progression) && is.null(year_lags.progression)) {
		year.progression <- tail(year.progression, length(tmp.gp))
		if (year.progression[1L] == "BASELINE") {
			year_lags.progression <- rep(1L, length(year.progression)-1L)
			year.progression.for.norm.group <- year.progression
		} else {
			year_lags.progression <- diff(as.numeric(sapply(strsplit(year.progression, '_'), '[', split.location(year.progression))))
			year.progression.for.norm.group <- year.progression
		}
	}

    if (!is.null(sgp.less.than.sgp.cohort.size.return) && max.cohort.size < sgp.cohort.size) {
        quantile.data <- data.table(ID=.get.panel.data(ss.data, tmp.num.prior, by.grade, tmp.gp)[[1L]], SGP=as.integer(NA), SGP_NOTE=sgp.less.than.sgp.cohort.size.return, GRADE = as.character(tail(grade.progression, 1)))
        if (!is.null(additional.vnames.to.return)) {
          quantile.data <- panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return)), with=FALSE][quantile.data, on="ID"]
          setnames(quantile.data, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
        }

        if (return.norm.group.identifier) quantile.data[,SGP_NORM_GROUP:=as.factor(paste(tail(paste(year.progression, paste(content_area.progression, grade.progression, sep="_"), sep="/"), tmp.num.prior+1L), collapse="; "))]
        if (!is.null(return.norm.group.preference)) quantile.data[, PREFERENCE := return.norm.group.preference]
        if (identical(sgp.labels[['my.extra.label']], "BASELINE")) setnames(quantile.data, "SGP", "SGP_BASELINE")
        if (identical(sgp.labels[['my.extra.label']], "BASELINE") & "SGP_NORM_GROUP" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE", names(quantile.data)))
        if (identical(sgp.labels[['my.extra.label']], "EQUATED")) setnames(quantile.data, "SGP", "SGP_EQUATED")
        if (identical(sgp.labels[['my.extra.label']], "EQUATED") & "SGP_NORM_GROUP" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_EQUATED", names(quantile.data)))

        SGPercentiles[[tmp.path]] <- rbindlist(list(quantile.data, SGPercentiles[[tmp.path]]), fill=TRUE)

        tmp.messages <- paste("\t\tNOTE: Supplied data together with grade progression contains fewer than the minimum cohort size.\n\t\tOnly", max.cohort.size,
          "valid cases provided with", sgp.cohort.size, "indicated as minimum cohort N size. Check data, function arguments and see help page for details.\n")

        if (print.time.taken) {
            if (calculate.sgps) cohort.n <- format(dim(quantile.data)[1L], big.mark=",") else cohort.n <- format(max.cohort.size, big.mark=",")
            messageSGP(paste("\tStarted studentGrowthPercentiles:", started.date))
                messageSGP(paste0("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
                    paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label, " (N=", cohort.n, ")"))
            if (verbose.output) messageSGP(Verbose_Messages)
            messageSGP(c(tmp.messages, "\tFinished studentGrowthPercentiles: ", prettyDate(), " in ", convertTime(timetakenSGP(started.at)), "\n"))
        }

        return(
            list(Coefficient_Matrices=Coefficient_Matrices,
                Cutscores=Cutscores,
                Goodness_of_Fit=Goodness_of_Fit,
                Knots_Boundaries=Knots_Boundaries,
                Panel_Data = if (return.panel.data) Panel_Data else NULL,
                SGPercentiles=SGPercentiles,
                SGProjections=SGProjections,
                Simulated_SGPs=Simulated_SGPs))
    }


	### Create Knots and Boundaries if requested (uses only grades in tmp.gp)

	if (missing(use.my.knots.boundaries)) {
		tmp.knots <- c(Knots_Boundaries[[tmp.path.knots.boundaries]], .get.knots.boundaries(ss.data, by.grade))
		Knots_Boundaries[[tmp.path.knots.boundaries]] <- tmp.knots[!duplicated(names(tmp.knots))]
	} else {
		if (is.character(use.my.knots.boundaries)) {
			if (!is.null(SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) {
				for (h in unique(content_area.progression)) {
					for (i in grep(h, names(SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
						Knots_Boundaries[[tmp.path.knots.boundaries]][[i]] <- SGP::SGPstateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]][[i]]
					}
				}
			}
		}
	}


	### QR Calculations: coefficient matrices are saved/read into/from panel.data[["Coefficient_Matrices"]]

	if (is.null(use.my.coefficient.matrices)) {
		if (exact.grade.progression.sequence) {
			coefficient.matrix.priors <- num.prior
		} else {
			coefficient.matrix.priors <- seq(num.prior)
		}
		for (k in coefficient.matrix.priors) {
			Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']] <- .create.coefficient.matrices(ss.data, k, by.grade, max.n.for.coefficient.matrices)
			if (identical(Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']], "Insufficient N")) {
				tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Some grade progressions contain fewer than the minimum cohort size.",
					"\n\t\tOnly analyses with MAX grade progression ", paste(rev(rev(tmp.gp)[seq.int(k)]), collapse = ', '), " will be produced given N=", prettyNum(sgp.cohort.size, big.mark=",", scientific=FALSE),
					" indicated as minimum cohort size. \n\t\tCheck data, function arguments and see help page for details.\n"))
				Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']] <- NULL
				grade.progression <- tmp.gp <- rev(rev(tmp.gp)[seq.int(k)])
        if (!is.null(year.progression) && length(year.progression) > length(grade.progression)) year.progression <- tail(year.progression, length(grade.progression))
        if (!is.null(year_lags.progression) && length(year_lags.progression) > length(grade.progression)-1L) year_lags.progression <- tail(year_lags.progression, length(grade.progression)-1L)
        if (!is.null(content_area.progression) && length(content_area.progression) > length(grade.progression)) content_area.progression <- tail(content_area.progression, length(grade.progression))
        if (!is.null(grade.progression.for.norm.group) && length(grade.progression.for.norm.group) > length(grade.progression)) grade.progression.for.norm.group <- tail(grade.progression.for.norm.group, length(grade.progression))
        if (!is.null(year.progression.for.norm.group) && length(year.progression.for.norm.group) > length(grade.progression)) year.progression.for.norm.group <- tail(year.progression.for.norm.group, length(grade.progression))
        coefficient.matrix.priors <- setdiff(coefficient.matrix.priors, k)
				break
			}
			if (identical(names(Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']]), "RQ_ERROR")) {
				tmp.err.message <- Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']][['RQ_ERROR']]
				tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: An Error in the quantile regression (coefficient matrix creation) has occurred",
					"\n\t\tin grade progression ", paste(rev(rev(tmp.gp)), collapse = ', '), " and produced the following error message: \n\t\t\t\"", tmp.err.message, "\""))
				messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
				messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
					paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label))
				messageSGP(paste(tmp.messages, "\n\t\tStudent Growth Percentile Analysis NOT RUN", prettyDate(), "\n"))
				Coefficient_Matrices[[tmp.path.coefficient.matrices]][['TMP_NAME']] <- NULL

				return(
					list(Coefficient_Matrices=Coefficient_Matrices,
						Cutscores=Cutscores,
						Goodness_of_Fit=Goodness_of_Fit,
						Knots_Boundaries=Knots_Boundaries,
						Panel_Data=Panel_Data,
						SGPercentiles=SGPercentiles,
						SGProjections=SGProjections,
						Simulated_SGPs=Simulated_SGPs))
			}

			names(Coefficient_Matrices[[tmp.path.coefficient.matrices]])[length(Coefficient_Matrices[[tmp.path.coefficient.matrices]])] <- get.coefficient.matrix.name(tmp.last, k)

			if (verbose.output) {
				tmp.coefficient.matrix.name <- get.coefficient.matrix.name(tmp.last, k)
				tmp.grade.names <- paste("Grade",
					rev(head(unlist(Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Grade_Progression), -1L)))
				Verbose_Messages <- paste0("\t\tNOTE: Coefficient Matrix ", tmp.coefficient.matrix.name, " created using:")
				for (l in seq_along(tmp.grade.names)) {
					tmp.knots <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Knots[l])
					tmp.boundaries <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name]]@Boundaries[l])
					Verbose_Messages <- c(Verbose_Messages, paste0("\n\t\t\tKnots: ", tmp.knots, " and Boundaries: ", tmp.boundaries, "."))
				}
			}
		}
	}


	### Calculate SIMEX corrected coefficient matrices and percentiles (if requested)

	if (simex.tf) {
		quantile.data.simex <- simex.sgp(
			state=calculate.simex$state,
			csem.data.vnames=calculate.simex$csem.data.vnames,
			lambda=calculate.simex$lambda,
			B=calculate.simex$simulation.iterations,
			simex.sample.size=calculate.simex$simex.sample.size,
			extrapolation=calculate.simex$extrapolation,
			save.matrices=calculate.simex$save.matrices,
			simex.use.my.coefficient.matrices=calculate.simex$simex.use.my.coefficient.matrices,
			calculate.simex.sgps=calculate.sgps,
			dependent.var.error=calculate.simex$dependent.var.error,
            use.cohort.for.ranking=calculate.simex$use.cohort.for.ranking,
			reproduce.old.values=calculate.simex$reproduce.old.values,
			verbose=calculate.simex$verbose)

		if (!is.null(quantile.data.simex[['MATRICES']])) {
			tmp_sgp_1 <- list(Coefficient_Matrices = list(TMP_SIMEX=Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]]))
			tmp_sgp_2 <- list(Coefficient_Matrices = list(TMP_SIMEX=quantile.data.simex[['MATRICES']]))
			tmp_sgp_combined <- mergeSGP(tmp_sgp_1, tmp_sgp_2)
			Coefficient_Matrices[[paste0(tmp.path.coefficient.matrices, '.SIMEX')]] <- tmp_sgp_combined[["Coefficient_Matrices"]][["TMP_SIMEX"]]
		}
	}


	### Calculate growth percentiles (if requested), percentile cuts (if requested), and simulated confidence intervals (if requested)

	if (calculate.sgps) {

		tmp.matrices <- getsplineMatrices(
					Coefficient_Matrices[[tmp.path.coefficient.matrices]],
					content_area.progression,
					grade.progression,
					year.progression,
					year_lags.progression,
					exact.grade.progression.sequence,
					my.matrix.time.dependency=SGPt)

		tmp.orders <- sapply(tmp.matrices, function(x) length(x@Grade_Progression[[1L]])-1L)
		max.order <- max(tmp.orders)

		if (max.order < num.prior) {
			tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Requested number of prior scores (num.prior=", num.prior, ") exceeds maximum matrix order (max.order=",
			max.order, "). Only matrices of order up to max.order=", max.order, " will be used.\n"))
		}
		if (max.order > num.prior) {
			tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Maximum coefficient matrix order (max.order=", max.order, ") exceeds that of specified number of priors,
				(num.prior=", num.prior, "). Only matrices of order up to num.prior=", num.prior, " will be used.\n"))
			tmp.matrices <- tmp.matrices[tmp.orders <= max.order]
		}

		tmp.quantiles <- tmp.percentile.cuts <- tmp.csem.quantiles <- list()

		for (j in seq_along(tmp.orders)) {
			tmp.data <- .get.panel.data(ss.data, tmp.orders[j], by.grade, tmp.gp)
			if (dim(tmp.data)[1L] > 0L) {
                tmp.matrix <- tmp.matrices[[j]]
				tmp.predictions <- .get.percentile.predictions(tmp.data, tmp.matrix)
				tmp.quantiles[[j]] <- data.table(ID=tmp.data[[1L]], ORDER=tmp.orders[j], SGP=.get.quantiles(tmp.predictions, tmp.data[[dim(tmp.data)[2L]]]))
				if (csem.tf) {
					if (is.null(calculate.confidence.intervals[['simulation.iterations']])) calculate.confidence.intervals[['simulation.iterations']] <- 100L
					if (!is.null(calculate.confidence.intervals[['variable']])) {
						tmp.csem.variable <- Panel_Data[Panel_Data[[1L]] %in% ss.data[list(tmp.data[[1L]])][[1L]]][[calculate.confidence.intervals[['variable']]]]
					} else {
						tmp.csem.variable <- NULL
					}
					if (!is.null(additional.vnames.to.return)) {
						tmp.id.etc <- panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return)), with=FALSE][tmp.data[, names(tmp.data)[1L], with=FALSE]]
						setnames(tmp.id.etc, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
					}	else tmp.id.etc <- tmp.data[, names(tmp.data)[1L], with=FALSE]

					tmp.csem.quantiles[[j]] <- data.table(
									tmp.id.etc,
									matrix(replicate(calculate.confidence.intervals[['simulation.iterations']],
												.get.quantiles(
													tmp.predictions,
													csemScoreSimulator(
													scale_scores=tmp.data[[dim(tmp.data)[2L]]],
													grade=tmp.last,
													content_area=sgp.labels[['my.subject']],
													year=sgp.labels[['my.year']],
													state=calculate.confidence.intervals[['state']],
													variable=tmp.csem.variable,
													distribution=calculate.confidence.intervals[['distribution']],
													round.digits=calculate.confidence.intervals[['round.digits']]))),
                                        ncol=calculate.confidence.intervals[['simulation.iterations']]))
					setnames(tmp.csem.quantiles[[j]], paste0("V", seq(calculate.confidence.intervals[['simulation.iterations']])),
										paste("SGP_SIM", seq(calculate.confidence.intervals[['simulation.iterations']]), sep="_"))
				} ## END CSEM analysis

                if (!is.null(percentile.cuts)) {
                    tmp.percentile.cuts[[paste("ORDER", j, sep="_")]] <- data.table(ID=tmp.data[[1L]], .get.percentile.cuts(tmp.predictions))
                    if (!is.null(SGPt.max.time)) tmp.percentile.cuts[[paste("ORDER", j, "MAX_TIME", sep="_")]] <- data.table(ID=tmp.data[[1L]], .get.percentile.cuts(.get.percentile.predictions(tmp.data, tmp.matrix, SGPt.max.time)))
                }
                if ((is.character(goodness.of.fit) || goodness.of.fit==TRUE || return.prior.scale.score) & j==1L) prior.ss <- tmp.data[[dim(tmp.data)[2L]-1L]]
                if (exact.grade.progression.sequence && return.prior.scale.score) prior.ss <- tmp.data[[dim(tmp.data)[2L]-1L]]
			} ### END if (dim(tmp.data)[1L] > 0)
		} ## END j loop

		quantile.data <- data.table(rbindlist(tmp.quantiles), key="ID")

		if (print.other.gp) {
			quantile.data <- data.table(ddcast(quantile.data, ID ~ ORDER, value.var=setdiff(names(quantile.data), c("ID", "ORDER"))),
				SGP=quantile.data[c(which(!duplicated(quantile.data, by=key(quantile.data)))[-1L]-1L, dim(quantile.data)[1L])][["SGP"]],
				ORDER=as.integer(quantile.data[c(which(!duplicated(quantile.data, by=key(quantile.data)))[-1L]-1L, dim(quantile.data)[1L])][["ORDER"]]))
			setnames(quantile.data, setdiff(names(quantile.data), c("ID", "SGP", "ORDER")), paste("SGP_ORDER", setdiff(names(quantile.data), c("ID", "SGP", "ORDER")), sep="_"))
		} else {
			if (print.sgp.order || return.norm.group.identifier) {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data, by=key(quantile.data)))[-1L]-1L, dim(quantile.data)[1L])]
			} else {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data, by=key(quantile.data)))[-1L]-1L, dim(quantile.data)[1L]), c("ID", "SGP"), with=FALSE]
			}
		}

        if (!is.null(return.additional.max.order.sgp)) {
            if (return.additional.max.order.sgp >=  max(tmp.orders)) {
                quantile.data[,paste("SGP_FROM", paste(as.numeric(unlist(strsplit(sgp.labels[['my.year']], "_")))-return.additional.max.order.sgp, collapse="_"), sep="_"):=SGP]
            } else {
                tmp.quantile.data <- data.table(rbindlist(tmp.quantiles[seq(return.additional.max.order.sgp)]), key="ID")
                quantile.data[,paste("SGP_FROM", paste(as.numeric(unlist(strsplit(sgp.labels[['my.year']], "_")))-return.additional.max.order.sgp, collapse="_"), sep="_"):=tmp.quantile.data[c(which(!duplicated(tmp.quantile.data, by=key(tmp.quantile.data)))[-1L]-1L, dim(tmp.quantile.data)[1L])][["SGP"]]]
            }
        }

		quantile.data[,SCALE_SCORE_PRIOR:=prior.ss]

		if (return.prior.scale.score.standardized) {
			SCALE_SCORE_PRIOR_STANDARDIZED <- NULL
			quantile.data[,SCALE_SCORE_PRIOR_STANDARDIZED:=round(as.numeric(scale(prior.ss)), digits=3L)]
		}

		if (tf.growth.levels) {
			SGP_LEVEL <- NULL
			quantile.data[, SGP_LEVEL:=factor(findInterval(quantile.data[["SGP"]], tmp.growth.levels[["my.cuts"]]),
				levels=seq(length(tmp.growth.levels[["my.levels"]]))-1L, ## Necessary in case the full range of SGPs isn't present
				labels=tmp.growth.levels[["my.levels"]], ordered=TRUE)]
		}

    if (csem.tf) {
			simulation.data <- data.table(rbindlist(tmp.csem.quantiles), key="ID")
			simulation.data <- simulation.data[c(which(!duplicated(simulation.data, by=key(simulation.data)))[-1L]-1L, dim(simulation.data)[1L])]

			if (is.character(calculate.confidence.intervals) || is.list(calculate.confidence.intervals)) {
				if (is.null(calculate.confidence.intervals$confidence.quantiles) || identical(toupper(calculate.confidence.intervals$confidence.quantiles), "STANDARD_ERROR")) {
                    if (print.other.gp) {
                        tmp.se <- list()
                        for (f in seq_along(tmp.csem.quantiles)) {
                            tmp.se[[f]] <- data.table("ID" = tmp.csem.quantiles[[f]][["ID"]], "ORDER" = f, "STANDARD_ERROR" = round(data.table(ID=tmp.csem.quantiles[[f]][[1L]], SGP=c(as.matrix(tmp.csem.quantiles[[f]][, grep("SGP", names(tmp.csem.quantiles[[f]])), with=FALSE])))[,sd(SGP), keyby=ID][['V1']], digits=2L))
                        }
                        tmp.se <- data.table(rbindlist(tmp.se), key="ID")
                        tmp.se <- dcast(tmp.se, ID~ORDER, value.var="STANDARD_ERROR")
                        setnames(tmp.se, grep("ID", names(tmp.se), invert=TRUE, value=TRUE), paste0("SGP_ORDER_", grep("ID", names(tmp.se), invert=TRUE, value=TRUE), "_STANDARD_ERROR"))
                        quantile.data <- merge(quantile.data, tmp.se, by="ID")
                    } # No 'else' - also return the plain SGP version
                    for (f in seq_along(tmp.csem.quantiles)) {
                        quantile.data[,SGP_STANDARD_ERROR:=round(data.table(ID=simulation.data[[1L]], SGP=c(as.matrix(simulation.data[, grep("SGP", names(tmp.csem.quantiles[[f]])), with=FALSE])))[,sd(SGP), keyby=ID][['V1']], digits=2L)]
                    }
				} else {
					if (!(is.numeric(calculate.confidence.intervals$confidence.quantiles) && all(calculate.confidence.intervals$confidence.quantiles < 1) &
						all(calculate.confidence.intervals$confidence.quantiles > 0))) {
						stop("Argument to 'calculate.confidence.intervals$confidence.quantiles' must be numeric and consist of quantiles.")
					}
                    if (print.other.gp) {
              tmp.se <- list()
              for (f in seq_along(tmp.csem.quantiles)) {
                  tmp.se[[f]] <- data.table("ID" = tmp.csem.quantiles[[f]][["ID"]], "ORDER" = f,
                  round(t(apply(tmp.csem.quantiles[[f]][, -1L, with=FALSE], 1, quantile, probs=calculate.confidence.intervals$confidence.quantiles))),
                      "STANDARD_ERROR" = round(data.table(ID=tmp.csem.quantiles[[f]][[1L]], SGP=c(as.matrix(tmp.csem.quantiles[[f]][,-1L,with=FALSE])))[,sd(SGP), keyby=ID][['V1']], digits=2L))
              }
              tmp.se <- data.table(rbindlist(tmp.se), key="ID")
              tmp.se <- dcast(tmp.se, ID~ORDER, value.var=c("STANDARD_ERROR", paste0(calculate.confidence.intervals$confidence.quantiles*100, "%")))
              setnames(tmp.se,
                  c(grep("STANDARD_ERROR", names(tmp.se), value=TRUE), grep(paste0(calculate.confidence.intervals$confidence.quantiles*100, "%", collapse="|"), names(tmp.se), value=TRUE)),
                  c(paste0("SGP_ORDER_", seq_along(tmp.csem.quantiles), "_STANDARD_ERROR"), paste0("SGP_ORDER_", as.vector(outer(seq_along(tmp.csem.quantiles), calculate.confidence.intervals$confidence.quantiles, paste, sep="_")), "_CONFIDENCE_BOUND")))
              quantile.data <- merge(quantile.data, tmp.se, by="ID")
          } # No 'else' - also return the plain SGP version
					tmp.cq <- data.table(round(t(apply(simulation.data[, -1L, with=FALSE], 1, quantile, probs=calculate.confidence.intervals$confidence.quantiles))))
					quantile.data[,paste0("SGP_", calculate.confidence.intervals$confidence.quantiles, "_CONFIDENCE_BOUND"):=tmp.cq]
					quantile.data[,SGP_STANDARD_ERROR:=round(data.table(ID=simulation.data[[1L]], SGP=c(as.matrix(simulation.data[,-1L,with=FALSE])))[,sd(SGP), keyby=ID][['V1']], digits=2L)]
				}
			}
            simulation.data[, GRADE := as.character(tail(grade.progression, 1))]
			Simulated_SGPs[[tmp.path]] <- rbindlist(list(simulation.data, Simulated_SGPs[[tmp.path]]), fill=TRUE)
		}

		if (simex.tf) {
			if (print.other.gp) {
				quantile.data <- quantile.data[quantile.data.simex[["DT"]]]
			} else {
        quantile.data[, SGP_SIMEX:=quantile.data.simex[['DT']][["SGP_SIMEX"]]]
        quantile.data[, SGP_SIMEX_RANKED:=quantile.data.simex[['DT']][["SGP_SIMEX_RANKED"]]]
      }
		}

		if (!is.null(percentile.cuts)) {
			quantile.data <- data.table(quantile.data, .get.best.cuts(tmp.percentile.cuts[grep("MAX_TIME", names(tmp.percentile.cuts), invert=TRUE)]))
            if (!is.null(SGPt.max.time)) quantile.data <- data.table(quantile.data, .get.best.cuts(tmp.percentile.cuts[grep("MAX_TIME", names(tmp.percentile.cuts))], "MAX_TIME"))
		}

		if (print.sgp.order || return.norm.group.identifier) {
			if (exact.grade.progression.sequence) {
				norm.groups <- paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression.for.norm.group, sep="_"), sep="/"), num.prior+1L), collapse="; ")
			} else {
				norm.groups <- sapply(seq_along(year.progression.for.norm.group)[-1L][seq.int(length(year.progression.for.norm.group)-1L)],
				    function(x) paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression.for.norm.group, sep="_"), sep="/"), x), collapse="; "))
			}
			if (!print.sgp.order) { # Return only SGP_NORM_GROUP
				if (exact.grade.progression.sequence) {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, labels=norm.groups))]
				} else {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups))]
				}
				quantile.data[, ORDER:=NULL]
			} else { # Return both ORDER and SGP_NORM_GROUP
				if (exact.grade.progression.sequence) {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, labels=norm.groups))]
				} else {
					quantile.data[, SGP_NORM_GROUP:=factor(factor(ORDER, levels=seq_along(norm.groups), labels=norm.groups))]
				}
				setnames(quantile.data, "ORDER", "SGP_ORDER")
			}
		}

		if (!is.null(return.norm.group.dates)) {
			my.tmp <- Panel_Data[,c("ID", setdiff(grep("TIME", names(Panel_Data), value=TRUE), grep("TIME_LAG", names(Panel_Data), value=TRUE))), with=FALSE][list(quantile.data$ID),-1L,with=FALSE,on="ID"]
            my.tmp <- my.tmp[,tail(seq(dim(my.tmp)[2L]), length(tmp.gp)),with=FALSE]
			quantile.data[,SGP_NORM_GROUP_DATES:=gsub("NA; ", "", do.call(paste, c(as.data.table(lapply(my.tmp, function(x) as.Date(x, origin="1970-01-01"))), list(sep="; "))))]
		}

		if (!is.null(return.norm.group.scale.scores)) {
			tmp.scale_scores <- ss.data[,c("ID", names(tmp.data)[-1L]), with=FALSE][list(quantile.data$ID),-1L,with=FALSE,on="ID"]
			quantile.data[,SGP_NORM_GROUP_SCALE_SCORES:=gsub("NA; ", "", do.call(paste, c(tmp.scale_scores, list(sep="; "))))]
		}

    if (!is.null(return.norm.group.preference)) quantile.data[, PREFERENCE := return.norm.group.preference]

		if ((is.character(goodness.of.fit) || goodness.of.fit==TRUE) && dim(quantile.data)[1L] <= goodness.of.fit.minimum.n) {
			messageSGP(c("\tNOTE: Due to small number of cases (", dim(quantile.data)[1L], ") no goodness of fit plots produced."))
			goodness.of.fit <- FALSE
		}

		if (is.character(goodness.of.fit) || goodness.of.fit==TRUE) {
			if (simex.tf) {
				sgps.for.gof <- c("SGP", "SGP_SIMEX", "SGP_SIMEX_RANKED")
				sgps.for.gof.path <- c(tmp.path, paste(tmp.path, "SIMEX", sep="."), paste(tmp.path, "RANKED_SIMEX", sep="."))
			} else {
				sgps.for.gof <- "SGP"
				sgps.for.gof.path <- tmp.path
			}
			if (is.character(goodness.of.fit) && goodness.of.fit %in% objects(SGP::SGPstateData) &&
                !identical(sgp.labels$my.extra.label, "EQUATED") &&
                !is.null(SGP::SGPstateData[[goodness.of.fit]][['Achievement']][['Cutscores']][[get.prior.cutscore.path(rev(content_area.progression)[2L], yearIncrement(rev(year.progression)[2L], 1L, year_lags.progression[1L]))]][[paste0("GRADE_", rev(tmp.gp)[2L])]])) {
                    GRADE <- YEAR <- CONTENT_AREA <- NULL
                    tmp.gof.data <- getAchievementLevel(
							sgp_data=data.table(
								ID=quantile.data[['ID']],
								SCALE_SCORE=quantile.data[['SCALE_SCORE_PRIOR']],
								quantile.data[, c(sgps.for.gof, "SGP_NORM_GROUP"), with=FALSE],
								VALID_CASE="VALID_CASE",
								CONTENT_AREA=rev(content_area.progression)[2L],
								YEAR=rev(year.progression.for.norm.group)[2L],
								GRADE=rev(tmp.gp)[2L],
								CONTENT_AREA_CURRENT=sgp.labels[['my.subject']],
								YEAR_CURRENT=sgp.labels[['my.year']],
								GRADE_CURRENT=tmp.last),
							state=goodness.of.fit,
							year=rev(year.progression.for.norm.group)[2L],
							content_area=rev(content_area.progression)[2L],
							grade=tail(tmp.gp, 2L)[1L])[,!"GRADE", with=FALSE]

				setnames(tmp.gof.data, c("SCALE_SCORE", "ACHIEVEMENT_LEVEL", "CONTENT_AREA", "CONTENT_AREA_CURRENT", "YEAR", "YEAR_CURRENT", "GRADE_CURRENT"),
					c("SCALE_SCORE_PRIOR", "ACHIEVEMENT_LEVEL_PRIOR", "CONTENT_AREA_PRIOR", "CONTENT_AREA", "YEAR_PRIOR", "YEAR", "GRADE"))
				setnames(ss.data, dim(ss.data)[2L], "SCALE_SCORE")
				setkeyv(tmp.gof.data, "ID")
				tmp.gof.data <- ss.data[, c("ID", "SCALE_SCORE"), with=FALSE][tmp.gof.data]

				### Rename SGP_NORM_GROUP_BASELINE for gofSGP - expecting consistent name to establish norm.group.var in that function
				if ("SGP_NORM_GROUP_BASELINE" %in% names(tmp.gof.data)) setnames(tmp.gof.data, "SGP_NORM_GROUP_BASELINE", "SGP_NORM_GROUP")

				for (gof.iter in seq_along(sgps.for.gof)) {
					Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]][['TMP_NAME']] <- gofSGP(
						sgp_object=tmp.gof.data,
						state=goodness.of.fit,
						years=sgp.labels[['my.year']],
						content_areas=sgp.labels[['my.subject']],
						content_areas_prior=tmp.gof.data[['CONTENT_AREA_PRIOR']][1L],
						grades=tmp.last,
						use.sgp=sgps.for.gof[gof.iter],
						output.format=goodness.of.fit.output.format)

                    if (!is.null(Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]][['TMP_NAME']])) {
						tmp.gof.plot.name <- paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression, sep="_"), sep="/"), num.prior+1L), collapse="; ")
                        tmp.gof.plot.name <- gsub("MATHEMATICS", "MATH", tmp.gof.plot.name)
                        names(Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]])[length(Goodness_of_Fit[[tmp.path]])] <-
                        gsub("/", "_", paste(gsub(";", "", rev(unlist(strsplit(tail(tmp.gof.plot.name, 1L), " ")))), collapse=";"))
					}
				}
			} else {
				tmp.gof.data <- data.table(
					ID=quantile.data[['ID']],
					SCALE_SCORE_PRIOR=quantile.data[['SCALE_SCORE_PRIOR']],
					quantile.data[, sgps.for.gof, with=FALSE],
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=sgp.labels[['my.subject']],
					YEAR=sgp.labels[['my.year']],
					GRADE=tmp.last, key="ID")

				setnames(ss.data, dim(ss.data)[2L], "SCALE_SCORE")
				tmp.gof.data <- ss.data[, c("ID", "SCALE_SCORE"), with=FALSE][tmp.gof.data]

				for (gof.iter in seq_along(sgps.for.gof)) {
					Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]][['TMP_NAME']] <- gofSGP(
						sgp_object=tmp.gof.data,
						years=sgp.labels[['my.year']],
						content_areas=sgp.labels[['my.subject']],
						grades=tmp.last,
						use.sgp=sgps.for.gof[gof.iter],
						output.format=goodness.of.fit.output.format)

					if (!is.null(Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]][['TMP_NAME']])) {
                        tmp.gof.plot.name <- paste(tail(paste(year.progression.for.norm.group, paste(content_area.progression, grade.progression, sep="_"), sep="/"), num.prior+1L), collapse="; ")
                        tmp.gof.plot.name <- gsub("MATHEMATICS", "MATH", tmp.gof.plot.name)
                        names(Goodness_of_Fit[[sgps.for.gof.path[gof.iter]]])[length(Goodness_of_Fit[[tmp.path]])] <-
                        gsub("/", "_", paste(gsub(";", "", rev(unlist(strsplit(tail(tmp.gof.plot.name, 1L), " ")))), collapse=";"))
                    }
				}
			}
		}

		if (identical(sgp.labels[['my.extra.label']], "BASELINE")) setnames(quantile.data, "SGP", "SGP_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") && tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_BASELINE")
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") && "SGP_STANDARD_ERROR" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_STANDARD_ERROR", "SGP_BASELINE_STANDARD_ERROR", names(quantile.data)))
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") && "SGP_ORDER" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_ORDER", "SGP_BASELINE_ORDER", names(quantile.data)))
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") && "SGP_NORM_GROUP" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE", names(quantile.data)))
		if (identical(sgp.labels[['my.extra.label']], "BASELINE") && simex.tf) setnames(quantile.data, gsub("_SIMEX", "_SIMEX_BASELINE", names(quantile.data))) # SGP_SIMEX and SGP_SIMEX_RANKED
        if (identical(sgp.labels[["my.extra.label"]], "BASELINE") && return.prior.scale.score) setnames(quantile.data, "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_BASELINE")
        if (identical(sgp.labels[["my.extra.label"]], "BASELINE") && return.prior.scale.score.standardized) setnames(quantile.data, "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE")
        if (identical(sgp.labels[["my.extra.label"]], "BASELINE") && !is.null(percentile.cuts)) setnames(quantile.data, gsub("PERCENTILE_CUT_", "PERCENTILE_CUT_BASELINE_", names(quantile.data)))

		if (identical(sgp.labels[['my.extra.label']], "EQUATED")) setnames(quantile.data, "SGP", "SGP_EQUATED")
		if (identical(sgp.labels[['my.extra.label']], "EQUATED") && tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_EQUATED")
		if (identical(sgp.labels[['my.extra.label']], "EQUATED") && "SGP_NORM_GROUP" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_EQUATED", names(quantile.data)))

		if (!is.null(additional.vnames.to.return)) {
			quantile.data <- panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return)), with=FALSE][quantile.data, on="ID"]
			setnames(quantile.data, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
		}

		if (!return.prior.scale.score) {
			quantile.data[,SCALE_SCORE_PRIOR:=NULL]
		}

    ##  Return GRADE value for SGP Key
    quantile.data[, GRADE := as.character(tail(grade.progression, 1))]

		SGPercentiles[[tmp.path]] <- rbindlist(list(quantile.data, SGPercentiles[[tmp.path]]), fill=TRUE)

	} ## End if calculate.sgps


	### Start/Finish Message & Return SGP Object

	if (print.time.taken) {
		if (calculate.sgps) cohort.n <- format(dim(quantile.data)[1L], big.mark=",") else cohort.n <- format(max.cohort.size, big.mark=",")
		messageSGP(paste("\tStarted studentGrowthPercentiles:", started.date))
			messageSGP(paste0("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
				paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label, " (N=", cohort.n, ")"))
		if (verbose.output) messageSGP(Verbose_Messages)
		messageSGP(c(tmp.messages, "\tFinished studentGrowthPercentiles: ", prettyDate(), " in ", convertTime(timetakenSGP(started.at)), "\n"))
	}

	list(Coefficient_Matrices=Coefficient_Matrices,
		Cutscores=Cutscores,
		Goodness_of_Fit=Goodness_of_Fit,
		Knots_Boundaries=Knots_Boundaries,
		Panel_Data = if (return.panel.data) Panel_Data else NULL,
		SGPercentiles=SGPercentiles,
		SGProjections=SGProjections,
		Simulated_SGPs=Simulated_SGPs)
} ### END studentGrowthPercentiles Function
