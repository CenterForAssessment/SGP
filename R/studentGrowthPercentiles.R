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
		 percuts.digits.internal=NULL,
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

    .smooth_bound_iso <- function(
        predicted_values
    ) {
        predicted_values |>
            { \(.) {
                if (!is.null(sgp.loss.hoss.adjustment)) {   ##  LOSS/HOSS adjustment
                    collapse::replace_outliers(., value = "clip", limits = loss.hoss)
                } else .}
            }() |> { \(.) {
                if (isotonize) {   ##  isotonize (sort) the predicted values by row/student
                    Rfast::rowSort(.)
                } else . }
            }() |> Rfast::Round(digit = percuts.digits.internal)
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

	.get.panel.data <- function(ss.data, k, by.grade, tmp.gp) {
		if (by.grade) {
            if (is.character(tmp.gp)) tmp.gp.gpd <- shQuote(rev(tmp.gp)[seq(k+1L)]) else tmp.gp.gpd <- rev(tmp.gp)[seq(k+1L)]
            eval(parse(text=paste0("na.omit(ss.data[.(", paste(tmp.gp.gpd, collapse=", "), "), on=names(ss.data)[c(", paste(1L+num.panels-(0:k), collapse=", ") , ")]], cols=names(ss.data)[c(",paste(1L+2*num.panels-0:k, collapse=", "), ")])[,c(1, ", paste(rev(1+2*num.panels-0:k), collapse=", "),  ")]")))
		} else {
            eval(parse(text=paste0("na.omit(ss.data, cols=names(ss.data)[c(",paste(1+2*num.panels-0:k, collapse=", "), ")])[,c(1, ", paste(rev(1L+2*num.panels-0:k), collapse=", "),  ")]")))
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

    .get_cohort_data <- function(
        n.priors = num.prior,
        include.SGPt = !is.null(SGPt_data)
    ) {
        if (by.grade) {
            if (is.character(tmp.gp)) {
                tmp.gp.gpd <- shQuote(rev(tmp.gp)[seq(n.priors+1L)])
            } else tmp.gp.gpd <- rev(tmp.gp)[seq(n.priors+1L)]
                tmp_data <-
                    eval(parse(text=paste0("ss.data[.(", paste(tmp.gp.gpd[1:2], collapse=", "),
                    "), on=names(ss.data)[c(", paste(1L+num.panels-(0:1), collapse=", ") , ")]]"))) #|>
                if (num.panels >= 3L ) {
                    tmp.gp.gpd <- gsub("'", "", tmp.gp.gpd)
                    ss.locs <- (1L + 2*num.panels) - 0:n.priors
                    gd.locs <- (1L + num.panels) - (0:(num.panels - 1))
                    for (g in 3:num.panels) {
                        tmp_data[
                            tmp_data[[names(tmp_data)[gd.locs[g]]]] != tmp.gp.gpd[g],
                            ss.locs[g] := NA
                        ]
                    }
                }
                tmp_data <-
                    collapse::ss(tmp_data, j = c(1, rev((1 + 2*num.panels) - 0:n.priors)), check = FALSE)
        } else {
            tmp_data <-
                collapse::ss(ss.data, j = c(1, rev((1 + 2*num.panels) - 0:n.priors)), check = FALSE)
        }
        tmp_data |>
            {\(.) if (exact.grade.progression.sequence) {
                    collapse::na_omit(X = .)
            }  else collapse::na_omit(X = ., cols = c(ncol(.)-1, ncol(.)))}() |>
                data.table::setnames(c("ID", paste0("prior_", rev(seq(n.priors))), "final_yr")) |>
                data.table::setkeyv("ID") |>
                    {\(.) if (include.SGPt) {
                        collapse::join(., SGPt_data, on = "ID", verbose = FALSE) |>
                        data.table::setcolorder(names(SGPt_data))
                    } else .}()
    }

    .get_model_data <-  function(
        cohort_data,
        prior.order,
        knots_bounds
    ) {
        # Create B-spline terms for each grade level and gather config/spec information
        n.orders <- length(prior.order)
        rhs_terms <- specs <- vector("list", n.orders)
        knt_list <- bnd_list <- list()
        for (g in seq_along(prior.order)) {
            grade <- prior.order[g]
            knt <- knots_bounds[[g]][[paste0("knots_", grade)]]
            knt_list <- c(knt_list, setNames(list(knt), paste0("knots_", grade)))
            bnd <- knots_bounds[[g]][[paste0("boundaries_", grade)]]
            bnd_list <- c(bnd_list, setNames(list(bnd), paste0("boundaries_", grade)))
            rhs_terms[[g]] <- paste0(
                paste0("bs(prior_", g, ", "),
                "knots = ", paste0("c(", paste(knt, collapse = ", "), ")"), ", ",
                "Boundary.knots = ", paste0("c(", paste(bnd, collapse = ", "), ")"), ")"
            )
            n.vars <- length(prior.order[1:g]) + 1L
            specs[[g]][["Knots"]] <- knt_list
            specs[[g]][["Boundaries"]] <- bnd_list
            specs[[g]][["grades"]] <- list(as.character(tail(grade.progression, n.vars)))
            specs[[g]][["content_areas"]] <- list(tail(content_area.progression, n.vars))
            specs[[g]][["years"]] <- list(as.character(tail(year.progression, n.vars)))
            specs[[g]][["year_lags"]] <- list(as.numeric(tail(year_lags.progression, n.vars - 1L)))
        }
        kb.attribs <- lapply(rhs_terms, attributes) |> unlist(recursive = FALSE)
        if (n.time.terms <- length(grep("TIME", names(cohort_data)))) {
            rhs_terms <- c(rhs_terms, "I(cohort_data[['TIME']])", "I(cohort_data[['TIME_LAG']])")
        }
        ##  Construct the final formula
        rhs_terms <- paste(rhs_terms, collapse=" + ")
        model <- as.formula(paste0("final_yr ~ ", rhs_terms))
        mf <- model.frame(model, cohort_data, na.action = NULL)
        Y <- model.response(mf)
        X <- .rename_model_matrix(model.matrix(model, mf), n.time.terms)
        rownames(X) <- cohort_data[["ID"]]

        if (exact.grade.progression.sequence || n.orders == 1) {
            return(list(
                X = list(X),
                Y = list(Y),
                specs = tail(specs, 1)
            ))
        }

        subx.list <- vector(mode = "list", length = n.orders)
        suby.list <- vector(mode = "list", length = n.orders)

        for (ord in seq(n.orders)) {
            cols.to.get <-
                grep(paste0("PRIOR_", ord:1, collapse = "|"),
                    colnames(X), value = TRUE)
            if (length(t.names <- grep("TIME", colnames(X), value = TRUE))) {
                cols.to.get <- c(cols.to.get, t.names)
            }
            subx <- collapse::na_omit(X, cols = cols.to.get, na.attr = TRUE)
            if (!is.null(attributes(subx)$na.action)) {
                suby.list[[ord]] <- Y[-(attributes(subx)$na.action)]
            } else  suby.list[[ord]] <- Y
            subx.list[[ord]] <- subx[, c("Intercept", cols.to.get)]
        }
        return(list(
            X = subx.list,
            Y = suby.list,
            specs = specs
        ))
    }

    .rename_model_matrix <- function(
        data,
        n.time.terms = 0L
    ) {
        n.priors <- (ncol(data) - 1 - n.time.terms) / 7
        bs.names <-
            paste0(
                rep(paste0("PRIOR_", seq(n.priors)), each = 7),
                rep(paste0("__BSPLINE_", 1:7), n.priors)
            )
        if (n.time.terms == 2L) {
            bs.names <- c(bs.names, "TIME", "TIME_LAG")
        }
        rownames(data) <- NULL
        return(collapse::setColnames(data, c("Intercept", bs.names)))
    }

    .qregSGP <- function(
        x, y, taus, method
    ) {
        tmp_res <- try(
            lapply(
                taus, \(tau.i) 
                quantreg::rq.fit(
                    x = x, y = y, tau = tau.i, method = method
                )[["coefficients"]]
            ),
            silent = TRUE
        )
        if (inherits(tmp_res, "try-error")) {
            tmp_res <- lapply(
                taus, \(tau.i) 
                quantreg::rq.fit.br(
                    x = x, y = y, tau = tau.i
                )[["coefficients"]]
            )
        }
        return(collapse::qM(tmp_res))
    }

    .create_coefficient_matrices <- function(
        model_data, par.config = NULL
    ) {
        n.matrices <- length(model_data[["specs"]])

        ##  Use internal par.config arg (instead of `parallel.config`)
        ##  so that NULL TAUS can be passed in SIMEX
        if (!is.null(tmp.par.config <- par.config)) {
            if (is.null(par.config[["WORKERS"]][["TAUS"]])) {
                tmp.par.config <- NULL
            } else {
                call_function <- define_compute(par.config, "TAUS")
            }
        }

        tmp_mtx <- vector(mode = "list", n.matrices)

        for (ord in seq(n.matrices)) {
            if (!is.null(max.n.for.coefficient.matrices) && length(model_data[["Y"]][[ord]]) > max.n.for.coefficient.matrices) {
                sample.index <- sample(seq.int(length(model_data[["Y"]][[ord]])), max.n.for.coefficient.matrices)
                model_data[["X"]][[ord]] <- model_data[["X"]][[ord]][sample.index, , drop = FALSE]
                model_data[["Y"]][[ord]] <- model_data[["Y"]][[ord]][sample.index]
            }
            if (is.null(max.n.for.coefficient.matrices) && length(model_data[["Y"]][[ord]]) >= 300000L) {
                rq.method <- rq.method.for.large.n
            }

            if (is.null(tmp.par.config)) {
                tmp_mtx[[ord]] <- sync_compute(
                    chunk_taus(
                        cohort_data = list(x = model_data[["X"]][[ord]], y = model_data[["Y"]][[ord]]),
                        taus, length(taus), rq.method
                    ),
                    .qregSGP) |> collapse::qM()
            } else {
                mod_tau_data <- chunk_taus(
                    cohort_data = list(x = model_data[["X"]][[ord]], y = model_data[["Y"]][[ord]]),
                    taus, par.config[["WORKERS"]][["TAUS"]], rq.method
                )
                tmp_mtx[[ord]] <- call_function(mod_tau_data, .qregSGP) |> collapse::qM()
            }

            tmp_mtx[[ord]] <- new("splineMatrix",
                .Data = collapse::setColnames(tmp_mtx[[ord]], paste0("tau=", format(round(taus, 3)))),
                Knots = model_data[["specs"]][[ord]][["Knots"]],
                Boundaries = model_data[["specs"]][[ord]][["Boundaries"]],
                Content_Areas = model_data[["specs"]][[ord]][["content_areas"]],
                Grade_Progression = model_data[["specs"]][[ord]][["grades"]],
                Time = model_data[["specs"]][[ord]][["years"]],
                Time_Lags = model_data[["specs"]][[ord]][["year_lags"]],
                Version = list(
                    SGP_Package_Version = as.character(packageVersion("SGP")),
                    Date_Prepared = prettyDate(),
                    Matrix_Information = list(
                        N = length(model_data[["Y"]][[ord]]),
                        SIMEX = model_data[["specs"]][[ord]][["SIMEX"]],
                        SGPt = if (is.null(SGPt)) NULL else {
                          list(VARIABLES = unlist(SGPt),
                                N = length(model_data[["Y"]][[ord]]),
                                MIN_TIME = min(model_data[["X"]][[ord]][,"TIME"], na.rm = TRUE),
                                MAX_TIME = max(model_data[["X"]][[ord]][,"TIME"], na.rm = TRUE),
                                MAX_TIME_PRIOR =
                                    max(model_data[["X"]][[ord]][,"TIME"] -
                                        model_data[["X"]][[ord]][,"TIME_LAG"], na.rm = TRUE),
                                RANGE_TIME_LAG = range(model_data[["X"]][[ord]][,"TIME_LAG"]))
                        }
                ))
            )
        }
        names(tmp_mtx) <- get.coefficient.matrix.name(tmp.last, coefficient.matrix.priors)
        return(tmp_mtx)
    } ### END .create_coefficient_matrices

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

    .get_percentile_predictions <- function(model_matrix, coef_matrix, SGPt.max.time = NULL) {
        if ("TIME" %in% colnames(model_matrix)) {
            model_matrix <- collapse::qDT(model_matrix)
            tmp.time.shift.index <-
                getTimeShiftIndex(max(as.numeric(model_matrix[["TIME"]])), coef_matrix)
            if (max(model_matrix$TIME+365*-tmp.time.shift.index, na.rm=TRUE) >
                coef_matrix@Version[["Matrix_Information"]][["SGPt"]][["MAX_TIME"]]+30) {
                    stop("Matrix Misfit with TIME data!!!")
            }
            if (is.null(SGPt.max.time) && tmp.time.shift.index != 0) {
                model_matrix[, TIME := TIME + 365 * -tmp.time.shift.index]
            }
            if (!is.null(SGPt.max.time)) {
                model_matrix[, TIME_LAG := TIME_LAG +
                    coef_matrix@Version[["Matrix_Information"]][["SGPt"]][["MAX_TIME"]] -
                    (TIME + 365 * -tmp.time.shift.index)
                ][, TIME := coef_matrix@Version[["Matrix_Information"]][["SGPt"]][["MAX_TIME"]]
                ]
            }
            model_matrix <- collapse::qM(model_matrix)
        }

        model_matrix %*% coef_matrix |>
		    .smooth_bound_iso()
    }

    .get_quantiles <- function(pred, obsvd, ranked.simex = FALSE) {
        if (is.character(ranked.simex)) {
            use.original.ranking.system <- TRUE; ranked.simex <- TRUE
        } else use.original.ranking.system <- FALSE

        if (ranked.simex) {
          for (p in seq.int(3)) { # Additional values between the tau predicted values - 1/8th percentiles for ranking
            pred_midpoint <- collapse::setop(
                pred[,(seq.int(ncol(pred))-1L)], "+",
                (Rfast::coldiffs(pred) |> collapse::setop("/", 2))
            )
            pred <- cbind(pred, pred_midpoint)[, order(c(seq.int(ncol(pred)), seq.int(ncol(pred)-1)))]
            if (!is.matrix(pred) & is.vector(pred)) {  #  Account for edge cases where a single student is fed in (e.g., baseline)
              pred <- matrix(pred, nrow = 1, byrow = TRUE)
            }
          }
          tmp.zero <- 794L
        } else tmp.zero <- 101L

        tmp <- Rfast::rowMins(cbind(pred < obsvd, FALSE)) - 1L
        if (ranked.simex) collapse::setop(tmp, "/", 8)

        if (!is.null(sgp.quantiles.labels)) {
            tmp <- as.data.table( collapse::qF(tmp, sort = FALSE))
            setattr(tmp[["V1"]], "levels", sgp.quantiles.labels)
            return(as.integer(levels(tmp[['V1']]))[tmp[['V1']]])
        } else {
            if (!is.null(sgp.loss.hoss.adjustment)) {
                if (length(tmp.index <- which(obsvd >= loss.hoss[2L]))) {
                    if (ranked.simex) {
                        tmp.adj <- (cbind(pred > obsvd, TRUE) |>
                            collapse::ss(i = tmp.index, check = FALSE) |> Rfast::rowMaxs()) -1
                        if (!use.original.ranking.system) collapse::setop(tmp.adj, "/", 8)
                        collapse::setv(tmp, tmp.index, tmp.adj)
                    } else {
                        collapse::setv(
                            tmp, tmp.index,
                            (cbind(pred > obsvd, TRUE) |>
                                collapse::ss(i = tmp.index, check = FALSE) |>
                                   Rfast::rowMaxs()
                            ) - 1 )
                    }
                }
            }
            if (convert.0and100) {
                tmp <- collapse::replace_outliers(tmp, value = "clip", limits = c(1L, 99L))
            }
            return(as.integer(tmp))
        }
    }

	.get.percentile.cuts <- function(data1) {
		tmp <- Rfast::Round(data1[ , percentile.cuts+1L, drop=FALSE], digit = percuts.digits)
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
        cuts.best <- cuts.best[c(which(!duplicated(cuts.best, by=key(cuts.best)))[-1L]-1L, dim(cuts.best)[1L])][,!"ID"]
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
        use.original.ranking.system=FALSE,
        set.seed.for.sim.data=TRUE,
        verbose=FALSE
    ) {
        GRADE <- CONTENT_AREA <- YEAR <- V1 <- Lambda <-
        tau <- b <- .SD <- TEMP <- CSEM <- VARIABLE <- NULL ## To avoid R CMD check warnings

      ###   simex.sgp internal utility functions

        get.simex.ranking.info <- function(
            table_list, cap, gp, yp, ylp
        ) {
            table.index <-
                sapply(table_list,
                    function(f) {
                        identical(attr(f, "content_area_progression"), cap) &
                        identical(attr(f, "grade_progression"), gp) &
                        identical(attr(f, "year_progression"), yp) &
                        identical(attr(f, "year_lags_progression"), ylp)
                    }) |> which()

            if (!length(table.index)) {
                stop("\n\t\tNo matching SIMEX `ranked_simex_table` entries found for progression ",
                    paste(gsub(" EOCT", "", paste(cap, gp)), collapse = " --> "))
            }
            if (length(table.index) > 1L){
                g  <- tail(gp, 1)
                ca <- tail(cap, 1)
                pr <- head(gsub(" EOCT", "", paste(cap, gp)), -1) # needs separate pastes? `paste(pr, collapse = ", ")` -v- SMH...
                msg.prior.info <- paste0(" { Prior", ifelse(length(pr) == 1L, ": ", "s: "), paste(pr, collapse = ", "), " }")
                messageSGP(c(
                    "\n\t\tMultiple matching SIMEX `ranked_simex_table` entries for", ca, if(g != "EOCT"){paste(" Grade", g)}, msg.prior.info,
                    "\n\t\tThe first `ranked_simex_table` entry will be used, but tables with duplicate `attributes` should be investigated!!!",
                    ifelse(identical(table_list[[table.index[1]]], table_list[[table.index[2]]]),
                        "\n\t\tThe first SET of duplicate tables are identical, so ... that's encouraging ...\n",
                        "\n\t\tThe first SET of duplicate tables are NOT identical\n\t\t\t\t!!! INVESTIGATE DIFFERENCES!!!\n"
                )))
            }
            return(table_list[[table.index[1]]])
        }

        ###   Check arguments/define variables
        tmp.last <- tail(tmp.gp, 1L)
        if (is.null(dependent.var.error)) dependent.var.error <- FALSE
        if (is.null(use.cohort.for.ranking)) use.cohort.for.ranking <- FALSE
        if (is.null(use.original.ranking.system)) use.original.ranking.system <- FALSE
        if (is.null(set.seed.for.sim.data)) set.seed.for.sim.data <- TRUE
        if (is.null(verbose)) verbose <- FALSE
        if (verbose) messageSGP(c("\n\tStarted SIMEX SGP calculation ", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " ", prettyDate()))
        if (is.logical(simex.use.my.coefficient.matrices) && !simex.use.my.coefficient.matrices) simex.use.my.coefficient.matrices <- NULL
        if (!is.null(state) && !is.null(csem.data.vnames)) stop("SIMEX config can not use both 'state' and 'csem.data.vnames' elements.")
        if (!is.null(parallel.config)) {
            if (is.null(parallel.config[["WORKERS"]][["SIMEX"]])) tmp.par.config <- NULL else tmp.par.config <- parallel.config
        } else tmp.par.config <- NULL
        if (!is.null(tmp.par.config)) {
            if (!exists('year.progression.for.norm.group')) # Needed for Baseline Matrix construction
                year.progression.for.norm.group <- year.progression
        }

        ##  Do all coefficient estimation with Frisch-Newton method
        tmp.rq.meth <- rq.method
        if (!grepl("fn", rq.method)) {
            rq.method <- "fn"
        }

        ## Establish the simulation iterations - either 1) 1:B, or 2) a sample of either B or the number of previously computed matrices
        sim.iters <- seq.int(B)

		fitted <- extrap <- tmp.quantiles.simex <- simex.coef.matrices <- list()
		my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))

        if (!is.null(csem.data.vnames)) {
            if (length(content_area.progression) == length(csem.data.vnames))
                csem.data.vnames <- head(csem.data.vnames, -1L)
            if (length(content_area.progression) < length(csem.data.vnames))
                csem.data.vnames <-
                    tail(head(csem.data.vnames, -1L),
                        (length(csem.data.vnames)-(length(csem.data.vnames)-length(content_area.progression)+1L)))
        }
		if (!is.null(use.my.coefficient.matrices)) { # Passed implicitly from studentGrowthPercentiles arguments
			if (exact.grade.progression.sequence) {
				simex.matrix.priors <- num.prior
			} else {
				simex.matrix.priors <- seq(num.prior)
			}
		} else simex.matrix.priors <- coefficient.matrix.priors

        simex.coef.matrices.path <- paste0(tmp.path.coefficient.matrices, ".SIMEX")

        ###   Loop over priors
		for (ord in simex.matrix.priors) {
            ord.lab <- paste0("order_", ord)
            # model_data <- .get_model_data(tmp_data, num.prior = k, SGPt_data) # Dependency on model_data could be a problem if SIMEX is extracted to its own function.
			n.records <- length(model_data[["Y"]][[ord]])
			tmp.num.variables <- ncol(cohort_data)
			tmp.gp.iter <- rev(tmp.gp)[2L:(ord+1L)]
			if (dependent.var.error) {
				perturb.var <- rev(tmp.gp)[seq.int((ord+1L))]
				start.index <- 1L
				num.perturb.vars <- tmp.num.variables+1L
			} else {
				perturb.var <- tmp.gp.iter
				start.index <- 2L
				num.perturb.vars <- tmp.num.variables
			}
			tmp.ca.iter <- rev(content_area.progression)[start.index:(ord+1L)]
			tmp.yr.iter <- rev(year.progression)[start.index:(ord+1L)]

			## naive model
            # Always calculate SIMEX SGPs (for ranked SIMEX table)
            fitted[[ord.lab]] <- matrix(0, nrow=length(lambda), ncol=n.records*length(taus))
            tmp.matrix <- getsplineMatrices(
                Coefficient_Matrices[[tmp.path.coefficient.matrices]],
                tail(content_area.progression, ord+1L),
                tail(grade.progression, ord+1L),
                tail(year.progression, ord+1L),
                tail(year_lags.progression, ord),
                my.matrix.order=ord,
                my.matrix.time.dependency=SGPt)[[1L]]

            fitted[[ord.lab]][1L,] <- .get_percentile_predictions(model_data[["X"]][[ord]], tmp.matrix)
		}  ##  Note that `perturb.var` ends up being the max prior one - important in v 2.3-0.0 forward

		##  add csems to cohort_data
		if (!is.null(state)) {
			for (g in rev(seq_along(perturb.var))) {
				if ("YEAR" %in% names(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
					CSEM_Data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]][
						GRADE==perturb.var[g] & CONTENT_AREA==tmp.ca.iter[g] & YEAR==tmp.yr.iter[g]]
				} else {
					CSEM_Data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]][
						GRADE==perturb.var[g] & CONTENT_AREA==tmp.ca.iter[g]]
				}
                if (dim(CSEM_Data)[1L] == 0L) {
                    stop(paste(
                        "CSEM data for", tmp.ca.iter[g], "Grade", perturb.var[g],
                        "is required to use SIMEX functionality, but is not available in SGPstateData.",
                        "\n\tPlease contact package administrators to add CSEM data."
                    ))
                }
				CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
				cohort_data[, paste0("icsem", perturb.var[g], tmp.ca.iter[g], tmp.yr.iter[g]) := CSEM_Function(cohort_data[[num.perturb.vars-g]])]
			}
		}

        if (!is.null(csem.data.vnames)) {
            for (g in rev(seq_along(perturb.var))) {
                cohort_data[,
                    paste0("icsem", perturb.var[g], tmp.ca.iter[g], tmp.yr.iter[g]) :=
                        Panel_Data[list(cohort_data[["ID"]])][[rev(csem.data.vnames)[g]]]
                ]
            }
        }

        if (verbose) messageSGP(c("\t\t", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " Order ", ord, " Started simulation process ", prettyDate()))

        ##  Create extended long data (N*B) with perturbed values for all values of Lambda
        big_data <- rbindlist(
                collapse::alloc(cohort_data, B, simplify = FALSE)
            )[, b := rep(seq.int(B), each = nrow(cohort_data))]
        if (dependent.var.error) {
            csem.col.offset <- (ncol(big_data)-2)/2
        } else csem.col.offset <- (ncol(big_data)-1)/2

        pert.cmd <- c()
        for (perturb.var.iter in rev(seq_along(perturb.var))) {
            tmp.var  <- names(big_data)[1L+perturb.var.iter] # paste0("prior_", perturb.var.iter)
            tmp.csem <- names(big_data)[1L+perturb.var.iter+csem.col.offset]
            for (L in lambda[-1L]) {
                pert.cmd <- c(pert.cmd,
                paste0(tmp.var, "_L", L, " = ", tmp.var, " + (sqrt(", L, ") * ", tmp.csem, 
                    " * Rfast::Rnorm(.N, seed X-X ceiling(369 * ", perturb.var.iter, " + ", L , ")))"))
            }
        }
        pert.cmd <- stats::setNames(pert.cmd, sub("^(.*) = .*", "\\1", pert.cmd))
        pert.cmd <- gsub("X-X", "=", pert.cmd)
        big_data <- data.table::setcbindlist(list(
            big_data[, c("ID", "b", if (!dependent.var.error) "final_yr"), with = FALSE],
            big_data[, lapply(pert.cmd, \(f) eval(parse(text = f)))
        ]))
        setkey(big_data, b, ID)
        bd.names <- names(big_data)

        if (dependent.var.error) perturb.var <- tail(perturb.var, -1)

        if (is.null(simex.use.my.coefficient.matrices) &&
            !identical(sgp.labels[["my.extra.label"]], "BASELINE")
        ) {
            simex_model_data <- vector(mode = "list", length(lambda[-1L]))
            for (L in lambda[-1L]) {
                L.idx <- which(L == lambda[-1L])
                L.lab <- paste0("_L", L)
                simex_knots_bounds <- vector(mode = "list", length(perturb.var))
                for (g in seq(perturb.var)) {
                    bnds <- big_data[[paste0("prior_", g, L.lab)]] |>
                        collapse::.range() |>
                            {\(.) list(
                                extendrange(r = ., f = 0.1) |> round(digits = 3),
                                extendrange(r = ., f = 0.0) |> round(digits = 3)
                            )}()  #  Boundaries and LOSS/HOSS
                    simex_knots_bounds[[g]][[paste0("boundaries_", perturb.var[g])]] <- bnds[[1L]]
                    simex_knots_bounds[[g]][[paste0("loss.hoss_", perturb.var[g])]] <- bnds[[2L]]
                    simex_knots_bounds[[g]][[paste0("knots_", perturb.var[g])]] <- 
                        big_data[[paste0("prior_", g, L.lab)]] |>
                          collapse::.quantile(probs = knot.cut.percentiles) |>
                            round(digits = 3)  #  Knots
                }
                ##  Split big_data into list of data.tables for matrix computations
                simex_model_data[[L.idx]] <-
                    lapply(sim.iters, \(z) {
                        list(model_data = .get_model_data(
                                cohort_data = big_data[list(z)]|>
                                  setnames(gsub(L.lab, "", bd.names)) |>
                                    collapse::ss(
                                      j = c("ID", paste0("prior_", simex.matrix.priors), "final_yr")),
                                prior.order = perturb.var,
                                knots_bounds = simex_knots_bounds
                        ))
                    })
                names(simex_model_data)[L.idx] <- paste0("lambda_", L)
            }
            rm(big_data)
        }
        if (!is.null(simex.use.my.coefficient.matrices)) { # Element from the 'calculate.simex' argument list.
            for (L in lambda[-1L]) {
                L.lab <- paste0("_L", L)
            for (ord in rev(simex.matrix.priors)) {
                ord.lab <- paste0("order_", ord)
                mtx.ord.path.name <- paste("qrmatrices", tmp.last, ord, sep = "_")
                n.records <- length(model_data[["Y"]][[ord]])

                available.matrices <- getsplineMatrices(
                    Coefficient_Matrices[[simex.coef.matrices.path]][[mtx.ord.path.name]][[paste0("lambda_", L)]],
                    tail(content_area.progression, ord+1L),
                    tail(grade.progression, ord+1L),
                    tail(year.progression, ord+1L),
                    tail(year_lags.progression, ord),
                    my.exact.grade.progression.sequence=TRUE,
                    return.multiple.matrices=TRUE,
                    my.matrix.order=ord,
                    my.matrix.time.dependency=SGPt) |> unlist(recursive=FALSE)

                if (length(available.matrices) > B)
                    sim.iters <- sample.int(length(available.matrices), B) # Stays as 1:B when length(available.matrices) == B
                if (length(available.matrices) < B)
                    sim.iters <- sample.int(length(available.matrices), B, replace=TRUE)
                if (length(available.matrices) != B)
                    available.matrices <- available.matrices[sim.iters]

                ##  Always calculate SIMEX SGPs (for ranked SIMEX table)
                if (verbose) {
                    messageSGP(c("\t\t\tStarted percentile prediction calculation, Lambda ", L, ": ", prettyDate()))
                }
                for (z in seq_along(sim.iters)) {
                    if (ord == max(simex.matrix.priors)) {
                        tmp_model_data <-
                            lapply(sim.iters, \(m)
                                list(model_data = .get_model_data(
                                        cohort_data = big_data[list(m)]|>
                                          setnames(gsub(L.lab, "", bd.names)) |>
                                            collapse::ss(
                                              j = c("ID", paste0("prior_", simex.matrix.priors), "final_yr")),
                                        prior.order = perturb.var,
                                        knots_bounds =
                                            lapply(1:length(perturb.var), \(x) {
                                                return(c(available.matrices[[m]]@Knots,
                                                        available.matrices[[m]]@Boundaries))
                                            })
                                ))
                            )
                        }

                        fitted[[ord.lab]][which(lambda == L), ] <-
                            collapse::setop(
                                X = fitted[[ord.lab]][which(lambda == L), ],
                                op = "+",
                                V = collapse::setop(
                                    .get_percentile_predictions(
                                        tmp_model_data[[z]][["model_data"]][["X"]][[ord]],
                                        available.matrices[[z]]
                                    ), "/", B)
                            )
                }
            }}
        } else {
            if (verbose) {
                messageSGP(c("\t\t\tStarted coefficient matrix calculation: ", prettyDate()))
            }

            simex_matrix_data <-
                vector(mode = "list", length = (length(sim.iters) * length(lambda[-1L])))
            n.records <- min(sapply(model_data[["Y"]], length))
            for (L in lambda[-1L]) {
                L.idx <- which(L == lambda[-1L])
                tmp.sim.iters <- (((L.idx-1) * 50)+1):(L.idx * 50)
                if (!is.null(simex.sample.size) && n.records >= simex.sample.size) {
                    for (z in tmp.sim.iters) {
                        Lz <- z - ((L.idx-1) * 50)
                    for (ord in simex.matrix.priors) {
                        subs.idx <- sample(seq.int(n.records), simex.sample.size)
                        simex_matrix_data[[z]][["model_data"]][["X"]][[ord]] <-
                            collapse::ss(simex_model_data[[L.idx]][[Lz]][["model_data"]][["X"]][[ord]],
                                    i = subs.idx, check = FALSE)
                        simex_matrix_data[[z]][["model_data"]][["Y"]][[ord]] <-
                            collapse::ss(simex_model_data[[L.idx]][[Lz]][["model_data"]][["Y"]][[ord]],
                                i = subs.idx, check = FALSE)
                        simex_matrix_data[[z]][["model_data"]][["specs"]][[ord]] <-
                            c(simex_model_data[[L.idx]][[Lz]][["model_data"]][["specs"]][[ord]],
                                list(SIMEX = paste0("lambda=", L)))
                        simex_matrix_data[[z]][["par.config"]] <- list()
                    }}
                }  else {
                    simex_matrix_data[tmp.sim.iters] <- simex_model_data[[L.idx]]
                    for (z in tmp.sim.iters) {
                        Lz <- z - ((L.idx-1) * 50)
                    for (ord in simex.matrix.priors) {
                        simex_matrix_data[[z]][["model_data"]][["specs"]][[ord]] <-
                            c(simex_model_data[[L.idx]][[Lz]][["model_data"]][["specs"]][[ord]],
                                list(SIMEX = paste0("lambda=", L)))
                        simex_matrix_data[[z]][["par.config"]] <- list()
                    }}
                }
            }

            if (is.null(tmp.par.config)) {
                ###   Sequential
                tmp.simex.matrices <-
                    lapply(simex_matrix_data, \(modl_dat) {
                        .create_coefficient_matrices(
                            model_data = modl_dat[["model_data"]],
                            par.config = parallel.config # use par taus here if available
                        )
                    })
            } else {
                ##  Calculate coefficient matricies in parallel
                call_function <- define_compute(tmp.par.config, "SIMEX")
                tmp.simex.matrices <-
                    call_function(simex_matrix_data, .create_coefficient_matrices)
                if (!all(err.index <- sapply(1:length(tmp.simex.matrices),
                    \(m) all(sapply(tmp.simex.matrices[[m]], is.splineMatrix))))
                ) {
                    recalc.index <- which(!err.index)
                    messageSGP(
                        c("\n\t\t", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " Order ", ord,
                        " Coefficient Matrix process(es) ", recalc.index, "FAILED!  Attempting to recalculate sequentially..."
                    ))
                    tmp.rq.meth2 <- rq.method
                    rq.method <- "br"
                    for (z in recalc.index) {
                        Lz <- z - ((L.idx-1) * 50)
                        tmp.simex.matrices[[Lz]] <-
                            .create_coefficient_matrices(
                                model_data = simex_model_data[[Lz]][["model_data"]],
                                par.config = parallel.config
                            )
                    }
                    tmp.rq.meth2 -> rq.method
                }
            }   ###   END parallel matrix calculation
            ##  get percentile predictions from coefficient matricies
            if (verbose) {
                messageSGP(c("\t\t\tStarted percentile prediction calculation: ", prettyDate()))
            }
            for (L in lambda[-1L]) {
                L.idx <- which(L == lambda[-1L])
                tmp.sim.iters <- (((L.idx-1) * 50)+1):(L.idx * 50)
                for (ord in simex.matrix.priors) {
                    ord.lab <- paste0("order_", ord)
                    fitted[[ord.lab]][which(lambda == L),] <-
                        lapply(tmp.sim.iters,
                            \(z) {
                                Lz <- z - ((L.idx-1) * 50)
                                .get_percentile_predictions(
                                    simex_model_data[[L.idx]][[Lz]][["model_data"]][["X"]][[ord]],
                                    tmp.simex.matrices[[z]][[ord]])
                            }) |>
                                Reduce(f = "+", x = _)/B

                    mtx.ord.name <- paste(tmp.last, ord, sep = "_")
                    simex.coef.matrices[[paste0("qrmatrices_", mtx.ord.name)]][[paste0("lambda_", L)]] <-
                        get_elem(tmp.simex.matrices[tmp.sim.iters], mtx.ord.name, regex = TRUE)
                }
            }
        }   ###   END Matrix creation and fitted values

        for (ord in simex.matrix.priors) {
            ord.lab <- paste0("order_", ord)
            mtx.ord.path.name <- paste("qrmatrices", tmp.last, ord, sep = "_")
            n.records <- length(model_data[["Y"]][[ord]])

            if (verbose) {
                messageSGP(c(
                    "\t\t", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L],
                    " Order ", ord, " Simulation process complete ", prettyDate()))
            }

            switch(
                extrapolation,
                LINEAR = Xmat <- matrix(c(rep(1, length(lambda)), lambda), ncol = 2),
                QUADRATIC = Xmat <- matrix(c(rep(1, length(lambda)), lambda, lambda^2), ncol = 3)
            )
            betas_k <- solve( crossprod(Xmat), crossprod(Xmat, fitted[[ord.lab]]) )

            extrap[[ord.lab]] <-
                collapse::setop(betas_k[2, ], "*", -1L) |>
                    {\(.) if (nrow(betas_k) == 3L) collapse::setop(., "+", betas_k[, 3]) else .}() |>
                        collapse::setop("+", betas_k[1, ]) |>
                            matrix(ncol = length(taus)) |> .smooth_bound_iso()

            tmp.ids <- rownames(model_data[["X"]][[ord]])

            if (is.null(simex.use.my.coefficient.matrices)) {
                ranked.simex.quantile.values <-
                    .get_quantiles(
                        extrap[[ord.lab]], model_data[["Y"]][[ord]],
                        ranked.simex = ifelse(use.original.ranking.system, "use.original.ranking.system", TRUE)
                    )
                ranked_simex_table <- collapse::qtable(ranked.simex.quantile.values)
                attr(ranked_simex_table, 'content_area_progression') <- tail(content_area.progression, ord+1L)
                attr(ranked_simex_table, 'grade_progression') <- tail(grade.progression, ord+1L)
                attr(ranked_simex_table, 'year_progression') <- tail(year.progression, ord+1L)
                attr(ranked_simex_table, 'year_lags_progression') <- tail(year_lags.progression, ord)
                attr(ranked_simex_table, 'n_records') <- n.records
                simex.coef.matrices[[mtx.ord.path.name]][["ranked_simex_table"]][[1]] <- ranked_simex_table

                tmp.quantiles.simex[[ord]] <- data.table(
                    ID = tmp.ids, SIMEX_ORDER = ord,
                    SGP_SIMEX = .get_quantiles(extrap[[ord.lab]], model_data[["Y"]][[ord]]),
                    SGP_SIMEX_RANKED = as.integer(Rfast::Round(100*(
                        data.table::frank(ties.method = "average", x = ranked.simex.quantile.values)/n.records), 0))
                )
            } else {
                if (any(grepl("ranked_simex_table", names(Coefficient_Matrices[[simex.coef.matrices.path]][[mtx.ord.path.name]])))) {
                    ranked.simex.info <- get.simex.ranking.info(
                    Coefficient_Matrices[[simex.coef.matrices.path]][[mtx.ord.path.name]][["ranked_simex_table"]],
                    tail(content_area.progression, ord+1L), tail(grade.progression, ord+1L), tail(year.progression, ord+1L), tail(year_lags.progression, ord))
                ranked.simex.tf <- TRUE
                } else {
                    if (use.cohort.for.ranking) {
                        ranked.simex.tf <- TRUE
                    } else {
                    messageSGP(c(
                        "\tRanked SIMEX SGP calculation with pre-calculated SGPs is only available with information embedded (as of",
                        "\n\tSGP version 1.9-4.0) or setting `use.cohort.for.ranking = TRUE` in the `calculate.simex` configuration.",
                        "\n\tNAs will be returned for SGP_SIMEX_RANKED."))
                        ranked.simex.tf <- FALSE
                    }
                }
                if (ranked.simex.tf) {
                    tmp.quantiles.simex[[ord]] <-
                        data.table(
                            ID = tmp.ids,
                            SIMEX_ORDER = ord,
                            SGP_SIMEX =
                            .get_quantiles(extrap[[ord.lab]], model_data[["Y"]][[ord]])
                        )
                    if (use.cohort.for.ranking) {
                    ##  Use `use.cohort.for.ranking=TRUE` if reproducing values with
                    ##  original data *OR* to rank against the updated/new cohort data ONLY
                    tmp.quantiles.simex[[ord]][,
                        SGP_SIMEX_RANKED := as.integer(Rfast::Round(100*(
                            data.table::frank(
                                ties.method = "average",
                                x = .get_quantiles(extrap[[ord.lab]], model_data[["Y"]][[ord]],
                                    ranked.simex =
                                        ifelse(use.original.ranking.system, "use.original.ranking.system", TRUE)
                                    )
                            ) / n.records)
                        ))
                    ]
                } else {
                    # creates a new "average" rank of the original data and the updated/new cohort
                    tmp.quantiles.simex[[ord]][,
                        SGP_SIMEX_RANKED := head(as.integer(Rfast::Round(100*(
                            data.table::frank(
                                ties.method = "average",
                                x = c(.get_quantiles(extrap[[ord.lab]], model_data[["Y"]][[ord]],
                                        ranked.simex =
                                            ifelse(use.original.ranking.system, "use.original.ranking.system", TRUE)
                                    ),
                                    as.numeric(rep(names(ranked.simex.info), ranked.simex.info))
                                    )
                            ) / (n.records+attr(ranked.simex.info, "n_records"))
                        ), 0)), n.records)
                    ]
                }}
            }
        }   ###   END for (ord in simex.matrix.priors)

		if (verbose) messageSGP(c("\tFinished SIMEX SGP calculation ", rev(content_area.progression)[1L], " Grade ", rev(tmp.gp)[1L], " ", prettyDate()))

		if (is.null(save.matrices)) simex.coef.matrices <- NULL
		if (calculate.simex.sgps) {
			quantile.data.simex <- data.table(rbindlist(tmp.quantiles.simex), key=c("ID", "SIMEX_ORDER"))
        if (convert.0and100) {
            quantile.data.simex[SGP_SIMEX_RANKED %in% c(0L, 100L), SGP_SIMEX_RANKED := fifelse(SGP_SIMEX_RANKED == 0L, 1L, 99L)]
        }
			setkey(quantile.data.simex, ID) # first key on ID and SIMEX_ORDER, then re-key on ID only to insure sorted order. Don't rely on rbindlist/k ordering...
		} else { # set up empty data.table for ddcast and subsets below.
			quantile.data.simex <-
				data.table("ID"=NA, "SIMEX_ORDER"=NA, "SGP_SIMEX"=NA, "SGP_SIMEX_RANKED"=NA)
		}

        if (verbose) { messageSGP(c(
                "\tFinished SIMEX SGP calculation ", rev(content_area.progression)[1L],
                " Grade ", rev(tmp.gp)[1L], " ", prettyDate(), "\n"))
        }
        tmp.rq.meth -> rq.method

		if (print.other.gp) {
            tmp.quantile.data.simex <-
                ddcast(quantile.data.simex,
                    ID ~ SIMEX_ORDER,
                    value.var = setdiff(names(quantile.data.simex), c("ID", "SIMEX_ORDER")),
                    sep = "_ORDER_"
                )
			quantile.data.simex <-
                data.table(tmp.quantile.data.simex,
                    SGP_SIMEX = quantile.data.simex[
                        c(which(!duplicated(quantile.data.simex, by=key(quantile.data.simex)))[-1L]-1L,
                          dim(quantile.data.simex)[1L])
                      ][["SGP_SIMEX"]],
                    SGP_SIMEX_RANKED = quantile.data.simex[
                        c(which(!duplicated(quantile.data.simex, by=key(quantile.data.simex)))[-1L]-1L,
                          dim(quantile.data.simex)[1L])
                      ][["SGP_SIMEX_RANKED"]]
                )
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
	} ###   END simex.sgp function

	############################################################################
	###
	###                    Data Preparation & Checks
	###
	############################################################################

	ID <- tmp.messages <- ORDER <- SCALE_SCORE_PRIOR <- SGP <- PREFERENCE <- TIME <- TIME_LAG <- NULL

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
			tmp.messages <- c(tmp.messages, "\t\tNOTE: `growth.levels` must be supplied as a list or character abbreviation. See help page for details.\n\t\t\t`studentGrowthPercentiles` will be calculated without augmented growth levels.\n")
			tf.growth.levels <- FALSE
		}
		if (is.list(growth.levels)) {
			if (!identical(names(growth.levels), c("my.cuts", "my.levels"))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Please specify an appropriate list for `growth.levels`. See help page for details.\n\t\t\tStudent growth percentiles will be calculated without augmented growth levels.\n")
				tf.growth.levels <- FALSE
			} else {
				tmp.growth.levels <- growth.levels
				tf.growth.levels <- TRUE
			}
		}
		if (is.character(growth.levels)) {
			if (is.null(SGP::SGPstateData[[growth.levels]][["Growth"]][["Levels"]])) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Growth Levels are currently not specified for the indicated state.\n\t\t\tPlease contact the SGP package administrator to have your state's data included in the package.\n\t\t\tStudent growth percentiles will be calculated without augmented growth levels.\n")
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
		if (!(all(sgp.quantiles > 0) & all(sgp.quantiles < 1))) {
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

    sgp.message.label <- sgp.labels[["my.extra.label"]]
    if (!is.null(calculate.simex)) sgp.message.label <- paste("SIMEX", sgp.message.label)

	if (is.null(percuts.digits.internal)) percuts.digits.internal <- 5L


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
				Panel_Data = if (return.panel.data) Panel_Data else NULL,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}

    if (!is.null(SGPt)) {
        setnames(Panel_Data, unlist(SGPt), c("TIME", "TIME_LAG"))
        SGPt_data <- Panel_Data[, c("ID", "TIME", "TIME_LAG"), with = FALSE]
        setattr(SGPt_data, "var.info", unlist(SGPt))
    } else {
        SGPt_data <- NULL
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
				Panel_Data = if (return.panel.data) Panel_Data else NULL,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}

	### Create ss.data

	tmp.last <- tail(tmp.gp, 1L)
	ss.data <- data.table(ss.data[,c(1L, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels)), with=FALSE])
    if (!is.character(ss.data[[1L]])) ss.data[, eval(names(ss.data)[1]) := as.character(get(names(ss.data)[1]))]
    ss.data <- collapse::na_omit(ss.data, cols = ncol(ss.data)) |> setkeyv(names(ss.data)[1L])
	num.panels <- (dim(ss.data)[2L]-1L)/2L
	if (exact.grade.progression.sequence) tmp.num.prior <- num.prior else tmp.num.prior <- 1L
    cohort_data <- .get_cohort_data()

    if (!is.null(sgp.test.cohort.size)) {
        cohort.ids <- cohort_data[["ID"]]
        max.cohort.size <- min(length(cohort.ids), as.numeric(sgp.test.cohort.size))
        if (any(grepl("_DUPS_[0-9]*", ss.data[[1L]]))) {
          dup.ids <- grep("_DUPS_[0-9]*", ss.data[[1L]], value=TRUE)
        } else dup.ids <- NULL
        ss.data <- ss.data[ss.data[[1L]] %in% unique(c(sample(cohort.ids, max.cohort.size), dup.ids))]
        setkeyv(ss.data, names(ss.data)[1L])
    } else max.cohort.size <- nrow(cohort_data)

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
				Panel_Data = if (return.panel.data) Panel_Data else NULL,
				SGPercentiles=SGPercentiles,
				SGProjections=SGProjections,
				Simulated_SGPs=Simulated_SGPs))
	}

	if (is.null(sgp.less.than.sgp.cohort.size.return) && max.cohort.size < sgp.cohort.size) {
		tmp.messages <-
		    paste("\t\tNOTE: Supplied data together with grade progression contains fewer than the minimum cohort size.\n\t\tOnly",
			      max.cohort.size, "valid cases provided with", sgp.cohort.size,
				  "indicated as minimum cohort N size.\n\t\tCheck data, function arguments and see help page for details.\n")
		messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ",
			paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label))
		messageSGP(paste(tmp.messages, "\tStudent Growth Percentile Analysis NOT RUN", prettyDate(), "\n"))

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
		if (!identical(sgp.labels[["my.extra.label"]], "BASELINE")) {
			year.progression <- year.progression.for.norm.group <- tail(rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year_lags.progression))))), length(tmp.gp))
		}
		if (identical(sgp.labels[["my.extra.label"]], "BASELINE")) {
			year.progression <- rep("BASELINE", length(tmp.gp))
			year.progression.for.norm.group <- tail(rev(yearIncrement(sgp.labels[['my.year']], c(0, -cumsum(rev(year_lags.progression))))), length(tmp.gp))
		}
		if (!inherits(year.progression, "character")) {
			stop("year.area.progression should be a character vector. See help page for details.")
		}
		if (!identical(sgp.labels[["my.extra.label"]], "BASELINE") && !identical(tail(year.progression, 1L), sgp.labels[['my.year']])) {
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
        quantile.data <- data.table(
            ID = cohort_data[["ID"]],
            SGP = as.integer(NA),
            SGP_NOTE = sgp.less.than.sgp.cohort.size.return,
            GRADE = as.character(tail(grade.progression, 1))
        )
        if (!is.null(additional.vnames.to.return)) {
            quantile.data <-
                panel.data[["Panel_Data"]][, c("ID", names(additional.vnames.to.return)), with=FALSE
                ][ quantile.data, on = "ID" ]
            setnames(quantile.data, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
        }

        if (return.norm.group.identifier) quantile.data[,SGP_NORM_GROUP:=as.factor(paste(tail(paste(year.progression, paste(content_area.progression, grade.progression, sep="_"), sep="/"), tmp.num.prior+1L), collapse="; "))]
        if (!is.null(return.norm.group.preference)) quantile.data[, PREFERENCE := return.norm.group.preference]
        if (identical(sgp.labels[["my.extra.label"]], "BASELINE")) setnames(quantile.data, "SGP", "SGP_BASELINE")
        if (identical(sgp.labels[["my.extra.label"]], "BASELINE") & "SGP_NORM_GROUP" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE", names(quantile.data)))
        if (identical(sgp.labels[["my.extra.label"]], "EQUATED")) setnames(quantile.data, "SGP", "SGP_EQUATED")
        if (identical(sgp.labels[["my.extra.label"]], "EQUATED") & "SGP_NORM_GROUP" %in% names(quantile.data)) setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_EQUATED", names(quantile.data)))

        SGPercentiles[[tmp.path]] <- rbindlist(list(quantile.data, SGPercentiles[[tmp.path]]), fill=TRUE)

        tmp.messages <-
		    paste("\t\tNOTE: Supplied data together with grade progression contains fewer than the minimum cohort size.\n\t\tOnly",
			      max.cohort.size, "valid cases provided with", sgp.cohort.size,
				  "indicated as minimum cohort N size.\n\t\tCheck data, function arguments and see help page for details.\n")

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

    if (!is.null(sgp.loss.hoss.adjustment) | csem.tf) {
        my.path.knots.boundaries <- get.my.knots.boundaries.path(sgp.labels$my.subject, as.character(sgp.labels$my.year))
        loss.hoss <- eval(parse(text=paste0("Knots_Boundaries", my.path.knots.boundaries, "[['loss.hoss_", tmp.last, "']]")))
    } else loss.hoss <- NULL


        prior.order <- as.character(rev(grade.progression)[2L:(num.prior+1L)])
        model_knots_bounds <- vector(mode = "list", length(prior.order))
        for (f in seq_along(prior.order)) {
            grade <- prior.order[f]
            subj  <- rev(content_area.progression)[f + 1L]
            yr    <- rev(year.progression)[f + 1L]
            l1 <- paste0("Knots_Boundaries", get.my.knots.boundaries.path(subj, yr))
            .check.knots.boundaries(names(eval(parse(text = l1))), grade)

            model_knots_bounds[[f]][[paste0("knots_", grade)]] <-
                eval(parse(text = paste0(l1, "[['knots_", grade, "']]")))
            model_knots_bounds[[f]][[paste0("boundaries_", grade)]] <-
                eval(parse(text = paste0(l1, "[['boundaries_", grade, "']]")))
            names(model_knots_bounds)[[f]] <- paste0(yr, ".", subj)
        }

        model_data <- .get_model_data(
            cohort_data,
            prior.order,
            model_knots_bounds
        )

	### QR Calculations: coefficient matrices are saved/read into/from panel.data[["Coefficient_Matrices"]]
	if (is.null(use.my.coefficient.matrices)) {
		if (exact.grade.progression.sequence) {
			coefficient.matrix.priors <- num.prior
		} else {
			coefficient.matrix.priors <- seq(num.prior)
		}

        if (length(coefficient.matrix.priors) > 1L) {# check cohort N size sufficiency
        for (p in coefficient.matrix.priors[-1]) {
            tmp_data <- collapse::na_omit(cohort_data, cols = grep(paste0("prior_", p), names(cohort_data)))

            if (nrow(tmp_data) < sgp.cohort.size) { # return("Insufficient N")
                tmp.messages <- c(tmp.messages, paste0(
                    "\t\tNOTE: Some grade progressions contain fewer than the minimum cohort size.",
                    "\n\t\tOnly analyses with MAX grade progression ",
                        paste(rev(rev(tmp.gp)[seq.int(p)]), collapse = ', '),
                    " will be produced given N = ", prettyNum(sgp.cohort.size, big.mark=",", scientific=FALSE),
                    " indicated as minimum cohort size.",
                    "\n\t\tCheck data, function arguments and see help page for details.\n"))
                ##  trim progressions accordingly
                grade.progression <- tmp.gp <- rev(rev(tmp.gp)[seq.int(p)])
                gp.len <- length(grade.progression)
                if (!is.null(year.progression) && length(year.progression) > gp.len) {
                    year.progression <- tail(year.progression, gp.len)
                }
                if (!is.null(year_lags.progression) && length(year_lags.progression) > gp.len-1L) {
                    year_lags.progression <- tail(year_lags.progression, gp.len-1L)
                }
                if (!is.null(content_area.progression) && length(content_area.progression) > gp.len) {
                    content_area.progression <- tail(content_area.progression, gp.len)
                }
                if (!is.null(grade.progression.for.norm.group) && length(grade.progression.for.norm.group) > gp.len) {
                    grade.progression.for.norm.group <- tail(grade.progression.for.norm.group, gp.len)
                }
                if (!is.null(year.progression.for.norm.group) && length(year.progression.for.norm.group) > gp.len) {
                    year.progression.for.norm.group <- tail(year.progression.for.norm.group, gp.len)
                }
                coefficient.matrix.priors <- setdiff(coefficient.matrix.priors, p)
                cohort_data <- .get_cohort_data(n.priors = p)
                break
            }
        }}

        Coefficient_Matrices[[tmp.path.coefficient.matrices]] <-
            .create_coefficient_matrices(
                model_data,
                par.config = parallel.config
            )

            err.tests <- lapply(
                names(Coefficient_Matrices[[tmp.path.coefficient.matrices]]),
                \(mat.name) {
                    if (identical(mat.name, "RQ_ERROR")) {
                        tmp.err.message <- Coefficient_Matrices[[tmp.path.coefficient.matrices]][[mat.name]][['RQ_ERROR']]
                        tmp.messages <- c(tmp.messages, paste0(
                            "\t\tNOTE: An Error in the quantile regression (coefficient matrix creation) has occurred",
                            "\n\t\tin grade progression ", paste(rev(rev(tmp.gp)), collapse = ', '),
                            " and produced the following error message: \n\t\t\t\"", tmp.err.message, "\""))
                        messageSGP(paste("\tStarted studentGrowthPercentiles", started.date))
                        messageSGP(paste0(
                            "\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year,
                            ", Grade Progression: ", paste(tmp.slot.gp, collapse=", "), " ", sgp.message.label))
                        messageSGP(paste(tmp.messages,
                            "\n\t\tStudent Growth Percentile Analysis NOT RUN", prettyDate(), "\n"))
                        Coefficient_Matrices[[tmp.path.coefficient.matrices]][[mat.name]] <- NULL
                        return(TRUE)
                    } else return(FALSE)
                }
            )
            if (any(unlist(err.tests))) {
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

        if (verbose.output) {
            tmp.coefficient.matrix.name <- get.coefficient.matrix.name(tmp.last, coefficient.matrix.priors)
            for (ord in seq_along(tmp.coefficient.matrix.name)) {    
                tmp.grade.names <- paste("Grade",
                    rev(head(unlist(Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name[ord]]]@Grade_Progression), -1L)))
                Verbose_Messages <- paste0("\t\tNOTE: Coefficient Matrix ", tmp.coefficient.matrix.name[ord], " created using:")
                for (l in seq_along(tmp.grade.names)) {
                    tmp.knots <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name[ord]]]@Knots[l])
                    tmp.boundaries <- paste(tmp.grade.names[l], Coefficient_Matrices[[tmp.path.coefficient.matrices]][[tmp.coefficient.matrix.name[ord]]]@Boundaries[l])
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
			use.original.ranking.system=calculate.simex$use.original.ranking.system,
			set.seed.for.sim.data=calculate.simex$set.seed.for.sim.data,
			verbose=calculate.simex$verbose)

		if (!is.null(quantile.data.simex[['MATRICES']])) {
            simex.coef.matrices.path <- paste0(tmp.path.coefficient.matrices, ".SIMEX")
			tmp_sgp_1 <- list(Coefficient_Matrices = list(TMP_SIMEX=Coefficient_Matrices[[simex.coef.matrices.path]]))
			tmp_sgp_2 <- list(Coefficient_Matrices = list(TMP_SIMEX=quantile.data.simex[['MATRICES']]))
			tmp_sgp_combined <- mergeSGP(tmp_sgp_1, tmp_sgp_2)
			Coefficient_Matrices[[simex.coef.matrices.path]] <- tmp_sgp_combined[["Coefficient_Matrices"]][["TMP_SIMEX"]]
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

		for (ord in seq_along(tmp.orders)) {
			if (length(model_data[["Y"]][[ord]])) {
                tmp.matrix <- tmp.matrices[[ord]]
                tmp.predictions <- .get_percentile_predictions(model_data[["X"]][[ord]], tmp.matrix)
				tmp.ids <- rownames(model_data[["X"]][[ord]])
                tmp.quantiles[[ord]] <- data.table(
                    ID = tmp.ids,
                    ORDER = tmp.orders[ord],
                    SGP = .get_quantiles(tmp.predictions, model_data[["Y"]][[ord]])
                )

				if (csem.tf) {
					if (is.null(calculate.confidence.intervals[['simulation.iterations']])) calculate.confidence.intervals[['simulation.iterations']] <- 100L
					if (!is.null(calculate.confidence.intervals[['variable']])) {
						tmp.csem.variable <- Panel_Data[Panel_Data[[1L]] %in% ss.data[list(tmp.ids)][[1L]]][[calculate.confidence.intervals[['variable']]]]
					} else {
						tmp.csem.variable <- NULL
					}

                    tmp.id.etc <- setnames(qDT(tmp.ids), "ID")
                    if (!is.null(additional.vnames.to.return)) {
						if (!is.character(Panel_Data[, ID])) Panel_Data[, ID := as.character(ID)]
                        	tmp.id.etc <-
                            Panel_Data[, c("ID", names(additional.vnames.to.return)), with=FALSE
                            ][ tmp.id.etc, on="ID" ]
                        setnames(tmp.id.etc, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
                    }

                    tmp.csem.quantiles[[ord]] <- data.table(
                            tmp.id.etc,
                            apply(
                                X = csemScoreSimulator(
                                      scale_scores = model_data[["Y"]][[ord]],
                                      grade = tmp.last,
                                      content_area = sgp.labels[["my.subject"]],
                                      year = sgp.labels[["my.year"]],
                                      loss.hoss = loss.hoss,
                                      state = calculate.confidence.intervals[["state"]],
                                      variable = tmp.csem.variable,
                                      iterations = calculate.confidence.intervals[["simulation.iterations"]],
                                      sgp.sim.seed = sgp.percentiles.set.seed),
                                MARGIN = 2,
                                FUN = function(x) .get_quantiles(tmp.predictions, x)
                            )
                        )
                    setnames(tmp.csem.quantiles[[ord]],
                        paste0("V", seq(calculate.confidence.intervals[["simulation.iterations"]])),
                        paste("SGP_SIM", seq(calculate.confidence.intervals[["simulation.iterations"]]), sep = "_"))
				} ## END CSEM analysis

                if (!is.null(percentile.cuts)) {
                    tmp.percentile.cuts[[paste("ORDER", ord, sep = "_")]] <-
                        data.table(ID = tmp.ids, .get.percentile.cuts(tmp.predictions))

                    if (!is.null(SGPt.max.time)) {
                        tmp.percentile.cuts[[paste("ORDER", ord, "MAX_TIME", sep = "_")]] <-
                            data.table(ID = tmp.ids,
                                .get.percentile.cuts(
                                    .get_percentile_predictions(model_data[["X"]][[ord]], tmp.matrix, SGPt.max.time)))
                    }
                }
			} ### END if (length(model_data[["Y"]][[ord]]))
		} ## END ord loop

        if ((is.character(goodness.of.fit) || goodness.of.fit==TRUE || return.prior.scale.score)) {
            prior.ss <- cohort_data[["prior_1"]]
        } else if (exact.grade.progression.sequence && return.prior.scale.score) {
            prior.ss <- cohort_data[["prior_1"]]
        }

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
			quantile.data[,
                SCALE_SCORE_PRIOR_STANDARDIZED := Rfast::Round(as.numeric(collapse::fscale(prior.ss)), digit = 3L)]
		}

		if (tf.growth.levels) {
			SGP_LEVEL <- NULL
			quantile.data[, SGP_LEVEL:=factor(findInterval(quantile.data[["SGP"]], tmp.growth.levels[["my.cuts"]]),
				levels=seq(length(tmp.growth.levels[["my.levels"]]))-1L, ## Necessary in case the full range of SGPs isn't present
				labels=tmp.growth.levels[["my.levels"]], ordered=TRUE)]
		}

        if (csem.tf) {
            simulation.data <- data.table(rbindlist(tmp.csem.quantiles), key="ID")
            simulation.data <-
                simulation.data[c(which(!duplicated(simulation.data, by=key(simulation.data)))[-1L]-1L, dim(simulation.data)[1L])]

            if (is.character(calculate.confidence.intervals) || is.list(calculate.confidence.intervals)) {
                if (is.null(calculate.confidence.intervals$confidence.quantiles) ||
                    identical(toupper(calculate.confidence.intervals$confidence.quantiles), "STANDARD_ERROR")
                ) {
                    if (print.other.gp) {
                        tmp.se <- list()
                        for (f in seq_along(tmp.csem.quantiles)) {
                            tmp.se[[f]] <- data.table(
                                "ID" = tmp.csem.quantiles[[f]][["ID"]],
                                "ORDER" = f,
                                "STANDARD_ERROR" = Rfast::Round(
                                    Rfast::rowVars(collapse::qM(tmp.csem.quantiles[[f]][, !"ID"]), std = TRUE), digit = 2L))
                        }
                        tmp.se <- data.table(rbindlist(tmp.se), key = "ID")
                        tmp.se <- dcast(tmp.se, ID ~ ORDER, value.var = "STANDARD_ERROR")
                        setnames(tmp.se,
                            grep("ID", names(tmp.se), invert = TRUE, value = TRUE),
                            paste0("SGP_ORDER_", grep("ID", names(tmp.se), invert = TRUE, value = TRUE), "_STANDARD_ERROR")
                        )
                        quantile.data <- merge(quantile.data, tmp.se, by = "ID")
                    } # No 'else' - also return the plain SGP version
                    for (f in seq_along(tmp.csem.quantiles)) {
                        quantile.data[,
                            SGP_STANDARD_ERROR := Rfast::Round(
                                Rfast::rowVars(collapse::qM(simulation.data[, !"ID"]), std = TRUE), digit = 2L)
                        ]
                    }
                } else {
                    if (!(is.numeric(calculate.confidence.intervals$confidence.quantiles) &&
                        all(calculate.confidence.intervals$confidence.quantiles < 1) &
                        all(calculate.confidence.intervals$confidence.quantiles > 0))
                    ) {
                        stop("Argument to 'calculate.confidence.intervals$confidence.quantiles' must be numeric and consist of quantiles.")
                    }
                    if (print.other.gp) {
                        tmp.se <- list()
                        for (f in seq_along(tmp.csem.quantiles)) {
                            tmp.se[[f]] <- data.table(
                                "ID" = tmp.csem.quantiles[[f]][["ID"]],
                                "ORDER" = f,
                                "CI" = Rfast::Round(t(apply(
                                    tmp.csem.quantiles[[f]][, !"ID"], 1,
                                    FUN = collapse::.quantile, probs = calculate.confidence.intervals$confidence.quantiles
                                ))),
                                "STANDARD_ERROR" = Rfast::Round(
                                    Rfast::rowVars(collapse::qM(tmp.csem.quantiles[[f]][, !"ID"]), std = TRUE), digit = 2L)
                            )
                        }
                        tmp.se <- data.table(rbindlist(tmp.se), key="ID")
                        tmp.se <- dcast(tmp.se, ID~ORDER, value.var=c("STANDARD_ERROR", "CI.V1", "CI.V2"))
                        setnames(tmp.se,
                            c(grep("STANDARD_ERROR", names(tmp.se), value=TRUE), grep("CI.V", names(tmp.se), value=TRUE)),
                            c(paste0("SGP_ORDER_", seq_along(tmp.csem.quantiles), "_STANDARD_ERROR"), paste0("SGP_ORDER_", as.vector(outer(seq_along(tmp.csem.quantiles), calculate.confidence.intervals$confidence.quantiles, paste, sep="_")), "_CONFIDENCE_BOUND")))
                        quantile.data <- merge(quantile.data, tmp.se, by="ID")
                    } # No 'else' - also return the plain SGP version
                    tmp.cq <- collapse::qDT(
                        Rfast::Round(
                            t(apply(simulation.data[, !"ID"], 1,
                                collapse::.quantile, probs = calculate.confidence.intervals$confidence.quantiles)))
                    )
                    quantile.data[,
                        paste0("SGP_", calculate.confidence.intervals$confidence.quantiles, "_CONFIDENCE_BOUND") := tmp.cq
                    ][, SGP_STANDARD_ERROR := Rfast::Round(
                            Rfast::rowVars(collapse::qM(simulation.data[, !"ID"]), std = TRUE), digit = 2L)
                    ]
                }
            }
            simulation.data[, GRADE := as.character(tail(grade.progression, 1))]
            Simulated_SGPs[[tmp.path]] <- rbindlist(list(simulation.data, Simulated_SGPs[[tmp.path]]), fill=TRUE)
        }

		if (simex.tf) {
			if (print.other.gp) {
				quantile.data <- quantile.data[quantile.data.simex[["DT"]]]
            } else {
                quantile.data[,
                    SGP_SIMEX := quantile.data.simex[['DT']][["SGP_SIMEX"]]
                ][, SGP_SIMEX_RANKED := quantile.data.simex[['DT']][["SGP_SIMEX_RANKED"]]]
            }
		}

		if (!is.null(percentile.cuts)) {
            quantile.data <- collapse::qDT(
                quantile.data,
                .get.best.cuts(tmp.percentile.cuts[grep("MAX_TIME", names(tmp.percentile.cuts), invert=TRUE)])
            )
            if (!is.null(SGPt.max.time)) {
                quantile.data <- collapse::qDT(
                    quantile.data,
                    .get.best.cuts(tmp.percentile.cuts[grep("MAX_TIME", names(tmp.percentile.cuts))], "MAX_TIME"))
            }
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
			my.tmp <- Panel_Data[,c("ID", setdiff(grep("TIME", names(Panel_Data), value=TRUE), grep("TIME_LAG", names(Panel_Data), value=TRUE))), with=FALSE][list(quantile.data[["ID"]]),-1L,with=FALSE,on="ID"]
            my.tmp <- my.tmp[,tail(seq(dim(my.tmp)[2L]), length(tmp.gp)),with=FALSE]
			quantile.data[,SGP_NORM_GROUP_DATES:=gsub("NA; ", "", do.call(paste, c(as.data.table(lapply(my.tmp, function(x) as.Date(x, origin="1970-01-01"))), list(sep="; "))))]
		}

		if (!is.null(return.norm.group.scale.scores)) {
            tmp.scale_scores <- collapse::ss(
                cohort_data, j = grep("prior_|final_yr", names(cohort_data)), check = FALSE)
            quantile.data[,
                SGP_NORM_GROUP_SCALE_SCORES :=
                    gsub("NA; ", "", do.call(paste, c(tmp.scale_scores, list(sep="; "))))]
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
				tmp.gof.data <- ss.data[, c("ID", "SCALE_SCORE"), with=FALSE][tmp.gof.data, on = "ID"]

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

		if (identical(sgp.labels[["my.extra.label"]], "BASELINE")) {
			setnames(quantile.data, "SGP", "SGP_BASELINE")
			if (tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_BASELINE")
			if ("SGP_STANDARD_ERROR" %in% names(quantile.data)) {
				setnames(quantile.data, gsub("SGP_STANDARD_ERROR", "SGP_BASELINE_STANDARD_ERROR", names(quantile.data)))
			}
			if (any(grepl("CONFIDENCE_BOUND", names(quantile.data)))) { #Needs to be before 'SGP_ORDER' check
				setnames(quantile.data,
					grep("CONFIDENCE_BOUND", names(quantile.data), value = TRUE),
					gsub("SGP_", "SGP_BASELINE_", grep("CONFIDENCE_BOUND", names(quantile.data), value = T))
				)
			}
			if (any(c("SGP_ORDER", "SGP_ORDER_1") %in% names(quantile.data))) {
				setnames(quantile.data, gsub("SGP_ORDER", "SGP_BASELINE_ORDER", names(quantile.data)))
			}
			if ("SGP_NORM_GROUP" %in% names(quantile.data)) {
				setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE", names(quantile.data)))
			}
			if (simex.tf) {
				setnames(quantile.data, gsub("_SIMEX", "_SIMEX_BASELINE", names(quantile.data))) # SGP_SIMEX and SGP_SIMEX_RANKED
			}
			if (return.prior.scale.score) {
				setnames(quantile.data, "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_BASELINE")
			}
			if (return.prior.scale.score.standardized) {
				setnames(quantile.data, "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE")
			}
			if (!is.null(percentile.cuts)) {
				setnames(quantile.data, gsub("PERCENTILE_CUT_", "PERCENTILE_CUT_BASELINE_", names(quantile.data)))
			}
		}

		if (identical(sgp.labels[["my.extra.label"]], "EQUATED")) {
			setnames(quantile.data, "SGP", "SGP_EQUATED")
			if (tf.growth.levels) setnames(quantile.data, "SGP_LEVEL", "SGP_LEVEL_EQUATED")
			if ("SGP_NORM_GROUP" %in% names(quantile.data)) {
				setnames(quantile.data, gsub("SGP_NORM_GROUP", "SGP_NORM_GROUP_EQUATED", names(quantile.data)))
			}
		}

		if (!is.null(additional.vnames.to.return)) {
			quantile.data <- panel.data[["Panel_Data"]][,c("ID", names(additional.vnames.to.return)), with=FALSE][quantile.data, on="ID"]
			setnames(quantile.data, names(additional.vnames.to.return), unlist(additional.vnames.to.return))
		}

		if (!return.prior.scale.score) {
			quantile.data[,SCALE_SCORE_PRIOR := NULL]
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
