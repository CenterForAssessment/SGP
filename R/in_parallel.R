#' Parallelization in SGP
#'
#' [in_parallel()] is a simple wrapper of [carrier::crate()], cribbed from the
#' `purrr` package. You may refer to those packages for more details.

in_parallel <- function(.f, ...) {
    inject(
        carrier::crate(
          !!substitute(.f),
          !!!list(...),
          .parent_env = globalenv(),
          .error_arg = ".f",
          .error_call = environment()
        )
    )
}

sync_compute <- function(arg_list, FUN) {
        lapply(arg_list, \(arg_set) {  do.call(FUN, arg_set)  })
}

##  Register in-memory SGPstateData[[state]] for mirai workers. studentGrowthProjections
##  and related functions read cutscores and configuration from SGP::SGPstateData; mirai
##  daemons start fresh and only see packaged data unless this snapshot is pushed.
set_sgp_parallel_state_data <- function(state = NULL) {
    if (is.null(state)) {
        options(sgp.parallel.state = NULL, sgp.parallel.state.data = NULL)
    } else {
        options(
            sgp.parallel.state = state,
            sgp.parallel.state.data = SGP::SGPstateData[[state]]
        )
    }
    invisible(NULL)
}

sync_sgp_state_data_mirai <- function() {
    sgp_state <- getOption("sgp.parallel.state")
    sgp_state_data <- getOption("sgp.parallel.state.data")
    if (is.null(sgp_state) || is.null(sgp_state_data)) return(invisible(NULL))

    mirai::everywhere(
        {
            suppressPackageStartupMessages(require(SGP))
            sd <- SGP::SGPstateData
            sd[[sgp_state]] <- sgp_state_data
            assignInNamespace("SGPstateData", sd, ns = "SGP")
        },
        sgp_state = sgp_state,
        sgp_state_data = sgp_state_data
    )
    invisible(NULL)
}

mirai_compute <- function(arg_list, FUN) {
        mirai::daemons(
            n = getOption("sgp.workers", 2L), dispatch = FALSE, output = TRUE
        )
        mirai::everywhere({
            data.table::setDTthreads(threads = 1)
        })
        sync_sgp_state_data_mirai()
        on.exit(mirai::daemons(0))

        mirai::mirai_map(
            arg_list,
            in_parallel(\(arg_set, y) {
                tryCatch(
                    expr = do.call(FUN, arg_set),
                    error = function(e) {
                        err <- paste(
                            "Error in `mirai` parallel processing:",
                            conditionMessage(e), sep = "\n\t")
                        class(err) <- "sgp-error"
                        err
                    }
                )}
            ),
            FUN = FUN
        ) |> mirai::collect_mirai()
}

fork_compute <- function(arg_list, FUN) {
        parallel::mclapply(
            X = arg_list,
            \(arg_set) {  do.call(FUN, arg_set)  },
            mc.cores = getOption("sgp.workers", 2L),
            mc.preschedule = getOption("mc.preschedule", FALSE)
        )
}

##    Define parallel processing backend and workers
define_compute <- function(parallel.config, process) {
    par.backend <- parallel.config[["BACKEND"]]
    tmp.workers <- parallel.config[["WORKERS"]][[process]]

    if (is.null(tmp.workers) || (tmp.workers < 2)) return(sync_compute)
    if (is.null(par.backend) && is.null(tmp.workers)) return(sync_compute)
    if (is.null(par.backend)) par.backend <- ""
    options(sgp.workers = tmp.workers)

    switch(
        par.backend,
        MIRAI = mirai_compute,
        FORK = fork_compute,
        MULTICORE = fork_compute,
        ifelse(.Platform$OS.type == "unix", fork_compute, mirai_compute)
    )
}

    chunk_taus <- function(cohort_data, qr.taus, workers, rq.method) {
        if (workers > 3) {
            if (workers %in% 4:10) {
                tmp.sml <- ceiling((length(qr.taus) / workers)*0.75)
                tmp.lrg <- ceiling((length(qr.taus)-(2*tmp.sml))/(workers-2))
                chunk.size <- c(tmp.sml, rep(tmp.lrg, (workers-2)), tmp.sml)
                if (sum(chunk.size) > length(qr.taus)) {
                    over <- (sum(chunk.size) - length(qr.taus)); index <- 0
                    while(over != 0) {
                        if (over %% 2 == 0) {
                            index <- index + 1
                            chunk.size[(length(chunk.size)-(index))] <- chunk.size[(length(chunk.size)-(index))]-1
                        } else chunk.size[(index + 1)] <- chunk.size[(index + 1)]-1
                        over <- over - 1
                    }
                }
            }
            if (workers > 10) {
                tmp.sml.a <- ceiling((length(qr.taus) / workers)*0.334)
                tmp.sml.b <- ceiling((length(qr.taus) / workers)*0.666)
                tmp.lrg <- ceiling((length(qr.taus)-(2*sum(tmp.sml.a, tmp.sml.b)))/(workers-4))
                chunk.size <- c(tmp.sml.a, tmp.sml.b, rep(tmp.lrg, (workers-4)), tmp.sml.b, tmp.sml.a)
                if (sum(chunk.size) > length(qr.taus)) {
                    over <- (sum(chunk.size) - length(qr.taus)); index <- 0
                    while(over != 0) {
                        if (over %% 2 != 0) {
                            index <- index + 1
                            chunk.size[(length(chunk.size)-(index + 1))] <- chunk.size[(length(chunk.size)-(index + 1))]-1
                        } else chunk.size[(index + 2)] <- chunk.size[(index + 2)]-1
                        over <- over -1
                    }
                }
            }
            if (workers > length(qr.taus)) chunk.size <- rep(1, length(qr.taus))
        } else chunk.size <- rep(ceiling(length(qr.taus) / workers), workers)

        TAUS.LIST <- vector(mode = "list", length = workers)
        count <- index <- 1
        for (ch in chunk.size) {
            TAUS.LIST[[index]] <- qr.taus[count:(count+ch-1)]
            count <- (count+ch); index <- index + 1
        }
        if (sum(chunk.size) > length(qr.taus)) {
            for (l in 1:length(TAUS.LIST)) {
                TAUS.LIST[[l]] <- TAUS.LIST[[l]][!is.na(TAUS.LIST[[l]])]
            }
        }

        ret.list <- vector(mode = "list", length = length(TAUS.LIST))
        for (l in 1:length(TAUS.LIST)) {
            ret.list[[l]] <- c(cohort_data, taus = list(TAUS.LIST[[l]]), method = rq.method)
        }
        return(ret.list)
    }
