`sgpSummary` <-
    function(
        long_data,
        sgp.groups.to.summarize,
        produce.confidence.interval,
        tmp_simulation_dt,
        sgp.summaries,
        confidence.interval.groups,
        sgp_key
    ) {

    WEIGHT <- MEDIAN_SGP_with_SHRINKAGE <- MEDIAN_SGP_COUNT <- NULL

    tmp.sgp.summaries <- sgp.summaries
    by.groups  <- unlist(strsplit(sgp.groups.to.summarize, ", "))
    smry.names <- gsub("^[^.]*.", "", names(unlist(sgp.summaries)))
    pct.names  <- grep("^PERCENT_AT_ABOVE|^PERCENT_CATCHING_UP|^PERCENT_MOVING_UP", smry.names, value = TRUE)
    MnSE.names <- grep("^MEAN_SGP.*.STANDARD_ERROR", smry.names, value = TRUE)

    tmp_res <-
        collapse::collapv(
            X = long_data,
            by = by.groups,
            custom = tmp.sgp.summaries,
            w = if ("WEIGHT" %in% names(long_data)) "WEIGHT" else NULL,
            keep.w = FALSE,
            give.names = FALSE
            # parallel = TRUE # way slower (at least with DEMO data size)
        ) |>
          collapse::replace_outliers(c(-2147483647, 2147483647)) |> # recode_num(`-2147483648` = NA, `2147483648` = NA) |>
            collapse::ftransformv(pct.names, `*`, 100, apply = FALSE) |>
              collapse::ftransformv(pct.names, round, 1L, apply = FALSE) |>
                collapse::ftransformv(MnSE.names, `/`, sqrt(MEDIAN_SGP_COUNT), apply = FALSE)

    tmp_res <-
      collapse::add_vars(tmp_res, collapse::get_vars(tmp_res, MnSE.names) |> (`*`)(1.253) |>
        collapse::rm_stub("MEAN_") |> collapse::add_stub("MEDIAN_")) |>
          collapse::ftransformv(c(MnSE.names, gsub("^MEAN_", "MEDIAN_", MnSE.names)), round, 2L, apply = FALSE)

    if (produce.confidence.interval & "CSEM" %in% confidence.interval.groups[["TYPE"]]) {
      se_csem <-
        collapse::collapv(
            X = collapse::join(
                    x = collapse::get_vars(long_data, vars = unique(c(sgp_key, by.groups))),
                    y = tmp_simulation_dt,
                    on = sgp_key, how = "right", drop.dup.cols = TRUE, verbose = 0
                ),
            by = c(by.groups, "NRM_REF_TYPE", "SIM_NUM"),
            custom = list(fmedian = c(MEDN = "SGP_SIM"), fmean = c(MEAN = "SGP_SIM")),
            give.names = FALSE
        ) |>
            collapse::collapv(
                by = c(by.groups, "NRM_REF_TYPE"),
                custom = list(fsd = c(MEDN_SE = "MEDN", MEAN_SE = "MEAN")),
                give.names = FALSE
            ) |>
                collapse::pivot(
                    ids = by.groups,
                    values = c("MEDN_SE", "MEAN_SE"),
                    names = "NRM_REF_TYPE",
                    how = "wider",  na.rm = TRUE, transpose = TRUE)
        if (!any(grepl("BASELINE", names(se_csem)))) {
            setnames(
                se_csem,
                c("COHORT_MEDN_SE", "COHORT_MEAN_SE"),
                c("MEDIAN_SGP_STANDARD_ERROR_CSEM", "MEAN_SGP_STANDARD_ERROR_CSEM")
            )
        } else {
            setnames(
                se_csem,
                c("COHORT_MEDN_SE", "COHORT_MEAN_SE", "BASELINE_MEDN_SE", "BASELINE_MEAN_SE"),
                c("MEDIAN_SGP_STANDARD_ERROR_CSEM", "MEAN_SGP_STANDARD_ERROR_CSEM",
                  "MEDIAN_SGP_BASELINE_STANDARD_ERROR_CSEM", "MEAN_SGP_BASELINE_STANDARD_ERROR_CSEM")
            )
        }

        tmp_res <-
            collapse::join(
                x = tmp_res,
                y = se_csem,
                on = by.groups, how = "left",
                drop.dup.cols = TRUE, verbose = 0
            )
        setcolorder(
            tmp_res,
            c(grep("CSEM", names(tmp_res), invert=TRUE), grep("CSEM", names(tmp_res)))
        )
    }

    if ("MEDIAN_SGP_STANDARD_ERROR" %in% names(tmp_res)) {
        constant <-
            collapse::fvar(tmp_res[["MEDIAN_SGP"]]) -
            collapse::fmean(tmp_res[["MEDIAN_SGP_STANDARD_ERROR"]]^2)
        tmp_res[,
            MEDIAN_SGP_with_SHRINKAGE :=
              round(50 +
                ((tmp_res[["MEDIAN_SGP"]]-50) *
                 (constant/
                 (constant + tmp_res[["MEDIAN_SGP_STANDARD_ERROR"]]^2)
                 ))
              )
        ]
    }

    if (produce.confidence.interval) {
        tmp.list <- list()
        if ("Bootstrap_CI" %in% confidence.interval.groups[["TYPE"]]) {
            tmp.quantiles <-
              paste0("c(", paste(confidence.interval.groups[["QUANTILES"]], collapse=", "), ")")
            for (i in confidence.interval.groups[["VARIABLES"]]) {
                i.nm <-
                  paste("MEDIAN", i, paste(confidence.interval.groups[["QUANTILES"]], collapse="_"),
                        "CONFIDENCE_BOUND_BOOTSTRAP", sep="_")
                tmp.list[[i.nm]] <-
                  paste0(i.nm, " = boot.sgp(", i, ", ", tmp.quantiles, ")")
            }
        }
        if ("Bootstrap_SE" %in% confidence.interval.groups[["TYPE"]]) {
            for (i in confidence.interval.groups[["VARIABLES"]]) {
                i.nm <-
                  paste("MEDIAN", i, "STANDARD_ERROR_BOOTSTRAP", sep="_")
                tmp.list[[i.nm]] <- paste0(i.nm, " = boot.sgp(", i, ")")
            }
        }

        ListExpr <-
          parse(text=paste0("list(", paste(unlist(tmp.list), collapse=", "),")"))
        tmp_idx <-
            tmp_res[MEDIAN_SGP_COUNT > 4, by.groups, with = FALSE] |> setkey()
        setkeyv(tmp_res, by.groups)
        setkeyv(long_data, by.groups)

        tmp_res <-
            long_data[tmp_idx
            ][, as.list(
                  longBoot(
                      data_table = .SD,
                      vars = confidence.interval.groups[["VARIABLES"]],
                      conf.quantiles = confidence.interval.groups[["QUANTILES"]]
                  )
                ),
                .SDcols = confidence.interval.groups[["VARIABLES"]],
                keyby = by.groups
            ][tmp_res]
    }

    messageSGP(paste("\tFinished with", paste(by.groups, collapse = ", ")))
    return(tmp_res)
} ### END sgpSummary function

`longBoot` <-
  function(
    data_table,
    vars,
    conf.quantiles,
    nboot = 100L,
    digts = 2L
  ) {
    data_table <- na_omit(data_table)
    N <- fnrow(data_table)
    CI <- rep(paste0("[", paste(rep(NA, 2L), collapse=", "), "]"), length(vars))
    SE <- rep(NA_real_, length(vars))
    if (N > 1L) {
      tst <-
        data_table |>
          collapse::roworderv(neworder = sample.int(n = N, size = N*nboot, replace = TRUE)) |>
            collapse::ftransform(BOOT = rep(seq.int(nboot), N)) |>
              collapse::collapv(by = "BOOT", FUN = fmedian, sort = FALSE, keep.by = FALSE)

      CI <- paste0(
              "[", 
              sapply(tst, .quantile, probs = conf.quantiles) |> round(digts) |>
                apply(MARGIN = 2, paste, collapse=", "),
              "]")
      SE <- lapply(tst, fsd) |> unlist() |> round(digits = digts)
    }

    setnames(
      data.table(cbind(rbind(CI), rbind(SE))),
      c(paste("MEDIAN", vars, paste(conf.quantiles, collapse="_"),
            "CONFIDENCE_BOUND_BOOTSTRAP", sep="_"),
        paste("MEDIAN", vars, "STANDARD_ERROR_BOOTSTRAP", sep="_"))
      )[]
  }
