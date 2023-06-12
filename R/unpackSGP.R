`unpackSGP` =
    function(
        sgp_object,
        slot.type = NULL,
        partitions = "YEAR",
        write.combined.results = FALSE,
        return.object = TRUE,
        base.directory = file.path("Data", "Arrow_SGP")
    ) {
        ###  Utility functions
        `%w/o%` = function(x, y) x[!x %in% y]

        fletch =
            function(data_table, output.dir = ".",
                     partitions = partitions,
                     ...
            ) {
                if (!"ArrowObject" %in% class(data_table)) {
                    data_table <- arrow::arrow_table(data_table)
                }
                arrow::write_dataset(
                    dataset = data_table,
                    path = output.dir,
                    partitioning = partitions,
                    compression = "gzip",
                    ...
                )
            }

        # getTableNameYear <- SGP:::getTableNameYear
        # getPercentileTableNames <- SGP:::getPercentileTableNames

        if (!is.SGP(sgp_object)) {
            if ("data.frame" %in% class(sgp_object)) {
                message(
                    "\n\n\t\t\tData Table like object provided for `sgp_object`,
                        we assume this is the LONGITUDINAL data for the @Data slot.\n
                        If not, please provide the data in a named list
                        labeled according to the desired slot.")

                sgp_object <- new("SGP", Data = sgp_object)
                slot.type <- "Data"
            } else {
                if (is.list(sgp_object)) {
                    if (is.null(sgp_object[["Data"]])) {
                        sgp_object[["Data"]] <- data.table()
                    }
                    sgp_object_list <- sgp_object
                    sgp_object <-
                        new("SGP",
                            Data = sgp_object_list[["Data"]],
                            SGP = NULL,
                            Data_Supplementary = sgp_object_list[["Data_Supplementary"]],
                            Summary = sgp_object_list[["Summary"]],
                            Names = NULL, Version = NULL
                        )

                    for(nm in names(sgp_object_list) %w/o% "Data") {
                        sgp_object@SGP[[nm]] <- sgp_object_list[[nm]]
                    }
                } else { 
                    stop(
                        "\n\t\t\tThe `sgp.object` provided must be a:
                          1) SGP class object,
                          2) named list labeled according to SGP slots, or
                          3) a `data.frame`-like object.")
                }
            }
        }

        if (is.null(slot.type)) {
            slot.type <-
                c("Data", "Data_Supplementary", "Summary",
                  "Coefficient_Matrices", "Goodness_of_Fit",
                  "SGPercentiles", "SGProjections", "Simulated_SGPs")
        }

        if (!is.null(sgp_object@Data) &&
            "Data" %in% slot.type &&
            is.data.table(sgp_object@Data)
        ) {
            if (write.combined.results) {
                output.names <- names(sgp_object@Data)
            } else {
                if (!is.null(sgp_object@Names)) {
                    output.names <- sgp_object@Names[["names.sgp"]]
                } else {
                    res.names <-
                        "SGP|PERCENTILE_CUT|BASELINE|MOVE_UP_STAY_UP|CATCH_UP_KEEP_UP_STATUS|_PRIOR"
                    output.names <-
                        grep(res.names, names(sgp_object@Data), invert = TRUE, value = TRUE)
                }
            }

            fletch(
                data_table = sgp_object@Data[, ..output.names],
                output.dir = file.path(base.directory, "LONG"),
                partitions = partitions,
                existing_data_behavior = "overwrite"
            )
            if (return.object) {
                sgp_object@Data <-
                    ArrowSGP$new(
                        source_path = file.path(base.directory, "LONG")
                    )
            }
        }

        if (!is.null(sgp_object@Data_Supplementary) && "Data_Supplementary" %in% slot.type) {
            for (nm in names(sgp_object@Data_Supplementary)) {
                if (is(sgp_object@Data_Supplementary[[nm]], "ArrowSGP")) next
                tmp.hive <-
                    intersect(
                        c("YEAR", "CONTENT_AREA"),
                        names(sgp_object@Data_Supplementary[[nm]])
                    )
                if (!length(tmp.hive)) tmp.hive <- NULL
                fletch(
                    data_table = sgp_object@Data_Supplementary[[nm]],
                    output.dir = file.path(base.directory, "Supplementary", nm),
                    partitions = tmp.hive,
                    existing_data_behavior = "overwrite"
                )
                if (return.object) {
                    sgp_object@Data_Supplementary[[nm]] <-
                        ArrowSGP$new(
                            source_path = file.path(base.directory, "Supplementary", nm)
                        )
                }
            }
        }

        if (!is.null(sgp_object@Summary) && "Summary" %in% slot.type) {
            for (nm in names(sgp_object@Summary)) {
                tmp.dir <- file.path(base.directory, "Summary", nm)
                if (!dir.exists(tmp.dir)) {
                     dir.create(tmp.dir, recursive = TRUE, showWarnings = FALSE)
                }

                for (smry in names(sgp_object@Summary[[nm]])) {
                    tmp.fname <- file.path(tmp.dir, paste0(smry, ".parquet"))
                    arrow::write_parquet(
                        x = sgp_object@Summary[[nm]][[smry]],
                        sink = tmp.fname,
                        compression = "uncompressed"
                    )
                    if (return.object) {
                        sgp_object@Summary[[nm]][[smry]] <-
                            ArrowSGP$new(source_path = tmp.fname)
                            # arrow::read_parquet(tmp.fname, as_data_frame = FALSE)
                    }
                }
            }
        }

        if (!is.null(sgp_object@SGP[["Coefficient_Matrices"]]) &&
            "Coefficient_Matrices" %in% slot.type
        ) {
            tmp.dir <- file.path(base.directory, "Coefficient_Matrices")
            if (!dir.exists(tmp.dir)) {
                 dir.create(tmp.dir, recursive = TRUE, showWarnings = FALSE)
            }

            tmp.names <- names(sgp_object@SGP[["Coefficient_Matrices"]])
            eqted.names <- grep("EQUATED$", tmp.names, value = TRUE)
            bline.names <- grep("BASELINE$", tmp.names, value = TRUE)
            bline.simex <- grep("BASELINE[.]SIMEX$", tmp.names, value = TRUE)
            cohrt.simex <- grep("SIMEX$", tmp.names, value = TRUE) %w/o% bline.simex
            cohrt.names <-
                tmp.names %w/o% c(eqted.names, bline.names, bline.simex, cohrt.simex)

            for (k in c(cohrt.names, cohrt.simex, eqted.names, bline.names, bline.simex)) {
                tmp_mtx <- sgp_object@SGP[["Coefficient_Matrices"]][[k]]
                if (is(tmp_mtx, "matrixSlot")) next

                saveRDS(tmp_mtx, file = file.path(tmp.dir, paste0(k, ".rda")))
                if (return.object) {
                    sgp_object@SGP[["Coefficient_Matrices"]][[k]] <-
                        matrixSlot$new(source_path = file.path(tmp.dir, paste0(k, ".rda")))
                }
            }
        }
            # for (j in 1:5) {
            #     ref.type <- 
            #         switch(j, 
            #             "1" = "COHORT", "2" = "SIMEX", "3" = "EQUATED"#,
            #             "4" = "BASELINE", "5" = "BASELINE_SIMEX"
            #         )
            #     if (!length(tmp.names[[j]])) next

                # tmp_mtx <- 
                #     arrow::write_parquet(
                #         x = splineMatrix2Arrow(
                #             sgp_object@SGP[["Coefficient_Matrices"]][[ca]][[gord]]
                #         ),
                #         sink = file.path(base.directory, "Coefficient_Matrices", ...),
                #         # sink = file.path(tmp.dir, paste0(smry, ".parquet")),
                #         compression = "uncompressed"
                #     )
            # }
        # }

        if (!is.null(sgp_object@SGP[["Goodness_of_Fit"]]) &&
            "Goodness_of_Fit" %in% slot.type
        ) {
            tmp.dir <- file.path(base.directory, "Goodness_of_Fit")
            if (!dir.exists(tmp.dir)) {
                 dir.create(tmp.dir, recursive = TRUE, showWarnings = FALSE)
            }

            for (k in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
                tmp_g0f <- sgp_object@SGP[["Goodness_of_Fit"]][[k]]
                if (is(tmp_g0f, "goFitSlot")) next

                saveRDS(tmp_g0f, file = file.path(tmp.dir, paste0(k, ".rda")))
                if (return.object) {
                    sgp_object@SGP[["Goodness_of_Fit"]][[k]] <-
                        goFitSlot$new(source_path = file.path(tmp.dir, paste0(k, ".rda")))
                }
            }
        }

        if (!is.null(sgp_object@SGP[["SGPercentiles"]]) && "SGPercentiles" %in% slot.type) {
            pctl.check.list <-
                c("sgp.percentiles", "sgp.percentiles", "sgp.percentiles.baseline")
            for (j in 1:3) {
                ref.type <- switch(j, "1" = "COHORT", "2" = "EQUATED", "3" = "BASELINE")
                tmp.names <-
                    getPercentileTableNames(
                        sgp_object, content_areas = NULL, state = NULL, years = NULL,
                        pctl.check.list[j], sgp.percentiles.equated = (j == 2),
                        use.cohort.for.baseline.when.missing = FALSE
                    )
                if (!length(tmp.names)) next

                for (i in tmp.names) {
                    tmp.year <- getTableNameYear(i)
                    tmp.subj <- strsplit(i, "[.]")[[1]][1L]
                    if (is(sgp_object@SGP[["SGPercentiles"]][[i]], "ArrowSGP")) next

                    fletch(
                        data.table(
                            sgp_object@SGP[["SGPercentiles"]][[i]],
                            CONTENT_AREA = tmp.subj,
                            YEAR = tmp.year,
                            VALID_CASE = "VALID_CASE"
                        ),
                        output.dir = file.path(base.directory, "SGPercentiles", ref.type),
                        partitions = c("YEAR", "CONTENT_AREA"),
                        existing_data_behavior = "overwrite"
                    )
                    if (return.object) {
                        file.loc <-
                            file.path(
                                base.directory, "SGPercentiles", ref.type,
                                paste0("YEAR=", tmp.year),
                                paste0("CONTENT_AREA=", tmp.subj),
                                "part-0.parquet"
                            )
                        sgp_object@SGP[["SGPercentiles"]][[i]] <-
                            ArrowSGP$new(source_path = file.loc)
                    }
                }
            }
        }

        if (!is.null(sgp_object@SGP[["SGProjections"]]) && "SGProjections" %in% slot.type) {
            pjct.check.list <-
                c("sgp.projections", "sgp.projections.lagged",
                  "sgp.projections.baseline", "sgp.projections.lagged.baseline",
                  "sgp.projections", "sgp.projections.lagged",
                  "sgp.projections.baseline", "sgp.projections.lagged.baseline")
            for (j in 1:8) {
                ref.type <- ifelse(j %in% c(1:2, 5:6), "COHORT", "BASELINE")
                pjct.type <- ifelse(grepl("lagged", pjct.check.list[j]), "LAGGED", "STRAIGHT")
                target.tf <- (j > 4)
                tmp.names <-
                    getPercentileTableNames(
                        sgp_object, content_areas = NULL, state = NULL, years = NULL,
                        pjct.check.list[j], use.cohort.for.baseline.when.missing = FALSE,
                        sgp.target.scale.scores = target.tf
                    )
                if (!length(tmp.names)) next

                for (i in tmp.names) {
                    if (is(sgp_object@SGP[["SGProjections"]][[i]], "ArrowSGP")) next

                    tmp.year <- getTableNameYear(i)
                    tmp.subj <- strsplit(i, "[.]")[[1]][1L]

                    fletch(
                        data.table(
                            sgp_object@SGP[["SGProjections"]][[i]],
                            CONTENT_AREA = tmp.subj,
                            YEAR = tmp.year,
                            VALID_CASE = "VALID_CASE"
                        ),
                        partitions = c("YEAR", "CONTENT_AREA"),
                        output.dir =
                            file.path(base.directory, "SGProjections", ref.type,
                                      ifelse(target.tf, "TARGET_SCALE_SCORES", "GROWTH"), pjct.type),
                        existing_data_behavior = "overwrite"
                    )
                    if (return.object) {
                        file.loc <-
                            file.path(
                                base.directory, "SGProjections", ref.type,
                                ifelse(target.tf, "TARGET_SCALE_SCORES", "GROWTH"), pjct.type,
                                paste0("YEAR=", tmp.year),
                                paste0("CONTENT_AREA=", tmp.subj),
                                "part-0.parquet"
                            )
                        sgp_object@SGP[["SGProjections"]][[i]] <-
                            ArrowSGP$new(source_path = file.loc)
                    }
                }
            }
        }

        if (!is.null(sgp_object@SGP$Simulated_SGPs) && "Simulated_SGPs" %in% slot.type) {
            # NOTE: The use of `getPercentileTableNames` here assumes that the
            #       Simulated_SGPs and SGPercentiles slots have matched named list structure.
            #       Reasonable unless some subjects don't/can't get sim SGPs (no CSEMs).
            pctl.check.list <-
                c("sgp.percentiles", "sgp.percentiles", "sgp.percentiles.baseline")
            for (j in 1:3) {
                ref.type <- switch(j, "1" = "COHORT", "2" = "EQUATED", "3" = "BASELINE")
                tmp.names <-
                    getPercentileTableNames(
                        sgp_object, content_areas = NULL, state = NULL, years = NULL,
                        pctl.check.list[j], sgp.percentiles.equated = (j == 2),
                        use.cohort.for.baseline.when.missing = FALSE
                    )
                if (!length(tmp.names)) next

                for (i in tmp.names) {
                    if (is(sgp_object@SGP[["Simulated_SGPs"]][[i]], "ArrowSGP")) next
                    tmp.year <- getTableNameYear(i)
                    tmp.subj <- strsplit(i, "[.]")[[1]][1L]

                    fletch(
                        data.table(
                            sgp_object@SGP[["Simulated_SGPs"]][[i]],
                            CONTENT_AREA = tmp.subj,
                            YEAR = tmp.year,
                            VALID_CASE = "VALID_CASE"
                        ),
                        partitions = c("YEAR", "CONTENT_AREA"),
                        output.dir = file.path(base.directory, "Simulated_SGPs", ref.type),
                        existing_data_behavior = "overwrite"
                    )
                    if (return.object) {
                        file.loc <-
                            file.path(
                                base.directory, "Simulated_SGPs", ref.type,
                                paste0("YEAR=", tmp.year),
                                paste0("CONTENT_AREA=", tmp.subj),
                                "part-0.parquet"
                            )
                        sgp_object@SGP[["Simulated_SGPs"]][[i]] <-
                            ArrowSGP$new(source_path = file.loc)
                    }
                }
            }
        }

        if (return.object) {
            return(sgp_object)
        }
    }

matrixSlot <- R6::R6Class("matrixSlot",
    cloneable = FALSE,
    portable = FALSE,
    public = list(
        source_path = NULL,
        initialize = \(source_path = NA) {
            source_path <<- source_path
        },
        print = function() {
            cat(self[["source_path"]])
        }
    ),
    active = list(
        get = \() readRDS(source_path)
    )
)

goFitSlot <- R6::R6Class("goFitSlot",
    cloneable = FALSE,
    portable = FALSE,
    public = list(
        source_path = NULL,
        initialize = \(source_path = NA) {
            source_path <<- source_path
        },
        print = function() {
            cat(self[["source_path"]])
        }
    ),
    active = list(
        get = \() readRDS(source_path)
    )
)


# splineMatrix2Arrow =
#     function(smat) {
#         tmp_mtx <- as.data.table(smat@.Data, keep.rownames= TRUE)

#         setattr(tmp_mtx, "Knots", smat@Knots)
#         setattr(tmp_mtx, "Boundaries", smat@Boundaries)
#         setattr(tmp_mtx, "Content_Areas", smat@Content_Areas)
#         setattr(tmp_mtx, "Grade_Progression", smat@Grade_Progression)
#         setattr(tmp_mtx, "Time", smat@Time)
#         setattr(tmp_mtx, "Time_Lags", smat@Time_Lags)
#         setattr(tmp_mtx, "Version", smat@Version)

#         arrow::arrow_table(tmp_mtx)
#     }

# arrow2splineMatrix =
#     function(dt) {
#         smat <-
#             new("splineMatrix",
#                 .Data = as.matrix(dt, rownames = 1),
#                 Knots = attr(dt, "Knots"), 
#                 Boundaries = attr(dt, "Boundaries"), 
#                 Content_Areas = attr(dt, "Content_Areas"), 
#                 Grade_Progression = attr(dt, "Grade_Progression"), 
#                 Time = attr(dt, "Time"), 
#                 Time_Lags = attr(dt, "Time_Lags"), 
#                 Version = "Version"
#             )
#     }
