`newArrowSGP` =
    function(
        sgp_object = NULL,
        state = NULL,
        years = NULL,
        grades = NULL,
        content.areas = NULL,
        slot.type = NULL,
        combine.results = TRUE,
        combineSGP.types = "sgp.percentiles",
        base.directory = file.path("Data", "Arrow_SGP")
    ) {
        ###   Utility functions
        `%w/o%` = function(x, y) x[!x %in% y]

        strReverse <- function(x, split) {
            sapply(lapply(strsplit(x, split), rev), paste, collapse = ".")
        }

        ###   Create new SGP object if applicable
        if (is.null(sgp_object)) sgp_object <- new("SGP")

        ###   Create state (if NULL) from sgp_object (if possible)
        if (is.null(state) & combine.results) {
            tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
            state <- getStateAbbreviation(tmp.name, "analyzeSGP")
        }

        ###   Set up NULL argument default replacements
        if (is.null(slot.type)) {
            slot.type <- c(
                if (dir.exists(file.path(base.directory, "LONG"))) "Data",
                if (dir.exists(file.path(base.directory, "Supplementary"))) "Data_Supplementary",
                if (dir.exists(file.path(base.directory, "Summary"))) "Summary",
                if (dir.exists(file.path(base.directory, "Coefficient_Matrices"))) "Coefficient_Matrices",
                if (dir.exists(file.path(base.directory, "Goodness_of_Fit"))) "Goodness_of_Fit",
                if (dir.exists(file.path(base.directory, "SGPercentiles"))) "SGPercentiles",
                if (dir.exists(file.path(base.directory, "SGProjections"))) "SGProjections",
                if (dir.exists(file.path(base.directory, "Simulated_SGPs"))) "Simulated_SGPs"
            )
            ##  Below is domain of unpackSGP
            # if (is.null(slot.type) & !"data.frame" %in% class(sgp_object)) {
            #     message(
            #         "\n\n\t\t\tNo `arrow` data files exist in the `base.directory` provided, and
            #             no new data provided in the `sgp_object` argument to create the
            #             @Data slot and output an arrow file system.
            #             Please ensure that either the correct arrow data is provided here:\n\n\t\t\t",
            #         normalizePath(file.path(base.directory, "LONG")),
            #         "\n\n\t\t\tor provide new data in the `sgp_object` argument.
            #             An empty SGP object will be returned."
            #     )
            #     combine.results <- FALSE
            # }
            # if ("data.frame" %in% class(sgp_object) & !"Data" %in% slot.type) {
            #     slot.type <- c(slot.type, "Data")
            # }
        }

        #####
        ###   Fill in Slots
        #####

        if ("Data" %in% slot.type) {
            data.dir <- file.path(base.directory, "LONG")
            if (!dir.exists(data.dir)) {
                stop(
                    "\n\n\t\t\tThe `Data` slot addition requires an arrow file system in place
                        for the LONGITUDINAL (base) data located at:\n\n\t\t\t",
                    normalizePath(data.dir),
                    "\n\n\t\t\tPlease ensure that the correct arrow data is provided here
                        and check specification of the `base.directory` argument."
                )
            } else {
                if (is(sgp_object@Data, "ArrowSGP")) {
                    sgp_object@Data$set
                }
                if (!length(sgp_object@Data)) {
                    sgp_object@Data <-
                        ArrowSGP$new(
                            source_path = data.dir,
                            years = years,
                            grades = grades,
                            content_areas = content.areas
                        )
                }
            }
        }

        if ("Data_Supplementary" %in% slot.type) {
            sup.dir <- file.path(base.directory, "Supplementary")
            if (!dir.exists(sup.dir)) {
                stop(
                    "\n\n\t\t\tThe `Data_Supplementary` slot addition requires an arrow file system
                        in place for the data source located at:\n\n\t\t\t",
                    normalizePath(sup.dir),
                    "\n\n\t\t\tPlease ensure that the correct arrow data is provided here
                        and check specification of the `base.directory` argument."
                )
            }

            for (nm in list.dirs(sup.dir, full.names = FALSE, recursive = FALSE)) {
                if (is(sgp_object@Data_Supplementary[[nm]], "ArrowSGP")) {
                    sgp_object@Data_Supplementary[[nm]]$set
                } else {
                    sgp_object@Data_Supplementary[[nm]] <-
                        ArrowSGP$new(
                            source_path = file.path(sup.dir, nm),
                            years = years,
                            grades = grades,
                            content_areas = content.areas
                        )
                }
            }
        }

        if ("Summary" %in% slot.type) {
            smry.dir <- file.path(base.directory, "Summary")
            if (!dir.exists(smry.dir)) {
                stop(
                    "\n\n\t\t\tThe `Summary` slot addition requires an arrow file system
                        in place for the individual summary files located at:\n\n\t\t\t",
                    normalizePath(smry.dir),
                    "\n\n\t\t\tPlease ensure that the correct arrow data is provided here
                        and check specification of the `base.directory` argument."
                )
            }
            for (nm in list.dirs(smry.dir, full.names = FALSE, recursive = FALSE)) {
                smry.to.fill <- list.files(file.path(smry.dir, nm))
                smry.to.fill <-
                    lapply(smry.to.fill, \(f) gsub("[.]parquet", "", f)) |>
                    unlist()
                for (smry in smry.to.fill) {
                    if (is(sgp_object@Summary[[nm]][[smry]], "ArrowSGP")) {
                        sgp_object@Summary[[nm]][[smry]]$set
                    } else {
                        sgp_object@Summary[[nm]][[smry]] <-
                            ArrowSGP$new(
                                source_path = file.path(smry.dir, nm, paste0(smry, ".parquet"))
                            )
                    }
                }
            }
        }

        if ("Coefficient_Matrices" %in% slot.type) {
            mtx.dir <- file.path(base.directory, "Coefficient_Matrices")
            if (!dir.exists(mtx.dir)) {
                stop(
                    "\n\n\t\t\tThe `Coefficient_Matrices` slot addition requires matrices
                        saved in subdirectories under:\n\n\t\t\t",
                    normalizePath(mtx.dir),
                    "\n\n\t\t\tPlease ensure that the correct R data files are provided here
                        and check specification of the `base.directory` argument."
                )
            }
            slots.to.fill <- list.files(mtx.dir, full.names = FALSE, recursive = FALSE)
            if (!is.null(content.areas)) {
                slots.to.fill <- grep(paste(content.areas, collapse = "|"), slots.to.fill, value = TRUE)
            }
            if (!is.null(years)) {
                slots.to.fill <- grep(paste(years, collapse = "|"), slots.to.fill, value = TRUE)
            }
            for (slt in slots.to.fill) {
                slot <- gsub(".rda", "", slt)
                if (is(sgp_object@SGP[["Coefficient_Matrices"]][[slot]], "matrixSlot")) {
                    sgp_object@SGP[["Coefficient_Matrices"]][[slot]]$set
                } else {
                    sgp_object@SGP[["Coefficient_Matrices"]][[slot]] <-
                        matrixSlot$new(source_path = file.path(mtx.dir, slt))
                }
            }
        }

        if ("Goodness_of_Fit" %in% slot.type) {
            fit.dir <- file.path(base.directory, "Goodness_of_Fit")
            if (!dir.exists(fit.dir)) {
                stop(
                    "\n\n\t\t\tThe `Goodness_of_Fit` slot addition requires graphical objects
                        saved in subdirectories under:\n\n\t\t\t",
                    normalizePath(fit.dir),
                    "\n\n\t\t\tPlease ensure that the correct R data files are provided here
                        and check specification of the `base.directory` argument."
                )
            }
            slots.to.fill <- list.files(fit.dir, full.names = FALSE, recursive = FALSE)
            if (!is.null(content.areas)) {
                slots.to.fill <- grep(paste(content.areas, collapse = "|"), slots.to.fill, value = TRUE)
            }
            if (!is.null(years)) {
                slots.to.fill <- grep(paste(years, collapse = "|"), slots.to.fill, value = TRUE)
            }
            for (slt in slots.to.fill) {
                slot <- gsub(".rda", "", slt)
                if (is(sgp_object@SGP[["Goodness_of_Fit"]][[slot]], "goFitSlot")) {
                    sgp_object@SGP[["Goodness_of_Fit"]][[slot]]$set
                } else {
                    sgp_object@SGP[["Goodness_of_Fit"]][[slot]] <-
                        matrixSlot$new(source_path = file.path(mtx.dir, slt))
                }
            }
        }

        if ("SGPercentiles" %in% slot.type) {
            pctl.dir <- file.path(base.directory, "SGPercentiles")
            if (!dir.exists(pctl.dir)) {
                stop(
                    "\n\n\t\t\tThe `SGPercentiles` slot addition requires an arrow file system
                        in place for previously calculated results located at:\n\n\t\t\t",
                    normalizePath(pctl.dir),
                    "\n\n\t\t\tPlease ensure that the correct arrow data is provided here
                        and check specification of the `base.directory` argument."
                )
            }
            for (refd in list.dirs(pctl.dir, full.names = FALSE, recursive = FALSE)) {
                pctl.files <- list.files(file.path(pctl.dir, refd), recursive = TRUE)
                if (!is.null(content.areas)) {
                    pctl.files <- grep(paste(content.areas, collapse = "|"), pctl.files, value = TRUE)
                }
                if (!is.null(years)) {
                    pctl.files <- grep(paste(years, collapse = "|"), pctl.files, value = TRUE)
                }

                pctl.res <- dirname(pctl.files) |> unique()
                pctl.res <- gsub("YEAR=", "", gsub("CONTENT_AREA=", "", pctl.res))
                pctl.res <- lapply(pctl.res, strReverse, "/") |> unlist() # .Platform$file.sep)
                if (refd != "COHORT") pctl.res <- paste0(pctl.res, ".", refd)

                for (s in seq(pctl.res)) {
                    if (is(sgp_object@SGP[["SGPercentiles"]][[pctl.res[s]]], "ArrowSGP")) {
                        sgp_object@SGP[["SGPercentiles"]][[pctl.res[s]]]$set
                    } else {
                        sgp_object@SGP[["SGPercentiles"]][[pctl.res[s]]] <-
                            ArrowSGP$new(
                                source_path = file.path(pctl.dir, refd, pctl.files[s])
                            )
                    }
                }
            }
        }

        if ("SGProjections" %in% slot.type) {
            pjct.dir <- file.path(base.directory, "SGProjections")
            if (!dir.exists(pjct.dir)) {
                stop(
                    "\n\n\t\t\tThe `SGProjections` slot addition requires an arrow file system
                        in place for previously calculated results located at:\n\n\t\t\t",
                    normalizePath(pjct.dir),
                    "\n\n\t\t\tPlease ensure that the correct arrow data is provided here
                        and check specification of the `base.directory` argument."
                )
            }
            for (refd in list.dirs(pjct.dir, full.names = FALSE, recursive = FALSE)) {
                pjct.files <- list.files(file.path(pjct.dir, refd), recursive = TRUE)
                if (!is.null(content.areas)) {
                    pjct.files <- grep(paste(content.areas, collapse = "|"), pjct.files, value = TRUE)
                }
                if (!is.null(years)) {
                    pjct.files <- grep(paste(years, collapse = "|"), pjct.files, value = TRUE)
                }

                pjct.res <- dirname(pjct.files) |> unique()
                pjct.res <- gsub("YEAR=", "", gsub("CONTENT_AREA=", "", pjct.res))
                pjct.res <- lapply(pjct.res, strReverse, "/") |> unlist()
                pjct.res <- gsub("[.]GROWTH$", "", pjct.res)
                pjct.res <- gsub("[.]STRAIGHT", "", pjct.res)
                if (refd != "COHORT") pjct.res <- paste0(pjct.res, ".", refd)

                for (s in seq(pjct.res)) {
                    if (is(sgp_object@SGP[["SGProjections"]][[pjct.res[s]]], "ArrowSGP")) {
                        sgp_object@SGP[["SGProjections"]][[pjct.res[s]]]$set
                    } else {
                        sgp_object@SGP[["SGProjections"]][[pjct.res[s]]] <-
                            ArrowSGP$new(
                                source_path = file.path(pjct.dir, refd, pjct.files[s])
                            )
                    }
                }
            }
        }

        if ("Simulated_SGPs" %in% slot.type) {
            simgp.dir <- file.path(base.directory, "Simulated_SGPs")
            if (!dir.exists(simgp.dir)) {
                stop(
                    "\n\n\t\t\tThe `Simulated_SGPs` slot addition requires an arrow file system
                        in place for previously calculated results located at:\n\n\t\t\t",
                    normalizePath(simgp.dir),
                    "\n\n\t\t\tPlease ensure that the correct arrow data is provided here
                        and check specification of the `base.directory` argument."
                )
            }
            for (refd in list.dirs(simgp.dir, full.names = FALSE, recursive = FALSE)) {
                simgp.files <- list.files(file.path(simgp.dir, refd), recursive = TRUE)
                if (!is.null(content.areas)) {
                    simgp.files <- grep(paste(content.areas, collapse = "|"), simgp.files, value = TRUE)
                }
                if (!is.null(years)) {
                    simgp.files <- grep(paste(years, collapse = "|"), simgp.files, value = TRUE)
                }

                simgp.res <- dirname(simgp.files) |> unique()
                simgp.res <- gsub("YEAR=", "", gsub("CONTENT_AREA=", "", simgp.res))
                simgp.res <- lapply(simgp.res, strReverse, "/") |> unlist()
                if (refd != "COHORT") simgp.res <- paste0(simgp.res, ".", refd)

                for (s in seq(simgp.res)) {
                    if (is(sgp_object@SGP[["Simulated_SGPs"]][[simgp.res[s]]], "ArrowSGP")) {
                        sgp_object@SGP[["Simulated_SGPs"]][[simgp.res[s]]]$set
                    } else {
                        sgp_object@SGP[["Simulated_SGPs"]][[simgp.res[s]]] <-
                            ArrowSGP$new(
                                source_path = file.path(simgp.dir, refd, simgp.files[s])
                            )
                    }
                }
            }
        }

        if (combine.results && !is.null(combineSGP.types)) {
            sgp_object <-
                combineSGP(
                    sgp_object,
                    state = state,
                    years = years,
                    content_areas = content.areas,
                    sgp.percentiles =
                        ("sgp.percentiles" %in% combineSGP.types),
                    sgp.percentiles.baseline =
                        ("sgp.percentiles.baseline" %in% combineSGP.types),
                    sgp.projections =
                        ("sgp.projections" %in% combineSGP.types),
                    sgp.projections.baseline =
                        ("sgp.projections.baseline" %in% combineSGP.types),
                    sgp.projections.lagged =
                        ("sgp.projections.lagged" %in% combineSGP.types),
                    sgp.projections.lagged.baseline =
                        ("sgp.projections.lagged.baseline" %in% combineSGP.types)
                )
        }

        return(sgp_object)
    }
