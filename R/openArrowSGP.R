`openArrowSGP` =
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
        ###   Make or load SGP object if applicable
        if (is.null(sgp_object)) sgp_object <- new("SGP")
        if (is.character(sgp_object)) {
            if (is.null(state)) {
                fname <- basename(sgp_object)
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(fname))))
                state <- getStateAbbreviation(tmp.name, "analyzeSGP")
            }
            sgp_object <- readRDS(sgp_object)
        }

        ###   Create state (if NULL) from sgp_object (if possible)
        if (is.null(state) & combine.results) {
            tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
            state <- getStateAbbreviation(tmp.name, "analyzeSGP")
        }

        ###   Set up NULL argument default replacements
        if (is.null(slot.type)) {
            slot.type <- c(
                if (!is.null(sgp_object@Data)) "Data",
                if (!is.null(sgp_object@Summary)) "Summary",
                if (!is.null(sgp_object@Data_Supplementary)) "Data_Supplementary",
                if (!is.null(sgp_object@SGP[["Coefficient_Matrices"]])) "Coefficient_Matrices",
                if (!is.null(sgp_object@SGP[["Goodness_of_Fit"]])) "Goodness_of_Fit",
                if (!is.null(sgp_object@SGP[["SGPercentiles"]])) "SGPercentiles",
                if (!is.null(sgp_object@SGP[["SGProjections"]])) "SGProjections",
                if (!is.null(sgp_object@SGP[["Simulated_SGPs"]])) "Simulated_SGPs"
            )
        }

        if (toupper(combineSGP.types) == "ALL") {
            combineSGP.types <-
                c("sgp.percentiles", "sgp.percentiles.baseline",
                  "sgp.projections", "sgp.projections.baseline",
                  "sgp.projections.lagged", "sgp.projections.lagged.baseline"
                )
        }

        #####
        ###   Re-set Slots
        #####

        if ("Data" %in% slot.type) {
            if (is(sgp_object@Data, "ArrowSGP")) {
                sgp_object@Data$set
            }
        }

        if ("Data_Supplementary" %in% slot.type) {
            for (nm in names(sgp_object@Data_Supplementary)) {
                if (is(sgp_object@Data_Supplementary[[nm]], "ArrowSGP")) {
                    sgp_object@Data_Supplementary[[nm]]$set
                }
            }
        }

        if ("Summary" %in% slot.type) {
            for (nm in names(sgp_object@Summary)) {
                for (smry in names(sgp_object@Summary[[nm]])) {
                    if (is(sgp_object@Summary[[nm]][[smry]], "ArrowSGP")) {
                        sgp_object@Summary[[nm]][[smry]]$set
                    }
                }
            }
        }

        if ("Coefficient_Matrices" %in% slot.type) {
            for (slot in names(sgp_object@SGP[["Coefficient_Matrices"]])) {
                if (is(sgp_object@SGP[["Coefficient_Matrices"]][[slot]], "matrixSlot")) {
                    sgp_object@SGP[["Coefficient_Matrices"]][[slot]]$set
                }
            }
        }

        if ("Goodness_of_Fit" %in% slot.type) {
            for (slot in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
                if (is(sgp_object@SGP[["Goodness_of_Fit"]][[slot]], "goFitSlot")) {
                    sgp_object@SGP[["Goodness_of_Fit"]][[slot]]$set
                }
            }
        }

        if ("SGPercentiles" %in% slot.type) {
            for (pctl in names(sgp_object@SGP[["SGPercentiles"]])) {
                if (is(sgp_object@SGP[["SGPercentiles"]][[pctl]], "ArrowSGP")) {
                    sgp_object@SGP[["SGPercentiles"]][[pctl]]$set
                }
            }
        }

        if ("SGProjections" %in% slot.type) {
            for (pjct in names(sgp_object@SGP[["SGProjections"]])) {
                if (is(sgp_object@SGP[["SGProjections"]][[pjct]], "ArrowSGP")) {
                    sgp_object@SGP[["SGProjections"]][[pjct]]$set
                }
            }
        }

        if ("Simulated_SGPs" %in% slot.type) {
            for (simgp in names(sgp_object@SGP[["Simulated_SGPs"]])) {
                if (is(sgp_object@SGP[["Simulated_SGPs"]][[simgp]], "ArrowSGP")) {
                    sgp_object@SGP[["Simulated_SGPs"]][[simgp]]$set
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
