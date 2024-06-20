`packArrowSGP` =
    function(
        sgp_object,
        slot.type = NULL
    ) {
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

        #####
        ###   Get Slots
        #####

        if ("Data" %in% slot.type) {
            if (is(sgp_object@Data, "ArrowSGP")) {
                sgp_object@Data <- sgp_object@Data$get
            }
        }

        if ("Data_Supplementary" %in% slot.type) {
            for (nm in names(sgp_object@Data_Supplementary)) {
                if (is(sgp_object@Data_Supplementary[[nm]], "ArrowSGP")) {
                    sgp_object@Data_Supplementary[[nm]] <-
                        sgp_object@Data_Supplementary[[nm]]$get
                }
            }
        }

        if ("Summary" %in% slot.type) {
            for (nm in names(sgp_object@Summary)) {
                for (smry in names(sgp_object@Summary[[nm]])) {
                    if (is(sgp_object@Summary[[nm]][[smry]], "ArrowSGP")) {
                        sgp_object@Summary[[nm]][[smry]] <-
                            sgp_object@Summary[[nm]][[smry]]$get
                    }
                }
            }
        }

        if ("Coefficient_Matrices" %in% slot.type) {
            for (slot in names(sgp_object@SGP[["Coefficient_Matrices"]])) {
                if (is(sgp_object@SGP[["Coefficient_Matrices"]][[slot]], "matrixSlot")) {
                    sgp_object@SGP[["Coefficient_Matrices"]][[slot]] <-
                        sgp_object@SGP[["Coefficient_Matrices"]][[slot]]$get
                }
            }
        }

        if ("Goodness_of_Fit" %in% slot.type) {
            for (slot in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
                if (is(sgp_object@SGP[["Goodness_of_Fit"]][[slot]], "goFitSlot")) {
                    sgp_object@SGP[["Goodness_of_Fit"]][[slot]] <-
                        sgp_object@SGP[["Goodness_of_Fit"]][[slot]]$get
                }
            }
        }

        if ("SGPercentiles" %in% slot.type) {
            for (pctl in names(sgp_object@SGP[["SGPercentiles"]])) {
                if (is(sgp_object@SGP[["SGPercentiles"]][[pctl]], "ArrowSGP")) {
                    sgp_object@SGP[["SGPercentiles"]][[pctl]] <-
                        sgp_object@SGP[["SGPercentiles"]][[pctl]]$get
                }
            }
        }

        if ("SGProjections" %in% slot.type) {
            for (pjct in names(sgp_object@SGP[["SGProjections"]])) {
                if (is(sgp_object@SGP[["SGProjections"]][[pjct]], "ArrowSGP")) {
                    sgp_object@SGP[["SGProjections"]][[pjct]] <-
                        sgp_object@SGP[["SGProjections"]][[pjct]]$get
                }
            }
        }

        if ("Simulated_SGPs" %in% slot.type) {
            for (simgp in names(sgp_object@SGP[["Simulated_SGPs"]])) {
                if (is(sgp_object@SGP[["Simulated_SGPs"]][[simgp]], "ArrowSGP")) {
                    sgp_object@SGP[["Simulated_SGPs"]][[simgp]] <-
                        sgp_object@SGP[["Simulated_SGPs"]][[simgp]]$get
                }
            }
        }

        return(sgp_object)
    }
