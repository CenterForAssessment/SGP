csemScoreSimulator <-
    function(
        scale_scores,
        grade,
        content_area,
        year,
        loss.hoss,
        state,
        variable=NULL,
        distribution=NULL,
        iterations = 1L,
        round.digits=NULL,
        sgp.sim.seed
    ) {

    GRADE <- CONTENT_AREA <- YEAR <- V1 <- NULL

    ### Define relevant variables

    if (is.null(round.digits)) round.digits <- 1L
    if (is.null(distribution)) distribution <- "Normal"

    Interpolation_Function <- function(scale_score, variance, round.digits) {
        return(splinefun(scale_score, variance/sqrt(round.digits), method = "natural"))
    }

    ### Create scale score dependent CSEMs

    if (is.null(variable) && !is.null(state)) {
        if ("YEAR" %in%
            names(csem.data <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])
        ) {
            Interpolation_Data <- csem.data[GRADE == grade & CONTENT_AREA == content_area & YEAR == year]
        } else {
            Interpolation_Data <- csem.data[GRADE == grade & CONTENT_AREA == content_area]
        }
        tmp.omega <-
            Interpolation_Function(
                Interpolation_Data[['SCALE_SCORE']],
                Interpolation_Data[['SCALE_SCORE_CSEM']],
                round.digits
            )(scale_scores)
    }
    if (!is.null(variable)) {
        tmp.dt <- setkey(unique(data.table(V1=scale_scores, V2=variable), by="V1"), V1)
        tmp.omega <- Interpolation_Function(tmp.dt[['V1']], tmp.dt[['V2']], round.digits)(scale_scores)
    }
    n <- length(scale_scores)
    if (distribution=="Skew-Normal") {
        tmp.scores <- Rfast::Round(
            rsn(n, xi = scale_scores, omega = tmp.omega,
                alpha = tan((pi/2)*((loss.hoss[1]+loss.hoss[2]) - 2*scale_scores)/(loss.hoss[2]-loss.hoss[1]))
            ), digit = round.digits)
    } else {
        tmp.scores <- lapply(
            1:iterations,
            function(x) {scale_scores + Rfast::Rnorm(n, seed = as.integer(sgp.sim.seed*x)) * tmp.omega}
        ) |> collapse::qM() |> Rfast::Round(digit = round.digits)
    }

    tmp.scores <- collapse::replace_outliers(tmp.scores, value = "clip", limits = loss.hoss)

    return(tmp.scores)
} ### END csemScoreSimulator
