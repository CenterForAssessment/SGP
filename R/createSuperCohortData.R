`createSuperCohortData` <-
function(
        dataForSuperCohort,
        sgp.config,
	simex.baseline.config,
        content_areas,
        years,
        grades,
        baseline.grade.sequences.lags,
        exclude.years) {

        VALID_CASE <- CONTENT_AREA <- YEAR <- GRADE <- YEAR_WITHIN <- COHORT_YEAR <- NULL


        ### Utility functions

        test.year.sequence <- function(content_areas, years, grades, baseline.grade.sequences.lags=NULL) {
            grades <- type.convert(as.character(grades), as.is=TRUE)
            if (is.null(baseline.grade.sequences.lags)) baseline.grade.sequences.lags <- rep(1L, length(grades)-1L)
            tmp.years.sequence <- list()
            tmp.years.sequence <- lapply(years, function(x) yearIncrement(year=x, increment=c(0,cumsum(baseline.grade.sequences.lags))))
            return(tmp.years.sequence[sapply(tmp.years.sequence, function(x) all(x %in% years))])
        } ### END test.year.sequence

        variables.to.get=c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", 
		"LAST_OBSERVATION", simex.baseline.config$csem.data.vnames)
        tmp_sgp_data_for_analysis <- dataForSuperCohort[,intersect(names(dataForSuperCohort), variables.to.get), with=FALSE]["VALID_CASE"]

        if ("YEAR_WITHIN" %in% names(tmp_sgp_data_for_analysis)) {
            setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEAR_WITHIN)
            year_within.tf <- TRUE
        } else {
            setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
            year_within.tf <- FALSE
        }

        tmp.year.sequence <- test.year.sequence(content_areas, years, grades, baseline.grade.sequences.lags)

        if (!is.null(exclude.years)) {
            tmp.year.sequence <- tmp.year.sequence[sapply(tmp.year.sequence, function(x) !tail(x, 1) %in% exclude.years)]
        }

        tmp.list <- list()
        for (cohort.iter in seq_along(tmp.year.sequence)) {
            tmp.sgp.iter <- sgp.config[[cohort.iter]] # Convert sgp.config into a valid sgp.iter for getPanelData
            names(tmp.sgp.iter) <- gsub('sgp.baseline.', 'sgp.', names(tmp.sgp.iter))
            tmp.sgp.iter$sgp.panel.years <- tmp.year.sequence[[cohort.iter]]
            tmp.sgp.iter$sgp.grade.sequences <- tmp.sgp.iter$sgp.grade.sequences
            if (!is.null(tmp.sgp.iter$sgp.exclude.sequences)) tmp.sgp.iter$sgp.exclude.sequences <- tmp.sgp.iter$sgp.exclude.sequences[COHORT_YEAR %in% tail(tmp.sgp.iter$sgp.panel.years, 1L)]
            if (!is.null(simex.baseline.config))  sgp.csem <- simex.baseline.config$csem.data.vnames else sgp.csem <- NULL
            tmp.list[[cohort.iter]] <- getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter = tmp.sgp.iter, sgp.csem=sgp.csem)
            setnames(tmp.list[[cohort.iter]], c("ID",
                paste("GRADE", rev(seq_along(tmp.year.sequence[[cohort.iter]])), sep="_"),
                paste("SCALE_SCORE", rev(seq_along(tmp.year.sequence[[cohort.iter]])), sep="_"),
                if(year_within.tf) paste("YEAR_WITHIN", rev(seq_along(tmp.year.sequence[[cohort.iter]])), sep="_"),
                if(!is.null(sgp.csem)) paste(sgp.csem, rev(seq_along(tmp.year.sequence[[cohort.iter]])), sep="_")))
        }
        tmp.dt <- rbindlist(tmp.list, fill=TRUE)
        if (year_within.tf) tmp.dt[, grep("YEAR_WITHIN", names(tmp.dt)) := NULL] # remove YEAR_WITHIN from Data where relevant
        setkey(tmp.dt)
        return(tmp.dt)
} ### END createSuperCohortData
