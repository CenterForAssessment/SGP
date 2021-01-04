`stratifiedRandomSampleSGP` <-
    function(
        norm_group_data,
        strata_variable,
        strata_proportions,
        sample_size=NULL) {

        CONTENT_AREA <- GRADE <- ID <- VALID_CASE <- YEAR <- NULL ### To prevent R CMD check warnings

        tmp.years <- unique(norm_group_data[['YEAR']])
        sample_data <- norm_group_data[YEAR==tail(tmp.years, 1)]
        if (is.null(sample_size)) sample_size=nrow(sample_data)
        stopifnot(length(strata_proportions) == uniqueN(norm_group_data[[strata_variable]]))
        tmp.replications <- ceiling(max((strata_proportions*sample_size)/table(sample_data[[strata_variable]])))
        setkey(norm_group_data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
        tmp.replicated_norm_group_data <- rbindlist(replicate(tmp.replications, return(norm_group_data), simplify=FALSE))[,ID:=paste(ID, rep(seq.int(tmp.replications), each=nrow(norm_group_data)), sep="_")]
        tmp.sample_data <- tmp.replicated_norm_group_data[YEAR==tail(tmp.years, 1)][, .SD[sample(.N, ceiling(sample_size*strata_proportions[[unlist(.BY)]]))], keyby = strata_variable]
        return(rbindlist(list(tmp.replicated_norm_group_data[YEAR %in% head(tmp.years, -1) & ID %in% tmp.sample_data[['ID']]], tmp.sample_data), use.names=TRUE))
} ### END stratifiedRandomSamplSGP
