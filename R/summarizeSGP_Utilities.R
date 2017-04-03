`sgpSummary` <-
function(sgp.groups.to.summarize,
	produce.confidence.interval,
	tmp.simulation.dt,
	state,
	sgp.summaries,
	confidence.interval.groups,
	my.sgp,
	sgp_key,
	variables.for.summaries,
	sim.info) {

	WEIGHT <- MEDIAN_SGP_with_SHRINKAGE <- NULL

	tmp.sgp.summaries <- sgp.summaries
	sgp.summaries.names <- unlist(strsplit(names(sgp.summaries), "[.]"))
	if (produce.confidence.interval) {
		if ("Bootstrap_CI" %in% confidence.interval.groups$TYPE) {
			tmp.list <- list()
			tmp.quantiles <- paste0("c(", paste(confidence.interval.groups$QUANTILES, collapse=", "), ")")
			for (i in confidence.interval.groups$VARIABLES) {
				tmp.list[[paste0("MEDIAN_", i, "_QUANTILES")]] <- paste0("boot.sgp(", i, ", ", tmp.quantiles, ")")
			}
			tmp.sgp.summaries <- c(tmp.sgp.summaries, tmp.list)
			sgp.summaries.names <- c(sgp.summaries.names, paste("MEDIAN", my.sgp, paste(confidence.interval.groups$QUANTILES, collapse="_"), "CONFIDENCE_BOUND_BOOTSTRAP", sep="_"))
		}
		if ("Bootstrap_SE" %in% confidence.interval.groups$TYPE) {
			tmp.list <- list()
			for (i in confidence.interval.groups$VARIABLES) {
				tmp.list[[paste0("MEDIAN_", i, "_SE")]] <- paste0("boot.sgp(", i, ")")
			}
			tmp.sgp.summaries <- c(tmp.sgp.summaries, tmp.list)
			sgp.summaries.names <- c(sgp.summaries.names, paste("MEDIAN", my.sgp, "STANDARD_ERROR_BOOTSTRAP", sep="_"))
		}
	}

	ListExpr <- parse(text=paste0("list(", paste(unlist(tmp.sgp.summaries), collapse=", "),")"))
	ByExpr <- parse(text=paste0("list(", paste(sgp.groups.to.summarize, collapse=", "), ")"))

	pull.vars <- c(unlist(sapply(dbListFields(dbConnect(SQLite(), dbname = file.path(tempdir(), "TMP_Summary_Data.sqlite")), "summary_data"),
		function(p) if (any(grepl(p, tmp.sgp.summaries))) return(p)), use.names=FALSE), strsplit(sgp.groups.to.summarize, ", ")[[1]])

	tmp <- pullData(tmp.simulation.dt, state, pull.vars, variables.for.summaries, sgp.groups.to.summarize, sgp_key)[, eval(ListExpr), keyby=eval(ByExpr)]
	setnames(tmp, paste0("V", seq_along(sgp.summaries.names)), sgp.summaries.names)

	if (produce.confidence.interval & "CSEM" %in% confidence.interval.groups[['TYPE']]) {
		pull.vars <- c(sgp_key, unlist(strsplit(sgp.groups.to.summarize, ", ")))
		tmp <- pullData(tmp.simulation.dt, state, pull.vars, variables.for.summaries, sgp.groups.to.summarize, sgp_key, tmp_key = key(tmp), sim.info=sim.info)[tmp]
		setcolorder(tmp, c(grep("CSEM", names(tmp), invert=TRUE), grep("CSEM", names(tmp))))
	}

	if ('MEDIAN_SGP_STANDARD_ERROR' %in% names(tmp)) {
		constant <- var(tmp[['MEDIAN_SGP']], na.rm=TRUE) - mean(tmp[['MEDIAN_SGP_STANDARD_ERROR']]^2, na.rm=TRUE)
		tmp[,MEDIAN_SGP_with_SHRINKAGE := round(50 + ((tmp[['MEDIAN_SGP']]-50) * (constant/(constant+tmp[['MEDIAN_SGP_STANDARD_ERROR']]^2))))]
	}

	messageSGP(paste("\tFinished with", sgp.groups.to.summarize))
	return(tmp)
} ### END sgpSummary function


`pullData` <-
function(tmp.simulation.dt,
	state,
	pull.vars,
	variables.for.summaries,
	sgp.groups.to.summarize,
	sgp_key,
	tmp_key,
	sim.info=NULL) {

	SGP_SIM <- V1 <- V2 <- SIM_NUM <- WEIGHT <- ACHIEVEMENT_LEVEL <- ACHIEVEMENT_LEVEL_PRIOR <- CATCH_UP_KEEP_UP_STATUS <- MOVE_UP_STAY_UP_STATUS <- NULL
	CATCH_UP_KEEP_UP_STATUS_BASELINE <- MOVE_UP_STAY_UP_STATUS_BASELINE <- NULL

	con <- dbConnect(SQLite(), dbname = file.path(tempdir(), "TMP_Summary_Data.sqlite"))

	if (!is.null(sim.info)) {
		tmp.list.1 <- list()
		tmp_data <- data.table(dbGetQuery(con, paste("select", paste(pull.vars, collapse = ","), "from summary_data")), key = sgp_key)
		if (is.data.frame(tmp.simulation.dt)) {
			tmp.list.1 <- lapply(seq.int(sim.info[['n.simulated.sgps']]), function(i) {
					tmp_data[,c(key(tmp_data), unlist(strsplit(sgp.groups.to.summarize, ", "))), with=FALSE][
					tmp.simulation.dt[seq.int(i, length.out=sim.info[['n.unique.cases']], by=sim.info[['n.simulated.sgps']])], allow.cartesian=TRUE][,
					list(median(SGP_SIM, na.rm=TRUE), mean(SGP_SIM, na.rm=TRUE)), keyby=c(unlist(strsplit(sgp.groups.to.summarize, ", ")), "BASELINE")]})
		} else {
			tmp.list.1 <- lapply(seq.int(sim.info[['n.simulated.sgps']]), function(i) {
					tmp_data[data.table(dbGetQuery(con, paste("select * from sim_data where SIM_NUM =", i)), key = sgp_key), allow.cartesian=TRUE][,
					list(median(SGP_SIM, na.rm=TRUE), mean(SGP_SIM, na.rm=TRUE)), keyby=c(unlist(strsplit(sgp.groups.to.summarize, ", ")), "BASELINE")]})
			dbDisconnect(con)
		}

		tmp.csem <- ddcast(rbindlist(tmp.list.1)[,list(sd(V1, na.rm=TRUE), sd(V2, na.rm=TRUE)), keyby=c(unlist(strsplit(sgp.groups.to.summarize, ", ")), "BASELINE")],
							... ~ BASELINE, value.var=c("V1", "V2"), sep=".")
		if (!any(grepl("BASELINE", names(tmp.csem)))) {
			setnames(tmp.csem, c("V1.COHORT", "V2.COHORT"), c("MEDIAN_SGP_STANDARD_ERROR_CSEM", "MEAN_SGP_STANDARD_ERROR_CSEM"))
		} else {
			setnames(tmp.csem, c("V1.COHORT", "V2.COHORT", "V1.BASELINE", "V2.BASELINE"),
				c("MEDIAN_SGP_STANDARD_ERROR_CSEM", "MEAN_SGP_STANDARD_ERROR_CSEM", "MEDIAN_SGP_BASELINE_STANDARD_ERROR_CSEM", "MEAN_SGP_BASELINE_STANDARD_ERROR_CSEM"))
		}
		return(tmp.csem)
	}

	tmp_data <- data.table(dbGetQuery(con, paste("select", paste(pull.vars, collapse = ","), "from summary_data")))
	if (all((my.key <- intersect(sgp_key, variables.for.summaries)) %in% names(tmp_data))) setkeyv(tmp_data, my.key)
	if ("CATCH_UP_KEEP_UP_STATUS" %in% names(tmp_data)) {
		tmp_data[, CATCH_UP_KEEP_UP_STATUS := factor(CATCH_UP_KEEP_UP_STATUS)]
	}
	if ("CATCH_UP_KEEP_UP_STATUS_BASELINE" %in% names(tmp_data)) {
		tmp_data[, CATCH_UP_KEEP_UP_STATUS_BASELINE := factor(CATCH_UP_KEEP_UP_STATUS_BASELINE)]
	}
	if ("MOVE_UP_STAY_UP_STATUS" %in% names(tmp_data)) {
		tmp_data[, MOVE_UP_STAY_UP_STATUS := factor(MOVE_UP_STAY_UP_STATUS)]
	}
	if ("MOVE_UP_STAY_UP_STATUS_BASELINE" %in% names(tmp_data)) {
		tmp_data[, MOVE_UP_STAY_UP_STATUS_BASELINE := factor(MOVE_UP_STAY_UP_STATUS_BASELINE)]
	}
	dbDisconnect(con)
	return(tmp_data)
} ### END pullData function


`median_na` <-
function(x,
	weight) {
	if (is.null(weight)) {
		as.numeric(median(x, na.rm=TRUE))
	} else {
		as.numeric(weightedMedian(x, w=weight, na.rm=TRUE))
	}
} ### END median_na function


`mean_na` <-
function(x,
	weight,
	result.digits=2L) {

	if (is.null(weight)) {
		round(mean(x, na.rm=TRUE), digits=result.digits)
	} else {
		round(weighted.mean(as.numeric(x), w=weight, na.rm=TRUE), digits=result.digits)
	}
} ### END mean_na function


`sd_na` <- function(x, result.digits=2L) round(sd(as.numeric(x), na.rm=TRUE), digits=result.digits)


`num_non_missing` <- function(x) sum(!is.na(x))


`sgp_standard_error` <- function(x,y=1) round(y*sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x))), digits=2L)


`percent_in_category` <-
function(x,
	in.categories,
	of.categories,
	result.digits=1L) {

	tmp <- table(x)
	round(100*sum(tmp[names(tmp) %in% in.categories])/sum(tmp[names(tmp) %in% of.categories]), digits=1L)
} ### END percent_in_category function


`boot.sgp` <-
function(dat,
	conf.quantiles=NULL,
	nboot=100) {

	ID <- SCORE <- NULL
	CI <- paste0("[", paste(rep(NA, 2L), collapse=", "), "]"); SE <- NA_real_
	if (length(dat.no.na <- dat[!is.na(dat)]) > 1L) {
		out <- data.table(ID=seq.int(nboot), SCORE=dat.no.na[sample.int(length(dat.no.na), length(dat.no.na)*nboot, replace=TRUE)])[,median(SCORE), by=ID][['V1']]
		if (!is.null(conf.quantiles)) CI <- paste0("[", paste(round(quantile(out, conf.quantiles), digits=1L), collapse=", "), "]") else SE <- round(sd(out), digits=1L)
	}
	if (!is.null(conf.quantiles)) return(CI) else return(SE)
} ### END boot.sgp function
