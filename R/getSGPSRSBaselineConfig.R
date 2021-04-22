`getSGPSRSBaselineConfig` <-
function(sgp_object,
	content_areas,
	grades,
	sgp.srs.baseline.panel.years,
	sgp.percentiles.srs.baseline.max.order,
	calculate.simex.srs.baseline=NULL) {

	sgp.srs.baseline.config <- tmp.sgp.srs.baseline.config <- .content_areas <- .years <- .grades <- .sgp.grade.sequences <- list()

	if (is.null(content_areas)) {
		.content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
	} else {
		.content_areas <- content_areas
	}

	if (is.null(sgp.srs.baseline.panel.years)) {
		.years <- head(sort(unique(sgp_object@Data[SJ("VALID_CASE", .content_areas)][["YEAR"]])), 5)
	} else {
		.years <- sgp.srs.baseline.panel.years
	}

	.srs.baseline.max.order <- min(sgp.percentiles.srs.baseline.max.order, length(.years)-2)

	for (i in .content_areas) {
		.grades <- sort(type.convert(unique(sgp_object@Data[SJ("VALID_CASE", i)][["GRADE"]])))
		tmp.sgp.grade.sequences <- lapply(.grades[-1], function(x) tail(.grades[.grades <= x], (.srs.baseline.max.order+1)))
		tmp.sgp.srs.baseline.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= length(.years)-2])
		if (!is.null(grades)) {
			tmp.sgp.srs.baseline.grade.sequences <- tmp.sgp.srs.baseline.grade.sequences[sapply(tmp.sgp.srs.baseline.grade.sequences, function(x) tail(x, 1) %in% grades)]
		}

		sgp.srs.baseline.grade.sequences <- list()
		for (a in seq_along(tmp.sgp.srs.baseline.grade.sequences)) {
			sgp.srs.baseline.grade.sequences[[a]] <-
				eval(parse(text=paste("list(", paste("tail(tmp.sgp.srs.baseline.grade.sequences[[", a, "]],", length(tmp.sgp.srs.baseline.grade.sequences[[a]]):2, ")", collapse=", "), ")")))
		}

		sgp.srs.baseline.grade.sequences <- unlist(sgp.srs.baseline.grade.sequences, recursive=FALSE)
		sgp.srs.baseline.grade.sequences.lags <- lapply(sgp.srs.baseline.grade.sequences, diff)

		tmp.sgp.srs.baseline.config[[as.character(i)]] <-
			list(
				sgp.srs.baseline.content.areas=i,
				sgp.srs.baseline.panel.years=.years,
				sgp.srs.baseline.grade.sequences=sgp.srs.baseline.grade.sequences,
				sgp.srs.baseline.grade.sequences.lags=sgp.srs.baseline.grade.sequences.lags,
				sgp.srs.baseline.calculate.simex.srs.baseline=calculate.simex.srs.baseline)
	}

	for (a in seq_along(tmp.sgp.srs.baseline.config)) {
		# Set tmp.length only once to the first list length.  Overwrites elements if subsequent list length differ from the first
		if (a == 1) tmp.length <- length(tmp.sgp.srs.baseline.config[[a]][["sgp.srs.baseline.grade.sequences"]])
		for (b in 1:length(tmp.sgp.srs.baseline.config[[a]][["sgp.srs.baseline.grade.sequences"]])) {
			sgp.srs.baseline.config[[b+(a-1)*tmp.length]] <- tmp.sgp.srs.baseline.config[[a]]
			sgp.srs.baseline.config[[b+(a-1)*tmp.length]][["sgp.srs.baseline.grade.sequences"]] <- unlist(tmp.sgp.srs.baseline.config[[a]][["sgp.srs.baseline.grade.sequences"]][b])
			sgp.srs.baseline.config[[b+(a-1)*tmp.length]][["sgp.srs.baseline.grade.sequences.lags"]] <- unlist(tmp.sgp.srs.baseline.config[[a]][["sgp.srs.baseline.grade.sequences.lags"]][b])
			sgp.srs.baseline.config[[b+(a-1)*tmp.length]][["sgp.srs.baseline.content.areas"]] <-
				rep(sgp.srs.baseline.config[[b+(a-1)*tmp.length]][["sgp.srs.baseline.content.areas"]], length(sgp.srs.baseline.config[[b+(a-1)*tmp.length]][["sgp.srs.baseline.grade.sequences"]]))
			sgp.srs.baseline.config[[b+(a-1)*tmp.length]][["sgp.srs.baseline.calculate.simex.srs.baseline"]] <- tmp.sgp.srs.baseline.config[[a]][["sgp.srs.baseline.calculate.simex.srs.baseline"]]
			if ("YEAR_WITHIN" %in% names(sgp_object@Data)) {
				sgp.srs.baseline.config[[b+(a-1)*tmp.length]][['sgp.srs.baseline.panel.years.within']] <- rep("LAST_OBSERVATION", length(content_areas))
			}
		}
	}

	checkConfig(sgp.srs.baseline.config, "Baseline")
} ## END getSGPBaselineSRSConfig
