`getSGPBaselineConfig` <- 
function(sgp_object, 
	content_areas,
	grades, 
	sgp.baseline.panel.years) {

	sgp.baseline.config <- tmp.sgp.baseline.config <- .content_areas <- .years <- .grades <- .sgp.grade.sequences <- list()

	if (is.null(content_areas)) {
		.content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
	} else {
		.content_areas <- content_areas
	}
	if (is.null(sgp.baseline.panel.years)) {
		.years <- head(sort(unique(sgp_object@Data[SJ("VALID_CASE", .content_areas)][["YEAR"]])), 5)
	} else {
		.years <- sgp.baseline.panel.years
	}
	if (is.null(grades)) {
		.grades <- sort(type.convert(unique(sgp_object@Data[SJ("VALID_CASE", .content_areas)][["GRADE"]])))
	} else {
		.grades <- grades
	}
	.baseline.max.order <- length(.years)-2
	tmp.sgp.grade.sequences <- lapply(.grades[-1], function(x) tail(.grades[.grades <= x], (.baseline.max.order+1)))
	tmp.sgp.baseline.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= .baseline.max.order])
	sgp.baseline.grade.sequences <- list()
	for (a in seq_along(tmp.sgp.baseline.grade.sequences)) {
		sgp.baseline.grade.sequences[[a]] <-
			eval(parse(text=paste("list(", paste("tail(tmp.sgp.baseline.grade.sequences[[", a, "]],", length(tmp.sgp.baseline.grade.sequences[[a]]):2, ")", collapse=", "), ")")))
		}
		sgp.baseline.grade.sequences <- unlist(sgp.baseline.grade.sequences, recursive=FALSE)

		for (i in .content_areas) {
			tmp.sgp.baseline.config[[as.character(i)]] <- 
				list(
					baseline.content.areas=i, 
					baseline.panel.years=.years,
					baseline.grade.sequences=sgp.baseline.grade.sequences)
		}

		for (a in seq_along(tmp.sgp.baseline.config)) {
			tmp.length <- length(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]])
			for (b in seq(tmp.length)) {
				sgp.baseline.config[[b+(a-1)*tmp.length]] <- tmp.sgp.baseline.config[[a]]
				sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.grade.sequences"]] <- unlist(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]][b])
				sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.content.areas"]] <- 
					rep(sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.content.areas"]], length(sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.grade.sequences"]]))
		}
	}
	checkConfig(sgp.baseline.config, "Baseline")
} ## END getSGPBaselineConfig
