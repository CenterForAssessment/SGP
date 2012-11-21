`getSGPConfig` <- 
function(sgp_object,
	tmp_sgp_object,
	content_areas, 
	years, 
	grades,
	sgp.config,
	sgp.percentiles.baseline,
	sgp.projections.baseline,
	sgp.projections.lagged.baseline,
	sgp.config.drop.nonsequential.grade.progression.variables) {

	YEAR <- CONTENT_AREA <- VALID_CASE <- NULL

	.get.config <- function(content_area, year, grades) {
		tmp.unique.data <- lapply(sgp_object@Data[SJ("VALID_CASE", content_area), nomatch=0][, c("YEAR", "GRADE"), with=FALSE], function(x) sort(unique(x)))
		.sgp.panel.years <- tmp.unique.data$YEAR[1:which(tmp.unique.data$YEAR==year)]
		.sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
		tmp.last.year.grades <- sort(unique(subset(sgp_object@Data, YEAR==tail(.sgp.panel.years, 1) & CONTENT_AREA==content_area & VALID_CASE=="VALID_CASE")[['GRADE']]))
		tmp.sgp.grade.sequences <- lapply(tail(tmp.last.year.grades, -1), function(x) tail(tmp.unique.data$GRADE[tmp.unique.data$GRADE <= x], length(tmp.unique.data$YEAR)))
		tmp.sgp.projection.grade.sequences <- lapply(head(tmp.last.year.grades, -1), function(x) tail(tmp.unique.data$GRADE[tmp.unique.data$GRADE <= x], length(tmp.unique.data$YEAR)))
		if (!is.null(grades)) {
			tmp.sgp.grade.sequences <- tmp.sgp.grade.sequences[sapply(tmp.sgp.grade.sequences, function(x) tail(x,1)) %in% grades]
			tmp.sgp.projection.grade.sequences <- tmp.sgp.projection.grade.sequences[sapply(tmp.sgp.grade.sequences, function(x) tail(x,1)) %in% grades]
		}
		.sgp.grade.sequences <- lapply(tmp.sgp.grade.sequences, function(x) if (length(x) > 1) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1])
		.sgp.projection.grade.sequences <- lapply(tmp.sgp.projection.grade.sequences, function(x) if (length(x) > 1) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1] else x)
		.sgp.grade.sequences <- .sgp.grade.sequences[!unlist(lapply(.sgp.grade.sequences, function(x) !length(x) > 1))]
		.sgp.grade.progression.labels=rep(FALSE, length(.sgp.grade.sequences))
		list(
			sgp.content.areas=.sgp.content.areas, 
			sgp.panel.years=.sgp.panel.years, 
			sgp.grade.sequences=.sgp.grade.sequences, 
			sgp.projection.grade.sequences=.sgp.projection.grade.sequences, 
			sgp.grade.progression.labels=.sgp.grade.progression.labels)
	}

	get.par.sgp.config <- function(sgp.config) {

		par.sgp.config <- list(); cnt <- 1
		for (a in names(sgp.config)) {
			for (b in seq_along(sgp.config[[a]][["sgp.grade.sequences"]])) {

				### Create a per sgp.grade.sequence branch in par.sgp.config list

				par.sgp.config[[cnt]] <- sgp.config[[a]]
				par.sgp.config[[cnt]][["sgp.grade.progression.labels"]] <- sgp.config[[a]][["sgp.grade.progression.labels"]][b]
				par.sgp.config[[cnt]][["sgp.grade.sequences"]] <- tmp.gp <- sgp.config[[a]][["sgp.grade.sequences"]][b]
				par.sgp.config[[cnt]][["sgp.projection.grade.sequences"]] <- sgp.config[[a]][["sgp.projection.grade.sequences"]][b]
				par.sgp.config[[cnt]][["sgp.exact.grade.progression"]] <- sgp.config[[a]][["sgp.exact.grade.progression"]][b]
				
				###  Set sgp.exact.grade.progression=TRUE if using multiple content areas in a single year as priors.

				if (any(duplicated(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]))) {  
					par.sgp.config[[cnt]][["sgp.exact.grade.progression"]] <- TRUE
				} else if (is.null(par.sgp.config[[cnt]][["sgp.exact.grade.progression"]])) par.sgp.config[[cnt]][["sgp.exact.grade.progression"]] <- FALSE

				### Create index and identify years from sgp.panel.years
				
				grade.span <- seq(min(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]), max(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]))
				index <- match(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], grade.span) ## Select out proper years
				if (!sgp.config.drop.nonsequential.grade.progression.variables) index <- seq_along(index) ## Take most recent years, OR, take years and subjects as specified in custom sgp.config
				par.sgp.config[[cnt]][["sgp.panel.years"]] <- tail(par.sgp.config[[cnt]][["sgp.panel.years"]], max(index))[index]
				par.sgp.config[[cnt]][["sgp.content.areas"]] <- tail(par.sgp.config[[cnt]][["sgp.content.areas"]], length(index))

				### Additional arguments associated with baseline analyses

				if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {
					mtx.names <- names(tmp_sgp_object[["Coefficient_Matrices"]][[paste(strsplit(a, "\\.")[[1]][1], ".BASELINE", sep="")]])
					if (is.null(mtx.names)) {
						par.sgp.config[[cnt]][["base.gp"]] <- "NO_BASELINE_COEFFICIENT_MATRICES"
						par.sgp.config[[cnt]][["max.order"]] <- "NO_BASELINE_COEFFICIENT_MATRICES"
					} else {
						max.order <- max(as.numeric(sapply(strsplit(mtx.names[
							grep(paste("qrmatrix_", tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1), sep=""), mtx.names)], "_"), function(x) x[3])))
						if (length(par.sgp.config[[cnt]][["sgp.panel.years"]])-1 < max.order) max.order <- length(par.sgp.config[[cnt]][["sgp.panel.years"]])-1
						if (sum(diff(tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order))) > length(par.sgp.config[[cnt]][["sgp.panel.years"]])) {
							base.gp <- par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]][tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1) -
								par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]] <= max.order]
							max.order <- length(base.gp) - 1
						} else {
							base.gp <- tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order)
						}

						par.sgp.config[[cnt]][["base.gp"]] <- base.gp
						par.sgp.config[[cnt]][["max.order"]] <- max.order
					}
				}
				
				cnt <- cnt + 1
			}
		}
        par.sgp.config
	} ## END get.par.sgp.config

	if (is.null(sgp.config)) {
		tmp.sgp.config <- tmp.years <- list()
		if (is.null(content_areas)) {
			content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
		}
		if (is.null(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[SJ("VALID_CASE", i)][["YEAR"]]), -2), decreasing=TRUE)
			}
		} else {
			for (i in content_areas) {
				tmp.years[[i]] <- years
			}
		}
		for (i in content_areas) {
			for (j in tmp.years[[i]]) {
				tmp.sgp.config[[paste(i,j,sep=".")]] <- .get.config(i,j,grades)
			}
		}
		return(get.par.sgp.config(tmp.sgp.config))
	} else {
		return(get.par.sgp.config(sgp.config))
	}
} ## END getSGPConfig
