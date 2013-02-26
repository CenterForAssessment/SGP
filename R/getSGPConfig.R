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
	sgp.config.drop.nonsequential.grade.progression.variables,
	sgp.minimum.default.panel.years) {

	YEAR <- CONTENT_AREA <- VALID_CASE <- NULL

	### Check arguments

	if (!is.null(grades)) {
		grades <- type.convert(as.character(grades), as.is=TRUE)
		if (!is.numeric(grades)) {
			stop("\tNOTE: Automatic configuration of analyses is currently only available for integer grade levels. Manual specification of 'sgp.config' is required for non-traditional End of Course grade and course progressions.")
		} 
	}

	get.config <- function(content_area, year, grades) {
		tmp.unique.data <- lapply(sgp_object@Data[SJ("VALID_CASE", content_area), nomatch=0][, c("YEAR", "GRADE"), with=FALSE], function(x) sort(type.convert(unique(x), as.is=TRUE)))
		.sgp.panel.years <- as.character(tmp.unique.data$YEAR[1:which(tmp.unique.data$YEAR==year)])
		.sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
		tmp.last.year.grades <- sort(type.convert(unique(subset(sgp_object@Data, YEAR==tail(.sgp.panel.years, 1) & CONTENT_AREA==content_area & VALID_CASE=="VALID_CASE")[['GRADE']]), as.is=TRUE))
		if (!is.numeric(tmp.last.year.grades) | !is.numeric(tmp.unique.data[['GRADE']])) {
			stop("\tNOTE: Automatic configuration of analyses is currently only available for integer grade levels. Manual specification of 'sgp.config' is required for non-traditional grade and course progressions.")
		}
		tmp.sgp.grade.sequences <- lapply(tail(tmp.last.year.grades, -1), function(x) tail(tmp.unique.data$GRADE[tmp.unique.data$GRADE <= x], length(tmp.unique.data$YEAR)))
		tmp.sgp.projection.grade.sequences <- lapply(head(tmp.last.year.grades, -1), function(x) tail(tmp.unique.data$GRADE[tmp.unique.data$GRADE <= x], length(tmp.unique.data$YEAR)))
		if (!is.null(grades)) {
			tmp.sgp.grade.sequences <- tmp.sgp.grade.sequences[sapply(tmp.sgp.grade.sequences, function(x) tail(x,1)) %in% grades]
			tmp.sgp.projection.grade.sequences <- tmp.sgp.projection.grade.sequences[sapply(tmp.sgp.projection.grade.sequences, function(x) tail(x,1)) %in% grades]
		}
		.sgp.grade.sequences <- lapply(tmp.sgp.grade.sequences, function(x) if (length(x) > 1) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1])
		.sgp.grade.sequences <- .sgp.grade.sequences[!unlist(lapply(.sgp.grade.sequences, function(x) !length(x) > 1))]
		.sgp.grade.sequences <- lapply(.sgp.grade.sequences, as.character)
		.sgp.projection.grade.sequences <- lapply(tmp.sgp.projection.grade.sequences, function(x) if (length(x) > 1) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1] else x)
		.sgp.projection.grade.sequences <- lapply(.sgp.projection.grade.sequences, as.character)

		list(
			sgp.content.areas=.sgp.content.areas, 
			sgp.panel.years=.sgp.panel.years, 
			sgp.grade.sequences=.sgp.grade.sequences, 
			sgp.projection.grade.sequences=.sgp.projection.grade.sequences)

	} ### END get.config 
	
	split.location <- function(years) sapply(strsplit(years, '_'), length)[1]

	get.par.sgp.config <- function(sgp.config) {
		
		par.sgp.config <- list(); cnt <- 1
		for (a in seq_along(sgp.config)) { # now seq_along names so that sgp.config lists can have same names for some elements

			tmp.matrices <- tmp_sgp_object[['Coefficient_Matrices']][[paste(strsplit(names(sgp.config)[a], "\\.")[[1]][1], ".BASELINE", sep="")]]

			for (b in seq_along(sgp.config[[a]][['sgp.grade.sequences']])) {

				### Create a per sgp.grade.sequence branch in par.sgp.config list

				par.sgp.config[[cnt]] <- sgp.config[[a]]
				par.sgp.config[[cnt]][['sgp.grade.sequences']] <- tmp.gp <- sgp.config[[a]][['sgp.grade.sequences']][b]
				par.sgp.config[[cnt]][['sgp.projection.grade.sequences']] <- sgp.config[[a]][['sgp.projection.grade.sequences']][b]
				par.sgp.config[[cnt]][['sgp.exact.grade.progression']] <- sgp.config[[a]][['sgp.exact.grade.progression']][b]
				
				###  Set sgp.exact.grade.progression=TRUE if using multiple content areas in a single year as priors.

				if (any(duplicated(paste(par.sgp.config[[cnt]][['sgp.panel.years']], par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]], sep=".")))) {  
					par.sgp.config[[cnt]][['sgp.exact.grade.progression']] <- TRUE
				} else {
					if (is.null(par.sgp.config[[cnt]][['sgp.exact.grade.progression']])) par.sgp.config[[cnt]][['sgp.exact.grade.progression']] <- FALSE
				}

				### Create index and identify years and content areas from sgp.panel.years

				#  Coerce sgp.grade.sequence to character first to deal with mixed config cases (some numeric/integer, some character)
				par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]] <- as.character(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]])

				if (is.numeric(type.convert(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]]))) {
					grade.span <- seq(min(type.convert(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]])),
						max(type.convert(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]])))
					index <- match(type.convert(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]]), grade.span)
					if (!sgp.config.drop.nonsequential.grade.progression.variables)  index <- seq_along(index) 
					par.sgp.config[[cnt]][['sgp.panel.years']] <- tail(par.sgp.config[[cnt]][['sgp.panel.years']], max(index))[index]
					par.sgp.config[[cnt]][['sgp.content.areas']] <- tail(par.sgp.config[[cnt]][['sgp.content.areas']], max(index))[index]
				}

				### Additional arguments associated with baseline analyses

				if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {

					### Check to see if a BASELINE splineMatrix exists for the element par.sgp.config[[cnt]]

					if (paste(strsplit(names(sgp.config)[a], "\\.")[[1]][1], ".BASELINE", sep="") %in% names(tmp_sgp_object[["Coefficient_Matrices"]])) {
						mtx.names <- names(tmp_sgp_object[["Coefficient_Matrices"]][[paste(strsplit(names(sgp.config)[a], "\\.")[[1]][1], ".BASELINE", sep="")]])
						mtx.index <- which(sapply(mtx.names, function(x) strsplit(x, "_")[[1]][2]) == tail(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]], 1))
						if (length(mtx.index) != 0) { # Cases where content area has some BASELINE coef matrices, but not with the present grade prog
							tmp.max.order <- max(as.numeric(sapply(strsplit(mtx.names[mtx.index], "_"), function(x) x[3])))
							if (length(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]])-1 < tmp.max.order) {
								tmp.max.order <- length(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]])-1
							}
							tmp.matrices.tf <- length(getsplineMatrix(
								my.matrices=tmp.matrices[mtx.index], 
								my.matrix.content.area.progression=tail(par.sgp.config[[cnt]][['sgp.content.areas']], tmp.max.order+1), 
								my.matrix.grade.progression=tail(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]], tmp.max.order+1), 
								my.matrix.time.progression=rep("BASELINE", tmp.max.order+1),
								my.matrix.time.progression.lags=diff(as.integer(sapply(strsplit(tail(par.sgp.config[[cnt]][['sgp.panel.years']], tmp.max.order+1), '_'), '[', 
									split.location(par.sgp.config[[cnt]][['sgp.panel.years']])))),
								what.to.return="ORDERS")) > 0 # Cases where content area has some BASELINE coef matrices, but not for this particular config (Time, Lag, etc off)
						} else tmp.matrices.tf <- FALSE
					} else tmp.matrices.tf <- FALSE

					if (!tmp.matrices.tf) {
						par.sgp.config[[cnt]][['base.gp']] <- "NO_BASELINE_COEFFICIENT_MATRICES"
						par.sgp.config[[cnt]][['max.order']] <- "NO_BASELINE_COEFFICIENT_MATRICES"
					} else {
						tmp.base.gp <- tail(par.sgp.config[[cnt]][['sgp.grade.sequences']][[1]], tmp.max.order+1)
						par.sgp.config[[cnt]][['base.gp']] <- as.character(tmp.base.gp)
						par.sgp.config[[cnt]][['max.order']] <- tmp.max.order
						par.sgp.config[[cnt]][['time.lags']] <- diff(as.integer(sapply(strsplit(tail(par.sgp.config[[cnt]][['sgp.panel.years']], tmp.max.order+1), '_'), '[', 
							split.location(par.sgp.config[[cnt]][['sgp.panel.years']]))))
					}
				} ### END if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline)
				cnt <- cnt + 1
			} ### END b loop
		} ### END a loop
        par.sgp.config
	} ## END get.par.sgp.config

	if (is.null(sgp.config)) {
		tmp.sgp.config <- tmp.years <- list()
		if (is.null(content_areas)) {
			content_areas <- unique(sgp_object@Data["VALID_CASE"][['CONTENT_AREA']])
		}
		if (is.null(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[SJ("VALID_CASE", i)][['YEAR']]), -(sgp.minimum.default.panel.years-1)), decreasing=TRUE)
			}
		} else {
			for (i in content_areas) {
				tmp.years[[i]] <- years
			}
		}
		for (i in content_areas) {
			for (j in tmp.years[[i]]) {
				tmp.sgp.config[[paste(i,j,sep=".")]] <- get.config(i,j,grades)
			}
		}
		checkConfig(get.par.sgp.config(tmp.sgp.config), "Standard")
	} else {
		checkConfig(get.par.sgp.config(sgp.config), "Standard")
	}
} ## END getSGPConfig
