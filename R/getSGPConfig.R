`getSGPConfig` <- 
function(sgp_object,
	tmp_sgp_object,
	content_areas, 
	years, 
	grades,
	sgp.config,
	sgp.percentiles,
	sgp.projections,
	sgp.projections.lagged,
	sgp.percentiles.baseline,
	sgp.projections.baseline,
	sgp.projections.lagged.baseline,
	sgp.config.drop.nonsequential.grade.progression.variables,
	sgp.minimum.default.panel.years) {

	YEAR <- CONTENT_AREA <- VALID_CASE <- NULL

	### Check arguments

	if (is.null(sgp.config) & !is.null(grades)) {
		grades <- type.convert(as.character(grades), as.is=TRUE)
		if (!is.numeric(grades)) {
			stop("\tNOTE: Automatic configuration of analyses is currently only available for integer grade levels. Manual specification of 'sgp.config' is required for non-traditional End of Course grade and course progressions.")
		} 
	}

	### get.config function

	get.config <- function(content_area, year, grades) {
		
		### Data for Years & Grades
		tmp.unique.data <- lapply(sgp_object@Data[SJ("VALID_CASE", content_area), nomatch=0][, c("YEAR", "GRADE"), with=FALSE], function(x) sort(type.convert(unique(x), as.is=TRUE)))

		### Years (sgp.panel.years)
		sgp.panel.years <- as.character(tmp.unique.data$YEAR[1:which(tmp.unique.data$YEAR==year)])

		### Content Areas (sgp.content.areas)
		sgp.content.areas <- rep(content_area, length(sgp.panel.years))

		### Grades (sgp.grade.sequences)
		tmp.last.year.grades <- sort(type.convert(unique(subset(sgp_object@Data, YEAR==tail(sgp.panel.years, 1) & CONTENT_AREA==content_area & VALID_CASE=="VALID_CASE")[['GRADE']]), as.is=TRUE))
		if (!is.numeric(tmp.last.year.grades) | !is.numeric(tmp.unique.data[['GRADE']])) {
			stop("\tNOTE: Automatic 'sgp.config' calculation is only available for integer grade levels. Manual specification of 'sgp.config' is required for non-traditional grade and course progressions.")
		}
		tmp.sgp.grade.sequences <- lapply(tmp.last.year.grades, function(x) tail(tmp.unique.data$GRADE[tmp.unique.data$GRADE <= x], length(tmp.unique.data$YEAR)))
		if (!is.null(grades)) {
			tmp.sgp.grade.sequences <- tmp.sgp.grade.sequences[sapply(tmp.sgp.grade.sequences, function(x) tail(x,1)) %in% grades]
		}
		sgp.grade.sequences <- lapply(tmp.sgp.grade.sequences, function(x) if (length(x) > 1) x[(tail(x,1)-x) <= length(sgp.panel.years)-1])
		sgp.grade.sequences <- sgp.grade.sequences[!unlist(lapply(sgp.grade.sequences, function(x) !length(x) > 1))]
		sgp.grade.sequences <- lapply(sgp.grade.sequences, as.character)

		### Create and return sgp.config
		if ("YEAR_WITHIN" %in% names(sgp_object@Data)) {
			sgp.panel.years.within <- rep("LAST_OBSERVATION", length(sgp.content.areas))
			return(list(
				sgp.content.areas=sgp.content.areas,
				sgp.panel.years=sgp.panel.years,
				sgp.grade.sequences=sgp.grade.sequences,
				sgp.panel.years.within=sgp.panel.years.within))
		} else {
			return(list(
				sgp.content.areas=sgp.content.areas,
				sgp.panel.years=sgp.panel.years,
				sgp.grade.sequences=sgp.grade.sequences
				))
		}
	} ### END get.config 
	
	### get.par.sgp.config function

	get.par.sgp.config <- function(sgp.config) {

		### Set-up

		par.sgp.config <- list()

		### Loop over each element of sgp.config
		for (a in seq_along(sgp.config)) { # now seq_along names so that sgp.config lists can have same names for some elements

			### Convert sgp.grade.sequences to a list if supplied as a vector
			if (is.numeric(sgp.config[[a]][['sgp.grade.sequences']])) sgp.config[[a]][['sgp.grade.sequences']] <- list(sgp.config[[a]][['sgp.grade.sequences']])

			### Loop over grade distinct grade sequences
			b.iter <- seq(from=length(par.sgp.config)+1, length.out=length(sgp.config[[a]][['sgp.grade.sequences']]))
			for (b in seq_along(b.iter)) {

				### Create a per sgp.grade.sequence branch in par.sgp.config list
				par.sgp.config[[b.iter[b]]] <- sgp.config[[a]]
				par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']] <- as.character(sgp.config[[a]][['sgp.grade.sequences']][[b]])

				### Create sgp.exact.grade.progression
				if (!is.null(sgp.config[[a]][['sgp.exact.grade.progression']])) {
					par.sgp.config[[b.iter[b]]][['sgp.exact.grade.progression']] <- sgp.config[[a]][['sgp.exact.grade.progression']][b]
				} else {
					par.sgp.config[[b.iter[b]]][['sgp.exact.grade.progression']] <- FALSE
				}
				
				###  Set sgp.exact.grade.progression=TRUE if using multiple content areas in a single year as priors.
				if (any(duplicated(paste(par.sgp.config[[b]][['sgp.panel.years']], par.sgp.config[[b]][['sgp.grade.sequences']], sep=".")))) {  
					par.sgp.config[[b.iter[b]]][['sgp.exact.grade.progression']] <- TRUE
				} else {
					if (is.null(par.sgp.config[[b]][['sgp.exact.grade.progression']])) {
						par.sgp.config[[b.iter[b]]][['sgp.exact.grade.progression']] <- FALSE
					}
				}

				### Create index and re-specify years and content areas from sgp.panel.years and sgp.content.areas
				if (is.numeric(type.convert(par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']]))) {
					tmp.numeric.grades <- sort(type.convert(par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']]))
					grade.span <- seq(min(tmp.numeric.grades), max(tmp.numeric.grades))
					index <- match(tmp.numeric.grades, grade.span)
					if (!sgp.config.drop.nonsequential.grade.progression.variables)  index <- seq_along(index) 
					par.sgp.config[[b.iter[b]]][['sgp.panel.years']] <- tail(par.sgp.config[[b.iter[b]]][['sgp.panel.years']], max(index))[index]
					par.sgp.config[[b.iter[b]]][['sgp.content.areas']] <- tail(par.sgp.config[[b.iter[b]]][['sgp.content.areas']], max(index))[index]
					if ('sgp.panel.years.within' %in% names(sgp.config[[a]])) {
						par.sgp.config[[b.iter[b]]][['sgp.panel.years.within']] <- tail(par.sgp.config[[b.iter[b]]][['sgp.panel.years.within']], max(index))[index]
					} 
				}

				### Create sgp.panel.years.lags
				if (is.null(sgp.config[[a]][['sgp.panel.year.lags']])) {
					par.sgp.config[[b.iter[b]]][['sgp.panel.years.lags']] <- diff(as.numeric(sapply(strsplit(par.sgp.config[[b.iter[b]]][['sgp.panel.years']], '_'), '[', 1)))
				}

				### Create sgp.projection.grade.sequences (if requested)
				if (is.null(sgp.config[[a]][['sgp.projection.grade.sequences']]) & (sgp.projections | sgp.projections.lagged | sgp.projections.baseline | sgp.projections.lagged.baseline)) {
					par.sgp.config[[b.iter[b]]][['sgp.projection.grade.sequences']] <- head(par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']], -1)
					par.sgp.config[[b.iter[b]]][['sgp.projection.content.areas']] <- head(par.sgp.config[[b.iter[b]]][['sgp.content.areas']], -1)
					par.sgp.config[[b.iter[b]]][['sgp.projection.panel.years.lags']] <- head(par.sgp.config[[b.iter[b]]][['sgp.panel.years.lags']], -1)
				}

				### Create baseline specific arguments
				if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {

					if (paste(strsplit(names(sgp.config)[a], "\\.")[[1]][1], ".BASELINE", sep="") %in% names(tmp_sgp_object[["Coefficient_Matrices"]])) {
						tmp.matrices <- tmp_sgp_object[['Coefficient_Matrices']][[paste(strsplit(names(sgp.config)[a], "\\.")[[1]][1], ".BASELINE", sep="")]]
						tmp.orders <- getsplineMatrices(
							my.matrices=tmp.matrices, 
							my.matrix.content_area.progression=par.sgp.config[[b.iter[b]]][['sgp.content.areas']], 
							my.matrix.grade.progression=par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']], 
							my.matrix.time.progression=rep("BASELINE", length(par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']])),
							my.matrix.time.progression.lags=par.sgp.config[[b.iter[b]]][['sgp.panel.years.lags']],
							what.to.return="ORDERS")

						if (length(tmp.orders) > 0) {
							tmp.matrices.tf <- TRUE
							tmp.max.order <- max(tmp.orders)
						} else {
							tmp.matrices.tf <- FALSE
						}
					} else tmp.matrices.tf <- FALSE

					if (!tmp.matrices.tf) {
						par.sgp.config[[b.iter[b]]][['sgp.baseline.grade.sequences']] <- "NO_BASELINE_COEFFICIENT_MATRICES"
						par.sgp.config[[b.iter[b]]][['sgp.baseline.max.order']] <- "NO_BASELINE_COEFFICIENT_MATRICES"
					} else {
						par.sgp.config[[b.iter[b]]][['sgp.baseline.grade.sequences']] <- as.character(tail(par.sgp.config[[b.iter[b]]][['sgp.grade.sequences']], tmp.max.order+1))
						par.sgp.config[[b.iter[b]]][['sgp.baseline.content.areas']] <- as.character(tail(par.sgp.config[[b.iter[b]]][['sgp.content.areas']], tmp.max.order+1))
						par.sgp.config[[b.iter[b]]][['sgp.baseline.max.order']] <- tmp.max.order
						par.sgp.config[[b.iter[b]]][['sgp.baseline.panel.years.lags']] <- tail(par.sgp.config[[b.iter[b]]][['sgp.panel.years.lags']], tmp.max.order) 
					}
				} ### END if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline

			} ### END b loop
		} ### END a loop
		return(par.sgp.config)
	} ## END get.par.sgp.config


	###
	### Construct sgp.config/par.sgp.config
	###

	if (is.null(sgp.config)) {
		tmp.sgp.config <- tmp.years <- list()
		if (is.null(content_areas)) {
			content_areas <- unique(sgp_object@Data["VALID_CASE"][['CONTENT_AREA']])
		}
		if (is.null(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[SJ("VALID_CASE", i)][['YEAR']]), - (sgp.minimum.default.panel.years-1)), decreasing=TRUE)
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
