`checkConfig` <-
function(my.config,
	config.type="Standard") {

	if (config.type=="Standard") {
		
		for (i in seq_along(my.config)) {
			my.config[[i]][['sgp.content.areas']] <- my.config[[i]][['sgp.content.areas']][!is.na(my.config[[i]][['sgp.content.areas']])]
			my.config[[i]][['sgp.panel.years']] <- my.config[[i]][['sgp.panel.years']][!is.na(my.config[[i]][['sgp.panel.years']])]
			my.config[[i]][['sgp.panel.years']] <- as.character(my.config[[i]][['sgp.panel.years']])
			my.config[[i]][['sgp.grade.sequences']][[1]] <- my.config[[i]][['sgp.grade.sequences']][[1]][!is.na(my.config[[i]][['sgp.grade.sequences']])]
			my.config[[i]][['sgp.grade.sequences']][[1]] <- as.character(my.config[[i]][['sgp.grade.sequences']][[1]])
			if (length(my.config[[i]][['sgp.projection.grade.sequences']][[1]]) > 0) {
				my.config[[i]][['sgp.projection.grade.sequences']][[1]] <- my.config[[i]][['sgp.projection.grade.sequences']][[1]][!is.na(my.config[[i]][['sgp.projection.grade.sequences']])]
				my.config[[i]][['sgp.projection.grade.sequences']][[1]] <- as.character(my.config[[i]][['sgp.projection.grade.sequences']][[1]])
			}

			if (length(my.config[[i]][['sgp.content.areas']]) != length(my.config[[i]][['sgp.grade.sequences']][[1]])) {
				tmp.min <- min(length(my.config[[i]][['sgp.content.areas']]), length(my.config[[i]][['sgp.grade.sequences']][[1]]))
				my.config[[i]][['sgp.content.areas']] <- tail( my.config[[i]][['sgp.content.areas']], tmp.min)
				my.config[[i]][['sgp.grade.sequences']][[1]] <- tail( my.config[[i]][['sgp.grade.sequences']][[1]], tmp.min)
			}
		}

		return(my.config)

	}

	if (config.type=="Baseline") {

		if (!all(unlist(sapply(lapply(my.config, names),
			function(x) x %in% c("baseline.content.areas", "baseline.panel.years", "baseline.grade.sequences", "baseline.grade.sequences.lags"))))) {
				stop("Please specify an appropriate list of SGP function labels (sgp.baseline.config).  See help page for details.")
		}       

		for (i in seq_along(my.config)) {
			my.config[[i]][['baseline.content.areas']] <- my.config[[i]][['baseline.content.areas']][!is.na(my.config[[i]][['baseline.content.areas']])]
			my.config[[i]][['baseline.panel.years']] <- my.config[[i]][['baseline.panel.years']][!is.na(my.config[[i]][['baseline.panel.years']])]
			my.config[[i]][['baseline.grade.sequences']] <- my.config[[i]][['baseline.grade.sequences']][!is.na(my.config[[i]][['baseline.grade.sequences']])]
			my.config[[i]][['baseline.panel.years']] <- as.character(my.config[[i]][['baseline.panel.years']])
			my.config[[i]][['baseline.grade.sequences']] <- as.character(my.config[[i]][['baseline.grade.sequences']])

			if (length(my.config[[i]][['baseline.content.areas']]) != length(my.config[[i]][['baseline.grade.sequences']])) {
				tmp.min <- min(length(my.config[[i]][['baseline.content.areas']]), length(my.config[[i]][['baseline.grade.sequences']]))
				my.config[[i]][['baseline.content.areas']] <- tail( my.config[[i]][['baseline.content.areas']], tmp.min)
				my.config[[i]][['baseline.grade.sequences']] <- tail( my.config[[i]][['baseline.grade.sequences']], tmp.min)
			}
		}

		return(my.config)
	}

} ### END checkConfig
