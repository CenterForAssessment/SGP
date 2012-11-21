`getKnotsBoundaries` <- 
function(sgp.iter,
	state) {

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.knots.boundaries.names <- names(SGPstateData[[state]][['Achievement']][['Knots_Boundaries']])
		tmp.knots.boundaries.names <- tmp.knots.boundaries.names[grep(content_area, tmp.knots.boundaries.names)]
		tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), function(x) x[2])
		if (any(!is.na(tmp.knots.boundaries.years))) {
			if (year %in% tmp.knots.boundaries.years) {
				return(paste(content_area, ".", year, sep=""))
			} else {
				if (year==sort(c(year, tmp.knots.boundaries.years))[1]) {
					return(content_area)
				} else {
					return(paste(content_area, ".", rev(sort(tmp.knots.boundaries.years))[1], sep=""))
				}
			}
		} else {
			return(content_area)
		}
	}
	
	kb <- list()
	tmp.gp <- sgp.iter[["sgp.grade.sequences"]][[1]]
	tmp.ca <- tail(	sgp.iter[["sgp.content.areas"]], 1)
	tmp.yr <- tail(sgp.iter[["sgp.panel.years"]], 1)
	num.prior <- length(tmp.gp)-1
		
	#  Check for repeat grades - either held back, multiple grade/subject priors, etc.  Add .1, .2 , etc.
	if (any(duplicated(tmp.gp[1:num.prior]))) {
		while(any(duplicated(tmp.gp[1:num.prior]))) {
			tmp.gp[which(duplicated(tmp.gp[1:num.prior]))] <- tmp.gp[which(duplicated(tmp.gp[1:num.prior]))] + 0.1
		}
		tmp.gp[1:num.prior] <- tmp.gp[1:num.prior]+0.1
	}
	tmp.gp <- as.character(tmp.gp)
		
	#  If all sgp.iter[["sgp.content.areas"]] are the same, use SGPstateData as usual:
	if (all(sapply(sgp.iter[["sgp.content.areas"]], function(x) identical(tmp.ca, x)))) {
		for (i in grep(tmp.ca, names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]), value=TRUE)) {
			kb[["Knots_Boundaries"]][[paste(tmp.ca, tmp.yr, sep=".")]][[i]] <- 
				SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[i]]
		}
	} else { # if not (e.g. "ELA", "HISTORY",  of "MATH", "ALGEBRA_I", then get the right knots and boundaries, but name them as 'my.subject')
		for (ca in seq_along(head(sgp.iter[["sgp.content.areas"]], -1))) {
			for (j in c('boundaries_', 'knots_', 'loss.hoss_')) {
				kb[["Knots_Boundaries"]][[paste(tmp.ca, tmp.yr, sep=".")]][[paste(j, tmp.gp[ca], sep="")]] <- 
					SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[get.my.knots.boundaries.path(sgp.iter[["sgp.content.areas"]][ca], sgp.iter[['sgp.panel.years']][ca])]][
						grep(paste(j, strsplit(tmp.gp, "[.]")[[ca]][1], sep=""), 
						names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[sgp.iter[["sgp.content.areas"]][ca]]]))][[1]]
			}
		}
		#  Add additional slot in Knots_Boundaries if same content area is used as one of the priors to coincide with how studentGrowthPercentiles works.
		if (!is.na(match(tmp.ca, head(sgp.iter[["sgp.content.areas"]], -1)))) { # must be exact match, not grep (which catches things like 'MATH' and 'EOC_MATH')
			kb[["Knots_Boundaries"]][[paste(tmp.ca, tmp.yr, sep=".")]][[tmp.ca]] <- kb[["Knots_Boundaries"]][[paste(tmp.ca, tmp.yr, sep=".")]]
		}
	}
	return(kb[["Knots_Boundaries"]])

} ## END getKnotsBoundaries
