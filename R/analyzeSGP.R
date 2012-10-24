`analyzeSGP` <- 
function(sgp_object,
         state=NULL,
         years=NULL,
         content_areas=NULL,
         grades=NULL,
         sgp.percentiles=TRUE, 
         sgp.projections=TRUE,
         sgp.projections.lagged=TRUE,
         sgp.percentiles.baseline=TRUE,
         sgp.projections.baseline=TRUE,
         sgp.projections.lagged.baseline=TRUE,
         sgp.percentiles.baseline.max.order=3,
         sgp.projections.baseline.max.order=3,
         sgp.projections.lagged.baseline.max.order=3,
         sgp.use.my.coefficient.matrices=NULL, ## SET to TRUE to utilize coefficient matrices associated with sgp.labels
         simulate.sgps=TRUE,
         goodness.of.fit.print=TRUE,
         sgp.config=NULL,
         sgp.config.drop.nonsequential.grade.progression.variables=TRUE,
         sgp.baseline.panel.years=NULL,
         sgp.baseline.config=NULL, 
         parallel.config=NULL,
         ...) {

	started.at <- proc.time()
	message(paste("\nStarted analyzeSGP", date()))

	VALID_CASE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- NULL ## To prevent R CMD check warnings


	###
	### Create relevant analyzeSGP variables
	###

	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		if (any(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(toupper(x), tmp.name))!=-1)) {
			state <- c(state.abb, "AOB", "DEMO")[which(sort(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(toupper(x), tmp.name)))!=-1)[1]]
		}
	}


	###
	### Tests associated with provided arguments
	###

	if (simulate.sgps==TRUE) {
		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			message("\tNOTE: CSEMs are required in SGPstateData to simulate SGPs for confidence interval calculations. Confidence intervals will not be calculated.")
			simulate.sgps <- FALSE
		}
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.config.drop.nonsequential.grade.progression.variables"]])) {
		sgp.config.drop.nonsequential.grade.progression.variables <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.config.drop.nonsequential.grade.progression.variables"]]
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]])) {
		sgp.loss.hoss.adjustment <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]]
	} else {
		sgp.loss.hoss.adjustment <- NULL
	}

	### 
	### Utility functions
	###

	## year.increment function

	year.increment <- function(year, increment, lag) {
                paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment-lag, collapse="_")
        }

	## Function to merge results from assorted multiple SGP function calls

	.mergeSGP <- function(list_1, list_2) {
		if (is.null(names(list_1))) return(list_2)
		if (!is.null(names(list_2))) {
			for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs", "Error_Reports")) {
				list_1[[j]] <- c(list_1[[j]], list_2[[j]])[!duplicated(names(c(list_1[[j]], list_2[[j]])))]
			}
			for (j in c("SGPercentiles", "SGProjections", "Simulated_SGPs")) {
				if (all(names(list_2[[j]]) %in% names(list_1[[j]]))) {
					for (k in names(list_2[[j]])) { # merging list_2 in with list_1, so use it here
						if (!identical(list_1[[j]][[k]], list_2[[j]][[k]])) { # keeps it from copying first set of results
							list_1[[j]][[k]] <- rbind.fill(list_1[[j]][[k]], list_2[[j]][[k]])
						}
					}
				}
			}
			for (j in c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries")) {
				for (k in names(list_2[[j]])) {
					if (!identical(list_1[[j]][[k]], list_2[[j]][[k]])) {
 						names.list <- c(unique(names(list_1[[j]][[k]])), unique(names(list_2[[j]][[k]]))) # Get list of (unique) names first.
						list_1[[j]][[k]] <- c(list_1[[j]][[k]], list_2[[j]][[k]][!names(list_2[[j]][[k]]) %in% names(list_1[[j]][[k]])]) #new.elements
						if (any(duplicated(names.list))) {
							dups <- names.list[which(duplicated(names.list))]
							for (l in seq(dups)) {
								if (!identical(list_1[[j]][[k]][[dups[l]]], list_2[[j]][[k]][[dups[l]]])) { # could be same matrices, different @Version (???)
									x <- length(list_1[[j]][[k]])+1
									list_1[[j]][[k]][[x]] <- list_2[[j]][[k]][[dups[l]]]
									names(list_1[[j]][[k]]) <- c(names(list_1[[j]][[k]])[-x], dups[l])
								}
							}
						}
					}
				}
			}
		}
	list_1[which(names(list_1) != "Panel_Data")]
	}

	## Function to export/print goodness of fit results as pdf files to directory Goodness_of_Fit

	gof.print <- function(sgp_object) {
		if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
				dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE, showWarnings=FALSE)
					for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
						pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=4.5)
						grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
						dev.off()
					}
				}
		} else {
			message("\tNOTE: No Goodness of Fit tables available to print. No tables will be produced.")
		}
	}

	## Function to calculate the maximum order for a progression based upon any scale changes for the assessment system

	get.max.order.for.progression <- function(year, content_area) {
		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]])) {
			return(SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]]) ## Returns NULL if it doesn't exist
		} else {
			tmp <- as.numeric(tail(unlist(strsplit(as.character(year), "_")), 1)) - as.numeric(tail(unlist(strsplit(as.character(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]), "_")), 1))
			if (tmp < 0) return(SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]])
			if (tmp > 0) return(min(c(as.numeric(tmp), SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]])))
			if (tmp==0) message(paste("\tNOTE: Based upon state scale changes in ", year, ". student growth projections are not possible. No student growth projections will be generated", sep=""))
		}
	}

	## Function to merge coefficient matrices from coefficient matrix productions

	merge.coefficient.matrices <- function(list.of.matrices) {
		tmp.list <- list()
		tmp.coefficient.matrices <- unlist(list.of.matrices, recursive=FALSE)
		for (tmp.names in unique(names(tmp.coefficient.matrices))) {
			tmp1 <- unlist(tmp.coefficient.matrices[grep(tmp.names, names(tmp.coefficient.matrices))], recursive=FALSE)
			names(tmp1) <- sapply(strsplit(names(tmp1), "[.]"), function(x) x[3])
			tmp.list[[tmp.names]] <- tmp1
		}
	tmp.list
	}

	## Function to create sgp.config based upon supplied content_areas, years, and grades

	get.sgp.config <- function(content_areas, years, grades) {

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
	tmp.sgp.config
	} ## END get.sgp.config


	## Function to create par.sgp.config based upon supplied sgp.config

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
						}	else base.gp <- tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order) 
						par.sgp.config[[cnt]][["base.gp"]] <- base.gp
						par.sgp.config[[cnt]][["max.order"]] <- max.order
					}
				}
				
				cnt <- cnt + 1
			}
		}
        par.sgp.config
	} ## END get.par.sgp.config


	## Function to create sgp.baseline.config based upon supplied sgp_object

	get.sgp.baseline.config <- function(sgp_object) {
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
			.grades <- sort(unique(sgp_object@Data[SJ("VALID_CASE", .content_areas)][["GRADE"]]))
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
				tmp.sgp.baseline.config[[as.character(i)]] <- list(baseline.content.areas=i, baseline.panel.years=.years,
					baseline.grade.sequences=sgp.baseline.grade.sequences)
			}

			for (a in seq_along(tmp.sgp.baseline.config)) {
				tmp.length <- length(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]])
				for (b in seq(tmp.length)) {
					sgp.baseline.config[[b+(a-1)*tmp.length]] <- tmp.sgp.baseline.config[[a]]
					sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.grade.sequences"]] <- unlist(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]][b])
			}
		}
	sgp.baseline.config
	} ## END get.sgp.baseline.config


	### Function to create panel data fed to studentGrowthPercentiles and StudentGrowthProjections functions

	get.panel.data <- function(sgp.type, sgp.iter) {

		if (sgp.type=="sgp.percentiles") {
			return(as.data.frame(reshape(
				tmp_sgp_data_for_analysis[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
#				sgp_object@Data[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
					tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sgp.iter[["sgp.grade.sequences"]][[1]]), nomatch=0][,
					'tmp.timevar' := paste(YEAR, CONTENT_AREA, sep="."), with=FALSE],
			idvar="ID",
			timevar="tmp.timevar",
			drop=names(tmp_sgp_data_for_analysis)[!names(tmp_sgp_data_for_analysis) %in% c("ID", "GRADE", "SCALE_SCORE", "tmp.timevar")],
#			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "tmp.timevar")],
			direction="wide")))
		}

		if (sgp.type=="sgp.projections") {
			return(as.data.frame(reshape(
				tmp_sgp_data_for_analysis[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
#				sgp_object@Data[SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
					tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.projection.grade.sequences"]][[1]])),
					sgp.iter[["sgp.projection.grade.sequences"]][[1]]), nomatch=0],
			idvar="ID",
			timevar="YEAR",
			drop=names(tmp_sgp_data_for_analysis)[!names(tmp_sgp_data_for_analysis) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
#			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
			direction="wide")))
		}

		if (sgp.type=="sgp.projections.lagged") {
			return(as.data.frame(reshape(
				data.table(
					data.table(tmp_sgp_data_for_analysis, key="ID")[
						tmp_sgp_data_for_analysis[SJ("VALID_CASE", 
#					data.table(sgp_object@Data, key="ID")[
#						sgp_object@Data[SJ("VALID_CASE", 
						tail(sgp.iter[["sgp.content.areas"]], 1), 
						tail(sgp.iter[["sgp.panel.years"]], 1), 
						tail(sgp.iter[["sgp.grade.sequences"]][[1]], 1))][,"ID", with=FALSE]], 
				key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[
				SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])-1),
					tail(head(sgp.iter[["sgp.panel.years"]], -1), length(sgp.iter[["sgp.grade.sequences"]][[1]])-1),
					head(sgp.iter[["sgp.grade.sequences"]][[1]], -1)), nomatch=0],
			idvar="ID",
			timevar="YEAR",
			drop=names(tmp_sgp_data_for_analysis)[!names(tmp_sgp_data_for_analysis) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR", "ACHIEVEMENT_LEVEL")],
#			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR", "ACHIEVEMENT_LEVEL")],
			direction="wide")))
		}
	} ## END get.panel.data


	### Function to create knots and boundaries list.  Selects knots and boundaries according to sgp.iter content areas, but names them the same
	### when fed into studentGrowthPercentiles.

	get.knots.boundaries <- function(sgp.iter) {

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

	} ### END get.knots.boundaries function


	### Function to create vnames assocaited with panel data fed to studentGrowthPercentiles and studentGrowthProjections functions

	get.panel.data.vnames <- function(sgp.type, sgp.iter) {

		if (sgp.type=="sgp.percentiles") {
			return(c("ID", paste("GRADE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep="."), 
				paste("SCALE_SCORE", tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), 
				tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), sep=".")))
		}

		if (sgp.type=="sgp.projections") {
			tmp.years <- sapply(rev(head(sgp.iter[["sgp.projection.grade.sequences"]][[1]], 1)-sgp.iter[["sgp.projection.grade.sequences"]][[1]]),
				year.increment, year=tail(sgp.iter[["sgp.panel.years"]], 1), lag=0)
			return(c("ID", paste("GRADE", tmp.years, sep="."), paste("SCALE_SCORE", tmp.years, sep=".")))
		}

		if (sgp.type=="sgp.projections.lagged") {
			return(c("ID", paste("GRADE", head(tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), sep="."), 
				paste("SCALE_SCORE", head(tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]][[1]])), -1), sep=".")))
		}
	} ## END get.panel.data.vnames

	
	# Function to get error messages and specific sgp.iter when errors thrown in mclapply (MULTICORE parallel)
	
	get.error.reports <- function() {
		tmp.errors <- tmp[tmp.tf]
		tmp.config <- par.sgp.config[tmp.tf]
		tmp.error.report <- list()
		for (i in 1:length(tmp.errors)) tmp.error.report[[i]] <- list(Error=tmp.errors[[i]], Analysis=tmp.config[[i]])
		return(tmp.error.report)
	}


	### Create sgp.config (if NULL) 

	if (is.null(sgp.config)) {
		sgp.config <- get.sgp.config(content_areas, years, grades)
	}


	#######################################################################################################################
	##   Set up the temporary sgp list object.  Fill with necessary old results if they exist first.
	##   Create subset of @Data containing essential data elements for analyses
	#######################################################################################################################

	tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

	tmp_sgp_data_for_analysis <- sgp_object@Data[,c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"), with=FALSE]
	setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)


	#######################################################################################################################
	##   Baseline SGP - compute matrices first if they are not in SGPstateData or merge them into sgp_object if in SGPstateData
	#######################################################################################################################

	if (sgp.percentiles.baseline) {
		if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
			if (is.null(sgp.baseline.config)) {
				sgp.baseline.config <- get.sgp.baseline.config(sgp_object)
			} else {
				for (i in names(sgp.baseline.config)) {
					if (!identical(names(sgp.baseline.config[[i]]), c("baseline.content.areas", "baseline.panel.years", "baseline.grade.sequences"))) {
						stop("Please specify an appropriate list of SGP function labels (sgp.baseline.config).	See help page for details.")
					}
				}
			}

			message("\n\tStarted Baseline Coefficient Matrix Calculation:\n")
			
			if (!is.null(parallel.config)) {
				par.start <- startParallel(parallel.config, 'BASELINE_MATRICES')

				###  FOREACH flavor
				if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
					tmp <- foreach(sgp.iter=iter(sgp.baseline.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(baselineSGP(
							sgp_object,
							state=state,
							sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
							return.matrices.only=TRUE,
							calculate.baseline.sgps=FALSE))
					}
					tmp_sgp_object <- .mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
					rm(tmp)
				} else {
					if (par.start$par.type=="SNOW") {
						tmp <- clusterApplyLB(par.start$internal.cl, sgp.baseline.config, function(sgp.iter) baselineSGP(
								sgp_object,
								state=state,
								sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
								return.matrices.only=TRUE,
								calculate.baseline.sgps=FALSE))
						
						tmp_sgp_object <- .mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
								rm(tmp)
					} # END if (SNOW)
					
					if (par.start$par.type=="MULTICORE") {
						tmp <- mclapply(sgp.baseline.config, function(sgp.iter) baselineSGP(
									sgp_object,
									state=state,
									sgp.baseline.config=list(sgp.iter), ## NOTE: list of sgp.iter must be passed for proper iteration
									return.matrices.only=TRUE,
									calculate.baseline.sgps=FALSE),
								mc.cores=par.start$workers, mc.preschedule=FALSE)
						tmp_sgp_object <- .mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
						rm(tmp)
					} # END if (MULTICORE)
					stopParallel(parallel.config, par.start)
				} #  END  if parallel
			} else { # process sequentially
				tmp <- list()
				for (sgp.iter in seq_along(sgp.baseline.config)) {
					tmp[[sgp.iter]] <- baselineSGP(
						sgp_object,
						state=state,
						sgp.baseline.config=sgp.baseline.config[sgp.iter], ## NOTE: must pass list, [...], not vector, [[...]].
						return.matrices.only=TRUE,
						calculate.baseline.sgps=FALSE)
				}
			tmp_sgp_object <- .mergeSGP(tmp_sgp_object, list(Coefficient_Matrices=merge.coefficient.matrices(tmp)))
			}

			assign(paste(state, "_Baseline_Matrices", sep=""), list())
			for (tmp.matrix.label in grep("BASELINE", names(tmp_sgp_object$Coefficient_Matrices), value=TRUE)) {
				eval(parse(text=paste(state, "_Baseline_Matrices[['", tmp.matrix.label, "']] <- tmp_sgp_object[['Coefficient_Matrices']][['", tmp.matrix.label, "']]", sep="")))
			}
			save(list=paste(state, "_Baseline_Matrices", sep=""), file=paste(state, "_Baseline_Matrices.Rdata", sep=""))
			message("\n\tFinished Calculating Baseline Coefficient Matrices\n")
		} else {
			tmp_sgp_object <- .mergeSGP(tmp_sgp_object, SGPstateData[[state]][["Baseline_splineMatrix"]])
		}
	suppressMessages(gc()) # clean up
	} # END Get/Compute baseline coefficient matrices


	#######################################################################################################################
	#######################################################################################################################
	##   Percentiles, Baseline Percentiles, Projections, Lagged Projections -  PARALLEL FLAVORS FIRST
	#######################################################################################################################
	#######################################################################################################################

	if (!is.null(parallel.config)) {

		setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
		par.sgp.config <- get.par.sgp.config(sgp.config)
		if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {
			if (any(sapply(par.sgp.config, function(x) identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))) {
				baseline.missings <- which(sapply(par.sgp.config, function(x) identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))
				baseline.missings <- paste(unique(unlist(sapply(par.sgp.config[baseline.missings], function(x) paste(x$sgp.content.areas, x$sgp.grade.sequences)))), collapse=", ")
				message("\tNOTE: Baseline coefficient matrices are not available for ", baseline.missings, ".", sep="")
			}
			par.sgp.config.baseline <- par.sgp.config[which(sapply(par.sgp.config, function(x) !identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))]
		}


	##################################		
	###  PERCENTILES
	##################################

		if (sgp.percentiles) {
			par.start <- startParallel(parallel.config, 'PERCENTILES')
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
								Knots_Boundaries=get.knots.boundaries(sgp.iter)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							...))
					}
				} else {
					tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
						.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
								Knots_Boundaries=get.knots.boundaries(sgp.iter)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							...))
					}
				}
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else { # END FOREACH
				###    SNOW flavor
				if (par.start$par.type == 'SNOW') {
					if (simulate.sgps) {
						if (!exists("calculate.confidence.intervals")) {
							calculate.confidence.intervals <- state
						}
						tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
								Knots_Boundaries=get.knots.boundaries(sgp.iter)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							...))
					} else {
						tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
								Knots_Boundaries=get.knots.boundaries(sgp.iter)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							...))
					}
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					if (simulate.sgps) {
						if (!exists("calculate.confidence.intervals")) {
							calculate.confidence.intervals <- state
						}
						tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
								Knots_Boundaries=get.knots.boundaries(sgp.iter)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							...), mc.cores=par.start$workers, mc.preschedule=FALSE)
					} else {
						tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles( 
							panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
								Knots_Boundaries=get.knots.boundaries(sgp.iter)),
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
							growth.levels=state,
							panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
							grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
							content.area.progression=sgp.iter[["sgp.content.areas"]],
							max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
							exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
							sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
							...), mc.cores=par.start$workers, mc.preschedule=FALSE)
					}
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.=get.error.reports())
					}
					rm(tmp)
				} # End MULTICORE
			} # #END not FOREACH
			stopParallel(parallel.config, par.start)
			suppressMessages(gc()) # clean up
		} #END if (sgp.percentiles)


	####################################
	###  BASELINE PERCENTILES
	####################################

		if (sgp.percentiles.baseline) {

			par.start <- startParallel(parallel.config, 'BASELINE_PERCENTILES')

			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config.baseline), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthPercentiles(
						panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["base.gp"]],
						content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
						num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						# goodness.of.fit=TRUE, # already the default.
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...))
				}
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else { # END FOREACH	
				###    SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthPercentiles(
						panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["base.gp"]],
						content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
						num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						# goodness.of.fit=TRUE, # already the default.
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...))
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config.baseline, function(sgp.iter)	studentGrowthPercentiles(
						panel.data=list(Panel_Data=get.panel.data("sgp.percentiles", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels=state,
						panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["base.gp"]],
						content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
						num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
						# goodness.of.fit=TRUE, # already the default.
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.percentiles.baseline.=get.error.reports())
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.percentiles.baseline


	#######################################################
	###  PROJECTIONS (COHORT referenced)
	#######################################################

		if (sgp.projections) {
		
			par.start <- startParallel(parallel.config, 'PROJECTIONS')
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.=get.error.reports())
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections


	#######################################################
	###  PROJECTIONS (BASELINE referenced)
	#######################################################

		if (sgp.projections.baseline) {
			par.start <- startParallel(parallel.config, 'PROJECTIONS')

			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config.baseline), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config.baseline, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						max.order.for.progression=sgp.projections.baseline.max.order,
						percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
						grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.baseline.=get.error.reports())
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.baseline


	#################################################
	###  LAGGED PROJECTIONS (COHORT Referenced)
	#################################################

		if (sgp.projections.lagged) {
			par.start <- startParallel(parallel.config, 'LAGGED_PROJECTIONS')
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections.lagged", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH
				###   SNOW flavor
				if (par.start$par.type == 'SNOW') {
					tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections.lagged", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					rm(tmp)
					} # END SNOW
				
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections.lagged", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...), mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.=get.error.reports())
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged


	#################################################
	###  LAGGED PROJECTIONS (BASELINE Referenced)
	#################################################

		if (sgp.projections.lagged.baseline) {
			par.start <- startParallel(parallel.config, 'LAGGED_PROJECTIONS')

			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
				tmp <- foreach(sgp.iter=iter(par.sgp.config.baseline), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
					.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=get.panel.data("sgp.projections.lagged", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=get.knots.boundaries(sgp.iter)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.order.for.progression=sgp.projections.lagged.baseline.max.order,
						panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
						achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
						...))
				}
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, tmp)
				rm(tmp)
			} else {# END FOREACH

			###  SNOW flavor
			if (par.start$par.type == 'SNOW') {
				tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config.baseline, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=get.panel.data("sgp.projections.lagged", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
						Knots_Boundaries=get.knots.boundaries(sgp.iter)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...))

				tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
				rm(tmp)
			} # END SNOW
			
			###  MULTICORE flavor
			if (par.start$par.type == 'MULTICORE') {
				tmp <- mclapply(par.sgp.config.baseline, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=get.panel.data("sgp.projections.lagged", sgp.iter), Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
						Knots_Boundaries=get.knots.boundaries(sgp.iter)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...), mc.cores=par.start$workers, mc.preschedule=FALSE)

				tmp_sgp_object <- .mergeSGP(Reduce(.mergeSGP, tmp), tmp_sgp_object)
				if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
					tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.baseline.=get.error.reports())
				}
				rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged.baseline
	}  ## END if (!is.null(parallel.config))


	################################################################
	################################################################
	###	SEQUENTIAL OPTION (NON-Parallel Option)
	################################################################
	################################################################

	if (is.null(parallel.config)) {

		setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
		par.sgp.config <- get.par.sgp.config(sgp.config)		
		if (sgp.percentiles.baseline | sgp.projections.baseline | sgp.projections.lagged.baseline) {
			par.sgp.config.baseline <- par.sgp.config[which(sapply(par.sgp.config, function(x) !identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))]
		}

		## sgp.percentiles
			
		if (sgp.percentiles) {
			for (sgp.iter in par.sgp.config) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", get.panel.data("sgp.percentiles", sgp.iter)))
				tmp.knots.boundaries <- get.knots.boundaries(sgp.iter) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
						content.area.progression=sgp.iter[["sgp.content.areas"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						calculate.confidence.intervals=calculate.confidence.intervals,
						drop.nonsequential.grade.progression.variables=FALSE,
						grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...)
				} else {
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=panel.data,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=sgp.use.my.coefficient.matrices,
						growth.levels=state,
						panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
						grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
						content.area.progression=sgp.iter[["sgp.content.areas"]],
						max.order.for.percentile=SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.percentile"]],
						drop.nonsequential.grade.progression.variables=FALSE,
						grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
						exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
						sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
						...)
				}
			}
		} ## END if sgp.percentiles


		## sgp.percentiles.baseline

		if (sgp.percentiles.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {
				
				panel.data=within(tmp_sgp_object, assign("Panel_Data", get.panel.data("sgp.percentiles", sgp.iter)))
				tmp.knots.boundaries <- get.knots.boundaries(sgp.iter) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthPercentiles(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), 
						my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					growth.levels=state,
					panel.data.vnames=get.panel.data.vnames("sgp.percentiles", sgp.iter),
					grade.progression=sgp.iter[["base.gp"]],
					content.area.progression=tail(sgp.iter[["sgp.content.areas"]], min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order)+1),
					num.prior=min(sgp.iter[["max.order"]], sgp.percentiles.baseline.max.order),
					# goodness.of.fit=TRUE, # already the default.
					drop.nonsequential.grade.progression.variables=FALSE,
					grade.progression.label=sgp.iter[["sgp.grade.progression.labels"]],
					exact.grade.progression.sequence=sgp.iter[["sgp.exact.grade.progression"]],
					sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
					...)
			}
		} ## END if sgp.percentiles.baseline

	
		## sgp.projections
	
		if (sgp.projections) {
			for (sgp.iter in par.sgp.config) {
	
				panel.data=within(tmp_sgp_object, assign("Panel_Data", get.panel.data("sgp.projections", sgp.iter)))
				tmp.knots.boundaries <- get.knots.boundaries(sgp.iter) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
					grade.progression=sgp.iter[["sgp.projection.grade.sequences"]][[1]],
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
			}
		} ## END if sgp.projections


		## sgp.projections.baseline
	
		if (sgp.projections.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", get.panel.data("sgp.projections", sgp.iter)))
				tmp.knots.boundaries <- get.knots.boundaries(sgp.iter) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1),
						my.extra.label="BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					max.order.for.progression=sgp.projections.baseline.max.order,
					percentile.trajectory.values=c(1, SGPstateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=get.panel.data.vnames("sgp.projections", sgp.iter),
					grade.progression=sgp.iter[["sgp.grade.sequences"]][[1]],
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
			}
		} ## END if sgp.projections.baseline
	
	
		## sgp.projections.lagged
	
		if (sgp.projections.lagged) {
			for (sgp.iter in par.sgp.config) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", get.panel.data("sgp.projections.lagged", sgp.iter)))
				tmp.knots.boundaries <- get.knots.boundaries(sgp.iter) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					max.order.for.progression=get.max.order.for.progression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1)),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
			}
		} ## END sgp.projections.lagged


		## sgp.projections.lagged.baseline
	
		if (sgp.projections.lagged.baseline) {
			for (sgp.iter in par.sgp.config.baseline) {

				panel.data=within(tmp_sgp_object, assign("Panel_Data", get.panel.data("sgp.projections.lagged", sgp.iter)))
				tmp.knots.boundaries <- get.knots.boundaries(sgp.iter) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.order.for.progression=sgp.projections.lagged.baseline.max.order,
					panel.data.vnames=get.panel.data.vnames("sgp.projections.lagged", sgp.iter),
					achievement.level.prior.vname=paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1), 1), sep="."),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
					...)
			}
		} ## END sgp.projections.lagged.baseline
	for (n in names(tmp_sgp_object)) if (is.null(tmp_sgp_object[[n]])) tmp_sgp_object[[n]] <- NULL
	tmp_sgp_object[['Panel_Data']] <- NULL
	} ## END sequential analyzeSGP

	sgp_object@SGP <- .mergeSGP(tmp_sgp_object, sgp_object@SGP)

	if (goodness.of.fit.print) gof.print(sgp_object)
	setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, ID) # re-key data for combineSGP, etc.
	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
