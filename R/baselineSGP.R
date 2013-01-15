`baselineSGP` <-
function(sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	grades=NULL,
	sgp.config=NULL,
	sgp.baseline.config=NULL,
	sgp.baseline.panel.years=NULL,
	sgp.percentiles.baseline.max.order=3,
	return.matrices.only=FALSE,
	calculate.baseline.sgps=TRUE,
	goodness.of.fit.print=TRUE,
	...) {


	started.at <- proc.time()
	message(paste("\tStarted baselineSGP", date(), "\n"))

	VALID_CASE <- YEAR <- GRADE <- CONTENT_AREA <- NULL ### To prevent R CMD check warnings

	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- getStateAbbreviation(tmp.name, "baselineSGP")
	}


	### Syncronize "return.matrices.only" and "calculate.baseline.sgps" arguments

	if (return.matrices.only & calculate.baseline.sgps) {
		message("\tArgument 'return.matrices.only=TRUE' obviates need to calculate baseline student growth percentiles. Argument 'calculate.baseline.sgps' will be set to FALSE")
		calculate.baseline.sgps <- FALSE
	}

	if (!is.null(SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]])) {
		sgp.loss.hoss.adjustment <- SGPstateData[[state]][["SGP_Configuration"]][["sgp.loss.hoss.adjustment"]]
	} else {
		sgp.loss.hoss.adjustment <- NULL
	}


	############################################
	###
	### Utility Functions
	###
	############################################


	baselineSGP_Internal <- function(sgp_object, state, years, content_areas, grade.sequences, baseline.grade.sequences.lags, knots.boundaries.iter) {

		started.at <- proc.time()
		started.date <- date()

		### Utility functions

		test.year.sequence <- function(content_areas, years, grades, baseline.grade.sequences.lags=NULL) {

			year.increment <- function(year, increment) {
				tmp.list <- list()
				for (i in seq_along(increment)) {
					tmp.list[[i]] <- paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment[i], collapse="_")
				}
				unlist(tmp.list)
			}

			grades <- type.convert(as.character(grades), as.is=TRUE)
			if (is.null(baseline.grade.sequences.lags)) baseline.grade.sequences.lags <- rep(1, length(grades)-1)

			tmp.years.sequence <- list()
			tmp.years.sequence <- lapply(years, function(x) year.increment(year=x, increment=c(0,cumsum(baseline.grade.sequences.lags))))
			return(tmp.years.sequence[sapply(tmp.years.sequence, function(x) all(x %in% years))])

		} ### END test.year.sequence


		### Reshape data 

		setkeyv(sgp_object@Data, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA"))
		tmp.year.sequence <- test.year.sequence(content_areas, years, grade.sequences, baseline.grade.sequences.lags)
		tmp.list <- list()
		for (k in seq_along(tmp.year.sequence)) {
			tmp.lookup <- data.table(CJ("VALID_CASE", tmp.year.sequence[[k]]), grade.sequences[!is.na(grade.sequences)], content_areas, seq_along(tmp.year.sequence[[k]]))
			setnames(tmp.lookup, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA", "tmp.timevar"))
			setkeyv(tmp.lookup, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA", "tmp.timevar"))
			tmp.list[[k]] <- reshape(sgp_object@Data[tmp.lookup, nomatch=0],
				idvar="ID",
				timevar="tmp.timevar",
				direction="wide",
				drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'SCALE_SCORE', 'tmp.timevar')])
			tmp.list[[k]] <- as.data.frame(tmp.list[[k]][!apply(is.na(tmp.list[[k]]), 1, any)])
		}
		tmp.df <- rbind.fill(tmp.list)


		### Calculate Coefficient Matrices and return list containing coefficient matrices

		tmp_sgp_list <- list(Coefficient_Matrices =
			studentGrowthPercentiles(
				panel.data=list(Panel_Data=data.frame(tmp.df[,1], matrix(grade.sequences[!is.na(grade.sequences)], nrow=1), tmp.df[,-1]), 
					Knots_Boundaries=getKnotsBoundaries(knots.boundaries.iter, state, "Baseline")),
				sgp.labels=list(my.year="BASELINE", my.subject=tail(content_areas, 1)),
				use.my.knots.boundaries=list(my.year="BASELINE", my.subject=tail(content_areas, 1)),
				calculate.sgps=FALSE,
				goodness.of.fit=FALSE,
				drop.nonsequential.grade.progression.variables=FALSE, # taken care of in data reshape above.
				grade.progression=grade.sequences,
				content.area.progression=content_areas,
				year.progression=rep("BASELINE", length(content_areas)),
				year.progression.lags=baseline.grade.sequences.lags,
				exact.grade.progression.sequence=TRUE,
				print.time.taken=FALSE,
				...)[["Coefficient_Matrices"]])

		message(paste("\tStarted baselineSGP Coefficient Matrix Calculation:", started.date))
		message(paste("\tContent Area: ", tail(content_areas, 1), ", Grade Progression: ", paste(grade.sequences, collapse=", "), ". ", sep=""))
		message(paste("\tFinished baselineSGP Coefficient Matrix Calculation ", date(), " in ", timetaken(started.at), ".\n", sep=""))

		return(tmp_sgp_list)

	} ### END baselineSGP_Internal Function


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


	#################################################################################
	###
	### Calculate/Retrieve baseline coefficient matrices if requested
	###
	#################################################################################

	if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
		
		if (is.null(sgp.baseline.config)) {
			sgp.baseline.config <- getSGPBaselineConfig(sgp_object, content_areas, grades, sgp.baseline.panel.years)
		} else {
			sgp.baseline.config <- checkConfig(sgp.baseline.config, "Baseline")
		}

		tmp.list <- list()

		for (sgp.iter in seq_along(sgp.baseline.config)) {
			tmp.list[[sgp.iter]] <- baselineSGP_Internal(
							sgp_object,
							state=state,
							years=sgp.baseline.config[[sgp.iter]][["baseline.panel.years"]],
							content_areas=sgp.baseline.config[[sgp.iter]][["baseline.content.areas"]],
							grade.sequences=sgp.baseline.config[[sgp.iter]][["baseline.grade.sequences"]],
							baseline.grade.sequences.lags=sgp.baseline.config[[sgp.iter]][["baseline.grade.sequences.lags"]],
							knots.boundaries.iter=sgp.baseline.config[[sgp.iter]])
		}

		sgp_object@SGP <- mergeSGP(Reduce(mergeSGP, tmp.list), sgp_object@SGP)
	} else {
		sgp_object@SGP <- mergeSGP(sgp_object@SGP, SGPstateData[[state]][["Baseline_splineMatrix"]])
        }


	################################################################
	###
	### Calculate baseline referenced student growth percentiles
	###
	################################################################

	if (calculate.baseline.sgps) {

		tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

		tmp_sgp_data_for_analysis <- sgp_object@Data[,c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"), with=FALSE]
			setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)

		sgp.config <- getSGPConfig(sgp_object, tmp_sgp_object, content_areas, years, grades, sgp.config,
			sgp.percentiles.baseline=TRUE, sgp.projections.baseline=FALSE, sgp.projections.lagged.baseline=FALSE,
			sgp.config.drop.nonsequential.grade.progression.variables=TRUE)
		sgp.config.baseline <- sgp.config[which(sapply(sgp.config, function(x) !identical(x[['base.gp']], "NO_BASELINE_COEFFICIENT_MATRICES")))]

		for (sgp.iter in sgp.config.baseline) {

			panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, "sgp.percentiles", sgp.iter)))
			tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
			panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]

			tmp_sgp_object <- studentGrowthPercentiles(
				panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1),
						my.subject=tail(sgp.iter[['sgp.content.areas']], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1), my.subject=tail(sgp.iter[['sgp.content.areas']], 1)),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[['sgp.content.areas']], 1)),
					growth.levels=state,
					panel.data.vnames=getPanelDataVnames("sgp.percentiles", sgp.iter),
					grade.progression=sgp.iter[['base.gp']],
					content.area.progression=tail(sgp.iter[['sgp.content.areas']], min(sgp.iter[['max.order']], sgp.percentiles.baseline.max.order)+1),
					num.prior=min(sgp.iter[['max.order']], sgp.percentiles.baseline.max.order),
					percentile.cuts=SGPstateData[[state]][['SGP_Configuration']][['percentile.cuts']],
					drop.nonsequential.grade.progression.variables=FALSE,
					exact.grade.progression.sequence=sgp.iter[['sgp.exact.grade.progression']],
					sgp.loss.hoss.adjustment=sgp.loss.hoss.adjustment,
					...)

		} ### END sgp.iter loop

		sgp_object@SGP <- mergeSGP(sgp_object@SGP, tmp_sgp_object)

		if (goodness.of.fit.print) gof.print(sgp_object)

	} ### END if (calculate.baseline.sgps)

    
	############################################################
	###
	### Return results
	###
	############################################################

	message(paste("\tFinished baselineSGP", date(), "in", timetaken(started.at), "\n"))

	if (return.matrices.only) {
		tmp.list <- list()
		for (ca in unique(sapply(sgp.baseline.config, function(x) tail(x[["baseline.content.areas"]],1)))) {
			tmp.list[[paste(ca, ".BASELINE", sep="")]] <- sgp_object@SGP[["Coefficient_Matrices"]][[paste(ca, ".BASELINE", sep="")]]
		}
		return(tmp.list)
	} else {
		return(sgp_object)
	}
} ### END baselineSGP Function
