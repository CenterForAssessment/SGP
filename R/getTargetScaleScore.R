`getTargetScaleScore` <-
function(sgp_object,
	state,
	sgp.targets,
	target.type,
	target.level,
	years.to.target.level,
	years.content_areas.grades,
	sgp.config=NULL,
	projection_group.identifier=NULL,
	sgp.projections.equated=NULL,
	SGPt=NULL,
	fix.duplicates=fix.duplicates,
	parallel.config=NULL) {

	VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- GRADE <- YEAR_WITHIN <- NULL

	### Define variables

    SGPstateData <- list(SGP::SGPstateData[[state]]) ### Needed due to possible assignment of values to SGPstateData
    names(SGPstateData) <- state

	if (!is.null(sgp.projections.equated)) {
		year.for.equate <- sgp.projections.equated$Year
		equate.variable <- "SCALE_SCORE_EQUATED"
		equate.label <- coefficient.matrix.type <- "EQUATED"
	} else {
		year.for.equate <- equate.variable <- equate.label <- coefficient.matrix.type <- NULL
	}

	if (!is.null(SGPt)) {
		if (identical(SGPt, TRUE)) SGPt <- "DATE"
		if (!all(SGPt %in% names(sgp_object@Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(SGPt, collapse=", "), "are not all contained in the supplied 'sgp_object@Data'. 'SGPt' is set to NULL.\n")
			SGPt <- NULL
		}
	}


	setkey(sgp_object@Data, VALID_CASE, ID)
	variables.to.get <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", "LAST_OBSERVATION", "STATE", equate.variable, SGPt)
	if(!is.null(fix.duplicates)) variables.to.get <- c(variables.to.get, "DUPS_FLAG", "SGP_NORM_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES_CURRENT")

	tmp_sgp_data_for_analysis <- sgp_object@Data[SJ("VALID_CASE", unique(sgp.targets[['ID']])), intersect(names(sgp_object@Data), variables.to.get), with=FALSE]
	setkeyv(tmp_sgp_data_for_analysis, intersect(names(sgp_object@Data), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "YEAR_WITHIN")))
	setkeyv(sgp_object@Data, getKey(sgp_object))
    sgp_data_names <- names(tmp_sgp_data_for_analysis)

	years.content_areas.grades <- data.table(unique(data.table(sgp_object@Data[data.table(VALID_CASE="VALID_CASE", sgp.targets, key=getKey(sgp_object))][,
		c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"), with=FALSE], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")), by=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")), key=c("VALID_CASE", "CONTENT_AREA", "YEAR"))[
		unique(data.table(VALID_CASE="VALID_CASE", years.content_areas.grades[,c("CONTENT_AREA", "YEAR"), with=FALSE], key=c("VALID_CASE", "CONTENT_AREA", "YEAR")), by=c("VALID_CASE", "CONTENT_AREA", "YEAR")), nomatch=0]

	if (target.type=="sgp.projections") {
		my.extra.label <- "TARGET_SCALE_SCORES"
		baseline.tf <- FALSE
		lag.increment <- 0L
		lag.increment.label <- "_CURRENT"
		my.content.areas <- "sgp.projection.content.areas"
		my.content.areas.label <- "sgp.projection.content.areas"
		my.grade.sequences <- "sgp.projection.grade.sequences"
		my.grade.sequences.label <- "sgp.projection.grade.sequences"
		my.panel.years <- "sgp.projection.panel.years"
		my.panel.years.lags <- "sgp.projection.panel.years.lags"
	}
	if (target.type=="sgp.projections.baseline") {
		my.extra.label <- "BASELINE.TARGET_SCALE_SCORES"
		baseline.tf <- TRUE
		lag.increment <- 0L
		lag.increment.label <- "_CURRENT"
		coefficient.matrix.type <- "BASELINE"
		my.content.areas <- "sgp.projection.baseline.content.areas"
		my.content.areas.label <- "sgp.projection.baseline.content.areas"
		my.grade.sequences <- "sgp.projection.baseline.grade.sequences"
		my.grade.sequences.label <- "sgp.projection.baseline.grade.sequences"
		my.panel.years.lags <- "sgp.projection.baseline.panel.years.lags"
	}
	if (target.type=="sgp.projections.lagged") {
		my.extra.label <- "LAGGED.TARGET_SCALE_SCORES"
		baseline.tf <- FALSE
		lag.increment <- 1L
		lag.increment.label <- ""
		my.content.areas <- "sgp.projection.content.areas"
		my.content.areas.label <- "sgp.content.areas"
		my.grade.sequences <- "sgp.projection.grade.sequences"
		my.grade.sequences.label <- "sgp.grade.sequences"
		my.panel.years <- "sgp.panel.years"
		my.panel.years.lags <- "sgp.projection.panel.years.lags"
	}
	if (target.type=="sgp.projections.lagged.baseline") {
		my.extra.label <- "LAGGED.BASELINE.TARGET_SCALE_SCORES"
		baseline.tf <- TRUE
		lag.increment <- 1L
		lag.increment.label <- ""
		coefficient.matrix.type <- "BASELINE"
		my.content.areas <- "sgp.projection.baseline.content.areas"
		my.content.areas.label <- "sgp.content.areas"
		my.grade.sequences <- "sgp.projection.baseline.grade.sequences"
		my.grade.sequences.label <- "sgp.grade.sequences"
		my.panel.years.lags <- "sgp.projection.baseline.panel.years.lags"
	}

    sgp.projections.max.forward.progression.years <-
        sapply(unlist(strsplit(target.level[1L], "_")),
            function(x) type.convert(x, as.is=FALSE)
        )[!sapply(
            lapply(unlist(strsplit(target.level[1L], "_")),
                   function(x) type.convert(x, as.is=FALSE)),
            is.factor
        )] |> as.numeric()

    .filter_matrices_for_projections <- function(matrices, coefficient.matrix.type) {
        if (identical(coefficient.matrix.type, "BASELINE")) {
            return(matrices[grep("BASELINE", names(matrices), value = TRUE)])
        }
        if (identical(coefficient.matrix.type, "EQUATED")) {
            return(matrices[grep("EQUATED", names(matrices), value = TRUE)])
        }
        return(matrices[
            grep("BASELINE|EQUATED|SIMEX", x = names(matrices), value = TRUE, invert = TRUE) #|>
                # grep(paste0(content_area, collapse = "|"), x = _, value = TRUE)
        ])
    }

    tmp_sgp_object <- list(
        Coefficient_Matrices =
            .filter_matrices_for_projections(
                sgp_object@SGP[["Coefficient_Matrices"]], 
                coefficient.matrix.type),
        Knots_Boundaries = sgp_object@SGP[["Knots_Boundaries"]]
    )

	par.sgp.config <- getSGPConfig(
				sgp_object,
				state,
				tmp_sgp_object,
				sort(unique(years.content_areas.grades[['CONTENT_AREA']])),
				sort(unique(years.content_areas.grades[['YEAR']])),
				sort(unique(years.content_areas.grades[['GRADE']])),
				sgp.config=sgp.config,
				trim.sgp.config=TRUE,
				sgp.percentiles=FALSE, ### NOT calculating sgp.percentiles. Just projections
				sgp.projections=!grepl(".lagged", target.type) & !grepl("baseline", target.type),
				sgp.projections.lagged=grepl(".lagged", target.type) & !grepl("baseline", target.type),
				sgp.percentiles.baseline=FALSE, ### NOT calculating sgp.percentiles.baseline. Just projections
				sgp.projections.baseline=grepl("baseline", target.type) & !grepl(".lagged", target.type),
				sgp.projections.lagged.baseline=grepl("baseline", target.type) & grepl("lagged", target.type),
				sgp.config.drop.nonsequential.grade.progression.variables=FALSE,
				sgp.projections.max.forward.progression.years=sgp.projections.max.forward.progression.years,
				sgp.use.my.coefficient.matrices=NULL,
				calculate.simex=NULL,
				calculate.simex.baseline=NULL,
				year.for.equate=year.for.equate,
				sgp.percentiles.equated=FALSE,
				SGPt=SGPt,
				projection_group.identifier=projection_group.identifier,
				from.getTargetScaleScore=TRUE) ### NOT calculating sgp.percentiles.equated. Just projections


	### Calculate targets
    call_function <- define_compute(parallel.config, "SGP_SCALE_SCORE_TARGETS")

    results <- lapply(
        par.sgp.config[[target.type]],
        \(sgp.iter) {
            list(
                panel.data = list(
                    Panel_Data = getPanelData(
                        tmp_sgp_data_for_analysis, target.type, sgp.iter,
                        sgp.targets = sgp.targets, sgp.scale.score.equated = equate.variable,
                        SGPt = SGPt, fix.duplicates = fix.duplicates),
                    Knots_Boundaries = getKnotsBoundaries(sgp.iter, state, target.type),
                    Coefficient_Matrices = tmp_sgp_object[["Coefficient_Matrices"]]),#[
                        # paste0(unique(sgp.iter[[my.content.areas.label]]), ".", ## this reduction causes issues with EOCT analyses
                            #    ifelse(baseline.tf, "BASELINE", tail(sgp.iter[[my.panel.years]], 1))
                        # ) |> paste(collapse = "|") |> grepl(names(tmp_sgp_object[["Coefficient_Matrices"]]))]),
                sgp.labels = list(
                    my.year = tail(sgp.iter[["sgp.panel.years"]], 1L),
                    my.subject = tail(sgp.iter[[my.content.areas.label]], 1L),
                    my.grade = tail(sgp.iter[[my.grade.sequences.label]], 1L),
                    my.extra.label = my.extra.label),
                grade.progression = sgp.iter[[my.grade.sequences]],
                content_area.progression = sgp.iter[[my.content.areas]],
                year_lags.progression = sgp.iter[[my.panel.years.lags]],
                grade.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                content_area.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                year_lags.projection.sequence = SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
                max.forward.progression.years = sgp.iter[["sgp.projections.max.forward.progression.years"]] + lag.increment,
                max.order.for.progression = getMaxOrderForProgression(
                    tail(sgp.iter[["sgp.panel.years"]], 1L),
                    tail(sgp.iter[[my.content.areas]], 1L),
                    state, sgp.projections.equated),
                use.my.knots.boundaries = list(
                    my.year = tail(sgp.iter[["sgp.panel.years"]], 1L),
                    my.subject = tail(sgp.iter[[my.content.areas.label]], 1L)),
                use.my.coefficient.matrices = list(
                    my.year = if (baseline.tf) "BASELINE" else tail(sgp.iter[["sgp.panel.years"]], 1L),
                    my.subject = tail(sgp.iter[[my.content.areas]], 1),
                    my.extra.label = equate.label),
                panel.data.vnames =
                    getPanelDataVnames(target.type, sgp.iter, sgp_data_names, equate.variable),
                performance.level.cutscores = state,
                calculate.sgps = !(tail(sgp.iter[["sgp.panel.years"]], 1) %in%
                    SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[[my.content.areas]], 1)]] &
                    is.null(sgp.projections.equated)),
                sgp.projections.equated = sgp.projections.equated,
                percentile.trajectory.values = target.level,
                percentile.trajectory.values.max.forward.progression.years = years.to.target.level,#
                return.percentile.trajectory.values = SGPstateData[[state]][["SGP_Configuration"]][["return.percentile.trajectory.values"]],#
                return.projection.group.identifier = projection_group.identifier,
                return.projection.group.scale.scores = !is.null(fix.duplicates),
                projcuts.digits = SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]],
                sgp.projections.use.only.complete.matrices = SGPstateData[[state]][["SGP_Configuration"]][["sgp.projections.use.only.complete.matrices"]],
                lag.increment = lag.increment,#
                lag.increment.label = lag.increment.label,#
                SGPt = getSGPtNames(sgp.iter, SGPt, target.type),

                ## Add in previously unspecified defaults too:
                max.forward.progression.grade = NULL,
                achievement.level.prior.vname = NULL,
                convert.0and100 = TRUE,
                trajectories.chunk.size = 50000L,
                projection.unit = "YEAR",
                projection.unit.label = NULL,
                return.projection.group.dates = NULL,
                isotonize = TRUE,
                sgp.exact.grade.progression = FALSE
            )}
        ) |>
            call_function(studentGrowthProjections)

    if (any(
        tmp.tf <- sapply(results, \(x)
            any(class(x) %in% c("try-error", "simpleError", "sgp-error")))
    )) {
        if (grepl("[.]lagged", target.type)) {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.target.scale.scores.lagged = 
                    getErrorReports(results, tmp.tf, par.sgp.config[[target.type]]))
        }  else {
            tmp_sgp_object[["Error_Reports"]] <- c(tmp_sgp_object[["Error_Reports"]],
                sgp.target.scale.scores =
                    getErrorReports(results, tmp.tf, par.sgp.config[[target.type]]))
        }
    }
    tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, results[!tmp.tf]), tmp_sgp_object)
    rm(results)

    sgp_object@SGP <- mergeSGP(tmp_sgp_object, sgp_object@SGP)
    return(sgp_object)
} ### END getTargetScaleScore
