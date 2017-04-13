`getTargetScaleScore` <-
function(sgp_object,
	state,
	sgp.targets,
	target.type,
	target.level,
	tmp.years.content_areas.grades,
	sgp.config=NULL,
	projection_group.identifier=NULL,
	sgp.projections.equated=NULL,
	SGPt=NULL,
	fix.duplicates=fix.duplicates,
	parallel.config=NULL) {

	VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- GRADE <- YEAR_WITHIN <- NULL

	### Define variables

	if (!is.null(sgp.projections.equated)) {
		year.for.equate <- sgp.projections.equated$Year
		equate.variable <- "SCALE_SCORE_EQUATED"
		equate.label <- coefficient.matrix.type <- "EQUATED"
	} else {
		year.for.equate <- equate.variable <- equate.label <- NULL
	}

	if (!is.null(SGPt)) {
		if (identical(SGPt, TRUE)) SGPt <- "DATE"
		if (!all(SGPt %in% names(sgp_object@Data))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Variables", paste(SGPt, collapse=", "), "are not all contained in the supplied 'sgp_object@Data'. 'SGPt' is set to NULL.\n")
			SGPt <- NULL
		}
	}


	tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])
	setkey(sgp_object@Data, VALID_CASE, ID)
	variables.to.get <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "FIRST_OBSERVATION", "LAST_OBSERVATION", "STATE", equate.variable, SGPt)
	if(!is.null(fix.duplicates)) variables.to.get <- c(variables.to.get, "DUPS_FLAG", "SGP_NORM_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES_CURRENT")

	tmp_sgp_data_for_analysis <- sgp_object@Data[SJ("VALID_CASE", unique(sgp.targets[['ID']])), intersect(names(sgp_object@Data), variables.to.get), with=FALSE]
	setkeyv(tmp_sgp_data_for_analysis, intersect(names(sgp_object@Data), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "YEAR_WITHIN")))
	setkeyv(sgp_object@Data, getKey(sgp_object))
	years.content_areas.grades <- data.table(unique(data.table(sgp_object@Data[data.table(VALID_CASE="VALID_CASE", sgp.targets, key=getKey(sgp_object))][,
		c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"), with=FALSE], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")), by=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")), key=c("VALID_CASE", "CONTENT_AREA", "YEAR"))[
		unique(data.table(VALID_CASE="VALID_CASE", tmp.years.content_areas.grades[,c("CONTENT_AREA", "YEAR"), with=FALSE], key=c("VALID_CASE", "CONTENT_AREA", "YEAR")), by=c("VALID_CASE", "CONTENT_AREA", "YEAR")), nomatch=0]

	if (target.type=="sgp.projections") {
		my.extra.label <- "TARGET_SCALE_SCORES"
		baseline.tf <- FALSE
		lag.increment <- 0
		my.target.type <- "sgp.projections"
		my.content.areas <- "sgp.projection.content.areas"
		my.content.areas.label <- "sgp.projection.content.areas"
		my.grade.sequences <- "sgp.projection.grade.sequences"
		my.panel.years.lags <- "sgp.projection.panel.years.lags"
	}
	if (target.type=="sgp.projections.baseline") {
		my.extra.label <- "BASELINE.TARGET_SCALE_SCORES"
		baseline.tf <- TRUE
		lag.increment <- 0
		my.target.type <- "sgp.projections.baseline"
		my.content.areas <- "sgp.projection.baseline.content.areas"
		my.content.areas.label <- "sgp.projection.baseline.content.areas"
		my.grade.sequences <- "sgp.projection.baseline.grade.sequences"
		my.panel.years.lags <- "sgp.projection.baseline.panel.years.lags"
	}
	if (target.type=="sgp.projections.lagged") {
		my.extra.label <- "LAGGED.TARGET_SCALE_SCORES"
		baseline.tf <- FALSE
		lag.increment <- 1
		my.target.type <- "sgp.projections.lagged"
		my.content.areas <- "sgp.projection.content.areas"
		my.content.areas.label <- "sgp.content.areas"
		my.grade.sequences <- "sgp.projection.grade.sequences"
		my.panel.years.lags <- "sgp.projection.panel.years.lags"
	}
	if (target.type=="sgp.projections.lagged.baseline") {
		my.extra.label <- "LAGGED.BASELINE.TARGET_SCALE_SCORES"
		baseline.tf <- TRUE
		lag.increment <- 1
		my.target.type <- "sgp.projections.lagged"
		my.content.areas <- "sgp.projection.baseline.content.areas"
		my.content.areas.label <- "sgp.content.areas"
		my.grade.sequences <- "sgp.projection.baseline.grade.sequences"
		my.panel.years.lags <- "sgp.projection.baseline.panel.years.lags"
	}

	sgp.projections.max.forward.progression.years <-
		as.numeric(sapply(unlist(strsplit(target.level[1], "_")), type.convert)[!sapply(lapply(unlist(strsplit(target.level[1], "_")), type.convert), is.factor)])

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
				sgp.projections=!grepl(".lagged", target.type),
				sgp.projections.lagged=grepl(".lagged", target.type),
				sgp.percentiles.baseline=FALSE, ### NOT calculating sgp.percentiles.baseline. Just projections
				sgp.projections.baseline=grepl("baseline", target.type),
				sgp.projections.lagged.baseline=grepl("baseline", target.type),
				sgp.config.drop.nonsequential.grade.progression.variables=FALSE,
				sgp.projections.max.forward.progression.years=sgp.projections.max.forward.progression.years,
				sgp.use.my.coefficient.matrices=NULL,
				calculate.simex=NULL,
				calculate.simex.baseline=NULL,
				year.for.equate=year.for.equate,
				sgp.percentiles.equated=FALSE, ### NOT calculating sgp.percentiles.equated. Just projections
				SGPt=SGPt)


	### Calculate targets

	if (!is.null(parallel.config[["WORKERS"]]) & !is.null(names(parallel.config[["WORKERS"]]))) parallel.config[["WORKERS"]][["SGP_SCALE_SCORE_TARGETS"]] <- parallel.config[["WORKERS"]][[1]]

	if (!is.null(parallel.config)) {
		par.start <- startParallel(parallel.config, 'SGP_SCALE_SCORE_TARGETS')

		###  FOREACH flavor

		if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
			tmp <- foreach(sgp.iter=iter(par.sgp.config[[target.type]]), .packages="SGP", .inorder=FALSE, .errorhandling = "pass",
				.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
				return(studentGrowthProjections(
					panel.data=list(
						Panel_Data=getPanelData(tmp_sgp_data_for_analysis, my.target.type, sgp.iter, sgp.targets=sgp.targets, sgp.scale.score.equated=equate.variable, SGPt=SGPt, fix.duplicates=fix.duplicates),
						Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
						Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, my.target.type)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1),
						my.grade=tail(sgp.iter[["sgp.grade.sequences"]], 1), my.extra.label=my.extra.label),
					use.my.coefficient.matrices=list(my.year=if (baseline.tf) "BASELINE" else tail(sgp.iter[["sgp.panel.years"]], 1),
						my.subject=tail(sgp.iter[[my.content.areas]], 1), my.extra.label=equate.label),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1)),
					performance.level.cutscores=state,
					max.forward.progression.years=sgp.iter[['sgp.projections.max.forward.progression.years']]+lag.increment,
					panel.data.vnames=getPanelDataVnames(my.target.type, sgp.iter, names(tmp_sgp_data_for_analysis), equate.variable),
					sgp.projections.equated=sgp.projections.equated,
					grade.progression=sgp.iter[[my.grade.sequences]],
					content_area.progression=sgp.iter[[my.content.areas]],
					year_lags.progression=sgp.iter[[my.panel.years.lags]],
					max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[[my.content.areas]], 1), state,
						sgp.projections.equated),
					lag.increment=lag.increment,
					grade.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					content_area.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					year_lags.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					percentile.trajectory.values=target.level,
					return.percentile.trajectory.values=SGP::SGPstateData[[state]][["SGP_Configuration"]][["return.percentile.trajectory.values"]],
					return.projection.group.identifier=projection_group.identifier,
					return.projection.group.scale.scores = !is.null(fix.duplicates),
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in%
						SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[[my.content.areas]], 1)]] &
						is.null(sgp.projections.equated)),
					sgp.projections.use.only.complete.matrices=SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']],
					SGPt=getSGPtNames(sgp.iter, SGPt, my.target.type),
					projcuts.digits=SGP::SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]]))
			}

			if (any(tmp.tf <- sapply(tmp, function(x) any(class(x) %in% c("try-error", "simpleError"))))) {
				if (grepl(".lagged", target.type)) {
					tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']],
						sgp.target.scale.scores.lagged=getErrorReports(tmp, tmp.tf, par.sgp.config[['sgp.projections']]))
				}  else {
					tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']],
						sgp.target.scale.scores=getErrorReports(tmp, tmp.tf, par.sgp.config[['sgp.projections']]))
				}
			}
			tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp[!tmp.tf]), tmp_sgp_object)
			rm(tmp)
		} else {# END FOREACH
			###   SNOW flavor
			if (par.start$par.type == 'SNOW') {
				tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config[[target.type]], function(sgp.iter) studentGrowthProjections(
					panel.data=list(
						Panel_Data=getPanelData(tmp_sgp_data_for_analysis, my.target.type, sgp.iter, sgp.targets=sgp.targets, sgp.scale.score.equated=equate.variable, SGPt=SGPt, fix.duplicates=fix.duplicates),
						Coefficient_Matrices=tmp_sgp_object[['Coefficient_Matrices']],
						Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, my.target.type)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1),
						my.grade=tail(sgp.iter[["sgp.grade.sequences"]], 1), my.extra.label=my.extra.label),
					use.my.coefficient.matrices=list(my.year=if (baseline.tf) "BASELINE" else tail(sgp.iter[["sgp.panel.years"]], 1),
						my.subject=tail(sgp.iter[[my.content.areas]], 1), my.extra.label=equate.label),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1)),
					performance.level.cutscores=state,
					max.forward.progression.years=sgp.iter[['sgp.projections.max.forward.progression.years']]+lag.increment,
					panel.data.vnames=getPanelDataVnames(my.target.type, sgp.iter, names(tmp_sgp_data_for_analysis), equate.variable),
					sgp.projections.equated=sgp.projections.equated,
					grade.progression=sgp.iter[[my.grade.sequences]],
					content_area.progression=sgp.iter[[my.content.areas]],
					year_lags.progression=sgp.iter[[my.panel.years.lags]],
					max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[[my.content.areas]], 1), state,
						sgp.projections.equated),
					lag.increment=lag.increment,
					grade.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					content_area.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					year_lags.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					percentile.trajectory.values=target.level,
					return.percentile.trajectory.values=SGP::SGPstateData[[state]][["SGP_Configuration"]][["return.percentile.trajectory.values"]],
					return.projection.group.identifier=projection_group.identifier,
					return.projection.group.scale.scores = !is.null(fix.duplicates),
					calculate.sgps=!(tail(sgp.iter[['sgp.panel.years']], 1) %in%
						SGP::SGPstateData[[state]][['Assessment_Program_Information']][['Scale_Change']][[tail(sgp.iter[[my.content.areas]], 1)]] &
						is.null(sgp.projections.equated)),
					sgp.projections.use.only.complete.matrices=SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']],
					SGPt=getSGPtNames(sgp.iter, SGPt, my.target.type),
					projcuts.digits=SGP::SGPstateData[[state]][['SGP_Configuration']][['projcuts.digits']]))

					if (any(tmp.tf <- sapply(tmp, function(x) any(class(x) %in% c("try-error", "simpleError"))))) {
						if (grepl(".lagged", target.type)) {
							tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']],
								sgp.target.scale.scores.lagged=getErrorReports(tmp, tmp.tf, par.sgp.config[['sgp.projections']]))
						}  else {
							tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']],
								sgp.target.scale.scores=getErrorReports(tmp, tmp.tf, par.sgp.config[['sgp.projections']]))
						}
					}
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp[!tmp.tf]), tmp_sgp_object)
					rm(tmp)
				} # END SNOW

				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config[[target.type]], function(sgp.iter) studentGrowthProjections(
						panel.data=list(
							Panel_Data=getPanelData(tmp_sgp_data_for_analysis, my.target.type, sgp.iter, sgp.targets=sgp.targets, sgp.scale.score.equated=equate.variable, SGPt=SGPt, fix.duplicates=fix.duplicates),
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]],
							Knots_Boundaries=getKnotsBoundaries(sgp.iter, state, my.target.type)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1),
							my.grade=tail(sgp.iter[["sgp.grade.sequences"]], 1), my.extra.label=my.extra.label),
						use.my.coefficient.matrices=list(my.year=if (baseline.tf) "BASELINE" else tail(sgp.iter[["sgp.panel.years"]], 1),
							my.subject=tail(sgp.iter[[my.content.areas]], 1), my.extra.label=equate.label),
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1)),
						performance.level.cutscores=state,
						max.forward.progression.years=sgp.iter[['sgp.projections.max.forward.progression.years']]+lag.increment,
						panel.data.vnames=getPanelDataVnames(my.target.type, sgp.iter, names(tmp_sgp_data_for_analysis), equate.variable),
						sgp.projections.equated=sgp.projections.equated,
						grade.progression=sgp.iter[[my.grade.sequences]],
						content_area.progression=sgp.iter[[my.content.areas]],
						year_lags.progression=sgp.iter[[my.panel.years.lags]],
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[[my.content.areas]], 1), state,
							sgp.projections.equated),
						lag.increment=lag.increment,
						grade.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
						content_area.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
						year_lags.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
						percentile.trajectory.values=target.level,
						return.percentile.trajectory.values=SGP::SGPstateData[[state]][["SGP_Configuration"]][["return.percentile.trajectory.values"]],
						return.projection.group.identifier=projection_group.identifier,
						return.projection.group.scale.scores = !is.null(fix.duplicates),
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in%
							SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[[my.content.areas]], 1)]] &
							is.null(sgp.projections.equated)),
						sgp.projections.use.only.complete.matrices=SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']],
						SGPt=getSGPtNames(sgp.iter, SGPt, my.target.type),
						projcuts.digits=SGP::SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]]),
						mc.cores=par.start$workers, mc.preschedule=FALSE)

					if (any(tmp.tf <- sapply(tmp, function(x) any(class(x) %in% c("try-error", "simpleError"))))) {
						if (grepl(".lagged", target.type)) {
							tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']],
								sgp.target.scale.scores.lagged=getErrorReports(tmp, tmp.tf, par.sgp.config[['sgp.projections']]))
						}  else {
							tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']],
								sgp.target.scale.scores=getErrorReports(tmp, tmp.tf, par.sgp.config[['sgp.projections']]))
						}
					}
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp[!tmp.tf]), tmp_sgp_object)
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
	} else { ### END if (!is.null(parallel.config))

		for (sgp.iter in par.sgp.config[[target.type]]) {
			panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, my.target.type, sgp.iter, sgp.targets=sgp.targets, sgp.scale.score.equated=equate.variable, SGPt=SGPt, fix.duplicates=fix.duplicates)))
			panel.data[['Coefficient_Matrices']] <- tmp_sgp_object[['Coefficient_Matrices']]
			panel.data[['Knots_Boundaries']] <- tmp_sgp_object[['Knots_Boundaries']]

			if (dim(panel.data$Panel_Data)[1] > 0) {
				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1),
						my.grade=tail(sgp.iter[["sgp.grade.sequences"]], 1), my.extra.label=my.extra.label),
					use.my.coefficient.matrices=list(my.year=if (baseline.tf) "BASELINE" else tail(sgp.iter[["sgp.panel.years"]], 1),
						my.subject=tail(sgp.iter[[my.content.areas]], 1), my.extra.label=equate.label),
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[[my.content.areas.label]], 1)),
					performance.level.cutscores=state,
					max.forward.progression.years=sgp.iter[['sgp.projections.max.forward.progression.years']]+lag.increment,
					panel.data.vnames=getPanelDataVnames(my.target.type, sgp.iter, names(tmp_sgp_data_for_analysis), equate.variable),
					sgp.projections.equated=sgp.projections.equated,
					grade.progression=sgp.iter[[my.grade.sequences]],
					content_area.progression=sgp.iter[[my.content.areas]],
					year_lags.progression=sgp.iter[[my.panel.years.lags]],
					max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[[my.content.areas]], 1), state,
						sgp.projections.equated),
					lag.increment=lag.increment,
					grade.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					content_area.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					year_lags.projection.sequence=SGP::SGPstateData[[state]][["SGP_Configuration"]][["year_lags.projection.sequence"]][[sgp.iter[["sgp.projection.sequence"]]]],
					percentile.trajectory.values=target.level,
					return.percentile.trajectory.values=SGP::SGPstateData[[state]][["SGP_Configuration"]][["return.percentile.trajectory.values"]],
					return.projection.group.identifier=projection_group.identifier,
					return.projection.group.scale.scores = !is.null(fix.duplicates),
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in%
						SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[[my.content.areas]], 1)]] &
						is.null(sgp.projections.equated)),
					sgp.projections.use.only.complete.matrices=SGP::SGPstateData[[state]][["SGP_Configuration"]][['sgp.projections.use.only.complete.matrices']],
					SGPt=getSGPtNames(sgp.iter, SGPt, my.target.type),
					projcuts.digits=SGP::SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]])
			} else {
				messageSGP(paste("\n\t\tNOTE: No student records &/or no prior data for scale score target student growth projections:", tail(sgp.iter[["sgp.panel.years"]], 1),
					tail(sgp.iter[[my.content.areas]], 1), "Grade", tail(sgp.iter[[my.grade.sequences]], 1), "to Projection Group", projection_group.identifier, "with", paste(paste(head(sgp.iter[[my.content.areas]], -1), "Grade", head(sgp.iter[[my.grade.sequences]], -1)), collapse=", "), "priors.\n"))
			}
		}
	} ### END if (is.null(parallel.config))

	sgp_object@SGP <- mergeSGP(tmp_sgp_object, sgp_object@SGP)
	return(sgp_object)
} ### END getTargetScaleScore
