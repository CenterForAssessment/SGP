`getTargetScaleScore` <- 
function(sgp_object,
	state,
	sgp.targets,
	target.type,
	target.level,
	tmp.years.content_areas.grades,
	parallel.config=NULL) {

	VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- GRADE <- NULL

	### Utility functions

	tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])
	setkey(sgp_object@Data, VALID_CASE, ID)
	tmp_sgp_data_for_analysis <- sgp_object@Data[SJ("VALID_CASE", sgp.targets[['ID']])][,
		c("VALID_CASE", "YEAR", "CONTENT_AREA", "GRADE", "ID", "SCALE_SCORE", grep("SGP_TARGET", names(sgp.targets), value=TRUE)), with=FALSE]
	setkey(tmp_sgp_data_for_analysis, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
	setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE)
	par.sgp.config <- getSGPConfig(
				sgp_object,
				sgp_object,
				sort(unique(tmp.years.content_areas.grades[['CONTENT_AREA']])),
				unique(tmp.years.content_areas.grades[['YEAR']]),
				sort(unique(tmp.years.content_areas.grades[['GRADE']])),
				sgp.config=NULL,
				sgp.percentiles.baseline=FALSE,
				sgp.projections.baseline=FALSE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.config.drop.nonsequential.grade.progression.variables=FALSE)

	if (!is.null(parallel.config)) {
		par.start <- startParallel(parallel.config, 'SG_PLOTS_TARGETS')
		
		###  FOREACH flavor

		if (toupper(parallel.config[["BACKEND"]]) == "FOREACH") {
			tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine="mergeSGP", .inorder=FALSE,
				.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
				return(studentGrowthProjections(
					panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, target.type, sgp.iter), 
						Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED_TARGET_SCALE_SCORES"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					panel.data.vnames=getPanelDataVnames(target.type, sgp.iter),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]]))
			}
			tmp_sgp_object <- mergeSGP(tmp_sgp_object, tmp)
			rm(tmp)
		} else {# END FOREACH
			###   SNOW flavor
			if (par.start$par.type == 'SNOW') {
				tmp <- clusterApplyLB(par.start$internal.cl, par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, target.type, sgp.iter), 
						Coefficient_Matrices=tmp_sgp_object[['Coefficient_Matrices']], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
					sgp.labels=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1), my.subject=tail(sgp.iter[['sgp.content.areas']], 1), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[['sgp.panel.years']], 1), my.subject=tail(sgp.iter[['sgp.content.areas']], 1)), 
					panel.data.vnames=getPanelDataVnames(target.type, sgp.iter),
					grade.progression=head(sgp.iter[['sgp.grade.sequences']][[1]], -1),
					max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[['sgp.panel.years']], 1), tail(sgp.iter[['sgp.content.areas']], 1), state),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[['sgp.panel.years']], 1) %in% SGPstateData[[state]][['Assessment_Program_Information']][['Scale_Change']][[tail(sgp.iter[['sgp.content.areas']], 1)]]),
					projcuts.digits=SGPstateData[[state]][['SGP_Configuration']][['projcuts.digits']])))

					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					rm(tmp)
				} # END SNOW
			
				###  MULTICORE flavor
				if (par.start$par.type == 'MULTICORE') {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
						panel.data=list(Panel_Data=getPanelData(tmp_sgp_data_for_analysis, target.type, sgp.iter), 
							Coefficient_Matrices=tmp_sgp_object[["Coefficient_Matrices"]], Knots_Boundaries=getKnotsBoundaries(sgp.iter, state)),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED_TARGET_SCALE_SCORES"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						panel.data.vnames=getPanelDataVnames(target.type, sgp.iter),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
						lag.increment=1,
						calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
						projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]]),
						mc.cores=par.start$workers, mc.preschedule=FALSE)
	
					tmp_sgp_object <- mergeSGP(Reduce(mergeSGP, tmp), tmp_sgp_object)
					if (any(tmp.tf <- sapply(tmp, function(x) identical(class(x), "try-error")))) {
						tmp_sgp_object[['Error_Reports']] <- c(tmp_sgp_object[['Error_Reports']], sgp.projections.lagged.=getErrorReports(tmp, tmp.tf, par.sgp.config))
					}
					rm(tmp)
				} # End MULTICORE
			} # END parallel flavors
		stopParallel(parallel.config, par.start)
		suppressMessages(gc()) # clean up
	} else { ### END if (!is.null(parallel.config))

			for (sgp.iter in par.sgp.config) {
				panel.data=within(tmp_sgp_object, assign("Panel_Data", getPanelData(tmp_sgp_data_for_analysis, target.type, sgp.iter)))
				tmp.knots.boundaries <- getKnotsBoundaries(sgp.iter, state) # Get specific knots and boundaries in case course sequence is different
				panel.data[["Knots_Boundaries"]][[names(tmp.knots.boundaries)]] <- tmp.knots.boundaries[[names(tmp.knots.boundaries)]]
	
				tmp_sgp_object <- studentGrowthProjections(
					panel.data=panel.data,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED_TARGET_SCALE_SCORES"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					panel.data.vnames=getPanelDataVnames("sgp.projections.lagged", sgp.iter),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					max.order.for.progression=getMaxOrderForProgression(tail(sgp.iter[["sgp.panel.years"]], 1), tail(sgp.iter[["sgp.content.areas"]], 1), state),
					lag.increment=1,
					calculate.sgps=!(tail(sgp.iter[["sgp.panel.years"]], 1) %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[tail(sgp.iter[["sgp.content.areas"]], 1)]]),
					projcuts.digits=SGPstateData[[state]][["SGP_Configuration"]][["projcuts.digits"]])
			}
		} ### END if (is.null(parallel.config))

} ### END getTargetScaleScore
