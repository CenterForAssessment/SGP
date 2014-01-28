`visualizeSGP` <- 
function(sgp_object,
		plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
		state=NULL,
		bPlot.years=NULL,
		bPlot.content_areas=NULL,
		bPlot.districts=NULL,
		bPlot.schools=NULL,
		bPlot.instructors=NULL,
		bPlot.styles=c(1),
		bPlot.levels=NULL,
		bPlot.level.cuts=NULL, 
		bPlot.full.academic.year=TRUE,
		bPlot.minimum.n=10,
		bPlot.anonymize=FALSE,
		bPlot.prior.achievement=TRUE, 
		bPlot.draft=FALSE,
		bPlot.demo=FALSE,
		bPlot.format="print",
		bPlot.folder="Visualizations/bubblePlots",
		sgPlot.save.sgPlot.data=FALSE,
		sgPlot.years=NULL,
		sgPlot.content_areas=NULL,
		sgPlot.districts=NULL,
		sgPlot.schools=NULL,
		sgPlot.reports.by.school=TRUE,
		sgPlot.instructors=NULL,
		sgPlot.reports.by.instructor=FALSE,
		sgPlot.students=NULL,
		sgPlot.reports.by.student=FALSE,
		sgPlot.header.footer.color="#4CB9CC",
		sgPlot.front.page=NULL,
		sgPlot.folder="Visualizations/studentGrowthPlots",
		sgPlot.folder.names="number",
		sgPlot.fan=TRUE,
		sgPlot.sgp.targets=FALSE,
		sgPlot.sgp.targets.timeframe=3,
		sgPlot.anonymize=FALSE,
		sgPlot.cleanup=TRUE,
		sgPlot.demo.report=FALSE,
		sgPlot.produce.plots=TRUE,
		sgPlot.baseline=NULL,
		sgPlot.zip=TRUE,
		sgPlot.output.format="PDF",
		sgPlot.year.span=5,
		gaPlot.years=NULL,
		gaPlot.content_areas=NULL, 
		gaPlot.students=NULL,
		gaPlot.format="print",
		gaPlot.baseline=NULL,
		gaPlot.max.order.for.progression=NULL,
		gaPlot.start.points="Achievement Level Cuts",
		gaPlot.folder="Visualizations/growthAchievementPlots",
		parallel.config=NULL) {

	started.at.visualizeSGP <- proc.time()
	message(paste("\nStarted visualizeSGP", date(), "\n"))

	### Setting variables to NULL to prevent R CMD check warnings

	DISTRICT_NUMBER <- DISTRICT_NAME <- SCHOOL_NUMBER <- SCHOOL_NAME <- YEAR <- CONTENT_AREA <- NULL ## To prevent R CMD check warnings
	ETHNICITY <- GENDER <- ID <- NULL ## To prevent R CMD check warnings
	TEST_LEVEL <- SUBJECT_CODE <- SCALE_SCORE <- GRADE <- NULL ## To prevent R CMD check warnings
	SCHOOL_ENROLLMENT_STATUS <- LAST_NAME <- FIRST_NAME <- NULL ## To prevent R CMD check warnings
	MEDIAN_SGP <- MEDIAN_SGP_COUNT <- VALID_CASE <- gaPlot.iter <- sgPlot.iter <- V1 <- variable <- INSTRUCTOR_NAME <- INSTRUCTOR_NUMBER <- NULL ## To prevent R CMD check warnings
	CONTENT_AREA_RESPONSIBILITY <- INSTRUCTOR_LAST_NAME <- INSTRUCTOR_FIRST_NAME <- TRANSFORMED_SCALE_SCORE <- SCALE_SCORE_ACTUAL <- CONTENT_AREA_LABELS <- NULL


	### Create state (if missing) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "visualizeSGP")
        }


	### Set up parallel.config if NULL

	if (is.null(parallel.config)) {
		parallel.config = list(BACKEND="PARALLEL", WORKERS=list(GA_PLOTS=1, SG_PLOTS=1))
	}

	### Utility functions	

	"%w/o%" <- function(x,y) x[!x %in% y]
	num_non_missing <- function(x) sum(!is.na(x))

	pretty_year <- function(x) sub("_", "-", x)

	get.max.order.for.progression <- function(year, content_area) {
		if (is.null(gaPlot.max.order.for.progression)) {
			if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]])) {
				return(NULL)
			} else {
				tmp <- as.numeric(tail(unlist(strsplit(as.character(year), "_")), 1)) - as.numeric(tail(unlist(strsplit(as.character(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]), "_")), 1))
				if (tmp < 0) return(NULL)
				if (tmp > 0) return(as.numeric(tmp))
				if (tmp==0) message(paste("\tNOTE: Based upon state scale changes in ", pretty_year(year), 
					". student growth projections are not possible. No student growth projections will be generated.\n", sep=""))
			}
		} else {
			return(gaPlot.max.order.for.progression)
		}
	} ### END get.max.order.for.progression


	get.sgPlot.iter <- function(districts.and.schools) {
		tmp.list <- list()
		for (k in 1:dim(districts.and.schools)[1]) {
			tmp.list[[k]] <- list(DISTRICT_NUMBER=districts.and.schools[["DISTRICT_NUMBER"]][k], SCHOOL_NUMBER=districts.and.schools[["SCHOOL_NUMBER"]][k])
		}
		return(tmp.list)
	} ### END get.sgPlot.iter


	get.gaPlot.iter <- function(gaPlot.years, gaPlot.content_areas, gaPlot.students, gaPlot.baseline) {

		tmp.list <- tmp.gaPlot.list <- tmp.df.list <- list()

		# Years 

		if (is.null(gaPlot.years)) {
			tmp.years <- tail(sort(unique(sgp_object@Data$YEAR)), 1)
		} else {
			tmp.years <- gaPlot.years
		}

		# Content Areas

		for (year.iter in seq_along(tmp.years)) {

			if (!is.null(gaPlot.content_areas)) {
				tmp.list[[year.iter]] <- data.table(YEAR=tmp.years[year.iter], CONTENT_AREA=gaPlot.content_areas)
			} else {
				setkey(sgp_object@Data, VALID_CASE, YEAR)
				tmp.list[[year.iter]] <- data.table(
					YEAR=tmp.years[year.iter],
					CONTENT_AREA=sort(intersect(
						unique(sgp_object@Data[SJ("VALID_CASE", tmp.years[year.iter]), nomatch=0][["CONTENT_AREA"]]),
						names(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Labels"]]))))
					setkeyv(sgp_object@Data, getKey(sgp_object))
			}
		}

		tmp.df <- rbind.fill(tmp.list)

		for (i in seq(length(gaPlot.baseline))) {
			tmp.df$BASELINE <- gaPlot.baseline[i]
			tmp.df.list[[i]] <- tmp.df
		}

		tmp.df <- rbind.fill(tmp.df.list)

		if (!is.null(gaPlot.students)) {
			 tmp.df <- merge(tmp.df, gaPlot.students)
			 names(tmp.df)[dim(tmp.df)[2]] <- "ID"
		}

		for (i in seq(dim(tmp.df)[1])) {
			tmp.gaPlot.list[[i]] <- list(YEAR=tmp.df[["YEAR"]][i], CONTENT_AREA=tmp.df[["CONTENT_AREA"]][i], ID=tmp.df[["ID"]][i], BASELINE=tmp.df[["BASELINE"]][i])
		}
		return(tmp.gaPlot.list)

	} ### END get.gaPlot.iter

	get.gaPlot.object <- function(sgp_object) {
		tmp.sgp <- new("SGP")
		tmp.sgp@Data <- sgp_object@Data
		tmp.sgp@SGP <- sgp_object@SGP[c("Coefficient_Matrices", "Knots_Boundaries")]
		return(tmp.sgp)
	} ### END get.gaPlot.object

##############################################################################################################
#### bubblePlot
##############################################################################################################

	if ("bubblePlot" %in% plot.types) {

		started.at <- proc.time()
		message(paste("Started bubblePlot in visualizeSGP", date(), "\n"))

		bubblePlot_Styles(sgp_object=sgp_object,
			state=state,
			bPlot.years=bPlot.years,
			bPlot.content_areas=bPlot.content_areas,
			bPlot.districts=bPlot.districts,
			bPlot.schools=bPlot.schools,
			bPlot.instructors=bPlot.instructors,
			bPlot.styles=bPlot.styles,
			bPlot.levels=bPlot.levels,
			bPlot.level.cuts=bPlot.level.cuts,
			bPlot.full.academic.year=bPlot.full.academic.year,
			bPlot.minimum.n=bPlot.minimum.n,
			bPlot.anonymize=bPlot.anonymize,
			bPlot.prior.achievement=bPlot.prior.achievement, 
			bPlot.draft=bPlot.draft,
			bPlot.demo=bPlot.demo,
			bPlot.format=bPlot.format,
			bPlot.folder=bPlot.folder)

		message(paste("Finished bubblePlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
	} ## END bubblePlot %in% plot.types	


####################################################################################################################
#### growthAchievementPlot
####################################################################################################################

	if ("growthAchievementPlot" %in% plot.types) {

		started.at <- proc.time()
		message(paste("Started growthAchievementPlot in visualizeSGP", date(), "\n"))

		# gaPlot.baseline stuff

		if (is.null(gaPlot.baseline)) {
			gaPlot.baseline <- FALSE ## Default to FALSE if not set by user
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort Referenced") gaPlot.baseline <- FALSE
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Baseline Referenced") gaPlot.baseline <- TRUE
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced") gaPlot.baseline <- c(TRUE, FALSE)
		}

		par.start <- startParallel(parallel.config, 'GA_PLOTS')

		gaPlot.sgp_object <- get.gaPlot.object(sgp_object)
		
		if (par.start$par.type=="FOREACH") {

			foreach(gaPlot.iter=iter(get.gaPlot.iter(gaPlot.years, gaPlot.content_areas, gaPlot.students, gaPlot.baseline)), .packages="SGP", .inorder=FALSE,
				.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					growthAchievementPlot(
						gaPlot.sgp_object=gaPlot.sgp_object,
						gaPlot.students=gaPlot.iter[["ID"]],
						gaPlot.max.order.for.progression=get.max.order.for.progression(gaPlot.iter[["YEAR"]], gaPlot.iter[["CONTENT_AREA"]]),
						gaPlot.start.points=gaPlot.start.points,
						state=state,
						content_area=gaPlot.iter[["CONTENT_AREA"]],
						year=gaPlot.iter[["YEAR"]], 
						format=gaPlot.format,
						baseline=gaPlot.iter[["BASELINE"]],
						output.format=c("PDF", "PNG"),
						output.folder=file.path(gaPlot.folder, gaPlot.iter[["YEAR"]]))

			} ## END dopar 
		} ## END FOREACH
		
		if (par.start$par.type=="SNOW") {
			
			gaPlot.list <- get.gaPlot.iter(gaPlot.years, gaPlot.content_areas, gaPlot.students, gaPlot.baseline)
			clusterApplyLB(par.start$internal.cl, gaPlot.list, function(gaPlot.iter) 
				growthAchievementPlot(
						gaPlot.sgp_object=gaPlot.sgp_object,
						gaPlot.students=gaPlot.iter[["ID"]],
						gaPlot.max.order.for.progression=get.max.order.for.progression(gaPlot.iter[["YEAR"]], gaPlot.iter[["CONTENT_AREA"]]),
						gaPlot.start.points=gaPlot.start.points,
						state=state,
						content_area=gaPlot.iter[["CONTENT_AREA"]],
						year=gaPlot.iter[["YEAR"]], 
						format=gaPlot.format,
						baseline=gaPlot.iter[["BASELINE"]],
						output.format=c("PDF", "PNG"),
						output.folder=file.path(gaPlot.folder, gaPlot.iter[["YEAR"]])))
		}
		
		if (par.start$par.type=="MULTICORE") {
			gaPlot.list <- get.gaPlot.iter(gaPlot.years, gaPlot.content_areas, gaPlot.students, gaPlot.baseline)
			mclapply(gaPlot.list, function(gaPlot.iter) {
						growthAchievementPlot(
						gaPlot.sgp_object=gaPlot.sgp_object,
						gaPlot.students=gaPlot.iter[["ID"]],
						gaPlot.max.order.for.progression=get.max.order.for.progression(gaPlot.iter[["YEAR"]], gaPlot.iter[["CONTENT_AREA"]]),
						gaPlot.start.points=gaPlot.start.points,
						state=state,
						content_area=gaPlot.iter[["CONTENT_AREA"]],
						year=gaPlot.iter[["YEAR"]], 
						format=gaPlot.format,
						baseline=gaPlot.iter[["BASELINE"]],
						output.format=c("PDF", "PNG"),
						output.folder=file.path(gaPlot.folder, gaPlot.iter[["YEAR"]]))}, 
				mc.cores=par.start$workers, mc.preschedule=FALSE)
		}
		
		stopParallel(parallel.config, par.start)

		message(paste("Finished growthAchievementPlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
	} ## END if (growthAchievementPlot %in% plot.types)


####################################################################################################################
#### studentGrowthPlot
####################################################################################################################

if ("studentGrowthPlot" %in% plot.types) {

	started.at <- proc.time()
	message(paste("Started studentGrowthPlot in visualizeSGP", date(), "\n"))

	#### Utility functions

	get.next.grade <- function(grade, content_area) {
		if (is.na(grade)) {
			return(NA)
		} else {
			tmp.grades.reported <- SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area]]
			c(tmp.grades.reported, NA)[match(grade, tmp.grades.reported)+1]
		}
	}

	get.my.label <- function(state, content_area, year, label="Cutscores") {
		tmp.cutscore.years <- sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][[label]])[grep(content_area, names(SGPstateData[[state]][["Achievement"]][[label]]))], "[.]"),
                        function(x) x[2])
		if (any(!is.na(tmp.cutscore.years))) {
			if (year %in% tmp.cutscore.years) {
				return(paste(content_area, year, sep="."))
			} else {
				if (year==sort(c(year, tmp.cutscore.years))[1]) {
					return(content_area)
				} else {
					return(paste(content_area, sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1], sep="."))
				}
			}
		} else {
			return(content_area)
		}
	}

	piecewise.transform <- function(scale_score, state, content_area, year, grade, output.digits=1) {
		my.cutscores.label <- get.my.label(state, content_area, year)
		my.knots_boundaries.label <- get.my.label(state, content_area, year, "Knots_Boundaries")
		if (content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &&
			grade %in% as.numeric(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[my.knots_boundaries.label]]), "_")), ncol=2, byrow=TRUE)[,2])) {
				tmp.loss.hoss <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[my.knots_boundaries.label]][[paste("loss.hoss_", grade, sep="")]]
				scale_score[scale_score < tmp.loss.hoss[1]] <- tmp.loss.hoss[1]; scale_score[scale_score > tmp.loss.hoss[2]] <- tmp.loss.hoss[2]
				tmp.old.cuts <- c(tmp.loss.hoss[1], SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.cutscores.label]][[paste("GRADE_", grade, sep="")]], tmp.loss.hoss[2])
				tmp.new.cuts <- SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]]
				tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
				tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
				round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
		} else {
			as.numeric(scale_score)
		}
	} ## END piecewise.transform


######################################################################
##### DISTINGUISH CASES WHEN WIDE data is or is not provided
######################################################################

	#### Some checks

		if (is.SGP(sgp_object)) {
			sgPlot.wide.data <- FALSE
			slot.data <- copy(sgp_object@Data)
		} else {
			slot.data <- sgp_object
		}

		if (!is.null(sgPlot.students) | !is.null(sgPlot.instructors) | !is.null(sgPlot.schools) | !is.null(sgPlot.districts)) sgPlot.demo.report <- FALSE
		if (!is.null(sgPlot.students)) sgPlot.reports.by.student <- TRUE
		if (!is.null(sgPlot.instructors)) sgPlot.reports.by.instructor <- TRUE
		if (!is.null(sgPlot.schools)) sgPlot.reports.by.school <- TRUE
		if (!is.null(sgPlot.districts)) sgPlot.reports.by.school <- TRUE
		if (is.data.frame(sgp_object)) sgPlot.wide.data <- TRUE

		if (is.null(sgPlot.baseline)) {
			sgPlot.baseline <- FALSE ## Default to cohort referenced SGPs if not set by user
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort Referenced") sgPlot.baseline <- FALSE
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Baseline Referenced") sgPlot.baseline <- TRUE
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced") sgPlot.baseline <- FALSE
		}

		if (!is.null(sgPlot.sgp.targets) && !is.null(SGPstateData[[state]][['SGP_Configuration']][['sgPlot.sgp.targets.timeframe']])) {
			sgPlot.sgp.targets.timeframe <- SGPstateData[[state]][['SGP_Configuration']][['sgPlot.sgp.targets.timeframe']]
		}

		if (is.null(sgPlot.sgp.targets) && !is.null(SGPstateData[[state]][['SGP_Configuration']][['sgPlot.sgp.targets']])) {
			sgPlot.sgp.targets <- SGPstateData[[state]][['SGP_Configuration']][['sgPlot.sgp.targets']]
		}

		if (identical(sgPlot.sgp.targets, TRUE)) {
			if (sgPlot.baseline) {
				sgPlot.sgp.targets <- c("sgp.projections.baseline", "sgp.projections.lagged.baseline")
			} else {
				sgPlot.sgp.targets <- c("sgp.projections", "sgp.projections.lagged")
			}
		} 
		if (identical(sgPlot.sgp.targets, FALSE)) {
			if (!is.null(SGPstateData[[state]][['SGP_Configuration']][['sgPlot.sgp.targets']])) {
				sgPlot.sgp.targets <- SGPstateData[[state]][['SGP_Configuration']][['sgPlot.sgp.targets']]
			} else {
				sgPlot.sgp.targets <- NULL
			}
		}
		if (!is.null(sgPlot.sgp.targets) & !sgPlot.baseline && !all(sgPlot.sgp.targets %in% c("sgp.projections", "sgp.projections.lagged"))) {
			message("\tNOTE: 'sgPlot.sgp.targets' must consist of 'sgp.projections' and/or 'sgp.projections.lagged'.")
			sgPlot.sgp.targets <- NULL
		}
		if (!is.null(sgPlot.sgp.targets) & sgPlot.baseline && !all(sgPlot.sgp.targets %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline"))) {
			message("\tNOTE: 'sgPlot.sgp.targets' must consist of 'sgp.projections.baseline' and/or 'sgp.projections.lagged.baseline'.")
			sgPlot.sgp.targets <- NULL
		}

		if (sgPlot.baseline) {
			my.sgp <- "SGP_BASELINE"
			my.sgp.level <- "SGP_LEVEL_BASELINE"
			my.sgp.targets <- grep("SCALE_SCORE", grep("BASELINE", grep("SGP_TARGET", names(slot.data), value=TRUE), value=TRUE), value=TRUE, invert=TRUE)
			if (identical("sgp.projections.baseline", sgPlot.sgp.targets)) my.sgp.targets <- grep("CURRENT", my.sgp.targets, value=TRUE) 
			if (identical("sgp.projections.lagged.baseline", sgPlot.sgp.targets)) my.sgp.targets <- grep("CURRENT", my.sgp.targets, value=TRUE, invert=TRUE) 
			if (is.null(sgPlot.sgp.targets)) my.sgp.targets <- NULL
		} else {
			my.sgp <- "SGP"
			my.sgp.level <- "SGP_LEVEL"
			my.sgp.targets <- grep("SCALE_SCORE", grep("BASELINE", grep("SGP_TARGET", names(slot.data), value=TRUE), value=TRUE, invert=TRUE), value=TRUE, invert=TRUE)
			if (identical("sgp.projections", sgPlot.sgp.targets)) my.sgp.targets <- grep("CURRENT", my.sgp.targets, value=TRUE) 
			if (identical("sgp.projections.lagged", sgPlot.sgp.targets)) my.sgp.targets <- grep("CURRENT", my.sgp.targets, value=TRUE, invert=TRUE) 
			if (is.null(sgPlot.sgp.targets)) my.sgp.targets <- NULL
		}

		if (!is.null(my.sgp.targets)) sgPlot.sgp.targets.timeframe <- as.numeric(rev(unlist(strsplit(unlist(strsplit(my.sgp.targets[1], "_YEAR"))[1], "_")))[1])

		if (sgPlot.demo.report & sgPlot.wide.data) {
			message("\tNOTE: Demonstration report is not supported using wide data. Process will proceed with demonstration report production using long data.\n")
			sgPlot.demo.report <- FALSE
		}


################################################
######## IF sgPlot.wide.data is supplied 
################################################

if (sgPlot.wide.data) { ### When WIDE data is provided

	#### Calculate years and content area from data
 
		tmp.all.years <- sort(unique(sapply(strsplit(names(sgp_object), "[.]"), function(x) x[2])))
		tmp.years.subset <- tail(tmp.all.years, sgPlot.year.span)
		tmp.last.year <- tail(tmp.all.years, 1)
		tmp.content_areas_domains <- unique(sgp_object[["CONTENT_AREA"]])

	#### Reconcile School and District selections

		if (is.null(sgPlot.students)) { ## sgPlot.students NOT specified with WIDE data

			## Calculate School size to order from largest to smallest and set key on WIDE data

			setkeyv(sgp_object, paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep="."))
			tmp.districts.and.schools.size <- sgp_object[,num_non_missing(ID), by=key(sgp_object)] 
			setnames(tmp.districts.and.schools.size, 1:2, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))

			setkeyv(sgp_object, c("CONTENT_AREA", paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))

			if (is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object[tmp.content_areas_domains], 
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			}

			if (is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object[CJ(tmp.content_areas_domains, sgPlot.districts)],
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			}

			if (!is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
				setkeyv(sgp_object, c("CONTENT_AREA", paste(c("SCHOOL_NUMBER", "DISTRICT_NUMBER"), tmp.last.year, sep=".")))
				tmp.districts.and.schools <- unique(data.table(sgp_object[CJ(tmp.content_areas_domains, sgPlot.schools)],
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			}

			if (!is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
				tmp.list <- list()
				for (i in seq_along(sgPlot.districts)) {
					tmp.schools <- unique(sgp_object[CJ(tmp.content_areas_domains, sgPlot.districts[i])][[paste("SCHOOL_NUMBER", tmp.last.year, sep=".")]])
					if (any(sgPlot.schools==tmp.schools)) {
						tmp.list[[i]] <- tmp.schools[sgPlot.schools==tmp.schools]
					} else {
						tmp.list[[i]] <- tmp.schools
					}
				}
				sgPlot.schools <- unique(c(sgPlot.schools, do.call(c, tmp.list)))
				setkeyv(sgp_object, c("CONTENT_AREA", paste(c("SCHOOL_NUMBER", "DISTRICT_NUMBER"), tmp.last.year, sep=".")))
				tmp.districts.and.schools <- unique(data.table(sgp_object[CJ(tmp.content_areas_domains, sgPlot.schools)],
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
				setkeyv(sgp_object, c("CONTENT_AREA", paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))
			}

			## Remove NAs (these arise from students without scores in ALL content areas)

			tmp.tf <- with(tmp.districts.and.schools, 
				eval(parse(text=paste("!is.na(DISTRICT_NUMBER.", tmp.last.year, ") & !is.na(SCHOOL_NUMBER.", tmp.last.year, ")", sep=""))))
			tmp.districts.and.schools <- tmp.districts.and.schools[tmp.tf]
			setnames(tmp.districts.and.schools, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			tmp.districts.and.schools <- tmp.districts.and.schools.size[tmp.districts.and.schools][order(V1, decreasing=TRUE)][,list(DISTRICT_NUMBER, SCHOOL_NUMBER)]

			## Get cases

			setkeyv(sgp_object, c("CONTENT_AREA", paste("SCHOOL_NUMBER", tmp.last.year, sep=".")))
			tmp.ids <- unique(sgp_object[CJ(tmp.content_areas_domains, tmp.districts.and.schools[["SCHOOL_NUMBER"]]), nomatch=0][["ID"]])
			setkey(sgp_object, ID)
			sgPlot.data <- sgp_object[list(tmp.ids)]
		} else { ## sgPlot.students specified with WIDE data
			setkey(sgp_object, ID)
			sgPlot.data <- sgp_object[list(sgPlot.students)]
			tmp.districts.and.schools <- unique(data.table(sgPlot.data, 
				key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
				c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			tmp.tf <- with(tmp.districts.and.schools, 
				eval(parse(text=paste("!is.na(DISTRICT_NUMBER.", tmp.last.year, ") & !is.na(SCHOOL_NUMBER.", tmp.last.year, ")", sep=""))))
			tmp.districts.and.schools <- tmp.districts.and.schools[tmp.tf]
			setnames(tmp.districts.and.schools, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
		}
} else { ### When WIDE data is NOT provided
###############################################
######## IF WIDE data is NOT supplied
###############################################

	#### Create FIRST_NAME, LAST_NAME, SCHOOL_NUMBER, SCHOOL_NAME, DISTRICT_NUMBER, DISTRICT_NAME if they don't exist

	if (!"FIRST_NAME" %in% names(slot.data)) slot.data[,FIRST_NAME:=""]
	if (!"LAST_NAME" %in% names(slot.data)) slot.data[,LAST_NAME:=""]
	if (!"SCHOOL_NAME" %in% names(slot.data)) slot.data[,SCHOOL_NAME:=""]
	if (!"SCHOOL_NUMBER" %in% names(slot.data)) slot.data[,SCHOOL_NUMBER:=""]
	if (!"DISTRICT_NAME" %in% names(slot.data)) slot.data[,DISTRICT_NAME:=""]
	if (!"DISTRICT_NUMBER" %in% names(slot.data)) slot.data[,DISTRICT_NUMBER:=""]

	#### Set key on LONG data

	long.key <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
	setkeyv(slot.data, long.key)

	#### Year stuff (NECESSARY even IF WIDE data is provided)

	if (is.null(sgPlot.years)) {
		tmp.years <- sort(unique(slot.data["VALID_CASE"][["YEAR"]])) 
		tmp.years.subset <- tail(tmp.years, sgPlot.year.span)
		tmp.last.year <- tail(tmp.years, 1)
	} else {
		tmp.years <- sort(unique(slot.data["VALID_CASE"][["YEAR"]])) 
		tmp.years.subset <- tail(tmp.years[1:which(tmp.years==tail(sort(sgPlot.years), 1))], sgPlot.year.span)
		tmp.last.year <- tail(tmp.years, 1)
	}

	#### Content area stuff (NECESSARY regardless of whether sgPlot.students is provided)

	slot.data[,CONTENT_AREA_LABELS:=CONTENT_AREA]
	if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])) {
		for (i in names(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])) {
			slot.data[VALID_CASE=="VALID_CASE" & CONTENT_AREA==i, CONTENT_AREA:=SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]][[i]]]	
		}
		setkeyv(slot.data, long.key)
	}

	if (is.null(sgPlot.content_areas)) {
		tmp.content_areas_domains <- sort(intersect(
						unique(slot.data[SJ("VALID_CASE", tmp.last.year), nomatch=0][["CONTENT_AREA"]]),
						names(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Labels"]])))
	} else {
		tmp.content_areas_domains <- sgPlot.content_areas
	}


	#### Melt and rename data if reporting by instructor number

	if (sgPlot.reports.by.instructor) {
		if (!"INSTRUCTOR_NUMBER" %in% names(sgp_object@Data_Supplementary)) {
			stop("\tNOTE: To create 'sgPlot.reports.by.instructor' an INSTRUCTOR_NUMBER lookup table must exist in @Data_Supplementary.")
		}
		lookup.variables.to.get <- c("ID", "INSTRUCTOR_NUMBER", "INSTRUCTOR_LAST_NAME", "INSTRUCTOR_FIRST_NAME")
		district.and.school.variable.names <- c("VALID_CASE", "ID", "CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
		student.teacher.lookup <- data.table(sgp_object@Data_Supplementary[['INSTRUCTOR_NUMBER']][YEAR==tmp.last.year][,VALID_CASE:="VALID_CASE"], 
						key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))[sgp_object@Data[,district.and.school.variable.names,with=FALSE], nomatch=0]

		if (!is.null(sgPlot.instructors)) {
			student.teacher.lookup <- subset(student.teacher.lookup, INSTRUCTOR_NUMBER %in% sgPlot.instructors)
		} else {
			student.teacher.lookup <- subset(student.teacher.lookup, !is.na(INSTRUCTOR_NUMBER))
		}

		if ("INSTRUCTOR_LAST_NAME" %in% names(student.teacher.lookup)) {
			student.teacher.lookup[,INSTRUCTOR_NAME:=paste(INSTRUCTOR_LAST_NAME, INSTRUCTOR_FIRST_NAME, sep=", ")]
			student.teacher.lookup[,INSTRUCTOR_LAST_NAME:=NULL]; student.teacher.lookup[,INSTRUCTOR_FIRST_NAME:=NULL]
		} else {
			student.teacher.lookup[,INSTRUCTOR_NAME:=paste("Instructor", INSTRUCTOR_NUMBER)]
		}

		tmp.district.and.schools.instructors <- unique(data.table(student.teacher.lookup, key=long.key)[,district.and.school.variable.names, with=FALSE])
		student.teacher.lookup <- student.teacher.lookup[,list(ID, CONTENT_AREA, INSTRUCTOR_NUMBER, INSTRUCTOR_NAME)]
		student.teacher.lookup[,CONTENT_AREA_RESPONSIBILITY:=factor(1, levels=0:1, labels=c("Content Area Responsibility: No", "Content Area Responsibility: Yes"))]
		setnames(student.teacher.lookup, c("INSTRUCTOR_NUMBER", "INSTRUCTOR_NAME"), paste(c("INSTRUCTOR_NUMBER", "INSTRUCTOR_NAME"), tmp.last.year, sep="."))
	}


	#### The following apply when sgPlot.students is not supplied 

	if (is.null(sgPlot.students)) {

		#### Demo report student selection (only available with LONG data) 

		if (sgPlot.demo.report) {
			sgPlot.anonymize <- TRUE
			tmp.ids <- list()
			setkeyv(slot.data, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA_LABELS"))
			if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]])) {
				tmp.data.table <- list()
				for (i in names(grep(unique(unlist(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]]))[1], unlist(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]]), value=TRUE))) {
					tmp.grades.reported <- SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[i]]
					tmp.data.table[[i]] <- data.table(
						VALID_CASE="VALID_CASE",
						YEAR=tmp.last.year,
						GRADE=tmp.grades.reported, 
						CONTENT_AREA_LABELS=i)
				}
				tmp.grades.content_areas.reported <- data.table(rbindlist(tmp.data.table), key=key(slot.data))
			} else {
				tmp.grades.reported <- as.character(unique(unlist(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]])))
				tmp.grades.content_areas.reported <- data.table(
					VALID_CASE="VALID_CASE",
					YEAR=tmp.last.year,
					GRADE=tmp.grades.reported, 
					CONTENT_AREA_LABELS=names(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]])[1], key=key(slot.data))
			}
			tmp.grades.content_areas.reported <- unique(slot.data)[,key(slot.data), with=FALSE][tmp.grades.content_areas.reported, nomatch=0]
			for (i in seq(dim(tmp.grades.content_areas.reported)[1])) {
				tmp.ids[[i]] <- as.character(sample(unique(slot.data[tmp.grades.content_areas.reported[i]]$ID), 10))
			}
			slot.data[,c("SCHOOL_NUMBER", "DISTRICT_NUMBER") := NULL]
			slot.data[slot.data$ID %in% unlist(tmp.ids), c("SCHOOL_NUMBER", "DISTRICT_NUMBER") := list(-99L, -999L)]
			tmp.districts.and.schools <- CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains, -999L, -99L)
			setnames(tmp.districts.and.schools, c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			setkeyv(tmp.districts.and.schools, long.key) 
			setkeyv(slot.data, long.key)
		} else {

		#### Reconcile School and District selections

			if (sgPlot.reports.by.instructor) {
				tmp.districts.and.schools <- tmp.district.and.schools.instructors
			} else {
				if (is.null(sgPlot.schools) | is.null(sgPlot.districts)) {
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
				} 
				if (is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains, sgPlot.districts)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
				}
				if (!is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
					setkeyv(slot.data, c("VALID_CASE", "YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains, sgPlot.schools)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
					setkeyv(slot.data, long.key)
				}
				if (!is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
					tmp.list <- list()
					for (i in seq_along(sgPlot.districts)) {
						tmp.schools <- unique(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains, sgPlot.districts[i])][["SCHOOL_NUMBER"]])
						if (any(sgPlot.schools==tmp.schools)) {
							tmp.list[[i]] <- tmp.schools[sgPlot.schools==tmp.schools]
						} else {
							tmp.list[[i]] <- tmp.schools
						}
					}
					sgPlot.schools <- unique(c(sgPlot.schools, do.call(c, tmp.list)))
					setkeyv(slot.data, c("VALID_CASE", "YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains, sgPlot.schools)][,
							list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
							key=long.key))
					setkeyv(slot.data, long.key)
				}
			}  ## END if else (sgPlot.report.by.instructors)
		} ## END if else (sgPlot.demo.report)
	} ## END if (is.null(sgPlot.students))

	#### Subset data (NOT NECESSARY IF WIDE data is provided)

		setkeyv(slot.data, c("VALID_CASE", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
		tmp.districts.and.schools.size <- slot.data[SJ("VALID_CASE", tmp.last.year)][,num_non_missing(ID), by=list(DISTRICT_NUMBER, SCHOOL_NUMBER)]
		setkeyv(tmp.districts.and.schools.size, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
		setkeyv(slot.data, long.key)

		if (is.null(sgPlot.students)) {
			report.ids <- unique(slot.data[tmp.districts.and.schools][["ID"]])
			if (sgPlot.reports.by.instructor) report.ids <- intersect(student.teacher.lookup[['ID']], report.ids)
			setkeyv(slot.data, c("CONTENT_AREA", "GRADE", "YEAR"))
			tmp.table <- data.table(slot.data[getYearsContentAreasGrades(state, years=tmp.years, content_areas_domains=tmp.content_areas_domains), nomatch=0], 
				key=c("ID", "CONTENT_AREA", "YEAR", "VALID_CASE"))[CJ(report.ids, tmp.content_areas_domains, tmp.years, "VALID_CASE")]
		} else {
			report.ids <- sgPlot.students
			setkeyv(slot.data, c("CONTENT_AREA", "GRADE", "YEAR"))
			tmp.table <- data.table(slot.data[getYearsContentAreasGrades(state, years=tmp.years, content_areas_domains=tmp.content_areas_domains), nomatch=0], 
				key=c("VALID_CASE", "ID", "CONTENT_AREA", "YEAR"))[CJ("VALID_CASE", report.ids, tmp.content_areas_domains, tmp.years)]
			setkeyv(tmp.table, c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			tmp.districts.and.schools <- tmp.table[CJ("VALID_CASE", tmp.last.year, tmp.content_areas_domains)][, list(DISTRICT_NUMBER, SCHOOL_NUMBER)]
		}

	#### Trim tmp.districts.and.schools

		tmp.districts.and.schools <- unique(data.table(tmp.districts.and.schools[,list(DISTRICT_NUMBER, SCHOOL_NUMBER)], key=c("DISTRICT_NUMBER", "SCHOOL_NUMBER")))
		tmp.districts.and.schools <- subset(tmp.districts.and.schools, !is.na(DISTRICT_NUMBER) & !is.na(SCHOOL_NUMBER))
		if (!sgPlot.demo.report) tmp.districts.and.schools <- tmp.districts.and.schools.size[tmp.districts.and.schools][order(V1, decreasing=TRUE)][,list(DISTRICT_NUMBER, SCHOOL_NUMBER)]


	#### Invalidate bad SGPs (if necessary)

		if ("VALID_SGP" %in% names(tmp.table)) {
			tmp.table$SGP[tmp.table$VALID_SGP=="INVALID_SGP"] <- NA
			if ("No SGP Provided" %in% levels(tmp.table$SGP_LEVEL)) {
				levels(tmp.table$SGP_LEVEL) <- c(levels(tmp.table$SGP_LEVEL), "No SGP Provided")
				tmp.table$SGP_LEVEL[tmp.table$VALID_SGP=="INVALID_SGP"] <- "No SGP Provided"
			}
		}

	#### Create transformed scale scores (NOT necessary if wide data is provided)

		setkeyv(tmp.table, c("CONTENT_AREA_LABELS", "YEAR", "GRADE"))
		tmp.table[, TRANSFORMED_SCALE_SCORE := piecewise.transform(SCALE_SCORE, state, CONTENT_AREA_LABELS, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA_LABELS, YEAR, GRADE)]

	#### Change SCALE_SCORE if Scale_Score_Lookup exists in SGPstateData (NOT necessary if wide data is provided)

		if ("SCALE_SCORE_ACTUAL" %in% names(sgp_object@Data)) {
			tmp.table[,SCALE_SCORE:=SCALE_SCORE_ACTUAL]
		}

	#### Anonymize (if requested) (NOT necessary if wide data is provided)
 
		if (sgPlot.anonymize) {
			suppressPackageStartupMessages(require(randomNames))
			if (!"ETHNICITY" %in% names(tmp.table)) tmp.table[["ETHNICITY"]] <- 1
			if (!"GENDER" %in% names(tmp.table)) tmp.table[["GENDER"]] <- round(runif(dim(tmp.table)[1], min=0, max=1))
			if ("LAST_NAME" %in% names(tmp.table)) tmp.table[,LAST_NAME:=NULL]
			if ("FIRST_NAME" %in% names(tmp.table)) tmp.table[,FIRST_NAME:=NULL]
			tmp.dt <- tmp.table[,list(ID, ETHNICITY, GENDER)]
			setkey(tmp.dt, ID)
			tmp.dt <- tmp.dt[!duplicated(tmp.dt),]
			tmp.dt[,LAST_NAME := randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="last")]
			tmp.dt[,FIRST_NAME := randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="first")]

			names.dt <- tmp.dt[,list(ID, LAST_NAME, FIRST_NAME)]
			setkey(names.dt, ID)

			setkey(tmp.table, ID)
			tmp.table <- names.dt[tmp.table]
			if (sgPlot.demo.report) {
				tmp.table[,DISTRICT_NAME := as.factor("Sample District")]
				tmp.table[,SCHOOL_NAME := as.factor("Sample School")]
			} else {
				setkey(tmp.table, DISTRICT_NUMBER)
				tmp.district.number <- data.table(DISTRICT_NUMBER=unique(tmp.table$DISTRICT_NUMBER) %w/o% NA, seq_along(unique(tmp.table$DISTRICT_NUMBER) %w/o% NA), 
					key="DISTRICT_NUMBER")[tmp.table][['V2']]
				tmp.table[, DISTRICT_NAME := as.character(tmp.table$DISTRICT_NAME)]
				tmp.table[!is.na(tmp.table$DISTRICT_NUMBER), DISTRICT_NAME := paste("Sample District", tmp.district.number[!is.na(tmp.table$DISTRICT_NUMBER)])]
				tmp.table[, DISTRICT_NAME := as.factor(tmp.table$DISTRICT_NAME)]
	
				setkey(tmp.table, SCHOOL_NUMBER)
				tmp.school.number <- data.table(SCHOOL_NUMBER=unique(tmp.table$SCHOOL_NUMBER) %w/o% NA, seq_along(unique(tmp.table$SCHOOL_NUMBER) %w/o% NA), 
					key="SCHOOL_NUMBER")[tmp.table]$V2
				tmp.table[SCHOOL_NAME := as.character(tmp.table$SCHOOL_NAME)]
				tmp.table[!is.na(tmp.table$SCHOOL_NUMBER), SCHOOL_NAME := paste("Sample School", tmp.school.number[!is.na(tmp.table$SCHOOL_NUMBER)])]
				tmp.table[, SCHOOL_NAME := as.factor(tmp.table$SCHOOL_NAME)]
			}
		} ## END if (sgPlot.anonymize)

	#### Reshape data (NOT NECESSARY IF WIDE data is provided)

		variables.to.keep <- c("VALID_CASE", "ID", "LAST_NAME", "FIRST_NAME", "CONTENT_AREA", "CONTENT_AREA_LABELS", "YEAR", "GRADE", 
			"SCALE_SCORE", "TRANSFORMED_SCALE_SCORE", "ACHIEVEMENT_LEVEL", my.sgp, my.sgp.level, my.sgp.targets, "SCHOOL_NAME", "SCHOOL_NUMBER", "DISTRICT_NAME", "DISTRICT_NUMBER")

		sgPlot.data <- reshape(tmp.table[,variables.to.keep, with=FALSE],
			idvar=c("ID", "CONTENT_AREA"),
			timevar="YEAR",
			drop=c("VALID_CASE"),
			direction="wide")

		variables.to.keep <- c("ID", "CONTENT_AREA", paste("CONTENT_AREA_LABELS", tmp.years, sep="."),
			paste("LAST_NAME", tmp.last.year, sep="."), paste("FIRST_NAME", tmp.last.year, sep="."), paste("GRADE", tmp.years, sep="."), 
			paste(my.sgp, tmp.years, sep="."), paste("SCALE_SCORE", tmp.years, sep="."), paste("TRANSFORMED_SCALE_SCORE", tmp.years, sep="."), 
			paste("ACHIEVEMENT_LEVEL", tmp.years, sep="."), paste(my.sgp.level, tmp.years, sep="."),
			paste("SCHOOL_NAME", tmp.last.year, sep="."), paste("SCHOOL_NUMBER", tmp.last.year, sep="."), 
			paste("DISTRICT_NAME", tmp.last.year, sep="."), paste("DISTRICT_NUMBER", tmp.last.year, sep="."))
		if (!is.null(my.sgp.targets)) variables.to.keep <- c(variables.to.keep, paste(my.sgp.targets, tmp.last.year, sep="."))

		sgPlot.data <- sgPlot.data[, variables.to.keep, with=FALSE]

	#### Merge in 1 year projections (if requested & available) and transform using piecewise.tranform (if required) (NOT NECESSARY IF WIDE data is provided)
	#### Merge in scale scores associated with SGP_TARGETs (if requested & available) and transform using piecewise.transform (if required) (NOT NECESSARY IF WIDE data is provided)

		if (sgPlot.fan | !is.null(sgPlot.sgp.targets)) {

			setnames(sgPlot.data, c(paste("CONTENT_AREA_LABELS", tmp.last.year, sep="."), "CONTENT_AREA"), c("CONTENT_AREA", "CONTENT_AREA_TEMP"))
			tmp.content_areas <- sort(unique(sgPlot.data[["CONTENT_AREA"]]))

			if (sgPlot.baseline) {
				tmp.proj.names <- intersect(names(sgp_object@SGP[["SGProjections"]]), paste(tmp.content_areas, tmp.last.year, "BASELINE", sep="."))
				tmp.proj.cut_score.names <- intersect(names(sgp_object@SGP[["SGProjections"]]), paste(tmp.content_areas, tmp.last.year, "BASELINE", "TARGET_SCALE_SCORES", sep="."))
				tmp.proj.cut_score.names.lagged <- intersect(names(sgp_object@SGP[["SGProjections"]]), paste(tmp.content_areas, tmp.last.year, "LAGGED", "BASELINE", "TARGET_SCALE_SCORES", sep="."))
			} else {
				tmp.proj.names <- intersect(names(sgp_object@SGP[["SGProjections"]]), paste(tmp.content_areas, tmp.last.year, sep="."))
				tmp.proj.cut_score.names <- intersect(names(sgp_object@SGP[["SGProjections"]]), paste(tmp.content_areas, tmp.last.year, "TARGET_SCALE_SCORES", sep="."))
				tmp.proj.cut_score.names.lagged <- intersect(names(sgp_object@SGP[["SGProjections"]]), paste(tmp.content_areas, tmp.last.year, "LAGGED", "TARGET_SCALE_SCORES", sep="."))
			}

			### Straight projections for fan

			if (sgPlot.fan & any(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
				setkeyv(sgPlot.data, c("ID", "CONTENT_AREA"))
				tmp.list <- list()
				for (i in tmp.proj.names) {
					tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep("PROJ", names(sgp_object@SGP[["SGProjections"]][[i]])))])
				}
				sgPlot.data <- data.table(rbind.fill(tmp.list), key=c("ID", "CONTENT_AREA"))[sgPlot.data]
			} ### END if (sgPlot.fan)

			### Straight projection scale score targets

			if (any(c("sgp.projections", "sgp.projections.baseline") %in% sgPlot.sgp.targets) & any(tmp.proj.cut_score.names %in% names(sgp_object@SGP[["SGProjections"]]))) {

				setkeyv(sgPlot.data, c("ID", "CONTENT_AREA"))
				tmp.list <- list()
				for (i in tmp.proj.cut_score.names) {
					tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1], sgp_object@SGP[["SGProjections"]][[i]], key=c("ID", "CONTENT_AREA"))
				}
				sgPlot.data <- data.table(rbind.fill(tmp.list), key=c("ID", "CONTENT_AREA"))[sgPlot.data]
			} ### END if ("sgp.projections" %in% sgPlot.sgp.targets)

			### Lagged projection scale score targets

			if (any(c("sgp.projections.lagged", "sgp.projections.lagged.baseline") %in% sgPlot.sgp.targets) & any(tmp.proj.cut_score.names.lagged %in% names(sgp_object@SGP[["SGProjections"]]))) {

				setkeyv(sgPlot.data, c("ID", "CONTENT_AREA"))
				tmp.list <- list()
				for (i in tmp.proj.cut_score.names.lagged) {
					tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1], sgp_object@SGP[["SGProjections"]][[i]], key=c("ID", "CONTENT_AREA"))
				}
				sgPlot.data <- data.table(rbind.fill(tmp.list), key=c("ID", "CONTENT_AREA"))[sgPlot.data]
			} ### END if ("sgp.projections.lagged" %in% sgPlot.sgp.targets)
		
			### Transform scale scores

			tmp.grade.name <- paste("GRADE", tmp.last.year, sep=".")
			setkeyv(sgPlot.data, c("CONTENT_AREA", tmp.grade.name))

			for (i in seq(8)) {
				if (length(grep(paste("PROJ_YEAR", i, sep="_"), names(sgPlot.data))) > 0) {
					for (proj.iter in grep(paste("PROJ_YEAR", i, sep="_"), names(sgPlot.data), value=TRUE)) {
						if (length(grep("CURRENT", proj.iter)) > 0) tmp.increment <- i else tmp.increment <- i-1
						eval(parse(text=paste("sgPlot.data[, ", proj.iter, ":=piecewise.transform(", proj.iter, ", state, CONTENT_AREA, yearIncrement('", tmp.last.year, "',", tmp.increment, "), get.next.grade(", tmp.grade.name, "[1], CONTENT_AREA[1])), by=list(CONTENT_AREA, ", tmp.grade.name, ")]", sep="")))
					}
				}
			}

			setnames(sgPlot.data, c("CONTENT_AREA", "CONTENT_AREA_TEMP"), c(paste("CONTENT_AREA_LABELS", tmp.last.year, sep="."), "CONTENT_AREA"))
		} ### END if (sgPlot.fan | !is.null(sgPlot.sgp.targets))

	#### Merge in INSTRUCTOR_NAME if requested

		if (sgPlot.reports.by.instructor) {
			setkeyv(student.teacher.lookup, c("ID", paste("INSTRUCTOR_NUMBER", tmp.last.year, sep=".")))
			unique.teacher.lookup <- unique(student.teacher.lookup)[,c("ID", paste(c("INSTRUCTOR_NUMBER", "INSTRUCTOR_NAME"), tmp.last.year, sep=".")), with=FALSE]
			setkey(sgPlot.data, ID); setkey(unique.teacher.lookup)
			sgPlot.data <- sgPlot.data[unique.teacher.lookup, allow.cartesian=TRUE]
			tmp.key <- c("ID", paste("INSTRUCTOR_NUMBER", tmp.last.year, sep="."), "CONTENT_AREA")
			setkeyv(sgPlot.data, tmp.key)
			sgPlot.data <- data.table(student.teacher.lookup[,c(tmp.key, "CONTENT_AREA_RESPONSIBILITY"), with=FALSE], key=tmp.key)[sgPlot.data]
			sgPlot.data[['CONTENT_AREA_RESPONSIBILITY']][is.na( sgPlot.data[['CONTENT_AREA_RESPONSIBILITY']])] <- "Content Area Responsibility: No"
		}

} ## END if else (sgPlot.wide.data)


#### Save WIDE file is requested

if (sgPlot.save.sgPlot.data) {
	setkey(sgPlot.data, ID)
	tmp.file.name <- paste(c(gsub(" ", "_", state.name), "Demonstration")[which(state==c(state.abb, "DEMO"))], "studentGrowthPlot_Data", sep="_")
	assign(tmp.file.name, sgPlot.data)
	save(list=tmp.file.name, file=paste(tmp.file.name, ".Rdata", sep=""))
}


#### studentGrowthPlot production

if (sgPlot.produce.plots) {

	if (parallel.config[['WORKERS']][['SG_PLOTS']]==1 | sgPlot.demo.report) { ### NO Parallel Processing

		studentGrowthPlot_Styles(
			sgPlot.data=sgPlot.data,
			state=state,
			last.year=tmp.last.year,
			content_areas=tmp.content_areas_domains,
			districts=tmp.districts.and.schools[["DISTRICT_NUMBER"]],
			schools=tmp.districts.and.schools[["SCHOOL_NUMBER"]],
			reports.by.student=sgPlot.reports.by.student,
			reports.by.instructor=sgPlot.reports.by.instructor,
			reports.by.school=sgPlot.reports.by.school,
			sgPlot.years=tmp.years.subset,
			sgPlot.folder=sgPlot.folder,
			sgPlot.folder.names=sgPlot.folder.names,
			sgPlot.demo.report=sgPlot.demo.report,
			sgPlot.anonymize=sgPlot.anonymize,
			sgPlot.front.page=sgPlot.front.page,
			sgPlot.header.footer.color=sgPlot.header.footer.color,
			sgPlot.fan=sgPlot.fan,
			sgPlot.sgp.targets=sgPlot.sgp.targets,
			sgPlot.cleanup=sgPlot.cleanup,
			sgPlot.baseline=sgPlot.baseline,
			sgPlot.sgp.targets.timeframe=sgPlot.sgp.targets.timeframe,
			sgPlot.zip=sgPlot.zip,
			sgPlot.output.format=sgPlot.output.format)

	} else { ### Parallel Processing
		
		par.start <- startParallel(parallel.config, 'SG_PLOTS')

		if (par.start$par.type=="FOREACH") {

			foreach.options <- parallel.config[["OPTIONS"]] # works fine if NULL
			foreach(sgPlot.iter=iter(get.sgPlot.iter(tmp.districts.and.schools)), .packages="SGP", .inorder=FALSE,
				.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
						studentGrowthPlot_Styles(
							sgPlot.data=sgPlot.data,
							state=state,
							last.year=tmp.last.year,
							content_areas=tmp.content_areas_domains,
							districts=sgPlot.iter[["DISTRICT_NUMBER"]],
							schools=sgPlot.iter[["SCHOOL_NUMBER"]],
							reports.by.student=sgPlot.reports.by.student,
							reports.by.instructor=sgPlot.reports.by.instructor,
							reports.by.school=sgPlot.reports.by.school,
							sgPlot.years=tmp.years.subset,
							sgPlot.folder=sgPlot.folder,
							sgPlot.folder.names=sgPlot.folder.names,
							sgPlot.demo.report=sgPlot.demo.report,
							sgPlot.anonymize=sgPlot.anonymize,
							sgPlot.front.page=sgPlot.front.page,
							sgPlot.header.footer.color=sgPlot.header.footer.color,
							sgPlot.fan=sgPlot.fan,
							sgPlot.sgp.targets=sgPlot.sgp.targets,
							sgPlot.cleanup=sgPlot.cleanup,
							sgPlot.baseline=sgPlot.baseline,
							sgPlot.sgp.targets.timeframe=sgPlot.sgp.targets.timeframe,
							sgPlot.zip=sgPlot.zip,
							sgPlot.output.format=sgPlot.output.format)
			} ### END dopar
		} ### END if FOREACH
		
		if (par.start$par.type=="SNOW") {
			
			sgPlot.list <- get.sgPlot.iter(tmp.districts.and.schools)
			clusterApplyLB(par.start$internal.cl, sgPlot.list, function(sgPlot.iter) 
				studentGrowthPlot_Styles(
					sgPlot.data=sgPlot.data,
					state=state,
					last.year=tmp.last.year,
					content_areas=tmp.content_areas_domains,
					districts=sgPlot.iter[["DISTRICT_NUMBER"]],
					schools=sgPlot.iter[["SCHOOL_NUMBER"]],
					reports.by.student=sgPlot.reports.by.student,
					reports.by.instructor=sgPlot.reports.by.instructor,
					reports.by.school=sgPlot.reports.by.school,
					sgPlot.years=tmp.years.subset,
					sgPlot.folder=sgPlot.folder,
					sgPlot.folder.names=sgPlot.folder.names,
					sgPlot.demo.report=sgPlot.demo.report,
					sgPlot.anonymize=sgPlot.anonymize,
					sgPlot.front.page=sgPlot.front.page,
					sgPlot.header.footer.color=sgPlot.header.footer.color,
					sgPlot.fan=sgPlot.fan,
					sgPlot.sgp.targets=sgPlot.sgp.targets,
					sgPlot.cleanup=sgPlot.cleanup,
					sgPlot.baseline=sgPlot.baseline,
					sgPlot.sgp.targets.timeframe=sgPlot.sgp.targets.timeframe,
					sgPlot.zip=sgPlot.zip,
					sgPlot.output.format=sgPlot.output.format))
		} ### END if SNOW
		
		if (par.start$par.type=="MULTICORE") {
			
			sgPlot.list <- get.sgPlot.iter(tmp.districts.and.schools)
			mclapply(sgPlot.list, function(sgPlot.iter) 
				studentGrowthPlot_Styles(
					sgPlot.data=sgPlot.data,
					state=state,
					last.year=tmp.last.year,
					content_areas=tmp.content_areas_domains,
					districts=sgPlot.iter[["DISTRICT_NUMBER"]],
					schools=sgPlot.iter[["SCHOOL_NUMBER"]],
					reports.by.student=sgPlot.reports.by.student,
					reports.by.instructor=sgPlot.reports.by.instructor,
					reports.by.school=sgPlot.reports.by.school,
					sgPlot.years=tmp.years.subset,
					sgPlot.folder=sgPlot.folder,
					sgPlot.folder.names=sgPlot.folder.names,
					sgPlot.demo.report=sgPlot.demo.report,
					sgPlot.anonymize=sgPlot.anonymize,
					sgPlot.front.page=sgPlot.front.page,
					sgPlot.header.footer.color=sgPlot.header.footer.color,
					sgPlot.fan=sgPlot.fan,
					sgPlot.sgp.targets=sgPlot.sgp.targets,
					sgPlot.cleanup=sgPlot.cleanup,
					sgPlot.baseline=sgPlot.baseline,
					sgPlot.sgp.targets.timeframe=sgPlot.sgp.targets.timeframe,
					sgPlot.zip=sgPlot.zip,
					sgPlot.output.format=sgPlot.output.format), mc.cores=par.start$workers, mc.preschedule=FALSE)
		}  ### END if MULTICORE
		
		stopParallel(parallel.config, par.start)
		
	} # END else Parallel Processing
} ## END if (sgPlot.produce.plots) 

	message(paste("Finished studentGrowthPlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))

} ## END if ("studentGrowthPlot" %in% plot.types) 

	message(paste("Finished visualizeSGP", date(), "in", timetaken(started.at.visualizeSGP), "\n"))

} ## END visualizeSGP Function
