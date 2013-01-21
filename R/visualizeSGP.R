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
		sgPlot.anonymize=FALSE,
		sgPlot.cleanup=TRUE,
		sgPlot.demo.report=FALSE,
		sgPlot.produce.plots=TRUE,
		sgPlot.baseline=NULL,
		sgPlot.zip=TRUE,
		sgPlot.output.format="PDF",
		sgPlot.show.targets.years.forward=NULL,
		gaPlot.years=NULL,
		gaPlot.content_areas=NULL, 
		gaPlot.students=NULL,
		gaPlot.format="print",
		gaPlot.baseline=NULL,
		gaPlot.max.order.for.progression=NULL,
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
	CONTENT_AREA_RESPONSIBILITY <- NULL


	### Create state (if missing) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "visualizeSGP")
        }


	### Set up parallel.config if NULL

	if (is.null(parallel.config)) {
		parallel.config = list(BACKEND="PARALLEL", WORKERS=list(GA_PLOTS=1, SG_PLOTS=1, SG_PLOTS_TARGETS=1))
	}

	### Utility functions	

	"%w/o%" <- function(x,y) x[!x %in% y]
	num_non_missing <- function(x) sum(!is.na(x))

	.year.increment <- function(year, increment) {
		paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")
	}

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


	get.gaPlot.iter <- function(gaPlot.years, gaPlot.content_areas, gaPlot.students) {

		tmp.list <- tmp.gaPlot.list <- list()

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
					setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
			}
		}

		tmp.df <- rbind.fill(tmp.list)

		if (!is.null(gaPlot.students)) {
			 tmp.df <- expand.grid(tmp.df, ID=gaPlot.students)
		}

		for (i in seq(dim(tmp.df)[1])) {
			tmp.gaPlot.list[[i]] <- list(YEAR=tmp.df[["YEAR"]][i], CONTENT_AREA=tmp.df[["CONTENT_AREA"]][i], ID=tmp.df[["ID"]][i])
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
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced") gaPlot.baseline <- FALSE
		}

		par.start <- startParallel(parallel.config, 'GA_PLOTS')

		gaPlot.sgp_object <- get.gaPlot.object(sgp_object)
		
		if (par.start$par.type=="FOREACH") {

			foreach(gaPlot.iter=iter(get.gaPlot.iter(gaPlot.years, gaPlot.content_areas, gaPlot.students)), .packages="SGP", .inorder=FALSE,
				.options.multicore=par.start$foreach.options, .options.mpi=par.start$foreach.options, .options.redis=par.start$foreach.options) %dopar% {
					growthAchievementPlot(
						gaPlot.sgp_object=gaPlot.sgp_object,
						gaPlot.students=gaPlot.iter[["ID"]],
						gaPlot.max.order.for.progression=get.max.order.for.progression(gaPlot.iter[["YEAR"]], gaPlot.iter[["CONTENT_AREA"]]),
						state=state,
						content_area=gaPlot.iter[["CONTENT_AREA"]],
						year=gaPlot.iter[["YEAR"]], 
						format=gaPlot.format,
						baseline=gaPlot.baseline,
						output.format=c("PDF", "PNG"),
						output.folder=file.path(gaPlot.folder, gaPlot.iter[["YEAR"]]))

			} ## END dopar 
		} ## END FOREACH
		
		if (par.start$par.type=="SNOW") {
			
			gaPlot.list <- get.gaPlot.iter(gaPlot.years, gaPlot.content_areas, gaPlot.students)
			clusterApplyLB(par.start$internal.cl, gaPlot.list, function(gaPlot.iter) 
				growthAchievementPlot(
						gaPlot.sgp_object=gaPlot.sgp_object,
						gaPlot.students=gaPlot.iter[["ID"]],
						gaPlot.max.order.for.progression=get.max.order.for.progression(gaPlot.iter[["YEAR"]], gaPlot.iter[["CONTENT_AREA"]]),
						state=state,
						content_area=gaPlot.iter[["CONTENT_AREA"]],
						year=gaPlot.iter[["YEAR"]], 
						format=gaPlot.format,
						baseline=gaPlot.baseline,
						output.format=c("PDF", "PNG"),
						output.folder=file.path(gaPlot.folder, gaPlot.iter[["YEAR"]])))
		}
		
		if (par.start$par.type=="MULTICORE") {
			gaPlot.list <- get.gaPlot.iter(gaPlot.years, gaPlot.content_areas, gaPlot.students)
			mclapply(gaPlot.list, function(gaPlot.iter) {
						growthAchievementPlot(
						gaPlot.sgp_object=gaPlot.sgp_object,
						gaPlot.students=gaPlot.iter[["ID"]],
						gaPlot.max.order.for.progression=get.max.order.for.progression(gaPlot.iter[["YEAR"]], gaPlot.iter[["CONTENT_AREA"]]),
						state=state,
						content_area=gaPlot.iter[["CONTENT_AREA"]],
						year=gaPlot.iter[["YEAR"]], 
						format=gaPlot.format,
						baseline=gaPlot.baseline,
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
		if (content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &
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

	get.years.content_areas.grades <- function(state) {
		tmp.list <- list()
		for (i in names(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]])) {
			tmp.df <- data.frame(GRADE=as.character(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[i]]), stringsAsFactors=FALSE)
			if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]])) {
				tmp.df <- CJ(tmp.df$GRADE, SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]]:tmp.last.year)
			} else {
				tmp.df <- CJ(tmp.df$GRADE, tmp.years)
			}
			setnames(tmp.df, c("GRADE", "YEAR")) 
			tmp.list[[i]] <- data.table(CONTENT_AREA=i, tmp.df)
		}
	tmp.dt <- data.table(rbind.fill(tmp.list))
	setkeyv(tmp.dt, c("CONTENT_AREA", "GRADE", "YEAR"))
	return(tmp.dt[!is.na(CONTENT_AREA)])
	} ## END get.years.content_areas.grades


######################################################################
##### DISTINGUISH CASES WHEN WIDE data is or is not provided
######################################################################

	#### Some checks

		if (!is.null(sgPlot.students) | !is.null(sgPlot.instructors) | !is.null(sgPlot.schools) | !is.null(sgPlot.districts)) sgPlot.demo.report <- FALSE
		if (!is.null(sgPlot.students)) sgPlot.reports.by.student <- TRUE
		if (!is.null(sgPlot.instructors)) sgPlot.reports.by.instructor <- TRUE
		if (!is.null(sgPlot.schools)) sgPlot.reports.by.school <- TRUE
		if (!is.null(sgPlot.districts)) sgPlot.reports.by.school <- TRUE
		if (is.data.frame(sgp_object)) sgPlot.wide.data <- TRUE
		if (is.SGP(sgp_object)) {
			sgPlot.wide.data <- FALSE
			slot.data <- copy(sgp_object@Data)
		}

		if (sgPlot.demo.report & sgPlot.wide.data) {
			message("\tNOTE: Demonstration report is not supported using wide data. Process will proceed with demonstration report production using long data.\n")
			sgPlot.demo.report <- FALSE
		}

	#### To Baseline or not to Baseline

		if (is.null(sgPlot.baseline)) {
			sgPlot.baseline <- FALSE ## Default to cohort referenced is not set by user
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort Referenced") sgPlot.baseline <- FALSE
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Baseline Referenced") sgPlot.baseline <- TRUE
			if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced") sgPlot.baseline <- FALSE
		}

		if (sgPlot.baseline) {
			my.sgp <- "SGP_BASELINE"
			my.sgp.level <- "SGP_LEVEL_BASELINE"
			if (!is.null(sgPlot.show.targets.years.forward)) my.target.types <- c("sgp.projections.baseline", "sgp.projections.lagged.baseline")
		} else {
			my.sgp <- "SGP"
			my.sgp.level <- "SGP_LEVEL"
			if (!is.null(sgPlot.show.targets.years.forward)) my.target.types <- c("sgp.projections", "sgp.projections.lagged")
		}

	#### Which targets

		if (length(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")) <= 1) {
			my.target.levels <- "CATCH_UP_KEEP_UP"
		} else {
			my.target.levels <- c("CATCH_UP_KEEP_UP", "MOVE_UP_STAY_UP")
		}


################################################
######## IF sgPlot.wide.data is supplied 
################################################

if (sgPlot.wide.data) { ### When WIDE data is provided

	#### Calculate years and content area from data
 
		tmp.all.years <- sort(unique(sapply(strsplit(names(sgp_object), "[.]"), function(x) x[2])))
		tmp.years <- tail(tmp.all.years, 5)
		tmp.last.year <- tail(tmp.all.years, 1)
		tmp.content_areas <- unique(sgp_object[["CONTENT_AREA"]])

	#### Reconcile School and District selections

		if (is.null(sgPlot.students)) { ## sgPlot.students NOT specified with WIDE data

			## Calculate School size to order from largest to smallest and set key on WIDE data

			setkeyv(sgp_object, paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep="."))
			tmp.districts.and.schools.size <- sgp_object[,num_non_missing(ID), by=key(sgp_object)] 
			setnames(tmp.districts.and.schools.size, 1:2, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))

			setkeyv(sgp_object, c("CONTENT_AREA", paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))

			if (is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object[tmp.content_areas], 
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			}

			if (is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object[CJ(tmp.content_areas, sgPlot.districts)],
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			}

			if (!is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
				setkeyv(sgp_object, c("CONTENT_AREA", paste(c("SCHOOL_NUMBER", "DISTRICT_NUMBER"), tmp.last.year, sep=".")))
				tmp.districts.and.schools <- unique(data.table(sgp_object[CJ(tmp.content_areas, sgPlot.schools)],
					key=paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))[,
					c(paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")), with=FALSE]
			}

			if (!is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
				tmp.list <- list()
				for (i in seq_along(sgPlot.districts)) {
					tmp.schools <- unique(sgp_object[CJ(tmp.content_areas, sgPlot.districts[i])][[paste("SCHOOL_NUMBER", tmp.last.year, sep=".")]])
					if (any(sgPlot.schools==tmp.schools)) {
						tmp.list[[i]] <- tmp.schools[sgPlot.schools==tmp.schools]
					} else {
						tmp.list[[i]] <- tmp.schools
					}
				}
				sgPlot.schools <- unique(c(sgPlot.schools, do.call(c, tmp.list)))
				setkeyv(sgp_object, c("CONTENT_AREA", paste(c("SCHOOL_NUMBER", "DISTRICT_NUMBER"), tmp.last.year, sep=".")))
				tmp.districts.and.schools <- unique(data.table(sgp_object[CJ(tmp.content_areas, sgPlot.schools)],
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
			tmp.ids <- unique(sgp_object[CJ(tmp.content_areas, tmp.districts.and.schools[["SCHOOL_NUMBER"]]), nomatch=0][["ID"]])
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

	#### Set key on LONG data

	long.key <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
	setkeyv(slot.data, long.key)

	#### Year stuff (NECESSARY even IF WIDE data is provided)

	if (is.null(sgPlot.years)) {
		tmp.years <- tail(sort(unique(slot.data["VALID_CASE"][["YEAR"]])), 5)
		tmp.last.year <- tail(tmp.years, 1)
	} else {
		tmp.all.years <- sort(unique(slot.data["VALID_CASE"][["YEAR"]])) 
		tmp.years <- tail(tmp.all.years[1:which(tmp.all.years==tail(sort(sgPlot.years), 1))], 5)
		tmp.last.year <- tail(tmp.years, 1)
	}

	#### Content area stuff (NECESSARY regardless of whether sgPlot.students is provided)

	if (is.null(sgPlot.content_areas)) {
		tmp.content_areas <- sort(intersect(
						unique(slot.data[SJ("VALID_CASE", tmp.last.year), nomatch=0][["CONTENT_AREA"]]),
						names(SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Labels"]])))
	} else {
		tmp.content_areas <- sgPlot.content_areas
	}


	#### Melt and rename data if reporting by instructor number

	if (sgPlot.reports.by.instructor) {
		instructor.variable.names <- grep("INSTRUCTOR_NUMBER", names(slot.data), value=TRUE)
		district.and.school.variable.names <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
		student.teacher.lookup <- data.table(melt(as.data.frame(slot.data[VALID_CASE=="VALID_CASE" & YEAR==tmp.last.year, 
			c(district.and.school.variable.names, "ID", instructor.variable.names), with=FALSE]),
                                        measure.vars=grep("INSTRUCTOR_NUMBER", names(slot.data), value=TRUE),
                                        value.name="INSTRUCTOR_NUMBER"))
		invisible(student.teacher.lookup[,variable:=NULL])
		if (length(grep("INSTRUCTOR_FIRST_NAME", names(slot.data))) > 0) {
			invisible(student.teacher.lookup[, INSTRUCTOR_NAME:=paste(
				melt(as.data.frame(slot.data[VALID_CASE=="VALID_CASE" & YEAR==tmp.last.year, grep("INSTRUCTOR_LAST_NAME", names(slot.data), value=TRUE), with=FALSE]),
				measure.vars=grep("INSTRUCTOR_LAST_NAME", names(slot.data), value=TRUE))[,2],
				melt(as.data.frame(slot.data[VALID_CASE=="VALID_CASE" & YEAR==tmp.last.year, grep("INSTRUCTOR_FIRST_NAME", names(slot.data), value=TRUE), with=FALSE]),
				measure.vars=grep("INSTRUCTOR_FIRST_NAME", names(slot.data), value=TRUE))[,2], sep=", ")])
			invisible(student.teacher.lookup[INSTRUCTOR_NAME=="NA, NA", INSTRUCTOR_NAME := as.character(NA)])
		} else {
			invisible(student.teacher.lookup[!is.na(INSTRUCTOR_NUMBER), INSTRUCTOR_NAME := paste("Instructor", INSTRUCTOR_NUMBER)])
		}
		if (!is.null(sgPlot.instructors)) {
			student.teacher.lookup <- subset(student.teacher.lookup, INSTRUCTOR_NUMBER %in% sgPlot.instructors)
		} else {
			student.teacher.lookup <- subset(student.teacher.lookup, !is.na(INSTRUCTOR_NUMBER))
		}
		tmp.district.and.schools.instructors <- unique(data.table(student.teacher.lookup, key=long.key)[,district.and.school.variable.names, with=FALSE])
		student.teacher.lookup <- student.teacher.lookup[,list(ID, CONTENT_AREA, INSTRUCTOR_NUMBER, INSTRUCTOR_NAME)]
		invisible(student.teacher.lookup[,CONTENT_AREA_RESPONSIBILITY:=factor(1, levels=0:1, labels=c("Content Area Responsibility: No", "Content Area Responsibility: Yes"))])
		setnames(student.teacher.lookup, c("INSTRUCTOR_NUMBER", "INSTRUCTOR_NAME"), paste(c("INSTRUCTOR_NUMBER", "INSTRUCTOR_NAME"), tmp.last.year, sep="."))
	}




	#### The following apply when sgPlot.students is not supplied 

	if (is.null(sgPlot.students)) {

		#### Demo report student selection (only available with LONG data) 

		if (sgPlot.demo.report) {
			sgPlot.anonymize <- TRUE
			tmp.ids <- list()
			setkeyv(slot.data, c("VALID_CASE", "YEAR", "GRADE"))
			tmp.grades.reported <- unique(unlist(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]]))
			tmp.grades.reported <- as.character(tmp.grades.reported[tmp.grades.reported %in% unique(slot.data)["VALID_CASE"][["GRADE"]]])
			for (i in seq_along(tmp.grades.reported)) {
				tmp.ids[[i]] <- as.character(sample(unique(slot.data[SJ("VALID_CASE", tmp.last.year, tmp.grades.reported[i])]$ID), 10))
			}
			slot.data[,c("SCHOOL_NUMBER", "DISTRICT_NUMBER") := NULL]
			slot.data[slot.data$ID %in% unlist(tmp.ids), c("SCHOOL_NUMBER", "DISTRICT_NUMBER") := list(-99L, -999L)]
			tmp.districts.and.schools <- CJ("VALID_CASE", tmp.last.year, tmp.content_areas, -999L, -99L)
			setnames(tmp.districts.and.schools, c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			setkeyv(tmp.districts.and.schools, long.key) 
			setkeyv(slot.data, long.key)
		} else {

		#### Reconcile School and District selections

			if (sgPlot.reports.by.instructor) {
				tmp.districts.and.schools <- tmp.district.and.schools.instructors
			} else {
				setkeyv(slot.data, long.key)

				if (is.null(sgPlot.schools) | is.null(sgPlot.districts)) {
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
				} 
				if (is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.districts)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
				}
				if (!is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
					setkeyv(slot.data, c("VALID_CASE", "YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.schools)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
					setkeyv(slot.data, long.key)
				}
				if (!is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
					tmp.list <- list()
					for (i in seq_along(sgPlot.districts)) {
						tmp.schools <- unique(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.districts[i])][["SCHOOL_NUMBER"]])
						if (any(sgPlot.schools==tmp.schools)) {
							tmp.list[[i]] <- tmp.schools[sgPlot.schools==tmp.schools]
						} else {
							tmp.list[[i]] <- tmp.schools
						}
					}
					sgPlot.schools <- unique(c(sgPlot.schools, do.call(c, tmp.list)))
					setkeyv(slot.data, c("VALID_CASE", "YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
					tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.schools)][,
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
			tmp.table <- data.table(slot.data[get.years.content_areas.grades(state)], 
				key=c("ID", "CONTENT_AREA", "YEAR", "VALID_CASE"))[CJ(report.ids, tmp.content_areas, tmp.years, "VALID_CASE")]
		} else {
			report.ids <- sgPlot.students
			setkeyv(slot.data, c("CONTENT_AREA", "GRADE", "YEAR"))
			tmp.table <- data.table(slot.data[get.years.content_areas.grades(state)], 
				key=c("VALID_CASE", "ID", "CONTENT_AREA", "YEAR"))[CJ("VALID_CASE", report.ids, tmp.content_areas, tmp.years)]
			setkeyv(tmp.table, c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			tmp.districts.and.schools <- tmp.table[CJ("VALID_CASE", tmp.last.year, tmp.content_areas)][, list(DISTRICT_NUMBER, SCHOOL_NUMBER)]
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

		setkeyv(tmp.table, c("CONTENT_AREA", "YEAR", "GRADE"))
		tmp.table$TRANSFORMED_SCALE_SCORE <- tmp.table[,
			piecewise.transform(SCALE_SCORE, state, CONTENT_AREA, as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA, YEAR, GRADE)]$V1

	#### Anonymize (if requested) (NOT necessary if wide data is provided)
 
		if (sgPlot.anonymize) {
			suppressPackageStartupMessages(require(randomNames))
			if (!"ETHNICITY" %in% names(tmp.table)) tmp.table[["ETHNICITY"]] <- 1
			if (!"GENDER" %in% names(tmp.table)) tmp.table[["GENDER"]] <- round(runif(dim(tmp.table)[1], min=0, max=1))
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

	#### Create FIRST_NAME and LAST_NAME if they don't exist

		if (!"FIRST_NAME" %in% names(tmp.table)) tmp.table$FIRST_NAME <- ""
		if (!"LAST_NAME" %in% names(tmp.table)) tmp.table$LAST_NAME <- ""

	#### Reshape data (NOT NECESSARY IF WIDE data is provided)

		variables.to.keep <- c("VALID_CASE", "ID", "LAST_NAME", "FIRST_NAME", "CONTENT_AREA", "YEAR", "GRADE", 
			"SCALE_SCORE", "TRANSFORMED_SCALE_SCORE", "ACHIEVEMENT_LEVEL", my.sgp, my.sgp.level, "SCHOOL_NAME", "SCHOOL_NUMBER", "DISTRICT_NAME", "DISTRICT_NUMBER")

		sgPlot.data <- reshape(tmp.table[,variables.to.keep, with=FALSE],
			idvar=c("ID", "CONTENT_AREA"),
			timevar="YEAR",
			drop=c("VALID_CASE"),
			direction="wide")

		variables.to.keep <- c("ID", "CONTENT_AREA", 
			paste("LAST_NAME", tmp.last.year, sep="."), paste("FIRST_NAME", tmp.last.year, sep="."), paste("GRADE", tmp.years, sep="."), 
			paste(my.sgp, tmp.years, sep="."), paste("SCALE_SCORE", tmp.years, sep="."), paste("TRANSFORMED_SCALE_SCORE", tmp.years, sep="."), 
			paste("ACHIEVEMENT_LEVEL", tmp.years, sep="."), paste(my.sgp.level, tmp.years, sep="."),
			paste("SCHOOL_NAME", tmp.last.year, sep="."), paste("SCHOOL_NUMBER", tmp.last.year, sep="."), 
			paste("DISTRICT_NAME", tmp.last.year, sep="."), paste("DISTRICT_NUMBER", tmp.last.year, sep="."))

		sgPlot.data <- sgPlot.data[, variables.to.keep, with=FALSE]

	#### Merge in 1 year projections (if requested & available) and transform using piecewise.tranform (if required) (NOT NECESSARY IF WIDE data is provided)

		if (sgPlot.baseline) {
			tmp.proj.names <- paste(tmp.content_areas, tmp.last.year, "BASELINE", sep=".")
		} else {
			tmp.proj.names <- paste(tmp.content_areas, tmp.last.year, sep=".")
		}
		if (sgPlot.fan & all(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
			setkeyv(sgPlot.data, c("ID", "CONTENT_AREA"))
			tmp.list <- list()
			for (i in tmp.proj.names) {
				tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep("PROJ_YEAR_1", names(sgp_object@SGP[["SGProjections"]][[i]])))])
			}
			sgPlot.data <- data.table(rbindlist(tmp.list), key=key(sgPlot.data))[sgPlot.data]
			tmp.grade.name <- paste("GRADE", tmp.last.year, sep=".")
			tmp.year.name <- .year.increment(tmp.last.year, 1)
			setkeyv(sgPlot.data, c("CONTENT_AREA", tmp.grade.name))
			for (proj.iter in grep("PROJ_YEAR_1", names(sgPlot.data))) {
				tmp.scale_score.name <- names(sgPlot.data)[proj.iter]
				sgPlot.data[[proj.iter]] <- sgPlot.data[,piecewise.transform(get(tmp.scale_score.name), state, CONTENT_AREA, tmp.year.name, get.next.grade(get(tmp.grade.name)[1], CONTENT_AREA[1])), 
					by=list(CONTENT_AREA, sgPlot.data[[tmp.grade.name]])][['V1']] 
			}
		} ### END if (sgPlot.baseline)





##############################################################################################################
	#### BEGIN CONSTRUCTION ZONE: Calculate and Merge in lagged targets if requested
##############################################################################################################

		if (!is.null(sgPlot.show.targets.years.forward)) {

			setkey(sgPlot.data, ID, CONTENT_AREA)

			for (target.type in my.target.types) {
				for (target.level in my.target.levels) {
					sgPlot.data <- data.table(getTargetSGP(sgp_object, tmp.content_areas, state, tmp.last.year, 
						target.type, target.level, sgPlot.show.targets.years.forward, unique(sgPlot.data[['ID']]), FALSE), key=c("ID", "CONTENT_AREA"))[sgPlot.data]
					invisible(sgPlot.data[,VALID_CASE := NULL]); invisible(sgPlot.data[,YEAR := NULL])
				}
			} 


browser()

			for (target.type in my.target.types) {
				for (target.level in my.target.levels) {
					my.sgp.target <- getTargetName(target.type, target.level, sgPlot.show.targets.years.forward)
					getTargetScaleScore(
						sgp_object, 
						state, 
						sgPlot.data[, c("ID", my.sgp.target), with=FALSE], 
						target.type, 
						target.level, 
						subset(get.years.content_areas.grades(state), YEAR==tmp.last.year),
						parallel.config=parallel.config)

				}
			}

		} ### if (!is.null(sgPlot.show.targets.years.forward))


##############################################################################################################
	#### END CONSTRUCTION ZONE: Calculate and Merge in lagged targets if requested
##############################################################################################################




	### Merge in INSTRUCTOR_NAME if requested

		if (sgPlot.reports.by.instructor) {
			setkeyv(student.teacher.lookup, c("ID", paste("INSTRUCTOR_NUMBER", tmp.last.year, sep=".")))
			unique.teacher.lookup <- unique(student.teacher.lookup)[,c("ID", paste(c("INSTRUCTOR_NUMBER", "INSTRUCTOR_NAME"), tmp.last.year, sep=".")), with=FALSE]
			setkey(sgPlot.data, ID); setkey(unique.teacher.lookup)
			sgPlot.data <- sgPlot.data[unique.teacher.lookup]
			tmp.key <- c("ID", paste("INSTRUCTOR_NUMBER", tmp.last.year, sep="."), "CONTENT_AREA")
			setkeyv(sgPlot.data, tmp.key)
			sgPlot.data <- data.table(student.teacher.lookup[,c(tmp.key, "CONTENT_AREA_RESPONSIBILITY"), with=FALSE], key=tmp.key)[sgPlot.data]
			sgPlot.data[['CONTENT_AREA_RESPONSIBILITY']][is.na( sgPlot.data[['CONTENT_AREA_RESPONSIBILITY']])] <- "Content Area Responsibility: No"
		}

		### Rekey @Data

		setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

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

	if (is.null(parallel.config) | sgPlot.demo.report) { ### NO Parallel Processing

		studentGrowthPlot_Styles(
			sgPlot.data=sgPlot.data,
			state=state,
			last.year=tmp.last.year,
			content_areas=tmp.content_areas,
			districts=tmp.districts.and.schools[["DISTRICT_NUMBER"]],
			schools=tmp.districts.and.schools[["SCHOOL_NUMBER"]],
			reports.by.student=sgPlot.reports.by.student,
			reports.by.instructor=sgPlot.reports.by.instructor,
			reports.by.school=sgPlot.reports.by.school,
			sgPlot.years=tmp.years,
			sgPlot.folder=sgPlot.folder,
			sgPlot.folder.names=sgPlot.folder.names,
			sgPlot.demo.report=sgPlot.demo.report,
			sgPlot.anonymize=sgPlot.anonymize,
			sgPlot.front.page=sgPlot.front.page,
			sgPlot.header.footer.color=sgPlot.header.footer.color,
			sgPlot.fan=sgPlot.fan,
			sgPlot.cleanup=sgPlot.cleanup,
			sgPlot.baseline=sgPlot.baseline,
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
							content_areas=tmp.content_areas,
							districts=sgPlot.iter[["DISTRICT_NUMBER"]],
							schools=sgPlot.iter[["SCHOOL_NUMBER"]],
							reports.by.student=sgPlot.reports.by.student,
							reports.by.instructor=sgPlot.reports.by.instructor,
							reports.by.school=sgPlot.reports.by.school,
							sgPlot.years=tmp.years,
							sgPlot.folder=sgPlot.folder,
							sgPlot.folder.names=sgPlot.folder.names,
							sgPlot.demo.report=sgPlot.demo.report,
							sgPlot.anonymize=sgPlot.anonymize,
							sgPlot.front.page=sgPlot.front.page,
							sgPlot.header.footer.color=sgPlot.header.footer.color,
							sgPlot.fan=sgPlot.fan,
							sgPlot.cleanup=sgPlot.cleanup,
							sgPlot.baseline=sgPlot.baseline,
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
					content_areas=tmp.content_areas,
					districts=sgPlot.iter[["DISTRICT_NUMBER"]],
					schools=sgPlot.iter[["SCHOOL_NUMBER"]],
					reports.by.student=sgPlot.reports.by.student,
					reports.by.instructor=sgPlot.reports.by.instructor,
					reports.by.school=sgPlot.reports.by.school,
					sgPlot.years=tmp.years,
					sgPlot.folder=sgPlot.folder,
					sgPlot.folder.names=sgPlot.folder.names,
					sgPlot.demo.report=sgPlot.demo.report,
					sgPlot.anonymize=sgPlot.anonymize,
					sgPlot.front.page=sgPlot.front.page,
					sgPlot.header.footer.color=sgPlot.header.footer.color,
					sgPlot.fan=sgPlot.fan,
					sgPlot.cleanup=sgPlot.cleanup,
					sgPlot.baseline=sgPlot.baseline,
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
					content_areas=tmp.content_areas,
					districts=sgPlot.iter[["DISTRICT_NUMBER"]],
					schools=sgPlot.iter[["SCHOOL_NUMBER"]],
					reports.by.student=sgPlot.reports.by.student,
					reports.by.instructor=sgPlot.reports.by.instructor,
					reports.by.school=sgPlot.reports.by.school,
					sgPlot.years=tmp.years,
					sgPlot.folder=sgPlot.folder,
					sgPlot.folder.names=sgPlot.folder.names,
					sgPlot.demo.report=sgPlot.demo.report,
					sgPlot.anonymize=sgPlot.anonymize,
					sgPlot.front.page=sgPlot.front.page,
					sgPlot.header.footer.color=sgPlot.header.footer.color,
					sgPlot.fan=sgPlot.fan,
					sgPlot.cleanup=sgPlot.cleanup,
					sgPlot.baseline=sgPlot.baseline,
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
