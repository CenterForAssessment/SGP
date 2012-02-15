`visualizeSGP` <- 
  function(sgp_object,
           plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
           state,
           bPlot.years=NULL,
           bPlot.content_areas=NULL,
           bPlot.districts=NULL,
           bPlot.schools=NULL,
           bPlot.styles=c(1),
           bPlot.levels=NULL,
           bPlot.level.cuts=NULL, 
           bPlot.full.academic.year=TRUE,
           bPlot.minimum.n=10,
           bPlot.anonymize=FALSE,
           bPlot.prior.achievement=TRUE, 
           bPlot.draft=FALSE,
           bPlot.format="print",
           bPlot.folder="Visualizations/bubblePlots",
           sgPlot.save.sgPlot.data=FALSE,
           sgPlot.years=NULL,
           sgPlot.content_areas=NULL,
           sgPlot.districts=NULL,
           sgPlot.schools=NULL,
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
           sgPlot.parallel.config=list(TYPE="FOREACH", OPTIONS=list(preschedule=FALSE, set.seed=FALSE)),
           sgPlot.baseline=NULL,
           gaPlot.years=NULL,
           gaPlot.content_areas=NULL, 
           gaPlot.students=NULL,
           gaPlot.format="print",
           gaPlot.baseline=NULL,
           gaPlot.max.order.for.progression=NULL,
           gaPlot.folder="Visualizations/growthAchievementPlots") {

    started.at <- proc.time()
    message(paste("\nStarted visualizeSGP", date()))

    ### Setting variables to NULL to prevent R CMD check warnings

    DISTRICT_NUMBER <- DISTRICT_NAME <- SCHOOL_NUMBER <- SCHOOL_NAME <- YEAR <- CONTENT_AREA <- NULL ## To prevent R CMD check warnings
    ETHNICITY <- GENDER <- ID <- NULL ## To prevent R CMD check warnings
    TEST_LEVEL <- SUBJECT_CODE <- SCALE_SCORE <- GRADE <- NULL ## To prevent R CMD check warnings
    SCHOOL_ENROLLMENT_STATUS <- LAST_NAME <- FIRST_NAME <- NULL ## To prevent R CMD check warnings
    MEDIAN_SGP <- MEDIAN_SGP_COUNT <- VALID_CASE <- district.school.iter <- NULL ## To prevent R CMD check warnings


   ### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }

    ### Utility functions	

    "%w/o%" <- function(x,y) x[!x %in% y]

    .year.increment <- function(year, increment) {
         paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")
    }

    capwords <- function(x) {
      special.words <- c("ELA", "EMH", "II", "III", "IV")
      if (x %in% special.words) return(x)
      s <- sub("_", " ", x)
      s <- strsplit(s, split=" ")[[1]]
      s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
      s <- strsplit(s, split="-")[[1]]
      s <- paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
      s <- strsplit(s, split="'")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="'")
    }

    get.max.order.for.progression <- function(year, content_area) {
	if (is.null(gaPlot.max.order.for.progression)) {
            if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]])) {
                    return(NULL)
            } else {
                    tmp <- as.numeric(tail(unlist(strsplit(as.character(year), "_")), 1)) - as.numeric(tail(unlist(strsplit(as.character(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]), "_")), 1))
                    if (tmp < 0) return(NULL)
                    if (tmp > 0) return(as.numeric(tmp))
                    if (tmp==0) message(paste("\tNOTE: Based upon state scale changes in ", capwords(year), ". student growth projections are not possible. No student growth projections will be generated.\n", sep=""))
            }
	} else {
	   return(gaPlot.max.order.for.progression)
	}
    }

##############################################################################################################
#### bubblePlot
##############################################################################################################

	if ("bubblePlot" %in% plot.types) {

		started.at <- proc.time()
		message(paste("Started bubblePlot in visualizeSGP", date()))

		bubblePlot_Styles(sgp_object=sgp_object,
			state=state,
			bPlot.years=bPlot.years,
			bPlot.content_areas=bPlot.content_areas,
			bPlot.districts=bPlot.districts,
			bPlot.schools=bPlot.schools,
			bPlot.styles=bPlot.styles,
			bPlot.levels=bPlot.levels,
			bPlot.level.cuts=bPlot.level.cuts,
			bPlot.full.academic.year=bPlot.full.academic.year,
			bPlot.minimum.n=bPlot.minimum.n,
			bPlot.anonymize=bPlot.anonymize,
			bPlot.prior.achievement=bPlot.prior.achievement, 
			bPlot.draft=bPlot.draft,
			bPlot.format=bPlot.format,
			bPlot.folder=bPlot.folder)

		message(paste("Finished bubblePlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
	} ## END bubblePlot %in% plot.types	


####################################################################################################################
#### growthAchievementPlot
####################################################################################################################

	if ("growthAchievementPlot" %in% plot.types) {

		started.at <- proc.time()
		message(paste("Started growthAchievementPlot in visualizeSGP", date()))

		#### Define/Calculate relevant quantities for growthAchievementPlot

		# Year stuff 

		if (is.null(gaPlot.years)) {
			tmp.years <- tail(sort(unique(sgp_object@Data$YEAR)), 1)
		} else {
			tmp.years <- gaPlot.years
		}


		# gaPlot.baseline stuff

                if (is.null(gaPlot.baseline)) {
			gaPlot.baseline <- FALSE ## Default to FALSE if not set by user
                        if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort Referenced") gaPlot.baseline <- FALSE
                        if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Baseline Referenced") gaPlot.baseline <- TRUE
                        if (SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced") gaPlot.baseline <- FALSE
                }


		# Loop over content areas and years

		for (year.iter in tmp.years) {

		if (!is.null(gaPlot.content_areas)) {
			tmp.content_areas <- gaPlot.content_areas
			if (is.factor(sgp_object@Data$CONTENT_AREA)) {
				tmp.content_areas <- as.factor(tmp.content_areas) ## Factor joins to Factor
			}
		} else {
			tmp.content_areas <- sort(unique(sgp_object@Data[YEAR==year.iter]$CONTENT_AREA)) %w/o% NA
		}

		for (content_area.iter in tmp.content_areas) {
      
			if (is.null(gaPlot.students)) {
				tmp.students <- NULL
			} else {
				tmp.students <- gaPlot.students
			}

			if (!year.iter %in% SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area.iter]]) {

				setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA"))

				growthAchievementPlot(
					gaPlot.sgp_object=sgp_object,
					gaPlot.students=tmp.students,
					gaPlot.max.order.for.progression=get.max.order.for.progression(year.iter, content_area.iter),
					state=state,
					content_area=content_area.iter,
					year=year.iter, 
					format=gaPlot.format,
					baseline=gaPlot.baseline,
					pdf.folder=file.path(gaPlot.folder, year.iter))
			} else {
				message(paste("\tNOTE: Based upon state scale changes in ", capwords(year.iter), ". student growth projections are not possible. No ", capwords(year.iter), " ", content_area.iter, " growth and achievement plot will be generated.\n", sep=""))
			}

		} ## END for loop content_area.iter
		} ## END for loop year.iter
    message(paste("Finished growthAchievementPlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
   } ## END if (growthAchievementPlot %in% plot.types)


####################################################################################################################
#### studentGrowthPlot
####################################################################################################################

if ("studentGrowthPlot" %in% plot.types) {

	started.at <- proc.time()
	message(paste("Started studentGrowthPlot in visualizeSGP", date()))

	#### Utility functions

	rbind.all <- function(.list, ...) {
		if (length(.list)==1) return (.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	get.my.cutscore.year <- function(state, content_area, year) {
		tmp.cutscore.years <- sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[grep(content_area, names(SGPstateData[[state]][["Achievement"]][["Cutscores"]]))], "[.]"),
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
		if (content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &
			grade %in% as.numeric(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]]), "_")), ncol=2, byrow=TRUE)[,2])) {
				tmp.loss.hoss <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", grade, sep="")]]
				scale_score[scale_score < tmp.loss.hoss[1]] <- tmp.loss.hoss[1]; scale_score[scale_score > tmp.loss.hoss[2]] <- tmp.loss.hoss[2]
				my.content_area <- get.my.cutscore.year(state, content_area, year)
				tmp.old.cuts <- c(tmp.loss.hoss[1], SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE_", grade, sep="")]], tmp.loss.hoss[2])
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
			tmp.df <- data.frame(GRADE=SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[i]])
			if (!is.null(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]])) {
				tmp.df <- CJ(tmp.df$GRADE, SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[i]]:tmp.last.year)
			} else {
				tmp.df <- CJ(tmp.df$GRADE, tmp.years)
			}
			setnames(tmp.df, c("GRADE", "YEAR")) 
			tmp.list[[i]] <- data.frame(CONTENT_AREA=i, tmp.df)
		}
	data.table(do.call(rbind, tmp.list), key=c("CONTENT_AREA", "GRADE", "YEAR"))
	} ## END get.years.content_areas.grades


######################################################################
##### DISTINGUISH CASES WHEN WIDE data is or is not provided
######################################################################

	#### Some checks

		if (!is.null(sgPlot.students) | !is.null(sgPlot.schools) | !is.null(sgPlot.districts)) sgPlot.demo.report <- FALSE
		if (is.data.frame(sgp_object)) sgPlot.wide.data <- TRUE
		if (is.SGP(sgp_object)) sgPlot.wide.data <- FALSE

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
                } else {
                         my.sgp <- "SGP"
                         my.sgp.level <- "SGP_LEVEL"
                }

################################################
######## IF sgPlot.wide.data is supplied 
################################################

if (sgPlot.wide.data) { ### When WIDE data is provided

	#### Calculate years and content area from data
 
		tmp.all.years <- sort(unique(sapply(strsplit(names(sgp_object), "[.]"), function(x) x[2])))
		tmp.years <- type.convert(tail(tmp.all.years, 5))
		tmp.last.year <- type.convert(tail(tmp.all.years, 1))
		tmp.content_areas <- as.factor(levels(sgp_object[["CONTENT_AREA"]]))

	#### Reconcile School and District selections

		if (is.null(sgPlot.students)) { ## sgPlot.students NOT specified with WIDE data

			## Set key on WIDE data (NOTE: sgp_object is a data.table)

			setkeyv(sgp_object, c("CONTENT_AREA", paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER"), tmp.last.year, sep=".")))

			if (is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object[J(tmp.content_areas)], 
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

			## Get cases

			setkeyv(sgp_object, c("CONTENT_AREA", paste("SCHOOL_NUMBER", tmp.last.year, sep=".")))
			tmp.ids <- unique(sgp_object[CJ(tmp.content_areas, tmp.districts.and.schools[["SCHOOL_NUMBER"]]), nomatch=0][["ID"]])
			setkey(sgp_object, ID)
			sgPlot.data <- sgp_object[J(tmp.ids)]
		} else { ## sgPlot.students specified with WIDE data
			setkey(sgp_object, ID)
			if (is.factor(sgp_object$ID)) sgPlot.students <- as.factor(sgPlot.students)
			sgPlot.data <- sgp_object[J(sgPlot.students)]
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

        #### Some data prep

        if (!is.null(sgPlot.districts)) {
                if (is.factor(sgp_object@Data$DISTRICT_NUMBER)) {
                        sgPlot.districts <- as.factor(sgPlot.districts)
                } else {
                        sgPlot.districts <- as.integer(sgPlot.districts)
                }
        }

        if (!is.null(sgPlot.schools)) {
                if (is.factor(sgp_object@Data$SCHOOL_NUMBER)) {
                        sgPlot.schools <- as.factor(sgPlot.schools)
                } else {
                        sgPlot.schools <- as.integer(sgPlot.schools)
                }
        }

	#### Set key on LONG data

	long.key <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
	setkeyv(sgp_object@Data, long.key)

	#### Year stuff (NECESSARY even IF WIDE data is provided)

	if (is.null(sgPlot.years)) {
		tmp.years <- tail(sort(unique(sgp_object@Data[J("VALID_CASE")][["YEAR"]])), 5)
		tmp.last.year <- tail(tmp.years, 1)
	} else {
		tmp.all.years <- sort(unique(sgp_object@Data[J("VALID_CASE")][["YEAR"]])) 
		tmp.years <- tail(tmp.all.years[1:which(tmp.all.years==tail(sort(sgPlot.years), 1))], 5)
		tmp.last.year <- tail(tmp.years, 1)
	}

	#### Content area stuff (NECESSARY regardless of whether sgPlot.students is provided)

	if (is.null(sgPlot.content_areas)) {
		tmp.content_areas <- sort(unique(sgp_object@Data[J("VALID_CASE", tmp.last.year)][["CONTENT_AREA"]]))
	} else {
		tmp.content_areas <- as.factor(sgPlot.content_areas)
	}

	#### The following apply when sgPlot.students is not supplied 

	if (is.null(sgPlot.students)) {

		#### Demo report student selection (only available with LONG data) 

		if (sgPlot.demo.report) {
			sgPlot.anonymize <- TRUE
			tmp.ids <- list()
			setkeyv(sgp_object@Data, c("VALID_CASE", "YEAR", "GRADE"))
			tmp.grades.reported <- unique(unlist(SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]]))
			tmp.grades.reported <- tmp.grades.reported[tmp.grades.reported %in% unique(sgp_object@Data)["VALID_CASE"][["GRADE"]]]
			for (i in seq_along(tmp.grades.reported)) {
				tmp.ids[[i]] <- as.character(sample(unique(sgp_object@Data[J("VALID_CASE", tmp.last.year, tmp.grades.reported[i])]$ID), 10))
			}
			sgp_object@Data$SCHOOL_NUMBER <- as.integer(sgp_object@Data$SCHOOL_NUMBER)
			sgp_object@Data$SCHOOL_NUMBER[sgp_object@Data$ID %in% unlist(tmp.ids)] <- -99L
			sgp_object@Data$DISTRICT_NUMBER <- as.integer(sgp_object@Data$DISTRICT_NUMBER)
			sgp_object@Data$DISTRICT_NUMBER[sgp_object@Data$ID %in% unlist(tmp.ids)] <- -999L
			tmp.districts.and.schools <- CJ("VALID_CASE", tmp.last.year, tmp.content_areas, -999L, -99L)
			setnames(tmp.districts.and.schools, c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			setkeyv(tmp.districts.and.schools, long.key) 
			setkeyv(sgp_object@Data, long.key)
		} else {

		#### Reconcile School and District selections

			if (is.null(sgPlot.schools) | is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object@Data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas)][,
					list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
					key=key(sgp_object)))
			} 
			if (is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
				tmp.districts.and.schools <- unique(data.table(sgp_object@Data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.districts)][,
					list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
					key=key(sgp_object)))
			}
			if (!is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
				setkeyv(sgp_object@Data, c("VALID_CASE", "YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
				tmp.districts.and.schools <- unique(data.table(sgp_object@Data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.schools)][,
					list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
					key=long.key))
				setkeyv(sgp_object@Data, long.key)
			}
			if (!is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
				tmp.list <- list()
				for (i in seq_along(sgPlot.districts)) {
					tmp.schools <- unique(sgp_object@Data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.districts[i])][["SCHOOL_NUMBER"]])
					if (any(sgPlot.schools==tmp.schools)) {
						tmp.list[[i]] <- tmp.schools[sgPlot.schools==tmp.schools]
					} else {
						tmp.list[[i]] <- tmp.schools
					}
				}
				sgPlot.schools <- unique(c(sgPlot.schools, do.call(c, tmp.list)))
				setkeyv(sgp_object@Data, c("VALID_CASE", "YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
				tmp.districts.and.schools <- unique(data.table(sgp_object@Data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas, sgPlot.schools)][,
						list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER)], 
						key=long.key))
				setkeyv(sgp_object@Data, long.key)
			} 
		}
	} ## END if (is.null(sgPlot.students))

	#### Subset data (NOT NECESSARY IF WIDE data is provided)

		if (is.null(sgPlot.students)) {
			report.ids <- unique(sgp_object@Data[tmp.districts.and.schools][["ID"]])
			setkeyv(sgp_object@Data, c("CONTENT_AREA", "GRADE", "YEAR"))
			tmp.table <- data.table(sgp_object@Data[get.years.content_areas.grades(state)], 
				key=c("ID", "CONTENT_AREA", "YEAR", "VALID_CASE"))[CJ(report.ids, tmp.content_areas, tmp.years, "VALID_CASE")]
		} else {
			report.ids <- sgPlot.students
			setkeyv(sgp_object@Data, c("CONTENT_AREA", "GRADE", "YEAR"))
			tmp.table <- data.table(sgp_object@Data[get.years.content_areas.grades(state)], 
				key=c("VALID_CASE", "ID", "CONTENT_AREA", "YEAR"))[CJ("VALID_CASE", report.ids, tmp.content_areas, tmp.years)]
			setkeyv(tmp.table, c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
			tmp.districts.and.schools <- tmp.table[CJ("VALID_CASE", tmp.last.year, tmp.content_areas)][,list(DISTRICT_NUMBER, SCHOOL_NUMBER)]
		}

	#### Trim tmp.districts.and.schools

		tmp.districts.and.schools <- unique(data.table(tmp.districts.and.schools, 
			key=c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))[,
			list(DISTRICT_NUMBER, SCHOOL_NUMBER)])
		tmp.districts.and.schools <- subset(tmp.districts.and.schools, !is.na(DISTRICT_NUMBER) & !is.na(SCHOOL_NUMBER))
	
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
			piecewise.transform(SCALE_SCORE, state, as.character(CONTENT_AREA), as.character(YEAR), as.character(GRADE)), by=list(CONTENT_AREA, YEAR, GRADE)]$V1

	#### Anonymize (if requested) (NOT necessary if wide data is provided)
     
		if (sgPlot.anonymize) {
			require(randomNames)
			if (!"ETHNICITY" %in% names(tmp.table)) tmp.table[["ETHNICITY"]] <- 1
			if (!"GENDER" %in% names(tmp.table)) tmp.table[["GENDER"]] <- round(runif(dim(tmp.table)[1], min=0, max=1))
			tmp.dt <- tmp.table[,list(ID, ETHNICITY, GENDER)]
			setkey(tmp.dt, ID)
			tmp.dt <- tmp.dt[!duplicated(tmp.dt),]

			tmp.dt$LAST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="last")
			tmp.dt$FIRST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="first")

			names.dt <- tmp.dt[,list(ID, LAST_NAME, FIRST_NAME)]
			setkey(names.dt, ID)

			setkey(tmp.table, ID)
			tmp.table <- names.dt[tmp.table]
			if (sgPlot.demo.report) {
				tmp.table$DISTRICT_NAME <- as.factor("Sample District")
				tmp.table$SCHOOL_NAME <- as.factor("Sample School")
			} else {
				setkey(tmp.table, DISTRICT_NUMBER)
				tmp.district.number <- J(DISTRICT_NUMBER=unique(tmp.table$DISTRICT_NUMBER) %w/o% NA, seq_along(unique(tmp.table$DISTRICT_NUMBER) %w/o% NA), 
					key="DISTRICT_NUMBER")[tmp.table]$V2
				tmp.table$DISTRICT_NAME <- as.character(tmp.table$DISTRICT_NAME)
				tmp.table$DISTRICT_NAME[!is.na(tmp.table$DISTRICT_NUMBER)] <- paste("Sample District", tmp.district.number[!is.na(tmp.table$DISTRICT_NUMBER)])
				tmp.table$DISTRICT_NAME <- as.factor(tmp.table$DISTRICT_NAME)
	
				setkey(tmp.table, SCHOOL_NUMBER)
				tmp.school.number <- J(SCHOOL_NUMBER=unique(tmp.table$SCHOOL_NUMBER) %w/o% NA, seq_along(unique(tmp.table$SCHOOL_NUMBER) %w/o% NA), 
					key="SCHOOL_NUMBER")[tmp.table]$V2
				tmp.table$SCHOOL_NAME <- as.character(tmp.table$SCHOOL_NAME)
				tmp.table$SCHOOL_NAME[!is.na(tmp.table$SCHOOL_NUMBER)] <- paste("Sample School", tmp.school.number[!is.na(tmp.table$SCHOOL_NUMBER)])
				tmp.table$SCHOOL_NAME <- as.factor(tmp.table$SCHOOL_NAME)
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
			sgPlot.data <- data.table(rbind.all(tmp.list), key=paste(key(sgPlot.data), collapse=","))[sgPlot.data]
			tmp.grade.name <- paste("GRADE", tmp.last.year, sep=".")
			tmp.year.name <- .year.increment(tmp.last.year, 1)
			setkeyv(sgPlot.data, c("CONTENT_AREA", tmp.grade.name))
			for (proj.iter in grep("PROJ_YEAR_1", names(sgPlot.data))) {
				tmp.scale_score.name <- names(sgPlot.data)[proj.iter]
				sgPlot.data[[proj.iter]] <- sgPlot.data[,piecewise.transform(get(tmp.scale_score.name), state, as.character(CONTENT_AREA), tmp.year.name, get(tmp.grade.name)[1]+1), 
					by=list(CONTENT_AREA, sgPlot.data[[tmp.grade.name]])]$V1 
			}
		}
} ## END if else (sgPlot.wide.data)


#### Save WIDE file if requested

if (sgPlot.save.sgPlot.data) {
	setkey(sgPlot.data, ID)
	tmp.file.name <- paste(c(gsub(" ", "_", state.name), "Demonstration")[which(state==c(state.abb, "DEMO"))], "ISR_Data", sep="_")
	assign(tmp.file.name, sgPlot.data)
	save(list=tmp.file.name, file=paste(tmp.file.name, ".Rdata", sep=""))
}

#### studentGrowthPlot production

if (sgPlot.produce.plots) {

	if (is.null(sgPlot.parallel.config)) { ### NO Parallel Processing

                        studentGrowthPlot_Styles(
                                sgPlot.data=sgPlot.data,
                                state=state,
                                last.year=tmp.last.year,
                                content_areas=tmp.content_areas,
                                districts=tmp.districts.and.schools[["DISTRICT_NUMBER"]],
                                schools=tmp.districts.and.schools[["SCHOOL_NUMBER"]],
                                reports.by.student=sgPlot.reports.by.student,
                                sgPlot.years=tmp.years,
                                sgPlot.folder=sgPlot.folder,
                                sgPlot.folder.names=sgPlot.folder.names,
                                sgPlot.demo.report=sgPlot.demo.report,
                                sgPlot.anonymize=sgPlot.anonymize,
                                sgPlot.front.page=sgPlot.front.page,
                                sgPlot.header.footer.color=sgPlot.header.footer.color,
                                sgPlot.fan=sgPlot.fan,
                                sgPlot.cleanup=sgPlot.cleanup,
                                sgPlot.baseline=sgPlot.baseline)
	} else { ### Parallel Processing

		tmp.iter <- list()
		for (k in 1:dim(tmp.districts.and.schools)[1]) {
			tmp.iter[[k]] <- list(DISTRICT_NUMBER=tmp.districts.and.schools[["DISTRICT_NUMBER"]][k], SCHOOL_NUMBER=tmp.districts.and.schools[["SCHOOL_NUMBER"]][k])
		}

		### FOREACH flavor

		if (toupper(sgPlot.parallel.config[["TYPE"]]) == "FOREACH") {

			if (!is.null(getOption("cores"))) {
				require(doMC)
				registerDoMC()
			}

			foreach.options <- sgPlot.parallel.config[["OPTIONS"]] # works fine if NULL
			foreach(district.school.iter=iter(tmp.iter), .packages="SGP", .inorder=FALSE,
				.options.multicore=foreach.options, .options.mpi=foreach.options, .options.redis=foreach.options, .options.smp=foreach.options, .verbose=TRUE) %dopar% {
						invisible(studentGrowthPlot_Styles(
							sgPlot.data=sgPlot.data,
							state=state,
							last.year=tmp.last.year,
							content_areas=tmp.content_areas,
							districts=district.school.iter[["DISTRICT_NUMBER"]],
							schools=district.school.iter[["SCHOOL_NUMBER"]],
							reports.by.student=sgPlot.reports.by.student,
							sgPlot.years=tmp.years,
							sgPlot.folder=sgPlot.folder,
							sgPlot.folder.names=sgPlot.folder.names,
							sgPlot.demo.report=sgPlot.demo.report,
							sgPlot.anonymize=sgPlot.anonymize,
							sgPlot.front.page=sgPlot.front.page,
							sgPlot.header.footer.color=sgPlot.header.footer.color,
							sgPlot.fan=sgPlot.fan,
							sgPlot.cleanup=sgPlot.cleanup,
							sgPlot.baseline=sgPlot.baseline))
			} ### END dopar
		} ### END if FOREACH
	} 
} ## END if (sgPlot.produce.plots) 

	message(paste("Finished studentGrowthPlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))

} ## END if ("studentGrowthPlot" %in% plot.types) 

	message(paste("Finished visualizeSGP", date(), "in", timetaken(started.at), "\n"))

} ## END visualizeSGP Function
