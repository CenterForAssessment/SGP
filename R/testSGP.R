`testSGP` <-
function(
	TEST_NUMBER,
	save.results=TRUE,
	test.option=list(),
	memory.profile=FALSE) {

	YEAR <- GRADE <- NULL

	if (missing(TEST_NUMBER)) {
		messageSGP("\ttestSGP carries out testing of SGP package. Tests currently included in testSGP:\n")
		messageSGP("\t\t1. abcSGP test using all available years.")
		messageSGP("\t\t2. abcSGP test using all available years except most recent followed by an updated analysis using the most recent year's data.")
	}

	sgpData.years <- sort(unique(data.table(SGPdata::sgpData_LONG[['YEAR']]), by='V1')[['V1']])

		#######################################################################################################################################################
		###
		### TEST NUMBER 0: Test of studentGrowthPercentiles, studentGrowthProjections, and sgpData
		##
		#######################################################################################################################################################

		if ("0" %in% toupper(TEST_NUMBER)) {

			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			sgpData.years.single <- sapply(strsplit(sgpData.years, "_"), '[', 2)
			Demonstration_SGP <- NULL
			tmp.messages <- "##### Begin testSGP test number 0 #####\n\n"

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				if (.Platform$OS.type != "unix") {
					parallel.config <- paste0("list(BACKEND='FOREACH', TYPE='doParallel', WORKERS=list(TAUS=", number.cores, "))")
				} else  parallel.config <- paste0("list(BACKEND='PARALLEL', WORKERS=list(TAUS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]


			### Part 1

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- list(Panel_Data=SGPdata::sgpData)\nmy.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)\nfor (i in seq_along(my.grade.sequences)) {\n\tDemonstration_SGP <- studentGrowthPercentiles(\n\t\tpanel.data=Demonstration_SGP,\n\t\tsgp.labels=list(my.year=", tail(sgpData.years.single, 1), ", my.subject='Reading'),\n\t\tgrowth.levels='DEMO',\n\t\tgoodness.of.fit='DEMO',\n\t\tgoodness.of.fit.output.format=c('PDF', 'PNG', 'SVG'),\n\t\tgrade.progression=my.grade.sequences[[i]],\n\t\tpercentile.cuts=c(1,35,50,65,99),\n\t\tprint.sgp.order=TRUE,\n\t\tcalculate.confidence.intervals='DEMO',\n\t\tprint.other.gp=TRUE,\n\t\tverbose.output=TRUE,\n\t\tmax.order.for.percentile=3,\n\t\treturn.additional.max.order.sgp=2,\n\t\treturn.norm.group.scale.scores=TRUE,\n\t\treturn.panel.data=TRUE,\n\t\tparallel.config=", parallel.config,")\n}")

			cat(paste0("EVALUATING Test Number 0, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(0)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP Variable

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 0: Part 1 #####\n")

			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1), sep=".")]][['SGP']]), "218a961b96d1f6588c408f5698ac04ce")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
			}

			### TEST of SGP_ORDER_1 Variable

			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1), sep=".")]][['SGP_ORDER_1']]), "543b9397789643ae0619256c05812a17")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER_1, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER_1, part 1: FAIL\n")
			}

			### TEST of SCALE_SCORE_PRIOR Variable

			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1), sep=".")]][['SCALE_SCORE_PRIOR']]), "87b318d12cad2d571cbb59297b6b0a3d")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR, part 1: FAIL\n")
			}

			### TEST of SGP_ORDER Variable

			if (identical(digest(table(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1), sep=".")]][['SGP_ORDER']])), "b2779fb3a7408ade8eea0c69cb9f265b")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER, part 1: FAIL\n")
			}

			### TEST of SGP_LEVEL Variable

			if (identical(digest(table(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1), sep=".")]][['SGP_LEVEL']])), "22d6dafb62d5d6b2927c7a08ba1d8461")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_LEVEL, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_LEVEL, part 1: FAIL\n")
			}

			### TEST of SGP_STANDARD_ERROR Variable

			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1), sep=".")]][['SGP_STANDARD_ERROR']]), "157ff9ac26da921092d7c115768537ac")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_STANDARD_ERROR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_STANDARD_ERROR, part 1: FAIL\n")
			}

			### TEST of SGP_FROM_**** variable

			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1), sep=".")]][[paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1))-2, sep="_")]]), "454ff2c53966e1cfd4c9280aab4b40b5")) {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ",  paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1))-2, sep="_"), ", part 2: OK\n"))
			} else {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ", paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1))-2, sep="_"), ", part 2: FAIL\n"))
			}

			### TEST of Goodness of Fit Output Files

			gof.files <-  c("gofSGP_Grade_4.pdf", "gofSGP_Grade_4.png", "gofSGP_Grade_4.svg",
							"gofSGP_Grade_5.pdf", "gofSGP_Grade_5.png", "gofSGP_Grade_5.svg",
							"gofSGP_Grade_6.pdf", "gofSGP_Grade_6.png", "gofSGP_Grade_6.svg",
							"gofSGP_Grade_7.pdf", "gofSGP_Grade_7.png", "gofSGP_Grade_7.svg",
							"gofSGP_Grade_8.pdf", "gofSGP_Grade_8.png", "gofSGP_Grade_8.svg")
			if (identical(sort(list.files(file.path("Goodness_of_Fit", paste("READING", tail(sgpData.years.single, 1), sep=".")))), gof.files)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of Goodness of Fit Output Files, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of Goodness of Fit Output Files, part 1: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 0, Part 1: ", convertTime(timetaken(started.at.overall)), "#####\n")


			### Part 2

			expression.to.evaluate <-
				paste0("Demonstration_SGP$Panel_Data <- SGPdata::sgpData[,c('ID',", paste('\'GRADE', head(sgpData.years.single, -1), sep='_', collapse='\', '), "', ", paste('\'SS', head(sgpData.years.single, -1), sep='_', collapse='\', '), "')]\nmy.grade.progressions <- list(3, 3:4, 3:5, 3:6, 4:7)\nfor (i in seq_along(my.grade.progressions)) {\n\tDemonstration_SGP <- studentGrowthProjections(\n\t\tpanel.data=Demonstration_SGP,\n\t\tsgp.labels=list(my.year=", tail(sgpData.years.single, 1), ", my.subject='Reading', my.extra.label='LAGGED'),\n\t\tuse.my.coefficient.matrices=list(my.year=", tail(sgpData.years.single, 1), ", my.subject='Reading'),\n\t\tprojcuts.digits=0,\n\t\tperformance.level.cutscores='DEMO',\n\t\tpercentile.trajectory.values=1:99,\n\t\tlag.increment=1,\n\t\tgrade.progression=my.grade.progressions[[i]],\n\t\treturn.projection.group.identifier='READING',\n\t\treturn.projection.group.scale.scores=TRUE)\n}")

			cat(paste0("EVALUATING Test Number 0, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(0)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")
			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of dimension of table READING.####.LAGGED dimensions

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 0: Part 2 #####\n")

			if (identical(digest(Demonstration_SGP[['SGProjections']][[paste('READING', tail(sgpData.years.single, 1), 'LAGGED', sep=".")]]), "8ed0dbdfe95ea48e426a1e1faf9e8da1")) {
				tmp.messages <- c(tmp.messages, paste("\t\tTest of",  paste('READING', tail(sgpData.years.single, 1), 'LAGGED', sep="."), "table dimensions, part 2: OK\n"))
			} else {
				tmp.messages <- c(tmp.messages, paste("\t\tTest of",  paste('READING', tail(sgpData.years.single, 1), 'LAGGED', sep="."), "table dimensions, part 2: FAIL\n"))
			}

			### TEST of LEVEL_1_SGP_TARGET_YEAR_1 Variable

			if (identical(digest(Demonstration_SGP[['SGProjections']][[paste('READING', tail(sgpData.years.single, 1), "LAGGED", sep=".")]][['LEVEL_1_SGP_TARGET_YEAR_1']]), "60193d09485ede52ddcf58a51dedf04f")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1, part 2: FAIL\n")
			}

			### TEST of P84_PROJ_YEAR_4 Variable

			if (identical(digest(Demonstration_SGP[['SGProjections']][[paste('READING', tail(sgpData.years.single, 1), "LAGGED", sep=".")]][['P84_PROJ_YEAR_4']]), "e691fdcb7f80f9d716685300a9e16b48")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P84_PROJ_YEAR_4, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P84_PROJ_YEAR_4, part 2: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 0, Part 2: ", convertTime(timetaken(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 0: ", convertTime(timetaken(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 0


		#######################################################################################################################################################
		###
		### TEST NUMBER 1: Test of abcSGP on sgpData_LONG
		###
		#######################################################################################################################################################

		if (any(c("1", "1B") %in% toupper(TEST_NUMBER))) {
			if (all(c("1", "1B") %in% toupper(TEST_NUMBER))) TEST_NUMBER <- "1B"

			options(error=recover)
			options(warn=2)
			Demonstration_SGP <- NULL
			tmp.messages <- ("##### Results of testSGP test number 1 #####\n\n")
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				if (.Platform$OS.type != "unix") {
					parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
				} else  parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
			} else parallel.config <- test.option[['parallel.config']]

			### Some minor modifications to SGPstateData for testing purposes

			SGPstateData[["DEMO"]][["SGP_Configuration"]][["print.other.gp"]] <- TRUE
			SGPstateData[["DEMO"]][["SGP_Configuration"]][["calculate.confidence.intervals"]] <- list(confidence.quantiles=c(0.025, 0.975))

			if (toupper(TEST_NUMBER) == "1B") sgp.sqlite <- TRUE else sgp.sqlite <- FALSE

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tdata_supplementary=list(INSTRUCTOR_NUMBER=SGPdata::sgpData_INSTRUCTOR_NUMBER),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.sqlite=", sgp.sqlite, ",\n\tget.cohort.data.info=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat(paste("##### Begin testSGP test number", TEST_NUMBER, "#####\n"), fill=TRUE)
			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) Rprof("testSGP(1)_Memory_Profile.out", memory.profiling=TRUE)

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			if (memory.profile) {
				Rprof(NULL)
			}

			### TEST of SGP variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP']], na.rm=TRUE), 8565260L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP']]), "d7843eab4ddf02bed640ec401dd35c65")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
			}

			### TEST of SGP_BASELINE variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP_BASELINE']], na.rm=TRUE), 8573825L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_BASELINE']]), "f6ffda530e470321159c39a56151e0c2")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: FAIL\n")
			}

			### TEST of SGP_TARGET_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP_TARGET_3_YEAR']], na.rm=TRUE), 7796624L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_TARGET_3_YEAR']]), "7cfb4055b5ebf739417d2220898ce99c")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR']], na.rm=TRUE), 9201802L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR']]), "9bfb90fbaed288104dfe1959294497ce")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data[['CATCH_UP_KEEP_UP_STATUS']])), c(41099, 10837, 35560, 84390))) {
			if (identical(digest(Demonstration_SGP@Data[['CATCH_UP_KEEP_UP_STATUS']]), "11e5e53c8cf330fcdb51a2e4894f5c2f")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: FAIL\n")
			}

			### TEST of MOVE_UP_STAY_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data[['MOVE_UP_STAY_UP_STATUS']])), c(72953, 15043, 18336, 13618))) {
			if (identical(digest(Demonstration_SGP@Data[['MOVE_UP_STAY_UP_STATUS']]), "663ff40f494a37d01dc3c593b887d24b")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: FAIL\n")
			}

			### TEST of SCALE_SCORE_PRIOR variable

#			if (identical(sum(Demonstration_SGP@Data[['SCALE_SCORE_PRIOR']], na.rm=TRUE), 100865095)) {
			if (identical(digest(Demonstration_SGP@Data[['SCALE_SCORE_PRIOR']]), "23e0433ed1574c73f923b23cddbfc0cb")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: FAIL\n")
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable for READING.XXXX_XXXX scale score targets

#			if (identical(as.integer(sum(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']])), 18313900L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']]), "d67a2c0b75f411683abe1a9c023c8c0e")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
			}

			### TEST of MEDIAN_SGP variable

#			if (identical(sum(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['MEDIAN_SGP']], na.rm=TRUE), 9140.5)) {
			if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['MEDIAN_SGP']]), "0d8c6f5489ff405a492a7ef193884a56")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: FAIL\n")
			}

			### TEST of PERCENT_AT_ABOVE_PROFICIENT_PRIOR variable

#			if (identical(sum(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['PERCENT_AT_ABOVE_PROFICIENT_PRIOR']], na.rm=TRUE), 12894.2)) {
			if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['PERCENT_AT_ABOVE_PROFICIENT_PRIOR']]), "f1e683f2c118135b28d285a754d1f188")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: FAIL\n")
			}

			### TEST of LAGGED PROJECTION CUTs variable P35_PROJ_YEAR_1 variable for MATHEMATICS.XXXX_XXXX.LAGGED

#			if (identical(sum(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED', sep=".")]][['P20_PROJ_YEAR_1']]), 15567825)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED', sep=".")]][['P20_PROJ_YEAR_1']]), "e83219e1830264576b42861aaaed067f")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable P20_PROJ_YEAR_1 (LAGGED PROJECTION): OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable P20_PROJ_YEAR_1 (LAGGED PROJECTION): FAIL\n")
			}

			### TEST of SGP_0.025_CONFIDENCE_BOUND variable in @Data

#			if (identical(sum(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']], na.rm=TRUE), 3952340)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']]), "dc13c362312f379fbbfa83c49f1e53eb")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_0.025_CONFIDENCE_BOUND: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_0.025_CONFIDENCE_BOUND: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste0("\n##### End testSGP test number ", TEST_NUMBER, ":  ", convertTime(timetaken(started.at.overall)), " #####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 1 & 1B


		##########################################################################################################################################################################
		###
		### TEST NUMBER 2: Various tests of updateSGP functionality.
		###
		### TEST NUMBER 2a: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2010-2011 to 2012-2013 followed by adding with_sgp_data_LONG 2014-2015 using
		###			overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=FALSE.
		### TEST NUMBER 2b: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2010-2011 to 2013-2014 followed by adding with_sgp_data_LONG 2014-2015 using
		###			overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=FALSE.
		### TEST NUMBER 2c: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2010-2011 to 2013-2014 followed by adding with_sgp_data_LONG 2014-2015 using
		###			overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=TRUE.
		### TEST NUMBER 2d: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2010-2011 to 2013-2014 followed by adding with_sgp_data_LONG 2014-2015 using
		###			overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=TRUE.
		###
		#########################################################################################################################################################################

		if (toupper(TEST_NUMBER) %in% c("2", "2A", "2B", "2C", "2D")) {
		if (identical(TEST_NUMBER, 2)) TEST_NUMBER <- c("2A", "2B", "2C")

		################################
		### TEST NUMBER 2a
		################################

		if ('2A' %in% toupper(TEST_NUMBER)) {

			options(error=recover)
			options(warn=2)
			Demonstration_SGP <- NULL
			Demonstration_Data_LONG <- subset(SGPdata::sgpData_LONG, YEAR %in% head(sgpData.years, -1))
			Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1))
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			tmp.messages <- "##### Begin testSGP test number 2a #####\n\n"

			### Part 1

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(2a)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall.2a <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP variable

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 2a: Part 1 #####\n")

#			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "7ad371d09d281ad5604b9288897ab337")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
			}

			### TEST of SGP_BASELINE variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 5667488L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "a2d8cfffa3565c2a2103eba276a3effe")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
			}

			### TEST of SGP_TARGET_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 5245437L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "d14088916dc65391d44cab3995a4f7b6")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: FAIL\n")
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 6088129L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "bfd8ae0c9497c8a4d13f09999eac7d33")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: FAIL\n")
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(27122, 6990, 24358, 55283))) {
			if (identical(digest(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS), "46373a48b1a926c107b66fc0222337b3")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 1: FAIL\n")
			}

			### TEST of MOVE_UP_STAY_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(48152, 10396, 12150, 8943))) {
			if (identical(digest(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS), "8a33c63b43e4b35e0f86f61c55583264")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 1: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste0("\t##### End testSGP test number 2a: Part 1: ", convertTime(timetaken(started.at.overall.2a)), " #####\n"))


			### Part 2

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_LAST_YEAR,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(2a)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate.2a <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 2a: Part 2 #####\n")

			### TEST of SGP variable

#			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "d7843eab4ddf02bed640ec401dd35c65")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
			}

			### TEST of SGP_BASELINE variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "f6ffda530e470321159c39a56151e0c2")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
			}

			### TEST of SGP_TARGET variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "7cfb4055b5ebf739417d2220898ce99c")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 9201802L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "9bfb90fbaed288104dfe1959294497ce")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
			if (identical(digest(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS), "11e5e53c8cf330fcdb51a2e4894f5c2f")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL\n")
			}

			### TEST of MOVE_UP_STAY_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(72953, 15043, 18336, 13618))) {
			if (identical(digest(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS), "663ff40f494a37d01dc3c593b887d24b")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 2a, Part 2: ", convertTime(timetaken(started.at.intermediate.2a)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 2a: ", convertTime(timetaken(started.at.overall.2a)), "#####\n"))
		} ### End TEST_NUMBER 2a


		################################
		### TEST NUMBER 2b, 2c, 2d
		################################
		if (any(toupper(TEST_NUMBER) %in% c('2B', '2C', '2D'))) {

			for (i in setdiff(toupper(TEST_NUMBER), '2A')) {
				options(error=recover)
				options(warn=2)
				Demonstration_SGP <- ID <- CONTENT_AREA <- NULL
				Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1))
				if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)

				############################################################
				### Part 1: Required for all Tests 2b, 2c, and 2d
				############################################################

				tmp.messages <- c(tmp.messages, paste("\n##### Begin testSGP test number", capwords(i), "#####\n\n"))
				tmp.messages <- c(tmp.messages, paste0("\t##### Begin testSGP test number ", capwords(i), ": Part 1 #####\n\n"))

				expression.to.evaluate <-
					paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='", tail(sgpData.years, 1), "',\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, "))\n)\n")

				cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

				if (memory.profile) {
					Rprof("testSGP(2)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
				}

				started.at.overall.2b <- proc.time()
				eval(parse(text=expression.to.evaluate))

				tmp.messages <- c(tmp.messages, paste0("\t##### End testSGP test number ", capwords(i), ": Part 1: ", convertTime(timetaken(started.at.overall.2b)), " #####\n"))

				#############################################################
				### Part 2
				#############################################################

				tmp.messages <- c(tmp.messages, paste0("\n\t##### Begin testSGP test number ", capwords(i), ": Part 2 #####\n\n"))

				### TEST 2b ###
				if (i=='2B') {
					Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1))

					expression.to.evaluate <-
						paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_LAST_YEAR,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

					cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

					started.at.intermediate.2b <- proc.time()
					eval(parse(text=expression.to.evaluate))
				} ### End TEST 2b

				### TEST 2c ###
				if (i=='2C') {
					Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1))

					expression.to.evaluate <-
						paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_LAST_YEAR,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

					cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

					started.at.intermediate.2b <- proc.time()
					eval(parse(text=expression.to.evaluate))
				} ### End TEST 2c

				### TEST 2d ###
				if (i=='2D') {
					Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1))
					tmp.LAST_YEAR.ids <- sort(unique(Demonstration_Data_LONG_LAST_YEAR[['ID']]))
					tmp.group.1 <- tmp.LAST_YEAR.ids[1:150]
					tmp.group.2 <- tmp.LAST_YEAR.ids[151:250]
					with_sgp_data_LONG <- subset(Demonstration_Data_LONG_LAST_YEAR, ID %in% tmp.group.1 | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS"))
					Demonstration_SGP@Data <- subset(Demonstration_SGP@Data, !((ID %in% tmp.group.1 & YEAR==tail(sgpData.years, 1)) | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS" & YEAR==tail(sgpData.years, 1))))
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('MATHEMATICS', tail(sgpData.years, 1), sep=".")]] <- subset(Demonstration_SGP@SGP$SGPercentiles[[paste('MATHEMATICS', tail(sgpData.years, 1), sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGPercentiles']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'BASELINE', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'BASELINE', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED.BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED.BASELINE', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]] <- subset(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), 'BASELINE', sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), 'BASELINE', sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), 'LAGGED', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), 'LAGGED', sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), 'LAGGED.BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), 'LAGGED.BASELINE', sep=".")]], !ID %in% tmp.group.1)

					expression.to.evaluate <-
						paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=with_sgp_data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

					cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

					started.at.intermediate.2b <- proc.time()
					eval(parse(text=expression.to.evaluate))
				} ### End TEST 2d

				### TEST of SGP variable

				tmp.messages <- c(tmp.messages, paste("\t\t##### Results of testSGP test number", capwords(i), "#####\n"))

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP, na.rm=TRUE), 2896606L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP), "0cc817e2864bc3e3bda3da9c8c8c8ab9")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
				}

				### TEST of SGP_BASELINE variable

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP_BASELINE, na.rm=TRUE), 2906337L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP_BASELINE), "6727904bc44c191e21be0dfb95a6187b")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
				}

				### TEST of SGP_TARGET_3_YEAR variable

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP_TARGET_3_YEAR), "cfc79dbc3b55744c3a4cfa1834d2eeaf")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
				}

				### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "eef9ed11b6eed237364e6916dae06b91")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
				}

				### TEST of CATCH_UP_KEEP_UP_STATUS variable

#				if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$CATCH_UP_KEEP_UP_STATUS)), c(13977, 3847, 11202, 29107))) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$CATCH_UP_KEEP_UP_STATUS), "cb981073080b1b8b6688338e06e1e074")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL\n")
				}

				### TEST of MOVE_UP_STAY_UP_STATUS variable

#				if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$MOVE_UP_STAY_UP_STATUS)), c(24801, 4647, 6186, 4675))) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1)]$MOVE_UP_STAY_UP_STATUS), "8dbe7770bd436331b81bd0a84dbaa1bb")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: FAIL\n")
				}

				tmp.messages <- c(tmp.messages, paste0("\t##### End testSGP test number ", capwords(i), ", Part 2: ", convertTime(timetaken(started.at.intermediate.2b)), " #####\n"))
				tmp.messages <- c(tmp.messages, paste0("\n##### End testSGP test number ", capwords(i), ": ", convertTime(timetaken(started.at.overall.2b)), " #####\n"))
			} ### End for (i in TEST_NUMBER)
			} ### End TEST_NUMBER 2b, 2c, 2d
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 2

		#######################################################################################################################################################
		###
		### TEST NUMBER 3: Test of EOCT like student growth projections
		###
		#######################################################################################################################################################

		if (3 %in% TEST_NUMBER) {

			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			Demonstration_SGP <- NULL
			tmp.messages <- ("\t##### Results of testSGP test number 3 #####\n\n")
			sgpData_LONG <- SGPdata::sgpData_LONG

			### Add EOCT courses to sgpData_LONG

			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '9'] <- 'ALGEBRA_I'
			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '10'] <- 'ALGEBRA_II'
			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '9'] <- 'GRADE_9_LIT'
			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '10'] <- 'AMERICAN_LIT'
			sgpData_LONG$GRADE_REPORTED <- sgpData_LONG$GRADE
			sgpData_LONG$GRADE[sgpData_LONG$CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II', 'GRADE_9_LIT', 'AMERICAN_LIT')] <-  'EOCT'

			### Modify SGPstateData

			SGPstateData[["DEMO"]][["Student_Report_Information"]] <- list(
				# Transformed_Achievement_Level_Cutscores=list(MATHEMATICS=sgpData.years, READING=sgpData.years, GRADE_9_LIT=sgpData.years, AMERICAN_LIT=sgpData.years, ALGEBRA_I=sgpData.years, ALGEBRA_II=sgpData.years), ### FOR TESTING
				# Transformed_Achievement_Level_Cutscores_gaPlot=list(MATHEMATICS=sgpData.years, READING=sgpData.years, GRADE_9_LIT=sgpData.years, AMERICAN_LIT=sgpData.years, ALGEBRA_I=sgpData.years, ALGEBRA_II=sgpData.years), ### FOR TESTING
				Vertical_Scale=list(MATHEMATICS=TRUE, READING=TRUE, GRADE_9_LIT=TRUE, AMERICAN_LIT=TRUE, ALGEBRA_I=TRUE, ALGEBRA_II=TRUE),
				Content_Areas_Labels=list(MATHEMATICS="Mathematics", READING="Reading", GRADE_9_LIT="Grade 9 Literature", AMERICAN_LIT="American Literature", ALGEBRA_I="Algebra I", ALGEBRA_II="Algebra II"),
				Content_Areas_Domains=list(MATHEMATICS="MATHEMATICS", READING="READING", GRADE_9_LIT="READING", AMERICAN_LIT="READING", ALGEBRA_I="MATHEMATICS", ALGEBRA_II="MATHEMATICS"),
				Grades_Reported=list(MATHEMATICS=c("3","4","5","6","7","8"), READING=c("3","4","5","6","7","8"), GRADE_9_LIT="EOCT", AMERICAN_LIT="EOCT", ALGEBRA_I="EOCT", ALGEBRA_II="EOCT"),
				Grades_Reported_Domains=list(MATHEMATICS=c("3","4","5","6","7","8","EOCT"), READING=c("3","4","5","6","7","8","EOCT")),
				Achievement_Level_Labels=list(
					"Unsatisfactory"="Unsatisfactory",
					"Part Proficient"="Partially Proficient",
					"Proficient"="Proficient",
					"Advanced"="Advanced"))

			SGPstateData[["DEMO"]][["SGP_Configuration"]][["grade.projection.sequence"]] <- list(
				READING=c("3", "4", "5", "6", "7", "8", "EOCT", "EOCT"),
				MATHEMATICS=c("3", "4", "5", "6", "7", "8", "EOCT", "EOCT"),
				GRADE_9_LIT=c("3", "4", "5", "6", "7", "8", "EOCT", "EOCT"),
				AMERICAN_LIT=c("3", "4", "5", "6", "7", "8", "EOCT", "EOCT"),
				ALGEBRA_I=c("3", "4", "5", "6", "7", "8", "EOCT", "EOCT"),
				ALGEBRA_II=c("3", "4", "5", "6", "7", "8", "EOCT", "EOCT"))
			SGPstateData[["DEMO"]][["SGP_Configuration"]][["content_area.projection.sequence"]] <- list(
				READING=c("READING", "READING", "READING", "READING", "READING", "READING", "GRADE_9_LIT", "AMERICAN_LIT"),
				GRADE_9_LIT=c("READING", "READING", "READING", "READING", "READING", "READING", "GRADE_9_LIT", "AMERICAN_LIT"),
				AMERICAN_LIT=c("READING", "READING", "READING", "READING", "READING", "READING", "GRADE_9_LIT", "AMERICAN_LIT"),
				MATHEMATICS=c("MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II"),
				ALGEBRA_I=c("MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II"),
				ALGEBRA_II=c("MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II"))
			SGPstateData[["DEMO"]][["SGP_Configuration"]][["year_lags.projection.sequence"]] <- list(
				READING=rep(1L, 7),
				MATHEMATICS=rep(1L, 7),
				GRADE_9_LIT=rep(1L, 7),
				AMERICAN_LIT=rep(1L, 7),
				ALGEBRA_I=rep(1L, 7),
				ALGEBRA_II=rep(1L, 7))
			SGPstateData[["DEMO"]][["SGP_Configuration"]][["max.forward.projection.sequence"]] <- list(
				READING=3,
				MATHEMATICS=3,
				GRADE_9_LIT=3,
				AMERICAN_LIT=3,
				ALGEBRA_I=3,
				ALGEBRA_II=3)

			SGPstateData[["DEMO"]][['SGP_Configuration']][['sgPlot.show.content_area.progression']] <- TRUE

			### Create configurations

			READING_LAST_YEAR.config <- list(
				READING.LAST_YEAR=list(
					sgp.content.areas=c('READING', 'READING', 'READING', 'READING', 'READING'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
			)

			GRADE_9_LIT_LAST_YEAR.config <- list(
				GRADE_9_LIT.LAST_YEAR=list(
					sgp.content.areas=c('READING', 'READING', 'READING', 'READING', 'GRADE_9_LIT'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(c(5:8, 'EOCT')))
			)

			AMERICAN_LIT_LAST_YEAR.config <- list(
				AMERICAN_LIT.LAST_YEAR=list(
					sgp.content.areas=c('READING', 'READING', 'READING', 'GRADE_9_LIT', 'AMERICAN_LIT'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
			)

			MATHEMATICS_LAST_YEAR.config <- list(
				MATHEMATICS.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
			)

			ALGEBRA_I_LAST_YEAR.config <- list(
				ALGEBRA_I.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(c(5:8, 'EOCT')))
			)

			ALGEBRA_II_LAST_YEAR.config <- list(
				ALGEBRA_II.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
			)

			sgp.config <- c(READING_LAST_YEAR.config, MATHEMATICS_LAST_YEAR.config, GRADE_9_LIT_LAST_YEAR.config, AMERICAN_LIT_LAST_YEAR.config, ALGEBRA_I_LAST_YEAR.config, ALGEBRA_II_LAST_YEAR.config)

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'summarizeSGP', 'visualizeSGP'),\n\tsimulate.sgps=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat("##### Begin testSGP test number 3 #####\n", fill=TRUE)

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(3)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			if (memory.profile) {
				Rprof(NULL)
			}

			### TEST of SGP variable

#			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 2896606L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "7c848c0bec09ae0c833ce778034ab85e")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
			}

			### TEST of SGP_TARGET_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "4c73b3d3237d181b51954529edaa3c4e")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "2eaaa1d6e0884ea2b28fa4245f8c63d1")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(13977, 3847, 11202, 29107))) {
			if (identical(digest(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS), "fd61668ae95e5978906bf804b5765b10")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: FAIL\n")
			}

			### TEST of MOVE_UP_STAY_UP_STATUS variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(24801, 4647, 6186, 4675))) {
			if (identical(digest(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS), "ee8ef7f0b8370d7eeee1677cb101af8d")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 3: ", convertTime(timetaken(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 3


		#######################################################################################################################################################
		###
		### TEST NUMBER 4: Test of SIMEX Measurement Error Correction Functionality
		###
		#######################################################################################################################################################

		if (4 %in% TEST_NUMBER) {

			sgpData_LONG <- SGPdata::sgpData_LONG

			if (is.null(test.option[['calculate.simex.baseline']])) calculate.simex.baseline <- FALSE else calculate.simex.baseline <- TRUE
			if (is.null(test.option[['rq.method']])) SGPstateData[["DEMO"]][["SGP_Configuration"]][["rq.method"]] <- test.option[['rq.method']]
			simex.parameters <- "list(state='DEMO', lambda=seq(0,2,0.5), simulation.iterations=50, extrapolation='linear', save.matrices=TRUE)"

			###  The test of SIMEX baseline functionality requires the DEMO SIMEX matrices to be loaded manually.
			### SGPstateData[["DEMO"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- c(SGPstateData[["DEMO"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]], DEMO_SIMEX_Baseline_Matrices)
			if (calculate.simex.baseline) {
				if (!any(grepl(".BASELINE.SIMEX", names(SGPstateData[["DEMO"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]])))) {
					stop("\n\tThis test requires the DEMO SIMEX matrices to be loaded manually:\n\t\tSGPstateData[['DEMO']][['Baseline_splineMatrix']][['Coefficient_Matrices']] <- c(SGPstateData[['DEMO']][['Baseline_splineMatrix']][['Coefficient_Matrices']], DEMO_SIMEX_Baseline_Matrices)\n\n")
				}
			}

			SGPstateData[["DEMO"]][["SGP_Configuration"]][["max.order.for.percentile"]] <- 2
			SGPstateData[["DEMO"]][["SGP_Configuration"]][["print.other.gp"]] <- TRUE
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][GRADE==9 & CONTENT_AREA == "READING", CONTENT_AREA := "GRADE_9_LIT"]
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][GRADE==10 & CONTENT_AREA == "READING", CONTENT_AREA := "AMERICAN_LIT"]
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][GRADE==9 & CONTENT_AREA == "MATHEMATICS", CONTENT_AREA := "ALGEBRA_I"]
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][GRADE==10 & CONTENT_AREA == "MATHEMATICS", CONTENT_AREA := "ALGEBRA_II"]
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][GRADE %in% 9:10, GRADE := "EOCT"]

			### Add EOCT courses to sgpData_LONG

			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '9'] <- 'ALGEBRA_I'
			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '10'] <- 'ALGEBRA_II'
			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '9'] <- 'GRADE_9_LIT'
			sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '10'] <- 'AMERICAN_LIT'
			sgpData_LONG$GRADE_REPORTED <- sgpData_LONG$GRADE
			sgpData_LONG$GRADE[sgpData_LONG$CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II', 'GRADE_9_LIT', 'AMERICAN_LIT')] <-  'EOCT'


			options(error=recover) # Don't use options(warn=2) - get warnings about knots and bounds from BASELINE SIMEX
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			Demonstration_SGP <- NULL
			tmp.messages <- ("##### Results of testSGP test number 4 #####\n\n")

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				if (.Platform$OS.type != "unix") {
					parallel.config <- paste0("list(BACKEND='FOREACH', TYPE='doParallel', WORKERS=list(SIMEX=", number.cores, ", TAUS=", number.cores, "))")
				} else 	parallel.config <- paste0("list(BACKEND='PARALLEL', WORKERS=list(SIMEX=", number.cores, ", TAUS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			expression.to.evaluate <-
				paste0("\nDemonstration_SGP <- prepareSGP(sgpData_LONG, create.additional.variables=FALSE)\n\nDemonstration_SGP <- analyzeSGP(\n\tsgp_object=Demonstration_SGP,\n\tyears='", tail(sgpData.years, 1), "',\n\tcontent_areas='READING',\n\tsgp.percentiles.baseline.max.order=2,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline,",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,\n\tcalculate.simex=", simex.parameters, ",\n\tcalculate.simex.baseline=", simex.parameters,",\n\tparallel.config=", parallel.config,"\n)\n")

			cat("##### Begin testSGP test number 4, Part 1 #####", fill=TRUE)
			cat("#### Grade-Level, Cohort and Baseline Tests with auto sgp.config construction. ####\n", fill=TRUE)

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(4.1)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP_SIMEX variable

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 4, Part 1 #####\n")

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX']]), 1029023L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX']]), "55fb02e2b05c308c3eb288474a9b3d00")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX: FAIL\n")
			}

			### TEST of SGP_SIMEX_BASELINE variable
			if (calculate.simex.baseline) {
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE']]), 1034475L)) {
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE']]), "816099ca957a29552f4ba1fea88ae1e5")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_BASELINE: FAIL\n")
				}
			}
			tmp.messages <- c(tmp.messages, paste("\n\t##### End testSGP test number 4, Part 1: ", convertTime(timetaken(started.at.overall)), "#####\n"))

			sgp.config <- list(
				AMERICAN_LIT.config=list(
					sgp.content.areas=c('READING', 'GRADE_9_LIT', 'AMERICAN_LIT'),
					sgp.panel.years=tail(sgpData.years, 3),
					sgp.grade.sequences=list(c(8, 'EOCT', 'EOCT')),
					sgp.calculate.simex=eval(parse(text=simex.parameters)),
					sgp.calculate.simex.baseline=eval(parse(text=simex.parameters))),
				ALGEBRA_II.config=list(
					sgp.content.areas=c('MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
					sgp.panel.years=tail(sgpData.years, 3),
					sgp.grade.sequences=list(c(8, 'EOCT', 'EOCT')),
					sgp.calculate.simex=eval(parse(text=simex.parameters)),
					sgp.calculate.simex.baseline=eval(parse(text=simex.parameters)))
			)

			expression.to.evaluate <-
				paste0("\nDemonstration_SGP <- analyzeSGP(\n\tsgp_object=Demonstration_SGP,\n\tsgp.config=sgp.config,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline, ",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,\n\tcalculate.simex=", simex.parameters, ",\n\tcalculate.simex.baseline=", simex.parameters,",\n\tparallel.config=", parallel.config,"\n)\nDemonstration_SGP <- combineSGP(Demonstration_SGP)")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat("\n##### Begin testSGP test number 4, Part 2 #####", fill=TRUE)
			cat("#### EOCT Baseline Tests with custom sgp.config. ####\n", fill=TRUE)

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(4.2)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP_SIMEX and SGP_SIMEX_BASELINE variable

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 4, Part 2 #####\n")

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX']]), 211555L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX']]), "a351d68d993a6ae42714142afd1ea6d3")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX: FAIL\n")
			}

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX']]), 212383L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX']]), "ab2862894723e1d9533a1499195dca98")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX: FAIL\n")
			}

#			if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX, na.rm=TRUE), 1452961L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX), "676e5bab01b85e849c1dab718fb58e78")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX: FAIL\n")
			}

			if (calculate.simex.baseline) {
				tmp.matrix.names <- c("MATHEMATICS.BASELINE", "READING.BASELINE", "GRADE_9_LIT.BASELINE", "AMERICAN_LIT.BASELINE", "ALGEBRA_I.BASELINE", "ALGEBRA_II.BASELINE",
					"MATHEMATICS.BASELINE.SIMEX", "READING.BASELINE.SIMEX", "GRADE_9_LIT.BASELINE.SIMEX", "AMERICAN_LIT.BASELINE.SIMEX", "ALGEBRA_I.BASELINE.SIMEX", "ALGEBRA_II.BASELINE.SIMEX",
					"READING.2015_2016", "READING.2015_2016.SIMEX", "ALGEBRA_II.2015_2016", "ALGEBRA_II.2015_2016.SIMEX", "AMERICAN_LIT.2015_2016", "AMERICAN_LIT.2015_2016.SIMEX")
				if (identical(names(Demonstration_SGP@SGP$Coefficient_Matrices), tmp.matrix.names)) {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: FAIL\n")
				}
			} else {
				tmp.matrix.names <- c("READING.2015_2016", "READING.2015_2016.SIMEX", "ALGEBRA_II.2015_2016", "ALGEBRA_II.2015_2016.SIMEX", "AMERICAN_LIT.2015_2016", "AMERICAN_LIT.2015_2016.SIMEX")
				if (identical(names(Demonstration_SGP@SGP$Coefficient_Matrices), tmp.matrix.names)) {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: FAIL\n")
				}
			}

			if (calculate.simex.baseline) {
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), 217894L)) {
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "73176856b45b8bc490b5e98068319411")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE: FAIL\n")
				}
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), 212818L)) {
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "d4fe0d79e985aacb6331ce95f4a2d01e")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_BASELINE: FAIL\n")
				}
#				if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_BASELINE, na.rm=TRUE), 1465187L)) {
				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE), "3e653f546bd93da33cc915b5d37100a4")) {
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_BASELINE: FAIL\n")
				}
			}
			tmp.messages <- c(tmp.messages, paste("\n\t##### End testSGP test number 4, Part 2: ", convertTime(timetaken(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 4: ", convertTime(timetaken(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 4


		#######################################################################################################################################################
		###
		### TEST NUMBER 5: Test of assessment change functionality
		###
		#######################################################################################################################################################

		if (5 %in% TEST_NUMBER) {

			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			Demonstration_SGP <- ACHIEVEMENT_LEVEL <- HIGH_NEED_STATUS <- NULL
			tmp.messages <- ("##### Results of testSGP test number 5 #####\n\n")
			if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
			sgpData_LONG <- SGPdata::sgpData_LONG
			years <- sgpData.years
			years.step.1 <- years[1:3]; years.step.2 <- years[4]; years.step.3 <- years[5]
			if (is.null(test.option[['Scale_Transition_Types']])) test.option[['Scale_Transition_Types']] <- c("Vertical", "Vertical")
			if (is.null(test.option[['Scale_Transition_Adjustments']])) test.option[['Scale_Transition_Adjustments']] <- list(MATHEMATICS=2100, READING=2200)
			if (identical(test.option[['Earliest_Year_Reported']], TRUE)) test.option[['Earliest_Year_Reported']] <- list(MATHEMATICS=years.step.2, READING=years.step.2)

			##############################################################################
			##### PART 1: Run analyses for year prior to assessment change
			##############################################################################

			##### Create data sets for Step 1 (2 & 3 after new SGPstateData put in place)

			sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$YEAR %in% c(years.step.2, years.step.3)] <-
				sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$YEAR %in% c(years.step.2, years.step.3)] +
				test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']]
			sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$YEAR %in% c(years.step.2, years.step.3)] <-
				sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$YEAR %in% c(years.step.2, years.step.3)] +
				test.option[['Scale_Transition_Adjustments']][['READING']]

			Demonstration_Data_LONG_STEP_1 <- as.data.table(subset(sgpData_LONG, YEAR %in% years.step.1))

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG_STEP_1,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'visualizeSGP'),\n\tplot.types=c('studentGrowthPlot', 'growthAchievementPlot'),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\toutputSGP.output.type='LONG_FINAL_YEAR_Data',\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

			cat(paste0("EVALUATING Test Number 5, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(5)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 5: Part 1 #####\n")

			### TEST of SGP variable

			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 2810958L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
			}

			### TEST of SGP_BASELINE variable

			if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 2844420L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
			}

			### TEST of SGP_TARGET_3_YEAR variable

			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 2548595L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: FAIL\n")
			}

			### TEST of SCALE_SCORE_PRIOR_STANDARDIZED variable

			if (identical(median(Demonstration_SGP@Data$SCALE_SCORE_PRIOR_STANDARDIZED, na.rm=TRUE), 0.059)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR_STANDARDIZED, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR_STANDARDIZED, part 1: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 5, Part 1: ", convertTime(timetaken(started.at.overall)), "#####\n"))


			##############################################################################
			##### PART 2: Create SGPs for assessment transtion year
			##############################################################################

			##### Modify SGPstateData

			tmp.list <-	list(
				MATHEMATICS.PENULTIMATE_YEAR=list(
					boundaries_3=c(150, 700)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_4=c(180, 780)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_5=c(220, 800)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_6=c(240, 830)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_7=c(280, 860)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_8=c(310, 890)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_9=c(340, 920)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					boundaries_10=c(370, 950)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_3=c(392, 440, 481, 529)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_4=c(425, 470, 506, 546)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_5=c(452, 495, 530, 569)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_6=c(465, 509, 546, 588)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_7=c(490, 530, 565, 600)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_8=c(500, 545, 580, 620)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_9=c(515, 560, 595, 630)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_10=c(530, 575, 610, 645)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_3=c(150, 700)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_4=c(180, 780)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_5=c(220, 800)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_6=c(240, 830)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_7=c(280, 860)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_8=c(310, 890)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_9=c(340, 920)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_10=c(370, 950)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']]),
				READING.PENULTIMATE_YEAR=list(
					boundaries_3=c(150, 795)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_4=c(180, 940)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_5=c(220, 955)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_6=c(260, 970)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_7=c(300, 980)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_8=c(330, 990)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_9=c(350, 995)+test.option[['Scale_Transition_Adjustments']][['READING']],
					boundaries_10=c(370, 999)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_3=c(510, 550, 580, 615)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_4=c(542, 580, 606, 635)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_5=c(562, 602, 632, 665)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_6=c(575, 615, 645, 675)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_7=c(586, 625, 655, 690)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_8=c(605, 642, 670, 702)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_9=c(620, 655, 680, 706)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_10=c(642, 675, 700, 730)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_3=c(150, 795)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_4=c(180, 940)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_5=c(220, 955)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_6=c(260, 970)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_7=c(300, 980)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_8=c(330, 990)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_9=c(350, 995)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_10=c(370, 999)+test.option[['Scale_Transition_Adjustments']][['READING']]),
				ALGEBRA_I.PENULTIMATE_YEAR=list(
					boundaries_EOCT=c(340, 920)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_EOCT=c(515, 560, 595, 630)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_EOCT=c(340, 920)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']]),
				ALGEBRA_II.PENULTIMATE_YEAR=list(
					boundaries_EOCT=c(370, 950)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					knots_EOCT=c(530, 575, 610, 645)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']],
					loss.hoss_EOCT=c(370, 950)+test.option[['Scale_Transition_Adjustments']][['MATHEMATICS']]),
				GRADE_9_LIT.PENULTIMATE_YEAR=list(
					boundaries_EOCT=c(350, 995)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_EOCT=c(620, 655, 680, 706)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_EOCT=c(350, 995)+test.option[['Scale_Transition_Adjustments']][['READING']]),
				AMERICAN_LIT.PENULTIMATE_YEAR=list(
					boundaries_EOCT=c(370, 999)+test.option[['Scale_Transition_Adjustments']][['READING']],
					knots_EOCT=c(642, 675, 700, 730)+test.option[['Scale_Transition_Adjustments']][['READING']],
					loss.hoss_EOCT=c(370, 999)+test.option[['Scale_Transition_Adjustments']][['READING']]))
			names(tmp.list) <- gsub("PENULTIMATE_YEAR", rev(sgpData.years)[2], names(tmp.list))
			SGPstateData[["DEMO"]][["Achievement"]][["Knots_Boundaries"]] <- c(SGPstateData[["DEMO"]][["Achievement"]][["Knots_Boundaries"]], tmp.list)

			tmp.list <-	list(
				MATHEMATICS.PENULTIMATE_YEAR=list(
					GRADE_3=as.integer(quantile(subset(sgpData_LONG, GRADE==3 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_4=as.integer(quantile(subset(sgpData_LONG, GRADE==4 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_5=as.integer(quantile(subset(sgpData_LONG, GRADE==5 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_6=as.integer(quantile(subset(sgpData_LONG, GRADE==6 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_7=as.integer(quantile(subset(sgpData_LONG, GRADE==7 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_8=as.integer(quantile(subset(sgpData_LONG, GRADE==8 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_9=as.integer(quantile(subset(sgpData_LONG, GRADE==9 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_10=as.integer(quantile(subset(sgpData_LONG, GRADE==10 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE))),
				READING.PENULTIMATE_YEAR=list(
					GRADE_3=as.integer(quantile(subset(sgpData_LONG, GRADE==3 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_4=as.integer(quantile(subset(sgpData_LONG, GRADE==4 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_5=as.integer(quantile(subset(sgpData_LONG, GRADE==5 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_6=as.integer(quantile(subset(sgpData_LONG, GRADE==6 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_7=as.integer(quantile(subset(sgpData_LONG, GRADE==7 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_8=as.integer(quantile(subset(sgpData_LONG, GRADE==8 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_9=as.integer(quantile(subset(sgpData_LONG, GRADE==9 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE)),
					GRADE_10=as.integer(quantile(subset(sgpData_LONG, GRADE==10 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE))),
				ALGEBRA_I.PENULTIMATE_YEAR=list(
					GRADE_EOCT=as.integer(quantile(subset(sgpData_LONG, GRADE==9 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE))),
				ALGEBRA_II.PENULTIMATE_YEAR=list(
					GRADE_EOCT=as.integer(quantile(subset(sgpData_LONG, GRADE==10 & CONTENT_AREA=="MATHEMATICS" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE))),
				GRADE_9_LIT.PENULTIMATE_YEAR=list(
					GRADE_EOCT=as.integer(quantile(subset(sgpData_LONG, GRADE==9 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE))),
				AMERICAN_LIT.PENULTIMATE_YEAR=list(
					GRADE_EOCT=as.integer(quantile(subset(sgpData_LONG, GRADE==10 & CONTENT_AREA=="READING" & YEAR==years.step.2)[['SCALE_SCORE']], probs=c(0.3, 0.45, 0.65, 0.9), na.rm=TRUE))))
			names(tmp.list) <- gsub("PENULTIMATE_YEAR", rev(sgpData.years)[2], names(tmp.list))
			SGPstateData[["DEMO"]][["Achievement"]][["Cutscores"]] <- c(SGPstateData[["DEMO"]][["Achievement"]][["Cutscores"]], tmp.list)

			SGPstateData[["DEMO"]][["Achievement"]][["Levels"]] <-
				list(
					Labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"),
					Proficient=c("Not Proficient", "Not Proficient", "Not Proficient", "Proficient", "Proficient"))

			SGPstateData[["DEMO"]][["Growth"]][["System_Type"]] <- "Cohort Referenced"

			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Scale_Change"]] <- list(MATHEMATICS=rev(sgpData.years)[2], READING=rev(sgpData.years)[2])

			tmp.list <-	list(
				Assessment_Abbreviation="DEMO Old",
				Assessment_Abbreviation.PENULTIMATE_YEAR="DEMO New",
				Assessment_Name="Old Demonstration Student Assessment Program",
				Assessment_Name.PENULTIMATE_YEAR="New Demonstration Student Assessment Program",
				Achievement_Levels=list(
					Labels=c("Unsatisfactory", "Partially Proficient", "Proficient", "Advanced", "No Score"),
					Proficient=c("Not Proficient", "Not Proficient", "Proficient", "Proficient", NA)),
				Achievement_Levels.PENULTIMATE_YEAR=list(
					Labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"),
					Proficient=c("Not Proficient", "Not Proficient", "Not Proficient", "Proficient", "Proficient")),
				Achievement_Level_Labels=list(
					"Unsatisfactory"="Unsatisfactory",
					"Part Proficient"="Partially Proficient",
					"Proficient"="Proficient",
					"Advanced"="Advanced"),
				Achievement_Level_Labels.PENULTIMATE_YEAR=list(
					"Level 1"="Level 1",
					"Level 2"="Level 2",
					"Level 3"="Level 3",
					"Level 4"="Level 4",
					"Level 5"="Level 5"),
				Content_Areas_Labels=list(MATHEMATICS="Math", READING="Reading"),
				Content_Areas_Labels.PENULTIMATE_YEAR=list(MATHEMATICS="Math", READING="Reading"),
				Grades_Tested=c(3,4,5,6,7,8,9,10),
				Grades_Tested.PENULTIMATE_YEAR=c(3,4,5,6,7,8,9,10),
				Year=rev(sgpData.years)[2]
			)
			names(tmp.list) <- gsub("PENULTIMATE_YEAR", rev(sgpData.years)[2], names(tmp.list))
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]] <- tmp.list

			SGPstateData[["DEMO"]][["Student_Report_Information"]] <-
				list(
					Vertical_Scale=list(MATHEMATICS=TRUE, READING=TRUE),
					Projection_Fan_Limits=c(5, 95),
					Content_Areas_Labels=list(MATHEMATICS="Math", READING="Reading"),
					Grades_Reported=list(MATHEMATICS=c(3,4,5,6,7,8,9,10), READING=c(3,4,5,6,7,8,9,10)),
					Achievement_Level_Labels=list(
						"Level 1"="Level 1",
						"Level 2"="Level 2",
						"Level 3"="Level 3",
						"Level 4"="Level 4",
						"Level 5"="Level 5"))

			if (identical(toupper(test.option[['Scale_Transition_Types']]), c("VERTICAL", "VERTICAL"))) {
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale"]] <- "Yes"
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Vertical_Scale", rev(sgpData.years)[2], sep=".")]] <- "Yes"
			}
			if (identical(toupper(test.option[['Scale_Transition_Types']]), c("NON-VERTICAL", "VERTICAL"))) {
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale"]] <- "No"
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Vertical_Scale", rev(sgpData.years)[2], sep=".")]] <- "Yes"
			}
			if (identical(toupper(test.option[['Scale_Transition_Types']]), c("NON-VERTICAL", "NON-VERTICAL"))) {
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale"]] <- "No"
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Vertical_Scale", rev(sgpData.years)[2], sep=".")]] <- "No"
			}

			### Create Data for Steps 2 & 3 with ACHIEVEMENT_LEVEL based upon new SGPstateData

			Demonstration_Data_LONG_STEP_2 <- as.data.table(subset(sgpData_LONG, YEAR %in% years.step.2))[,ACHIEVEMENT_LEVEL:=NULL]
			Demonstration_Data_LONG_STEP_2 <- suppressMessages(prepareSGP(Demonstration_Data_LONG_STEP_2)@Data)
			Demonstration_Data_LONG_STEP_2[, HIGH_NEED_STATUS:=NULL]
			setcolorder(Demonstration_Data_LONG_STEP_2, names(Demonstration_Data_LONG_STEP_1))
			Demonstration_Data_LONG_STEP_3 <- as.data.table(subset(sgpData_LONG, YEAR %in% years.step.3))[,ACHIEVEMENT_LEVEL:=NULL]
			Demonstration_Data_LONG_STEP_3 <- suppressMessages(prepareSGP(Demonstration_Data_LONG_STEP_3)@Data)
			Demonstration_Data_LONG_STEP_3[, HIGH_NEED_STATUS:=NULL]
			setcolorder(Demonstration_Data_LONG_STEP_3, names(Demonstration_Data_LONG_STEP_3))

			### updateSGP

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_STEP_2,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=TRUE,\n\tsgp.projections.lagged=TRUE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.percentiles.equated=TRUE,\n\tsgp.percentiles.equating.method=c('identity', 'mean', 'linear', 'equipercentile'),\n\tsave.intermediate.results=FALSE,\n\toutputSGP.output.type='LONG_FINAL_YEAR_Data',\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number 5, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(5)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 5: Part 2 #####\n")

			### TEST of SGP variable

			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
			}

			### TEST of SGP variable from equated analyses

			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', rev(sgpData.years)[2], 'EQUATED', sep=".")]][['SGP_EQUATED']], na.rm=TRUE), 1418003L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP from equated analysis, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP from equated analysis, part 2: FAIL\n")
			}

			### TEST of SCALE_SCORE_EQUATED variable

			if (identical(as.integer(sum(Demonstration_SGP@Data$SCALE_SCORE_EQUATED, na.rm=TRUE)), 796049037L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_EQUATED, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_EQUATED, part 2: FAIL\n")
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable

			if (identical(as.integer(sum(Demonstration_SGP@SGP$SGProjections[[paste('READING', rev(sgpData.years)[2], 'LAGGED.TARGET_SCALE_SCORES', sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']])), 82327526L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1, part 2: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 5, Part 2: ", convertTime(timetaken(started.at.intermediate)), "#####\n"))


			##############################################################################
			##### STEP 3: Create SGPs for year after assessment transtion year
			##############################################################################

			##### Modify SGPstateData

			SGPstateData[["DEMO"]][["Student_Report_Information"]][["Earliest_Year_Reported"]] <- test.option[['Earliest_Year_Reported']]
			SGPstateData[["DEMO"]][['SGP_Configuration']][['sgPlot.plot.test.transition']] <- test.option[['sgPlot.plot.test.transition']]

			### updateSGP

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_STEP_3,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=TRUE,\n\tsgp.projections.lagged=TRUE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsave.intermediate.results=FALSE,\n\toutputSGP.output.type=c('LONG_Data', 'LONG_FINAL_YEAR_Data', 'WIDE_Data', 'INSTRUCTOR_Data'),\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number 5, Part 3:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(5)_Memory_Profile_Part_3.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 5: Part 3 #####\n")

			### TEST of SGP variable

			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 3: FAIL\n")
			}

			### TEST of LEVEL_1_SGP_TARGET_YEAR_1_CURRENT variable

			if (identical(as.integer(sum(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1), sep=".")]][['LEVEL_1_SGP_TARGET_YEAR_1_CURRENT']], na.rm=TRUE)), 987731L)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1_CURRENT, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1_CURRENT, part 3: FAIL\n")
			}

			### TEST of ACHIEVEMENT_LEVEL_PRIOR Variable in MATHEMATICS.XXXX_XXXX.LAGGED projections

			if (identical(as.vector(table(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1), 'LAGGED', sep=".")]][['ACHIEVEMENT_LEVEL_PRIOR']])), c(8178L, 4316L, 5991L, 7552L, 3145L))) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable ACHIEVEMENT_LEVEL_PRIOR, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable ACHIEVEMENT_LEVEL_PRIOR, part 3: FAIL\n")
			}

			### TEST of SGP_FROM_2014_2015 Variable in @Data

			if (identical(sum(Demonstration_SGP@Data[[paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_")]], na.rm=TRUE), 2900310L)) {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ", paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_"), ", part 3: OK\n"))
			} else {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ", paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_"), ", part 3: FAIL\n"))
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 5, Part 3: ", convertTime(timetaken(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 5: ", convertTime(timetaken(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 5


		#######################################################################################################################################################
		###
		### TEST NUMBER 6: Test of baseline coefficient matrix generation functionality
		###
		#######################################################################################################################################################

		if (6 %in% TEST_NUMBER) {

			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
			Demonstration_SGP <- tmp.messages <- NULL

			### Modify SGPstateData

			SGPstateData[['DEMO']][['Baseline_splineMatrix']] <- NULL

			expression.to.evaluate <-
				paste0("\nDemonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='", tail(sgpData.years, 1), "',\n\tcontent_areas='MATHEMATICS',\n\tsgp.percentiles=FALSE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=TRUE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(BASELINE_PERCENTILES=", number.cores, ", BASELINE_MATRICES=", number.cores, "))\n)")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat("##### Begin testSGP test number 6 #####\n", fill=TRUE)
			cat("## Basic Baseline Coefficient Matrix Test. ##\n", fill=TRUE)

			cat(paste0("EVALUATING Test Number 6:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(6)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP_BASELINE variable

			tmp.messages <- ("##### Results of testSGP test number 6 #####\n\n")

#			if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 1469353L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "49a36274c536fe13ac2bcdc1c49cadc9")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of SGP_BASELINE: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of SGP_BASELINE: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 6: ", convertTime(timetaken(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 6


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI: Test of SGPt functionality
		###
		#######################################################################################################################################################

		if ("RLI" %in% TEST_NUMBER) {

			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				if (.Platform$OS.type != "unix") {
					parallel.config <- paste0("list(BACKEND='FOREACH', TYPE='doParallel', WORKERS=list(TAUS=", number.cores, "))")
				} else  parallel.config <- paste0("list(BACKEND='PARALLEL', WORKERS=list(TAUS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			RLI_SGPt_PART_1 <- RLI_SGPt_PART_2 <- RLI_SGPt_PART_3 <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI #####\n\n"
			tmp.last.window <- tail(sort(unique(SGPdata::sgptData_LONG[['YEAR']])), 1)

			###############################################################################
			### PART 1: Using RLI_SGPt_UPDATE_SHELL
			###############################################################################

			RLI_Data_LONG_UPDATE <- SGPdata::sgptData_LONG[YEAR %in% tmp.last.window]
			RLI_SGPt_UPDATE_SHELL <- suppressMessages(prepareSGP(SGPdata::sgptData_LONG[!YEAR %in% tmp.last.window], state="RLI"))
			SGPstateData[["RLI"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <-
				eval(parse(text=paste("RLI_SGPt_Baseline_Matrices[['RLI_SGPt_Baseline_Matrices_2014_2015.3']]")))
			SGPstateData[["RLI"]][["Assessment_Program_Information"]][['CSEM']] <- "SEM"
			SGPstateData[["RLI"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_SGPt_UPDATE_SHELL,\n\tadditional.data=RLI_Data_LONG_UPDATE,\n\ttesting.window='SPRING',\n\teow.or.update='UPDATE',configuration.year='2014_2015',\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, "))\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI_SGPt_PART_1, file='Data/RLI_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			file.rename("Data/RLI", "Data/RLI_PART_1")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number RLI: Part 1 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 107188L)) {
			if (identical(digest(RLI_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "208ac70d7e8349eddcaaf2f62c1e174e")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
			}

			### TEST of SGP_TARGET_BASELINE_10_TIME_CURRENT variable from READING

#			if (identical(sum(RLI_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']], na.rm=TRUE), 49402L)) { ## 2015 was 45054L, 2016 was 48668L
			if (identical(digest(RLI_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']]), "f0a5e3227b56a42cfcdedbf2fe72d61b")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: FAIL\n")
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 533302)) {
			if (identical(digest(RLI_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]), "5e4d2ca953248833801ca748cd5d31d7")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: FAIL\n")
			}

			### TEST of READING SGPercentiles output: Dimension

			invisible(unzip(paste("Data/RLI_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
			tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(2179L, 10L))) {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: FAIL\n")
			}

			### TEST of READING SGPercentiles output: Length of SGP_NORM_GROUP_BASELINE,

			my.tmp.1 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE,perl=TRUE), function(x) length(x))
			my.tmp.2 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_SCALE_SCORES,perl=TRUE), function(x) length(x))
			my.tmp.3 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_DATES,perl=TRUE), function(x) length(x))

			if (identical(my.tmp.1, my.tmp.2) & identical(my.tmp.2, my.tmp.3)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: FAIL\n")
			}

			### TEST of READING SGProjections output

			invisible(unzip(paste("Data/RLI_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
			tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(981L, 118L))) {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
			}

			### TEST of READING SGProjections TARGETS output

			invisible(unzip(paste("Data/RLI_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt.zip", sep=".")))
			tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
			unlink(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))

			if (identical(dim(tmp.data), c(819L, 25L))) { ### 2014-2015 was 867L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI, Part 1: ", convertTime(timetaken(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 2: Using LONG Data
			###############################################################################

			RLI_SGPt_Data_LONG <- SGPdata::sgptData_LONG

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI_SGPt_PART_2 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, "))\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI_SGPt_PART_2, file='Data/RLI_SGPt_PART_2.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			file.rename("Data/RLI", "Data/RLI_PART_2")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI: Part 2 #####\n")

			### TEST of equality between RLI_SGPt_PART_1@SGP and RLI_SGPt_PART_2@SGP

			if (identical(RLI_SGPt_PART_1@SGP, RLI_SGPt_PART_2@SGP)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI_PART_1@SGP and RLI_PART_2@SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI_PART_1@SGP and RLI_PART_2@SGP, part 2: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI: Part 2", convertTime(timetaken(started.at.intermediate)), "#####\n"))


			###############################################################################
			### PART 3: Using Matrices from Next Window
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI_SGPt_PART_3 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\tcoefficient.matrices=RLI_SGPt_Baseline_Matrices$RLI_SGPt_Baseline_Matrices_2014_2015.2,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, "))\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI_SGPt_PART_3, file='Data/RLI_SGPt_PART_3.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI, Part 3:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_3.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			file.rename("Data/RLI", "Data/RLI_PART_3")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI: Part 3 #####\n")

			### TEST of SGP variable from READING (equality with PART 1)

#			if (identical(sum(RLI_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 107188L)) {
			if (identical(digest(RLI_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "208ac70d7e8349eddcaaf2f62c1e174e")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2014_2015.2 matrices = 2014_2015.3 matrices), part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 3: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI: Part 3", convertTime(timetaken(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number RLI: ", convertTime(timetaken(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER RLI
	} ### END testSGP Function
