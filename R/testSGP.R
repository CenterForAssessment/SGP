`testSGP` <-
function(
	TEST_NUMBER,
	save.results=TRUE,
	test.option=list(),
	memory.profile=FALSE,
	stop.fail=TRUE) {

	YEAR <- GRADE <- DUPS_FLAG <- SGP_NORM_GROUP <- SCALE_SCORE_CSEM <- NULL

	if (missing(TEST_NUMBER)) {
		messageSGP("\ttestSGP carries out testing of SGP package. Tests currently included in testSGP:\n")
		messageSGP("\t\t1. abcSGP test using all available years.")
		messageSGP("\t\t2. abcSGP test using all available years except most recent followed by an updated analysis using the most recent year's data.")
	}

	if (is.null(stop.fail)) stop.fail <- FALSE

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
				paste0("Demonstration_SGP <- list(Panel_Data=SGPdata::sgpData)\nmy.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)\nfor (i in seq_along(my.grade.sequences)) {\n\tDemonstration_SGP <- studentGrowthPercentiles(\n\t\tpanel.data=Demonstration_SGP,\n\t\tsgp.labels=list(my.year=", tail(sgpData.years.single, 1L), ", my.subject='Reading'),\n\t\tgrowth.levels='DEMO',\n\t\tgoodness.of.fit='DEMO',\n\t\tgoodness.of.fit.output.format=c('PDF', 'PNG', 'SVG', 'DECILE_TABLES'),\n\t\tgrade.progression=my.grade.sequences[[i]],\n\t\tpercentile.cuts=c(1,35,50,65,99),\n\t\tprint.sgp.order=TRUE,\n\t\tcalculate.confidence.intervals='DEMO',\n\t\tprint.other.gp=TRUE,\n\t\tverbose.output=TRUE,\n\t\tmax.order.for.percentile=3,\n\t\treturn.additional.max.order.sgp=2,\n\t\treturn.norm.group.scale.scores=TRUE,\n\t\treturn.panel.data=TRUE,\n\t\tparallel.config=", parallel.config,")\n}")

			cat(paste0("EVALUATING Test Number 0, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(0)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 0: Part 1 #####\n")

			### TEST of SGP Variable
			# if (identical(sum(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP']]), 1707319L)) {
			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP']]), "218a961b96d1f6588c408f5698ac04ce")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_ORDER_1 Variable
			# if (identical(sum(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP_ORDER_1']]), 1708589L)) {
			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP_ORDER_1']]), "543b9397789643ae0619256c05812a17")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER_1, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER_1, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_PRIOR Variable
			# if (identical(sum(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SCALE_SCORE_PRIOR']]), 20707938)) {
			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SCALE_SCORE_PRIOR']]), "87b318d12cad2d571cbb59297b6b0a3d")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_ORDER Variable
			# if (identical(as.vector(table(Demonstration_SGP$SGPercentiles[[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP_ORDER']])), c(8666L, 7639L, 17920L))) {
			if (identical(digest(table(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP_ORDER']])), "b2779fb3a7408ade8eea0c69cb9f265b")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_LEVEL Variable
			# if (identical(as.vector(table(Demonstration_SGP$SGPercentiles[[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP_LEVEL']])), c(6740L, 6824L, 7176L, 6835L, 6650L))) {
			if (identical(digest(table(Demonstration_SGP[['SGPercentiles']][[paste("READING", tail(sgpData.years.single, 1L), sep=".")]][['SGP_LEVEL']])), "22d6dafb62d5d6b2927c7a08ba1d8461")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_LEVEL, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_LEVEL, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_STANDARD_ERROR Variable
			# if (identical(sum(Demonstration_SGP$SGPercentiles[[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][['SGP_STANDARD_ERROR']], na.rm=TRUE), 283761.5)) {
			# if (identical(sum(Demonstration_SGP$SGPercentiles[[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][['SGP_STANDARD_ERROR']], na.rm=TRUE), 543735.7)) { ## 1.9-0.0
			# if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][['SGP_STANDARD_ERROR']]), "bc17af609fa1ac0bf71548a36cff8ceb")) { ## Pre 1.9-0.0
			# if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][['SGP_STANDARD_ERROR']]), "157ff9ac26da921092d7c115768537ac")) { ## Pre mod allow state corrected loss.hoss
			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][['SGP_STANDARD_ERROR']]), "cc80e301fa445eb1925b8ed125a9818c")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_STANDARD_ERROR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_STANDARD_ERROR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_FROM_**** variable
			#  sum(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][[paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1L))-2, sep="_")]]) # 1708877
			if (identical(digest(Demonstration_SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years.single, 1L), sep=".")]][[paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1L))-2, sep="_")]]), "454ff2c53966e1cfd4c9280aab4b40b5")) {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ",  paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1L))-2, sep="_"), ", part 2: OK\n"))
			} else {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ", paste('SGP_FROM', as.numeric(tail(sgpData.years.single, 1L))-2, sep="_"), ", part 2: FAIL\n"))
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of Goodness of Fit Output Files

			gof.files <-  c("Decile_Tables",
							"gofSGP_Grade_4.pdf", "gofSGP_Grade_4.png", "gofSGP_Grade_4.svg",
							"gofSGP_Grade_5.pdf", "gofSGP_Grade_5.png", "gofSGP_Grade_5.svg",
							"gofSGP_Grade_6.pdf", "gofSGP_Grade_6.png", "gofSGP_Grade_6.svg",
							"gofSGP_Grade_7.pdf", "gofSGP_Grade_7.png", "gofSGP_Grade_7.svg",
							"gofSGP_Grade_8.pdf", "gofSGP_Grade_8.png", "gofSGP_Grade_8.svg")
			if (identical(sort(list.files(file.path("Goodness_of_Fit", paste("READING", tail(sgpData.years.single, 1L), sep=".")))), gof.files)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of Goodness of Fit Output Files, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of Goodness of Fit Output Files, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 0, Part 1: ", convertTime(timetakenSGP(started.at.overall)), "#####\n")


			### Part 2

			expression.to.evaluate <-
				paste0("Demonstration_SGP$Panel_Data <- SGPdata::sgpData[,c('ID',", paste('\'GRADE', head(sgpData.years.single, -1L), sep='_', collapse='\', '), "', ", paste('\'SS', head(sgpData.years.single, -1L), sep='_', collapse='\', '), "')]\nmy.grade.progressions <- list(3, 3:4, 3:5, 3:6, 4:7)\nfor (i in seq_along(my.grade.progressions)) {\n\tDemonstration_SGP <- studentGrowthProjections(\n\t\tpanel.data=Demonstration_SGP,\n\t\tsgp.labels=list(my.year=", tail(sgpData.years.single, 1L), ", my.subject='Reading', my.extra.label='LAGGED'),\n\t\tuse.my.coefficient.matrices=list(my.year=", tail(sgpData.years.single, 1L), ", my.subject='Reading'),\n\t\tprojcuts.digits=0,\n\t\tperformance.level.cutscores='DEMO',\n\t\tpercentile.trajectory.values=1:99,\n\t\tlag.increment=1,\n\t\tgrade.progression=my.grade.progressions[[i]],\n\t\treturn.projection.group.identifier='READING',\n\t\treturn.projection.group.scale.scores=TRUE)\n}")

			cat(paste0("EVALUATING Test Number 0, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(0)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")
			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 0: Part 2 #####\n")

			### TEST of dimension of table READING.####.LAGGED dimensions
			# if (identical(dim(Demonstration_SGP$SGProjections[[paste('READING', tail(sgpData.years.single, 1L), 'LAGGED', sep=".")]]), c(36478L, 513L))) {
			if (identical(digest(Demonstration_SGP[['SGProjections']][[paste('READING', tail(sgpData.years.single, 1L), 'LAGGED', sep=".")]]), "8ed0dbdfe95ea48e426a1e1faf9e8da1")) {
				tmp.messages <- c(tmp.messages, paste("\t\tTest of",  paste('READING', tail(sgpData.years.single, 1L), 'LAGGED', sep="."), "table dimensions, part 2: OK\n"))
			} else {
				tmp.messages <- c(tmp.messages, paste("\t\tTest of",  paste('READING', tail(sgpData.years.single, 1L), 'LAGGED', sep="."), "table dimensions, part 2: FAIL\n"))
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of LEVEL_1_SGP_TARGET_YEAR_1 Variable
			# if (identical(sum(Demonstration_SGP$SGProjections[[paste('READING', tail(sgpData.years.single, 1L), "LAGGED", sep=".")]][['LEVEL_1_SGP_TARGET_YEAR_1']]), 402866L)) {
			if (identical(digest(Demonstration_SGP[['SGProjections']][[paste('READING', tail(sgpData.years.single, 1L), "LAGGED", sep=".")]][['LEVEL_1_SGP_TARGET_YEAR_1']]), "60193d09485ede52ddcf58a51dedf04f")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P84_PROJ_YEAR_4 Variable
			# if (identical(sum(Demonstration_SGP$SGProjections[[paste('READING', tail(sgpData.years.single, 1L), "LAGGED", sep=".")]][['P84_PROJ_YEAR_4']], na.rm=TRUE), 10545791)) {
			if (identical(digest(Demonstration_SGP[['SGProjections']][[paste('READING', tail(sgpData.years.single, 1L), "LAGGED", sep=".")]][['P84_PROJ_YEAR_4']]), "e691fdcb7f80f9d716685300a9e16b48")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P84_PROJ_YEAR_4, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P84_PROJ_YEAR_4, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 0, Part 2: ", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 0: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
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
					parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
				} else  parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
			} else parallel.config <- test.option[['parallel.config']]

			### Some minor modifications to SGPstateData for testing purposes

			SGPstateData[["DEMO"]][["SGP_Configuration"]][["print.other.gp"]] <- TRUE
			SGPstateData[["DEMO"]][["SGP_Configuration"]][["calculate.confidence.intervals"]] <- list(confidence.quantiles=c(0.025, 0.975))

			if (toupper(TEST_NUMBER) == "1B") sgp.sqlite <- TRUE else sgp.sqlite <- FALSE

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tdata_supplementary=list(INSTRUCTOR_NUMBER=SGPdata::sgpData_INSTRUCTOR_NUMBER),\n\tprepareSGP.create.additional.variables=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.sqlite=", sgp.sqlite, ",\n\tget.cohort.data.info=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

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
			if (identical(digest(Demonstration_SGP@Data[['SGP']]), "89f1cb732587f3d6ffeb1afe587941e0")) { # pre-GRADE key: d7843eab4ddf02bed640ec401dd35c65
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_BASELINE variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP_BASELINE']], na.rm=TRUE), 8573825L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_BASELINE']]), "5056dc27abde7c6d8ff896947f4b0070")) { # pre-GRADE key: f6ffda530e470321159c39a56151e0c2
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP_TARGET_3_YEAR']], na.rm=TRUE), 7796624L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_TARGET_3_YEAR']]), "4955e2f61a1cd34347ffceba51a5670b")) { # pre-GRADE key: 7cfb4055b5ebf739417d2220898ce99c
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data[['SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR']], na.rm=TRUE), 9201802L)) {
			if (identical(digest(Demonstration_SGP@Data[['SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR']]), "5e6fcc4c3eedce14cc6639163677fdc7")) { # pre-GRADE key: 9bfb90fbaed288104dfe1959294497ce
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data[['CATCH_UP_KEEP_UP_STATUS_3_YEAR']])), c(41099, 10837, 35560, 84390))) {
			if (identical(digest(Demonstration_SGP@Data[['CATCH_UP_KEEP_UP_STATUS_3_YEAR']]), "eb710e8b3256be3f9009ecc686c0ff37")) { # pre-GRADE key: 11e5e53c8cf330fcdb51a2e4894f5c2f
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of MOVE_UP_STAY_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data[['MOVE_UP_STAY_UP_STATUS_3_YEAR']])), c(72953, 15043, 18336, 13618))) {
			if (identical(digest(Demonstration_SGP@Data[['MOVE_UP_STAY_UP_STATUS_3_YEAR']]), "f67a9a4d2bf8c8af201e2533c31c0ce8")) { # pre-GRADE key: 663ff40f494a37d01dc3c593b887d24b
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_PRIOR variable

#			if (identical(sum(Demonstration_SGP@Data[['SCALE_SCORE_PRIOR']], na.rm=TRUE), 100865095)) {
			if (identical(digest(Demonstration_SGP@Data[['SCALE_SCORE_PRIOR']]), "6c352608f27873a09487ec2859de7574")) { # pre-GRADE key: 23e0433ed1574c73f923b23cddbfc0cb
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable for READING.XXXX_XXXX scale score targets

#			if (identical(as.integer(sum(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']])), 18313900L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']]), "d67a2c0b75f411683abe1a9c023c8c0e")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of MEDIAN_SGP variable

#			if (identical(sum(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['MEDIAN_SGP']], na.rm=TRUE), 9140.5)) {
			# if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['MEDIAN_SGP']]), "0d8c6f5489ff405a492a7ef193884a56")) { # pre-`collapse` integration
			if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['MEDIAN_SGP']]), "1a23b08f0ed5770514320beaf1b0cd3a")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of PERCENT_AT_ABOVE_PROFICIENT_PRIOR variable

#			if (identical(sum(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['PERCENT_AT_ABOVE_PROFICIENT_PRIOR']], na.rm=TRUE), 12894.2)) {
			# if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['PERCENT_AT_ABOVE_PROFICIENT_PRIOR']]), "f1e683f2c118135b28d285a754d1f188")) { # pre-`collapse` integration
			if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['PERCENT_AT_ABOVE_PROFICIENT_PRIOR']]), "ac54994420c54e5fc008ea1df4159845")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of LAGGED PROJECTION CUTs variable P35_PROJ_YEAR_1 variable for MATHEMATICS.XXXX_XXXX.LAGGED

#			if (identical(sum(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]][['P20_PROJ_YEAR_1']]), 15567825)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]][['P20_PROJ_YEAR_1']]), "e83219e1830264576b42861aaaed067f")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable P20_PROJ_YEAR_1 (LAGGED PROJECTION): OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable P20_PROJ_YEAR_1 (LAGGED PROJECTION): FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_0.025_CONFIDENCE_BOUND variable in @Data

#			if (identical(sum(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']], na.rm=TRUE), 6096120)) {
#			if (identical(sum(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']], na.rm=TRUE), 3952340)) {
#			if (identical(digest(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']]), "5e48f22520ff2607032c2e4b51ddccc2")) { # pre-GRADE key: dc13c362312f379fbbfa83c49f1e53eb
#			if (identical(digest(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']]), "237d3505dfbbc4a4dfbdbdbd73c45629")) { # pre Adam fix of cohort/baseline confidence intervals
			if (identical(digest(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']]), "2644dab9800beb4d45fe0be848704a7f")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_0.025_CONFIDENCE_BOUND: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_0.025_CONFIDENCE_BOUND: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of HIGH_NEED_STATUS variable in @Data

#			if (identical(as.numeric(table(Demonstration_SGP@Data[['HIGH_NEED_STATUS']])), c(57059, 58942))) {
			if (identical(digest(Demonstration_SGP@Data[['HIGH_NEED_STATUS']]), "ab4d97f1d56ea8cf936008708cf4ed84")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable HIGH_NEED_STATUS: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable HIGH_NEED_STATUS: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable in @Data

#			if (identical(sum(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1, na.rm=TRUE), 103740628)) {
			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1), "60ada507fc67c9bfd9822e0ff48a33e0")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT variable in @Data

#			if (identical(sum(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT, na.rm=TRUE), 117211493)) {
			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT), "314bc76b76b0047f641c1a8a149ae0a8")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste0("\n##### End testSGP test number ", TEST_NUMBER, ":  ", convertTime(timetakenSGP(started.at.overall)), " #####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 1 & 1B


		##########################################################################################################################################################################
		###
		### TEST NUMBER 2: Various tests of updateSGP functionality.
		###
		### TEST NUMBER 2a: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2012-2013 to 2015-2016 followed by adding with_sgp_data_LONG 2016-2017 using
		###			overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=FALSE.
		### TEST NUMBER 2b: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2012-2013 to 2015-2016 followed by adding with_sgp_data_LONG 2016-2017 using
		###			overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=FALSE.
		### TEST NUMBER 2c: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2012-2013 to 2015-2016 followed by adding with_sgp_data_LONG 2016-2017 using
		###			overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=TRUE.
		### TEST NUMBER 2d: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2012-2013 to 2015-2016 followed by adding with_sgp_data_LONG 2016-2017 using
		###			overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=TRUE.
		###
		#########################################################################################################################################################################

		if (toupper(TEST_NUMBER) %in% c("2", "2A", "2B", "2C", "2D")) {
		if (identical(TEST_NUMBER, 2)) TEST_NUMBER <- c("2A", "2B", "2C")

		if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
		if (is.null(test.option[['parallel.config']])) {
			if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
			parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
		} else parallel.config <- test.option[['parallel.config']]

		################################
		### TEST NUMBER 2a
		################################

		if ('2A' %in% toupper(TEST_NUMBER)) {

			options(error=recover)
			options(warn=2)
			Demonstration_SGP <- NULL
			Demonstration_Data_LONG <- subset(SGPdata::sgpData_LONG, YEAR %in% head(sgpData.years, -1L))
			Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1L))
			tmp.messages <- "##### Begin testSGP test number 2a #####\n\n"

			### Part 1

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(2a)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall.2a <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP variable

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 2a: Part 1 #####\n")

#			if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "3ba0ed9a36804c4e05b214c4001a28f6")) { # pre-GRADE key: 7ad371d09d281ad5604b9288897ab337, # pre-091222 93092a02ed23590cdfe45fd25fac7792
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_BASELINE variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 5667488L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "428f3be2573bdf43c37692dc6035d064")) { # pre-GRADE key: a2d8cfffa3565c2a2103eba276a3effe
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 5245437L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "de3928618b7dc062f246f9f6dc580a93")) { # pre-GRADE key: d14088916dc65391d44cab3995a4f7b6
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 6088129L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "2f3c426ddac754668662706ba4ac543a")) { # pre-GRADE key: bfd8ae0c9497c8a4d13f09999eac7d33
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS_3_YEAR)), c(27122, 6990, 24358, 55283))) {
			if (identical(digest(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS_3_YEAR), "5ce288b5ce7861c8cbb6e8ecdf024d11")) { # pre-GRADE key: 46373a48b1a926c107b66fc0222337b3
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of MOVE_UP_STAY_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS_3_YEAR)), c(48152, 10396, 12150, 8943))) {
			if (identical(digest(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS_3_YEAR), "6eda205f912ded935746dbe29d188fdc")) { # pre-GRADE key: 8a33c63b43e4b35e0f86f61c55583264
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste0("\t##### End testSGP test number 2a: Part 1: ", convertTime(timetakenSGP(started.at.overall.2a)), " #####\n"))


			### Part 2

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_LAST_YEAR,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

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
			if (identical(digest(Demonstration_SGP@Data$SGP), "89f1cb732587f3d6ffeb1afe587941e0")) { # pre-GRADE key: d7843eab4ddf02bed640ec401dd35c65
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_BASELINE variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "5056dc27abde7c6d8ff896947f4b0070")) { # pre-GRADE key: f6ffda530e470321159c39a56151e0c2
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "4955e2f61a1cd34347ffceba51a5670b")) { # pre-GRADE key: 7cfb4055b5ebf739417d2220898ce99c
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 9201802L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "5e6fcc4c3eedce14cc6639163677fdc7")) { # pre-GRADE key: 9bfb90fbaed288104dfe1959294497ce
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS_3_YEAR)), c(41099, 10837, 35560, 84390))) {
			if (identical(digest(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS_3_YEAR), "eb710e8b3256be3f9009ecc686c0ff37")) { # pre-GRADE key: 11e5e53c8cf330fcdb51a2e4894f5c2f
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of MOVE_UP_STAY_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS_3_YEAR)), c(72953, 15043, 18336, 13618))) {
			if (identical(digest(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS_3_YEAR), "f67a9a4d2bf8c8af201e2533c31c0ce8")) { # pre-GRADE key: 663ff40f494a37d01dc3c593b887d24b
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 2a, Part 2: ", convertTime(timetakenSGP(started.at.intermediate.2a)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 2a: ", convertTime(timetakenSGP(started.at.overall.2a)), "#####\n"))
		} ### End TEST_NUMBER 2a


		################################
		### TEST NUMBER 2b, 2c, 2d
		################################
		if (any(toupper(TEST_NUMBER) %in% c('2B', '2C', '2D'))) {

			for (i in setdiff(toupper(TEST_NUMBER), '2A')) {
				options(error=recover)
				options(warn=2)
				Demonstration_SGP <- ID <- CONTENT_AREA <- NULL
				Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1L))

				############################################################
				### Part 1: Required for all Tests 2b, 2c, and 2d
				############################################################

				tmp.messages <- c(tmp.messages, paste("\n##### Begin testSGP test number", capwords(i), "#####\n\n"))
				tmp.messages <- c(tmp.messages, paste0("\t##### Begin testSGP test number ", capwords(i), ": Part 1 #####\n\n"))

				expression.to.evaluate <-
					paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='", tail(sgpData.years, 1L), "',\n\tparallel.config=", parallel.config, "\n)\n")

				cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

				if (memory.profile) {
					Rprof("testSGP(2)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
				}

				started.at.overall.2b <- proc.time()
				eval(parse(text=expression.to.evaluate))

				tmp.messages <- c(tmp.messages, paste0("\t##### End testSGP test number ", capwords(i), ": Part 1: ", convertTime(timetakenSGP(started.at.overall.2b)), " #####\n"))

				#############################################################
				### Part 2
				#############################################################

				tmp.messages <- c(tmp.messages, paste0("\n\t##### Begin testSGP test number ", capwords(i), ": Part 2 #####\n\n"))

				### TEST 2b ###
				if (i=='2B') {
					Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1L))

					expression.to.evaluate <-
						paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_LAST_YEAR,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

					cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

					started.at.intermediate.2b <- proc.time()
					eval(parse(text=expression.to.evaluate))
				} ### End TEST 2b

				### TEST 2c ###
				if (i=='2C') {
					Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1L))

					expression.to.evaluate <-
						paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_LAST_YEAR,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

					cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

					started.at.intermediate.2b <- proc.time()
					eval(parse(text=expression.to.evaluate))
				} ### End TEST 2c

				### TEST 2d ###
				if (i=='2D') {
					Demonstration_Data_LONG_LAST_YEAR <- subset(SGPdata::sgpData_LONG, YEAR==tail(sgpData.years, 1L))
					tmp.LAST_YEAR.ids <- sort(unique(Demonstration_Data_LONG_LAST_YEAR[['ID']]))
					tmp.group.1 <- tmp.LAST_YEAR.ids[1:150]
					tmp.group.2 <- tmp.LAST_YEAR.ids[151:250]
					with_sgp_data_LONG <- subset(Demonstration_Data_LONG_LAST_YEAR, ID %in% tmp.group.1 | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS"))
					Demonstration_SGP@Data <- subset(Demonstration_SGP@Data, !((ID %in% tmp.group.1 & YEAR==tail(sgpData.years, 1L)) | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS" & YEAR==tail(sgpData.years, 1L))))
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('MATHEMATICS', tail(sgpData.years, 1L), sep=".")]] <- subset(Demonstration_SGP@SGP$SGPercentiles[[paste('MATHEMATICS', tail(sgpData.years, 1L), sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGPercentiles']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'BASELINE', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'BASELINE', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED.BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED.BASELINE', sep=".")]], !ID %in% c(tmp.group.1, tmp.group.2))
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]] <- subset(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), 'BASELINE', sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), 'BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), 'BASELINE', sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), 'LAGGED', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), 'LAGGED', sep=".")]], !ID %in% tmp.group.1)
					Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), 'LAGGED.BASELINE', sep=".")]] <- subset(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), 'LAGGED.BASELINE', sep=".")]], !ID %in% tmp.group.1)

					expression.to.evaluate <-
						paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=with_sgp_data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

					cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

					started.at.intermediate.2b <- proc.time()
					eval(parse(text=expression.to.evaluate))
				} ### End TEST 2d

				### TEST of SGP variable

				tmp.messages <- c(tmp.messages, paste("\t\t##### Results of testSGP test number", capwords(i), "#####\n"))

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP, na.rm=TRUE), 2896606L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP), "94cdd800a205121ca0113bcbd623985b")) { # pre-GRADE key: 0cc817e2864bc3e3bda3da9c8c8c8ab9
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

				### TEST of SGP_BASELINE variable

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP_BASELINE, na.rm=TRUE), 2906337L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP_BASELINE), "b58870bc707d3b07d157e57b0efb53d1")) { # pre-GRADE key: 6727904bc44c191e21be0dfb95a6187b
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

				### TEST of SGP_TARGET_3_YEAR variable

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP_TARGET_3_YEAR), "4cedb67edc5582c1c6f136dfb57f2b74")) { # pre-GRADE key: cfc79dbc3b55744c3a4cfa1834d2eeaf
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

				### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#				if (identical(sum(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "101811cd16d1100e23bab6a52c54a499")) { # pre-GRADE key: eef9ed11b6eed237364e6916dae06b91
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

				### TEST of CATCH_UP_KEEP_UP_STATUS_3_YEAR variable

#				if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$CATCH_UP_KEEP_UP_STATUS_3_YEAR)), c(13977, 3847, 11202, 29107))) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$CATCH_UP_KEEP_UP_STATUS_3_YEAR), "0f79344e49378c59d80900270080c0ec")) { # pre-GRADE key: cb981073080b1b8b6688338e06e1e074
					tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR, part 2: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

				### TEST of MOVE_UP_STAY_UP_STATUS variable

#				if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$MOVE_UP_STAY_UP_STATUS_3_YEAR)), c(24801, 4647, 6186, 4675))) {
				if (identical(digest(Demonstration_SGP@Data[YEAR==tail(sgpData.years, 1L)]$MOVE_UP_STAY_UP_STATUS_3_YEAR), "8d5bf02dadb02b1d009c0f7b93ba5854")) { # pre-GRADE key: 8dbe7770bd436331b81bd0a84dbaa1bb
					tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR, part 2: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR, part 2: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

				tmp.messages <- c(tmp.messages, paste0("\t##### End testSGP test number ", capwords(i), ", Part 2: ", convertTime(timetakenSGP(started.at.intermediate.2b)), " #####\n"))
				tmp.messages <- c(tmp.messages, paste0("\n##### End testSGP test number ", capwords(i), ": ", convertTime(timetakenSGP(started.at.overall.2b)), " #####\n"))
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
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
			} else parallel.config <- test.option[['parallel.config']]
			if (is.null(test.option[['use.csems.embedded.in.data']])) use.csems.embedded.in.data <- FALSE else use.csems.embedded.in.data <- TRUE

			Demonstration_SGP <- NULL
			sgpData_LONG <- SGPdata::sgpData_LONG
			tmp.messages <- ("\t##### Results of testSGP test number 3 #####\n\n")

			### Some minor modifications to SGPstateData for testing purposes

			SGPstateData[['DEMO_EOCT']][['SGP_Configuration']][['max.sgp.target.years.forward']] <- 1:7
			SGPstateData[['DEMO_EOCT']][['SGP_Configuration']][['sgPlot.sgp.targets.timeframe']] <- 3
			SGPstateData[['DEMO_EOCT']][['SGP_Configuration']][['sgp.target.scale.scores.merge']] <- "all_years_lagged_current"
			SGPstateData[['DEMO_EOCT']][['SGP_Configuration']][['return.sgp.target.num.years']] <- TRUE
			SGPstateData[['DEMO_EOCT']][['SGP_Configuration']][['round.digits']] <- 4L

			### Add EOCT courses to sgpData_LONG and CSEMs if using use.csems.embedded.in.data argument

			if (use.csems.embedded.in.data) {
				tmp.csems <- setkey(SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]], CONTENT_AREA, GRADE, SCALE_SCORE)
				sgpData_LONG[,SCALE_SCORE_CSEM:=splinefun(tmp.csems[eval(list(CONTENT_AREA[1], GRADE[1]))][[3]], tmp.csems[eval(list(CONTENT_AREA[1], GRADE[1]))][[4]], method="natural")(SCALE_SCORE), keyby=c("CONTENT_AREA", "GRADE")]
				sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="10", SCALE_SCORE_CSEM:=NA]
				SGPstateData[["DEMO_EOCT"]][["Assessment_Program_Information"]][["CSEM"]] <- "SCALE_SCORE_CSEM"
			} else {
					SGPstateData[["DEMO_EOCT"]][["Assessment_Program_Information"]][["CSEM"]] <- setkey(SGPstateData[["DEMO_EOCT"]][["Assessment_Program_Information"]][["CSEM"]][CONTENT_AREA!="ALGEBRA_II"], CONTENT_AREA, GRADE)
			}

			sgpData_LONG[CONTENT_AREA=='MATHEMATICS' & GRADE=='9', CONTENT_AREA:='ALGEBRA_I']
			sgpData_LONG[CONTENT_AREA=='MATHEMATICS' & GRADE=='10', CONTENT_AREA:='ALGEBRA_II']
			sgpData_LONG[CONTENT_AREA=='READING' & GRADE=='9', CONTENT_AREA:='GRADE_9_LIT']
			sgpData_LONG[CONTENT_AREA=='READING' & GRADE=='10', CONTENT_AREA:='AMERICAN_LIT']
			sgpData_LONG[, GRADE_REPORTED:=GRADE]
			sgpData_LONG[CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II', 'GRADE_9_LIT', 'AMERICAN_LIT'), GRADE:='EOCT']

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
				paste0("Demonstration_SGP <- abcSGP(state='DEMO_EOCT',\n\tsgp_object=sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'summarizeSGP', 'visualizeSGP'),\n\tsimulate.sgps=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

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
			if (identical(digest(Demonstration_SGP@Data$SGP), "85c4aff4ded8b8a3fbc6ce94eb7b28a1")) { # pre-GRADE key: 7c848c0bec09ae0c833ce778034ab85e
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "0e896b557023d560fdab2a8f3b525cb1")) { # pre-GRADE key: 4c73b3d3237d181b51954529edaa3c4e
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_7_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_7_YEAR, na.rm=TRUE), 2681954L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_7_YEAR), "21b59408430e50a769cf872b2e0183c1")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_7_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_7_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR), "0e6253ab172cbecd01a7c131b4e63ccd")) { # pre-GRADE key: 2eaaa1d6e0884ea2b28fa4245f8c63d1
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_5_YEAR_CURRENT variable

#			if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_5_YEAR_CURRENT, na.rm=TRUE), 3528961L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_5_YEAR_CURRENT), "ab183b483c362915f3639cffeaa636d3")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_5_YEAR_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_5_YEAR_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS_3_YEAR)), c(13977, 3847, 11202, 29107))) {
			if (identical(digest(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS_3_YEAR), "379338f44b485e2f18a4b52c16865db8")) { # pre-GRADE key: fd61668ae95e5978906bf804b5765b10
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of MOVE_UP_STAY_UP_STATUS_3_YEAR variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS_3_YEAR)), c(24801, 4647, 6186, 4675))) {
			if (identical(digest(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS_3_YEAR), "e964fdf83418aedbeacb116abf6d484b")) { # pre-GRADE key: ee8ef7f0b8370d7eeee1677cb101af8d
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(sum(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_7_YEAR_PROJ_YEAR_1, na.rm=TRUE), 35023835L)) {
			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_7_YEAR_PROJ_YEAR_1), "81a28b00e88b6a2417d57af784d0c17f")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_7_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_7_YEAR_PROJ_YEAR_1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			if (identical(sum(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_MOVE_UP_STAY_UP_7_YEAR_PROJ_YEAR_1_CURRENT, na.rm=TRUE), 29549398)) { # FAILs on 29549398L
			# if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_MOVE_UP_STAY_UP_7_YEAR_PROJ_YEAR_1_CURRENT), "20686f97ae1ca5a8d7bd65207f118851")) { # 22da22385b9657751568292777191796
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_MOVE_UP_STAY_UP_7_YEAR_PROJ_YEAR_1_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_MOVE_UP_STAY_UP_7_YEAR_PROJ_YEAR_1_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_##_YEAR_NUM_YEARS_TO_TARGET variable

#			if (identical(as.numeric(table(Demonstration_SGP@Data$SGP_TARGET_4_YEAR_CURRENT_NUM_YEARS_TO_TARGET)), c(17078, 9240, 11929, 28046))) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_4_YEAR_CURRENT_NUM_YEARS_TO_TARGET), "a4fed8e5f9f3708f5debacf0eb4d1d57")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_4_YEAR_CURRENT_NUM_YEARS_TO_TARGET: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_4_YEAR_CURRENT_NUM_YEARS_TO_TARGET: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(as.numeric(table(Demonstration_SGP@Data$SGP_TARGET_2_YEAR_NUM_YEARS_TO_TARGET)), c(15878, 10007, 32248))) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_2_YEAR_NUM_YEARS_TO_TARGET), "4992ff90a97032cae042b89e9cd15bb4")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_2_YEAR_NUM_YEARS_TO_TARGET: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_2_YEAR_NUM_YEARS_TO_TARGET: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(mean(Demonstration_SGP@Data$SGP_STANDARD_ERROR, na.rm=TRUE), 8.50872)) {
#			if (identical(digest(Demonstration_SGP@Data$SGP_STANDARD_ERROR), "cb81264f5933f0e4e22d515b5437f97f")) {
			if (identical(digest(Demonstration_SGP@Data$SGP_STANDARD_ERROR), "ba125c9bb4a62a0e76bcbac55fca913b")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_STANDARD_ERROR (Omitting ALGEBRA_II): OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_STANDARD_ERROR (Omitting ALGEBRA_II): FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 3: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
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
			if (is.null(test.option[['simex.sample.size']])) {
				simex.parameters <- "list(state='DEMO', lambda=seq(0,2,0.5), simulation.iterations=50, extrapolation='linear', save.matrices=TRUE, set.seed.for.sim.data = FALSE)"
				simex.sample.size <- FALSE
			}
			if (!is.null(test.option[['simex.sample.size']]) & calculate.simex.baseline) {
				messageSGP("The test option 'simex.sample.size' is not available with the 'calculate.simex.baseline' test. 'calculate.simex.baseline' set to FALSE.")
				calculate.simex.baseline <- FALSE
			}
			if (!is.null(test.option[['simex.sample.size']]) && !calculate.simex.baseline) {
				simex.parameters <- "list(state='DEMO', lambda=seq(0,2,0.5), simulation.iterations=50, extrapolation='linear', save.matrices=TRUE, simex.sample.size=2500, set.seed.for.sim.data = FALSE)"
				simex.sample.size <- TRUE
				messageSGP("\n\tParameter `simex.sample.size` set to 2500\n")
			}
			if (is.null(test.option[['use.csems.embedded.in.data']])) use.csems.embedded.in.data <- FALSE else use.csems.embedded.in.data <- TRUE
			if (is.null(test.option[['dependent.var.error']])) dependent.var.error <- FALSE else dependent.var.error <- TRUE
			if (dependent.var.error & !is.null(test.option[['test.updated.students']])) {
				messageSGP("The test option 'test.updated.students' is not available with the 'dependent.var.error' test. 'test.updated.students' set to NULL.")
				test.option[['test.updated.students']] <- NULL
			}

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

			if (use.csems.embedded.in.data) {
				tmp.csems <- setkey(SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]], CONTENT_AREA, GRADE, SCALE_SCORE)
				sgpData_LONG[,SCALE_SCORE_CSEM:=splinefun(tmp.csems[eval(list(CONTENT_AREA[1], GRADE[1]))][[3]], tmp.csems[eval(list(CONTENT_AREA[1], GRADE[1]))][[4]], method="natural")(SCALE_SCORE), keyby=c("CONTENT_AREA", "GRADE")]
				SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]] <- "SCALE_SCORE_CSEM"
				simex.parameters <- sub("save.matrices=TRUE", "save.matrices=TRUE, csem.data.vnames='SCALE_SCORE_CSEM'", simex.parameters)
				simex.parameters <- sub("state='DEMO', ", "", simex.parameters)
			}
			if (dependent.var.error) {
				simex.parameters <- sub("save.matrices=TRUE", "save.matrices=TRUE, dependent.var.error=TRUE", simex.parameters)
			}

			options(error=recover) # Don't use options(warn=2) - get warnings about knots and boundaries from BASELINE SIMEX
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			Demonstration_SGP <- NULL
			tmp.messages <- ("##### Results of testSGP test number 4 #####\n\n")

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				if (.Platform$OS.type != "unix") {
					parallel.config <- paste0("list(BACKEND='FOREACH', TYPE='doParallel', WORKERS=list(\n\t\tSIMEX=", number.cores, ", \n\t\tTAUS=", number.cores, "))")
				} else 	parallel.config <- paste0("list(BACKEND='PARALLEL', WORKERS=list(\n\t\tSIMEX=", number.cores, ", \n\t\tTAUS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			expression.to.evaluate <-"\nDemonstration_SGP <- prepareSGP(sgpData_LONG, create.additional.variables=FALSE)\n\n"
			if (!any(grepl('use.my.coefficient.matrices', names(test.option)))) {
				expression.to.evaluate <- paste0(expression.to.evaluate, "Demonstration_SGP <- analyzeSGP(\n\tsgp_object=Demonstration_SGP,\n\tyears='", tail(sgpData.years, 1L), "',\n\tcontent_areas='READING',\n\tsgp.percentiles.baseline.max.order=2,",
					"\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline, ",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=TRUE,",
					"\n\tcalculate.simex=", simex.parameters, ",\n\tcalculate.simex.baseline=", ifelse(calculate.simex.baseline, gsub("save.matrices=TRUE", "save.matrices=TRUE, use.cohort.for.ranking=TRUE", simex.parameters), "NULL"),",\n\tparallel.config=", parallel.config,"\n)\n")
			} else {
				expression.to.evaluate <- paste0(expression.to.evaluate, "Demonstration_SGP@SGP[['Coefficient_Matrices']] <- test.option[[grep('use.my.coefficient.matrices', names(test.option))]]\n\n",
					"Demonstration_SGP <- analyzeSGP(\n\tsgp_object=Demonstration_SGP,\n\tyears='", tail(sgpData.years, 1L), "',\n\tcontent_areas='READING',\n\tsgp.percentiles.baseline.max.order=2,",
					"\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline, ",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=TRUE,",
					"\n\tsgp.use.my.coefficient.matrices=TRUE,", # Use "Naive" Matrices for SGPs and in SIMEX too.
					"\n\tcalculate.simex=", gsub("save.matrices=TRUE", "simex.use.my.coefficient.matrices=TRUE, use.cohort.for.ranking=TRUE", simex.parameters), ",\n\tcalculate.simex.baseline=", ifelse(calculate.simex.baseline, gsub("save.matrices=TRUE", "save.matrices=TRUE, use.cohort.for.ranking=TRUE", simex.parameters), "NULL"),",",
					"\n\tparallel.config=", parallel.config,"\n)\n")
				available.matrices <- test.option[[grep('use.my.coefficient.matrices', names(test.option))]]
				available.matrices <- available.matrices[grep("[.]SIMEX", names(available.matrices))][1][[1]][[1]][[1]]
				if (!is.splineMatrix(available.matrices[[1]])) stop("\n\n\tThe test.option[['use.my.coefficient.matrices']] element must be a full @SGP[['Coefficient_Matrices']] slot from a previous run of testSGP(4)\n\te.g. testSGP(4, test.option=list(use.my.coefficient.matrices=Demonstration_SGP@SGP[['Coefficient_Matrices']]))\n")
				simex.mtx.size <- unique(sapply(length(available.matrices), function(f) available.matrices[[f]]@Version[["Matrix_Information"]][["N"]]))
				if (identical(2500L, simex.mtx.size)) simex.sample.size <- TRUE else simex.sample.size <- FALSE
			}

			cat("##### Begin testSGP test number 4, Part 1 #####", fill=TRUE)
			cat("#### Grade-Level, Cohort and Baseline Tests with auto sgp.config construction. ####\n", fill=TRUE)

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(4.1)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 4, Part 1 #####\n")

			### TEST of SGP_SIMEX variable

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, 1028656L, 1029023L))) {
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "b7a4dadefa56e7034210db02fd5acb36", "55fb02e2b05c308c3eb288474a9b3d00"))) { # pre-1.9-4.0 sgp.simex fix
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "ee04a88161ab6173420e970bf69aee23", "c7ca3a4e0795cfb2d61d85b36e135c91"))) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "97d87739f9d80a9e7861bda055c30c45", ifelse(dependent.var.error, "27176e8d237650f806c7ab62d533834c", "c7ca3a4e0795cfb2d61d85b36e135c91")))) { # post 1.9-9.1
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_SIMEX_RANKED variable

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, 1032411L, 1032498L))) {
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "84370af691ee17b803bc683d3e243f4a", "22babea1ccfdea54c6cd073bb24cf82e"))) { # pre-1.9-4.0 sgp.simex fix
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "3636c7881c5c89664e3a8e0ce96564dc", "6ad93e28790b68d5d51d83a7501a7a7c"))) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "9bc1532d4de47615c59a76a2638245dc", ifelse (dependent.var.error, "77c1ca82479f442f91e8248f0888a351", "6ad93e28790b68d5d51d83a7501a7a7c")))) { # post 1.9-9.1
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_RANKED: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_RANKED: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_SIMEX_BASELINE and SGP_SIMEX_BASELINE_RANKED variables
			if (calculate.simex.baseline) {
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE']]), 1034475L)) {
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE']]), "816099ca957a29552f4ba1fea88ae1e5")) { # pre-GRADE key: 816099ca957a29552f4ba1fea88ae1e5
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE']]), "3e259b0a8a2d287d58945bf3635f1270")) { # post 1.9-4.0
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE']]), "97825a406491a5bba42f2fd5e1c49ed7")) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_BASELINE: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), 1031773L)) { # 1031775L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "721d5eac4cb20a70f9344bbe88b8ef2b")) {
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "866a60bfb9e21d420ff566cb87c8521b")) { # post 1.9-4.0
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1L), "BASELINE", sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "43a2cf2f8bcd691087ca149eea631b66")) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_BASELINE_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_SIMEX_BASELINE_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
			}
			tmp.messages <- c(tmp.messages, paste("\n\t##### End testSGP test number 4, Part 1: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))

			sgp.config <- list(
				AMERICAN_LIT.config=list(
					sgp.content.areas=c('READING', 'GRADE_9_LIT', 'AMERICAN_LIT'),
					sgp.panel.years=tail(sgpData.years, 3),
					sgp.grade.sequences=list(c(8, 'EOCT', 'EOCT'))),
				ALGEBRA_II.config=list(
					sgp.content.areas=c('MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
					sgp.panel.years=tail(sgpData.years, 3),
					sgp.grade.sequences=list(c(8, 'EOCT', 'EOCT')))
			)

			if (!any(grepl('use.my.coefficient.matrices', names(test.option)))) {
				expression.to.evaluate <-
					paste0("\nDemonstration_SGP <- analyzeSGP(\n\tsgp_object=Demonstration_SGP,\n\tsgp.config=sgp.config,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline, ",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,",
								 "\n\tcalculate.simex=", simex.parameters, ",\n\tcalculate.simex.baseline=", ifelse(calculate.simex.baseline, gsub("save.matrices=TRUE", "save.matrices=TRUE, use.cohort.for.ranking=TRUE", simex.parameters), "NULL"),",\n\tparallel.config=", parallel.config, "\n)\n",
								 "\nDemonstration_SGP <- combineSGP(Demonstration_SGP)\n\n")
			} else {
				expression.to.evaluate <-
					paste0("\nDemonstration_SGP <- analyzeSGP(\n\tsgp_object=Demonstration_SGP,\n\tsgp.config=sgp.config,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline, ",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,",
								 "\n\tsgp.use.my.coefficient.matrices=TRUE,", # Use "Naive" Matrices for SGPs and in SIMEX too.
								 "\n\tcalculate.simex=", gsub("save.matrices=TRUE", "simex.use.my.coefficient.matrices=TRUE, use.cohort.for.ranking=TRUE", simex.parameters), ",\n\tcalculate.simex.baseline=", ifelse(calculate.simex.baseline, gsub("save.matrices=TRUE", "save.matrices=TRUE, use.cohort.for.ranking=TRUE", simex.parameters), "NULL"),",\n\tparallel.config=", parallel.config, "\n)\n",
								 "\nDemonstration_SGP <- combineSGP(Demonstration_SGP)\n\n")
			}

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat("\n##### Begin testSGP test number 4, Part 2 #####", fill=TRUE)
			cat("#### EOCT Baseline Tests with custom sgp.config. ####\n", fill=TRUE)

			cat(paste0("EVALUATING:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(4.2)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of SGP_SIMEX and SGP_SIMEX_BASELINE variables

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 4, Part 2 #####\n")

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, 211609L, 211555L))) {
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "df4cc817b52d2fcbbe6f7addd1b4e2f3", "a351d68d993a6ae42714142afd1ea6d3"))) { # pre-1.9-4.0 sgp.simex fix
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "2f881955c5e24766f8e1ab2df6eff6b5", ifelse(dependent.var.error, "f8580018874717543d573cb11d779ac7", "1fc0b74894a50300a307ff1ac7f300eb")))) { # "47474057b07a5cead92b9d184bee6237"
				tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, 211814L, 211864L))) {
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "a7270bc07d7ce6eca7055ce1d1fd91d6", "af4aa783fc81ba1c81ad758a5c92efff"))) { # pre-1.9-4.0 sgp.simex fix
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "b73341834f894dd8632b50b26df2d5c6", "241e933dbcfbf3a2d0d6f536272b7bad"))) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "46dfd4d5a87bed7d6f3398c54277d005", ifelse(dependent.var.error, "1dfc935d020386ec2bc39bafeeeeb384", "241e933dbcfbf3a2d0d6f536272b7bad")))) { # "b73341834f894dd8632b50b26df2d5c6"
				tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_RANKED: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_RANKED: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, 212639L, 212383L))) {
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "89446b39fa32bbe91d435081a9aa41e6", "ab2862894723e1d9533a1499195dca98"))) { # pre-1.9-4.0 sgp.simex fix
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX']]), ifelse(simex.sample.size, "d581cb1cdd22a53421659989ffbfaffa", ifelse(dependent.var.error, "1b49b1d56a93264443e5ab0036054568", "d744baf8de99087a252940f9d1fe0348")))) { # "78a18a4a82516b5156f55061d3d0d3e8",
				tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, 213318L, 213361L))) {
#			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "8a01548a0d3489660cfc9f4ce66ceb98", "152b1048bb2d191a1ed9244fde160d2d"))) { # pre-1.9-4.0 sgp.simex fix
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "df49ff250072bd8d1e6764dde8c2624c", ifelse(dependent.var.error, "a525ed4fdc64d3af1d35ca4a1689e69c", "a6dadc01f6368657dcd49c2437725ccd")))) { # "00f6f613d386740e32999cc38a06d26d"
				tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_RANKED: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_RANKED: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX, na.rm=TRUE), ifelse(simex.sample.size, 1454082L, 1452961L))) { #  , 1452904L,
#			if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX), ifelse(simex.sample.size, "65e6aaf41e6e8f1f5a3a394cbcac9e0f", "928e4428e146a3486a5c8eede3c92ab9"))) { # pre-1.9-4.0 sgp.simex fix
#			if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX), ifelse(simex.sample.size, "8571479f09e79517d3ed95ca938b4524", "02acad263034242149b6604812b44b20"))) {
			if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX), ifelse(simex.sample.size, "77896592f239eea13340a32ad096bf4e", ifelse(dependent.var.error, "c11843c1f1a38f18d3d1cd658be4dd43", "02acad263034242149b6604812b44b20")))) {
				tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

#			if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_RANKED, na.rm=TRUE), ifelse(simex.sample.size, 1457831L, 1457723L))) { # ,  1457543L,
#			if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_RANKED), ifelse(simex.sample.size, "d29e7c9a6ef3f21dff1a534550d667ab", "421e5f144da90b8ac8f04c4616370e73"))) { # pre-1.9-4.0 sgp.simex fix
			if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_RANKED), ifelse(simex.sample.size, "391f0d1523aaba19d85f733dc7818d81", ifelse(dependent.var.error, "26c943a456f8e44fcc3bd61076a59be5", "f90f641444e545398cbccf1a2bc648d2")))) { #  "6e9bb077ba1de7d9e1e08c4b37af6aba"
				tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_RANKED: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_RANKED: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_SIMEX and SGP_SIMEX_BASELINE variable

			if (calculate.simex.baseline) {
				tmp.matrix.names <- c("MATHEMATICS.BASELINE", "READING.BASELINE", "GRADE_9_LIT.BASELINE", "AMERICAN_LIT.BASELINE", "ALGEBRA_I.BASELINE", "ALGEBRA_II.BASELINE",
					"MATHEMATICS.BASELINE.SIMEX", "READING.BASELINE.SIMEX", "GRADE_9_LIT.BASELINE.SIMEX", "AMERICAN_LIT.BASELINE.SIMEX", "ALGEBRA_I.BASELINE.SIMEX", "ALGEBRA_II.BASELINE.SIMEX",
					"READING.2015_2016", "READING.2015_2016.SIMEX", "ALGEBRA_II.2015_2016", "ALGEBRA_II.2015_2016.SIMEX", "AMERICAN_LIT.2015_2016", "AMERICAN_LIT.2015_2016.SIMEX")
				if (identical(sort(names(Demonstration_SGP@SGP$Coefficient_Matrices)), sort(gsub("2015_2016", tail(sgpData.years, 1L), tmp.matrix.names)))) {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
			} else {
				tmp.matrix.names <- c("READING.2015_2016", "READING.2015_2016.SIMEX", "ALGEBRA_II.2015_2016", "ALGEBRA_II.2015_2016.SIMEX", "AMERICAN_LIT.2015_2016", "AMERICAN_LIT.2015_2016.SIMEX")
				if (identical(sort(names(Demonstration_SGP@SGP$Coefficient_Matrices)), sort(gsub("2015_2016", tail(sgpData.years, 1L), tmp.matrix.names)))) {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @SGP Coefficient Matrix Names: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
			}

			if (calculate.simex.baseline) {
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), 217614L)) { # 217894L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "1dbd929906efe4a131c68180d83c4164")) { # pre-GRADE key: 73176856b45b8bc490b5e98068319411
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "af1033d6a7cdf0ba65f8e0d08bf67c6d")) { # post 1.9-4.0
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "ce1c7d0cf5a20cb67593d9ecb2577a63")) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), 211843L)) { # 211837L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "f3e9a7c0e1eb4564ddc04d5b38f5223e")) { # xXx 40cfc13e853dfa9af8398ca38fea96e8
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "75542c44a8463b3bb14aa2611e83cdd7")) { # post 1.9-4.0
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "dbd9e77bd9050b6b096eaffb725d81b1")) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), 213472L)) { # 212818L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "e9ac73391f257c5d21df89c2ca6d5283")) { # pre-GRADE key: d4fe0d79e985aacb6331ce95f4a2d01e
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "49fe29a83e7f22f4bb6ee4e3538f5bff")) { # post 1.9-4.0
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE']]), "9e6012824c2525c633e296e644967b15")) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_BASELINE: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), 213473L)) { # 213377L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "9fb272b324afab3f9aeb7107c277ac23")) { # xXx 5df2787d9bed17d59004a9eeb76930a6
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "581eb5580530038fb03f352c731d4930")) { # post 1.9-4.0
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1L), 'BASELINE', sep=".")]][['SGP_SIMEX_BASELINE_RANKED']]), "dbfa602f675d9b460a2f414f43b5c526")) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_BASELINE_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II SGP_SIMEX_BASELINE_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
#				if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_BASELINE, na.rm=TRUE), 1465473L)) { # 1465187L
#				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE), "010289791f188481d48577bf3f247c2e")) { # pre-GRADE key: 3e653f546bd93da33cc915b5d37100a4  xXx  61ff473b2777c2b7ea14a2c0bcb662ec
#				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE), "a193b6fe6b26f8fcd221c945081bbf3b")) {# post 1.9-4.0
				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE), "44be364974deff2403f4b78f2e7e98d2")) {# post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_BASELINE: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_BASELINE: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
#				if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_BASELINE_RANKED, na.rm=TRUE), 1457089L)) { # 1456989L
#				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE_RANKED), "0acbb6fb922e87c54bc2aa5acf55ba73")) {#  xXx  3818ece7a3a5f5e6e7a0ff343c8a752a
#				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE_RANKED), "ed623b7000d943acbc2fd524f7ce4efc")) {# post 1.9-4.0
				if (identical(digest(Demonstration_SGP@Data$SGP_SIMEX_BASELINE_RANKED), "b7adcbf2d1b4f69b5475a84986313e0c")) {# post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_BASELINE_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data variable SGP_SIMEX_BASELINE_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
			}
			tmp.messages <- c(tmp.messages, paste("\n\t##### End testSGP test number 4, Part 2: ", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))

			if (!is.null(test.option[['test.updated.students']])) {
				#  Subset and remove a random sample of students
				set.seed(719)
				updated.student.ids <- sgpData_LONG[VALID_CASE=="VALID_CASE" & YEAR==tail(sgpData.years, 1) & CONTENT_AREA %in% c("READING", "AMERICAN_LIT", "ALGEBRA_II")][, list(ID = sample(unique(ID), 100)), by=c("CONTENT_AREA", "GRADE")][GRADE != "3"]

				sgp.key <- key(Demonstration_SGP@Data)
				setkeyv(updated.student.ids, c("CONTENT_AREA", "GRADE", "ID"))
				setkeyv(Demonstration_SGP@Data, c("CONTENT_AREA", "GRADE", "ID"))
				Demonstration_Data_LONG_Updated <- data.table(Demonstration_SGP@Data[updated.student.ids, nomatch=0], key=sgp.key) # nomatch=0 not needed?
				# Demonstration_Data_LONG_Updated[, grep("SGP|PRIOR|PERCENTILE", names(Demonstration_Data_LONG_Updated), value=TRUE) := NULL]
				invisible(Demonstration_Data_LONG_Updated[, grep("PERCENTILE", names(Demonstration_Data_LONG_Updated), value=TRUE) := NULL])
				setnames(Demonstration_Data_LONG_Updated, gsub("^SGP", "ORIG", names(Demonstration_Data_LONG_Updated)))
				Demonstration_SGP@Data <- data.table(Demonstration_SGP@Data[!updated.student.ids], key=sgp.key)

				for (ca in names(Demonstration_SGP@SGP[["SGPercentiles"]])) {
					Demonstration_SGP@SGP[["SGPercentiles"]][[ca]]$CONTENT_AREA <- strsplit(ca, "[.]")[[1]][1]
					setkeyv(Demonstration_SGP@SGP[["SGPercentiles"]][[ca]], c("CONTENT_AREA", "GRADE", "ID"))
					Demonstration_SGP@SGP[["SGPercentiles"]][[ca]] <- Demonstration_SGP@SGP[["SGPercentiles"]][[ca]][!updated.student.ids]
					Demonstration_SGP@SGP[["SGPercentiles"]][[ca]][, CONTENT_AREA := NULL]
				}

				#  Perturb SCALE_SCORE of students to update
				set.seed(589)
				invisible(Demonstration_Data_LONG_Updated[, SCALE_SCORE := SCALE_SCORE - round(runif(nrow(Demonstration_Data_LONG_Updated), -10, 10), 0)])

				#  Add READING to sgp.config
				sgp.config <- c(sgp.config, list(
				  READING.config=list(
				    sgp.content.areas=rep('READING', 3),
				    sgp.panel.years=tail(sgpData.years, 3),
				    sgp.grade.sequences=list(c('3', '4'), c('3', '4', '5'), c( '4', '5', '6'), c('5', '6', '7'), c('6', '7', '8'))))
				)

				expression.to.evaluate <-
					paste0("\nDemonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_Updated,",
								 "\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'), \n\tsgp.config=sgp.config,",
								 "\n\toverwrite.existing.data=FALSE,\n\tupdate.old.data.with.new=TRUE,",
								 "\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=", calculate.simex.baseline, ",\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,",
								 "\n\tsgp.use.my.coefficient.matrices=TRUE,", # Use "Naive" Matrices for SGPs and in SIMEX too.
								 "\n\tcalculate.simex=", gsub("save.matrices=TRUE", "\n\t\tsimex.use.my.coefficient.matrices=TRUE, use.cohort.for.ranking=FALSE\n\t\t", simex.parameters), ",\n\tcalculate.simex.baseline=", ifelse(calculate.simex.baseline, gsub(", simex.sample.size=2500", "", simex.parameters), "NULL"), ",",
								 "\n\tsave.intermediate.results=FALSE,\n\tparallel.config=", parallel.config, "\n)\n")

				if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

				cat(paste0("EVALUATING test number 4, Part 3:\n", expression.to.evaluate), fill=TRUE)

				if (memory.profile) {
					Rprof("testSGP(4)_Memory_Profile.out", memory.profiling=TRUE)
				}

				started.at.intermediate3 <- proc.time()
				eval(parse(text=expression.to.evaluate))

				if (memory.profile) {
					Rprof(NULL)
				}

				tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 4: Part 3 #####\n")

#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, 1032437L, 1032384L))) { # 1032372L, 1032465L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "af0bd14d3e6c830ecaf638fcafc57bb6", "a316661bb39fa58a0daa432b6a7cda8f"))) {
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "6b020293b8c97213100ec4be1f95e7f0", "2007e2e95b4ed64d2963f527c0c605e5"))) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of READING Updated SGP_SIMEX_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of READING Updated SGP_SIMEX_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, 211887L, 211939L))) { 211710L, 212048L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "b7a29a91fb2e4a0d9555d530fd55cc0a", "b1efeb656dd1cce6cf753d7aaf85f3cc"))) {
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('AMERICAN_LIT', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "6e03644e18eb014551180ad1e66383a5", "d738e8734a2897bd4787b3d7542932b4"))) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT Updated SGP_SIMEX_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of AMERICAN_LIT Updated SGP_SIMEX_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

#				if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, 213390L, 213420L))) { # 213300L, 213327L
#				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "2cfbc31e5801ac1006c933be5362657f", "d8c707ff51d892232a203960254553c3"))) {
				if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('ALGEBRA_II', tail(sgpData.years, 1), sep=".")]][['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "19eec3b4e8cc13409243a0f9c0598658", "3a49de0f8d4dd7254669726c39b7b42b"))) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II Updated SGP_SIMEX_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of ALGEBRA_II Updated SGP_SIMEX_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

#				if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX, na.rm=TRUE), ifelse(simex.sample.size, 1453987L, 1453850L))) { # 1453181L, 1453991L
#				if (identical(digest(Demonstration_SGP@Data[['SGP_SIMEX']]), ifelse(simex.sample.size, "9c490a7cfa61902b0f7ee733ef1decc9", "dd02dc6c4711840c2a8ba5b152c0a864"))) {
				if (identical(digest(Demonstration_SGP@Data[['SGP_SIMEX']]), ifelse(simex.sample.size, "a7e182e55fe9c2fdff7f591be8948ef3", "35671327de945e37fabb71f63362855d"))) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data UPDATED variable SGP_SIMEX: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data UPDATED variable SGP_SIMEX: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}

#				if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_RANKED, na.rm=TRUE), ifelse(simex.sample.size, 1457714L, 1457743L))) { # 1457549L, 1457840L
#				if (identical(digest(Demonstration_SGP@Data[['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "e07fd1aff19438dbbc30e42bdab7557a", "bbb3c63a095d9638acd45eff321e0862"))) {
				if (identical(digest(Demonstration_SGP@Data[['SGP_SIMEX_RANKED']]), ifelse(simex.sample.size, "f97983548a96fd0d8698f1f86118787c", "dbbf615f55f92023f3b89e4ad1f490b8"))) { # post 1.9-9.1
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data UPDATED variable SGP_SIMEX_RANKED: OK\n")
				} else {
					tmp.messages <- c(tmp.messages, "\t\tTest of @Data UPDATED variable SGP_SIMEX_RANKED: FAIL\n")
					if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
				}
				tmp.messages <- c(tmp.messages, paste("\n\t##### End testSGP test number 4, Part 3: ", convertTime(timetakenSGP(started.at.intermediate3)), "#####\n"))
			} ###  End 'test.updated.students'

			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 4: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
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
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
			} else parallel.config <- test.option[['parallel.config']]
			Demonstration_SGP <- ACHIEVEMENT_LEVEL <- HIGH_NEED_STATUS <- NULL
			tmp.messages <- ("##### Results of testSGP test number 5 #####\n\n")
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
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG_STEP_1,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'visualizeSGP'),\n\tplot.types=c('studentGrowthPlot', 'growthAchievementPlot'),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\toutputSGP.output.type='LONG_FINAL_YEAR_Data',\n\tparallel.config=", parallel.config, "\n)\n")

			cat(paste0("EVALUATING Test Number 5, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(5)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 5: Part 1 #####\n")

			### TEST of SGP variable

			# if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 2810958L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "456b3be4f8af5b830989c8a54a5437ee")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_BASELINE variable

			# if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 2844420L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "5374d1bd032bb60a7e1ea2bcccfbf830")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR variable

			# if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 2548595L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "e8ae9f4534037744b753ae88229e1c21")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_PRIOR_STANDARDIZED variable

			# if (identical(median(Demonstration_SGP@Data$SCALE_SCORE_PRIOR_STANDARDIZED, na.rm=TRUE), 0.059)) {
			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_PRIOR_STANDARDIZED), "bfeaa95c8c19a61d6b69228929e09810")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR_STANDARDIZED, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR_STANDARDIZED, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 5, Part 1: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


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
				paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_STEP_2,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=TRUE,\n\tsgp.projections.lagged=TRUE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.percentiles.equated=TRUE,\n\tsgp.percentiles.equating.method=c('identity', 'mean', 'linear', 'equipercentile'),\n\tsave.intermediate.results=FALSE,\n\toutputSGP.output.type='LONG_FINAL_YEAR_Data',\n\tparallel.config=", parallel.config, "\n)\n")

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

			# if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "3ba0ed9a36804c4e05b214c4001a28f6")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP variable from equated analyses

			# if (identical(sum(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', rev(sgpData.years)[2], 'EQUATED', sep=".")]][['SGP_EQUATED']], na.rm=TRUE), 1418003L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGPercentiles']][[paste('READING', rev(sgpData.years)[2], 'EQUATED', sep=".")]][['SGP_EQUATED']]), "b1ea9b92fa692cec135c18c5ba979a42")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_EQUATED from READING equated analysis, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_EQUATED from READING equated analysis, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_EQUATED variable

			# if (identical(as.integer(sum(Demonstration_SGP@Data$SCALE_SCORE_EQUATED, na.rm=TRUE)), 796049037L)) {
			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_EQUATED), "67502e263ddefce6eb63b348f3cd9b2f")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_EQUATED, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_EQUATED, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable

			# if (identical(as.integer(sum(Demonstration_SGP@SGP$SGProjections[[paste('READING', rev(sgpData.years)[2], 'LAGGED.TARGET_SCALE_SCORES', sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']])), 82327526L)) {
			if (identical(digest(Demonstration_SGP@SGP$SGProjections[[paste('READING', rev(sgpData.years)[2], 'LAGGED.TARGET_SCALE_SCORES', sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']]), "73dbceab534c213e36cee5c75804588d")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 5, Part 2: ", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))


			##############################################################################
			##### STEP 3: Create SGPs for year after assessment transition year
			##############################################################################

			##### Modify SGPstateData

			SGPstateData[["DEMO"]][["Student_Report_Information"]][["Earliest_Year_Reported"]] <- test.option[['Earliest_Year_Reported']]
			SGPstateData[["DEMO"]][['SGP_Configuration']][['sgPlot.plot.test.transition']] <- test.option[['sgPlot.plot.test.transition']]

			### updateSGP

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_STEP_3,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=TRUE,\n\tsgp.projections.lagged=TRUE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsave.intermediate.results=FALSE,\n\toutputSGP.output.type=c('LONG_Data', 'LONG_FINAL_YEAR_Data', 'WIDE_Data', 'INSTRUCTOR_Data'),\n\tparallel.config=", parallel.config, "\n)\n")

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

			# if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
			if (identical(digest(Demonstration_SGP@Data$SGP), "89f1cb732587f3d6ffeb1afe587941e0")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of LEVEL_1_SGP_TARGET_YEAR_1_CURRENT variable

			# if (identical(as.integer(sum(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['LEVEL_1_SGP_TARGET_YEAR_1_CURRENT']], na.rm=TRUE)), 987731L)) {
			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), sep=".")]][['LEVEL_1_SGP_TARGET_YEAR_1_CURRENT']]), "003cce734efc7ca41d7f726d572229bd")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1_CURRENT, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1_CURRENT, part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of ACHIEVEMENT_LEVEL_PRIOR Variable in MATHEMATICS.XXXX_XXXX.LAGGED projections

			# if (identical(as.vector(table(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]][['ACHIEVEMENT_LEVEL_PRIOR']])), c(8178L, 4316L, 5991L, 7552L, 3145L))) {
			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]][['ACHIEVEMENT_LEVEL_PRIOR']]), "e07ef3e5e79828e925ea0e2a5b5304f5")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable ACHIEVEMENT_LEVEL_PRIOR, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable ACHIEVEMENT_LEVEL_PRIOR, part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_FROM_2015_2016 Variable in @Data

			# if (identical(sum(Demonstration_SGP@Data[[paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_")]], na.rm=TRUE), 2900310L)) {
			if (identical(digest(Demonstration_SGP@Data[[paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_")]]), "0f6b5e959c6601981cd28f6348295596")) {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ", paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_"), ", part 3: OK\n"))
			} else {
				tmp.messages <- c(tmp.messages, paste0("\t\tTest of variable ", paste('SGP_FROM', tail(sgpData.years, 2)[1], sep="_"), ", part 3: FAIL\n"))
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 5, Part 3: ", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 5: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
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
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", \n\t\tBASELINE_MATRICES=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]
			Demonstration_SGP <- tmp.messages <- NULL

			### Modify SGPstateData

			SGPstateData[['DEMO']][['Baseline_splineMatrix']] <- NULL

			expression.to.evaluate <-
				paste0("\nDemonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='", tail(sgpData.years, 1L), "',\n\tcontent_areas='MATHEMATICS',\n\tsgp.percentiles=FALSE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=TRUE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tparallel.config=", parallel.config, "\n)\n")

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
			if (identical(digest(Demonstration_SGP@Data$SGP_BASELINE), "a1c203d8d16671a1047188761afc25e2")) { # pre-GRADE key: 49a36274c536fe13ac2bcdc1c49cadc9
				tmp.messages <- c(tmp.messages, "\t\tTest of SGP_BASELINE: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of SGP_BASELINE: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 6: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 6


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI1: Test of SGPt functionality with STAR scores
		###
		#######################################################################################################################################################

		if ("RLI1" %in% TEST_NUMBER) {

			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			RLI1_SGPt_PART_1 <- RLI1_SGPt_PART_2 <- RLI1_SGPt_PART_3 <- SCALE_SCORE_RASCH <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI1 (STAR Scores) #####\n\n"
			tmp.last.window <- tail(sort(unique(SGPdata::sgptData_LONG[['YEAR']])), 1L)

			###############################################################################
			### PART 1: Using RLI_SGPt_UPDATE_SHELL
			###############################################################################

			RLI_Data_LONG_UPDATE <- SGPdata::sgptData_LONG[YEAR %in% tmp.last.window][,SCALE_SCORE_RASCH:=NULL]
			SGPstateData[["RLI"]][["SGP_Configuration"]][["fix.duplicates"]] <- "KEEP.ALL"
			RLI_SGPt_UPDATE_SHELL <- suppressMessages(prepareSGP(SGPdata::sgptData_LONG[!YEAR %in% tmp.last.window][,SCALE_SCORE_RASCH:=NULL], state="RLI"))
			SGPstateData[["RLI"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <-
				eval(parse(text=paste("RLI_SGPt_Baseline_Matrices[['RLI_SGPt_Baseline_Matrices_2016_2017.3']]")))
			SGPstateData[["RLI"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50
			SGPstateData[["RLI"]][["Assessment_Program_Information"]][["CSEM"]] <- "SEM"
			RLI_Cutscores <- SGPstateData[['RLI']][['SGP_Configuration']][['testSGP.cutscores']][['STAR']]

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI1_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_SGPt_UPDATE_SHELL,\n\tadditional.data=RLI_Data_LONG_UPDATE,\n\ttesting.window='SPRING',\n\teow.or.update='UPDATE',\n\tconfiguration.year='2016_2017',\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='STAR',\n\tsimulate.sgps=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI1_SGPt_PART_1, file='Data/RLI1_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI1 (STAR Scores), Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI1_PART_1")) unlink("Data/RLI1_PART_1", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI1_PART_1")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number RLI1 (STAR Scale): Part 1 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI1_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 110397L)) { ### Pre RLI1 107188L, 108675L
			if (identical(digest(RLI1_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "d11fe88a76e9ef09500aa94f24d08aaf")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_BASELINE_10_TIME_CURRENT variable from READING

#			if (identical(sum(RLI1_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']], na.rm=TRUE), 51019L)) { ## 2015 was 45054L, 2016 was 50186L, RLI1 was 52105,  SGP 1.6-4.16 :: 52105
			if (identical(digest(RLI1_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']]), "7197a0d493d624da4c90be7e6d5ca127")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI1_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 551654.4)) { ### RLI1 543598.8, 533302
			if (identical(digest(RLI1_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]), "473d9e900cb77658f330a09fc184d03e")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGPercentiles output: Dimension

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI1_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			} else tmp.data <- fread(paste("Data/RLI1_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(2212L, 10L))) {  # preSGP 1.6-4.16 +Dups: 2179L, 10L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGPercentiles output: Length of SGP_NORM_GROUP_BASELINE,

			my.tmp.1 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE,perl=TRUE), function(x) length(x))
			my.tmp.2 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_SCALE_SCORES,perl=TRUE), function(x) length(x))
			my.tmp.3 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_DATES,perl=TRUE), function(x) length(x))

			if (identical(my.tmp.1, my.tmp.2) & identical(my.tmp.2, my.tmp.3)) { # SGP 1.6-4.16 +Dups :: TRUE
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGProjections output

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI1_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			} else tmp.data <- fread(paste("Data/RLI1_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(996L, 1038L))) { ### pre-RLI1 c(981L, 118L)  ### preSGP 1.6-4.16 +Dups :: 981L, 1038L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGProjections TARGETS output

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI1_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI1_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))


			if (identical(dim(tmp.data), c(914L, 25L))) { ### 2014-2015 was 867L, pre RLI1 was c(819L, 25L) ### preSGP 1.6-4.16 +Dups :: 900L 25L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI1 (STAR Scores), Part 1: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 2: Using LONG Data
			###############################################################################

			RLI_SGPt_Data_LONG <- copy(SGPdata::sgptData_LONG)[,SCALE_SCORE_RASCH:=NULL]

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI1_SGPt_PART_2 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='STAR',\n\tsimulate.sgps=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI1_SGPt_PART_2, file='Data/RLI1_SGPt_PART_2.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI1_PART_2")) unlink("Data/RLI1_PART_2", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI1_PART_2")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI1 (STAR Scores): Part 2 #####\n")

			### TEST of equality between RLI_SGPt_PART_1@SGP and RLI_SGPt_PART_2@SGP

			if (identical(RLI1_SGPt_PART_1@SGP, RLI1_SGPt_PART_2@SGP)) { #
				tmp.messages <- c(tmp.messages, "\t\tTest of equality of RLI1_PART_1@SGP and RLI1_PART_2@SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of equality of RLI1_PART_1@SGP and RLI1_PART_2@SGP, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI1 (STAR Scores): Part 2", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))


			###############################################################################
			### PART 3: Using Matrices from Next Window
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI1_SGPt_PART_3 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\tcoefficient.matrices=RLI_SGPt_Baseline_Matrices$RLI_SGPt_Baseline_Matrices_2016_2017.2,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='STAR',\n\tsimulate.sgps=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI1_SGPt_PART_3, file='Data/RLI1_SGPt_PART_3.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI1 (STAR Scores), Part 3:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_3.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI1_PART_3")) unlink("Data/RLI1_PART_3", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI1_PART_3")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI1 (STAR Scores): Part 3 #####\n")

			### TEST of SGP variable from READING (equality with PART 1)

#			if (identical(sum(RLI1_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 110397L)) { ## Pre-RLI1 107188L preSGP 1.6-4.16 +Dups : 108675L
			if (identical(digest(RLI1_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "d11fe88a76e9ef09500aa94f24d08aaf")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2016_2017.3 matrices = 2016_2017.2 matrices), part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2016_2017.3 matrices = 2016_2017.2 matrices), part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI (STAR Scores): Part 3", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number RLI (STAR Scores): ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER RLI1


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI2: Test of SGPt functionality with RASCH scores
		###
		#######################################################################################################################################################

		if ("RLI2" %in% TEST_NUMBER) {

			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			RLI2_SGPt_PART_1 <- RLI2_SGPt_PART_2 <- RLI2_SGPt_PART_3 <- SCALE_SCORE <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI2 (RASCH Scores) #####\n\n"
			tmp.last.window <- tail(sort(unique(SGPdata::sgptData_LONG[['YEAR']])), 1L)

			###############################################################################
			### PART 1: Using RLI_SGPt_UPDATE_SHELL
			###############################################################################

			RLI_Data_LONG <- copy(SGPdata::sgptData_LONG)
			RLI_Data_LONG[,SCALE_SCORE:=NULL]
			setnames(RLI_Data_LONG, "SCALE_SCORE_RASCH", "SCALE_SCORE")
			SGPstateData[["RLI"]][["SGP_Configuration"]][["fix.duplicates"]] <- "KEEP.ALL"
			RLI_SGPt_UPDATE_SHELL <- suppressMessages(prepareSGP(RLI_Data_LONG[!YEAR %in% tmp.last.window], state="RLI"))
			RLI_Data_LONG_UPDATE <- RLI_Data_LONG[YEAR %in% tmp.last.window]
			SGPstateData[["RLI"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <-
				eval(parse(text=paste("RLI_SGPt_Baseline_Matrices[['RLI_SGPt_Baseline_Matrices_2016_2017.3']]")))
			SGPstateData[["RLI"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50
			RLI_Cutscores <- SGPstateData[['RLI']][['SGP_Configuration']][['testSGP.cutscores']][['RASCH']]

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI2_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_SGPt_UPDATE_SHELL,\n\tadditional.data=RLI_Data_LONG_UPDATE,\n\ttesting.window='SPRING',\n\teow.or.update='UPDATE',\n\tconfiguration.year='2016_2017',\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI2_SGPt_PART_1, file='Data/RLI2_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI2 (RASCH Scores), Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI2_PART_1")) unlink("Data/RLI2_PART_1", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI2_PART_1")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number RLI2 (RASCH Scores): Part 1 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI2_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 109949L)) { #107694L SGPdata 15.0, preSGP 1.6-4.16 +Dups 107696L, 109568L +Dups
			if (identical(digest(RLI2_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "2d11719cc20456977939b677eb14f393")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE for READING_RASCH, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE for READING_RASCH, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP variable from READING_UNIFIED

#			if (identical(sum(RLI2_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_UNIFIED_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 19726L))
			if (identical(digest(RLI2_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "2d11719cc20456977939b677eb14f393")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE for READING_UNIFIED_RASCH, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE for READING_UNIFIED_RASCH, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_BASELINE_10_TIME_CURRENT variable from READING

#			if (identical(sum(RLI2_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']], na.rm=TRUE), 51796L)) { ## 2015 was 45054L, 2016 was 50186L, preRLI was 51714L, SGP pre 1.6-4.16 + Dups 50160L, 52462L +DUPS,49937L 1.9-0.0
			if (identical(digest(RLI2_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']]), "6438ea4692055a3c6e4e52d2af06658e")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI2_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 1099.79)) { ## 1085.258 SGP 15.0, SGP pre1.6-4.16 +Dups 1085.253, +Dups 1098.585
			if (identical(digest(RLI2_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]), "79e5dda6077771d655ab8dc7653b3303")) { ## SGP 1.6-4.16 +Dups :: 856cf4c1dd2f2aca521d0d6b4e61ce2b / 1098.585 sum; SGP 1.9-0.0 "06656be9a40eaded9069370b345fcd50"
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGPercentiles output: Dimension

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI2_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI2_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(2212L, 10L))) {  ## preSGP 1.6-4.16 +Dups : 2179L, 10L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGPercentiles output: Length of SGP_NORM_GROUP_BASELINE,

			my.tmp.1 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE,perl=TRUE), function(x) length(x))
			my.tmp.2 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_SCALE_SCORES,perl=TRUE), function(x) length(x))
			my.tmp.3 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_DATES,perl=TRUE), function(x) length(x))

			if (identical(my.tmp.1, my.tmp.2) & identical(my.tmp.2, my.tmp.3)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGProjections output

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI2_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI2_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(996L, 1038L))) { ### preRLI2 c(981L, 118L), preSGP 1.6-4.16 +Dups: 981L, 1038L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGProjections TARGETS output

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI2_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI2_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))

			if (identical(dim(tmp.data), c(913L, 25L))) { ### 2014-2015 was 867L, preRLI2 was 819L, preSGP 1.6-4.16 +Dups: 900L, 25L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI2 (RASCH Scores), Part 1: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 2: Using LONG Data
			###############################################################################

			RLI_SGPt_Data_LONG <- copy(SGPdata::sgptData_LONG)
			RLI_SGPt_Data_LONG[,SCALE_SCORE:=NULL]
			setnames(RLI_SGPt_Data_LONG, "SCALE_SCORE_RASCH", "SCALE_SCORE")

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI2_SGPt_PART_2 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI2_SGPt_PART_2, file='Data/RLI2_SGPt_PART_2.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI2 (RASCH Scores), Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI2_PART_2")) unlink("Data/RLI2_PART_2", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI2_PART_2")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI2 (RASCH Scores): Part 2 #####\n")

			### TEST of equality between RLI_SGPt_PART_1@SGP and RLI_SGPt_PART_2@SGP

			if (identical(RLI2_SGPt_PART_1@SGP, RLI2_SGPt_PART_2@SGP)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI2_PART_1@SGP and RLI2_PART_2@SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI2_PART_1@SGP and RLI2_PART_2@SGP, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI2 (RASCH Scores): Part 2", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))


			###############################################################################
			### PART 3: Using Matrices from Next Window
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI2_SGPt_PART_3 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\tcoefficient.matrices=RLI_SGPt_Baseline_Matrices$RLI_SGPt_Baseline_Matrices_2017_2018.1,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI2_SGPt_PART_3, file='Data/RLI2_SGPt_PART_3.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI2 (RASCH Scores), Part 3:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_3.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI2_PART_3")) unlink("Data/RLI2_PART_3", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI2_PART_3")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI2 (RASCH Scores): Part 3 #####\n")

			### TEST of SGP variable from READING (equality with PART 1)

#			if (identical(sum(RLI2_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 109949L)) { ## 107694L SGP 15.0, ##preSGP 1.6-4.16 +Dups 107696L, +Dups 109568L
			if (identical(digest(RLI2_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "2d11719cc20456977939b677eb14f393")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2016_2017.3 matrices = 2017_2018.1 matrices), part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2016_2017.3 matrices = 2017_2018.1 matrices), part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI2 (RASCH Scores): Part 3", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number RLI2 (RASCH Scores): ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER RLI2


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI3: Test of SGPt functionality with UNIQUE RASCH scores
		###
		#######################################################################################################################################################

		if ("RLI3" %in% TEST_NUMBER) {

			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			RLI3_SGPt_PART_1 <- RLI3_SGPt_PART_2 <- RLI3_SGPt_PART_3 <- SCALE_SCORE <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI3 (RASCH Scores) #####\n\n"
			tmp.last.window <- tail(sort(unique(SGPdata::sgptData_LONG[['YEAR']])), 1L)

			###############################################################################
			### PART 1: Using RLI_SGPt_UPDATE_SHELL
			###############################################################################

			RLI_Data_LONG <- copy(SGPdata::sgptData_LONG)
			RLI_Data_LONG[,SCALE_SCORE:=NULL]
			setnames(RLI_Data_LONG, "SCALE_SCORE_RASCH", "SCALE_SCORE")
			RLI_Data_LONG <- unique(RLI_Data_LONG, by=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			RLI_SGPt_UPDATE_SHELL <- suppressMessages(prepareSGP(RLI_Data_LONG[!YEAR %in% tmp.last.window], state="RLI"))
			RLI_Data_LONG_UPDATE <- RLI_Data_LONG[YEAR %in% tmp.last.window]
			SGPstateData[["RLI"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <-
				eval(parse(text=paste("RLI_SGPt_Baseline_Matrices[['RLI_SGPt_Baseline_Matrices_2016_2017.3']]")))
			SGPstateData[["RLI"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50
			SGPstateData[["RLI"]][["Assessment_Program_Information"]][["CSEM"]] <- "SEM"
			RLI_Cutscores <- SGPstateData[['RLI']][['SGP_Configuration']][['testSGP.cutscores']][['RASCH']]

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI3_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_SGPt_UPDATE_SHELL,\n\tadditional.data=RLI_Data_LONG_UPDATE,\n\ttesting.window='SPRING',\n\teow.or.update='UPDATE',\n\tconfiguration.year='2016_2017',\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI3_SGPt_PART_1, file='Data/RLI3_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI3 (RASCH Scores), Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI3_PART_1")) unlink("Data/RLI3_PART_1", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI3_PART_1")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number RLI3 (RASCH Scores): Part 1 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI3_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 108072L)) { #107694L SGPdata 15.0, RLImatrices 6.1 #107696
			if (identical(digest(RLI3_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "0022858af7b6d9faf0f2a9b2f9081766")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_BASELINE_10_TIME_CURRENT variable from READING

#			if (identical(sum(RLI3_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']], na.rm=TRUE), 51058L)) { ## 2015 was 45054L, 2016 was 50186L, preRLI was 51714L, RLImatrices 6.1 50160L, 1.9-0.0 49229L
			if (identical(digest(RLI3_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", "TARGET_SCALE_SCORES", sep=".")]][['SGP_TARGET_BASELINE_10_TIME_CURRENT']]), "255b76bb9a3c316aa2b89d5911f271ba")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_BASELINE_10_TIME_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI3_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 1086.214)) { ## 1085.258 SGP 15.0, RLImatrices 6.1 1085.253
			if (identical(digest(RLI3_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]), "cedb3d9ba935a5af5f53bb3f5760a501")) { ## 1.9-0.0 "e22076c79cc007efeefe4151ab7b36c0"
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGPercentiles output: Dimension

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI3_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI3_PART_1/SGPercentiles/READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(2179L, 10L))) {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGPercentiles output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGPercentiles output: Length of SGP_NORM_GROUP_BASELINE,

			my.tmp.1 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE,perl=TRUE), function(x) length(x))
			my.tmp.2 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_SCALE_SCORES,perl=TRUE), function(x) length(x))
			my.tmp.3 <- sapply(gregexpr("(?=; )",tmp.data$SGP_NORM_GROUP_BASELINE_DATES,perl=TRUE), function(x) length(x))

			if (identical(my.tmp.1, my.tmp.2) & identical(my.tmp.2, my.tmp.3)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of number of semi-colons of SGP_NORM_GROUP variables, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGProjections output

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI3_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI3_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.txt", sep="."))

			if (identical(dim(tmp.data), c(981L, 1038L))) { ### preRLI3 c(981L, 118L)
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of READING SGProjections TARGETS output

			if (identical(.Platform$OS.type, "unix")) {
				invisible(unzip(paste("Data/RLI3_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt.zip", sep=".")))
				tmp.data <- fread(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
				unlink(paste("READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))
			} else	tmp.data <- fread(paste("Data/RLI3_PART_1/SGProjections/READING", tmp.last.window, "BASELINE.TARGET_SCALE_SCORES.txt", sep="."))

			if (identical(dim(tmp.data), c(900L, 25L))) { ### 2014-2015 was 867L, preRLI3 was 819L
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of dimension of SGProjections output, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI3 (RASCH Scores), Part 1: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 2: Using LONG Data
			###############################################################################

			RLI_SGPt_Data_LONG <- copy(SGPdata::sgptData_LONG)
			RLI_SGPt_Data_LONG[,SCALE_SCORE:=NULL]
			RLI_SGPt_Data_LONG <- unique(RLI_SGPt_Data_LONG, by=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			setnames(RLI_SGPt_Data_LONG, "SCALE_SCORE_RASCH", "SCALE_SCORE")

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI3_SGPt_PART_2 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI3_SGPt_PART_2, file='Data/RLI3_SGPt_PART_2.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI3 (RASCH Scores), Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI3_PART_2")) unlink("Data/RLI3_PART_2", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI3_PART_2")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI3 (RASCH Scores): Part 2 #####\n")

			### TEST of equality between RLI_SGPt_PART_1@SGP and RLI_SGPt_PART_2@SGP

			if (identical(RLI3_SGPt_PART_1@SGP, RLI3_SGPt_PART_2@SGP)) {
				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI3_PART_1@SGP and RLI3_PART_2@SGP, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI3_PART_1@SGP and RLI3_PART_2@SGP, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI3 (RASCH Scores): Part 2", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))


			###############################################################################
			### PART 3: Using Matrices from Next Window
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI3_SGPt_PART_3 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\tcoefficient.matrices=RLI_SGPt_Baseline_Matrices$RLI_SGPt_Baseline_Matrices_2017_2018.1,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI3_SGPt_PART_3, file='Data/RLI3_SGPt_PART_3.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI3 (RASCH Scores), Part 3:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_3.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI3_PART_3")) unlink("Data/RLI3_PART_3", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI3_PART_3")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI3 (RASCH Scores): Part 3 #####\n")

			### TEST of SGP variable from READING (equality with PART 1)

#			if (identical(sum(RLI3_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 108072L)) { ## 107694L SGP 15.0, RLImatrices 6.1 107696L
			if (identical(digest(RLI3_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "0022858af7b6d9faf0f2a9b2f9081766")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2016_2017.3 matrices = 2017_2018.1 matrices), part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE (2016_2017.3 matrices = 2017_2018.1 matrices), part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI3 (RASCH Scores): Part 3", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number RLI3 (RASCH Scores): ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER RLI3


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI4: Test of SGPt functionality with UK data
		###
		#######################################################################################################################################################

		if ("RLI4" %in% TEST_NUMBER) {

			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]
			SGPstateData[["RLI_UK"]][["SGP_Configuration"]][["fix.duplicates"]] <- "KEEP.ALL"
			SGPstateData[["RLI_UK"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50
			SGPstateData[["RLI_UK"]][["Assessment_Program_Information"]][["CSEM"]] <- "SEM"
			RLI_Cutscores <- SGPstateData[['RLI']][['SGP_Configuration']][['testSGP.cutscores']][['STAR']]
			RLI4_UK_SGPt_PART_1 <- RLI4_UK_SGPt_PART_2 <- COUNTRY <- STATE <- DATE <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI4 (STAR Scores for UK) #####\n"
			tmp.last.window <- tail(sort(unique(SGPdata::sgptData_LONG[['YEAR']])), 1L)

			RLI_UK_SGPt_Data_LONG <- copy(SGPdata::sgptData_LONG)[CONTENT_AREA %in% c("READING")][,c("ACHIEVEMENT_LEVEL", "SCALE_SCORE_RASCH"):=NULL][,COUNTRY:="GB"][,STATE:="HAMP"]

			###############################################################################
			### PART 1: Calculate using actual year in data set (currently 2016_2017.3)
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI4_UK_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_UK_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=,\n\tscore.type='STAR',\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI4_UK_SGPt_PART_1, file='Data/RLI4_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI4, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI_UK_PART_1")) unlink("Data/RLI_UK_PART_1", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI_UK_PART_1")


			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI4 (STAR Scores for UK): Part 1 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI4_UK_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 109845L)) {
			if (identical(digest(RLI4_UK_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "e125a33a1ce622a083d2d857e6d25e09")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI4_UK_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 541497.7)) {
			if (identical(digest(RLI4_UK_SGPt_PART_1@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]), "eba03531daba48781b47fee56c8404f2")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI4 (STAR Scores for UK): Part 1", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 2: Calculate using year + 2 in data set (currently 2018_2019.3)
			###############################################################################

			### Bump dates forward by two years

			tmp.date <- as.POSIXlt(RLI_UK_SGPt_Data_LONG[['DATE']])
			tmp.date$year <- tmp.date$year+2
			RLI_UK_SGPt_Data_LONG[,DATE:=as.Date(tmp.date)]
			RLI_UK_SGPt_Data_LONG[,YEAR:=gsub("2016_2017", "2018_2019", YEAR)]
			RLI_UK_SGPt_Data_LONG[,YEAR:=gsub("2015_2016", "2017_2018", YEAR)]
			RLI_UK_SGPt_Data_LONG[,YEAR:=gsub("2014_2015", "2016_2017", YEAR)]
			tmp.last.window <- tail(sort(unique(RLI_UK_SGPt_Data_LONG[['YEAR']])), 1L)


			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI4_UK_SGPt_PART_2 <- rliSGP(\n\tsgp_object=RLI_UK_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=,\n\tscore.type='STAR',\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI4_UK_SGPt_PART_2, file='Data/RLI4_SGPt_PART_2.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI4, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI_UK_PART_2")) unlink("Data/RLI_UK_PART_2", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI_UK_PART_2")


			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI4 (STAR Scores for UK): Part 2 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI4_UK_SGPt_PART_2@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 109799L)) {
			if (identical(digest(RLI4_UK_SGPt_PART_2@SGP[['SGPercentiles']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "36b2431ba6eac8508e239acc6f3a1e87")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI4_UK_SGPt_PART_2@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 548304.5)) {
			if (identical(digest(RLI4_UK_SGPt_PART_2@SGP[['SGProjections']][[paste("READING", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]), "a0a4e95cda2862934568568a45813817")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI4 (STAR Scores for UK): Part 2", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number RLI4 (STAR Scores for UK): ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER RLI4


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI5: Test of SGPt functionality with UK data, RASCH scores
		###
		#######################################################################################################################################################

		if ("RLI5" %in% TEST_NUMBER) {

			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]
			SGPstateData[["RLI_UK"]][["SGP_Configuration"]][["fix.duplicates"]] <- "KEEP.ALL"
			SGPstateData[["RLI_UK"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50
			SGPstateData[["RLI_UK"]][["Assessment_Program_Information"]][["CSEM"]] <- "SEM"
			RLI_Cutscores <- SGPstateData[['RLI']][['SGP_Configuration']][['testSGP.cutscores']][['STAR']]
			RLI5_UK_SGPt_PART_1 <- RLI5_UK_SGPt_PART_2 <- RLI5_UK_SGPt_PART_3 <- COUNTRY <- STATE <- DATE <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI5 (RASCH Scores for UK) #####\n"

			RLI_UK_SGPt_Data_LONG <- copy(SGPdata::sgptData_LONG)[CONTENT_AREA %in% c("READING", "MATHEMATICS")][,c("ACHIEVEMENT_LEVEL", "SCALE_SCORE"):=NULL][,COUNTRY:="GB"][,STATE:="HAMP"]
			setnames(RLI_UK_SGPt_Data_LONG, "SCALE_SCORE_RASCH", "SCALE_SCORE")

			### Bump dates forward by five years to utilize updated matrices

			tmp.date <- as.POSIXlt(RLI_UK_SGPt_Data_LONG[['DATE']])
			tmp.date$year <- tmp.date$year+5
			RLI_UK_SGPt_Data_LONG[,DATE:=as.Date(tmp.date)]
			RLI_UK_SGPt_Data_LONG[,YEAR:=gsub("2016_2017", "2021_2022", YEAR)]
			RLI_UK_SGPt_Data_LONG[,YEAR:=gsub("2015_2016", "2020_2021", YEAR)]
			RLI_UK_SGPt_Data_LONG[,YEAR:=gsub("2014_2015", "2019_2020", YEAR)]
			tmp.last.window <- tail(sort(unique(RLI_UK_SGPt_Data_LONG[['YEAR']])), 1L)

			###############################################################################
			### PART 1: Calculate SPRING using modified year in data set (currently 2021_2022.3)
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI5_UK_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_UK_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='RASCH',\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI5_UK_SGPt_PART_1, file='Data/RLI5_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI5, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI_UK_PART_1")) unlink("Data/RLI_UK_PART_1", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI_UK_PART_1")


			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI5 (RASCH Scores for UK): Part 1 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI5_UK_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 106747L)) {
			if (identical(digest(RLI5_UK_SGPt_PART_1@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "b163bd22d7e7a4d91794c65cdb35420a")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI5_UK_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 506.035)) {
			if (identical(digest(RLI5_UK_SGPt_PART_1@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]),  "09324220ac2f0d99365fc826b3ac6af0")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI5 (RASCH Scores for UK): Part 1", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 2: Calculate WINTER using modified year in data set (currently 2021_2022.2)
			###############################################################################

			RLI_UK_SGPt_Data_LONG <- RLI_UK_SGPt_Data_LONG[YEAR <= "2021_2022.2"]
			tmp.last.window <- tail(sort(unique(RLI_UK_SGPt_Data_LONG[['YEAR']])), 1L)

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI5_UK_SGPt_PART_2 <- rliSGP(\n\tsgp_object=RLI_UK_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='RASCH',\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI5_UK_SGPt_PART_2, file='Data/RLI5_SGPt_PART_2.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI5, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI_UK_PART_2")) unlink("Data/RLI_UK_PART_2", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI_UK_PART_2")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI5 (RASCH Scores for UK): Part 2 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI5_UK_SGPt_PART_2@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 79088L)) {
			if (identical(digest(RLI5_UK_SGPt_PART_2@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "58d33bdc3163712099eb13bc44aa4407")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI5_UK_SGPt_PART_2@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 1100.36)) {
			if (identical(digest(RLI5_UK_SGPt_PART_2@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]),  "ea419749a7ab8cbf881cf05c4c16f31d")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 2: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 2: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI5 (RASCH Scores for UK): Part 2", convertTime(timetakenSGP(started.at.overall)), "#####\n"))


			###############################################################################
			### PART 3: Calculate FALL using modified year in data set (currently 2021_2022.1)
			###############################################################################

			RLI_UK_SGPt_Data_LONG <- RLI_UK_SGPt_Data_LONG[YEAR <= "2021_2022.1"]
			tmp.last.window <- tail(sort(unique(RLI_UK_SGPt_Data_LONG[['YEAR']])), 1L)

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI5_UK_SGPt_PART_3 <- rliSGP(\n\tsgp_object=RLI_UK_SGPt_Data_LONG,\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='RASCH',\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI5_UK_SGPt_PART_3, file='Data/RLI5_SGPt_PART_3.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI5, Part 3:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_3.out", memory.profiling=TRUE)
			}

			started.at.overall <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI_UK_PART_3")) unlink("Data/RLI_UK_PART_3", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI_UK_PART_3")


			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI5 (RASCH Scores for UK): Part 3 #####\n")

			### TEST of SGP variable from READING

#			if (identical(sum(RLI5_UK_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']], na.rm=TRUE), 143361L)) {
			if (identical(digest(RLI5_UK_SGPt_PART_3@SGP[['SGPercentiles']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['SGP_BASELINE']]), "0226d0993b1649f197a8e1f3fa61aa9a")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of P50_PROJ_TIME_1_CURRENT variable from READING

#			if (identical(sum(RLI5_UK_SGPt_PART_3@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']], na.rm=TRUE), 1845.455)) {
			if (identical(digest(RLI5_UK_SGPt_PART_3@SGP[['SGProjections']][[paste("READING_RASCH", tmp.last.window, "BASELINE", sep=".")]][['P50_PROJ_TIME_1_CURRENT']]),  "e96b846e43b8726bea586f1abfefd872")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 3: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable P50_PROJ_TIME_1_CURRENT, part 3: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI5 (RASCH Scores for UK): Part 3", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number RLI5 (RASCH Scores for UK): ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER RLI5


		#######################################################################################################################################################
		###
		### TEST NUMBER RLI6: Test of FALL Analyses
		###
		#######################################################################################################################################################

		if ("RLI6" %in% TEST_NUMBER) {
			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]
			SGPstateData[["RLI"]][["SGP_Configuration"]][["fix.duplicates"]] <- "KEEP.ALL"
			SGPstateData[["RLI"]][["SGP_Configuration"]][["goodness.of.fit.minimum.n"]] <- 50
			SGPstateData[["RLI"]][["Assessment_Program_Information"]][["CSEM"]] <- "SEM"
			RLI_Cutscores <- SGPstateData[['RLI']][['SGP_Configuration']][['testSGP.cutscores']][['STAR']]
			RLI6_SGPt_PART_1 <- RLI6_SGPt_PART_2 <- COUNTRY <- STATE <- DATE <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI6 (FALL Analyses) #####\n"

			RLI_SGPt_Data_LONG <- copy(SGPdata::sgptData_LONG)[,SCALE_SCORE_RASCH:=NULL][YEAR <= "2016_2017.1"]
			tmp.last.window <- tail(sort(unique(RLI_SGPt_Data_LONG$YEAR)), 1L)

			###############################################################################
			### PART 1: Using LONG Data
			###############################################################################

			### Calculate SGPs

			expression.to.evaluate <-
				paste0("RLI6_SGPt_PART_1 <- rliSGP(\n\tsgp_object=RLI_SGPt_Data_LONG,\n\ttesting.window='FALL',\n\treturn.updated.shell=TRUE,\n\tgoodness.of.fit.print=TRUE,\n\tscore.type='STAR',\n\tsimulate.sgps=TRUE,\n\tcutscore.file.name=RLI_Cutscores,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(RLI6_SGPt_PART_1, file='Data/RLI6_SGPt_PART_1.Rdata')", sep="\n")

			cat(paste0("EVALUATING Test Number RLI, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(RLI)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
			}

			started.at.intermediate <- proc.time()
			eval(parse(text=expression.to.evaluate))
			if (dir.exists("Data/RLI6_PART_1")) unlink("Data/RLI6_PART_1", recursive = TRUE)
			file.rename("Data/RLI", "Data/RLI6_PART_1")

			### TEST of variable values

			tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number RLI6 (STAR Scores): Part 1 #####\n")

			### TEST of equality between RLI_SGPt_PART_1@SGP and RLI_SGPt_PART_2@SGP

#			if (identical(RLI6_SGPt_PART_1@SGP, RLI1_SGPt_PART_2@SGP)) { #
#				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI1_PART_1@SGP and RLI1_PART_2@SGP, part 2: OK\n")
#			} else {
#				tmp.messages <- c(tmp.messages, "\t\tTest of equatity of RLI1_PART_1@SGP and RLI1_PART_2@SGP, part 2: FAIL\n")
#				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
#			}

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number RLI6 (STAR Scores): Part 1", convertTime(timetakenSGP(started.at.intermediate)), "#####\n"))
		} ### END TEST_NUMBER RLI6


		#######################################################################################################################################################
		###
		### TEST NUMBER 7: Test of ability to deal with duplicates in test data
		###
		#######################################################################################################################################################

		if (7 %in% TEST_NUMBER) {

			started.at.overall <- proc.time()
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
			} else parallel.config <- test.option[['parallel.config']]

			Demonstration_SGP <- GRADE_REPORTED <- SCALE_SCORE <- VALID_CASE <- NULL
			tmp.messages <- "##### Begin testSGP test number 7 #####\n\n"
			sgpData_LONG <- copy(as.data.table(SGPdata::sgpData_LONG))

			### Add EOCT courses to sgpData_LONG and add duplicates

			sgpData_LONG[CONTENT_AREA=='MATHEMATICS' & GRADE=='9', CONTENT_AREA:='ALGEBRA_I']
			sgpData_LONG[CONTENT_AREA=='MATHEMATICS' & GRADE=='10', CONTENT_AREA:='ALGEBRA_II']
			sgpData_LONG[,GRADE_REPORTED:=GRADE]
			sgpData_LONG[CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II'), GRADE:='EOCT']

			dups <- rbindlist(list(
								##  Current and prior year dups in PENULTIMATE_YEAR & ULTIMATE_YEAR
								##  IDs  ::  "1001148" "1002063" "1008260" "1013238" "1013584"
								head(sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="8" & YEAR==rev(sgpData.years)[3]], 5), # , c("CONTENT_AREA","YEAR","ID", "GRADE", "SCALE_SCORE"), with=F
								head(sgpData_LONG[CONTENT_AREA=="ALGEBRA_I" & YEAR==rev(sgpData.years)[2]], 5),
								head(sgpData_LONG[CONTENT_AREA=="ALGEBRA_II" & YEAR==rev(sgpData.years)[1]], 5),

								##  Dups when grade level ignored in key (7th graders have 8th grade score too) - NO SGP from DUP in PENULTIMATE_YEAR  but 3 students in ULTIMATE_YEAR
								##  IDs  ::  "1000789" "1000849" "1002713" (years 1 & 2)  &   "1003204" "1003309" (year 1 - current only)
								head(sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="6" & YEAR==rev(sgpData.years)[2]][, GRADE := as.character(as.numeric(GRADE)+1)], 3),
								head(sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="7" & YEAR==rev(sgpData.years)[1]][, GRADE := as.character(as.numeric(GRADE)+1)], 5),

								##  Single, prior-year dups :: IDs "1001687" "1001746" "1001882" "1003696" "1005857"
								head(sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="3" & YEAR==rev(sgpData.years)[2]], 5),
								##  Single, current- (part 1) and prior-year (part 2, dups 1 year removed)  dups :: IDs "1005155" "1009161" "1010369" "1010526" "1011348"
								head(sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="5" & YEAR==rev(sgpData.years)[2]], 5),
								##  Single, prior-year (part 1 = dups 1 year removed and part 2 = dups 2 years removed) dups :: IDs "1000372" "1000512" "1001500" "1004556" "1004918"
								head(sgpData_LONG[CONTENT_AREA=="MATHEMATICS" & GRADE=="3" & YEAR==rev(sgpData.years)[3]], 5),
								##  Single, current-year dups :: IDs "1000452" "1000482" "1006856" "1008223" "1008957"  ("1008223" "1008957" - NO Prior Scores - No SGPs - No DUPS_FLAG???)
								head(sgpData_LONG[CONTENT_AREA=="ALGEBRA_I" & YEAR==rev(sgpData.years)[1]], 5)
							)) # dups[, c(1,4:7), with=F] # 38 records

			dups[,SCALE_SCORE:=SCALE_SCORE+10]

			sgpData_LONG <- rbindlist(list(sgpData_LONG[CONTENT_AREA %in% c("MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II")], dups))
			setkey(sgpData_LONG, VALID_CASE, CONTENT_AREA, YEAR, ID)

			### Modify SGPstateData  -  Move to SGPstateData.R for SNOW versions of testSGP(7)  -  getTargetScaleScore doesn't work (because function non exported???)
			SGPstateData[["DEMO_EOCT"]][["SGP_Configuration"]][["fix.duplicates"]] <- "KEEP.ALL"
			SGPstateData[["DEMO_EOCT"]][["SGP_Norm_Group_Preference"]] <- NULL
			SGPstateData[["DEMO_EOCT"]][["Growth"]][["System_Type"]] <- "Cohort Referenced"
			SGPstateData[["DEMO_EOCT"]][['SGP_Configuration']][['sgPlot.output.format']] <- NULL
			### Create configurations

			MATHEMATICS_PENULTIMATE_YEAR.config <- list(
				MATHEMATICS.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS'),
					sgp.panel.years=head(sgpData.years, -1L),
					sgp.grade.sequences=list(3:4, 3:5, 3:6, 4:7, 5:8))
			)

			MATHEMATICS_LAST_YEAR.config <- list(
				MATHEMATICS.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
			)

			ALGEBRA_I_PENULTIMATE_YEAR.config <- list(
				ALGEBRA_I.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I'),
					sgp.panel.years=head(sgpData.years, -1L),
					sgp.grade.sequences=list(c(6:8, 'EOCT')))
			)

			ALGEBRA_I_LAST_YEAR.config <- list(
				ALGEBRA_I.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(c(5:8, 'EOCT')))
			)

			ALGEBRA_II_PENULTIMATE_YEAR.config <- list(
				ALGEBRA_II.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
					sgp.panel.years=head(sgpData.years, -1L),
					sgp.grade.sequences=list(c(7:8, 'EOCT', 'EOCT')))
			)

			ALGEBRA_II_LAST_YEAR.config <- list(
				ALGEBRA_II.LAST_YEAR=list(
					sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
					sgp.panel.years=sgpData.years,
					sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
			)

			sgp.config.PENULTIMATE <- c(MATHEMATICS_PENULTIMATE_YEAR.config, ALGEBRA_I_PENULTIMATE_YEAR.config, ALGEBRA_II_PENULTIMATE_YEAR.config)
			sgp.config.ULTIMATE <- c(MATHEMATICS_LAST_YEAR.config, ALGEBRA_I_LAST_YEAR.config, ALGEBRA_II_LAST_YEAR.config)

			### Part 1

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(state='DEMO_EOCT',\n\tsgp_object=sgpData_LONG[YEAR %in% head(sgpData.years, -1L)],\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.percentiles=TRUE,\n\tsgp.projections=TRUE,\n\tsgp.projections.lagged=TRUE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config.PENULTIMATE,\n\tparallel.config=", parallel.config, "\n)\n")

			cat("##### Begin testSGP test number 7 #####\n", fill=TRUE)
			cat(paste0("EVALUATING Test Number 7, Part 1:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(7)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.intermediate1 <- proc.time()
			eval(parse(text=expression.to.evaluate))

			if (memory.profile) {
				Rprof(NULL)
			}

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 7: Part 1 #####\n")

			### TEST of SGP variable
			#   identical(sum(Demonstration_SGP@Data$SGP, na.rm=T), 1441034L)
			if (identical(digest::digest(Demonstration_SGP@Data$SGP), "83851035b2dd968c4f92fcf66edd5de0")) { # pre-GRADE key: 463af59acba4825cfb9114528fbc96a4
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR variable
			#   identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=T), 1761988L)
			if (identical(digest::digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "07673a71188a1e01acbd21b2caff5655")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR_CURRENT variable
			#   identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR_CURRENT, na.rm=T), 1888559L)
			if (identical(digest::digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR_CURRENT), "a937271073e02625264de7a779d0225a")) {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT variable for MATHEMATICS.XXXX_XXXX scale score targets  :: sum 15097046  ::  dim 27207     9
			setkey(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(head(sgpData.years, -1L), 1L), "TARGET_SCALE_SCORES", sep=".")]])
			if (identical(digest::digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(head(sgpData.years, -1L), 1L), "TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT']]), "1b39f58739785da3d0c5dda548b6ad3b")) { # bf576ec83a3f22420d87db5bea983baf  v14: cfdbc67cc171bb0f254f9768759078cb :: SNOW febf7c535de5ef1d4ccb3d7689b21b5a
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable for MATHEMATICS.XXXX_XXXX scale score targets  :: sum 11284534  ::  dim = 20468    11
			setkey(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(head(sgpData.years, -1L), 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]])
			if (identical(digest::digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(head(sgpData.years, -1L), 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']]), "bb4bd2c3868b5089fa97e7bcca7d8918")) { # seq 0a0d9b6b1e85257964806039436a5101  ::  v14: 68538a5a21ccb1b6d544ecaf342081ad :: SNOW 7804cb64c45e41224b7249ff36149d3a
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of DUPS @Data
			vars.to.check <- c("CONTENT_AREA", "ID", "SCALE_SCORE", "SCALE_SCORE_PRIOR", "GRADE", "SGP", "SGP_NORM_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES", "CATCH_UP_KEEP_UP_STATUS_3_YEAR")
			# dup.data <- Demonstration_SGP@Data[!is.na(DUPS_FLAG), vars.to.check, with=FALSE]
			dup.ids <- unique(Demonstration_SGP@Data[duplicated(Demonstration_SGP@Data, by=setdiff(getKey(Demonstration_SGP@Data), "GRADE")), ID]) # Still check "GRADE excluded" dups
			dup.data <- Demonstration_SGP@Data[ID %in% dup.ids, vars.to.check, with=FALSE]
			setkey(dup.data)
			if (identical(digest::digest(dup.data), "4f0ff253da62439b7bde584554e7ecee")) { # Pre 1.9-0.0 "1f948cfd6f1c8940795cb339c5efb917" # with DUPS_FLAG: 2b85afc2857336aaa165dd04feb47ef4
				tmp.messages <- c(tmp.messages, "\t\tTest of duplicated cases in @Data: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of duplicated cases in @Data: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of Coefficient_Matrices
			# coef.mtx.index <- grep("ALGEBRA_I[.]", names(Demonstration_SGP@SGP[["Coefficient_Matrices"]]))
			# coef.mtx.to.check <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_EOCT_3"]]@.Data)
			# if (identical(digest::digest(coef.mtx.to.check), "a350a6f8cadff1562dacb40af2aff655")) { # 2014_2015 as PENULTIMATE_YEAR :: e82305e666e5f154eb385f0679229e16 - use 'as.numeric' to remove rownames, etc.
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (1): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (1): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }

			# coef.mtx.index <- grep("MATHEMATICS", names(Demonstration_SGP@SGP[["Coefficient_Matrices"]]))
			# coef.mtx.to.check1 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_4_1"]]@.Data)
			# coef.mtx.to.check2 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_5_1"]]@.Data)
			# coef.mtx.to.check3 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_6_1"]]@.Data)
			# coef.mtx.to.check4 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_7_1"]]@.Data)
			# coef.mtx.to.check5 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_8_1"]]@.Data)
			# if (identical(digest::digest(coef.mtx.to.check1), "f332e90d30252e9736b19f77815bd5c5")) { # 142501756fb3548dd768669a25a6e29c
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (2): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (2): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check2), "ec280c6a44de8825bd42ff2a6d184522")) { # d2756efffbe106f8a189f9aaed7f59f4
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (3): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (3): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check3), "56f8903607317231286a7a7a0b605d93")) { # 71367c2559ef1038bea60919246a6bc5
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (4): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (4): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check4), "a2ccf47e36fc5f8f90944dbbc8eb0aa7")) { # cf27eded199f369d7275e598f614e297
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (5): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (5): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check5), "8446a0ede4a5e1b0946fd772aa95304e")) { # 0a3e11fbc4a7a7e766d423ab998465ad
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (6): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (6): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 7, Part 1: ", convertTime(timetakenSGP(started.at.intermediate1)), "#####\n\n"))


			### Part 2

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- updateSGP(state='DEMO_EOCT',\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=sgpData_LONG[YEAR == tail(sgpData.years, 1L)],\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'outputSGP'), \n\tsgp.percentiles=TRUE,\n\tsgp.projections=TRUE,\n\tsgp.projections.lagged=TRUE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config.ULTIMATE,\n\tparallel.config=", parallel.config, "\n)\n")

			if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

			cat(paste0("EVALUATING test number 7, Part 2:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(7)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.intermediate2 <- proc.time()
			eval(parse(text=expression.to.evaluate))

			if (memory.profile) {
				Rprof(NULL)
			}

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 7: Part 2 #####\n")

			### TEST of SGP variable
			#   identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 2901002L)
			if (identical(digest::digest(Demonstration_SGP@Data$SGP), "e3cb2835d8e9210583aa81577b938f50")) { # pre-GRADE key: 02231cb28da81d38302d8550c5f3c576
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR variable
			#   identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 3442573L)
			if (identical(digest::digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR), "27e97f7bed234f25cb8070b27c9b9f05")) { # pre-GRADE key: f18d29d1595406c153c7068872d5c720
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SGP_TARGET_3_YEAR_CURRENT variable
			#   identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR_CURRENT, na.rm=TRUE), 3691422L)
			if (identical(digest::digest(Demonstration_SGP@Data$SGP_TARGET_3_YEAR_CURRENT), "46e9dff1f07043cdf29ef61a08f83722")) { # pre-GRADE key: f7ee20abc5e2d56cf70ef1cafdf46a93
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT variable for MATHEMATICS.XXXX_XXXX scale score targets  ::  dim 27834 x 9  ::  sum 15398506
			setkey(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), "TARGET_SCALE_SCORES", sep=".")]])
			if (identical(digest::digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), "TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT']]), "94e2e53b97a5b64024b3d92f9619578b")) { # cfdbc67cc171bb0f254f9768759078cb
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable for MATHEMATICS.XXXX_XXXX scale score targets  SEQ sum 13934897 ::  dim 24956 x 11
			setkey(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]])
			if (identical(digest::digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']]), "7c1d199dcde9c0187bf9efd4e5730ebf")) { # seq 296de3ef1829819cf3f6c0775c0ab76c v14 (?)
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of DUPS @Data
			vars.to.check <- c("CONTENT_AREA", "ID", "SCALE_SCORE", "SCALE_SCORE_PRIOR", "GRADE", "SGP", "SGP_NORM_GROUP_SCALE_SCORES", "SGP_PROJECTION_GROUP_SCALE_SCORES", "CATCH_UP_KEEP_UP_STATUS_3_YEAR")
			dup.ids <- unique(Demonstration_SGP@Data[duplicated(Demonstration_SGP@Data, by=setdiff(getKey(Demonstration_SGP@Data), "GRADE")), ID]) # Still check "GRADE excluded" dups
			dup.data <- Demonstration_SGP@Data[ID %in% dup.ids, vars.to.check, with=FALSE]
			setkey(dup.data)
			if (identical(digest::digest(dup.data), "b807be39ea50348ac5d26e245ce260bc")) { # Pre 1.9-0.0 1096ae04d748ba69d7dd63ef398ac434  #  With DUPS_FLAG: 12e253be3ce98f8f519b6a452f13cc93
				tmp.messages <- c(tmp.messages, "\t\tTest of duplicated cases in @Data: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of duplicated cases in @Data: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of Coefficient Matrix
			# coef.mtx.index <- grep("ALGEBRA_II", sort(names(Demonstration_SGP@SGP[["Coefficient_Matrices"]])), value=TRUE)[2]
			# coef.mtx.to.check <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_EOCT_2"]]@.Data)
			# coef.mtx.to.check.n <- Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_EOCT_2"]]@Version[["Matrix_Information"]][["N"]]
			# if (identical(digest::digest(coef.mtx.to.check), "bce86db75d2215f78e28cfef20d60c0f") & coef.mtx.to.check.n == 3752L) { # 75b4e472b89b0f6c7c966a381a93ee99
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (1): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (1): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }

			# coef.mtx.index <- grep("MATHEMATICS", sort(names(Demonstration_SGP@SGP[["Coefficient_Matrices"]])), value=TRUE)[2]
			# coef.mtx.to.check1 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_4_1"]]@.Data)
			# coef.mtx.to.check2 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_5_1"]]@.Data)
			# coef.mtx.to.check3 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_6_1"]]@.Data)
			# coef.mtx.to.check4 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_7_1"]]@.Data)
			# coef.mtx.to.check5 <- as.numeric(Demonstration_SGP@SGP[["Coefficient_Matrices"]][[coef.mtx.index]][["qrmatrix_8_1"]]@.Data)
			# if (identical(digest::digest(coef.mtx.to.check1), "3e00c3825eafed6b508ba6a679afb24b")) {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (2): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (2): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check2), "433ab99726913bf12fb8922ab285fdc9")) {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (3): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (3): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check3), "eb73b35a533e62f43829b9d64006c134")) {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (4): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (4): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check4), "9e2bd1979b14b34759cafffdbe943d80")) {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (5): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (5): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }
			# if (identical(digest::digest(coef.mtx.to.check5), "3962206bc6333e1dabbb3057d1281899")) {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (6): OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Coefficient Matrix (6): FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 7, Part 2: ", convertTime(timetakenSGP(started.at.intermediate2)), "#####\n\n"))

			### Part 3a - summarizeSGP

			#  Change the EOCT info back to Grade Level Mathematics for the (INSTRUCTOR_NUMBER) summaries
			Demonstration_SGP@Data_Supplementary[['INSTRUCTOR_NUMBER']] <- SGPdata::sgpData_INSTRUCTOR_NUMBER
		  Demonstration_SGP@Data[, GRADE := GRADE_REPORTED]
		  Demonstration_SGP@Data[CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II'), CONTENT_AREA := 'MATHEMATICS']
		  setkeyv(Demonstration_SGP@Data, getKey(Demonstration_SGP@Data))

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- summarizeSGP(\n\tDemonstration_SGP,\n\tstate='DEMO_EOCT',\n\tparallel.config = ", parallel.config, "\n)\n")

			cat(paste0("EVALUATING test number 7, Part 3a:\n", expression.to.evaluate), fill=TRUE)

			if (memory.profile) {
				Rprof("testSGP(7)_Memory_Profile.out", memory.profiling=TRUE)
			}

			started.at.intermediate3 <- proc.time()
			eval(parse(text=expression.to.evaluate))

			### Part 3b - visualizeSGP

			#  Change EOCT back to EOCT for Visualizations.
			Demonstration_SGP@Data[CONTENT_AREA=='MATHEMATICS' & GRADE=='9', CONTENT_AREA:='ALGEBRA_I']
			Demonstration_SGP@Data[CONTENT_AREA=='MATHEMATICS' & GRADE=='10', CONTENT_AREA:='ALGEBRA_II']
			Demonstration_SGP@Data[CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II'), GRADE:='EOCT']

			expression.to.evaluate <-
				paste0("visualizeSGP(state='DEMO_EOCT',\n\tDemonstration_SGP, \n\tsgPlot.students = dup.ids, \n\tparallel.config = ", parallel.config, "\n)\n")

			cat(paste0("EVALUATING test number 7, Part 3b:\n", expression.to.evaluate), fill=TRUE)

			eval(parse(text=expression.to.evaluate))

			if (memory.profile) {
				Rprof(NULL)
			}

			tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 7: Part 3 #####\n")

			### TEST of School Summary Table  :: dim = 637 x 22
			setkey(Demonstration_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__GRADE__SCHOOL_ENROLLMENT_STATUS"]])
			if (identical(digest::digest(Demonstration_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__GRADE__SCHOOL_ENROLLMENT_STATUS"]]), "fa3b25ad7ec2384e8a72d5e8acbd34b8")) { # Pre 1.9-0.0 e13379b7384154f36d26eb0de40d79c2 # 3472a7f8a5ee67e248de158feb3da808  # pre-`collapse` integration 1b5d0c6b4d39d96285c018c9f410a337
				# identical(sum(Demonstration_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__GRADE__SCHOOL_ENROLLMENT_STATUS"]]$MEDIAN_SGP_COUNT), 58069L) # Same as with YEAR!
				tmp.messages <- c(tmp.messages, "\t\tTest of School Summary Table: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\t\tTest of School Summary Table: FAIL\n")
				if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			}

			### TEST of Instructor Summary Table  :: dim = 3516 x 23
			# setkey(Demonstration_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__GRADE__INSTRUCTOR_ENROLLMENT_STATUS"]])
			# if (identical(digest::digest(Demonstration_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__GRADE__INSTRUCTOR_ENROLLMENT_STATUS"]]), "464e5b89ed3e951bb3357c82aacac5b7")) { # Pre 1.9-0.0 3f0d7039e96e44c9ccd445cc0d1bb098 # 059d0a7f18c704589cb38e0da0b3aeec
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Instructor Summary Table: OK\n")
			# } else {
			# 	tmp.messages <- c(tmp.messages, "\t\tTest of Instructor Summary Table: FAIL\n")
			# 	if (stop.fail) {messageSGP(tmp.messages); stop("\n\n\t FAILED TEST!")}
			# }

			tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number 7, Part 3: ", convertTime(timetakenSGP(started.at.intermediate3)), "#####\n"))
			tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number 7: ", convertTime(timetakenSGP(started.at.overall)), "#####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 7


		#######################################################################################################################################################
		###
		### TEST NUMBER 8: Test of Stratified Random Sample (SRS) functionality
		###
		#######################################################################################################################################################

		if (8 %in% toupper(TEST_NUMBER)) {
			eval(parse(text="require(RLImatrices)"))
			options(error=recover)
			options(warn=2)
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)
			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tBASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, "))")
			} else parallel.config <- test.option[['parallel.config']]

			RLI1_SGPt_PART_1 <- RLI1_SGPt_PART_2 <- RLI1_SGPt_PART_3 <- SCALE_SCORE_RASCH <- NULL
			tmp.messages <- "##### Begin testSGP test number RLI1 (STAR Scores) #####\n\n"
			tmp.last.window <- tail(sort(unique(SGPdata::sgptData_LONG[['YEAR']])), 1L)
			options(error=recover)
			options(warn=2)
			Demonstration_SGP <- NULL

			tmp.messages <- ("##### Results of testSGP test number 8 #####\n\n")
			if (.Platform$OS.type == "unix") number.cores <- detectSGPCores(logical=TRUE) else number.cores <- detectSGPCores(logical=FALSE)

			if (is.null(test.option[['parallel.config']])) {
				if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE='doParallel', "
				if (.Platform$OS.type != "unix") {
					parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
				} else  parallel.config <- paste0("list(BACKEND=", tmp.backend, "WORKERS=list(\n\t\tPERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", \n\t\tSGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))")
			} else parallel.config <- test.option[['parallel.config']]
			tmp.last.window <- tail(sort(unique(SGPdata::sgpData_LONG[['YEAR']])), 1L)

			### Some minor modifications to SGPstateData for testing purposes

			SGPstateData[["DEMO"]][["SGP_Configuration"]][["print.other.gp"]] <- TRUE

			expression.to.evaluate <-
				paste0("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tyears='", tmp.last.window, "',\n\tcalculate.srs=TRUE,\n\tcalculate.srs.baseline=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tparallel.config=", parallel.config, "\n)\n")

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

			if (identical(digest(Demonstration_SGP@Data[['SGP']]), "89f1cb732587f3d6ffeb1afe587941e0")) { # pre-GRADE key: d7843eab4ddf02bed640ec401dd35c65
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
			}

			### TEST of SGP_BASELINE variable

			if (identical(digest(Demonstration_SGP@Data[['SGP_BASELINE']]), "5056dc27abde7c6d8ff896947f4b0070")) { # pre-GRADE key: f6ffda530e470321159c39a56151e0c2
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: FAIL\n")
			}

			### TEST of SGP_TARGET_3_YEAR variable

			if (identical(digest(Demonstration_SGP@Data[['SGP_TARGET_3_YEAR']]), "4955e2f61a1cd34347ffceba51a5670b")) { # pre-GRADE key: 7cfb4055b5ebf739417d2220898ce99c
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
			}

			### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

			if (identical(digest(Demonstration_SGP@Data[['SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR']]), "5e6fcc4c3eedce14cc6639163677fdc7")) { # pre-GRADE key: 9bfb90fbaed288104dfe1959294497ce
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
			}

			### TEST of CATCH_UP_KEEP_UP_STATUS_3_YEAR variable

			if (identical(digest(Demonstration_SGP@Data[['CATCH_UP_KEEP_UP_STATUS_3_YEAR']]), "eb710e8b3256be3f9009ecc686c0ff37")) { # pre-GRADE key: 11e5e53c8cf330fcdb51a2e4894f5c2f
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS_3_YEAR: FAIL\n")
			}

			### TEST of MOVE_UP_STAY_UP_STATUS_3_YEAR variable

			if (identical(digest(Demonstration_SGP@Data[['MOVE_UP_STAY_UP_STATUS_3_YEAR']]), "f67a9a4d2bf8c8af201e2533c31c0ce8")) { # pre-GRADE key: 663ff40f494a37d01dc3c593b887d24b
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS_3_YEAR: FAIL\n")
			}

			### TEST of SCALE_SCORE_PRIOR variable

			if (identical(digest(Demonstration_SGP@Data[['SCALE_SCORE_PRIOR']]), "6c352608f27873a09487ec2859de7574")) { # pre-GRADE key: 23e0433ed1574c73f923b23cddbfc0cb
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: FAIL\n")
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable for READING.XXXX_XXXX scale score targets

			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('READING', tail(sgpData.years, 1L), "LAGGED.TARGET_SCALE_SCORES", sep=".")]][['SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1']]), "d67a2c0b75f411683abe1a9c023c8c0e")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
			}

			### TEST of MEDIAN_SGP variable

			if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['MEDIAN_SGP']]), "0d8c6f5489ff405a492a7ef193884a56")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: FAIL\n")
			}

			### TEST of PERCENT_AT_ABOVE_PROFICIENT_PRIOR variable

			if (identical(digest(Demonstration_SGP@Summary[['SCHOOL_NUMBER']][['SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS']][['PERCENT_AT_ABOVE_PROFICIENT_PRIOR']]), "f1e683f2c118135b28d285a754d1f188")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: FAIL\n")
			}

			### TEST of LAGGED PROJECTION CUTs variable P35_PROJ_YEAR_1 variable for MATHEMATICS.XXXX_XXXX.LAGGED

			if (identical(digest(Demonstration_SGP@SGP[['SGProjections']][[paste('MATHEMATICS', tail(sgpData.years, 1L), 'LAGGED', sep=".")]][['P20_PROJ_YEAR_1']]), "e83219e1830264576b42861aaaed067f")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable P20_PROJ_YEAR_1 (LAGGED PROJECTION): OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable P20_PROJ_YEAR_1 (LAGGED PROJECTION): FAIL\n")
			}

			### TEST of SGP_0.025_CONFIDENCE_BOUND variable in @Data

			if (identical(digest(Demonstration_SGP@Data[['SGP_0.025_CONFIDENCE_BOUND']]), "237d3505dfbbc4a4dfbdbdbd73c45629")) { # pre-GRADE key: dc13c362312f379fbbfa83c49f1e53eb
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_0.025_CONFIDENCE_BOUND: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SGP_0.025_CONFIDENCE_BOUND: FAIL\n")
			}

			### TEST of HIGH_NEED_STATUS variable in @Data

			if (identical(digest(Demonstration_SGP@Data[['HIGH_NEED_STATUS']]), "ab4d97f1d56ea8cf936008708cf4ed84")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable HIGH_NEED_STATUS: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable HIGH_NEED_STATUS: FAIL\n")
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable in @Data

			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1), "60ada507fc67c9bfd9822e0ff48a33e0")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
			}

			### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT variable in @Data

			if (identical(digest(Demonstration_SGP@Data$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT), "314bc76b76b0047f641c1a8a149ae0a8")) {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: OK\n")
			} else {
				tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT: FAIL\n")
			}

			tmp.messages <- c(tmp.messages, paste0("\n##### End testSGP test number ", TEST_NUMBER, ":  ", convertTime(timetakenSGP(started.at.overall)), " #####\n"))
			messageSGP(tmp.messages)
		} ### End TEST_NUMBER 8



	} ### END testSGP Function
