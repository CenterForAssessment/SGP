`testSGP` <- 
function(
	TEST_NUMBER,
	save.results=TRUE,
	test.option=NULL,
	memory.profile=FALSE) {

	YEAR <- GRADE <- NULL

	if (missing(TEST_NUMBER)) {
		message("\ttestSGP carries out testing of SGP package. Tests currently included in testSGP:\n")
		message("\t\t1. abcSGP test using all available years.")
		message("\t\t2. abcSGP test using all available years except most recent followed by an updated analysis using the most recent year's data.")
	}

	#######################################################################################################################################################
	###
	### TEST NUMBER 0: Test of studentGrowthPercentiles, studentGrowthProjections, and sgpData
	###
	#######################################################################################################################################################

	if ("0" %in% toupper(TEST_NUMBER)) {

		options(error=recover)
		options(warn=2)
		Demonstration_SGP <- tmp.messages <- NULL
		tmp.messages <- "##### Begin testSGP test number 0 #####\n\n"

		### Part 1

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- list(Panel_Data=SGPdata::sgpData)\nmy.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)\nfor (i in seq_along(my.grade.sequences)) {\n\tDemonstration_SGP <- studentGrowthPercentiles(\n\t\tpanel.data=Demonstration_SGP,\n\t\tsgp.labels=list(my.year=2015, my.subject='Reading'),\n\t\tgrowth.levels='DEMO',\n\t\tgoodness.of.fit='DEMO',\n\t\tgrade.progression=my.grade.sequences[[i]],\n\t\tprint.sgp.order=TRUE,\n\t\tprint.other.gp=TRUE,\n\t\tverbose.output=TRUE,\n\t\tmax.order.for.percentile=3,\n\t\treturn.norm.group.scale.scores=TRUE,\n\t\treturn.panel.data=TRUE)\n}", sep="")

		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) {
			Rprof("testSGP(0)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
		}

		eval(parse(text=expression.to.evaluate))
	
		### TEST of SGP Variable

		tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 0: Part 1 #####\n")

		if (identical(sum(Demonstration_SGP[['SGPercentiles']][['READING.2015']][['SGP']]), 1707319L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
		}

		### TEST of SGP_ORDER_1 Variable

		if (identical(sum(Demonstration_SGP[['SGPercentiles']][['READING.2015']][['SGP_ORDER_1']]), 1708589L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER_1, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER_1, part 1: FAIL\n")
		}

		### TEST of SCALE_SCORE_PRIOR Variable

		if (identical(sum(Demonstration_SGP[['SGPercentiles']][['READING.2015']][['SCALE_SCORE_PRIOR']]), 20707938)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SCALE_SCORE_PRIOR, part 1: FAIL\n")
		}

		### TEST of SGP_ORDER Variable

		if (identical(as.vector(table(Demonstration_SGP$SGPercentiles[['READING.2015']][['SGP_ORDER']])), c(8666L, 7639L, 17920L))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_ORDER, part 1: FAIL\n")
		}

		### TEST of SGP_LEVEL Variable

		if (identical(as.vector(table(Demonstration_SGP$SGPercentiles[['READING.2015']][['SGP_LEVEL']])), c(6740L, 6824L, 7176L, 6835L, 6650L))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_LEVEL, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_LEVEL, part 1: FAIL\n")
		}

		tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 0: Part 1 #####\n")


		### Part 2

		expression.to.evaluate <- 
			paste("Demonstration_SGP$Panel_Data <- SGPdata::sgpData[,c('ID','GRADE_2011','GRADE_2012','GRADE_2013','GRADE_2014','SS_2011','SS_2012','SS_2013','SS_2014')]\nmy.grade.progressions <- list(3, 3:4, 3:5, 3:6, 4:7)\nfor (i in seq_along(my.grade.progressions)) {\n\tDemonstration_SGP <- studentGrowthProjections(\n\t\tpanel.data=Demonstration_SGP,\n\t\tsgp.labels=list(my.year=2015, my.subject='Reading', my.extra.label='LAGGED'),\n\t\tuse.my.coefficient.matrices=list(my.year=2015, my.subject='Reading'),\n\t\tprojcuts.digits=0,\n\t\tperformance.level.cutscores='DEMO',\n\t\tpercentile.trajectory.values=1:99,\n\t\tlag.increment=1,\n\t\tgrade.progression=my.grade.progressions[[i]])\n}", sep="")

		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) {
			Rprof("testSGP(0)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
		}

		if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")
		eval(parse(text=expression.to.evaluate))
	
		### TEST of dimension of table READING.2015.LAGGED dimensions

		tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 0: Part 2 #####\n")

		if (identical(dim(Demonstration_SGP$SGProjections[['READING.2015.LAGGED']]), c(36478L, 511L))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of READING.2015.LAGGED table dimensions, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of READING.2015.LAGGED table dimensions, part 2: FAIL\n")
		}

		### TEST of LEVEL_1_SGP_TARGET_YEAR_1 Variable

		if (identical(sum(Demonstration_SGP$SGProjections[['READING.2015.LAGGED']][['LEVEL_1_SGP_TARGET_YEAR_1']]), 402866L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable LEVEL_1_SGP_TARGET_YEAR_1, part 2: FAIL\n")
		}

		### TEST of P84_PROJ_YEAR_4 Variable

		if (identical(sum(Demonstration_SGP$SGProjections[['READING.2015.LAGGED']][['P84_PROJ_YEAR_4']], na.rm=TRUE), 10545791)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable P84_PROJ_YEAR_4, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable P84_PROJ_YEAR_4, part 2: FAIL\n")
		}

		tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 0: Part 2 #####\n")
		tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 0 #####\n")
		cat(tmp.messages)

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
		Demonstration_SGP <- tmp.messages <- NULL
		number.cores <- detectCores(logical=FALSE)-1 # adding logical=FALSE seems get physical cores only in Windows (which is good for SNOW/SOCK)

		if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE = 'doParallel', "
		if (toupper(TEST_NUMBER) == "1B") sgp.sqlite <- TRUE else sgp.sqlite <- FALSE

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tdata_supplementary=list(INSTRUCTOR_NUMBER=SGPdata::sgpData_INSTRUCTOR_NUMBER),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.sqlite=", sgp.sqlite, ",\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

		if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

		cat(paste("##### Begin testSGP test number", TEST_NUMBER, "#####\n"), fill=TRUE)
		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) Rprof("testSGP(1)_Memory_Profile.out", memory.profiling=TRUE)
	
		eval(parse(text=expression.to.evaluate))

		if (memory.profile) {
			Rprof(NULL)
		}

		### TEST of SGP variable

		tmp.messages <- ("\t##### Results of testSGP test number 1 #####\n\n")

		if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
		}

		### TEST of SGP_BASELINE variable
	
		if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE: FAIL\n")
		}

		### TEST of SGP_TARGET_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
		}

		### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable
	
		if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 9201802L)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
		}

		### TEST of CATCH_UP_KEEP_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
			tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: FAIL\n")
		}

		### TEST of MOVE_UP_STAY_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(72953, 15043, 18336, 13618))) {
			tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: FAIL\n")
		}

		### TEST of SCALE_SCORE_PRIOR variable

		if (identical(sum(Demonstration_SGP@Data$SCALE_SCORE_PRIOR, na.rm=TRUE), 100865095)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_PRIOR: FAIL\n")
		}

		### TEST of SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1 variable for READING.2014_2015 scale score targets

		if (identical(as.integer(sum(Demonstration_SGP@SGP$SGProjections$READING.2014_2015.LAGGED.TARGET_SCALE_SCORES$SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1)), 18313900L)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable SCALE_SCORE_SGP_TARGET_3_YEAR_PROJ_YEAR_1: FAIL\n")
		}
	
		### TEST of MEDIAN_SGP variable

		if (identical(sum(Demonstration_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS"]]$MEDIAN_SGP, na.rm=TRUE), 9140.5)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: FAIL\n")
		}

		### TEST of PERCENT_AT_ABOVE_PROFICIENT_PRIOR variable

		if (identical(sum(Demonstration_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS"]]$PERCENT_AT_ABOVE_PROFICIENT_PRIOR, na.rm=TRUE), 12894.2)) {
			tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\tTest of variable PERCENT_AT_ABOVE_PROFICIENT_PRIOR: FAIL\n")
		}

		tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number", TEST_NUMBER, "#####\n"))
		cat(tmp.messages)

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

	if (identical(TEST_NUMBER, 2)) TEST_NUMBER <- c("2A", "2B", "2C")

	################################
	### TEST NUMBER 2a
	################################

	if ('2A' %in% toupper(TEST_NUMBER)) {

		options(error=recover)
		options(warn=2)
		Demonstration_SGP <- NULL
		Demonstration_Data_LONG <- subset(SGPdata::sgpData_LONG, YEAR %in% c("2010_2011", "2011_2012", "2012_2013", "2013_2014"))
		Demonstration_Data_LONG_2014_2015 <- subset(SGPdata::sgpData_LONG, YEAR %in% c("2014_2015"))
		number.cores <- detectCores()-1
		tmp.messages <- "##### Begin testSGP test number 2a #####\n\n"

		### Part 1

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) {
			Rprof("testSGP(2a)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
		}

		eval(parse(text=expression.to.evaluate))
	
		### TEST of SGP variable

		tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 2a: Part 1 #####\n")

		if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 1: FAIL\n")
		}

		### TEST of SGP_BASELINE variable

		if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 5667488L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 1: FAIL\n")
		}

		### TEST of SGP_TARGET_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 5245437L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 1: FAIL\n")
		}

		### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 6088129L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: FAIL\n")
		}

		### TEST of CATCH_UP_KEEP_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(27122, 6990, 24358, 55283))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 1: FAIL\n")
		}

		### TEST of MOVE_UP_STAY_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(48152, 10396, 12150, 8943))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 1: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 1: FAIL\n")
		}

		tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 2a: Part 1 #####\n")


		### Part 2

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2014_2015,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) {
			Rprof("testSGP(2a)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
		}

		eval(parse(text=expression.to.evaluate))

		### TEST of variable values

		tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 2a: Part 2 #####\n")

		### TEST of SGP variable

		if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
		}

		### TEST of SGP_BASELINE variable

		if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
		}

		### TEST of SGP_TARGET variable

		if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
		}

		### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

		if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 9201802L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
		}

		### TEST of CATCH_UP_KEEP_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL\n")
		}

		### TEST of MOVE_UP_STAY_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(72953, 15043, 18336, 13618))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: FAIL\n")
		}

		tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 2a: Part 2 #####\n")

		tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 2a #####\n")
		cat(tmp.messages)

	} ### End TEST_NUMBER 2a


	################################
	### TEST NUMBER 2b, 2c, 2d
	################################
	if (any(toupper(TEST_NUMBER) %in% c('2B', '2C', '2D'))) {

	for (i in toupper(TEST_NUMBER)) {
		options(error=recover)
		options(warn=2)
		Demonstration_SGP <- ID <- CONTENT_AREA <- NULL
		Demonstration_Data_LONG_2014_2015 <- subset(SGPdata::sgpData_LONG, YEAR %in% c("2014_2015"))
		number.cores <- detectCores()-1

		############################################################
		### Part 1: Required for all Tests 2b, 2c, and 2d
		############################################################

		tmp.messages <- paste("##### Begin testSGP test number", capwords(i), "#####\n\n")
		tmp.messages <- c(tmp.messages, paste("\t##### Begin testSGP test number ", capwords(i), ": Part 1 #####\n\n", sep=""))

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='2014_2015',\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, "))\n)\n", sep="")

		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) {
			Rprof("testSGP(2)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
		}

		eval(parse(text=expression.to.evaluate))
	
		tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number ", capwords(i), ": Part 1 #####\n", sep=""))


		#############################################################
		### Part 2 
		#############################################################

		tmp.messages <- c(tmp.messages, paste("\n\t##### Begin testSGP test number ", capwords(i), ": Part 2 #####\n\n", sep=""))

		### TEST 2b ###
		if (i=='2B') {
			Demonstration_Data_LONG_2014_2015 <- subset(SGPdata::sgpData_LONG, YEAR %in% c("2014_2015"))

			expression.to.evaluate <- 
				paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2014_2015,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

			cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

			eval(parse(text=expression.to.evaluate))
		} ### End TEST 2b

		### TEST 2c ###
		if (i=='2C') {
			Demonstration_Data_LONG_2014_2015 <- subset(SGPdata::sgpData_LONG, YEAR %in% c("2014_2015"))

			expression.to.evaluate <- 
				paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2014_2015,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

			cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

			eval(parse(text=expression.to.evaluate))
		} ### End TEST 2c

		### TEST 2d ###
		if (i=='2D') {
			Demonstration_Data_LONG_2014_2015 <- subset(SGPdata::sgpData_LONG, YEAR %in% c("2014_2015"))
			tmp.2014_2015.ids <- sort(unique(Demonstration_Data_LONG_2014_2015[['ID']]))
			tmp.group.1 <- tmp.2014_2015.ids[1:150]
			tmp.group.2 <- tmp.2014_2015.ids[151:250]
			with_sgp_data_LONG <- subset(Demonstration_Data_LONG_2014_2015, ID %in% tmp.group.1 | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS"))
			Demonstration_SGP@Data <- subset(Demonstration_SGP@Data, !((ID %in% tmp.group.1 & YEAR=="2014_2015") | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS" & YEAR=="2014_2015")))
			Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2014_2015 <- subset(Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2014_2015, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2014_2015.BASELINE <- subset(Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2014_2015.BASELINE, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015 <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015.BASELINE, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015.LAGGED <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015.LAGGED, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015.LAGGED.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2014_2015.LAGGED.BASELINE, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGPercentiles$READING.2014_2015 <- subset(Demonstration_SGP@SGP$SGPercentiles$READING.2014_2015, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGPercentiles$READING.2014_2015.BASELINE <- subset(Demonstration_SGP@SGP$SGPercentiles$READING.2014_2015.BASELINE, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2014_2015 <- subset(Demonstration_SGP@SGP$SGProjections$READING.2014_2015, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2014_2015.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$READING.2014_2015.BASELINE, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2014_2015.LAGGED <- subset(Demonstration_SGP@SGP$SGProjections$READING.2014_2015.LAGGED, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2014_2015.LAGGED.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$READING.2014_2015.LAGGED.BASELINE, !ID %in% tmp.group.1)

			expression.to.evaluate <- 
				paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=with_sgp_data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

			cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

			eval(parse(text=expression.to.evaluate))
		} ### End TEST 2d

		### TEST of SGP variable

		tmp.messages <- c(tmp.messages, paste("\t\t##### Results of testSGP test number", capwords(i), "#####\n"))

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2014_2015"]$SGP, na.rm=TRUE), 2896606L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
		}

		### TEST of SGP_BASELINE variable

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2014_2015"]$SGP_BASELINE, na.rm=TRUE), 2906337L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
		}

		### TEST of SGP_TARGET_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2014_2015"]$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
		}

		### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2014_2015"]$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
		}

		### TEST of CATCH_UP_KEEP_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR=="2014_2015"]$CATCH_UP_KEEP_UP_STATUS)), c(13977, 3847, 11202, 29107))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL\n")
		}

		### TEST of MOVE_UP_STAY_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR=="2014_2015"]$MOVE_UP_STAY_UP_STATUS)), c(24801, 4647, 6186, 4675))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: FAIL\n")
		}

		tmp.messages <- c(tmp.messages, paste("\t##### End testSGP test number ", capwords(i), ": Part 2 #####\n", sep=""))
		tmp.messages <- c(tmp.messages, paste("\n##### End testSGP test number", capwords(i), "#####\n"))
		cat(tmp.messages)
	} ### End for (i in TEST_NUMBER)
	} ### End TEST_NUMBER 2b, 2c, 2d

	#######################################################################################################################################################
	###
	### TEST NUMBER 3: Test of EOCT like student growth projections  
	###
	#######################################################################################################################################################

	if (3 %in% TEST_NUMBER) {

	options(error=recover)
	options(warn=2)
	number.cores <- detectCores()-1
	Demonstration_SGP <- tmp.messages <- NULL
	sgpData_LONG <- SGPdata::sgpData_LONG

	### Add EOCT courses to sgpData_LONG

	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '9'] <- 'ALGEBRA_I'
	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '10'] <- 'ALGEBRA_II'
	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '9'] <- 'GRADE_9_LIT'
	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '10'] <- 'AMERICAN_LIT'
	sgpData_LONG$GRADE_REPORTED <- sgpData_LONG$GRADE
	sgpData_LONG$GRADE[sgpData_LONG$CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II', 'GRADE_9_LIT', 'AMERICAN_LIT')] <-  'EOCT'

	### Modify SGPstateData

SGPstateData[["DEMO"]][["Student_Report_Information"]] <-
	list(
	# Transformed_Achievement_Level_Cutscores=list(MATHEMATICS=c(0,100,200,300,400), READING=c(0,100,200,300,400), GRADE_9_LIT=c(0,100,200,300,400), AMERICAN_LIT=c(0,100,200,300,400), ALGEBRA_I=c(0,100,200,300,400), ALGEBRA_II=c(0,100,200,300,400)), ### FOR TESTING
	# Transformed_Achievement_Level_Cutscores_gaPlot=list(MATHEMATICS=c(0,100,200,300,400), READING=c(0,100,200,300,400), GRADE_9_LIT=c(0,100,200,300,400), AMERICAN_LIT=c(0,100,200,300,400), ALGEBRA_I=c(0,100,200,300,400), ALGEBRA_II=c(0,100,200,300,400)), ### FOR TESTING
	Vertical_Scale="Yes",
	Content_Areas_Labels=list(MATHEMATICS="Math", READING="Reading", GRADE_9_LIT="Grade 9 Lit", AMERICAN_LIT="American Lit", ALGEBRA_I="Algebra I", ALGEBRA_II="Algebra II"),
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

	READING_2014_2015.config <- list(
		READING.2014_2015 = list(
			sgp.content.areas=c('READING', 'READING', 'READING', 'READING', 'READING'),
			sgp.panel.years=c('2010_2011', '2011_2012', '2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
	)

	GRADE_9_LIT_2014_2015.config <- list(
		GRADE_9_LIT.2014_2015 = list(
			sgp.content.areas=c('READING', 'READING', 'READING', 'READING', 'GRADE_9_LIT'),
			sgp.panel.years=c('2010_2011', '2011_2012', '2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(c(5:8, 'EOCT')))
	)
	
	AMERICAN_LIT_2014_2015.config <- list(
		AMERICAN_LIT.2014_2015 = list(
			sgp.content.areas=c('READING', 'READING', 'READING', 'GRADE_9_LIT', 'AMERICAN_LIT'),
			sgp.panel.years=c('2010_2011', '2011_2012', '2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
	)

	MATHEMATICS_2014_2015.config <- list(
		MATHEMATICS.2014_2015 = list(
			sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS'),
			sgp.panel.years=c('2010_2011', '2011_2012', '2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
	)

	ALGEBRA_I_2014_2015.config <- list(
		 ALGEBRA_I.2014_2015 = list(
			sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I'),
			sgp.panel.years=c('2010_2011', '2011_2012', '2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(c(5:8, 'EOCT')))
	)

	ALGEBRA_II_2014_2015.config <- list(
		 ALGEBRA_II.2014_2015 = list(
			sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
			sgp.panel.years=c('2010_2011', '2011_2012', '2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
	)

	sgp.config <- c(READING_2014_2015.config, MATHEMATICS_2014_2015.config, GRADE_9_LIT_2014_2015.config, AMERICAN_LIT_2014_2015.config, ALGEBRA_I_2014_2015.config, ALGEBRA_II_2014_2015.config)
	 
	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'summarizeSGP', 'visualizeSGP'),\n\tsimulate.sgps=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

	cat("##### Begin testSGP test number 3 #####\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(3)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
	eval(parse(text=expression.to.evaluate))

	if (memory.profile) {
		Rprof(NULL)
	}

	### TEST of SGP variable

	tmp.messages <- ("\t##### Results of testSGP test number 3 #####\n\n")
	
	if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 2896606L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP: FAIL\n")
	}

	### TEST of SGP_TARGET_3_YEAR variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR: FAIL\n")
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(13977, 3847, 11202, 29107))) {
		tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS: FAIL\n")
	}

	### TEST of MOVE_UP_STAY_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(24801, 4647, 6186, 4675))) {
		tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS: FAIL\n")
	}

	tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 3 #####\n")
	cat(tmp.messages)
	} ### End TEST_NUMBER 3


	#######################################################################################################################################################
	###
	### TEST NUMBER 4: Test of SIMEX Measurement Error Correction Functionality
	###
	#######################################################################################################################################################

	if (4 %in% TEST_NUMBER) {

	sgpData_LONG <- SGPdata::sgpData_LONG

	###  This test requires the DEMO SIMEX matrices to be loaded manually.
	# SGPstateData[["DEMO"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- c(SGPstateData[["DEMO"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]], DEMO_SIMEX_Baseline_Matrices)
	if (!any(grepl(".BASELINE.SIMEX", names(SGPstateData[["DEMO"]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]])))) {
		stop("\n\tThis test requires the DEMO SIMEX matrices to be loaded manually.\n\t\t
			SGPstateData[['DEMO']][['Baseline_splineMatrix']][['Coefficient_Matrices']] <- c(SGPstateData[['DEMO']][['Baseline_splineMatrix']][['Coefficient_Matrices']], DEMO_SIMEX_Baseline_Matrices)\n\n")
	}
	
	SGPstateData[["DEMO"]][["SGP_Configuration"]][["max.order.for.percentile"]] <- 2
	SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]] <- data.table(SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]])
	SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][which(GRADE==9 & CONTENT_AREA == "READING"), CONTENT_AREA := "GRADE_9_LIT"]
	SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][which(GRADE==10 & CONTENT_AREA == "READING"), CONTENT_AREA := "AMERICAN_LIT"]
	SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][which(GRADE==9 & CONTENT_AREA == "MATHEMATICS"), CONTENT_AREA := "ALGEBRA_I"]
	SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][which(GRADE==10 & CONTENT_AREA == "MATHEMATICS"), CONTENT_AREA := "ALGEBRA_II"]
	SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]][which(GRADE %in% 9:10), GRADE := "EOCT"]

table(SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]]$GRADE, SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["CSEM"]]$CONTENT_AREA)
	### Add EOCT courses to sgpData_LONG

	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '9'] <- 'ALGEBRA_I'
	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$GRADE == '10'] <- 'ALGEBRA_II'
	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '9'] <- 'GRADE_9_LIT'
	sgpData_LONG$CONTENT_AREA[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$GRADE == '10'] <- 'AMERICAN_LIT'
	sgpData_LONG$GRADE_REPORTED <- sgpData_LONG$GRADE
	sgpData_LONG$GRADE[sgpData_LONG$CONTENT_AREA %in% c('ALGEBRA_I', 'ALGEBRA_II', 'GRADE_9_LIT', 'AMERICAN_LIT')] <-  'EOCT'


	options(error=recover) # Don't use options(warn=2) - get warnings about knots and bounds from BASELINE SIMEX
	number.cores <- detectCores(logical=FALSE)-1
	Demonstration_SGP <- tmp.messages <- NULL

	if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE = 'doParallel', "

	expression.to.evaluate <- 
		paste("\nDemonstration_SGP <- prepareSGP(sgpData_LONG, create.additional.variables=FALSE)\n\nDemonstration_SGP <- analyzeSGP(\n\tsgp_object= Demonstration_SGP,\n\tyears='2013_2014',\n\tcontent_areas='READING',\n\tsgp.percentiles.baseline.max.order=2,\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=TRUE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,\n\tcalculate.simex=TRUE,\n\tcalculate.simex.baseline=TRUE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, "))\n)\n", sep="")
		
		#parallel.config=list(BACKEND='PARALLEL', WORKERS=list(SIMEX=", number.cores, ", TAUS=", number.cores, "))\n)\n", sep="")

	cat("#####  Begin testSGP test number 4, Part 1                                    #####", fill=TRUE)
	cat("##     Grade-Level, Cohort and Baseline Tests with auto sgp.config construction. ##\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(4.1)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
	eval(parse(text=expression.to.evaluate))

	### TEST of SGP_SIMEX variable

	tmp.messages <- ("\t##            Results of testSGP test number 4, Part 1            ##\n\n")
	
	if (identical(sum(Demonstration_SGP@SGP$SGPercentiles$READING.2014_2015$SGP_SIMEX), 1029023L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_SIMEX: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_SIMEX: FAIL\n")
	}

	### TEST of SGP_SIMEX_BASELINE variable
	if (identical(sum(Demonstration_SGP@SGP$SGPercentiles$READING.2014_2015.BASELINE$SGP_SIMEX_BASELINE), 1034475L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_SIMEX_BASELINE: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_SIMEX_BASELINE: FAIL\n")
	}
	tmp.messages <- c(tmp.messages, "\n\t#####         End testSGP test number 4, Part 1             #####\n")
	cat(tmp.messages)

	sgp.config <- list(
		AMERICAN_LIT.2014_2015 = list(
			sgp.content.areas=c('READING', 'GRADE_9_LIT', 'AMERICAN_LIT'),
			sgp.panel.years=c('2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(c(8, 'EOCT', 'EOCT')),
			sgp.calculate.simex.baseline=TRUE),
	
		 ALGEBRA_II.2014_2015 = list(
			sgp.content.areas=c('MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
			sgp.panel.years=c('2012_2013', '2013_2014', '2014_2015'),
			sgp.grade.sequences=list(c(8, 'EOCT', 'EOCT')),
			sgp.calculate.simex.baseline=TRUE)
	)

	expression.to.evaluate <- 
		paste("\nDemonstration_SGP <- analyzeSGP(\n\tsgp_object= Demonstration_SGP,\n\tsgp.config=sgp.config,\n\tsgp.percentiles=FALSE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=TRUE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tsimulate.sgps=FALSE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(BASELINE_PERCENTILES=", number.cores, "))\n)\nDemonstration_SGP <- combineSGP(Demonstration_SGP)", sep="")

	if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

	cat("\n\t#####        Begin testSGP test number 4, Part 2            #####\n", fill=TRUE)
	cat("\t##           EOCT Baseline Tests with custom sgp.config.       ##\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(4.2)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
	eval(parse(text=expression.to.evaluate))

	### TEST of SGP_SIMEX_BASELINE variable

	tmp.messages <- ("\t##            Results of testSGP test number 4, Part 2            ##\n\n")
	
	if (identical(sum(Demonstration_SGP@SGP$SGPercentiles$AMERICAN_LIT.2014_2015.BASELINE$SGP_SIMEX_BASELINE), 218029L)) {
		tmp.messages <- c(tmp.messages, "\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of AMERICAN_LIT SGP_SIMEX_BASELINE: FAIL\n")
	}
	if (identical(sum(Demonstration_SGP@SGP$SGPercentiles$ALGEBRA_II.2014_2015.BASELINE$SGP_SIMEX_BASELINE), 212985L)) {
		tmp.messages <- c(tmp.messages, "\tTest of ALGEBRA_II SGP_SIMEX_BASELINE: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of ALGEBRA_II SGP_SIMEX_BASELINE: FAIL\n")
	}
	if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_BASELINE, na.rm=TRUE), 1465489L)) {
		tmp.messages <- c(tmp.messages, "\tTest of @Data variable SGP_SIMEX_BASELINE: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of @Data variable SGP_SIMEX_BASELINE: FAIL\n")
	}
	tmp.messages <- c(tmp.messages, "\n\t#####         End testSGP test number 4, Part 2             #####\n\n", "#####  End testSGP test number 4                                   #####\n")
	cat(tmp.messages)
	} ### End TEST_NUMBER 4


	#######################################################################################################################################################
	###
	### TEST NUMBER 5: Test of assessment change functionality
	###
	#######################################################################################################################################################

	if (5 %in% TEST_NUMBER) {

	options(error=recover)
	options(warn=2)
	number.cores <- detectCores()-1
	Demonstration_SGP <- ACHIEVEMENT_LEVEL <- HIGH_NEED_STATUS <- tmp.messages <- NULL
	sgpData_LONG <- SGPdata::sgpData_LONG
	if (is.null(test.option)) test.option <- c("Yes", "Yes")

	##############################################################################
	##### STEP 1: Run analyses for year prior to assessment change in 2014-2015
	##############################################################################

		##### Create LONG Data set for STEP 1 analysis.

		sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$YEAR == '2014_2015'] <- 
			sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'MATHEMATICS' & sgpData_LONG$YEAR == '2014_2015'] + 1000
		sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$YEAR == '2014_2015'] <- 
			sgpData_LONG$SCALE_SCORE[sgpData_LONG$CONTENT_AREA == 'READING' & sgpData_LONG$YEAR == '2014_2015'] + 1200

		Demonstration_Data_LONG <- as.data.table(subset(sgpData_LONG, YEAR!="2014_2015"))
		Demonstration_Data_LONG_2014_2015 <- as.data.table(subset(sgpData_LONG, YEAR=="2014_2015"))[,ACHIEVEMENT_LEVEL:=NULL]
		Demonstration_Data_LONG_2014_2015 <- prepareSGP(Demonstration_Data_LONG_2014_2015)@Data
		Demonstration_Data_LONG_2014_2015[, HIGH_NEED_STATUS:=NULL]
		setcolorder(Demonstration_Data_LONG_2014_2015, names(Demonstration_Data_LONG))

		### Calculate SGPs

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.sqlite=", sgp.sqlite, ",\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")


	##############################################################################
	##### STEP 2: Create SGPs for assessment transtion year
	##############################################################################

		##### Modify SGPstateData

		SGPstateData[["DEMO"]][["Achievement"]][["Knots_Boundaries"]] <- c(
			SGPstateData[["DEMO"]][["Achievement"]][["Knots_Boundaries"]],
			list(MATHEMATICS.2014_2015=list(
				boundaries_3=c(1150, 1700),
				boundaries_4=c(1180, 1780),
				boundaries_5=c(1220, 1800),
				boundaries_6=c(1240, 1830),
				boundaries_7=c(1280, 1860),
				boundaries_8=c(1310, 1890),
				boundaries_9=c(1340, 1920),
				boundaries_10=c(1370, 1950),
				knots_3=c(1392, 1440, 1481, 1529),
				knots_4=c(1425, 1470, 1506, 1546),
				knots_5=c(1452, 1495, 1530, 1569),
				knots_6=c(1465, 1509, 1546, 1588),
				knots_7=c(1490, 1530, 1565, 1600),
				knots_8=c(1500, 1545, 1580, 1620),
				knots_9=c(1515, 1560, 1595, 1630),
				knots_10=c(1530, 1575, 1610, 1645),
				loss.hoss_3=c(1150, 1700),
				loss.hoss_4=c(1180, 1780),
				loss.hoss_5=c(1220, 1800),
				loss.hoss_6=c(1240, 1830),
				loss.hoss_7=c(1280, 1860),
				loss.hoss_8=c(1310, 1890),
				loss.hoss_9=c(1340, 1920),
				loss.hoss_10=c(1370, 1950)),
		READING.2014_2015=list(
				boundaries_3=c(1350, 1995),
				boundaries_4=c(1380, 2140),
				boundaries_5=c(1420, 2155),
				boundaries_6=c(1460, 2170),
				boundaries_7=c(1500, 2180),
				boundaries_8=c(1530, 2190),
				boundaries_9=c(1550, 2195),
				boundaries_10=c(1570, 2199),
				knots_3=c(1710, 1750, 1780, 1815),
				knots_4=c(1742, 1780, 1806, 1835),
				knots_5=c(1762, 1802, 1832, 1865),
				knots_6=c(1775, 1815, 1845, 1875),
				knots_7=c(1786, 1825, 1855, 1890),
				knots_8=c(1805, 1842, 1870, 1902),
				knots_9=c(1820, 1855, 1880, 1906),
				knots_10=c(1842, 1875, 1900, 1930),
				loss.hoss_3=c(1350, 1995),
				loss.hoss_4=c(1380, 2140),
				loss.hoss_5=c(1420, 2155),
				loss.hoss_6=c(1460, 2170),
				loss.hoss_7=c(1500, 2180),
				loss.hoss_8=c(1530, 2190),
				loss.hoss_9=c(1550, 2195),
				loss.hoss_10=c(1570, 2199))))
	
		SGPstateData[["DEMO"]][["Achievement"]][["Cutscores"]] <- c(
			SGPstateData[["DEMO"]][["Achievement"]][["Cutscores"]],
			list(MATHEMATICS.2014_2015=list(
				GRADE_3=c(1335, 1419, 1510, 1560),
				GRADE_4=c(1383, 1455, 1538, 1588),
				GRADE_5=c(1422, 1494, 1562, 1612),
				GRADE_6=c(1454, 1520, 1589, 1639),
				GRADE_7=c(1487, 1559, 1614, 1664),
				GRADE_8=c(1521, 1577, 1628, 1678),
				GRADE_9=c(1548, 1602, 1652, 1702),
				GRADE_10=c(1562, 1627, 1692, 1742)),
			READING.2014_2015=list(
				GRADE_3=c(1666, 1726, 1856, 1906),
				GRADE_4=c(1717, 1772, 1871, 1921),
				GRADE_5=c(1738, 1788, 1891, 1941),
				GRADE_6=c(1743, 1800, 1896, 1946),
				GRADE_7=c(1767, 1820, 1916, 1966),
				GRADE_8=c(1778, 1832, 1924, 1974),
				GRADE_9=c(1785, 1842, 1939, 1989),
				GRADE_10=c(1807, 1863, 1947, 1997))))
	
		SGPstateData[["DEMO"]][["Achievement"]][["Levels"]] <-
			list(
				Labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"),
				Proficient=c("Not Proficient", "Not Proficient", "Not Proficient", "Proficient", "Proficient"))

		SGPstateData[["DEMO"]][["Growth"]][["System_Type"]] <- "Cohort Referenced"

		SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]] <-
			list(
				Assessment_Abbreviation="DEMO_OLD",
				Assessment_Abbreviation.2014_2015="DEMO_NEW",
				Assessment_Name="Old Demonstration Student Assessment Program",
				Assessment_Name.2014_2015="New Demonstration Student Assessment Program",
				Achievement_Levels=list(
					Labels=c("Unsatisfactory", "Partially Proficient", "Proficient", "Advanced", "No Score"),
					Proficient=c("Not Proficient", "Not Proficient", "Proficient", "Proficient", NA)),
				Achievement_Levels.2014_2015=list(
					Labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"),
					Proficient=c("Not Proficient", "Not Proficient", "Not Proficient", "Proficient", "Proficient")),
				Achievement_Level_Labels=list(
					"Unsatisfactory"="Unsatisfactory",
					"Part Proficient"="Proficient",
					"Proficient"="Proficient",
					"Advanced"="Advanced"),
				Achievement_Level_Labels.2014_2015=list(
					"Level 1"="Level 1",
					"Level 2"="Level 2",
					"Level 3"="Level 3",
					"Level 4"="Level 4",
					"Level 5"="Level 5"),
				Content_Areas_Labels=list(MATHEMATICS="Math", READING="Reading"),
				Content_Areas_Labels.2014_2015=list(MATHEMATICS="Math", READING="Reading"),
				Year="2014_2015"
		)

		if (test.option==c("Yes", "Yes")) {
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale"]] <- "Yes"
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale.2014_2015"]] <- "Yes"
		}
		if (test.option==c("No", "Yes")) {
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale"]] <- "No"
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale.2014_2015"]] <- "Yes"
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Transformed_Achievement_Level_Cutscores"]] <- 
				list(MATHEMATICS=c(100,200,300,400,500), READING=c(100,200,300,400,500))
		}
		if (test.option==c("No", "No")) {
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale"]] <- "No"
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Vertical_Scale.2014_2015"]] <- "No"
			SGPstateData[["DEMO"]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Transformed_Achievement_Level_Cutscores"]] <- 
				list(MATHEMATICS=c(100,200,300,400,500), READING=c(100,200,300,400,500))
		}

		### updateSGP

		Demonstration_SGP <- updateSGP(
			Demonstration_SGP,
			Demonstration_Data_LONG_2014_2015,
			sgp.percentiles=TRUE,
			sgp.projections=TRUE,
			sgp.projections.lagged=TRUE,
			sgp.percentiles.baseline=FALSE,
			sgp.projections.baseline=FALSE,
			sgp.projections.lagged.baseline=FALSE,
			sgp.target.scale.scores=TRUE,
			save.intermediate.results=FALSE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=4, PROJECTIONS=4, LAGGED_PROJECTIONS=4, SGP_SCALE_SCORE_TARGETS=4, SUMMARY=4)))

	} ### End TEST_NUMBER 5


	#######################################################################################################################################################
	###
	### TEST NUMBER 6: Test of baseline coefficient matrix generation functionality
	###
	#######################################################################################################################################################

	if (6 %in% TEST_NUMBER) {

	options(error=recover)
	options(warn=2)
	number.cores <- detectCores()-1
        if (.Platform$OS.type == "unix") tmp.backend <- "'PARALLEL', " else tmp.backend <- "'FOREACH', TYPE = 'doParallel', "
	Demonstration_SGP <- tmp.messages <- NULL

	### Modify SGPstateData

	SGPstateData[['DEMO']][['Baseline_splineMatrix']] <- NULL

	expression.to.evaluate <- 
		paste("\nDemonstration_SGP <- abcSGP(\n\tsgp_object=SGPdata::sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='2014_2015',\n\tcontent_areas='MATHEMATICS',\n\tsgp.percentiles=FALSE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=TRUE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tparallel.config=list(BACKEND=", tmp.backend, "WORKERS=list(BASELINE_PERCENTILES=", number.cores, ", BASELINE_MATRICES=", number.cores, "))\n)", sep="")

	if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "dir.create('Data', showWarnings=FALSE)", "save(Demonstration_SGP, file='Data/Demonstration_SGP.Rdata')", sep="\n")

	cat("#####        Begin testSGP test number 6            #####\n", fill=TRUE)
	cat("##           Basic Baseline Coefficient Matrix Test.       ##\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(6)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
	eval(parse(text=expression.to.evaluate))

	### TEST of SGP_BASELINE variable

	tmp.messages <- ("\t#####            Results of testSGP test number 6           #####\n\n")
	
	if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 1469353L)) {
		tmp.messages <- c(tmp.messages, "\tTest of SGP_BASELINE: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of SGP_BASELINE: FAIL\n")
	}
	tmp.messages <- c(tmp.messages, "\n\t#####         End testSGP test number 6             #####\n")
	cat(tmp.messages)
	} ### End TEST_NUMBER 6
} ### END testSGP Function
