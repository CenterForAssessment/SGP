`testSGP` <- 
function(
	TEST_NUMBER,
	save.results=TRUE,
	memory.profile=FALSE) {

	YEAR <- NULL

	if (missing(TEST_NUMBER)) {
		message("\ttestSGP carries out testing of SGP package. Tests currently included in testSGP:\n")
		message("\t\t1. abcSGP test using all available years.")
		message("\t\t2. abcSGP test using all available years except most recent followed by an updated analysis using the most recent year's data.")
	}

	if (identical(TEST_NUMBER, 2)) TEST_NUMBER <- c("2A", "2B", "2C")

	suppressPackageStartupMessages(require(SGPdata))

	#######################################################################################################################################################
	###
	### TEST NUMBER 1: Test of abcSGP on sgpData_LONG
	###
	#######################################################################################################################################################

	if (1 %in% TEST_NUMBER) {

	options(error=recover)
	options(warn=2)
	suppressPackageStartupMessages(require(parallel))
	Demonstration_SGP <- tmp.messages <- NULL
	number.cores <- detectCores()-1

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tdata_supplementary=list(INSTRUCTOR_NUMBER=sgpData_INSTRUCTOR_NUMBER),\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Demonstration_SGP.Rdata')", sep="\n")

	cat("##### Begin testSGP test number 1 #####\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(1)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
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

	### TEST of MEDIAN_SGP variable

	if (identical(sum(Demonstration_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__SCHOOL_ENROLLMENT_STATUS"]]$MEDIAN_SGP, na.rm=TRUE), 9140.5)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable MEDIAN_SGP: FAIL\n")
	}

	tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 1 #####\n")
	cat(tmp.messages)

	} ### End TEST_NUMBER 1


	##########################################################################################################################################################################
	###
	### TEST NUMBER 2: Various tests of updateSGP functionality.
	###
	### TEST NUMBER 2a: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2008-2009 to 2011-2012 followed by adding with_sgp_data_LONG 2012-2013 using
	###			overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=FALSE.
	### TEST NUMBER 2b: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2008-2009 to 2012-2013 followed by adding with_sgp_data_LONG 2012-2013 using
	###			overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=FALSE.
	### TEST NUMBER 2c: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2008-2009 to 2012-2013 followed by adding with_sgp_data_LONG 2012-2013 using
	###			overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=TRUE.
	### TEST NUMBER 2d: Test of updateSGP performing SGP analyses in two steps: Create what_sgp_object: 2008-2009 to 2012-2013 followed by adding with_sgp_data_LONG 2012-2013 using
	###			overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=TRUE.
	###
	#########################################################################################################################################################################

	################################
	### TEST NUMBER 2a
	################################
	if ('2A' %in% toupper(TEST_NUMBER)) {

		options(error=recover)
		options(warn=2)
		suppressPackageStartupMessages(require(parallel))
		Demonstration_SGP <- NULL
		Demonstration_Data_LONG <- subset(sgpData_LONG, YEAR %in% c("2008_2009", "2009_2010", "2010_2011", "2011_2012"))
		Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))
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

		tmp.messages <- c(tmp.messages, "\t\t##### Results of testSGP test number 2a: Part 1 #####\n")

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
			paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2012_2013,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

		cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

		if (memory.profile) {
			Rprof("testSGP(2a)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
		}

		eval(parse(text=expression.to.evaluate))

		### TEST of variable values

		tmp.messages <- c(tmp.messages, "\n\t\t##### Results of testSGP test number 2a: Part 2 #####\n")

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
		suppressPackageStartupMessages(require(parallel))
		Demonstration_SGP <- ID <- CONTENT_AREA <- NULL
		Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))
		number.cores <- detectCores()-1

		############################################################
		### Part 1: Required for all Tests 2b, 2c, and 2d
		############################################################

		tmp.messages <- paste("##### Begin testSGP test number", capwords(i), "#####\n\n")
		tmp.messages <- c(tmp.messages, paste("\t##### Begin testSGP test number ", capwords(i), ": Part 1 #####\n\n", sep=""))

		expression.to.evaluate <- 
			paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tyears='2012_2013',\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, "))\n)\n", sep="")

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
			Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))

			expression.to.evaluate <- 
				paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2012_2013,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

			cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

			eval(parse(text=expression.to.evaluate))
		} ### End TEST 2b

		### TEST 2c ###
		if (i=='2C') {
			Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))

			expression.to.evaluate <- 
				paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2012_2013,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\toverwrite.existing.data=TRUE,\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

			cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

			eval(parse(text=expression.to.evaluate))
		} ### End TEST 2c

		### TEST 2d ###
		if (i=='2D') {
			Demonstration_Data_LONG <- subset(sgpData_LONG, YEAR %in% c("2008_2009", "2009_2010", "2010_2011", "2011_2012"))
			Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))
			tmp.2012_2013.ids <- sort(unique(Demonstration_Data_LONG_2012_2013[['ID']]))
			tmp.group.1 <- tmp.2012_2013.ids[1:150]
			tmp.group.2 <- tmp.2012_2013.ids[151:250]
			with_sgp_data_LONG <- subset(Demonstration_Data_LONG_2012_2013, ID %in% tmp.group.1 | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS"))
			Demonstration_SGP@Data <- subset(Demonstration_SGP@Data, !((ID %in% tmp.group.1 & YEAR=="2012_2013") | (ID %in% tmp.group.2 & CONTENT_AREA=="MATHEMATICS" & YEAR=="2012_2013")))
			Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2012_2013 <- subset(Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2012_2013, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2012_2013.BASELINE <- subset(Demonstration_SGP@SGP$SGPercentiles$MATHEMATICS.2012_2013.BASELINE, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013 <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013.BASELINE, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013.LAGGED <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013.LAGGED, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013.LAGGED.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$MATHEMATICS.2012_2013.LAGGED.BASELINE, !ID %in% c(tmp.group.1, tmp.group.2))
			Demonstration_SGP@SGP$SGPercentiles$READING.2012_2013 <- subset(Demonstration_SGP@SGP$SGPercentiles$READING.2012_2013, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGPercentiles$READING.2012_2013.BASELINE <- subset(Demonstration_SGP@SGP$SGPercentiles$READING.2012_2013.BASELINE, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2012_2013 <- subset(Demonstration_SGP@SGP$SGProjections$READING.2012_2013, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2012_2013.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$READING.2012_2013.BASELINE, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2012_2013.LAGGED <- subset(Demonstration_SGP@SGP$SGProjections$READING.2012_2013.LAGGED, !ID %in% tmp.group.1)
			Demonstration_SGP@SGP$SGProjections$READING.2012_2013.LAGGED.BASELINE <- subset(Demonstration_SGP@SGP$SGProjections$READING.2012_2013.LAGGED.BASELINE, !ID %in% tmp.group.1)

			expression.to.evaluate <- 
				paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=with_sgp_data_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsgp.use.my.coefficient.matrices=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

			cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

			eval(parse(text=expression.to.evaluate))
		} ### End TEST 2d

		### TEST of SGP variable

		tmp.messages <- c(tmp.messages, paste("\t\t##### Results of testSGP test number", capwords(i), "#####\n"))

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2012_2013"]$SGP, na.rm=TRUE), 2896606L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP, part 2: FAIL\n")
		}

		### TEST of SGP_BASELINE variable

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2012_2013"]$SGP_BASELINE, na.rm=TRUE), 2906337L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_BASELINE, part 2: FAIL\n")
		}

		### TEST of SGP_TARGET_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2012_2013"]$SGP_TARGET_3_YEAR, na.rm=TRUE), 2551187L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
		}

		### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

		if (identical(sum(Demonstration_SGP@Data[YEAR=="2012_2013"]$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 3113673L)) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 2: FAIL\n")
		}

		### TEST of CATCH_UP_KEEP_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR=="2012_2013"]$CATCH_UP_KEEP_UP_STATUS)), c(13977, 3847, 11202, 29107))) {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK\n")
		} else {
			tmp.messages <- c(tmp.messages, "\t\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL\n")
		}

		### TEST of MOVE_UP_STAY_UP_STATUS variable

		if (identical(as.numeric(table(Demonstration_SGP@Data[YEAR=="2012_2013"]$MOVE_UP_STAY_UP_STATUS)), c(24801, 4647, 6186, 4675))) {
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
	suppressPackageStartupMessages(require(parallel))
	number.cores <- detectCores()-1
	Demonstration_SGP <- tmp.messages <- NULL

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
	# Transformed_Achievement_Level_Cutscores=list(MATHEMATICS=c(0,100,200,300,400), READING=c(0,100,200,300,400, GRADE_9_LIT=c(0,100,200,300,400), AMERICAN_LIT=c(0,100,200,300,400), ALGEBRA_I=c(0,100,200,300,400), ALGEBRA_II=c(0,100,200,300,400))), ### FOR TESTING
	# Transformed_Achievement_Level_Cutscores_gaPlot=list(MATHEMATICS=c(0,100,200,300,400), READING=c(0,100,200,300,400, GRADE_9_LIT=c(0,100,200,300,400), AMERICAN_LIT=c(0,100,200,300,400), ALGEBRA_I=c(0,100,200,300,400), ALGEBRA_II=c(0,100,200,300,400))), ### FOR TESTING
	Vertical_Scale="Yes",
	Content_Areas_Labels=list(MATHEMATICS="Math", READING="Reading", GRADE_9_LIT="Grade 9 Lit", AMERICAN_LIT="American Lit", ALGEBRA_I="Algebra I", ALGEBRA_II="Algebra II"),
	Content_Areas_Domains=list(MATHEMATICS="MATHEMATICS", READING="READING", GRADE_9_LIT="READING", AMERICAN_LIT="READING", ALGEBRA_I="MATHEMATICS", ALGEBRA_II="MATHEMATICS"),
	Grades_Reported=list(MATHEMATICS=c("3","4","5","6","7","8"), READING=c("3","4","5","6","7","8"), GRADE_9_LIT="EOCT", AMERICAN_LIT="EOCT", ALGEBRA_I="EOCT", ALGEBRA_II="EOCT"),
	Grades_Reported_Domains=list(MATHEMATICS=c("3","4","5","6","7","8", "EOCT"), READING=c("3","4","5","6","7","8", "9", "EOCT")),
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

	SGPstateData[["DEMO"]][['SGP_Configuration']][['sgPlot.show.content_area.progression']] <- TRUE

	### Create configurations

	READING_2012_2013.config <- list(
		READING.2012_2013 = list(
			sgp.content.areas=c('READING', 'READING', 'READING', 'READING', 'READING'),
			sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011', '2011_2012', '2012_2013'),
			sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
	)

	GRADE_9_LIT_2012_2013.config <- list(
		GRADE_9_LIT.2012_2013 = list(
			sgp.content.areas=c('READING', 'READING', 'READING', 'READING', 'GRADE_9_LIT'),
			sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011', '2011_2012', '2012_2013'),
			sgp.grade.sequences=list(c(5:8, 'EOCT')))
	)
	
	AMERICAN_LIT_2012_2013.config <- list(
		AMERICAN_LIT.2012_2013 = list(
			sgp.content.areas=c('READING', 'READING', 'READING', 'GRADE_9_LIT', 'AMERICAN_LIT'),
			sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011', '2011_2012', '2012_2013'),
			sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
	)

	MATHEMATICS_2012_2013.config <- list(
		MATHEMATICS.2012_2013 = list(
			sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS'),
			sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011', '2011_2012', '2012_2013'),
			sgp.grade.sequences=list(3:4, 3:5, 3:6, 3:7, 4:8))
	)

	ALGEBRA_I_2012_2013.config <- list(
		 ALGEBRA_I.2012_2013 = list(
			sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I'),
			sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011', '2011_2012', '2012_2013'),
			sgp.grade.sequences=list(c(5:8, 'EOCT')))
	)

	ALGEBRA_II_2012_2013.config <- list(
		 ALGEBRA_II.2012_2013 = list(
			sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA_I', 'ALGEBRA_II'),
			sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011', '2011_2012', '2012_2013'),
			sgp.grade.sequences=list(c(6:8, 'EOCT', 'EOCT')))
	)

	sgp.config <- c(READING_2012_2013.config, MATHEMATICS_2012_2013.config, GRADE_9_LIT_2012_2013.config, AMERICAN_LIT_2012_2013.config, ALGEBRA_I_2012_2013.config, ALGEBRA_II_2012_2013.config)
	 
	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP', 'summarizeSGP', 'visualizeSGP'),\n\tsimulate.sgps=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config,\n\tplot.types=c('bubblePlot', 'studentGrowthPlot'),\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Demonstration_SGP.Rdata')", sep="\n")

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

	options(error=recover)
	options(warn=2)
	suppressPackageStartupMessages(require(parallel))
	number.cores <- detectCores()-1
	Demonstration_SGP <- tmp.messages <- NULL

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,steps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tdata_supplementary=list(INSTRUCTOR_NUMBER=sgpData_INSTRUCTOR_NUMBER),\n\tyears='2012_2013',\n\tsgp.percentiles=TRUE,\n\tsgp.projections=FALSE,\n\tsgp.projections.lagged=FALSE,\n\tsgp.percentiles.baseline=FALSE,\n\tsgp.projections.baseline=FALSE,\n\tsgp.projections.lagged.baseline=FALSE,\n\tcalculate.simex=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(SIMEX=", number.cores, ", TAUS=", number.cores, "))\n)\n", sep="")

	if (save.results) expression.to.evaluate <- paste(expression.to.evaluate, "save(Demonstration_SGP, file='Demonstration_SGP.Rdata')", sep="\n")

	cat("##### Begin testSGP test number 4 #####\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(4)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
	eval(parse(text=expression.to.evaluate))

	### TEST of SGP_SIMEX_BASELINE variable

	tmp.messages <- ("\t##### Results of testSGP test number 3 #####\n\n")
	
	if (identical(sum(Demonstration_SGP@Data$SGP_SIMEX_BASELINE, na.rm=TRUE), 8585922L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_SIMEX_BASELINE: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_SIMEX_BASELINE: FAIL\n")
	}
	tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 4 #####\n")
	cat(tmp.messages)
	} ### End TEST_NUMBER 4
} ### END testSGP Function
