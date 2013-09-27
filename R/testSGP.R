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


	#######################################################################################################################################################
	###
	### TEST NUMBER 1
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

	### TEST of SGP_TARGET variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR: FAIL\n")
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP, na.rm=TRUE), 9201802L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP: FAIL\n")
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

	tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 1 #####\n")
	cat(tmp.messages)

	} ### End TEST_NUMBER 1


	#######################################################################################################################################################
	###
	### TEST NUMBER 2
	###
	#######################################################################################################################################################

	if (2 %in% TEST_NUMBER) {

	options(error=recover)
	options(warn=2)
	suppressPackageStartupMessages(require(parallel))
	Demonstration_SGP <- tmp.messages <- NULL
	Demonstration_Data_LONG <- subset(sgpData_LONG, YEAR %in% c("2008_2009", "2009_2010", "2010_2011", "2011_2012"))
	Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))
	number.cores <- detectCores()-1

	### Part 1

	tmp.messages <- "##### Begin testSGP test number 2 #####\n\n"

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(2)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
	}

	eval(parse(text=expression.to.evaluate))
	
	### TEST of SGP variable

	tmp.messages <- c(tmp.messages, "\t##### Results of testSGP test number 2: Part 1 #####\n")

	if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP, part 1: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP, part 1: FAIL\n")
	}

	### TEST of SGP_BASELINE variable

	if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 5667488L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE, part 1: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE, part 1: FAIL\n")
	}

	### TEST of SGP_TARGET_3_YEAR variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 5245437L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR, part 1: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR, part 1: FAIL\n")
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 6088129L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: FAIL\n")
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(27122, 6990, 24358, 55283))) {
		tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 1: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 1: FAIL\n")
	}

	### TEST of MOVE_UP_STAY_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(48152, 10396, 12150, 8943))) {
		tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS, part 1: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS, part 1: FAIL\n")
	}

	tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 2: Part 1 #####\n")

	### Part 2

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2012_2013,\n\tsgp.target.scale.scores=TRUE,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(2)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
	}

	eval(parse(text=expression.to.evaluate))
	
	### TEST of SGP variable

	tmp.messages <- c(tmp.messages, "\n\t##### Results of testSGP test number 2: Part 2 #####\n")

	if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP, part 2: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP, part 2: FAIL\n")
	}

	### TEST of SGP_BASELINE variable

	if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE, part 2: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_BASELINE, part 2: FAIL\n")
	}

	### TEST of SGP_TARGET variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR, part 2: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_3_YEAR, part 2: FAIL\n")
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP, na.rm=TRUE), 9201802L)) {
		tmp.messages <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP, part 2: OK\n")
	} else {
		tmp.messges <- c(tmp.messages, "\tTest of variable SGP_TARGET_MOVE_UP_STAY_UP, part 2: FAIL\n")
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
		tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL\n")
	}

	### TEST of MOVE_UP_STAY_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(72953, 15043, 18336, 13618))) {
		tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: OK\n")
	} else {
		tmp.messages <- c(tmp.messages, "\tTest of variable MOVE_UP_STAY_UP_STATUS, part 2: FAIL\n")
	}

	tmp.messages <- c(tmp.messages, "\t##### End testSGP test number 2: Part 2 #####\n")

	tmp.messages <- c(tmp.messages, "\n##### End testSGP test number 2 #####\n")
	cat(tmp.messages)

	} ### End TEST_NUMBER 2


	#######################################################################################################################################################
	###
	### TEST NUMBER 3
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
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsteps=c('prepareSGP', 'analyzeSGP', 'combineSGP'),\n\tsimulate.sgps=FALSE,\n\tsgPlot.demo.report=TRUE,\n\tsgp.target.scale.scores=TRUE,\n\tsgp.config=sgp.config,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SGP_SCALE_SCORE_TARGETS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

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

	} ### End TEST_NUMBER 3

} ### END testSGP Function
