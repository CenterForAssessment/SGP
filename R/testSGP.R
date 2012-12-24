`testSGP` <- 
function(
	TEST_NUMBER, 
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
	require(parallel)
	Demonstration_SGP <- NULL
	number.cores <- detectCores()-1

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")


	cat("##### Beginning testSGP test number 1 #####\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(1)_Memory_Profile.out", memory.profiling=TRUE)
	}
	
	eval(parse(text=expression.to.evaluate))

	if (memory.profile) {
		Rprof(NULL)
	}

	### TEST of SGP variable

	if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
		cat("Test of variable SGP: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP: FAIL", fill=TRUE)
	}

	### TEST of SGP_BASELINE variable

	if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
		cat("Test of variable SGP_BASELINE: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_BASELINE: FAIL", fill=TRUE)
	}

	### TEST of SGP_TARGET variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
		cat("Test of variable SGP_TARGET_3_YEAR: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET_3_YEAR: FAIL", fill=TRUE)
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP, na.rm=TRUE), 9201802L)) {
		cat("Test of variable SGP_TARGET_MOVE_UP_STAY_UP: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET_MOVE_UP_STAY_UP: FAIL", fill=TRUE)
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS: OK", fill=TRUE)
	} else {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS: FAIL", fill=TRUE)
	}

	### TEST of MOVE_UP_STAY_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(72953, 15043, 18336, 13618))) {
		cat("Test of variable MOVE_UP_STAY_UP_STATUS: OK", fill=TRUE)
	} else {
		cat("Test of variable MOVE_UP_STAY_UP_STATUS: FAIL", fill=TRUE)
	}

	cat("##### End testSGP test number 1 #####\n", fill=TRUE)

	} ### End TEST_NUMBER 1


	#######################################################################################################################################################
	###
	### TEST NUMBER 2
	###
	#######################################################################################################################################################

	if (2 %in% TEST_NUMBER) {

	options(error=recover)
	require(parallel)
	Demonstration_SGP <- NULL
	Demonstration_Data_LONG <- subset(sgpData_LONG, YEAR %in% c("2007_2008", "2008_2009", "2009_2010", "2010_2011"))
	Demonstration_Data_LONG_2011_2012 <- subset(sgpData_LONG, YEAR %in% c("2011_2012"))
	number.cores <- detectCores()-1

	cat("##### Beginning testSGP test number 2: Part 1 #####\n", fill=TRUE)

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(2)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
	}

	eval(parse(text=expression.to.evaluate))
	
	### TEST of SGP variable

	if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 5668654L)) {
		cat("Test of variable SGP, part 1: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP, part 1: FAIL", fill=TRUE)
	}

	### TEST of SGP_BASELINE variable

	if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 5667488L)) {
		cat("Test of variable SGP_BASELINE, part 1: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_BASELINE, part 1: FAIL", fill=TRUE)
	}

	### TEST of SGP_TARGET_3_YEAR variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 5245437L)) {
		cat("Test of variable SGP_TARGET_3_YEAR, part 1: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET_3_YEAR, part 1: FAIL", fill=TRUE)
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, na.rm=TRUE), 6088129L)) {
		cat("Test of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET_MOVE_UP_STAY_UP_3_YEAR, part 1: FAIL", fill=TRUE)
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(27122, 6990, 24358, 55283))) {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS, part 1: OK", fill=TRUE)
	} else {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS, part 1: FAIL", fill=TRUE)
	}

	### TEST of MOVE_UP_STAY_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(48152, 10393, 12150, 8943))) {
		cat("Test of variable MOVE_UP_STAY_UP_STATUS, part 1: OK", fill=TRUE)
	} else {
		cat("Test of variable MOVE_UP_STAY_UP_STATUS, part 1: FAIL", fill=TRUE)
	}

	cat("##### Beginning testSGP test number 2: Part 2 #####\n", fill=TRUE)

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2011_2012\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(2)_Memory_Profile_Part_2.out", memory.profiling=TRUE)
	}

	eval(parse(text=expression.to.evaluate))
	
	### TEST of SGP variable

	if (identical(sum(Demonstration_SGP@Data$SGP, na.rm=TRUE), 8565260L)) {
		cat("Test of variable SGP, part 2: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP, part 2: FAIL", fill=TRUE)
	}

	### TEST of SGP_BASELINE variable

	if (identical(sum(Demonstration_SGP@Data$SGP_BASELINE, na.rm=TRUE), 8573825L)) {
		cat("Test of variable SGP_BASELINE, part 2: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_BASELINE, part 2: FAIL", fill=TRUE)
	}

	### TEST of SGP_TARGET variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_3_YEAR, na.rm=TRUE), 7796624L)) {
		cat("Test of variable SGP_TARGET_3_YEAR, part 2: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET_3_YEAR, part 2: FAIL", fill=TRUE)
	}

	### TEST of SGP_TARGET_MOVE_UP_STAY_UP variable

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET_MOVE_UP_STAY_UP, na.rm=TRUE), 9201802L)) {
		cat("Test of variable SGP_TARGET_MOVE_UP_STAY_UP, part 2: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET_MOVE_UP_STAY_UP, part 2: FAIL", fill=TRUE)
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS, part 2: OK", fill=TRUE)
	} else {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS, part 2: FAIL", fill=TRUE)
	}

	### TEST of MOVE_UP_STAY_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$MOVE_UP_STAY_UP_STATUS)), c(72953, 15043, 18336, 13618))) {
		cat("Test of variable MOVE_UP_STAY_UP_STATUS, part 2: OK", fill=TRUE)
	} else {
		cat("Test of variable MOVE_UP_STAY_UP_STATUS, part 2: FAIL", fill=TRUE)
	}

	} ### End TEST_NUMBER 2

} ### END testSGP Function
