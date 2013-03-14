`testSGP` <- 
function(
	TEST_NUMBER,
	save.results=FALSE,
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
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tdata_supplementary=list(INSTRUCTOR_NUMBER=sgpData_INSTRUCTOR_NUMBER),\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

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

	tmp.messages <- ("\t##### Results of testSGP test number 1 #####\n")

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
	cat(tmp.messages, fill=TRUE)

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

	tmp.messages <- "##### Begin testSGP test number 2 #####\n"

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=Demonstration_Data_LONG,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	if (memory.profile) {
		Rprof("testSGP(2)_Memory_Profile_Part_1.out", memory.profiling=TRUE)
	}

	eval(parse(text=expression.to.evaluate))
	
	### TEST of SGP variable

	tmp.messages <- ("\t##### Results of testSGP test number 2: Part 1 #####\n")

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
		paste("Demonstration_SGP <- updateSGP(\n\twhat_sgp_object=Demonstration_SGP,\n\twith_sgp_data_LONG=Demonstration_Data_LONG_2012_2013,\n\tsgPlot.demo.report=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")

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
	cat(tmp.messages, fill=TRUE)

	} ### End TEST_NUMBER 2

} ### END testSGP Function
