`testSGP` <- function(TEST_NUMBER) {

	if (missing(TEST_NUMBER)) {
		message("\ttestSGP carries out testing of SGP package. Tests currently included in testSGP:\n")
		message("\t\t1. abcSGP test using all available years.")
		message("\t\t2. abcSGP test using all available years except most recent followed by an updated analysis using the most recent year's data.")
	}


	###
	### TEST NUMBER 1
	###

	if (1 %in% TEST_NUMBER) {

	require(parallel)
	Demonstration_SGP <- NULL
	number.cores <- detectCores()-1

	expression.to.evaluate <- 
		paste("Demonstration_SGP <- abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsgPlot.demo.report=TRUE,\n\tsave.intermediate.results=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=", number.cores, ", BASELINE_PERCENTILES=", number.cores, ", PROJECTIONS=", number.cores, ", LAGGED_PROJECTIONS=", number.cores, ", SUMMARY=", number.cores, ", GA_PLOTS=", number.cores, ", SG_PLOTS=1))\n)\n", sep="")


	cat("##### Beginning testSGP test number 1 #####\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	eval(parse(text=expression.to.evaluate))

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

	if (identical(sum(Demonstration_SGP@Data$SGP_TARGET, na.rm=TRUE), 7796624L)) {
		cat("Test of variable SGP_TARGET: OK", fill=TRUE)
	} else {
		cat("Test of variable SGP_TARGET: FAIL", fill=TRUE)
	}

	### TEST of CATCH_UP_KEEP_UP_STATUS variable

	if (identical(as.numeric(table(Demonstration_SGP@Data$CATCH_UP_KEEP_UP_STATUS)), c(41099, 10837, 35560, 84390))) {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS: OK", fill=TRUE)
	} else {
		cat("Test of variable CATCH_UP_KEEP_UP_STATUS: FAIL", fill=TRUE)
	}

	cat("##### End testSGP test number 1 #####\n", fill=TRUE)

	} ### End TEST_NUMBER 1


	###
	### TEST NUMBER 2
	###

} ### END testSGP Function
