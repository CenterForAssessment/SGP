`testSGP` <- function(TEST_NUMBER) {

	if (missing(TEST_NUMBER)) {
		message("\ttestSGP carries out testing of SGP package. Tests currently included in testSGP:\n")
		message("\t\t1. abcSGP test using all available years.")
		message("\t\t2. abcSGP test using all available years except most recent followed by an updated analysis using the most recent year's data.")
	}

	if (1 %in% TEST_NUMBER) {

	expression.to.evaluate <- 
		"abcSGP(\n\tsgp_object=sgpData_LONG,\n\tsgPlot.demo.report=TRUE,\n\tsave.intermediate.results=TRUE,\n\tparallel.config=list(BACKEND='PARALLEL', WORKERS=list(PERCENTILES=30, BASELINE_PERCENTILES=30, PROJECTIONS=14, LAGGED_PROJECTIONS=14, SUMMARY=30, GA_PLOTS=10, SG_PLOTS=1))\n)\n"


	cat("##### Beginning testSGP test number 1 #####\n", fill=TRUE)

	cat(paste("EVALUATING:\n", expression.to.evaluate, sep=""), fill=TRUE)

	eval(parse(text=expression.to.evaluate))

	cat("##### End testSGP test number 1 #####\n", fill=TRUE)

	} ### End TEST_NUMBER 1

} ### END testSGP Function
