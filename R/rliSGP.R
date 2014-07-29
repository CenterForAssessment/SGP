`rliSGP` <-
function(sgp_object, 
	additional.data,
	testing.window, ### FALL, WINTER, SPRING, EARLY_SPRING, LATE_SPRING for UPDATE 
	eow.or.update="UPDATE", ### UPDATE or EOW
	configuration.year,
	parallel.config=NULL) {

	### Tests for arguments

	if (missing(testing.window) || length(testing.window) != 1 || !testing.window %in% c("FALL", "WINTER", "EARLY_SPRING", "LATE_SPRING", "SPRING")) {
		stop("\tPlease supply either 'FALL', 'WINTER', 'EARLY_SPRING' or 'LATE_SPRING' for the testing.window argument.")
	}

	if (eow.or.update=="EOW" && testing.window %in% c("EARLY_SPRING", "LATE_SPRING")) {
		testing.window <- "SPRING"
	}

	if (eow.or.update=="UPDATE" && testing.window=="LATE_SPRING") {
		testing.window <- "SPRING"
	}


	########################################################################
	###
	### WITHIN_WINDOW UPDATE scripts
	###
	########################################################################

	if (eow.or.update=="UPDATE") {

		### FALL, WINTER, SPRING (not EARLY_SPRING)

		if (testing.window %in% c("FALL", "WINTER", "SPRING")) {

			RLI_SGP_SAMPLE_UPDATE_SHELL <- updateSGP(
				what_sgp_object=sgp_object,
				with_sgp_data_LONG=additional.data,
				state="RLI",
				steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
				save.intermediate.results=TRUE,
				sgp.percentiles=FALSE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=TRUE,
				sgp.projections.baseline=TRUE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.target.scale.scores.only=TRUE,
				outputSGP.output.type="RLI",
				goodness.of.fit.print=FALSE,
				update.old.data.with.new=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", testing.window)))
		}

		### EARLY_SPRING special case

		if (testing.window=="EARLY_SPRING") {

			RLI_SGP_SAMPLE_UPDATE_SHELL <- updateSGP(
				what_sgp_object=sgp_object,
				with_sgp_data_LONG=additional.data,
				state="RLI",
				steps=c("prepareSGP", "analyzeSGP"),
				save.intermediate.results=TRUE,
				sgp.percentiles=FALSE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=TRUE,
				sgp.projections.baseline=TRUE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.target.scale.scores.only=TRUE,
				outputSGP.output.type="RLI",
				goodness.of.fit.print=FALSE,
				update.old.data.with.new=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", testing.window)))


			RLI_SGP_UPDATE_SHELL <- abcSGP(
				RLI_SGP_UPDATE_SHELL,
				steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
				state="RLI",
				sgp.percentiles=FALSE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=TRUE,
				sgp.projections.baseline=TRUE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.target.scale.scores.only=TRUE,
				outputSGP.output.type="RLI",
				goodness.of.fit.print=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", "SPRING"),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", "SPRING"),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", "SPRING")))
		}

	} ### END UPDATE scripts


	###############################################################################
	###
	### END_OF_WINDOW UPDATE scripts
	###
	###############################################################################

	if (eow.or.update=="EOW") {

		### FALL/WINTER

		if (testing.window %in% c("FALL", "WINTER")) {

			sgp_object <- updateSGP(
				what_sgp_object=sgp_object,
				with_sgp_data_LONG=additional.data,
				state="RLI",
				steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
				save.intermediate.results=TRUE,
				sgp.percentiles=TRUE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=TRUE,
				sgp.projections.baseline=TRUE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.target.scale.scores.only=TRUE,
				outputSGP.output.type="RLI",
				update.old.data.with.new=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", testing.window)))
		}

		### SPRING

		if (testing.window=="SPRING") {

			sgp_object <- updateSGP(
				what_sgp_object=sgp_object,
				with_sgp_data_LONG=additional.data,
				state="RLI",
				steps=c("prepareSGP", "analyzeSGP"),
				save.intermediate.results=TRUE,
				sgp.percentiles=TRUE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=FALSE,
				sgp.projections.baseline=FALSE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.target.scale.scores.only=TRUE,
				outputSGP.output.type="RLI",
				update.old.data.with.new=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", "EARLY_SPRING"),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", "EARLY_SPRING"),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", "EARLY_SPRING")))

			

			sgp_object <- updateSGP(
				what_sgp_object=sgp_object,
				with_sgp_data_LONG=additional.data,
				state="RLI",
				steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
				save.intermediate.results=TRUE,
				sgp.percentiles=TRUE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=TRUE,
				sgp.projections.baseline=TRUE,
				sgp.projections.lagged.baseline=FALSE,
				sgp.target.scale.scores.only=TRUE,
				outputSGP.output.type="RLI",
				update.old.data.with.new=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", testing.window),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", testing.window)))
		}

	} ### END END_OF_WINDOW scripts
} ### END rliSGP
