`rliSGP` <-
function(sgp_object, 
	additional.data,
	testing.window, ### FALL, WINTER, SPRING, EARLY_SPRING, LATE_SPRING for UPDATE 
	eow.or.update="UPDATE", ### UPDATE or EOW
	configuration.year,
	parallel.config=NULL) {

	YEAR <- NULL

	### Utility functions

	convertToBaseline <- function(baseline_matrices) {
		tmp.list <- list()
		for (i in names(baseline_matrices)) {
			for (j in seq_along(baseline_matrices[[i]])) {
				baseline_matrices[[i]][[j]]@Time <- list(rep("BASELINE", length(unlist(baseline_matrices[[i]][[j]]@Time))))
			}	
		}

		tmp.content_areas <- unique(sapply(strsplit(names(baseline_matrices), "[.]"), '[', 1))
		for (i in tmp.content_areas) {
			tmp.list[[paste(i, "BASELINE", sep=".")]] <- unlist(baseline_matrices[grep(i, names(baseline_matrices))], recursive=FALSE)
		}
		return(tmp.list)
	}


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

	if (!is.data.table(additional.data)) additional.data <- as.data.table(additional.data)


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

			### Create and save new UPDATE_SHELL

			RLI_SGP_UPDATE_SHELL <- prepareSGP(subset(sgp_object@Data, YEAR %in% tail(sort(unique(sgp_object@Data$YEAR)), 7)), create.additional.variables=FALSE)
			save(RLI_SGP_UPDATE_SHELL, file="RLI_SGP_UPDATE_SHELL.Rdata")


			### Convert and save coefficient matrices

			if (testing.window=="FALL") tmp.separator <- "1" else tmp.separator <- "2"
			tmp.index <- grep(configuration.year, names(sgp_object@SGP$Coefficient_Matrices))
			assign(paste("RLI_Baseline_Matrices_", paste(configuration.year, tmp.separator, "Rdata", sep="."), sep=""), convertToBaseline(sgp_object@SGP$Coefficient_Matrices[tmp.index]))
		}

		### SPRING

		if (testing.window=="SPRING") {

			### STEP 1: Create EARLY_SPRING to LATE_SPRING coefficient matrices

			sgp_object.1 <- updateSGP(
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
				update.old.data.with.new=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config=parallel.config,
				sgp.config=c(
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "MATHEMATICS", "EARLY_SPRING"),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "READING", "EARLY_SPRING"),
					SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, "EARLY_LIT", "EARLY_SPRING")))

			### Convert and save coefficient matrices

			tmp.index <- grep(configuration.year, names(sgp_object.1@SGP$Coefficient_Matrices))
			assign(paste("RLI_Baseline_Matrices_", paste(configuration.year, "4.Rdata", sep="."), sep=""), convertToBaseline(sgp_object.1@SGP$Coefficient_Matrices[tmp.index]))


			### STEP 2: Get official SPRING scores for SGP spring analysis

			setkeyv(additional.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			additional.data <- additional.data[!(which(duplicated(additional.data))-1)]

			sgp_object.2 <- updateSGP(
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

			### Create and save new UPDATE_SHELL

			RLI_SGP_UPDATE_SHELL <- prepareSGP(subset(sgp_object.2@Data, YEAR %in% tail(sort(unique(sgp_object.2@Data$YEAR)), 7)), create.additional.variables=FALSE)
			save(RLI_SGP_UPDATE_SHELL, file="RLI_SGP_UPDATE_SHELL.Rdata")


			### Convert and save coefficient matrices

			tmp.index <- grep(configuration.year, names(sgp_object.2@SGP$Coefficient_Matrices))
			assign(paste("RLI_Baseline_Matrices_", paste(configuration.year, "3.Rdata", sep="."), sep=""), convertToBaseline(sgp_object.2@SGP$Coefficient_Matrices[tmp.index]))
		}

	} ### END END_OF_WINDOW scripts
} ### END rliSGP
