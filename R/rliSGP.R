`rliSGP` <-
function(sgp_object,
	additional.data,
	state=NULL,
	content_areas=c("MATHEMATICS", "READING", "EARLY_LITERACY"),
	testing.window, ### FALL, WINTER, SPRING 
	eow.or.update="UPDATE", ### UPDATE or EOW
	update.save.shell.only=FALSE,
	configuration.year,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.target.scale.scores=TRUE,
	update.ids=NULL,
	SGPt=NULL,
	parallel.config=NULL) {

	YEAR <- GRADE <- ID <- NEW_ID <- .EACHI <- DATE <- NULL

	started.at <- proc.time()
	message(paste("\nStarted rliSGP", date()), "\n")

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "abcSGP")
        }

	if (!state %in% c("RLI", "RLI_UK")) stop("\tNOTE: 'rliSGP' only works with states RLI or RLI_UK currently")

	### Utility functions

	convertToBaseline <- function(baseline_matrices) {
		tmp.list <- list()
		if (is.null(baseline_matrices)) {
			return(NULL)
		} else {
			for (i in names(baseline_matrices)) {
				for (j in seq_along(baseline_matrices[[i]])) {
					baseline_matrices[[i]][[j]]@Time <- list(rep("BASELINE", length(unlist(baseline_matrices[[i]][[j]]@Time))))
				}
				names(baseline_matrices[[i]]) <- sub("[.][1234]_", "_", names(baseline_matrices[[i]]))
			}

			tmp.content_areas <- unique(sapply(strsplit(names(baseline_matrices), "[.]"), '[', 1))
			for (i in tmp.content_areas) {
				tmp.list[[paste(i, "BASELINE", sep=".")]] <- unlist(baseline_matrices[grep(i, names(baseline_matrices))], recursive=FALSE)
			}
			return(tmp.list)
		}
	}

	updateIDS <- function(my.data, id.lookup) {
		setnames(id.lookup, 1:2, c("ID", "NEW_ID"))
		id.lookup[,ID:=as.character(ID)]; id.lookup[,NEW_ID:=as.character(NEW_ID)]
		setkey(id.lookup, ID)
		if (is.SGP(my.data)) {
			tmp.dt <- copy(my.data@Data)
			setkey(tmp.dt, ID)
			tmp.dt[id.lookup, ID:=NEW_ID, by=.EACHI]
			sgp_object@Data <- tmp.dt
			setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
			return(sgp_object)
		}
		if (is.data.frame(my.data)) {
			setkey(my.data, ID)
			my.data[id.lookup, ID:=NEW_ID, by=.EACHI]
			return(my.data)
		}
	}

	getRLIConfig <- function(content_areas, configuration.year, testing.window, SGPt) {
		tmp.list <- list()
		for (i in content_areas) {
			tmp.list[[i]] <- SGP::SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, i, testing.window, SGPt)
		}
		return(unlist(tmp.list, recursive=FALSE))
	}


	### Tests for arguments

	if (missing(testing.window) || length(testing.window) != 1 || !testing.window %in% c("FALL", "WINTER", "EARLY_SPRING", "LATE_SPRING", "SPRING")) {
		stop("\tPlease supply either 'FALL', 'WINTER', 'EARLY_SPRING' or 'LATE_SPRING' for the testing.window argument.")
	}

	if (eow.or.update=="EOW" && testing.window %in% c("EARLY_SPRING", "LATE_SPRING")) {
		testing.window <- "SPRING"
	}

	if (eow.or.update=="UPDATE" && testing.window %in% c("EARLY_SPRING", "LATE_SPRING")) {
		testing.window <- "SPRING"
	}

	if (!is.data.table(additional.data)) additional.data <- as.data.table(additional.data)

	if ("DATE" %in% names(additional.data)) additional.data[,DATE:=as.Date(DATE)]

	if (!is.null(update.ids) && !is.data.table(update.ids)) update.ids <- as.data.table(update.ids)

	if (state=="RLI_UK") content_areas <- "READING"

	### Update IDS if requested

	if (!is.null(update.ids)) {
		sgp_object <- updateIDS(sgp_object, update.ids)
		additional.data <- updateIDS(additional.data, update.ids)
	}


	########################################################################
	###
	### WITHIN_WINDOW UPDATE scripts
	###
	########################################################################

	if (eow.or.update=="UPDATE") {

		sgp_object <- updateSGP(
			what_sgp_object=sgp_object,
			with_sgp_data_LONG=additional.data,
			state=state,
			steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
			save.intermediate.results=FALSE,
			sgp.percentiles=FALSE,
			sgp.projections=FALSE,
			sgp.projections.lagged=FALSE,
			sgp.percentiles.baseline=sgp.percentiles.baseline,
			sgp.projections.baseline=sgp.projections.baseline,
			sgp.projections.lagged.baseline=FALSE,
			sgp.target.scale.scores.only=sgp.target.scale.scores,
			outputSGP.output.type="RLI",
			goodness.of.fit.print=FALSE,
			update.old.data.with.new=FALSE,
			SGPt=SGPt,
			parallel.config=parallel.config,
			sgp.config=getRLIConfig(content_areas, configuration.year, testing.window, SGPt))

		if (!is.null(update.ids)) {
			update.shell.name <- paste(state, "SGP_UPDATE_SHELL", sep="_")
			assign(update.shell.name, sgp_object)
			save(list=update.shell.name, paste(update.shell.name, "Rdata", sep="."))
		}
	} ### END UPDATE scripts


	###############################################################################
	###
	### END_OF_WINDOW UPDATE scripts
	###
	###############################################################################

	if (eow.or.update=="EOW") {

		update.shell.name <- paste(state, "SGP_UPDATE_SHELL", sep="_")

		### FALL/WINTER

		if (testing.window %in% c("FALL", "WINTER")) {

			if (testing.window=="FALL") num.windows.to.keep <- 5 else num.windows.to.keep <- 6
			if (update.save.shell.only) {
				tmp.data <- rbindlist(list(sgp_object@Data, additional.data), fill=TRUE)
				assign(update.shell.name, prepareSGP(subset(tmp.data, YEAR %in% tail(sort(unique(tmp.data$YEAR)), num.windows.to.keep)), state=state, create.additional.variables=FALSE))
				save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))
			} else {
				sgp_object <- updateSGP(
					what_sgp_object=sgp_object,
					with_sgp_data_LONG=additional.data,
					state=state,
					steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
					save.intermediate.results=FALSE,
					sgp.percentiles=TRUE,
					sgp.projections=FALSE,
					sgp.projections.lagged=FALSE,
					sgp.percentiles.baseline=sgp.percentiles.baseline,
					sgp.projections.baseline=sgp.projections.baseline,
					sgp.projections.lagged.baseline=FALSE,
					sgp.target.scale.scores.only=sgp.target.scale.scores,
					outputSGP.output.type="RLI",
					update.old.data.with.new=TRUE,
					goodness.of.fit.print=FALSE,
					SGPt=SGPt,
					parallel.config=parallel.config,
					sgp.config=getRLIConfig(content_areas, configuration.year, testing.window, SGPt))

				### Create and save new UPDATE_SHELL

				assign(update.shell.name, prepareSGP(subset(sgp_object@Data, YEAR %in% tail(sort(unique(sgp_object@Data$YEAR)), num.windows.to.keep)), state=state, create.additional.variables=FALSE))
				save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))


				### Convert and save coefficient matrices

				if (testing.window=="FALL") tmp.separator <- "1" else tmp.separator <- "2"
				tmp.index <- grep(configuration.year, names(sgp_object@SGP$Coefficient_Matrices))
				assign(paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), tmp.separator, sep="."), sep=""), 
					convertToBaseline(sgp_object@SGP$Coefficient_Matrices[tmp.index]))
				save(list=paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), tmp.separator, sep="."), sep=""), 
					file=paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), tmp.separator, "Rdata", sep="."), sep=""))
			}
		} ### END if (testing.window %in% c("FALL", "WINTER"))

		### SPRING

		if (testing.window=="SPRING") {

			### Create additional.data.unique

			setkeyv(additional.data, c("VALID_CASE", "CONTENT_AREA", "ID", "YEAR"))
			setkeyv(additional.data, c("VALID_CASE", "CONTENT_AREA", "ID"))
			additional.data.unique <- additional.data[!(which(duplicated(additional.data))-1)]
			additional.data.unique[,YEAR:=paste(configuration.year, "3", sep=".")]
			additional.data.unique[,GRADE:=as.factor(GRADE)]
			levels(additional.data.unique$GRADE) <- sub("[.]4", ".3", levels(additional.data.unique$GRADE))
			additional.data.unique[,GRADE:=as.character(GRADE)]

			if (update.save.shell.only) {
				tmp.data <- rbindlist(list(sgp_object@Data, additional.data.unique), fill=TRUE)
				assign(update.shell.name, prepareSGP(subset(tmp.data, YEAR %in% tail(sort(unique(tmp.data$YEAR)), 6)), state=state, create.additional.variables=FALSE))
				save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))
			} else {
				### STEP 1: Create EARLY_SPRING to LATE_SPRING coefficient matrices

				sgp_object.1 <- updateSGP(
					what_sgp_object=sgp_object,
					with_sgp_data_LONG=additional.data,
					state=state,
					steps=c("prepareSGP", "analyzeSGP"),
					save.intermediate.results=FALSE,
					sgp.percentiles=TRUE,
					sgp.projections=FALSE,
					sgp.projections.lagged=FALSE,
					sgp.percentiles.baseline=FALSE,
					sgp.projections.baseline=FALSE,
					sgp.projections.lagged.baseline=FALSE,
					update.old.data.with.new=FALSE,
					goodness.of.fit.print=FALSE,
					SGPt=SGPt,
					parallel.config=parallel.config,
					sgp.config=getRLIConfig(content_areas, configuration.year, "EARLY_SPRING", SGPt))

				### Convert and save coefficient matrices

				tmp.index <- grep(configuration.year, names(sgp_object.1@SGP$Coefficient_Matrices))
				assign(paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), "4", sep="."), sep=""), 
					convertToBaseline(sgp_object.1@SGP$Coefficient_Matrices[tmp.index]))
				save(list=paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), "4", sep="."), sep=""), 
					file=paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), "4", "Rdata", sep="."), sep=""))


				### STEP 2: Get official SPRING scores for SGP spring analysis

				sgp_object.2 <- updateSGP(
					what_sgp_object=sgp_object,
					with_sgp_data_LONG=additional.data.unique,
					state=state,
					steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
					save.intermediate.results=FALSE,
					sgp.percentiles=TRUE,
					sgp.projections=FALSE,
					sgp.projections.lagged=FALSE,
					sgp.percentiles.baseline=sgp.percentiles.baseline,
					sgp.projections.baseline=sgp.projections.baseline,
					sgp.projections.lagged.baseline=FALSE,
					sgp.target.scale.scores.only=sgp.target.scale.scores,
					outputSGP.output.type="RLI",
					update.old.data.with.new=TRUE,
					goodness.of.fit.print=FALSE,
					SGPt=SGPt,
					parallel.config=parallel.config,
					sgp.config=getRLIConfig(content_areas, configuration.year, testing.window, SGPt))


				### Create and save new UPDATE_SHELL

				tmp.years <- sort(unique(sgp_object.2@Data$YEAR)); tmp.indices <- sapply(strsplit(tmp.years, "[.]"), '[', 2)
				years.to.keep <- tmp.years[sort(c(tail(which(tmp.indices==1), 2), tail(which(tmp.indices==2), 1), tail(which(tmp.indices==3), 2)))]
				assign(update.shell.name, prepareSGP(subset(sgp_object.2@Data, YEAR %in% years.to.keep), state=state, create.additional.variables=FALSE))
				save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))


				### Convert and save coefficient matrices

				tmp.index <- grep(configuration.year, names(sgp_object.2@SGP$Coefficient_Matrices))
				assign(paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), "3", sep="."), sep=""), 
					convertToBaseline(sgp_object.2@SGP$Coefficient_Matrices[tmp.index]))
				save(list=paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), "3", sep="."), sep=""), 
					file=paste(state, "_Baseline_Matrices_", paste(yearIncrement(configuration.year, 1), "3", "Rdata", sep="."), sep=""))
			} ### END if (update.save.shell.only)
		} ### END if (testing.window=="SPRING")
	} ### END END_OF_WINDOW scripts

	message(paste("Finished rliSGP", date(), "in", timetaken(started.at), "\n"))
} ### END rliSGP
