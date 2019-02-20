`rliSGP` <-
function(sgp_object,
	additional.data=NULL,
	state=NULL,
	content_areas=c("MATHEMATICS", "READING", "EARLY_LITERACY"),
	testing.window=NULL, ### FALL, WINTER, SPRING
	eow.or.update="UPDATE", ### UPDATE or EOW
	update.save.shell.only=FALSE,
	configuration.year=NULL,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=FALSE,
	sgp.target.scale.scores=TRUE,
	update.ids=NULL,
	SGPt=TRUE,
	simulate.sgps=FALSE,
	save.intermediate.results=FALSE,
	coefficient.matrices=NULL,
	goodness.of.fit.print=FALSE,
	return.updated.shell=FALSE,
	fix.duplicates="KEEP.ALL",
	eow.calculate.sgps=FALSE,
	score.type="RASCH",
	cutscore.file.name="Cutscores.csv",
	parallel.config=NULL) {

	YEAR <- GRADE <- ID <- NEW_ID <- .EACHI <- DATE <- CONTENT_AREA <- NULL
	SGPstateData <- SGP::SGPstateData ### Needed due to possible assignment of values to SGPstateData

	started.at <- proc.time()
	messageSGP(paste("\nStarted rliSGP", prettyDate()), "\n")

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "abcSGP")
        }

	if (!state %in% c("RLI", "RLI_UK")) stop("\tNOTE: 'rliSGP' only works with states RLI or RLI_UK currently")

	if (!score.type %in% c("RASCH", "STAR")) stop("\tNOTE: 'score.type argument must be set to either RASCH or STAR.'")


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

	getRLIConfig <- function(content_areas, configuration.year, testing.window, score.type) {
		tmp.list <- list()
		for (i in content_areas) {
			tmp.list[[i]] <- SGPstateData$RLI$SGP_Configuration$sgp.config.function$value(configuration.year, i, testing.window, score.type)
		}
		if (score.type=="RASCH") setattr(tmp.list, "names", paste(names(tmp.list), "RASCH", sep="_"))
		return(unlist(tmp.list, recursive=FALSE))
	}

	getRLIMatrixYears <- function(score.type) {
		tmp.years <- unlist(lapply(lapply(strsplit(names(get(paste0(state, "_SGPt_Baseline_Matrices"))), "_"), tail, 2), paste, collapse="_"))
		if (score.type=="STAR") {
			return(sort(tmp.years[grep("READING[.]", lapply(get(paste0(state, "_SGPt_Baseline_Matrices")), names))]))
		}
		if (score.type=="RASCH") {
			return(sort(tmp.years[grep("READING_RASCH[.]", lapply(get(paste0(state, "_SGPt_Baseline_Matrices")), names))]))
		}
	}


	### Tests for arguments

	if (!is.null(additional.data) && !is.data.table(additional.data)) additional.data <- as.data.table(additional.data)

	if ("DATE" %in% names(additional.data)) additional.data[,DATE:=as.Date(DATE)]

	if (!is.null(update.ids) && !is.data.table(update.ids)) update.ids <- as.data.table(update.ids)

	if (state=="RLI_UK") content_areas <- "READING"

	### Create Cutscores and embed in SGPstateData

	if (state=="RLI") {
		if (is.character(cutscore.file.name) || is.data.frame(cutscore.file.name)) {
			messageSGP(paste0("\tNOTE: Using cutscores file (", deparse(substitute(cutscore.file.name)), ") supplied in the working directory for projection/growth-to-standard analyses."))
			if (is.character(cutscore.file.name) && !file.exists(cutscore.file.name)) stop("\tNOTE: Cutscores file (", cutscore.file.name, ") does not exist in working directory or supplied path.")
			tmp.list <- rliCutscoreCreation(cutscore.file.name, score.type)
			SGPstateData[["RLI"]][["Achievement"]][["Cutscores"]] <- tmp.list[['Cutscores']]
			SGPstateData[["RLI"]][["Achievement"]][["Cutscore_Information"]] <- tmp.list[['Cutscore_Information']]
		} else {
			messageSGP(paste0("\tNOTE: Using cutscores embedded in SGPstateData for RLI projection/growth-to-standard analyses."))
			tmp.list <- SGPstateData[["RLI"]][["Achievement"]][["Cutscores"]][[score.type]]
			SGPstateData[["RLI"]][["Achievement"]][["Cutscores"]] <- tmp.list[['Cutscores']]
			SGPstateData[["RLI"]][["Achievement"]][["Cutscore_Information"]] <- tmp.list[['Cutscore_Information']]
		}
	}

	### Take supplied data and break up if necessary

	if (long.data.supplied <- is.data.frame(sgp_object)) {
		sgp_object <- as.data.table(sgp_object)
		if (score.type=="RASCH") sgp_object[,CONTENT_AREA:=paste(CONTENT_AREA, "RASCH", sep="_")]
		tmp.last.year <- tail(sort(unique(sgp_object[['YEAR']])), 1)
		additional.data <- sgp_object[YEAR==tmp.last.year]
		sgp_object <- new("SGP", Data=suppressMessages(prepareSGP(sgp_object[YEAR!=tmp.last.year], state=state)@Data), Version=getVersion(sgp_object))
		gc(FALSE)
	} else {
		if (score.type=="RASCH") {
			if (!is.null(additional.data)) additional.data[,CONTENT_AREA:=paste(CONTENT_AREA, "RASCH", sep="_")]
			sgp_object@Data$CONTENT_AREA <- paste(sgp_object@Data$CONTENT_AREA, "RASCH", sep="_")
		}
	}

	if (!is.null(testing.window) && (length(testing.window) != 1 || !testing.window %in% c("FALL", "WINTER", "SPRING"))) {
		stop("\tPlease supply either 'FALL', 'WINTER', or 'SPRING' for the testing.window argument.")
	} else {
		testing.window <- c("FALL", "WINTER", "SPRING")[as.numeric(tail(unlist(strsplit(tail(sort(unique(additional.data[['YEAR']])), 1), '[.]')), 1))]
	}

	if (is.null(configuration.year)) configuration.year <- head(unlist(strsplit(tail(sort(unique(additional.data[['YEAR']])), 1), '[.]')), 1)

	if (length(find.package("RLImatrices", quiet=TRUE))==0) stop("Package RLImatrices required from GitHub.")
	if (is.null(coefficient.matrices)) {
		eval(parse(text="require(RLImatrices)"))
		matrix.years <- getRLIMatrixYears(score.type)
		tmp.configuration.year <- paste(configuration.year, match(testing.window, c("FALL", "WINTER", "SPRING")), sep=".")
		tmp.data.last.year <- tail(sort(unique(additional.data[['YEAR']])), 1)
		if (!tmp.configuration.year %in% matrix.years) {
			messageSGP(paste0("\tNOTE: ", tmp.configuration.year, " indicated in the configuration has no matrices in ", paste(state, "SGPt_Baseline_Matrices", sep="_")))
			if (tmp.data.last.year > tail(matrix.years, 1)) tmp.matrix.year <- tail(matrix.years, 1)
			if (tmp.data.last.year < head(matrix.years, 1)) tmp.matrix.year <- head(matrix.years, 1)
		} else {
			tmp.matrix.year <- tmp.configuration.year
		}
		matrix.label <- paste0(paste(state, "SGPt_Baseline_Matrices", sep="_"), "$", paste(state, "SGPt_Baseline_Matrices", tmp.matrix.year, sep="_"))
		messageSGP(paste0("\tNOTE: rliSGP using matrices ", paste(state, "SGPt_Baseline_Matrices", tmp.matrix.year, sep="_")))
		SGPstateData[[state]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- eval(parse(text=matrix.label))
	} else {
		SGPstateData[[state]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]] <- coefficient.matrices
	}

	### Create variables

	if (is.null(SGPt)) update.shell.name <- paste(state, "SGP_UPDATE_SHELL", sep="_") else update.shell.name <- paste(state, "SGPt_UPDATE_SHELL", sep="_")
	if (testing.window=="FALL") num.windows.to.keep <- 5 else num.windows.to.keep <- 6


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
			save.intermediate.results=save.intermediate.results,
			sgp.percentiles=FALSE,
			sgp.projections=FALSE,
			sgp.projections.lagged=FALSE,
			sgp.percentiles.baseline=sgp.percentiles.baseline,
			sgp.projections.baseline=sgp.projections.baseline,
			sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
			sgp.target.scale.scores=sgp.target.scale.scores,
			sgp.target.scale.scores.only=TRUE,
			simulate.sgps=simulate.sgps,
			outputSGP.output.type="RLI",
			goodness.of.fit.print=goodness.of.fit.print,
			update.old.data.with.new=FALSE,
			SGPt=SGPt,
			fix.duplicates=fix.duplicates,
			parallel.config=parallel.config,
			sgp.config=getRLIConfig(content_areas, configuration.year, testing.window, score.type))

		if (!is.null(update.ids)) {
			assign(update.shell.name, sgp_object)
			save(list=update.shell.name, paste(update.shell.name, "Rdata", sep="."))
		}

		if (update.save.shell.only) {
			assign(update.shell.name, prepareSGP(sgp_object@Data[YEAR %in% tail(head(sort(unique(sgp_object@Data[['YEAR']])), -1), num.windows.to.keep)],
				state=state, create.additional.variables=FALSE))
			save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))
		}
	} ### END UPDATE scripts


	###############################################################################
	###
	### END_OF_WINDOW UPDATE scripts
	###
	###############################################################################

	if (eow.or.update=="EOW") {

		if (update.save.shell.only) {
			tmp.data <- rbindlist(list(sgp_object@Data, additional.data), fill=TRUE)
			assign(update.shell.name, prepareSGP(tmp.data[YEAR %in% tail(sort(unique(tmp.data[['YEAR']])), num.windows.to.keep)], state=state, create.additional.variables=FALSE))
			save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))
		} else {
			if (eow.calculate.sgps) my.steps <- c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP") else steps <- c("prepareSGP", "analyzeSGP")
			latest.RLImatrices.version <- sub("-", ".", unlist(strsplit(read.table("https://raw.githubusercontent.com/CenterForAssessment/RLImatrices/master/DESCRIPTION", sep="!", colClasses="character")$V1[4], ": "))[2])
			if (as.character(packageVersion("RLImatrices"))!=latest.RLImatrices.version) stop(paste0("Installed 'RLImatrices' package is not most current version. Install latest version (", latest.RLImatrices.version, ") using install_github('centerforassessment/RLImatrices')."))
			sgp_object <- updateSGP(
				what_sgp_object=sgp_object,
				with_sgp_data_LONG=additional.data,
				state=state,
				steps=steps,
				save.intermediate.results=save.intermediate.results,
				sgp.percentiles=TRUE,
				sgp.projections=FALSE,
				sgp.projections.lagged=FALSE,
				sgp.percentiles.baseline=sgp.percentiles.baseline & eow.calculate.sgps,
				sgp.projections.baseline=sgp.projections.baseline & eow.calculate.sgps,
				sgp.projections.lagged.baseline=sgp.projections.lagged.baseline & eow.calculate.sgps,
				sgp.target.scale.scores=sgp.target.scale.scores & eow.calculate.sgps,
				sgp.target.scale.scores.only=TRUE,
				simulate.sgps=simulate.sgps,
				outputSGP.output.type="RLI",
				update.old.data.with.new=TRUE,
				goodness.of.fit.print=goodness.of.fit.print,
				SGPt=SGPt,
				fix.duplicates=fix.duplicates,
				sgp.percentiles.calculate.sgps=eow.calculate.sgps,
				parallel.config=parallel.config,
				sgp.config=getRLIConfig(content_areas, configuration.year, testing.window, score.type))

			### Create and save new UPDATE_SHELL

			if (!long.data.supplied) {
				assign(update.shell.name, prepareSGP(sgp_object@Data[YEAR %in% tail(sort(unique(sgp_object@Data[['YEAR']])), num.windows.to.keep)],
					state=state, create.additional.variables=FALSE))
				save(list=update.shell.name, file=paste(update.shell.name, "Rdata", sep="."))
			}


			### Convert and save coefficient matrices for inclusion in RLImatrices package

			if (testing.window=="FALL") {
				matrix.window <- paste(configuration.year, 3, sep=".")
			} else {
				matrix.window <- paste(yearIncrement(configuration.year, 1), c(3, 1, 2)[match(testing.window, c("FALL", "WINTER", "SPRING"))], sep=".")
			}
			new.matrices <-convertToBaseline(sgp_object@SGP$Coefficient_Matrices[grep(configuration.year, names(sgp_object@SGP$Coefficient_Matrices))])
			old.matrix.label <- paste0(paste(state, "SGPt_Baseline_Matrices", sep="_"), "$", tail(sort(names(get(paste(state, "SGPt_Baseline_Matrices", sep="_")))), 1))
			old.matrices <- eval(parse(text=old.matrix.label))
			if (score.type=="RASCH") tmp.content_areas <- paste0(c("EARLY_LITERACY", "MATHEMATICS", "READING"), "_RASCH.BASELINE") else tmp.content_areas <- paste0(c("EARLY_LITERACY", "MATHEMATICS", "READING"), ".BASELINE")
			year.to.replace <- head(sort(unique(sapply(lapply(sapply(names(old.matrices[[tmp.content_areas[3]]]), strsplit, '[.]'), '[', 2:3), paste, collapse="."))), 1)
			for (content_area.iter in tmp.content_areas) {
				old.matrices[[content_area.iter]][grep(year.to.replace, names(old.matrices[[content_area.iter]]))] <- NULL
				old.matrices[[content_area.iter]] <- c(old.matrices[[content_area.iter]], new.matrices[[content_area.iter]])
			}
			eval(parse(text=paste0(paste(state, "SGPt_Baseline_Matrices$", sep="_"), paste(state, "SGPt_Baseline_Matrices", matrix.window, sep="_"), " <- old.matrices")))
			save(list=paste(state, "SGPt_Baseline_Matrices", sep="_"), file=paste(paste(state, "SGPt_Baseline_Matrices", sep="_"), "rda", sep="."), compress="xz")
			messageSGP(paste0("\tNOTE: ", paste(state, "SGPt_Baseline_Matrices", sep="_"), " saved to working directory contains matrices for use in ", matrix.window, "."))
			messageSGP(paste("\t\tAdd", paste(paste(state, "SGPt_Baseline_Matrices", sep="_"), "rda", sep="."), "to the RLImatrices GitHub repo 'data' directory,"))
			messageSGP("\t\tupdate version number/date, tag repo and commit tagged version to GitHub.\n")
		}
	} ### END END_OF_WINDOW scripts


	### Return SGP object if requested

	if (return.updated.shell) return(sgp_object)

	messageSGP(paste("Finished rliSGP", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))
} ### END rliSGP
