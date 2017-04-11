`outputSGP` <-
function(sgp_object,
	state=NULL,
	output.type=c("LONG_Data", "LONG_FINAL_YEAR_Data", "WIDE_Data", "INSTRUCTOR_Data"),
	baseline.sgps=FALSE,
	outputSGP_SUMMARY.years=NULL,
	outputSGP_SUMMARY.content_areas=NULL,
	outputSGP_INDIVIDUAL.years=NULL,
	outputSGP_INDIVIDUAL.content_areas=NULL,
	outputSGP.anonymize=FALSE,
	outputSGP.student.groups=NULL,
	outputSGP.directory="Data",
	outputSGP.translate.names=TRUE,
	outputSGP.projection.years.for.target=3,
	outputSGP.pass.through.variables=NULL) {

	started.at.outputSGP <- proc.time()
	messageSGP(paste0("\nStarted outputSGP ", prettyDate(), ": Files produced from outputSGP saved in '", outputSGP.directory, "'\n"))
	messageSGP(match.call())

	### Create directory

	dir.create(outputSGP.directory, recursive=TRUE, showWarnings=FALSE)

	### Define varaibles (to prevent R CMD check warnings)

	SCALE_SCORE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- ETHNICITY <- GENDER <- LAST_NAME <- FIRST_NAME <- VALID_CASE <- DISTRICT_NUMBER <- SCHOOL_NUMBER <- YEAR_BY_CONTENT_AREA <- NULL
	names.type <- names.provided <- names.output <- names.sgp <- STATE_ENROLLMENT_STATUS <- EMH_LEVEL <- STATE_ASSIGNED_ID <- .N <- TRANSFORMED_SCALE_SCORE <- GROUP <- STATE <- YEAR_WITHIN <- NULL
	DISADVANTAGED_STATUS <- SPECIAL_EDUCATION_STATUS <- ELL_STATUS <- HLS_CODE <- IEP_CODE <- LANGUAGE_PROFICIENCY <- GIFTED_CODE <- FRL_CODE <- STUDENT_GROWTH_ID <- MIDDLE_NAME <- NULL
	OCTOBER_ENROLLMENT_STATUS <- SCALE_SCORE_EQUATED <- SCALE_SCORE_ACTUAL <- ACHIEVEMENT_LEVEL <- TEMP <- TEMP_SCORE <- TEMP_GRADE <- TEMP_CONTENT_AREA <- SGP_NORM_GROUP_BASELINE <- SGP_PROJECTION_GROUP <- NULL

	### Create state (if missing) and tmp.state from sgp_object (if possible)

    if (is.null(state)) {
        tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
        state <- getStateAbbreviation(tmp.name, "outputSGP")
    }

	tmp.state <- gsub(" ", "_", getStateAbbreviation(state, type="LONG"))


	### If sgp_object is a data.frame or data.table, embed in an SGP object

	if ("data.frame" %in% class(sgp_object)) {
		sgp_object <- suppressMessages(prepareSGP(sgp_object, state=state, create.additional.variables=FALSE))
	}

	### Create relevant variables

	if (is.null(outputSGP.student.groups)) {
		outputSGP.student.groups <- intersect(names(sgp_object@Data),
			subset(sgp_object@Names, names.type=="demographic" & names.output==TRUE, select=names.sgp, drop=TRUE))
	}

	if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['outputSGP.pass.through.variables']]) &
		all(SGP::SGPstateData[[state]][['SGP_Configuration']][['outputSGP.pass.through.variables']] %in% names(sgp_object@Data))) {
			outputSGP.pass.through.variables <- SGP::SGPstateData[[state]][['SGP_Configuration']][['outputSGP.pass.through.variables']]
	}

	if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['outputSGP.translate.names']])) {
		outputSGP.translate.names <- SGP::SGPstateData[[state]][['SGP_Configuration']][['outputSGP.translate.names']]
	}

	if (is.null(sgp_object@Names)) {
		outputSGP.translate.names <- FALSE
	}

	###############################################
	###
	### LONG_Data
	###
	###############################################

	if ("LONG_Data" %in% output.type) {

		### Write LONG table

		started.at <- proc.time()
		messageSGP(paste("\tStarted LONG data production in outputSGP", prettyDate()))

		names.in.data <- which(sgp_object@Names[['names.sgp']] %in% names(sgp_object@Data))
		output.data <- copy(sgp_object@Data)
		if (outputSGP.translate.names) setnames(output.data, sgp_object@Names[['names.sgp']][names.in.data], sgp_object@Names[['names.provided']][names.in.data])
		if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['output.column.order']][['SGP_Data_LONG']])) {
			output.column.order <- c(intersect(SGP::SGPstateData[[state]][['SGP_Configuration']][['output.column.order']][['SGP_Data_LONG']], names(output.data)),
						setdiff(names(output.data), SGP::SGPstateData[[state]][['SGP_Configuration']][['output.column.order']][['SGP_Data_LONG']]))
			setcolorder(output.data, output.column.order)
		}
		assign(paste(tmp.state, "SGP_LONG_Data", sep="_"), output.data)
		save(list=paste(tmp.state, "SGP_LONG_Data", sep="_"), file=file.path(outputSGP.directory, paste(tmp.state, "SGP_LONG_Data.Rdata", sep="_")))
		fwrite(output.data, file=file.path(outputSGP.directory, paste(tmp.state, "SGP_LONG_Data.txt", sep="_")), sep="|", quote=FALSE)
		if (identical(.Platform$OS.type, "unix")) {
			if (file.info(file.path(outputSGP.directory, paste(tmp.state, "SGP_LONG_Data.txt", sep="_")))$size > 4000000000) {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste(tmp.state, "SGP_LONG_Data.txt.gz", sep="_") %in% list.files()) file.remove(paste(tmp.state, "SGP_LONG_Data.txt.gz", sep="_"))
				system(paste("gzip", paste(tmp.state, "SGP_LONG_Data.txt", sep="_")))
				setwd(tmp.working.directory)
			} else {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste(tmp.state, "SGP_LONG_Data.txt.zip", sep="_") %in% list.files()) file.remove(paste(tmp.state, "SGP_LONG_Data.txt.zip", sep="_"))
				suppressMessages(
					zip(paste(tmp.state, "SGP_LONG_Data.txt.zip", sep="_"), paste(tmp.state, "SGP_LONG_Data.txt", sep="_"), flags="-rmq1")
				)
				setwd(tmp.working.directory)
			}
		}

		messageSGP(paste("\tFinished LONG data production in outputSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))

	} ### END if LONG_Data %in% output.type


	###############################################
	###
	### LONG_FINAL_YEAR_Data
	###
	###############################################

	if ("LONG_FINAL_YEAR_Data" %in% output.type) {

		### Write LONG table

		started.at <- proc.time()
		messageSGP(paste("\tStarted LONG FINAL YEAR data production in outputSGP", prettyDate()))

		final.year <- tail(sort(unique(sgp_object@Data, by='YEAR')[['YEAR']]), 1)
		names.in.data <- which(sgp_object@Names[['names.sgp']] %in% names(sgp_object@Data))
		output.data.final.year <- sgp_object@Data[YEAR==final.year]
		if (outputSGP.translate.names) setnames(output.data.final.year, sgp_object@Names[['names.sgp']][names.in.data], sgp_object@Names[['names.provided']][names.in.data])
		if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['output.column.order']][['SGP_Data_LONG']])) {
			output.column.order <- c(intersect(SGP::SGPstateData[[state]][['SGP_Configuration']][['output.column.order']][['SGP_Data_LONG']], names(output.data.final.year)),
						setdiff(names(output.data.final.year), SGP::SGPstateData[[state]][['SGP_Configuration']][['output.column.order']][['SGP_Data_LONG']]))
			setcolorder(output.data.final.year, output.column.order)
		}
		assign(paste(tmp.state, "SGP_LONG_Data", final.year, sep="_"), output.data.final.year)
		save(list=paste(tmp.state, "SGP_LONG_Data", final.year, sep="_"), file=file.path(outputSGP.directory, paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".Rdata")))
		fwrite(output.data.final.year, file=file.path(outputSGP.directory, paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt")), sep="|", quote=FALSE)
		if (identical(.Platform$OS.type, "unix")) {
			if (file.info(file.path(outputSGP.directory, paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt")))$size > 4000000000) {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt.gz") %in% list.files()) file.remove(paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt.gz"))
				system(paste("gzip", paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt")))
				setwd(tmp.working.directory)
			} else {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt.zip") %in% list.files()) file.remove(paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt.zip"))
				suppressMessages(
					zip(paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt.zip"), paste0(tmp.state, "_SGP_LONG_Data_", final.year, ".txt"), flags="-rmq1")
				)
				setwd(tmp.working.directory)
			}
		}

		messageSGP(paste("\tFinished LONG FINAL YEAR data production in outputSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))

	} ### END if LONG_FINAL_YEAR_Data %in% output.type


	###############################################
	###
	### WIDE data
	###
	###############################################

	if ("WIDE_Data" %in% output.type) {

		### Write WIDE table

		started.at <- proc.time()
		messageSGP(paste("\tStarted WIDE data production in outputSGP", prettyDate()))

		long_data_tmp <- copy(sgp_object@Data)
		setkeyv(long_data_tmp, getKey(long_data_tmp))
		if ('YEAR_WITHIN' %in% names(long_data_tmp)) {
			long_data_tmp <- long_data_tmp[,YEAR_BY_CONTENT_AREA:=paste(YEAR, CONTENT_AREA, YEAR_WITHIN, sep=".")]["VALID_CASE"]
		} else {
			long_data_tmp <- long_data_tmp[,YEAR_BY_CONTENT_AREA:=paste(YEAR, CONTENT_AREA, sep=".")]["VALID_CASE"]
		}
		if (dups.tf <- any(duplicated(long_data_tmp, by=key(long_data_tmp)))){
			long_data_tmp <- createUniqueLongData(long_data_tmp, wide.output=TRUE)
			setkeyv(long_data_tmp, getKey(long_data_tmp))
		}
		assign(paste(tmp.state, "SGP_WIDE_Data", sep="_"), ddcast(long_data_tmp, ID ~ YEAR_BY_CONTENT_AREA,
			value.var=setdiff(names(long_data_tmp), c("ID", "YEAR_BY_CONTENT_AREA", "VALID_CASE", "CONTENT_AREA", "YEAR")), sep="."))
		if (dups.tf) eval(parse(text=paste0("invisible(", paste(tmp.state, "SGP_WIDE_Data", sep="_"), "[, ID := gsub('_DUPS_[0-9]*', '', ID)])")))
		save(list=paste(tmp.state, "SGP_WIDE_Data", sep="_"), file=file.path(outputSGP.directory, paste(tmp.state, "SGP_WIDE_Data.Rdata", sep="_")))
		fwrite(get(paste(tmp.state, "SGP_WIDE_Data", sep="_")), file=file.path(outputSGP.directory, paste(tmp.state, "SGP_WIDE_Data.txt", sep="_")), sep="|", quote=FALSE)

		if (identical(.Platform$OS.type, "unix")) {
			if (file.info(file.path(outputSGP.directory, paste(tmp.state, "SGP_WIDE_Data.txt", sep="_")))$size > 4000000000) {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste(tmp.state, "SGP_WIDE_Data.txt.gz", sep="_") %in% list.files()) file.remove(paste(tmp.state, "SGP_WIDE_Data.txt.gz", sep="_"))
				system(paste("gzip", paste(tmp.state, "SGP_WIDE_Data.txt", sep="_")))
				setwd(tmp.working.directory)
			} else {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste(tmp.state, "SGP_WIDE_Data.txt.zip", sep="_") %in% list.files()) file.remove(paste(tmp.state, "SGP_WIDE_Data.txt.zip", sep="_"))
				suppressMessages(
					zip(paste(tmp.state, "SGP_WIDE_Data.txt.zip", sep="_"), paste(tmp.state, "SGP_WIDE_Data.txt", sep="_"), flags="-rmq1")
				)
				setwd(tmp.working.directory)
			}
		}

		messageSGP(paste("\tFinished WIDE data production in outputSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))

	} ### END if WIDE_Data %in% output.type


	###############################################
	###
	### INSTRUCTOR data
	###
	###############################################

	if ("INSTRUCTOR_Data" %in% output.type && "INSTRUCTOR_NUMBER" %in% names(sgp_object@Data_Supplementary)) {

		### Write WIDE table

		started.at <- proc.time()
		messageSGP(paste("\tStarted INSTRUCTOR data production in outputSGP", prettyDate()))

		if ("GRADE" %in% names(sgp_object@Data_Supplementary[["INSTRUCTOR_NUMBER"]])) tmp_key <- getKey(sgp_object@Data) else tmp_key <- setdiff(getKey(sgp_object@Data), "GRADE")

		assign(paste(tmp.state, "SGP_INSTRUCTOR_Data", sep="_"), sgp_object@Data[data.table(sgp_object@Data_Supplementary[["INSTRUCTOR_NUMBER"]][,VALID_CASE:="VALID_CASE"],
			key=tmp_key), nomatch=0, on=tmp_key])

		save(list=paste(tmp.state, "SGP_INSTRUCTOR_Data", sep="_"), file=file.path(outputSGP.directory, paste(tmp.state, "SGP_INSTRUCTOR_Data.Rdata", sep="_")))
		fwrite(get(paste(tmp.state, "SGP_INSTRUCTOR_Data", sep="_")), file=file.path(outputSGP.directory, paste(tmp.state, "SGP_INSTRUCTOR_Data.txt", sep="_")), sep="|", quote=FALSE)

		if (identical(.Platform$OS.type, "unix")) {
			if (file.info(file.path(outputSGP.directory, paste(tmp.state, "SGP_INSTRUCTOR_Data.txt", sep="_")))$size > 4000000000) {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste(tmp.state, "SGP_INSTRUCTOR_Data.txt.gz", sep="_") %in% list.files()) file.remove(paste(tmp.state, "SGP_INSTRUCTOR_Data.txt.gz", sep="_"))
				system(paste("gzip", paste(tmp.state, "SGP_INSTRUCTOR_Data.txt", sep="_")))
				setwd(tmp.working.directory)
			} else {
				tmp.working.directory <- getwd()
				setwd(file.path(outputSGP.directory))
				if (paste(tmp.state, "SGP_INSTRUCTOR_Data.txt.zip", sep="_") %in% list.files()) file.remove(paste(tmp.state, "SGP_INSTRUCTOR_Data.txt.zip", sep="_"))
				suppressMessages(
					zip(paste(tmp.state, "SGP_INSTRUCTOR_Data.txt.zip", sep="_"), paste(tmp.state, "SGP_INSTRUCTOR_Data.txt", sep="_"), flags="-rmq1")
				)
				setwd(tmp.working.directory)
			}
		}

		messageSGP(paste("\tFinished INSTRUCTOR data production in outputSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))

	} ### END if INSTRUCTOR_Data %in% output.type


	###############################################
	###
	### SchoolView tables
	###
	###############################################

	if ("SchoolView" %in% output.type) {

		###
		### Summary Tables
		###

			sqliteSGP(
				sgp_object=sgp_object,
				state=state,
				years=outputSGP_SUMMARY.years,
				content_areas=outputSGP_SUMMARY.content_areas,
				other.student.groups=outputSGP.student.groups,
				output.directory=file.path(outputSGP.directory, "SchoolView"))


		###
		### WIDE Data
		###

			started.at <- proc.time()
			messageSGP(paste("\tStarted SchoolView STUDENT_GROWTH data production in outputSGP", prettyDate()))
			slot.data <- copy(sgp_object@Data)

		### Check arguments

		if (!all(c("LAST_NAME", "FIRST_NAME") %in% names(slot.data))) {
			messageSGP("\tNOTE: 'LAST_NAME' and 'FIRST_NAME' are not included in supplied data. Anonymized last names and first names will be supplied.")
			outputSGP.anonymize <- TRUE
			slot.data[['FIRST_NAME']] <- slot.data[['LAST_NAME']] <- as.character(NA)
		}

		## Create group variable names

		if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["output.groups"]])) {
			output.groups.num <- which(!SGP::SGPstateData[[state]][["SGP_Configuration"]][["output.groups"]]==c("DISTRICT", "SCHOOL"))
			output.groups.old <- c("DISTRICT", "SCHOOL")[output.groups.num]
			output.groups.new <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["output.groups"]][output.groups.num]
			setnames(slot.data, c(paste(output.groups.old, "NUMBER", sep="_"), paste(output.groups.old, "ENROLLMENT_STATUS", sep="_")),
				c(paste(output.groups.old, "NUMBER_OLD", sep="_"), paste(output.groups.old, "ENROLLMENT_STATUS_OLD", sep="_")))
			setnames(slot.data, paste(output.groups.new, "NUMBER", sep="_"), paste(output.groups.old, "NUMBER", sep="_"))
			setnames(slot.data, paste(output.groups.new, "ENROLLMENT_STATUS", sep="_"), paste(output.groups.old, "ENROLLMENT_STATUS", sep="_"))
			for (i in seq_along(output.groups.old)) {
				levels(slot.data[[paste(output.groups.old[i], "ENROLLMENT_STATUS", sep="_")]]) <-
					gsub(capwords(output.groups.new[i]), capwords(output.groups.old[i]), levels(slot.data[[paste(output.groups.old[i], "ENROLLMENT_STATUS", sep="_")]]))
			}
		}


		### Utility functions

		"%w/o%" <- function(x,y) x[!x %in% y]

		convert.variables <- function(tmp.df) {
			if ("YEAR" %in% names(tmp.df) && is.character(tmp.df$YEAR)) {
				if (any(grepl("_", tmp.df$YEAR))) tmp.df[,YEAR:=as.integer(sapply(strsplit(YEAR, "_"), '[', 2))] else tmp.df[,YEAR:=as.integer(YEAR)]
			}
			if ("LAST_NAME" %in% names(tmp.df) && is.factor(tmp.df$LAST_NAME)) {
				tmp.df[,LAST_NAME:=as.character(LAST_NAME)]
			}
			if ("FIRST_NAME" %in% names(tmp.df) && is.factor(tmp.df$FIRST_NAME)) {
				tmp.df[,FIRST_NAME:=as.character(FIRST_NAME)]
			}
			if ("EMH_LEVEL" %in% names(tmp.df)) {
				tmp.df[,EMH_LEVEL:=substr(as.character(EMH_LEVEL), 1, 1)]
			}
			if ("GENDER" %in% names(tmp.df) && is.factor(tmp.df$GENDER)) {
				tmp.female <- grep("FEMALE", levels(slot.data$GENDER), ignore.case=TRUE)
				if (tmp.female==1) {
					levels(tmp.df$GENDER) <- c("F", "M")
				} else {
					levels(tmp.df$GENDER) <- c("M", "F")

				}
			}
			if ("ETHNICITY" %in% names(tmp.df)) {
				tmp.df[,ETHNICITY:=unclass(as.factor(ETHNICITY))]
			}
			if ("ACHIEVEMENT_LEVEL" %in% names(tmp.df)) {
				tmp.df[,ACHIEVEMENT_LEVEL:=unclass(as.factor(ACHIEVEMENT_LEVEL))]
			}
			for (names.iter in c(outputSGP.student.groups, "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS") %w/o% grep("ETHNICITY", outputSGP.student.groups, value=TRUE)) {
				if (names.iter %in% names(tmp.df) && is.factor(tmp.df[[names.iter]])) {
					tmp.df[[names.iter]] <- as.character(tmp.df[[names.iter]])
					tmp.df[[names.iter]][grep("Yes", tmp.df[[names.iter]])] <- "Y"
					if (names.iter=="ESEA_WAIVER_SUBGROUPS") {
						tmp.df[[names.iter]][intersect(grep("No", tmp.df[[names.iter]]), grep("Not", tmp.table[["ESEA_WAIVER_SUBGROUPS"]], invert=TRUE))] <- "N"
					} else {
						tmp.df[[names.iter]][grep("No", tmp.df[[names.iter]])] <- "N"
					}
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="Students with Disabilities (IEP)"] <- "Y"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="Students with 504 Plan"] <- "Y"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="High Need Status: ELL, Special Education, or Disadvantaged Student"] <- "Y"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="Economically Disadvantaged"] <- "Y"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="English Language Learners (ELL)"] <- "Y"
					if (names.iter=="ELL_STATUS_MULTILEVEL" & state=="HI") {
						tmp.df[[names.iter]][tmp.df[[names.iter]]=="Currently ELL"] <- "CE"
						tmp.df[[names.iter]][tmp.df[[names.iter]]=="Formerly ELL"] <- "FE"
						tmp.df[[names.iter]][tmp.df[[names.iter]]=="Monitoring ELL"] <- "ME"
						tmp.df[[names.iter]][tmp.df[[names.iter]]=="Not ELL"] <- "NE"
						tmp.df[[names.iter]][tmp.df[[names.iter]]=="Parent Refusal"] <- "PR"
						tmp.df[[names.iter]][tmp.df[[names.iter]]=="Potential ELL"] <- "PE"
					}
				}
			}
			return(tmp.df)
		}

		unclass.data.table <- function(my.data.table) {
			as.data.table(lapply(convert.variables(my.data.table[VALID_CASE=="VALID_CASE"]), unclass))
		}

		get.next.content_area <- function(grade, content_area, increment) {
			if (is.na(grade) | is.na(content_area)) {
				return(NA)
			} else {
				if (!is.null(tmp.domain <- SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]][[content_area]]) & increment != 0) {
					if (!is.numeric(type.convert(grade))) {
						tmp.index <- which(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tmp.domain]] == content_area)
						} else tmp.index <- which(SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tmp.domain]] == grade)
						return(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tmp.domain]][tmp.index + increment])
					} else return(content_area)
			}
		}

		get.next.grade <- function(grade, content_area, increment) {
			if (is.na(grade)) {
				return(NA)
			}
			if (!is.null(tmp.domain <- SGP::SGPstateData[[state]][["Student_Report_Information"]][["Content_Areas_Domains"]][[content_area]])) {
				if (is.character(type.convert(grade, as.is=TRUE))) {
					tmp.index <- which(SGP::SGPstateData[[state]][["SGP_Configuration"]][["content_area.projection.sequence"]][[tmp.domain]] == content_area)
				} else tmp.index <- which(SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tmp.domain]] == grade)
				as.character(SGP::SGPstateData[[state]][["SGP_Configuration"]][["grade.projection.sequence"]][[tmp.domain]][tmp.index + increment])
			} else {
				tmp.grades.reported <- SGP::SGPstateData[[state]][["Student_Report_Information"]][["Grades_Reported"]][[content_area]]
				as.character(c(tmp.grades.reported, NA)[match(grade, tmp.grades.reported) + increment])
			}
		}


		#### Set key

		long.key <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
		setkeyv(slot.data, long.key)


		#### Year stuff

		if (is.null(outputSGP_INDIVIDUAL.years)) {
			tmp.years <- sort(unique(slot.data["VALID_CASE"], by='YEAR')[['YEAR']])
			tmp.last.year <- tail(tmp.years, 1)
			if (any(grepl("_", tmp.years))) {
				tmp.years.short <- sapply(strsplit(tmp.years, "_"), '[', 2)
				tmp.last.year.short <- tail(unlist(strsplit(tail(tmp.years, 1), "_")), 1)
			} else {
				tmp.years.short <- tmp.years
				tmp.last.year.short <- tmp.last.year
			}
		} else {
			tmp.all.years <- sort(unique(slot.data["VALID_CASE"], by='YEAR')[["YEAR"]])
			tmp.years <- tmp.all.years[1:which(tmp.all.years==tail(sort(outputSGP_INDIVIDUAL.years), 1))]
			tmp.last.year <- tail(tmp.years, 1)
			if (any(grepl("_", tmp.years))) {
				tmp.years.short <- sapply(strsplit(tmp.years, "_"), '[', 2)
				tmp.last.year.short <- tail(unlist(strsplit(tail(tmp.years, 1), "_")), 1)
			} else {
				tmp.years.short <- tmp.years
				tmp.last.year.short <- tmp.last.year
			}
		}


		#### Content area stuff

		if (is.null(outputSGP_INDIVIDUAL.content_areas)) {
			tmp.content_areas <- sort(unique(slot.data[SJ("VALID_CASE", tmp.last.year)], by='CONTENT_AREA')[['CONTENT_AREA']])
		} else {
			tmp.content_areas <- sort(outputSGP_INDIVIDUAL.content_areas)
		}

		### subset data

		tmp.districts.and.schools <- unique(data.table(slot.data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas)][,
								list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER, EMH_LEVEL)], key=key(sgp_object)), by=key(sgp_object))
		report.ids <- data.table(slot.data[tmp.districts.and.schools][VALID_CASE=="VALID_CASE" & STATE_ENROLLMENT_STATUS=="Enrolled State: Yes" & !is.na(EMH_LEVEL)][,
			list(ID, FIRST_NAME, LAST_NAME, DISTRICT_NUMBER, SCHOOL_NUMBER, EMH_LEVEL)], key=c("ID", "FIRST_NAME", "LAST_NAME", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
		setkey(report.ids, ID)
		report.ids <- unique(report.ids, by=key(report.ids))
		setkeyv(slot.data, c("ID", "CONTENT_AREA", "YEAR", "VALID_CASE"))
		tmp.table <- slot.data[data.table(data.table(CJ(report.ids[["ID"]], tmp.content_areas, tmp.years, "VALID_CASE"), key="V1")[report.ids], key=c("V1", "V2", "V3", "V4"))]
		tmp.table[,FIRST_NAME:=NULL]; tmp.table[,LAST_NAME:=NULL]; tmp.table[,DISTRICT_NUMBER:=NULL]; tmp.table[,SCHOOL_NUMBER:=NULL]; tmp.table[,EMH_LEVEL:=NULL]
		setnames(tmp.table, "i.FIRST_NAME", "FIRST_NAME"); setnames(tmp.table, "i.LAST_NAME", "LAST_NAME");
		setnames(tmp.table, "i.DISTRICT_NUMBER", "DISTRICT_NUMBER"); setnames(tmp.table, "i.SCHOOL_NUMBER", "SCHOOL_NUMBER"); setnames(tmp.table, "i.EMH_LEVEL", "EMH_LEVEL")
		setkeyv(slot.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))

		### Create transformed scale scores

		setkeyv(tmp.table, c("CONTENT_AREA", "YEAR", "GRADE"))
		if (!is.null(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]])) {
			year.for.transition <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Year"]]
			tmp.table[, TRANSFORMED_SCALE_SCORE:=SCALE_SCORE]
			tmp.table[YEAR < year.for.transition, TRANSFORMED_SCALE_SCORE:=piecewiseTransform(
					SCALE_SCORE,
					state,
					CONTENT_AREA,
					YEAR,
					GRADE,
					new.cutscores=sort(c(SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[paste(CONTENT_AREA[1], year.for.transition, sep=".")]][[paste0("loss.hoss_", GRADE[1])]],
											SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[paste(CONTENT_AREA[1], year.for.transition, sep=".")]][[paste("GRADE", GRADE[1], sep="_")]]))),
				by=list(CONTENT_AREA, YEAR, GRADE)]
		} else {
			tmp.table[, TRANSFORMED_SCALE_SCORE:=piecewiseTransform(SCALE_SCORE, state, CONTENT_AREA, YEAR, GRADE), by=list(CONTENT_AREA, YEAR, GRADE)]
		}
		if ("SCALE_SCORE_ACTUAL" %in% names(tmp.table)) tmp.table[,SCALE_SCORE:=SCALE_SCORE_ACTUAL]


		#### Anonymize (if requested) (NOT necessary if wide data is provided)

		if (outputSGP.anonymize) {
			if (!"ETHNICITY" %in% names(tmp.table)) tmp.table[["ETHNICITY"]] <- 1
			if (!"GENDER" %in% names(tmp.table)) tmp.table[["GENDER"]] <- round(runif(dim(tmp.table)[1], min=0, max=1))
			tmp.dt <- tmp.table[,list(ID, ETHNICITY, GENDER)]
			setkey(tmp.dt, ID)
			tmp.dt <- tmp.dt[!duplicated(tmp.dt),]

			tmp.dt[,LAST_NAME:=randomNames::randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="last")]
			tmp.dt[,FIRST_NAME:=randomNames::randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="first")]

			names.dt <- tmp.dt[,list(ID, LAST_NAME, FIRST_NAME)]
			setkey(names.dt, ID)

			setkey(tmp.table, ID)
			tmp.table <- names.dt[tmp.table]
		} ## END if (outputSGP.anonymize)

		### Reshape data set

		variables.to.keep <- intersect(names(tmp.table), c("VALID_CASE", "ID", "LAST_NAME", "FIRST_NAME", "CONTENT_AREA", "YEAR", "GRADE", "EMH_LEVEL",
			"SCALE_SCORE", "TRANSFORMED_SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SGP", getTargetName(target.years=outputSGP.projection.years.for.target),
			"SCHOOL_NUMBER", "DISTRICT_NUMBER", outputSGP.student.groups, "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS"))

		outputSGP.data <- ddcast(unclass.data.table(tmp.table)[,setdiff(variables.to.keep, "VALID_CASE"), with=FALSE], ID + CONTENT_AREA ~ YEAR,
			value.var=setdiff(variables.to.keep, c("VALID_CASE", "ID", "CONTENT_AREA", "YEAR")), sep=".")
		outputSGP.data[,TEMP_CONTENT_AREA:=CONTENT_AREA]


		#### Merge in 1, 2, and 3 year projections

		tmp.proj.names <- paste(tmp.content_areas, tmp.last.year, sep=".")
		if (!all(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
			messageSGP(paste0("\tNOTE: Projections for cuts not available for content areas: ", paste(setdiff(tmp.proj.names, names(sgp_object@SGP[["SGProjections"]])), collapse=", "), "."))
			tmp.proj.names <- intersect(tmp.proj.names, names(sgp_object@SGP[["SGProjections"]]))
		}
		for (j in seq(3)) {
			setkeyv(outputSGP.data, c("ID", "CONTENT_AREA"))
			tmp.list <- list()
			for (i in tmp.proj.names) {
				tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep(paste("PROJ_YEAR", j, sep="_"), names(sgp_object@SGP[["SGProjections"]][[i]]))), with=FALSE])
			}
			outputSGP.data <- data.table(convert.variables(rbindlist(tmp.list, fill=TRUE)), key=paste(key(outputSGP.data), collapse=","))[outputSGP.data]
			for (proj.iter in grep(paste("PROJ_YEAR", j, sep="_"), names(outputSGP.data), value=TRUE)) {
				if (grepl("CURRENT", proj.iter)) tmp.increment <- j else tmp.increment <- j-1
				setnames(outputSGP.data, c(proj.iter, paste("GRADE", tmp.last.year.short, sep=".")), c("TEMP_SCORE", "TEMP_GRADE"))
				outputSGP.data[, TEMP:=piecewiseTransform(
								scale_score=TEMP_SCORE,
								state=state,
								content_area=get.next.content_area(TEMP_GRADE[1], CONTENT_AREA[1], tmp.increment),
								year=yearIncrement(tmp.last.year, tmp.increment),
								grade=get.next.grade(TEMP_GRADE[1], CONTENT_AREA[1], tmp.increment),
								sgp.projections.equated=NULL),
					by=list(CONTENT_AREA, TEMP_GRADE)]

				setnames(outputSGP.data, c("TEMP", "TEMP_SCORE", "TEMP_GRADE"), c(paste(proj.iter, "TRANSFORMED", sep="_"), proj.iter, paste("GRADE", tmp.last.year.short, sep=".")))
			}
		}


		### Tidying up variables

		outputSGP.data[,CONTENT_AREA:=as.integer(as.factor(CONTENT_AREA))]


		#### Rename variables (needs to be improved)

		tmp.order <- c("CY", paste0("PY", 1:20))
		tmp.cuts.for.output <- c(1, 20, 35, 40, 60, 65, 80, 99)
		tmp.years.short <-  tail(tmp.years.short, length(tmp.order))



		## Rename variables to keep

		setnames(outputSGP.data, which(names(outputSGP.data)=="ID"), "STATE_ASSIGNED_ID")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("LAST_NAME", tmp.last.year.short, sep=".")), "LAST_NAME")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("FIRST_NAME", tmp.last.year.short, sep=".")), "FIRST_NAME")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("DISTRICT_NUMBER", tmp.last.year.short, sep=".")), "DISTRICT_NUMBER")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("SCHOOL_NUMBER", tmp.last.year.short, sep=".")), "SCHOOL_NUMBER")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("EMH_LEVEL", tmp.last.year.short, sep=".")), "EMH_LEVEL")
		for (i in outputSGP.student.groups) {
			setnames(outputSGP.data, which(names(outputSGP.data)==paste(i, tmp.last.year.short, sep=".")), i)
		}
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("SCHOOL_ENROLLMENT_STATUS", tmp.last.year.short, sep=".")), "SCHOOL_ENROLLMENT_STATUS")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("DISTRICT_ENROLLMENT_STATUS", tmp.last.year.short, sep=".")), "DISTRICT_ENROLLMENT_STATUS")
		setnames(outputSGP.data, which(names(outputSGP.data)==paste("STATE_ENROLLMENT_STATUS", tmp.last.year.short, sep=".")), "STATE_ENROLLMENT_STATUS")

		if ("ELL_STATUS" %in% outputSGP.student.groups) {
			outputSGP.data[,ELL_STATUS:=as.character(outputSGP.data$ELL_STATUS)]
			outputSGP.data[grep("No", ELL_STATUS), ELL_STATUS:="N"]
			outputSGP.data[grep("Yes", ELL_STATUS), ELL_STATUS:="Y"]
			setnames(outputSGP.data, which(names(outputSGP.data)=="ELL_STATUS"), "LANGUAGE_PROFICIENCY")
		} else {
			outputSGP.data[,LANGUAGE_PROFICIENCY:=as.character(NA)]
		}
		if ("GIFTED_AND_TALENTED_PROGRAM_STATUS" %in% outputSGP.student.groups) {
			setnames(outputSGP.data, which(names(outputSGP.data)=="GIFTED_AND_TALENTED_PROGRAM_STATUS"), "GIFTED_CODE")
		} else {
			outputSGP.data[,GIFTED_CODE:=as.character(NA)]
		}
		if ("HOMELESS_STATUS" %in% outputSGP.student.groups) {
			setnames(outputSGP.data, which(names(outputSGP.data)=="HOMELESS_STATUS"), "HLS_CODE")
		} else {
			outputSGP.data[,HLS_CODE:=as.character(NA)]
		}
		if (any(c("IEP_STATUS", "SPECIAL_EDUCATION_STATUS") %in% names(outputSGP.data))) {
			if (state != "RI") setnames(outputSGP.data, which(names(outputSGP.data) %in% c("IEP_STATUS", "SPECIAL_EDUCATION_STATUS")), "IEP_CODE")
		} else {
			outputSGP.data[,IEP_CODE:=as.character(NA)]
		}
		if (any(c("FREE_REDUCED_LUNCH_STATUS", "DISADVANTAGED_STATUS") %in% names(outputSGP.data))) {
			if (state != "RI") setnames(outputSGP.data, which(names(outputSGP.data) %in% c("FREE_REDUCED_LUNCH_STATUS", "DISADVANTAGED_STATUS")), "FRL_CODE")
		} else {
			outputSGP.data[,FRL_CODE:=as.character(NA)]
		}

		for (i in seq_along(tmp.years.short)) {
			setnames(outputSGP.data, grep(paste("GRADE", rev(tmp.years.short)[i], sep="."), names(outputSGP.data)), paste("GRADE_LEVEL", tmp.order[i], sep="_"))
			setnames(outputSGP.data, grep(paste("SCALE_SCORE", rev(tmp.years.short)[i], sep="."), names(outputSGP.data)) %w/o%
				grep(paste("TRANSFORMED_SCALE_SCORE", rev(tmp.years.short)[i], sep="."), names(outputSGP.data)),
				paste("SCALE_SCORE", tmp.order[i], sep="_"))
			setnames(outputSGP.data, grep(paste("TRANSFORMED_SCALE_SCORE", rev(tmp.years.short)[i], sep="."), names(outputSGP.data)), paste("TRANSFORMED_SCALE_SCORE", tmp.order[i], sep="_"))
			setnames(outputSGP.data, grep(paste("SGP", rev(tmp.years.short)[i], sep="."), names(outputSGP.data)), paste("GROWTH_PERCENTILE", tmp.order[i], sep="_"))
			setnames(outputSGP.data, grep(paste("ACHIEVEMENT_LEVEL", rev(tmp.years.short)[i], sep="."), names(outputSGP.data)), paste("PERFORMANCE_LEVEL", tmp.order[i], sep="_"))
			setnames(outputSGP.data, grep(paste(getTargetName(target.years=outputSGP.projection.years.for.target), rev(tmp.years.short)[i], sep="."), names(outputSGP.data)),
				paste("GROWTH_TARGET", tmp.order[i], sep="_"))
		}


		## NULLify variable to be removed

		for (i in head(tmp.years.short, -1)) {
			outputSGP.data[,paste("LAST_NAME", i, sep="."):=NULL]
			outputSGP.data[,paste("FIRST_NAME", i, sep="."):=NULL]
			outputSGP.data[,paste("DISTRICT_NUMBER", i, sep="."):=NULL]
			outputSGP.data[,paste("SCHOOL_NUMBER", i, sep="."):=NULL]
			outputSGP.data[,paste("EMH_LEVEL", i, sep="."):=NULL]
			for (j in outputSGP.student.groups) {
				outputSGP.data[,paste(j, i, sep="."):=NULL]
			}
			outputSGP.data[,paste("SCHOOL_ENROLLMENT_STATUS", i, sep="."):=NULL]
			outputSGP.data[,paste("DISTRICT_ENROLLMENT_STATUS", i, sep="."):=NULL]
			outputSGP.data[,paste("STATE_ENROLLMENT_STATUS", i, sep="."):=NULL]
		}

		if ("ELL_STATUS" %in% outputSGP.student.groups) {
			outputSGP.student.groups[outputSGP.student.groups=="ELL_STATUS"] <- "LANGUAGE_PROFICIENCY"
		} else {
			outputSGP.student.groups <- c(outputSGP.student.groups, "LANGUAGE_PROFICIENCY")
		}

		if ("GIFTED_AND_TALENTED_PROGRAM_STATUS" %in% outputSGP.student.groups) {
			outputSGP.student.groups[outputSGP.student.groups=="GIFTED_AND_TALENTED_PROGRAM_STATUS"] <- "GIFTED_CODE"
		} else {
			outputSGP.student.groups <- c(outputSGP.student.groups, "GIFTED_CODE")
		}

		if ("HOMELESS_STATUS" %in% outputSGP.student.groups) {
			outputSGP.student.groups[outputSGP.student.groups=="HOMELESS_STATUS"] <- "HLS_CODE"
		} else {
			outputSGP.student.groups <- c(outputSGP.student.groups, "HLS_CODE")
		}

		if (any(c("FREE_REDUCED_LUNCH_STATUS", "DISADVANTAGED_STATUS") %in% outputSGP.student.groups)) {
			if (state != "RI") outputSGP.student.groups[outputSGP.student.groups %in% c("FREE_REDUCED_LUNCH_STATUS", "DISADVANTAGED_STATUS")] <- "FRL_CODE"
		} else {
			outputSGP.student.groups <- c(outputSGP.student.groups, "FRL_CODE")
		}

		if (any(c("IEP_STATUS", "SPECIAL_EDUCATION_STATUS") %in% outputSGP.student.groups)) {
			if (state != "RI") outputSGP.student.groups[outputSGP.student.groups %in% c("IEP_STATUS", "SPECIAL_EDUCATION_STATUS")] <- "IEP_CODE"
		} else {
			outputSGP.student.groups <- c(outputSGP.student.groups, "IEP_CODE")
		}

		## Tidy up outputSGP.student.groups

		for (i in intersect(outputSGP.student.groups, names(outputSGP.data))) {
			if (any(is.na(outputSGP.data[[i]]))) {
				setkeyv(outputSGP.data, c("STATE_ASSIGNED_ID", i))
				outputSGP.data[,(i):=outputSGP.data[,rep(rev(get(i))[1], .N), by=STATE_ASSIGNED_ID][['V1']]]
			}
		}


		## Create missing variables

		outputSGP.data[,YEAR:=tmp.last.year.short]
		outputSGP.data[,STUDENT_GROWTH_ID:=seq(dim(outputSGP.data)[1])]
		outputSGP.data[,MIDDLE_NAME:=as.character(NA)]
		outputSGP.data[,OCTOBER_ENROLLMENT_STATUS:=as.character(NA)]

		if (length(tmp.years.short) < length(tmp.order)) {
			for (i in tmp.order[(length(tmp.years.short)+1):length(tmp.order)]) {
				outputSGP.data[,paste("GRADE_LEVEL", i, sep="_"):=NA]
				outputSGP.data[,paste("SCALE_SCORE", i, sep="_"):=NA]
				outputSGP.data[,paste("TRANSFORMED_SCALE_SCORE", i, sep="_"):=NA]
				outputSGP.data[,paste("GROWTH_TARGET", i, sep="_"):=NA]
				outputSGP.data[,paste("GROWTH_PERCENTILE", i, sep="_"):=NA]
				outputSGP.data[,paste("PERFORMANCE_LEVEL", i, sep="_"):=NA]
			}
		}

		tmp.cut.names <- substr(sapply(strsplit(names(outputSGP.data)[grep("YEAR_1_CURRENT_TRANSFORMED", names(outputSGP.data))], "_"), '[', 1), 2, 5)
		tmp.cut.names.missing <- setdiff(tmp.cuts.for.output, tmp.cut.names)
		for (i in 1:3) {
			setnames(outputSGP.data, grep(paste("YEAR", i, "CURRENT_TRANSFORMED", sep="_"), names(outputSGP.data)), paste("CUT", tmp.cut.names, "YEAR", i, sep="_"))
			if (length(tmp.cut.names.missing) > 0) outputSGP.data[,paste("CUT", tmp.cut.names.missing, "YEAR", i, sep="_"):=NA]
		}

		tmp.names <- grep("GROWTH_TARGET", names(outputSGP.data), value=TRUE)
		outputSGP.data[,gsub("GROWTH_TARGET", "SGP_TARGET", tmp.names):=outputSGP.data[,tmp.names,with=FALSE]]


		## Rearrange variables

		tmp.variable.names <- c("STUDENT_GROWTH_ID", "STATE_ASSIGNED_ID", "LAST_NAME", "FIRST_NAME", "MIDDLE_NAME",
			"CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL",
			unique(outputSGP.student.groups),
			"OCTOBER_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS",
			"GRADE_LEVEL_CY", "SCALE_SCORE_CY", "TRANSFORMED_SCALE_SCORE_CY", "PERFORMANCE_LEVEL_CY", "GROWTH_PERCENTILE_CY", "SGP_TARGET_CY", "GROWTH_TARGET_CY",
			"GRADE_LEVEL_PY1", "SCALE_SCORE_PY1", "TRANSFORMED_SCALE_SCORE_PY1", "PERFORMANCE_LEVEL_PY1", "GROWTH_PERCENTILE_PY1", "SGP_TARGET_PY1", "GROWTH_TARGET_PY1",
			"GRADE_LEVEL_PY2", "SCALE_SCORE_PY2", "TRANSFORMED_SCALE_SCORE_PY2", "PERFORMANCE_LEVEL_PY2", "GROWTH_PERCENTILE_PY2", "SGP_TARGET_PY2", "GROWTH_TARGET_PY2",
			"GRADE_LEVEL_PY3", "SCALE_SCORE_PY3", "TRANSFORMED_SCALE_SCORE_PY3", "PERFORMANCE_LEVEL_PY3", "GROWTH_PERCENTILE_PY3", "SGP_TARGET_PY3", "GROWTH_TARGET_PY3",
			"GRADE_LEVEL_PY4", "SCALE_SCORE_PY4", "TRANSFORMED_SCALE_SCORE_PY4", "PERFORMANCE_LEVEL_PY4", "GROWTH_PERCENTILE_PY4", "SGP_TARGET_PY4", "GROWTH_TARGET_PY4",
			"GRADE_LEVEL_PY5", "SCALE_SCORE_PY5", "TRANSFORMED_SCALE_SCORE_PY5", "PERFORMANCE_LEVEL_PY5", "GROWTH_PERCENTILE_PY5", "SGP_TARGET_PY5", "GROWTH_TARGET_PY5",
			"GRADE_LEVEL_PY6", "SCALE_SCORE_PY6", "TRANSFORMED_SCALE_SCORE_PY6", "PERFORMANCE_LEVEL_PY6", "GROWTH_PERCENTILE_PY6", "SGP_TARGET_PY6", "GROWTH_TARGET_PY6",
			"GRADE_LEVEL_PY7", "SCALE_SCORE_PY7", "TRANSFORMED_SCALE_SCORE_PY7", "PERFORMANCE_LEVEL_PY7", "GROWTH_PERCENTILE_PY7", "SGP_TARGET_PY7", "GROWTH_TARGET_PY7",
			c(paste("CUT", tmp.cuts.for.output, "YEAR_1", sep="_"), paste("CUT", tmp.cuts.for.output, "YEAR_2", sep="_"), paste("CUT", tmp.cuts.for.output, "YEAR_3", sep="_")))

		STUDENT_GROWTH <- outputSGP.data[,tmp.variable.names, with=FALSE]


		## Check for NAs in select variables STUDENT_GROWTH

		variables.to.check <- c("EMH_LEVEL", unique(outputSGP.student.groups)) %w/o% c("GIFTED_CODE", "HLS_CODE", "IEP_CODE", "FRL_CODE", "LANGUAGE_PROFICIENCY", "HIGH_NEED_STATUS")

		for (i in variables.to.check) {
			if (any(is.na(STUDENT_GROWTH[[i]]))) {
				messageSGP(paste("\tNAs are present in variable:", i, "of the 'STUDENT_GROWTH' table. NAs being changed to 'Unknown' to avoid data loading problems."))
				STUDENT_GROWTH[,(i):=as.character(get(i))]
				STUDENT_GROWTH[is.na(get(i)), (i):="Unknown"]
			}
		}


		## Output results

		fwrite(STUDENT_GROWTH, file=file.path(outputSGP.directory, "SchoolView", "TEXT", "STUDENT_GROWTH.dat"), quote=FALSE, sep="|")
			tmp.working.directory <- getwd()
			setwd(file.path(outputSGP.directory, "SchoolView", "TEXT"))
			if ("STUDENT_GROWTH.dat.zip" %in% list.files()) file.remove("STUDENT_GROWTH.dat.zip")
			suppressMessages(
				zip("STUDENT_GROWTH.dat.zip", "STUDENT_GROWTH.dat", flags="-rmq1")
			)
			setwd(tmp.working.directory)

		dir.create(file.path(outputSGP.directory, "SchoolView", "RDATA"), recursive=TRUE, showWarnings=FALSE)
		save(STUDENT_GROWTH, file=file.path(outputSGP.directory, "SchoolView", "RDATA", "STUDENT_GROWTH.Rdata"))

		messageSGP(paste("\tFinished SchoolView STUDENT_GROWTH data production in outputSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))

	} ## End if SchoolView %in% output.type

	###############################################
	###
	### RLI output
	###
	###############################################

	if (any(c("RLI", "RLI_UK") %in% output.type)) {

		started.at <- proc.time()
		messageSGP(paste("\tStarted RLI in outputSGP", prettyDate()))

		### SGPercentiles

		for (names.iter in grep("BASELINE", names(sgp_object@SGP[['SGPercentiles']]), value=TRUE)) {
			dir.create(file.path(outputSGP.directory, "RLI", "SGPercentiles"), recursive=TRUE, showWarnings=FALSE)
			file.label <- gsub("_RASCH", "", names.iter)

			output.column.order <- c(SGP::SGPstateData$RLI$SGP_Configuration$output.column.order$SGPercentiles, outputSGP.pass.through.variables)
			tmp.dt <- sgp_object@Data[,c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", outputSGP.pass.through.variables), with=FALSE][
				data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(names.iter, "[.]"))[1],
					YEAR=getTableNameYear(names.iter),
					sgp_object@SGP[["SGPercentiles"]][[names.iter]]), on=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")]
			if (any(!output.column.order %in% names(tmp.dt))) tmp.dt[,(output.column.order[!output.column.order %in% names(tmp.dt)]):=as.numeric(NA)]
			tmp.dt <- tmp.dt[,ID:=gsub("_DUPS_[0-9]*", "", ID)][,SGP_NORM_GROUP_BASELINE:=gsub("_RASCH", "", SGP_NORM_GROUP_BASELINE)][,output.column.order, with=FALSE]
			fwrite(tmp.dt, file=file.path(outputSGP.directory, "RLI", "SGPercentiles", paste(file.label, "txt", sep=".")), sep=",", quote=FALSE)

			if (identical(.Platform$OS.type, "unix")) {
				if (file.info(file.path(outputSGP.directory, "RLI", "SGPercentiles", paste(file.label, "txt", sep=".")))$size > 4000000000) {
					tmp.working.directory <- getwd()
					setwd(file.path(outputSGP.directory, "RLI", "SGPercentiles"))
					if (paste(file.label, "txt.gz", sep=".") %in% list.files()) file.remove(paste(file.label, "txt.gz", sep="."))
					system(paste("gzip", paste(file.label, "txt", sep=".")))
					setwd(tmp.working.directory)
				} else {
					tmp.working.directory <- getwd()
					setwd(file.path(outputSGP.directory, "RLI", "SGPercentiles"))
					if (paste(file.label, "txt.zip", sep=".") %in% list.files()) file.remove(paste(file.label, "txt.zip", sep="."))
					suppressMessages(
						zip(paste(file.label, "txt.zip", sep="."), paste(file.label, "txt", sep="."), flags="-rmq1")
					)
					setwd(tmp.working.directory)
				}
			}
		}

		### SGProjections

		## Create CATCH_UP_KEEP_UP_INITIAL/MOVE_UP_STAY_UP_INITIAL variables

		slot.data <- copy(sgp_object@Data)
		setkey(slot.data, STATE)
		tmp.unique.states <- sort(unique(slot.data, by='STATE')[['STATE']])
		tmp.unique.states <- intersect(tmp.unique.states, SGP::SGPstateData[["RLI"]][["Achievement"]][["Cutscore_Information"]][['Cutscore_States']])

		for (target.level in c("CATCH_UP_KEEP_UP", "MOVE_UP_STAY_UP")) {
			for (state.iter in tmp.unique.states) {
				slot.data[state.iter, paste(target.level, "STATUS_INITIAL", sep="_") :=
					as.character(getTargetInitialStatus(slot.data[state.iter][['ACHIEVEMENT_LEVEL']], state, state.iter, target.level))]
			}
		}
		setkey(slot.data, VALID_CASE, CONTENT_AREA, YEAR, ID)

		if (any(c("CATCH_UP_KEEP_UP_STATUS_INITIAL", "MOVE_UP_STAY_UP_STATUS_INITIAL") %in% names(slot.data))) {
			setnames(slot.data, intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL", "MOVE_UP_STAY_UP_STATUS_INITIAL")),
				paste(intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL", "MOVE_UP_STAY_UP_STATUS_INITIAL")), "CURRENT", sep="_"))
		}

		for (names.iter in grep("BASELINE", names(sgp_object@SGP[['SGProjections']]), value=TRUE)) {
			dir.create(file.path(outputSGP.directory, "RLI", "SGProjections"), recursive=TRUE, showWarnings=FALSE)
			file.label <- gsub("_RASCH", "", names.iter)

			if (!grepl("TARGET_SCALE_SCORES", names.iter)) {
				tmp.table <- data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(names.iter, "[.]"))[1],
					YEAR=getTableNameYear(names.iter),
					sgp_object@SGP[["SGProjections"]][[names.iter]], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
				tmp.index <- tmp.table[,c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"), with=FALSE]

				if (any(c("CATCH_UP_KEEP_UP_STATUS_INITIAL_CURRENT", "MOVE_UP_STAY_UP_STATUS_INITIAL_CURRENT") %in% names(slot.data))) {
					sgp_object@SGP[["SGProjections"]][[names.iter]] <-
						tmp.table[,intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL_CURRENT", "MOVE_UP_STAY_UP_STATUS_INITIAL_CURRENT")) := slot.data[tmp.index][,
							intersect(names(slot.data), c("CATCH_UP_KEEP_UP_STATUS_INITIAL_CURRENT", "MOVE_UP_STAY_UP_STATUS_INITIAL_CURRENT")), with=FALSE]][,!c("VALID_CASE", "CONTENT_AREA", "YEAR"), with=FALSE]
				}
				output.column.order <- SGP::SGPstateData[['RLI']][['SGP_Configuration']][['output.column.order']][['SGProjection']]
			} else {
				if (any(grepl("6_TIME", names(sgp_object@SGP[['SGProjections']][[names.iter]])))) {
					output.column.order <- SGP::SGPstateData[['RLI']][['SGP_Configuration']][['output.column.order']][['SGProjection_Target_6_TIME']]
				}
				if (any(grepl("10_TIME", names(sgp_object@SGP[['SGProjections']][[names.iter]])))) {
					output.column.order <- SGP::SGPstateData[['RLI']][['SGP_Configuration']][['output.column.order']][['SGProjection_Target_10_TIME']]
				}
			}

			output.column.order <- c(output.column.order, outputSGP.pass.through.variables)
			tmp.dt <- sgp_object@Data[,c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", outputSGP.pass.through.variables), with=FALSE][
				data.table(
					VALID_CASE="VALID_CASE",
					CONTENT_AREA=unlist(strsplit(names.iter, "[.]"))[1],
					YEAR=getTableNameYear(names.iter),
					sgp_object@SGP[["SGProjections"]][[names.iter]]), on=c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")][,GROUP:=names.iter]
			if (any(!output.column.order %in% names(tmp.dt))) tmp.dt[,output.column.order[!output.column.order %in% names(tmp.dt)]:=as.numeric(NA)]
			tmp.dt <- tmp.dt[,ID:=gsub("_DUPS_[0-9]*", "", ID)][,CONTENT_AREA:=gsub("_RASCH", "", CONTENT_AREA)][,SGP_PROJECTION_GROUP:=gsub("_RASCH", "", SGP_PROJECTION_GROUP)][,GROUP:=gsub("_RASCH", "", GROUP)][,output.column.order, with=FALSE]
			fwrite(tmp.dt, file=file.path(outputSGP.directory, "RLI", "SGProjections", paste(file.label, "txt", sep=".")), sep=",", quote=FALSE)

			if (identical(.Platform$OS.type, "unix")) {
				if (file.info(file.path(outputSGP.directory, "RLI", "SGProjections", paste(file.label, "txt", sep=".")))$size > 4000000000) {
					tmp.working.directory <- getwd()
					setwd(file.path(outputSGP.directory, "RLI", "SGProjections"))
					if (paste(file.label, "txt.gz", sep=".") %in% list.files()) file.remove(paste(file.label, "txt.gz", sep="."))
					system(paste("gzip", paste(file.label, "txt", sep=".")))
					setwd(tmp.working.directory)
				} else {
					tmp.working.directory <- getwd()
					setwd(file.path(outputSGP.directory, "RLI", "SGProjections"))
					if (paste(file.label, "txt.zip", sep=".") %in% list.files()) file.remove(paste(file.label, "txt.zip", sep="."))
					suppressMessages(
						zip(paste(file.label, "txt.zip", sep="."), paste(file.label, "txt", sep="."), flags="-rmq1")
					)
					setwd(tmp.working.directory)
				}
			}
		}

		messageSGP(paste("\tFinished RLI in outputSGP", prettyDate(), "in", convertTime(timetaken(started.at)), "\n"))

	} ## End if RLI %in% output.type


	setkeyv(sgp_object@Data, getKey(sgp_object))
	messageSGP(paste("Finished outputSGP", prettyDate(), "in", convertTime(timetaken(started.at.outputSGP)), "\n"))

} ## END outputSGP
