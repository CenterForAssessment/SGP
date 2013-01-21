`outputSGP` <- 
function(sgp_object,
	state=NULL,
	output.type=c("LONG_Data", "WIDE_Data"),
	baseline.sgps=FALSE,
	outputSGP_SUMMARY.years=NULL,
	outputSGP_SUMMARY.content_areas=NULL,
	outputSGP_INDIVIDUAL.years=NULL,
	outputSGP_INDIVIDUAL.content_areas=NULL,
	outputSGP.anonymize=FALSE,
	outputSGP.student.groups=NULL,
	outputSGP.directory="Data",
	outputSGP.translate.names=TRUE,
	outputSGP.projection.years.for.target=3) {

        started.at.outputSGP <- proc.time()
        message(paste("\nStarted outputSGP ", date(), ": Files produced from outputSGP saved in '", outputSGP.directory, "'\n", sep=""))

	### Create directory

	dir.create(outputSGP.directory, recursive=TRUE, showWarnings=FALSE)

	### Define varaibles (to prevent R CMD check warnings)

	SCALE_SCORE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- ETHNICITY <- GENDER <- LAST_NAME <- FIRST_NAME <- VALID_CASE <- DISTRICT_NUMBER <- SCHOOL_NUMBER <- YEAR_BY_CONTENT_AREA <- NULL
	names.type <- names.provided <- names.output <- STATE_ENROLLMENT_STATUS <- EMH_LEVEL <- NULL

	### Create state (if missing) from sgp_object (if possible)

        if (is.null(state)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "outputSGP")
        }


	### Create relevant variables

	if (is.null(outputSGP.student.groups)) {
		outputSGP.student.groups <- intersect(names(sgp_object@Data), 
			subset(sgp_object@Names, names.type=="demographic" & names.output==TRUE, select=names.provided, drop=TRUE))
	}


	###############################################
	###
	### LONG_Data
	###
	###############################################


	if ("LONG_Data" %in% output.type) {

		### Create state name

		if (state %in% c(state.abb, "DEMO")) {
			tmp.state <- gsub(" ", "_", c(state.name, "Demonstration")[state==c(state.abb, "DEMO")])
		} else {
			tmp.state <- gsub(" ", "_", state)
		}

		### Write LONG table

		started.at <- proc.time()
		message(paste("\tStarted LONG data production in outputSGP", date()))

		names.in.data <- which(sgp_object@Names[['names.sgp']] %in% names(sgp_object@Data))
		if (outputSGP.translate.names) setnames(sgp_object@Data, sgp_object@Names[['names.sgp']][names.in.data], sgp_object@Names[['names.provided']][names.in.data])
		write.table(sgp_object@Data, file=file.path(outputSGP.directory, paste(tmp.state, "SGP_LONG_Data.txt", sep="_")), sep="|", quote=FALSE, row.names=FALSE, na="")
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
					zip(paste(tmp.state, "SGP_LONG_Data.txt.zip", sep="_"), paste(tmp.state, "SGP_LONG_Data.txt", sep="_"))
				)
				setwd(tmp.working.directory)
			}
		}
		if (outputSGP.translate.names) setnames(sgp_object@Data, sgp_object@Names[['names.provided']][names.in.data], sgp_object@Names[['names.sgp']][names.in.data])

		message(paste("\tFinished LONG data production in outputSGP", date(), "in", timetaken(started.at), "\n"))

	} ### END if LONG_Data %in% output.type


	###############################################
	###
	### WIDE data
	###
	###############################################

	if ("WIDE_Data" %in% output.type) {

		### Create state name

		if (state %in% c(state.abb, "DEMO")) {
			tmp.state <- gsub(" ", "_", c(state.name, "Demonstration")[state==c(state.abb, "DEMO")])
		} else {
			tmp.state <- gsub(" ", "_", state)
		}

		### Write WIDE table

		started.at <- proc.time()
		message(paste("\tStarted WIDE data production in outputSGP", date()))

		long_data_tmp <- copy(sgp_object@Data)
		setkeyv(long_data_tmp, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID"))
		suppressMessages(invisible(long_data_tmp[,YEAR_BY_CONTENT_AREA := paste(YEAR, CONTENT_AREA, sep=".")]))
		assign(paste(tmp.state, "SGP_WIDE_Data", sep="_"), reshape(long_data_tmp["VALID_CASE"], idvar="ID", 
			timevar="YEAR_BY_CONTENT_AREA", drop=c("VALID_CASE", "CONTENT_AREA", "YEAR"), direction="wide"))
		eval(parse(text=paste("setkey(", tmp.state, "_SGP_WIDE_Data, ID)", sep="")))

		save(list=paste(tmp.state, "SGP_WIDE_Data", sep="_"), file=file.path(outputSGP.directory, paste(tmp.state, "SGP_WIDE_Data.Rdata", sep="_")))
		write.table(get(paste(tmp.state, "SGP_WIDE_Data", sep="_")), 
			file=file.path(outputSGP.directory, paste(tmp.state, "SGP_WIDE_Data.txt", sep="_")), sep="|", quote=FALSE, row.names=FALSE, na="")

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
					zip(paste(tmp.state, "SGP_WIDE_Data.txt.zip", sep="_"), paste(tmp.state, "SGP_WIDE_Data.txt", sep="_"))
				)
				setwd(tmp.working.directory)
			}
		}

		message(paste("\tFinished WIDE data production in outputSGP", date(), "in", timetaken(started.at), "\n"))

	} ### END if WIDE_Data %in% output.type


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
			message(paste("\tStarted SchoolView STUDENT_GROWTH data production in outputSGP", date()))

		### Utility functions

		.year.increment <- function(year, increment) {
			paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")
		}

		get.my.label <- function(state, content_area, year, label="Cutscores") {
			tmp.cutscore.years <- sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][[label]])[grep(content_area, names(SGPstateData[[state]][["Achievement"]][[label]]))], "[.]"),
				function(x) x[2])
			if (any(!is.na(tmp.cutscore.years))) {
				if (year %in% tmp.cutscore.years) {
					return(paste(content_area, year, sep="."))
				} else {
					if (year==sort(c(year, tmp.cutscore.years))[1]) {
						return(content_area)
					} else {
						return(paste(content_area, sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1], sep="."))
					}
				}
			} else {
				return(content_area)
			}
		}

		piecewise.transform <- function(scale_score, state, content_area, year, grade, output.digits=1) {
			if (content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &
				grade %in% as.numeric(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]]), "_")), 
					ncol=2, byrow=TRUE)[,2])) {
				my.knots_boundaries.label <- get.my.label(state, content_area, year, "Knots_Boundaries")
				tmp.loss.hoss <- SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[my.knots_boundaries.label]][[paste("loss.hoss_", grade, sep="")]]
				scale_score[scale_score < tmp.loss.hoss[1]] <- tmp.loss.hoss[1]; scale_score[scale_score > tmp.loss.hoss[2]] <- tmp.loss.hoss[2]
				my.content_area <- get.my.label(state, content_area, year)
				tmp.old.cuts <- c(tmp.loss.hoss[1], SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE_", grade, sep="")]], 
					tmp.loss.hoss[2])
				tmp.new.cuts <- SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]]
				tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
				tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
				round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
			} else {
				as.numeric(scale_score)
			}
		} ## END piecewise.transform

		"%w/o%" <- function(x,y) x[!x %in% y]

		convert.variables <- function(tmp.df) {
			if ("YEAR" %in% names(tmp.df) && is.character(tmp.df$YEAR)) {
				tmp.df$YEAR <- as.integer(sapply(strsplit(tmp.df$YEAR, "_"), '[', 2))
			}
			if ("CONTENT_AREA" %in% names(tmp.df) && is.character(tmp.df$CONTENT_AREA)) {
				tmp.df$CONTENT_AREA <- as.integer(as.factor(tmp.df$CONTENT_AREA))
			}
			if ("LAST_NAME" %in% names(tmp.df) && is.factor(tmp.df$LAST_NAME)) {
				tmp.df$LAST_NAME <- as.character(tmp.df$LAST_NAME)
			}
			if ("FIRST_NAME" %in% names(tmp.df) && is.factor(tmp.df$FIRST_NAME)) {
				tmp.df$FIRST_NAME <- as.character(tmp.df$FIRST_NAME)
			}
			if ("EMH_LEVEL" %in% names(tmp.df) && is.factor(tmp.df$EMH_LEVEL)) {
				tmp.df[['EMH_LEVEL']] <- substr(tmp.df$EMH_LEVEL, 1, 1)
			}
			if ("GENDER" %in% names(tmp.df) && is.factor(tmp.df$GENDER)) {
				tmp.df[['GENDER']] <- substr(tmp.df$GENDER, 1, 1)
			}
			for (names.iter in c(outputSGP.student.groups, "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS") %w/o% "ETHNICITY") {
				if (names.iter %in% names(tmp.df) && is.factor(tmp.df[[names.iter]])) {
					tmp.df[[names.iter]] <- as.character(tmp.df[[names.iter]])
					tmp.df[[names.iter]][grep("Yes", tmp.df[[names.iter]])] <- "Y"
					tmp.df[[names.iter]][grep("No", tmp.df[[names.iter]])] <- "N"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="Students with Disabilities (IEP)"] <- "Y"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="Economically Disadvantaged"] <- "Y"
					tmp.df[[names.iter]][tmp.df[[names.iter]]=="English Language Learners (ELL)"] <- "N"
				}
			}
			return(tmp.df)
		}

		unclass.data.table <- function(my.data.table) {
			as.data.table(lapply(convert.variables(subset(my.data.table, VALID_CASE=="VALID_CASE")), unclass))
		}

		#### Set key

		long.key <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
		setkeyv(sgp_object@Data, long.key)


		#### Year stuff 

		if (is.null(outputSGP_INDIVIDUAL.years)) {
			tmp.years <- sort(unique(sgp_object@Data["VALID_CASE"][["YEAR"]]))
			tmp.last.year <- tail(tmp.years, 1)
			tmp.years.short <- sapply(strsplit(tmp.years, "_"), '[', 2)
			tmp.last.year.short <- tail(unlist(strsplit(tail(tmp.years, 1), "_")), 1)
		} else {
			tmp.all.years <- sort(unique(sgp_object@Data["VALID_CASE"][["YEAR"]])) 
			tmp.years <- tmp.all.years[1:which(tmp.all.years==tail(sort(outputSGP_INDIVIDUAL.years), 1))]
			tmp.last.year <- tail(tmp.years, 1)
			tmp.years.short <- sapply(strsplit(tmp.years, "_"), '[', 2)
			tmp.last.year.short <- tail(unlist(strsplit(tail(tmp.years, 1), "_")), 1)
		}


		#### Content area stuff 

		if (is.null(outputSGP_INDIVIDUAL.content_areas)) {
			tmp.content_areas <- sort(unique(sgp_object@Data[SJ("VALID_CASE", tmp.last.year)][["CONTENT_AREA"]]))
		} else {
			tmp.content_areas <- sort(outputSGP_INDIVIDUAL.content_areas)
		}


		### subset data

		tmp.districts.and.schools <- unique(data.table(sgp_object@Data[CJ("VALID_CASE", tmp.last.year, tmp.content_areas)][,
								list(VALID_CASE, YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER, EMH_LEVEL)], key=key(sgp_object)))
		report.ids <- data.table(sgp_object@Data[tmp.districts.and.schools][VALID_CASE=="VALID_CASE" & STATE_ENROLLMENT_STATUS=="Enrolled State: Yes"][, 
			list(ID, FIRST_NAME, LAST_NAME, DISTRICT_NUMBER, SCHOOL_NUMBER, EMH_LEVEL)], key=c("ID", "FIRST_NAME", "LAST_NAME", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
		setkey(report.ids, ID)
		report.ids <- unique(report.ids)
		setkeyv(sgp_object@Data, c("ID", "CONTENT_AREA", "YEAR", "VALID_CASE"))
		tmp.table <- sgp_object@Data[CJ(report.ids[["ID"]], tmp.content_areas, tmp.years, "VALID_CASE")[report.ids]]
		tmp.table[,FIRST_NAME:=NULL]; tmp.table[,LAST_NAME:=NULL]; tmp.table[,DISTRICT_NUMBER:=NULL]; tmp.table[,SCHOOL_NUMBER:=NULL]; tmp.table[,EMH_LEVEL:=NULL]
		setnames(tmp.table, "FIRST_NAME.1", "FIRST_NAME"); setnames(tmp.table, "LAST_NAME.1", "LAST_NAME"); 
		setnames(tmp.table, "DISTRICT_NUMBER.1", "DISTRICT_NUMBER"); setnames(tmp.table, "SCHOOL_NUMBER.1", "SCHOOL_NUMBER"); setnames(tmp.table, "EMH_LEVEL.1", "EMH_LEVEL")
		setkeyv(sgp_object@Data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))

		### Create transformed scale scores

		setkeyv(tmp.table, c("CONTENT_AREA", "YEAR", "GRADE"))
		tmp.table$TRANSFORMED_SCALE_SCORE <- tmp.table[,
			piecewise.transform(SCALE_SCORE, state, as.character(CONTENT_AREA[1]), as.character(YEAR[1]), as.character(GRADE[1])), 
				by=list(CONTENT_AREA, YEAR, GRADE)]$V1


		#### Anonymize (if requested) (NOT necessary if wide data is provided)
 
		if (outputSGP.anonymize | !all(c("LAST_NAME", "FIRST_NAME") %in% names(tmp.table))) {
			suppressPackageStartupMessages(require(randomNames))
			if (!"ETHNICITY" %in% names(tmp.table)) tmp.table[["ETHNICITY"]] <- 1
			if (!"GENDER" %in% names(tmp.table)) tmp.table[["GENDER"]] <- round(runif(dim(tmp.table)[1], min=0, max=1))
			tmp.dt <- tmp.table[,list(ID, ETHNICITY, GENDER)]
			setkey(tmp.dt, ID)
			tmp.dt <- tmp.dt[!duplicated(tmp.dt),]

			tmp.dt$LAST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="last")
			tmp.dt$FIRST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="first")	

			names.dt <- tmp.dt[,list(ID, LAST_NAME, FIRST_NAME)]
			setkey(names.dt, ID)

			setkey(tmp.table, ID)
			tmp.table <- names.dt[tmp.table]
		} ## END if (outputSGP.anonymize)


	### Reshape data set

		variables.to.keep <- c("VALID_CASE", "ID", "LAST_NAME", "FIRST_NAME", "CONTENT_AREA", "YEAR", "GRADE", "EMH_LEVEL", 
			"SCALE_SCORE", "TRANSFORMED_SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SGP", getTargetName(target.years=outputSGP.projection.years.for.target),
			"SCHOOL_NUMBER", "DISTRICT_NUMBER", outputSGP.student.groups, "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS")

		outputSGP.data <- reshape(unclass.data.table(tmp.table)[, variables.to.keep, with=FALSE],
			idvar=c("ID", "CONTENT_AREA"),
			timevar="YEAR",
			drop=c("VALID_CASE"),
			direction="wide")


		#### Merge in 1, 2, and 3 year projections 

		for (j in 1:3) {
			tmp.proj.names <- paste(tmp.content_areas, tmp.last.year, sep=".")
			if (all(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
				setkeyv(outputSGP.data, c("ID", "CONTENT_AREA"))
				tmp.list <- list()
				for (i in tmp.proj.names) {
					tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
						sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep(paste("PROJ_YEAR", j, sep="_"), names(sgp_object@SGP[["SGProjections"]][[i]])))])
				}
				outputSGP.data <- data.table(convert.variables(rbind.fill(tmp.list)), key=paste(key(outputSGP.data), collapse=","))[outputSGP.data]
				tmp.grade.name <- paste("GRADE", tmp.last.year.short, sep=".")
				tmp.year.name <- .year.increment(tmp.last.year.short, j)
				setkeyv(outputSGP.data, c("CONTENT_AREA", tmp.grade.name))
				for (proj.iter in grep(paste("PROJ_YEAR", j, sep="_"), names(outputSGP.data))) {
					tmp.scale_score.name <- names(outputSGP.data)[proj.iter]
					outputSGP.data[[proj.iter]] <- outputSGP.data[,
						piecewise.transform(get(tmp.scale_score.name), state, tmp.content_areas[CONTENT_AREA[1]], tmp.year.name, as.character(type.convert(get(tmp.grade.name)[1])+1)), 
						by=list(CONTENT_AREA, outputSGP.data[[tmp.grade.name]])]$V1 
				}
			}
		}


		#### Rename variables (needs to be improved)

		tmp.order <- c("CY", "PY1", "PY2", "PY3", "PY4", "PY5", "PY6", "PY7")

	
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
			setnames(outputSGP.data, which(names(outputSGP.data)=="ELL_STATUS"), "LANGUAGE_PROFICIENCY")
		} else {
			outputSGP.data[['LANGUAGE_PROFICIENCY']] <- as.character(NA)
		}
		if ("GIFTED_AND_TALENTED_PROGRAM_STATUS" %in% outputSGP.student.groups) {
			setnames(outputSGP.data, which(names(outputSGP.data)=="GIFTED_AND_TALENTED_PROGRAM_STATUS"), "GIFTED_CODE")
		} else {
			outputSGP.data[['GIFTED_CODE']] <- as.character(NA)
		}
		if ("HOMELESS_STATUS" %in% outputSGP.student.groups) {
			setnames(outputSGP.data, which(names(outputSGP.data)=="HOMELESS_STATUS"), "HLS_CODE")
		} else {
			outputSGP.data[['HLS_CODE']] <- as.character(NA)
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
			outputSGP.data[[paste("LAST_NAME", i, sep=".")]] <- NULL
			outputSGP.data[[paste("FIRST_NAME", i, sep=".")]] <- NULL
			outputSGP.data[[paste("DISTRICT_NUMBER", i, sep=".")]] <- NULL
			outputSGP.data[[paste("SCHOOL_NUMBER", i, sep=".")]] <- NULL
			outputSGP.data[[paste("EMH_LEVEL", i, sep=".")]] <- NULL
			for (j in outputSGP.student.groups) {
				outputSGP.data[[paste(j, i, sep=".")]] <- NULL
			}
			outputSGP.data[[paste("SCHOOL_ENROLLMENT_STATUS", i, sep=".")]] <- NULL
			outputSGP.data[[paste("DISTRICT_ENROLLMENT_STATUS", i, sep=".")]] <- NULL
			outputSGP.data[[paste("STATE_ENROLLMENT_STATUS", i, sep=".")]] <- NULL
	
			if ("ELL_STATUS" %in% outputSGP.student.groups) {
				outputSGP.data[[paste("ELL_STATUS", i, sep=".")]] <- NULL
				outputSGP.student.groups[outputSGP.student.groups=="ELL_STATUS"] <- "LANGUAGE_PROFICIENCY"
			} else {
				outputSGP.student.groups <- c(outputSGP.student.groups, "LANGUAGE_PROFICIENCY")
			}
	
			if ("GIFTED_AND_TALENTED_PROGRAM_STATUS" %in% outputSGP.student.groups) {
				outputSGP.data[[paste("GIFTED_AND_TALENTED_PROGRAM_STATUS", i, sep=".")]] <- NULL
				outputSGP.student.groups[outputSGP.student.groups=="GIFTED_AND_TALENTED_PROGRAM_STATUS"] <- "GIFTED_CODE"
			} else {
				outputSGP.student.groups <- c(outputSGP.student.groups, "GIFTED_CODE")
			}
	
			if ("HOMELESS_STATUS" %in% outputSGP.student.groups) {
				outputSGP.data[[paste("HOMELESS_STATUS", i, sep=".")]] <- NULL
				outputSGP.student.groups[outputSGP.student.groups=="HOMELESS_STATUS"] <- "HLS_CODE"
			} else {
				outputSGP.student.groups <- c(outputSGP.student.groups, "HLS_CODE")
			}
		}


		## Create missing variables

		outputSGP.data[["YEAR"]] <- tmp.last.year.short
		outputSGP.data[["STUDENT_GROWTH_ID"]] <- seq(dim(outputSGP.data)[1])
		outputSGP.data[["MIDDLE_NAME"]] <- as.character(NA)
		outputSGP.data[["OCTOBER_ENROLLMENT_STATUS"]] <- as.character(NA)

		if (length(tmp.years.short) < length(tmp.order)) {
			for (i in tmp.order[(length(tmp.years.short)+1):length(tmp.order)]) {
				outputSGP.data[[paste("GRADE_LEVEL", i, sep="_")]] <- NA
				outputSGP.data[[paste("SCALE_SCORE", i, sep="_")]] <- NA
				outputSGP.data[[paste("TRANSFORMED_SCALE_SCORE", i, sep="_")]] <- NA
				outputSGP.data[[paste("GROWTH_TARGET", i, sep="_")]] <- NA
				outputSGP.data[[paste("GROWTH_PERCENTILE", i, sep="_")]] <- NA
				outputSGP.data[[paste("PERFORMANCE_LEVEL", i, sep="_")]] <- NA
			}
		}	

		if (length(names(outputSGP.data)[grep("YEAR_1", names(outputSGP.data))]) == 4) {
			for (i in 1:3) {
				setnames(outputSGP.data, grep(paste("YEAR", i, sep="_"), names(outputSGP.data)), paste("CUT", c(1, 35, 65, 99), "YEAR", i, sep="_"))
				for (j in c(20, 40, 60, 80)) {
					outputSGP.data[[paste("CUT", j, "YEAR", i, sep="_")]] <- NA	
				}
			}
		}
		if (length(names(outputSGP.data)[grep("YEAR_1", names(outputSGP.data))]) == 6) {
			for (i in 1:3) {
				setnames(outputSGP.data, grep(paste("YEAR", i, sep="_"), names(outputSGP.data)), paste("CUT", c(1, 20, 40, 60, 80, 99), "YEAR", i, sep="_"))
				for (j in c(35, 65)) {
					outputSGP.data[[paste("CUT", j, "YEAR", i, sep="_")]] <- NA
				}
			}
		}

		## Rearrange variables

		tmp.gt.name <- getTargetName(target.years=outputSGP.projection.years.for.target, target.label="GROWTH_TARGET")
		tmp.variable.names <- c("STUDENT_GROWTH_ID", "STATE_ASSIGNED_ID", "LAST_NAME", "FIRST_NAME", "MIDDLE_NAME", 
			"CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL",
			unique(outputSGP.student.groups),
			"OCTOBER_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS",
			"GRADE_LEVEL_CY", "SCALE_SCORE_CY", "TRANSFORMED_SCALE_SCORE_CY", "PERFORMANCE_LEVEL_CY", "GROWTH_PERCENTILE_CY", "GROWTH_TARGET_CY", 
			"GRADE_LEVEL_PY1", "SCALE_SCORE_PY1", "TRANSFORMED_SCALE_SCORE_PY1", "PERFORMANCE_LEVEL_PY1", "GROWTH_PERCENTILE_PY1", "GROWTH_TARGET_PY1",
			"GRADE_LEVEL_PY2", "SCALE_SCORE_PY2", "TRANSFORMED_SCALE_SCORE_PY2", "PERFORMANCE_LEVEL_PY2", "GROWTH_PERCENTILE_PY2", "GROWTH_TARGET_PY2",
			"GRADE_LEVEL_PY3", "SCALE_SCORE_PY3", "TRANSFORMED_SCALE_SCORE_PY3", "PERFORMANCE_LEVEL_PY3", "GROWTH_PERCENTILE_PY3", "GROWTH_TARGET_PY3",
			"GRADE_LEVEL_PY4", "SCALE_SCORE_PY4", "TRANSFORMED_SCALE_SCORE_PY4", "PERFORMANCE_LEVEL_PY4", "GROWTH_PERCENTILE_PY4", "GROWTH_TARGET_PY4",
			"GRADE_LEVEL_PY5", "SCALE_SCORE_PY5", "TRANSFORMED_SCALE_SCORE_PY5", "PERFORMANCE_LEVEL_PY5", "GROWTH_PERCENTILE_PY5", "GROWTH_TARGET_PY5",
			"GRADE_LEVEL_PY6", "SCALE_SCORE_PY6", "TRANSFORMED_SCALE_SCORE_PY6", "PERFORMANCE_LEVEL_PY6", "GROWTH_PERCENTILE_PY6", "GROWTH_TARGET_PY6",
			"GRADE_LEVEL_PY7", "SCALE_SCORE_PY7", "TRANSFORMED_SCALE_SCORE_PY7", "PERFORMANCE_LEVEL_PY7", "GROWTH_PERCENTILE_PY7", "GROWTH_TARGET_PY7",
			"CUT_1_YEAR_1", "CUT_99_YEAR_1", "CUT_35_YEAR_1", "CUT_65_YEAR_1", "CUT_20_YEAR_1", "CUT_40_YEAR_1", "CUT_60_YEAR_1", "CUT_80_YEAR_1",
			"CUT_1_YEAR_2", "CUT_99_YEAR_2", "CUT_35_YEAR_2", "CUT_65_YEAR_2", "CUT_20_YEAR_2", "CUT_40_YEAR_2", "CUT_60_YEAR_2", "CUT_80_YEAR_2",
			"CUT_1_YEAR_3", "CUT_99_YEAR_3", "CUT_35_YEAR_3", "CUT_65_YEAR_3", "CUT_20_YEAR_3", "CUT_40_YEAR_3", "CUT_60_YEAR_3", "CUT_80_YEAR_3")

		write.table(outputSGP.data[,tmp.variable.names, with=FALSE], file=file.path(outputSGP.directory, "SchoolView", "STUDENT_GROWTH.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")
			tmp.working.directory <- getwd()
			setwd(file.path(outputSGP.directory, "SchoolView"))
			if ("STUDENT_GROWTH.dat.zip" %in% list.files()) file.remove("STUDENT_GROWTH.dat.zip")
			suppressMessages(
				zip("STUDENT_GROWTH.dat.zip", "STUDENT_GROWTH.dat")
			)
			setwd(tmp.working.directory)

		STUDENT_GROWTH <- outputSGP.data[,tmp.variable.names, with=FALSE]
		save(STUDENT_GROWTH, file=file.path(outputSGP.directory, "SchoolView", "STUDENT_GROWTH.Rdata"))

		message(paste("\tFinished SchoolView STUDENT_GROWTH data production in outputSGP", date(), "in", timetaken(started.at), "\n"))

	} ## End if SchoolView %in% output.type

		setkey(sgp_object@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
		message(paste("Finished outputSGP", date(), "in", timetaken(started.at.outputSGP), "\n"))

} ## END outputSGP

