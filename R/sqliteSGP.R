`sqliteSGP` <-
function(sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	other.student.groups,
	text.output=TRUE,
	output.directory=file.path("Data", "SchoolView")) {

	YEAR <- DISTRICT_NUMBER <- SCHOOL_NUMBER <- CONTENT_AREA <- DISTRICT_ENROLLMENT_STATUS <- GRADE <- ETHNICITY <- STUDENTGROUP <- SCHOOL_ENROLLMENT_STATUS <- EMH_LEVEL <- NULL

	## Load packages

		require(RSQLite)

        ## Create state (if NULL) from sgp_object (if possible)

		if (is.null(state)) {
			tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
			if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
				state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
			}
		}


	## Create/Set database

		if (state %in% c(state.abb, "DEMO")) {
			tmp.state <- gsub(" ", "_", c(state.name, "Demonstration")[state==c(state.abb, "DEMO")])
		} else {
			tmp.state <- gsub(" ", "_", state)
		}

		dir.create(output.directory, showWarnings=FALSE)
		db <- dbConnect(SQLite(), dbname=file.path(output.directory, paste(tmp.state, "_Data_SQLITE.sqlite", sep="")))


	## Utility functions

		.year.increment <- function(year, increment) {
			sapply(increment, function(x) paste(as.numeric(unlist(strsplit(as.character(year), "_")))+x, collapse="_"))
		}

		rbind.all <- function(.list, ...){
			if(length(.list)==1) return(.list[[1]])
			Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
		}

		sqlite.create.table <- function(table.name, field.types, primary.key) {
			tmp.sql <- paste("CREATE TABLE ", table.name, " (", paste(field.types, collapse=", "), 
				", PRIMARY KEY (", paste(primary.key, collapse=", "), "))", sep="")
			return(tmp.sql)
		}

		"%w/o%" <- function(x, y) x[!x %in% y]

		convert.variables <- function(tmp.df) {
			if (is.factor(tmp.df$YEAR)) {
				tmp1 <- unlist(strsplit(as.character(tmp.df$YEAR), "_"))
				tmp.df$YEAR <- as.integer(tmp1[seq(length(tmp1)) %% 2 == 0])
			}
			if (is.factor(tmp.df$DISTRICT_NUMBER)) {
				tmp.df$DISTRICT_NUMBER <- as.character(tmp.df$DISTRICT_NUMBER)
			}
			if (is.factor(tmp.df$SCHOOL_NUMBER)) {
				tmp.df$SCHOOL_NUMBER <- as.character(tmp.df$SCHOOL_NUMBER)
			}
			tmp.df[sapply(tmp.df, is.nan)] <- NA
			return(tmp.df)
		}

		get.grade <- function(grade) {
			if (SGPstateData[[state]][["Assessment_Program_Information"]][["Test_Season"]]=="Fall") grade-1 else grade
		}

		get.year <- function(year) {
			if (SGPstateData[[state]][["Assessment_Program_Information"]][["Test_Season"]]=="Fall") {
				.year.increment(year, -1)				
			} else {
				return(year)
			}
		}

	## Create relevant variables

		if (is.null(years)) years <- unique(sgp_object@Data$YEAR) %w/o% NA
		if (is.null(content_areas)) content_areas <- unique(sgp_object@Data$CONTENT_AREA) %w/o% NA


	## Create tmp.school.and.district.by.year table

		setkeyv(sgp_object@Data, c("YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER"))
		tmp.school.and.district.by.year  <- as.data.frame(sapply(convert.variables(unique(sgp_object@Data)[, list(YEAR, DISTRICT_NUMBER, SCHOOL_NUMBER)]), unclass))


	###
	### Construct tables
	###

	### Table 1. DISTRICT

		field.types <- c( 
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__DISTRICT_ENROLLMENT_STATUS"]],
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")), unclass))
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"  ### TEMPORARY UNTIL NAMES ARE ALIGNED
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("DISTRICT", field.types, c("YEAR", "DISTRICT_NUMBER", "CONTENT_AREA")))
		dbWriteTable(db, "DISTRICT", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "DISTRICT.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	### Table 2. DISTRICT_GRADE

		field.types <- c( 
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"GRADE INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__GRADE__DISTRICT_ENROLLMENT_STATUS"]],
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(GRADE) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")), unclass))
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"  ### TEMPORARY UNTIL NAMES ARE ALIGNED
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("DISTRICT_GRADE", field.types, c("YEAR", "DISTRICT_NUMBER", "CONTENT_AREA", "GRADE")))
		dbWriteTable(db, "DISTRICT_GRADE", tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))], row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "DISTRICT_GRADE.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	### Table 3. DISTRICT_ETHNICITY

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"ETHNICITY INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER",
			"ENROLLMENT_PERCENTAGE REAL")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__DISTRICT_ENROLLMENT_STATUS__ETHNICITY"]],
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(ETHNICITY) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")), unclass))
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"  ### TEMPORARY UNTIL NAMES ARE ALIGNED
		tmp$ENROLLMENT_PERCENTAGE <- NA
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("DISTRICT_ETHNICITY", field.types, c("YEAR", "DISTRICT_NUMBER", "CONTENT_AREA", "ETHNICITY")))
		dbWriteTable(db, "DISTRICT_ETHNICITY", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "DISTRICT_ETHNICITY.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	### Table 4. DISTRICT_GRADE_ETHNICITY

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"GRADE INTEGER NOT NULL",
			"ETHNICITY INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__GRADE__DISTRICT_ENROLLMENT_STATUS__ETHNICITY"]],
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(GRADE) & !is.na(ETHNICITY) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")), unclass))
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"  ### TEMPORARY UNTIL NAMES ARE ALIGNED
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("DISTRICT_GRADE_ETHNICITY", field.types, c("YEAR", "DISTRICT_NUMBER", "CONTENT_AREA", "GRADE", "ETHNICITY")))
		dbWriteTable(db, "DISTRICT_GRADE_ETHNICITY", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "DISTRICT_GRADE_ETHNICITY.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	### Table 5. DISTRICT_STUDENTGROUP

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"STUDENTGROUP TEXT NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER",
			"ENROLLMENT_PERCENTAGE REAL")

		tmp.list <- list()
		for (i in other.student.groups %w/o% c("ETHNICITY", "GENDER")) {
			tmp.list[[i]] <- sgp_object@Summary[["DISTRICT_NUMBER"]][[paste("DISTRICT_NUMBER__CONTENT_AREA__YEAR__DISTRICT_ENROLLMENT_STATUS__", i, sep="")]]
		}

		for (i in seq_along(tmp.list)) {
			setnames(tmp.list[[i]], 5, "STUDENTGROUP")
		}

		tmp <- as.data.frame(convert.variables(subset(rbind.all(tmp.list), 
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(STUDENTGROUP) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")))
		tmp$CONTENT_AREA <- unclass(tmp$CONTENT_AREA)
		tmp$STUDENTGROUP <- unclass(tmp$STUDENTGROUP)
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"  ### TEMPORARY UNTIL NAMES ARE ALIGNED
		tmp$ENROLLMENT_PERCENTAGE <- NA
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("DISTRICT_STUDENTGROUP", field.types, c("YEAR", "DISTRICT_NUMBER", "CONTENT_AREA", "STUDENTGROUP")))
		dbWriteTable(db, "DISTRICT_STUDENTGROUP", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "DISTRICT_STUDENTGROUP.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	### Table 6. DISTRICT_GRADE_STUDENTGROUP

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"GRADE INTEGER NOT NULL",
			"STUDENTGROUP INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER")

		tmp.list <- list()
		for (i in other.student.groups %w/o% c("ETHNICITY", "GENDER")) {
			tmp.list[[i]] <- sgp_object@Summary[["DISTRICT_NUMBER"]][[paste("DISTRICT_NUMBER__CONTENT_AREA__YEAR__GRADE__DISTRICT_ENROLLMENT_STATUS__", i, sep="")]]
		}

		for (i in seq_along(tmp.list)) {
			setnames(tmp.list[[i]], 6, "STUDENTGROUP")
		}

		tmp <- as.data.frame(convert.variables(subset(rbind.all(tmp.list), 
			!is.na(DISTRICT_NUMBER) & YEAR %in% years & CONTENT_AREA %in% content_areas & !is.na(STUDENTGROUP) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")))
		tmp$CONTENT_AREA <- unclass(tmp$CONTENT_AREA)
		tmp$STUDENTGROUP <- unclass(tmp$STUDENTGROUP)
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"  ### TEMPORARY UNTIL NAMES ARE ALIGNED
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("DISTRICT_GRADE_STUDENTGROUP", field.types, c("YEAR", "DISTRICT_NUMBER", "CONTENT_AREA", "GRADE", "STUDENTGROUP")))
		dbWriteTable(db, "DISTRICT_GRADE_STUDENTGROUP", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "DISTRICT_GRADE_STUDENTGROUP.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	## Table 7. SCHOOL

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"SCHOOL_NUMBER INTEGER NOT NULL",
			"EMH_LEVEL TEXT NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__EMH_LEVEL__SCHOOL_ENROLLMENT_STATUS"]],
			!is.na(SCHOOL_NUMBER) & !is.na(EMH_LEVEL) & CONTENT_AREA %in% content_areas & YEAR %in% years & SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes")), unclass))
		tmp <- as.data.frame(merge(tmp, as.data.frame(tmp.school.and.district.by.year), all.x=TRUE)) 
	### Temporary stuff
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET"
		tmp$EMH_LEVEL <- as.character(factor(tmp$EMH_LEVEL, levels=1:3, labels=c("E", "H", "M")))
	###
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("SCHOOL", field.types, c("YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL", "CONTENT_AREA")))
		dbWriteTable(db, "SCHOOL", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "SCHOOL.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	## Table 8. SCHOOL_GRADE

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"SCHOOL_NUMBER INTEGER NOT NULL",
			"EMH_LEVEL TEXT NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"GRADE INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__EMH_LEVEL__GRADE__SCHOOL_ENROLLMENT_STATUS"]],
			!is.na(SCHOOL_NUMBER) & !is.na(EMH_LEVEL) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(GRADE) & SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes")), unclass))
		tmp <- data.frame(merge(tmp, as.data.frame(tmp.school.and.district.by.year), all.x=TRUE)) 
	### Temporary stuff
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET" 
		tmp$EMH_LEVEL <- as.character(factor(tmp$EMH_LEVEL, levels=1:3, labels=c("E", "H", "M")))
	###
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("SCHOOL_GRADE", field.types, c("YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL", "GRADE", "CONTENT_AREA")))
		dbWriteTable(db, "SCHOOL_GRADE", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "SCHOOL_GRADE.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	## Table 9. SCHOOL_ETHNICITY

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"SCHOOL_NUMBER INTEGER NOT NULL",
			"EMH_LEVEL TEXT NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"ETHNICITY INTEGER NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER",
			"ENROLLMENT_PERCENTAGE REAL")

		tmp <- as.data.frame(sapply(convert.variables(subset(sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__EMH_LEVEL__SCHOOL_ENROLLMENT_STATUS__ETHNICITY"]],
			!is.na(SCHOOL_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(ETHNICITY) & SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes")), unclass))
		tmp <- data.frame(merge(tmp, as.data.frame(tmp.school.and.district.by.year), all.x=TRUE)) 
	### Temporary stuff
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET" 
		tmp$EMH_LEVEL <- as.character(factor(tmp$EMH_LEVEL, levels=1:3, labels=c("E", "H", "M")))
		tmp$ENROLLMENT_PERCENTAGE <- NA
	###
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("SCHOOL_ETHNICITY", field.types, c("YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL", "CONTENT_AREA", "ETHNICITY")))
		dbWriteTable(db, "SCHOOL_ETHNICITY", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "SCHOOL_ETHNICITY.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	## Table 10. SCHOOL_STUDENTGROUP

		field.types <- c(
			"DISTRICT_NUMBER INTEGER NOT NULL",
			"SCHOOL_NUMBER INTEGER NOT NULL",
			"EMH_LEVEL TEXT NOT NULL",
			"CONTENT_AREA INTEGER NOT NULL",
			"YEAR INTEGER NOT NULL",
			"STUDENTGROUP TEXT NOT NULL",
			"MEDIAN_SGP REAL",
			"MEDIAN_SGP_TARGET REAL",
			"PERCENT_AT_ABOVE_TARGET REAL",
			"PERCENT_AT_ABOVE_PROFICIENT REAL",
			"MEDIAN_SGP_COUNT INTEGER",
			"PERCENT_AT_ABOVE_PROFICIENT_COUNT INTEGER",
			"ENROLLMENT_PERCENTAGE REAL")

		tmp.list <- list()
		for (i in other.student.groups %w/o% c("ETHNICITY", "GENDER")) {
			tmp.list[[i]] <- sgp_object@Summary[["SCHOOL_NUMBER"]][[paste("SCHOOL_NUMBER__CONTENT_AREA__YEAR__EMH_LEVEL__SCHOOL_ENROLLMENT_STATUS__", i, sep="")]]
		}

		for (i in seq_along(tmp.list)) {
			setnames(tmp.list[[i]], 6, "STUDENTGROUP")
		}

		tmp <- as.data.frame(convert.variables(subset(rbind.all(tmp.list), 
			!is.na(SCHOOL_NUMBER) & YEAR %in% years & CONTENT_AREA %in% content_areas & !is.na(STUDENTGROUP) & SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes")))
		tmp <- as.data.frame(merge(tmp, as.data.frame(tmp.school.and.district.by.year), all.x=TRUE)) 
		tmp$CONTENT_AREA <- unclass(tmp$CONTENT_AREA)
		tmp$STUDENTGROUP <- unclass(tmp$STUDENTGROUP)
		tmp$EMH_LEVEL <- unclass(tmp$EMH_LEVEL)
	### Temporary stuff
		names(tmp)[names(tmp)=="PERCENT_CATCHING_UP_KEEPING_UP"] <- "PERCENT_AT_ABOVE_TARGET" 
		tmp$EMH_LEVEL <- as.character(factor(tmp$EMH_LEVEL, levels=1:3, labels=c("E", "H", "M")))
		tmp$ENROLLMENT_PERCENTAGE <- NA
	###
		tmp <- tmp[, sapply(strsplit(field.types, " "), function(x) head(x,1))]

		dbGetQuery(db, sqlite.create.table("SCHOOL_STUDENTGROUP", field.types, 
			c("YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL", "CONTENT_AREA", "STUDENTGROUP")))
		dbWriteTable(db, "SCHOOL_STUDENTGROUP", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "SCHOOL_STUDENTGROUP.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")


	## Table 11. KEY_VALUE_LOOKUP (ADD CONTENT_AREA and YEAR)

		field.types <- c(
			"KEY_VALUE_ID INTEGER NOT NULL",
			"KEY_VALUE_KEY TEXT",
			"KEY_VALUE_CODE TEXT",
			"KEY_VALUE_TEXT TEXT")

		# CONTENT_AREA

		tmp <- as.data.frame(convert.variables(subset(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__DISTRICT_ENROLLMENT_STATUS"]],
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")))
		tmp.CONTENT_AREA <- data.frame(
			KEY_VALUE_KEY="CONTENT_AREA", 
			KEY_VALUE_CODE=sort(unique(as.integer(tmp$CONTENT_AREA))), 
			KEY_VALUE_TEXT=unlist(lapply(levels(tmp$CONTENT_AREA)[sort(unique(as.integer(tmp$CONTENT_AREA)))], capwords)))

		# YEAR

		tmp.YEAR <- data.frame(
			KEY_VALUE_KEY="YEAR", 
			KEY_VALUE_CODE=sort(unique(tmp$YEAR)), 
			KEY_VALUE_TEXT=paste(as.numeric(sapply(sort(unique(tmp$YEAR)), get.year))-1, "-", sapply(sort(unique(tmp$YEAR)), get.year), sep=""))

		# GRADE

		tmp <- subset(as.data.frame(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__GRADE__DISTRICT_ENROLLMENT_STATUS"]]),
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(GRADE) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")
		tmp.GRADE <- data.frame(
			KEY_VALUE_KEY="GRADE", 
			KEY_VALUE_CODE=sort(unique(as.integer(tmp$GRADE))), 
			KEY_VALUE_TEXT=paste("Grade", get.grade(sort(unique(as.integer(tmp$GRADE))))))

		# EMH_LEVEL

		tmp <- subset(sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__EMH_LEVEL__SCHOOL_ENROLLMENT_STATUS"]],
			!is.na(SCHOOL_NUMBER) & !is.na(EMH_LEVEL) & CONTENT_AREA %in% content_areas & YEAR %in% years & SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes")
		tmp.EMH <- data.frame(
			KEY_VALUE_KEY="EMH_LEVEL",
			KEY_VALUE_CODE=c("E", "H", "M"), ## TEMP fix until EMH_LEVEL is fixed up
#			KEY_VALUE_CODE=sort(unique(as.integer(tmp$EMH_LEVEL))),
			KEY_VALUE_TEXT= levels(tmp$EMH_LEVEL)[sort(unique(as.integer(tmp$EMH_LEVEL)))])

		# ETHNICITY

		tmp <- subset(as.data.frame(sgp_object@Summary[["DISTRICT_NUMBER"]][["DISTRICT_NUMBER__CONTENT_AREA__YEAR__DISTRICT_ENROLLMENT_STATUS__ETHNICITY"]]),
			!is.na(DISTRICT_NUMBER) & CONTENT_AREA %in% content_areas & YEAR %in% years & !is.na(ETHNICITY) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes")
		tmp.ETHNICITY <- data.frame(
			KEY_VALUE_KEY="ETHNICITY", 
			KEY_VALUE_CODE=sort(unique(as.integer(tmp$ETHNICITY))), 
			KEY_VALUE_TEXT=levels(tmp$ETHNICITY)[sort(unique(as.integer(tmp$ETHNICITY)))])

		# STUDENTGROUP

		tmp.list <- list()
		for (i in other.student.groups %w/o% c("GENDER", "ETHNICITY")) {
			tmp.list[[i]] <- sgp_object@Summary[["DISTRICT_NUMBER"]][[paste("DISTRICT_NUMBER__CONTENT_AREA__YEAR__DISTRICT_ENROLLMENT_STATUS__", i, sep="")]]
		}

		for (i in seq_along(tmp.list)) {
			setnames(tmp.list[[i]], 5, "STUDENTGROUP")
		}


		tmp <- as.data.frame(subset(rbind.all(tmp.list), !is.na(DISTRICT_NUMBER) & !is.na(STUDENTGROUP) & DISTRICT_ENROLLMENT_STATUS=="Enrolled District: Yes"))

		tmp.STUDENTGROUP <- data.frame(
			KEY_VALUE_KEY="STUDENT_GROUP", ### NOTE: Must have underscore. It's an older version of the table
			KEY_VALUE_CODE=sort(unique(as.integer(tmp$STUDENTGROUP))),
			KEY_VALUE_TEXT=levels(tmp$STUDENTGROUP)[sort(unique(as.integer(tmp$STUDENTGROUP)))])
#			KEY_VALUE_CODE=toupper(substr(unique(as.character(tmp$STUDENTGROUP)), 1, 50)), 
#			KEY_VALUE_TEXT=unique(as.character(tmp$STUDENTGROUP)))

		tmp <- rbind(tmp.CONTENT_AREA, tmp.YEAR, tmp.GRADE, tmp.EMH, tmp.ETHNICITY, tmp.STUDENTGROUP)
		tmp <- data.frame(KEY_VALUE_ID=1:dim(tmp)[1], tmp)

		dbGetQuery(db, sqlite.create.table("KEY_VALUE_LOOKUP", field.types, "KEY_VALUE_ID"))
		dbWriteTable(db, "KEY_VALUE_LOOKUP", tmp, row.names=FALSE, append=TRUE) 

		if (text.output) write.table(tmp, file=file.path(output.directory, "KEY_VALUE_LOOKUP.dat"), row.names=FALSE, na="", quote=FALSE, sep="|")

###
### Disconnect database
###

dbDisconnect(db)

} ### END sqliteSGP
