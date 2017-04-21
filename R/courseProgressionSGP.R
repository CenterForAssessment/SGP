`courseProgressionSGP` <-
function(
	sgp_object,
	lag.direction=c("FORWARD", "BACKWARD"),
	year) {

	### Define list to hold results
	CONTENT_AREA <- CONTENT_AREA_by_GRADE <- GRADE <- GRADE_CHAR <- ID <- PERCENTAGE_IN_GROUP <- VALID_CASE <- YEAR <- YEAR_INTEGER <- NULL
	course.progression.list <- list()


	### Utility functions

	lagged_content_area <- function(sgp_object, lag.value, lag.direction=lag.direction) {
		tmp <- sgp_object[SJ(ID, YEAR_INTEGER-as.integer(lag.value)), allow.cartesian=TRUE][,list(ID, YEAR_INTEGER, CONTENT_AREA_by_GRADE)]
		invisible(tmp[,YEAR_INTEGER:=YEAR_INTEGER+as.integer(lag.value)])
		setkey(tmp, ID, YEAR_INTEGER, CONTENT_AREA_by_GRADE)
		tmp <- unique(tmp, by=key(tmp))
	}


	### Loop over FORWARD and/or BACKWARD

	for (lag.direction in lag.direction) {

		### Create relevant data and variables

		if (is.SGP(sgp_object)) {
			sgp_object_subset <- sgp_object@Data[VALID_CASE=="VALID_CASE"][, c("ID", "YEAR", "CONTENT_AREA", "GRADE"), with=FALSE]
		} else {
			sgp_object_subset <- sgp_object[VALID_CASE=="VALID_CASE"][, c("ID", "YEAR", "CONTENT_AREA", "GRADE"), with=FALSE]
		}

		if (identical(lag.direction, "FORWARD")) tmp.years <- sort(unique(sgp_object_subset, by='YEAR')[['YEAR']], decreasing=TRUE)
		if (identical(lag.direction, "BACKWARD")) tmp.years <- sort(unique(sgp_object_subset, by='YEAR')[['YEAR']])

		invisible(sgp_object_subset[,GRADE_CHAR:=as.factor(GRADE)])
		levels(sgp_object_subset[["GRADE_CHAR"]]) <- sapply(lapply(strsplit(paste0("0", levels(sgp_object_subset[["GRADE_CHAR"]])), ""), tail, 2), paste, collapse="")
		if (any(levels(sgp_object_subset[["GRADE_CHAR"]])=="CT")) {
			levels(sgp_object_subset[["GRADE_CHAR"]])[which(levels(sgp_object_subset[["GRADE_CHAR"]])=="CT")] <- "EOCT"
		}
		invisible(sgp_object_subset[, CONTENT_AREA_by_GRADE:=paste(CONTENT_AREA, GRADE_CHAR, sep=".")])
		invisible(sgp_object_subset[,YEAR_INTEGER:=as.integer(as.factor(YEAR))])
		invisible(sgp_object_subset[,CONTENT_AREA:=NULL])
		invisible(sgp_object_subset[,GRADE_CHAR:=NULL])


		### Merge in previous years' content areas

		setkeyv(sgp_object_subset, c("ID", "YEAR_INTEGER"))
		for (i in 1:(length(tmp.years)-1)) {
			if (identical(lag.direction, "FORWARD")) i <- -i
			sgp_object_subset <- sgp_object_subset[lagged_content_area(sgp_object_subset, i), allow.cartesian=TRUE]
		}

		if (identical(lag.direction, "FORWARD")) tmp.label <- "NEXT"
		if (identical(lag.direction, "BACKWARD")) tmp.label <- "PRIOR"

		if (length(tmp.years) > 2) {
			setnames(sgp_object_subset,
				# c("CONTENT_AREA_by_GRADE", paste("CONTENT_AREA_by_GRADE", 1:(length(tmp.years)-1), sep=".")),
				c("CONTENT_AREA_by_GRADE", "i.CONTENT_AREA_by_GRADE", paste("i.CONTENT_AREA_by_GRADE", 1:(length(tmp.years)-2), sep=".")), # changes for data.table 1.9.4
				paste(paste0("CONTENT_AREA_by_GRADE_", tmp.label, "_YEAR"), 0:(length(tmp.years)-1), sep="."))
		} else {
			setnames(sgp_object_subset, c("CONTENT_AREA_by_GRADE", "i.CONTENT_AREA_by_GRADE"), c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.0", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.1"))
		}

		### Create course.progresion.list containing all data frames

		if (identical(lag.direction, "FORWARD")) course.progression.years <- sort(tail(tmp.years, -1), decreasing=TRUE)
		if (identical(lag.direction, "BACKWARD")) course.progression.years <- sort(tail(tmp.years, -1))
		setkey(sgp_object_subset, YEAR)

		for (years in seq_along(course.progression.years)) {

			tmp.course.progression.data <- sgp_object_subset[data.table(course.progression.years[years])][
				, c("ID", paste(paste0("CONTENT_AREA_by_GRADE_", tmp.label, "_YEAR"), 0:years, sep=".")), with=FALSE]
			setkeyv(tmp.course.progression.data, paste(paste0("CONTENT_AREA_by_GRADE_", tmp.label, "_YEAR"), 0:years, sep="."))

			sorted.levels.iter <- sort(unique(tmp.course.progression.data[[paste(paste0("CONTENT_AREA_by_GRADE_", tmp.label, "_YEAR"), 0, sep=".")]]))
			for (grades_by_content_areas in sorted.levels.iter) {
				tmp.data <- tmp.course.progression.data[data.table(grades_by_content_areas)]
				num.rows <- uniqueN(tmp.data[["ID"]])
				course.progression.list[[lag.direction]][[as.character(course.progression.years[years])]][[grades_by_content_areas]] <-
					tmp.data[,list(COUNT=.N, PERCENTAGE_IN_GROUP=round(100*.N/num.rows, digits=2)),
						by=key(tmp.data)][order(PERCENTAGE_IN_GROUP, decreasing=TRUE)]
			}
		}
	} ### END for (lag.direction in lag.direction)

	return(course.progression.list)

} ### END courseProgressionSGP
