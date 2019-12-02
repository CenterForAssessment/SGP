`checkKnotsBoundaries` <-
function(
		sgp_data,
		state=NULL) {

		VALID_CASE <- CONTENT_AREA <- YEAR <- GRADE <- SCALE_SCORE <- OBSERVED_MIN <- OBSERVED_MAX <- LOSS <- HOSS <- BOUNDARY_LOW <- BOUNDARY_HIGH <- OBSERVED_WITHIN_LOSS_HOSS_RANGE <- OBSERVED_WITHIN_BOUNDARY_RANGE <- NULL


		### Utility functions

		get.my.knots.loss.hoss.boundary.values <- function(content_area, year, grade, value) {
			tmp.knots.boundaries.names <- names(Knots_Boundaries)[content_area==sapply(strsplit(names(Knots_Boundaries), "[.]"), '[', 1L)]
			if (length(tmp.knots.boundaries.names)==0L) {
				stop(paste("No Knots/Boundaries found for state ", state, "and CONTENT_AREA", content_area))
			} else {
				tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), '[', 2L)
				tmp.index <- sum(year >= tmp.knots.boundaries.years, na.rm=TRUE)
				return(Knots_Boundaries[[paste(c(content_area, sort(tmp.knots.boundaries.years)[tmp.index]), collapse=".")]][[paste0(value, "_", grade)]])
			}
		}

		### Get state (if possible)

		if (is.null(state)) {
			tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_data))))
			state <- getStateAbbreviation(tmp.name, "prepareSGP")
		}

		Knots_Boundaries <- SGP::SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]]


		### Get knots_boundaries_data

		if (is.SGP(sgp_data)) {
			knots_boundaries_data <- sgp_data@Data[VALID_CASE=="VALID_CASE", c("CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE")]
		} else {
			knots_boundaries_data <- sgp_data[VALID_CASE=="VALID_CASE", c("CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE")]
		}


		### Create max/min for knots_boundaries_data by CONTENT_AREA, YEAR, GRADE

		knots_boundaries_data_min_max <- knots_boundaries_data[,list(OBSERVED_MIN=min(SCALE_SCORE, na.rm=TRUE), OBSERVED_MAX=max(SCALE_SCORE, na.rm=TRUE)), keyby=c("CONTENT_AREA", "YEAR", "GRADE")]


		### Add loss/hoss by CONTENT_AREA, YEAR, and GRADE to knots_boundaries_data_min_max

		knots_boundaries_data_min_max[,LOSS:=get.my.knots.loss.hoss.boundary.values(CONTENT_AREA, YEAR, GRADE, "loss.hoss")[1], by=c("CONTENT_AREA", "YEAR", "GRADE")]
		knots_boundaries_data_min_max[,HOSS:=get.my.knots.loss.hoss.boundary.values(CONTENT_AREA, YEAR, GRADE, "loss.hoss")[2], by=c("CONTENT_AREA", "YEAR", "GRADE")]
		knots_boundaries_data_min_max[,BOUNDARY_LOW:=get.my.knots.loss.hoss.boundary.values(CONTENT_AREA, YEAR, GRADE, "boundaries")[1], by=c("CONTENT_AREA", "YEAR", "GRADE")]
		knots_boundaries_data_min_max[,BOUNDARY_HIGH:=get.my.knots.loss.hoss.boundary.values(CONTENT_AREA, YEAR, GRADE, "boundaries")[2], by=c("CONTENT_AREA", "YEAR", "GRADE")]


		### Check whether observed min/max lie within loss/hoss range

		knots_boundaries_data_min_max[,OBSERVED_WITHIN_LOSS_HOSS_RANGE:=TRUE]
		knots_boundaries_data_min_max[,OBSERVED_WITHIN_BOUNDARY_RANGE:=TRUE]
		knots_boundaries_data_min_max[OBSERVED_MIN < LOSS, OBSERVED_WITHIN_LOSS_HOSS_RANGE:=FALSE]
		knots_boundaries_data_min_max[OBSERVED_MAX > HOSS, OBSERVED_WITHIN_LOSS_HOSS_RANGE:=FALSE]
		knots_boundaries_data_min_max[OBSERVED_MIN < BOUNDARY_LOW, OBSERVED_WITHIN_BOUNDARY_RANGE:=FALSE]
		knots_boundaries_data_min_max[OBSERVED_MAX > BOUNDARY_HIGH, OBSERVED_WITHIN_BOUNDARY_RANGE:=FALSE]

		if (!all(knots_boundaries_data_min_max[['OBSERVED_WITHIN_LOSS_HOSS_RANGE']])) {
				messageSGP(paste0("\tNOTE: Some values of observed data reside outside the LOSS/HOSS values in the SGPstateData meta-data for ", state, ". Examine saved output knots_boundaries_data_min_max for details."))
				save(knots_boundaries_data_min_max, file="knots_boundaries_data_min_max.Rdata")
		} else {
				messageSGP(paste0("\tNOTE: All values of observed data reside within the LOSS/HOSS values in the SGPstateData meta-data for ", state, "."))
		}

		if (!all(knots_boundaries_data_min_max[['OBSERVED_WITHIN_BOUNDARY_RANGE']])) {
				messageSGP(paste0("\tNOTE: Some values of observed data reside outside the BOUNDARY values in the SGPstateData meta-data for ", state, ". Examine saved output knots_boundaries_data_min_max for details."))
				save(knots_boundaries_data_min_max, file="knots_boundaries_data_min_max.Rdata")
		} else {
				messageSGP(paste0("\tNOTE: All values of observed data reside within the BOUNDARY values in the SGPstateData meta-data for ", state, "."))
		}
} ## END checkKnotsBoundaries
