`checkKnotsBoundaries` <-
function(
		sgp_data,
		state=NULL) {


		### Utility functions

		get.my.knots.boundaries.path <- function(content_area, year) {
			if (is.null(sgp.percentiles.equated)) {
				tmp.knots.boundaries.names <-
					names(Knots_Boundaries[[tmp.path.knots.boundaries]])[content_area==sapply(strsplit(names(Knots_Boundaries[[tmp.path.knots.boundaries]]), "[.]"), '[', 1L)]
				if (length(tmp.knots.boundaries.names)==0L) {
					return(paste0("[['", tmp.path.knots.boundaries, "']]"))
				} else {
					tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), '[', 2L)
					tmp.index <- sum(year >= tmp.knots.boundaries.years, na.rm=TRUE)
					return(paste0("[['", tmp.path.knots.boundaries, "']][['", paste(c(content_area, sort(tmp.knots.boundaries.years)[tmp.index]), collapse="."), "']]"))
				}
			} else {
				return(paste0("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", sgp.percentiles.equated[['Year']], "']]"))
			}
		}
		

		### Get state (if possible)

		if (is.null(state)) {
			tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_data))))
			state <- getStateAbbreviation(tmp.name, "prepareSGP")
		}


		### Get knots_boundaries_data

		if (is.SGP(sgp_data)) {
			knots_boundaries_data <- sgp_data@Data[VALID_CASE=="VALID_CASE", c("CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE")]
		} else {
			knots_boundaries_data <- sgp_data[VALID_CASE=="VALID_CASE", c("CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE")]
		}


		### Create max/min for knots_boundaries_data by CONTENT_AREA, YEAR, GRADE

		knots_boundaries_data_max_min <- knots_boundaries_data[,list(MIN=min(SCALE_SCORE, na.rm=TRUE), MAX=max(SCALE_SCORE, na.rm=TRUE)), keyby=c("CONTENT_AREA", "YEAR", "GRADE")]


		### Add loss/hoss by CONTENT_AREA, YEAR, and GRADE to knots_boundaries_data_max_min




		return(knots_boundaries_data_max_min)


} ## END checkKnotsBoundaries
