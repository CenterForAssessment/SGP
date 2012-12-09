`getTargetName` <- 
function(target.type="sgp.projections.lagged",
	target.level="CATCH_UP_KEEP_UP",
	target.years=3,
	target.label="SGP_TARGET") {

	if (target.type=="sgp.projections" & target.level=="CATCH_UP_KEEP_UP") {
		return(paste(target.label, "CURRENT_YEAR_PROJECTION", target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections" & target.level=="MOVE_UP_STAY_UP") {
		return(paste(target.label, "CURRENT_YEAR_PROJECTION_MOVE_UP_STAY_UP", target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections.baseline" & target.level=="CATCH_UP_KEEP_UP") {
		return(paste(target.label, "BASELINE_CURRENT_YEAR_PROJECTION", target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections.baseline" & target.level=="MOVE_UP_STAY_UP") {
		return(paste(target.label, "BASELINE_CURRENT_YEAR_PROJECTION_MOVE_UP_STAY_UP", target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections.lagged" & target.level=="CATCH_UP_KEEP_UP") {
		return(paste(target.label, target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections.lagged" & target.level=="MOVE_UP_STAY_UP") {
		return(paste(target.label, "MOVE_UP_STAY_UP", target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections.lagged.baseline" & target.level=="CATCH_UP_KEEP_UP") {
		return(paste(target.label, "BASELINE", target.years, "YEAR", sep="_"))
	}
	if (target.type=="sgp.projections.lagged.baseline" & target.level=="MOVE_UP_STAY_UP") {
		return(paste(target.label, "BASELINE_MOVE_UP_STAY_UP", target.years, "YEAR", sep="_"))
	}
} ### END getTargetName
