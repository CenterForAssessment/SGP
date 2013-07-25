`getTargetName` <- 
function(target.type="sgp.projections.lagged",
	target.level="CATCH_UP_KEEP_UP",
	target.years=3,
	target.label="SGP_TARGET") {

	if (target.type %in% c("sgp.projections", "sgp.projections.lagged") & target.level=="CATCH_UP_KEEP_UP") {
		tmp <- paste(target.label, target.years, "YEAR", sep="_")
	}

	if (target.type %in% c("sgp.projections", "sgp.projections.lagged") & target.level=="MOVE_UP_STAY_UP") {
		tmp <- paste(target.label, "MOVE_UP_STAY_UP", target.years, "YEAR", sep="_")
	}

	if (target.type %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline") & target.level=="CATCH_UP_KEEP_UP") {
		tmp <- paste(target.label, "BASELINE", target.years, "YEAR", sep="_")
	}

	if (target.type %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline") & target.level=="MOVE_UP_STAY_UP") {
		tmp <- paste(target.label, "BASELINE_MOVE_UP_STAY_UP", target.years, "YEAR", sep="_")
	}

	if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) return(paste(tmp, "CURRENT", sep="_")) else return(tmp)

} ### END getTargetName
