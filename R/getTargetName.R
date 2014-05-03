`getTargetName` <- 
function(target.type="sgp.projections.lagged",
	target.level="CATCH_UP_KEEP_UP",
	target.years=3,
	target.label="SGP_TARGET",
	sgp.projections.projection.unit.label="YEAR") {

	if (target.type %in% c("sgp.projections", "sgp.projections.lagged") & target.level=="CATCH_UP_KEEP_UP") {
		tmp <- paste(target.label, target.years, sgp.projections.projection.unit.label, sep="_")
	}

	if (target.type %in% c("sgp.projections", "sgp.projections.lagged") & target.level=="MOVE_UP_STAY_UP") {
		tmp <- paste(target.label, "MOVE_UP_STAY_UP", target.years, sgp.projections.projection.unit.label, sep="_")
	}

	if (target.type %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline") & target.level=="CATCH_UP_KEEP_UP") {
		tmp <- paste(target.label, "BASELINE", target.years, sgp.projections.projection.unit.label, sep="_")
	}

	if (target.type %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline") & target.level=="MOVE_UP_STAY_UP") {
		tmp <- paste(target.label, "BASELINE_MOVE_UP_STAY_UP", target.years, sgp.projections.projection.unit.label, sep="_")
	}

	if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) return(paste(tmp, "CURRENT", sep="_")) else return(tmp)

} ### END getTargetName
