`getTargetSGP` <- 
function(sgp_object,
	content_areas, 
	state, 
	years, 
	target.type, 
	target.level,
	max.sgp.target.years.forward=3,
	subset.ids=NULL,
	return.lagged.status=TRUE) {

	TARGET_STATUS_INITIAL <- VALID_CASE <- ID <- CONTENT_AREA <- YEAR <- NULL


	### Define variables

	if (target.level=="CATCH_UP_KEEP_UP") level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1
	if (target.level=="MOVE_UP_STAY_UP") {
		if (length(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")) <= 1) {
			stop(paste("\tNOTE: MOVE_UP_STAY_UP Targets cannot be calculated because no levels above PROFICIENT exist in ", state, ".", sep=""))  
		} else {
			level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")
		}
	}

	catch_keep_move_functions <- c(min, max)

	max.sgp.target.years.forward.label <- max.sgp.target.years.forward
	if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) max.sgp.target.years.forward <- max.sgp.target.years.forward+1


	#### Utility functions

	"%w/o%" <- function(x, y) x[!x %in% y]


	### Calculate Targets

	tmp.names <- getPercentileTableNames(sgp_object, content_areas, state, years, target.type)
	tmp.list <- list()
	for (i in tmp.names) {
		cols.to.get.names <- names(sgp_object@SGP[["SGProjections"]][[i]])[
			grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))]
		num.years.to.get <- min(max.sgp.target.years.forward, length(cols.to.get.names))
		cols.to.get.names <- cols.to.get.names[as.integer(sapply(sapply(cols.to.get.names, strsplit, "_"), tail, 1) ) <= num.years.to.get]
		if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) cols.to.get.names <- c("ACHIEVEMENT_LEVEL_PRIOR", cols.to.get.names)
		cols.to.get <- sapply(cols.to.get.names, function(x) which(x==names(sgp_object@SGP[["SGProjections"]][[i]])))
		tmp.list[[i]] <- data.table(
			CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
			YEAR=unlist(strsplit(i, "[.]"))[2],
			sgp_object@SGP[["SGProjections"]][[i]][,c(1,cols.to.get)])
	}

	if (!is.null(subset.ids)) {
		tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE", key=c("ID"))[subset.ids, nomatch=0]
	} else {
		tmp_object_1 <- data.table(rbind.fill(tmp.list), VALID_CASE="VALID_CASE")
	}
	setkeyv(tmp_object_1, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))

	if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) {
		tmp_object_1 <- data.table(sgp_object@Data[,c(key(tmp_object_1), "ACHIEVEMENT_LEVEL"), with=FALSE], key=key(tmp_object_1))[tmp_object_1]
	}
	tmp_object_1[, paste(target.level, "STATUS_INITIAL", sep="_") := getTargetInitialStatus(tmp_object_1[[grep("ACHIEVEMENT", names(tmp_object_1), value=TRUE)]], state, target.level), with=FALSE]
	tmp_object_1 <- tmp_object_1[!is.na(get(paste(target.level, "STATUS_INITIAL", sep="_")))]

	## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

	jExpression <- parse(text=paste("{catch_keep_move_functions[[unclass(", target.level, "_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1)) %w/o% 
		grep("ACHIEVEMENT", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)}", sep=""))
	tmp_object_2 <- tmp_object_1[, eval(jExpression), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]

	if (target.type %in% c("sgp.projections.baseline", "sgp.projections.lagged.baseline")) baseline.label <- "_BASELINE" else baseline.label <- NULL
	if (target.type %in% c("sgp.projections", "sgp.projections.baseline")) projection.label <- "_CURRENT_YEAR_PROJECTION" else projection.label <- NULL
	if (target.level=="MOVE_UP_STAY_UP") target.level.label <- "_MOVE_UP_STAY_UP" else target.level.label <- NULL

	setnames(tmp_object_2, "V1", paste("SGP_TARGET", baseline.label, projection.label, target.level.label, "_",  max.sgp.target.years.forward.label, "_YEAR", sep=""))

	if (target.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline") & return.lagged.status) {
		tmp_object_2[,c("ACHIEVEMENT_LEVEL_PRIOR", grep("STATUS_INITIAL", names(tmp_object_1), value=TRUE)) := 
			list(tmp_object_1[["ACHIEVEMENT_LEVEL_PRIOR"]], tmp_object_1[[grep("STATUS_INITIAL", names(tmp_object_1), value=TRUE)]]), with=FALSE]
	}

	setkeyv(tmp_object_2, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))
	return(tmp_object_2)

} ### END getTargetSGP
