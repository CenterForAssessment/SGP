`getTargetInitialStatus` <- 
function(achievement_level,
	state,
	state.iter=NULL,
	status.type="CATCH_UP_KEEP_UP") {

		if (!is.null(SGPstateData[[state]][['Achievement']][['Cutscore_Information']])) {
			tmp.state.level <- which(sapply(lapply(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']], '[[', 1), function(x) state.iter %in% x))
			levels.that.are.proficient <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][["State_Levels"]][[tmp.state.level]][['Levels']]=="Proficient")]
			levels.that.are.not.proficient <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][["State_Levels"]][[tmp.state.level]][['Levels']]=="Not Proficient")]
		} else {
			levels.that.are.proficient <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")]
			levels.that.are.not.proficient <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Not Proficient")]
		}

		if (status.type=="CATCH_UP_KEEP_UP") {
			achievement_level <- as.character(achievement_level)
			achievement_level[achievement_level %in% levels.that.are.proficient] <- 2
			achievement_level[achievement_level %in% levels.that.are.not.proficient] <- 1
			return(factor(achievement_level, levels=1:2, labels=c("Catching Up", "Keeping Up"), ordered=FALSE))
		}

		if (status.type=="MOVE_UP_STAY_UP") {
			achievement_level[achievement_level %in% levels.that.are.not.proficient] <- NA
			achievement_level <- unclass(factor(achievement_level))
			achievement_level[achievement_level > 2] <- 2
			return(factor(achievement_level, levels=1:2, labels=c("Moving Up", "Staying Up"), ordered=FALSE))
		}
} ### END getTargetInitialStatus
