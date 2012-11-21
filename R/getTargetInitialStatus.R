`getTargetInitialStatus` <- 
function(achievement_level,
	state,
	status.type="CATCH_UP_KEEP_UP") {

		if (!all(levels(achievement_level) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]])) {
			levels(achievement_level)[!levels(achievement_level) %in% SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]]] <- NA
		}

		if (status.type=="CATCH_UP_KEEP_UP") {
			levels(achievement_level) <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]] 
			levels(achievement_level) <- c("Catching Up", "Keeping Up")
			return(factor(achievement_level, ordered=FALSE))
		}

		if (status.type=="MOVE_UP_STAY_UP") {
			achievement.level.for.start <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")]
			achievement_level[!achievement_level %in% achievement.level.for.start] <- NA 
			achievement_level <- unclass(factor(achievement_level))
			achievement_level[achievement_level > 2] <- 2
			return(factor(achievement_level, levels=1:2, labels=c("Moving Up", "Staying Up"), ordered=FALSE))
		}
} ### END getTargetInitialStatus
