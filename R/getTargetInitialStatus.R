`getTargetInitialStatus` <- 
function(achievement_level,
	state,
	state.iter=NULL,
	status.type="CATCH_UP_KEEP_UP") {

		if (!is.null(SGPstateData[[state]][['Achievement']][['Cutscore_Information']])) {
			tmp.state.level <- which(sapply(lapply(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']], '[[', 1), function(x) state.iter %in% x[[1]]))
			if (status.type=="CATCH_UP_KEEP_UP") {
				cut.level <- which.max(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']][[tmp.state.level]][['Levels']]=="Proficient")
				tmp.labels <- c("Catching Up", "Keeping Up")
			}
			if (status.type=="MOVE_UP_STAY_UP") {
                                if (length(which(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']][[tmp.state.level]][['Levels']]=="Proficient")) <= 1) {
                                        stop(paste("\tNOTE: MOVE_UP_STAY_UP Targets cannot be calculated because no achievement levels above PROFICIENT exist in ", state, "/", state.iter, ".", sep=""))
                                } else {
                                        cut.level <- which.max(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']][[tmp.state.level]][['Levels']]=="Proficient")+1
                                }
				tmp.labels <- c("Moving Up", "Staying Up")
			}
			tmp.levels <- rep("Proficient", length(levels(achievement_level)))
			tmp.levels[as.numeric(sapply(strsplit(levels(achievement_level), " "), '[', 2)) <= cut.level] <- "Not Proficient"
			levels(achievement_level) <- tmp.levels
			levels(achievement_level) <- tmp.labels

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
