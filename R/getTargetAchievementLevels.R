`getTargetAchievementLevels` <- 
function(state,
	status.type="CATCH_UP_KEEP_UP") {

	SGPstateData <- SGPstateData

	tmp.list <- list()

	if (status.type=="CATCH_UP_KEEP_UP") {
		if (!is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]])) {
			tmp.index <- grep("Achievement_Levels", names(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]]))
			levels.that.are.proficient <- 
				sort(as.vector(sapply(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][tmp.index], function(x) x[['Labels']]))[
				as.vector(sapply(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][tmp.index], function(x) x[['Proficient']]))=="Proficient"])
			levels.that.are.not.proficient <- 
				sort(as.vector(sapply(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][tmp.index], function(x) x[['Labels']]))[
				as.vector(sapply(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][tmp.index], function(x) x[['Proficient']]))=="Not Proficient"])
		} else {
			levels.that.are.proficient <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")]
			levels.that.are.not.proficient <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Not Proficient")]
		}
		return(list(NO=as.character(levels.that.are.not.proficient), YES=as.character(levels.that.are.proficient)))
	}

	if (status.type=="MOVE_UP_STAY_UP") {
		if (!is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]])) {
			levels.that.are.advanced <- levels.that.are.not.advanced <- list()
			for (i in grep("Achievement_Levels", names(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]]), value=TRUE)) {
				levels.that.are.advanced[[i]] <- SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[i]][['Labels']][
					tail(which(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[i]][["Proficient"]]=="Proficient"), -1)]
				levels.that.are.not.advanced[[i]] <- SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[i]][['Labels']][
					c(which(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[i]][["Proficient"]]=="Not Proficient"),
					head(which(SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[i]][["Proficient"]]=="Proficient"), 1))]
			}
		} else {
			levels.that.are.advanced <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				tail(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient"), -1)]
			levels.that.are.not.advanced <- SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][
				c(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Not Proficient"),
				head(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient"), 1))]
		}
		return(list(NO=as.character(unlist(levels.that.are.not.advanced)), YES=as.character(unlist(levels.that.are.advanced))))
	}
} ### END getTargetAchievementLevels
