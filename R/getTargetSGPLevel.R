`getTargetSGPLevel` <- 
function(state,
	state.iter=NULL,
	target.level){
		if (!is.null(SGPstateData[[state]][['Achievement']][['Cutscore_Information']])) {
			tmp.state.level <- which(sapply(lapply(SGPstateData[["RLI"]][["Achievement"]][["Cutscore_Information"]][['State_Levels']], '[[', 1), function(x) state.iter %in% x[[1]]))
			if (target.level=="CATCH_UP_KEEP_UP") {
				level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']][[tmp.state.level]][['Levels']]=="Proficient")-1
			}
			if (target.level=="MOVE_UP_STAY_UP") {
				if (length(which(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']][[tmp.state.level]][['Levels']]=="Proficient")) <= 1) {
					stop(paste("\tNOTE: MOVE_UP_STAY_UP Targets cannot be calculated because no achievement levels above PROFICIENT exist in ", state, "/", state.iter, ".", sep=""))  
				} else {
					level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Cutscore_Information"]][['State_Levels']][[tmp.state.level]][['Levels']]=="Proficient")
				}
			}
		} else {
			if (target.level=="CATCH_UP_KEEP_UP") level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1
			if (target.level=="MOVE_UP_STAY_UP") {
				if (length(which(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")) <= 1) {
					stop(paste("\tNOTE: MOVE_UP_STAY_UP Targets cannot be calculated because no achievement levels above PROFICIENT exist in ", state, ".", sep=""))  
				} else {
					level.to.get <- which.max(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")
				}
			}
		}
		return(level.to.get) 
} ### END getTargetSGPLevel
