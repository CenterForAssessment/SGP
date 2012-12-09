`getMaxOrderForProgression` <- 
function(year,
	content_area,
	state) {

		if (is.null(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]])) {
			return(SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]]) ## Returns NULL if it doesn't exist
		} else {
			tmp <- as.numeric(tail(unlist(strsplit(as.character(year), "_")), 1)) - as.numeric(tail(unlist(strsplit(as.character(SGPstateData[[state]][["Assessment_Program_Information"]][["Scale_Change"]][[content_area]]), "_")), 1))
			if (tmp < 0) return(SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]])
			if (tmp > 0) return(min(c(as.numeric(tmp), SGPstateData[[state]][["SGP_Configuration"]][["max.order.for.projection"]])))
			if (tmp==0) message(paste("\tNOTE: Based upon state scale changes in ", year, ". student growth projections are not possible. No student growth projections will be generated", sep=""))
		}
} ### getMaxOrderForProgression
