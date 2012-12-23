`getAchievementLevel` <- 
function(sgp_data,
	state=NULL,
	year=NULL,
	content_area=NULL,
	grade=NULL,
	achievement.level.name="ACHIEVEMENT_LEVEL",
	scale.score.name="SCALE_SCORE") {

	CONTENT_AREA <- YEAR <- GRADE <- NULL

	if (!achievement.level.name %in% names(sgp_data)) {
		sgp_data[[achievement.level.name]] <- 
			factor(1, levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])]),
			labels=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])], ordered=TRUE)
	}

	if (is.null(year)) year <- sort(unique(sgp_data[['YEAR']]))
	if (is.null(content_area)) content_area <- sort(unique(sgp_data[['CONTENT_AREA']][sgp_data[['YEAR']] %in% year]))
	if (is.null(grade)) grade <- sort(unique(sgp_data[['GRADE']][sgp_data[['YEAR']] %in% year & sgp_data[['CONTENT_AREA']] %in% content_area]))

	get.cutscore.label <- function(state, year, content_area) {
		tmp.cutscore.names <- names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])
		tmp.cutscore.years <- sapply(strsplit(tmp.cutscore.names[grep(content_area, tmp.cutscore.names)], "[.]"), function(x) x[2])
		if (any(!is.na(tmp.cutscore.years))) {
			if (year %in% tmp.cutscore.years) {
				return(paste(content_area, year, sep="."))
			} else {
				if (year==sort(c(year, tmp.cutscore.years))[1]) {
					return(content_area)
				} else {
					return(paste(content_area, sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1], sep="."))
				}
			}
		} else {
			return(content_area)
		}
	}

	getAchievementLevel_INTERNAL <- function(state, content_area, year, grade, scale_score) {
		factor(findInterval(scale_score, SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, year, content_area)]][[paste("GRADE_", grade, sep="")]])+1,
			levels=seq_along(SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])]),
			labels=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]][!is.na(SGPstateData[[state]][["Achievement"]][["Levels"]][["Proficient"]])], ordered=TRUE)
	}

	setkeyv(sgp_data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
		sgp_data[[achievement.level.name]][sgp_data[CJ("VALID_CASE", content_area, year, grade), which=TRUE, nomatch=0]] <- 
		sgp_data[CJ("VALID_CASE", content_area, year, grade), nomatch=0][, getAchievementLevel_INTERNAL(state, CONTENT_AREA, YEAR, GRADE, get(scale.score.name)), 
			by=list(CONTENT_AREA, YEAR, GRADE)][["V1"]]

	return(sgp_data)
} ### END getAchievementLevel Function
