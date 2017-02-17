`rliCutscoreCreation` <-
function(rli.cs.long,
		 score.type="RASCH",
		 content_areas=c("MATHEMATICS", "READING")){

	STATE <- GRADE <- CONTENT_AREA <- ProficiencyFlag <- ProficiencyLevel <- TestCode <- Subject <- Grade <- NUM_LEVELS <- NUM_ABOVE_PROF <- NULL

	### Lists to populate

	cutscore.list <- list()
	cutscore.information.list <- list()
	if (score.type=="RASCH") tmp.scores <- c("MinRasch", "MaxRasch") else tmp.scores <- c("MinSS", "MaxSS")


	###  Read in the cutscore long data file

	if (!all(rli.cs.long$Subject %in% content_areas)) stop("\tNOTE: 'Subject' variable in supplied cutscores must be 'READING' or 'MATHEMATICS'.")


	###  Reshape the long file into a wide file

	rli.cs <- reshape(rli.cs.long, timevar= 'ProficiencyLevel', idvar=c('TestCode', 'Subject', 'Grade'), direction='wide', drop=c("CountryCode", "RegionCode", "ProficiencyName", tmp.scores[2], "Linked", "ProficiencyFlag"))
	setnames(rli.cs, c('TestCode', "Subject", "Grade"), c('STATE', "CONTENT_AREA", "GRADE"))
	if (score.type=="RASCH") {
		rli.cs[,CONTENT_AREA:=paste(CONTENT_AREA, "RASCH", sep="_")]
		content_areas <- paste(content_areas, "RASCH", sep="_")
	}


	### cutscore.information.list creation.

	cutscore.information.list[['Cutscore_States']] <- sort(unique(rli.cs[['STATE']]))

	tmp.info.data <- rli.cs.long[ProficiencyFlag >= 0, list(NUM_LEVELS=max(ProficiencyLevel), NUM_ABOVE_PROF=sum(ProficiencyFlag)), keyby=list(TestCode, Subject, Grade)][,head(.SD, 1), by=TestCode]
	cutscore.information.list[['State_Levels']] <- list()
	cutscore.information.list[['State_Levels']][['Two_Level_States']] <- list(States=tmp.info.data[NUM_LEVELS==2][['TestCode']],
																				Levels=c("Not Proficient", "Proficient"))
	cutscore.information.list[['State_Levels']][['Three_Level_States']] <- list(States=tmp.info.data[NUM_LEVELS==3][['TestCode']],
																				Levels=c("Not Proficient", "Not Proficient", "Proficient"))
	cutscore.information.list[['State_Levels']][['Four_Level_States']] <- list(States=tmp.info.data[NUM_LEVELS==4][['TestCode']],
																				Levels=c("Not Proficient", "Not Proficient", "Proficient", "Proficient"))
	cutscore.information.list[['State_Levels']][['Five_Level_States_A']] <- list(States=tmp.info.data[NUM_LEVELS==5 & NUM_ABOVE_PROF==2][['TestCode']],
																				Levels=c("Not Proficient", "Not Proficient", "Proficient", "Proficient", "Proficient"))
	cutscore.information.list[['State_Levels']][['Five_Level_States_B']] <- list(States=tmp.info.data[NUM_LEVELS==5 & NUM_ABOVE_PROF==1][['TestCode']],
																				Levels=c("Not Proficient", "Not Proficient", "Not Proficient", "Proficient", "Proficient"))

	# Make sure we have all state cutscore information updated in levels:

	if (length(tmp <- setdiff(cutscore.information.list$Cutscore_States, unlist(sapply(cutscore.information.list[['State_Levels']], '[[', 1), use.names=FALSE))) > 0) {
		message(paste("NOTE: Not all RLI Cutscore states (", paste(tmp, collapse=", "), ") included in 2 - 5 Level list!", sep=""))
	}
	if (length(tmp <- setdiff(unlist(sapply(cutscore.information.list[['State_Levels']], '[[', 1), use.names=FALSE), cutscore.information.list$Cutscore_States)) > 0) {
		message(paste("NOTE: Some states included in 2 - 5 Level list (", paste(tmp, collapse=", "), ") that do not have Cutscores!", sep=""))
	}


	###  cutscore.list creation.

	for (state in unique(rli.cs$STATE)) {

		for (ca in content_areas) {
			x <- rli.cs[STATE==state & CONTENT_AREA==ca]
			if (dim(x)[1] == 0) next
			fall.tf <- spring.tf <- TRUE
			if (length(grep("Fall", state)) > 0) {
				tmp.state <- gsub(" Fall", "", state); spring.tf <- FALSE
			} else tmp.state <- state
			if (length(grep("Spring", state)) > 0) {
				fall.tf <- FALSE # Leave state name with "Spring" in it to help find it in the list text later
			}

			cutscore.list.name <- paste(ca, tmp.state, sep=".")
			cutscore.list[[cutscore.list.name]] <- list()

			for (g in x$GRADE) {
				if (fall.tf) cutscore.list[[cutscore.list.name]][[paste("GRADE_", g, ".1", sep="")]] <- sort(as.numeric(x[GRADE==g, paste(tmp.scores[1], 2:5, sep="."), with=FALSE]))
				if (fall.tf) cutscore.list[[cutscore.list.name]][[paste("GRADE_", g, ".2", sep="")]] <- sort(as.numeric(x[GRADE==g, paste(tmp.scores[1], 2:5, sep="."), with=FALSE]))
				if (spring.tf) cutscore.list[[cutscore.list.name]][[paste("GRADE_", g, ".3", sep="")]] <- sort(as.numeric(x[GRADE==g, paste(tmp.scores[1], 2:5, sep="."), with=FALSE]))
			}
		}
	}

	return(list(Cutscores=cutscore.list, Cutscore_Information=cutscore.information.list))
} ### END rliCutscoreCreation
