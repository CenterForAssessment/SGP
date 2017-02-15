`rliCutscoreCreation` <-
function(cutscore.file.name,
		 score.type){


	###  Read in the cutscore long data file
	rli.cs.long <- fread(cutscore.file.name)
	setnames(rli.cs.long, c("MinRasch", "MaxRasch"), c("MinSS", "MaxSS"))
	if (!all(rli.cs.long$Subject %in% content_areas <- c("READING", "MATHEMATICS"))) stop("\tNOTE: 'Subject' variable in supplied cutscores must be 'READING' or 'MATHEMATICS'.")


	###  Reshape the long file into a wide file
	rli.cs <- reshape(rli.cs.long, timevar= 'ProficiencyLevel', idvar=c('TestCode', 'Subject', 'Grade'), direction='wide', drop=c("CountryCode", "RegionCode", "ProficiencyName", "MaxSS", "Linked", "ProficiencyFlag"))
	setnames(rli.cs, c('TestCode', "Subject", "Grade"), c('STATE', "CONTENT_AREA", "GRADE"))
	if (score.type=="RASCH") {
		rli.cs[,CONTENT_AREA:=paste(CONTENT_AREA, "RASCH", sep="_")]
		content_areas <- paste(content_areas, "RASCH", sep="_")
	}

	cutscore.information.list <- list()


	###  Run nested loop to create a text object that can be output as text to the console.
	###  This is then copied and pasted (and cleaned slightly) into the RLI_Cutscores.R file

	cutscore.list <- list()
	for (state in unique(rli.cs$STATE)) {

		for (ca in c("MATHEMATICS", "READING")) {
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
				if (fall.tf) cutscore.list[[cutscore.list.name]][[paste("GRADE_", g, ".1", sep="")]] <- sort(as.numeric(x[GRADE==g,c("MinSS.2", "MinSS.3", "MinSS.4", "MinSS.5"), with=FALSE]))
				if (fall.tf) cutscore.list[[cutscore.list.name]][[paste("GRADE_", g, ".2", sep="")]] <- sort(as.numeric(x[GRADE==g,c("MinSS.2", "MinSS.3", "MinSS.4", "MinSS.5"), with=FALSE]))
				if (spring.tf) cutscore.list[[cutscore.list.name]][[paste("GRADE_", g, ".3", sep="")]] <- sort(as.numeric(x[GRADE==g,c("MinSS.2", "MinSS.3", "MinSS.4", "MinSS.5"), with=FALSE]))
			}
		}
	}

	return(cutscore.list)
} ### END rliCutscoreCreation
