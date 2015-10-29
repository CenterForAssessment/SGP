`linkagePlot` <-
function(linkage.data,
        conversion.type,
        equating.method,
        year.for.equate,
        state) {

    GRADE <- CONTENT_AREA <- YEAR <- NULL

    get.cutscore.label <- function(state, year, content_area) {
		tmp.cutscore.names <- names(SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]])
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

    tmp.years <- sort(unique(linkage.data[['YEAR']]))
    linkage.var.name <- grep('SCALE_SCORE_EQUATED', names(linkage.data), value=TRUE)
    if (conversion.type=="OLD_TO_NEW") {
        x.axis.year <- rev(tmp.years)[2]
        y.axis.year <- rev(tmp.years)[1]
        x.axis.cut.level <- which.max(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Achievement_Levels"]][["Proficient"]]=="Proficient")-1
        y.axis.cut.level <- which.max(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Achievement_Levels", year.for.equate, sep=".")]][["Proficient"]]=="Proficient")-1
        x.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Assessment_Abbreviation"]]
        y.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Assessment_Abbreviation", year.for.equate, sep=".")]]
        x.axis.label <- paste(x.abb, "Scale Score")
        y.axis.label <- paste(y.abb, "Scale Score")
    } else {
        x.axis.year <- rev(tmp.years)[1]
        y.axis.year <- rev(tmp.years)[2]
        x.axis.cut.level <- which.max(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Achievement_Levels", year.for.equate, sep=".")]][["Proficient"]]=="Proficient")-1
        y.axis.cut.level <- which.max(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Achievement_Levels"]][["Proficient"]]=="Proficient")-1
        x.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Assessment_Abbreviation", year.for.equate, sep=".")]]
        y.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Assessment_Abbreviation"]]
        x.axis.label <- paste(x.abb, "Scale Score")
        y.axis.label <- paste(y.abb, "Scale Score")
    }
    linkage.data <- linkage.data[YEAR==x.axis.year & !is.na(get(linkage.var.name))]
    for (grade.iter in unique(linkage.data[['GRADE']])) {
        for (content_area.iter in unique(linkage.data[['CONTENT_AREA']])) {
            x.axis.cut <- SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, x.axis.year, content_area.iter)]][[paste("GRADE", grade.iter, sep="_")]][x.axis.cut.level]
            y.axis.cut <- SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, y.axis.year, content_area.iter)]][[paste("GRADE", grade.iter, sep="_")]][y.axis.cut.level]
            pdf(file=paste("Data/Linkages_", year.for.equate, "/Figures/", toupper(equating.method), "_", conversion.type, "_", content_area.iter, "_GRADE_", grade.iter, ".pdf", sep=""), width=8, height=8)
                plot(linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][['SCALE_SCORE']],
                    linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][[linkage.var.name]],
                    type="p", xlab=x.axis.label, ylab=y.axis.label,
                    main=paste(x.abb, "to", y.abb, equating.method, "concordance:", content_area.iter, "Grade", grade.iter))
                    abline(h=y.axis.cut, lty=2, col="grey50")
                    abline(v=x.axis.cut, lty=2, col="grey50")
            dev.off()
        }
    }
} ### END linkagePlot function
