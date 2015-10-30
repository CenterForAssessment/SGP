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

    myTicks <- function(my.range) {
        tmp.floor <- floor(log10(diff(c(my.range)))-0.3)
        tmp.seq <- my.range/10^tmp.floor
        tmp.seq[1] <- floor(tmp.seq[1])*10^tmp.floor; tmp.seq[2] <- ceiling(tmp.seq[2])*10^tmp.floor
        tmp.seq <- c(my.range[1], head(tail(seq(tmp.seq[1], tmp.seq[2], by=10^tmp.floor), -1), -1), my.range[2])
        return(tmp.seq)
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
        x.axis.label <- paste(x.axis.year, "Scale Score")
        y.axis.label <- paste(y.axis.year, "Scale Score")
    } else {
        x.axis.year <- rev(tmp.years)[1]
        y.axis.year <- rev(tmp.years)[2]
        x.axis.cut.level <- which.max(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Achievement_Levels", year.for.equate, sep=".")]][["Proficient"]]=="Proficient")-1
        y.axis.cut.level <- which.max(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Achievement_Levels"]][["Proficient"]]=="Proficient")-1
        x.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Assessment_Abbreviation", year.for.equate, sep=".")]]
        y.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Assessment_Abbreviation"]]
        x.axis.label <- paste(x.axis.year, "Scale Score")
        y.axis.label <- paste(y.axis.year, "Scale Score")
    }

    for (grade.iter in unique(linkage.data[['GRADE']])) {
        for (content_area.iter in unique(linkage.data[['CONTENT_AREA']])) {
            linkage.data <- linkage.data[YEAR==x.axis.year & !is.na(get(linkage.var.name)) & GRADE==grade.iter & CONTENT_AREA==content_area.iter]

            plot(linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][['SCALE_SCORE']], linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][[linkage.var.name]], type="n")
            x.axis.cut <- SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, x.axis.year, content_area.iter)]][[paste("GRADE", grade.iter, sep="_")]][x.axis.cut.level]
            x.axis.ticks <- myTicks(range(linkage.data[['SCALE_SCORE']]))
            x.axis.cut.text <- paste("grid.lines(x=unit(", x.axis.cut, ", 'native'), y=c(", x.axis.ticks[1], ",", rev(x.axis.ticks)[1], "), gp=gpar(col='grey40', lwd=1.25, lty=2, alpha=0.5))")
            y.axis.cut <- SGP::SGPstateData[[state]][["Achievement"]][["Cutscores"]][[get.cutscore.label(state, y.axis.year, content_area.iter)]][[paste("GRADE", grade.iter, sep="_")]][y.axis.cut.level]
            y.axis.ticks <- myTicks(range(linkage.data[[linkage.var.name]]))
            y.axis.cut.text <- paste("grid.lines(x=unit(", y.axis.cut, ", 'native'), y=c(", y.axis.ticks[1], ",", rev(y.axis.ticks)[1], "), gp=gpar(col='grey40', lwd=1.25, lty=2, alpha=0.5))")

            bubblePlot(
    			bubble_plot_data.X=linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][['SCALE_SCORE']],
    			bubble_plot_data.Y=linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][[linkage.var.name]],
    			bubble_plot_data.SUBSET=NULL,
    			bubble_plot_data.INDICATE=NULL,
    			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
    			bubble_plot_data.SIZE=rep(50, length(linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][['SCALE_SCORE']])),
    			bubble_plot_data.LEVELS=NULL,
    			bubble_plot_data.BUBBLE_TIPS_LINES=NULL,
    			bubble_plot_labels.X=c(x.abb, x.axis.label),
    			bubble_plot_labels.Y=c(y.abb, y.axis.label),
    			bubble_plot_labels.SIZE=NULL,
    			bubble_plot_labels.LEVELS=NULL,
    			bubble_plot_labels.BUBBLE_TIPS_LINES=NULL,
    			bubble_plot_labels.BUBBLE_TITLES=NULL,
    			bubble_plot_titles.MAIN=paste(capwords(equating.method), "Linkage"),
    			bubble_plot_titles.SUB1=paste(capwords(content_area.iter), ", Grade ", grade.iter, sep=""),
    			bubble_plot_titles.SUB2=paste(x.axis.year, "to", y.axis.year),
    			bubble_plot_titles.LEGEND1="",
    			bubble_plot_titles.LEGEND2_P1=NULL,
    			bubble_plot_titles.LEGEND2_P2=NULL,
    			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.07, 0.07),
    			bubble_plot_configs.BUBBLE_X_TICKS=x.axis.ticks,
    			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=rep(0.7, length(x.axis.ticks)),
    			bubble_plot_configs.BUBBLE_Y_TICKS=y.axis.ticks,
    			bubble_plot_configs.BUBBLE_Y_TICKS_SIZE=rep(0.7, length(y.axis.ticks)),
    			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
    			bubble_plot_configs.BUBBLE_COLOR="blue",
    			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
    			bubble_plot_configs.BUBBLE_TIPS="FALSE",
    			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
    			bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",
    			bubble_plot_configs.BUBBLE_PLOT_LEGEND="FALSE",
    			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
    			bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=NULL,
    			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=c(x.axis.cut.text, y.axis.cut.text),
    			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(toupper(equating.method), "_", conversion.type, "_", content_area.iter, "_GRADE_", grade.iter, ".pdf", sep=""),
    			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Data", paste("Linkages", year.for.equate, sep="_"), "Figures"),
    			bubble_plot_pdftk.CREATE_CATALOG=FALSE)

#            pdf(file=paste("Data/Linkages_", year.for.equate, "/Figures/", toupper(equating.method), "_", conversion.type, "_", content_area.iter, "_GRADE_", grade.iter, ".pdf", sep=""), width=8, height=8)
#                plot(linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][['SCALE_SCORE']],
#                    linkage.data[GRADE==grade.iter & CONTENT_AREA==content_area.iter][[linkage.var.name]],
#                    type="p", xlab=x.axis.label, ylab=y.axis.label,
#                    main=paste(x.abb, "to", y.abb, equating.method, "concordance:", content_area.iter, "Grade", grade.iter))
#                    abline(h=y.axis.cut, lty=2, col="grey50")
#                    abline(v=x.axis.cut, lty=2, col="grey50")
#            dev.off()
        }
    }
} ### END linkagePlot function
