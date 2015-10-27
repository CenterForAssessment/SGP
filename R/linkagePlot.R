`linkagePlot` <-
function(linkage.data,
        conversion.type,
        year.for.equate,
        state) {

    tmp.years <- sort(unique(linkage.data[['YEAR']]))
    if (conversion.type=="OLD_TO_NEW") {
        linkage.year <- rev(tmp.years)[2]
        x.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Assessment_Abbreviation"]]
        y.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Assessment_Abbreviation", year.for.equate, sep=".")]]
        x.axis.label <- paste(x.abb, "Scale Score")
        y.axis.label <- paste(y.abb, "Scale Score")
    } else {
        linkage.year <- rev(tmp.years)[1]
        x.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][[paste("Assessment_Abbreviation", year.for.equate, sep=".")]]
        y.abb <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][["Assessment_Abbreviation"]]
        x.axis.label <- paste(x.abb, "Scale Score")
        y.axis.label <- paste(y.abb, "Scale Score")
    }
    tmp.linkage.data <- linkage.data[YEAR==linkage.year]
    for (grade.iter in unique(linkage.data[['GRADE']])) {
        for (content_area.iter in unique(linkage.data[['CONTENT_AREA']])) {
            pdf(file=paste("Data/Linkages/Figures/", toupper(equating.method.iter), "_", content_area.iter, "_GRADE_", grade.iter, ".pdf", sep=""), width=8, height=8)
            plot(tmp.linkage.data[['SCALE_SCORE']], tmp.linkage.data[[grep('SCALE_SCORE_EQUATED', names(linkage.data), value=TRUE)]],
                type="p", xlab=x.axis.label, ylab=y.axis.label,
                main=paste(x.abb, "to", y.abb, equating.method.iter, "concordance:", content_area.iter, "Grade", grade.iter))
} ### END linkagePlot function
