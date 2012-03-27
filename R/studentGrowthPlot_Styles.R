`studentGrowthPlot_Styles` <- 
  function(sgPlot.data,
           state,
           last.year,
           content_areas,
           districts,
           schools,
           reports.by.student,
           sgPlot.years,
           sgPlot.demo.report,
           sgPlot.folder,
           sgPlot.folder.names,
           sgPlot.anonymize,
           sgPlot.front.page,
           sgPlot.header.footer.color,
           sgPlot.fan,
           sgPlot.cleanup,
           sgPlot.baseline) {

	CUTLEVEL <- ID <- CONTENT_AREA <- NULL ## To prevent R CMD check warnings

	### Utility functions

	"%w/o%" <- function(x,y) x[!x %in% y]

	pretty_year <- function(x) sub("_", "-", x)

	create.long.cutscores.sgPlot <- function(state, content_area) {
		number.achievement.level.regions <- length(SGPstateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])
		if (!content_area %in% names(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
			tmp.list <- list()
			for (i in grep(content_area, names(SGPstateData[[state]][["Achievement"]][["Cutscores"]]))) {
				tmp.grades <- as.numeric(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]), "_")),
					ncol=2, byrow=TRUE)[,2])
				tmp.cutscores <- matrix(unlist(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[i]]),
					ncol=number.achievement.level.regions-1, byrow=TRUE)
				tmp.year <- unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[i], "[.]"))[2]
				for (j in seq(number.achievement.level.regions-1)) {
					tmp.list[[paste(i, j, sep="_")]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
						CUTLEVEL=rep(j, length(tmp.grades)+2),
						CUTSCORES=c(extendrange(tmp.cutscores[,j], f=0.15)[1], tmp.cutscores[,j], extendrange(tmp.cutscores[,j], f=0.15)[2]),
						YEAR=rep(tmp.year, length(tmp.grades)+2))
				}
			}
			tmp.long.cutscores <- subset(do.call(rbind, tmp.list), CUTLEVEL %in% 1:(number.achievement.level.regions-1))
			if (length(sort(tmp.year)) > 0 & !is.null(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[content_area]])) {
				tmp.long.cutscores <- subset(tmp.long.cutscores, as.numeric(unlist(sapply(strsplit(as.character(tmp.long.cutscores$YEAR), "_"), function(x) x[1]))) >= 
					as.numeric(sapply(strsplit(as.character(SGPstateData[[state]][["Student_Report_Information"]][["Earliest_Year_Reported"]][[content_area]]), "_"), function(x) x[1])))
			}
			tmp.long.cutscores
		} else {
			tmp.grades <- as.numeric(matrix(unlist(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]][[content_area]]), "_")),
				ncol=2, byrow=TRUE)[,2])
			tmp.list <- list()
			for (i in seq(number.achievement.level.regions-1)) {
				tmp.list[[i]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
					CUTLEVEL=rep(i, length(tmp.grades)+2),
					CUTSCORES=rep(SGPstateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]][i+1],
						length(tmp.grades)+2))
			}
			do.call(rbind, tmp.list)
		}
	} ## END create.long.cutscores.sgPlot


	### Define quantities/variables related to state

	if (state %in% names(SGPstateData)) {
		tmp.abbreviation <- SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]
		tmp.state <- paste(state.name[state==state.abb], tmp.abbreviation)
		tmp.organization <- SGPstateData[[state]][["Assessment_Program_Information"]][["Organization"]]
		number.achievement.level.regions <- length(SGPstateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])
		Cutscores <- list()
		for (i in content_areas) {
			Cutscores[[i]] <- create.long.cutscores.sgPlot(state, as.character(i))
		}
	} else {
		stop("Construction of student growth plots requires state meta-data to be included in the embedded SGPstateData set.\nPlease augment the SGPstateData set with your data or contact the SGP package maintainer to have your data added to the SGP package.")
	}	

	year_folder <- as.character(last.year) 

	## To Baseline or not to Baseline

        if (sgPlot.baseline) {
                 my.sgp <- "SGP_BASELINE"
                 my.sgp.level <- "SGP_LEVEL_BASELINE"
        } else {
                 my.sgp <- "SGP"
                 my.sgp.level <- "SGP_LEVEL"
        }

	### Loop over unique DISTRICTS, SCHOOLS, GRADES and then STUDENTS

	tmp.keys <- paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER", "GRADE", "LAST_NAME", "FIRST_NAME"), last.year, sep=".")

	## Districts

	setkeyv(sgPlot.data, tmp.keys[1])

	for (i in districts) {
		if (reports.by.student) {
			tmp_district_name <- as.character(sgPlot.data[J(i)][[paste("DISTRICT_NAME", last.year, sep=".")]][1])
			district_folder <- "Uncollated_Student_Reports"
		} else {
			if (sgPlot.demo.report) {
				tmp_district_name <- "Sample District"
				district_folder <- "Sample_District"
			} else {
				if (sgPlot.folder.names=="name") {
					tmp_district_name <- as.character(sgPlot.data[J(i)][[paste("DISTRICT_NAME", last.year, sep=".")]][1])
						district_folder <- gsub(" ", "_", paste(tmp_district_name, " (", i, ")", sep=""))
				} else {
					tmp_district_name <- as.character(sgPlot.data[J(i)][[paste("DISTRICT_NAME", last.year, sep=".")]][1])
						district_folder <- as.character(i)
				}
			}
		}
		tmp_district_ids <- unique(sgPlot.data[J(i)]$ID)
		tmp_district_data <- subset(sgPlot.data, ID %in% tmp_district_ids)

	## Schools

	setkeyv(tmp_district_data, tmp.keys[2])

	for (j in schools) {

		started.at <- proc.time()
		started.date <- date()

		if (reports.by.student) {
			tmp_school_name <- as.character(tmp_district_data[J(j)][[paste("SCHOOL_NAME", last.year, sep=".")]][1])
			school_folder <- NULL
		} else {
			if (sgPlot.demo.report) {
				tmp_school_name <- "Sample School"
				school_folder <- "Sample_School"
			} else {
				if (sgPlot.folder.names=="name") {
					tmp_school_name <- as.character(tmp_district_data[J(j)][[paste("SCHOOL_NAME", last.year, sep=".")]][1])
					school_folder <- gsub(" ", "_", paste(tmp_school_name, " (", j, ")", sep=""))
				} else {
					tmp_school_name <- as.character(tmp_district_data[J(j)][[paste("SCHOOL_NAME", last.year, sep=".")]][1])
					school_folder <- as.character(j)
				}
			}

			######################## SCHOOL Report Catalog LaTeX Header #################################################################################
			cat("\\documentclass[pdftex]{book}\n\\usepackage{hyperref,pdfpages}\n\\hypersetup{%\n", file=paste("school_catalog_", j, ".tex", sep=""))
			cat(paste("pdftitle={", tmp_school_name, ": ", pretty_year(last.year), " ", tmp.state, " Growth and Achievement Reports},\n", sep=""), 
				file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
			cat(paste("pdfauthor={", tmp.organization$Name, "/Center for Assessment Inc.},\n", sep=""), "pdfcreator={pdfLaTeX},\n", 
			paste("pdfproducer={", tmp.organization$Name, "/Center for Assessment Inc.}}\n", sep=""), 
				"\\begin{document}\n", sep="", file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
			cat(paste("\\pdfbookmark[-1]{", tmp_district_name, "}{", i, "}\n", sep=""), file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
			cat(paste("\\pdfbookmark[0]{", tmp_school_name, "}{", j, "}\n", sep=""), file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
			##############################################################################################################################################
		}
		tmp_school_ids <- unique(tmp_district_data[J(j)]$ID)
		tmp_school_data <- subset(tmp_district_data, ID %in% tmp_school_ids)

	## Grades

	setkeyv(tmp_school_data, tmp.keys[2])
	grades <- sort(unique(unlist(tmp_school_data[J(j), tmp.keys[3], with=FALSE]))) %w/o% NA
	setkeyv(tmp_school_data, tmp.keys[3])

	for (k in grades) {
		if (reports.by.student) {
			grade_folder <- NULL
		} else {
			if (sgPlot.folder.names=="name") {
				grade_folder <- paste("Grade", k, sep="_")
			} else {
				grade_folder <- substr(paste("0", as.character(k), sep=""), nchar(k), nchar(k)+1)
			}

			################################ SCHOOL Report Catalog LaTeX Code #########################################################
			cat(paste("\\pdfbookmark[1]{Grade ", k, "}{", j, k, "}\n", sep=""), 
				file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE) ## NOTE: j, k included in anchor for uniqueness
			###########################################################################################################################
		}
		tmp_grade_ids <- unique(tmp_school_data[J(k)]$ID)
		tmp_grade_data <- subset(tmp_school_data, ID %in% tmp_grade_ids)

	### Create path to pdf files

	path.to.pdfs <- paste(c(sgPlot.folder, year_folder, district_folder, school_folder, grade_folder), collapse="/")
	dir.create(path.to.pdfs, recursive=TRUE, showWarnings=FALSE)

	## Students

	setkeyv(tmp_grade_data, tmp.keys[4:5])

	for (n in unique(tmp_grade_data[["ID"]])) {
		FIRST_NAME <- gsub(" |/", "-", sort(tmp_grade_data[ID==n][[tmp.keys[5]]])[1]) 
		LAST_NAME <- gsub(" |/", "-", sort(tmp_grade_data[ID==n][[tmp.keys[4]]])[1])
		if (sgPlot.anonymize) {
			student_number <- 1234567890
		} else {
			student_number <- n
		}
		if (sgPlot.folder.names=="name" | sgPlot.anonymize) {
			file_name <- paste(paste(FIRST_NAME, LAST_NAME, student_number, year_folder, sep="_"), ".pdf", sep="")
		} else {
			file_name <- paste(n, ".pdf", sep="")
		}

		if (!reports.by.student) {
			################################ SCHOOL Report Catalog LaTeX Code ###################################################################################
			if (is.null(sgPlot.front.page)) {
			cat(paste("\\pdfbookmark[2]{", paste(LAST_NAME, ", ", FIRST_NAME, " (", student_number, ")", sep=""), "}{", n , "}\n\\includepdf[fitpaper=true]{", 
				path.to.pdfs, "/", file_name, "}\n", sep=""), file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
			} else {
				cat(paste("\\include{", sgPlot.front.page, "}\n\\pdfbookmark[2]{", paste(LAST_NAME, ", ", FIRST_NAME, " (", 
				student_number, ")", sep=""), "}{", n , "}\n\\includepdf[fitpaper=true,pages=2]{", path.to.pdfs, "/", file_name, "}\n", sep=""), 
					file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
			}
			#####################################################################################################################################################
		}

		################################ STUDENT Report Front Page Attach LaTeX Code ########################################################################
		cat("\\documentclass[pdftex]{article}\n\\usepackage{hyperref,pdfpages}\n\\hypersetup{%\n", file=paste("student_report_", j, ".tex", sep=""))
		cat(paste("pdftitle={", FIRST_NAME, " ", LAST_NAME, " (", student_number, ")", ": ", pretty_year(last.year), " ", 
			tmp.state, " Growth and Achievement Report},\n", sep=""), file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
		cat(paste("pdfauthor={", tmp.organization$Name, "/Center for Assessment Inc.},\n", sep=""), file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
		cat("pdfcreator={pdfLaTeX},\n", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
		cat(paste("pdfproducer={", tmp.organization$Name, "/Center for Assessment Inc.}}\n", sep=""), file=paste("student_report_", j, ".tex", sep=""), append=TRUE) 
		cat("\\begin{document}\n", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
		if (!is.null(sgPlot.front.page)) {
			cat("\\includepdf[fitpaper=true]{", sgPlot.front.page, "}\n", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
		}
		cat(paste("\\includepdf[fitpaper=true]{", path.to.pdfs, "/", file_name, "}\n", sep=""), 
			file=paste("student_report_", j, ".tex", sep=""), append=TRUE)

		cat("\\end{document}", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
		####################################################################################################################################################


	## Start pdf device
              
              if (length(content_areas)==2) {
                report.width=11
                report.height=8.5
              }
              if (length(content_areas)==3) {
                report.width=8.5
                report.height=11
              }
              if (!length(content_areas) %in% c(2,3)) {
                stop("Individual Student Report Templates currently only available for situations with 2 or 3 content areas.")
              }

              pdf(paste(path.to.pdfs, "/", file_name, sep=""), 
                  width=report.width, height=report.height, version="1.4")


              ########################################################################################################
              ###
              ### Overall Report viewport creation
              ###
              ########################################################################################################

              if (length(content_areas)==2) {
                report.vp <- viewport(layout = grid.layout(7, 4, widths = unit(c(2.5, 0.1, 8.3, 0.1), rep("inches", 4)), 
                                        heights = unit(c(0.35, 0.2, 3.55, 0.25, 3.55, 0.2, 0.4), rep("inches", 7))))

                content_area_1.vp <- viewport(layout.pos.row=3, layout.pos.col=3)
                content_area_2.vp <- viewport(layout.pos.row=5, layout.pos.col=3)
                top.border.vp <- viewport(layout.pos.row=1, layout.pos.col=1:4)
                bottom.border.vp <- viewport(layout.pos.row=7, layout.pos.col=1:4)
                left.legend.vp <- viewport(layout.pos.row=2:6, layout.pos.col=1)
              }

              if (length(content_areas)==3) {
                report.vp <- viewport(layout = grid.layout(9, 3, widths = unit(c(0.125, 8.3, 0.075), rep("inches", 3)), 
                                        heights = unit(c(0.35, 0.1, 3.256, 0.14, 3.256, 0.14, 3.256, 0.1, 0.4), rep("inches", 9))))

                content_area_1.vp <- viewport(layout.pos.row=3, layout.pos.col=2)
                content_area_2.vp <- viewport(layout.pos.row=5, layout.pos.col=2)
                content_area_3.vp <- viewport(layout.pos.row=7, layout.pos.col=2)
                top.border.vp <- viewport(layout.pos.row=1, layout.pos.col=1:3)
                bottom.border.vp <- viewport(layout.pos.row=9, layout.pos.col=1:3)
              }

              pushViewport(report.vp)

              for (vp in seq_along(content_areas)) {

		tmp_student_data <- as.data.frame(tmp_grade_data[ID==n & CONTENT_AREA==content_areas[vp]])
		pushViewport(get(paste("content_area_", vp, ".vp", sep="")))
                studentGrowthPlot(
                                  Scale_Scores=as.numeric(subset(tmp_student_data, select=paste("SCALE_SCORE", rev(sgPlot.years), sep="."))),
                                  Plotting_Scale_Scores=as.numeric(subset(tmp_student_data, select=paste("TRANSFORMED_SCALE_SCORE", rev(sgPlot.years), sep="."))),
                                  Achievement_Levels=as.character(unlist(subset(tmp_student_data, select=paste("ACHIEVEMENT_LEVEL", rev(sgPlot.years), sep=".")))),
                                  SGP=as.numeric(subset(tmp_student_data, select=paste(my.sgp, rev(sgPlot.years), sep="."))),
                                  SGP_Levels=as.character(unlist(subset(tmp_student_data, select=paste(my.sgp.level, rev(sgPlot.years), sep=".")))),
                                  Grades=as.numeric(subset(tmp_student_data, select=paste("GRADE", rev(sgPlot.years), sep="."))),
                                  Cuts_NY1=as.numeric(subset(tmp_student_data, select=grep("PROJ", names(tmp_student_data)))),
                                  Cutscores=Cutscores[[as.character(content_areas[vp])]],
                                  Report_Parameters=list(Current_Year=last.year, Content_Area=as.character(content_areas[vp]), State=state))

		popViewport()

              } ## END loop over content_areas


              ## Top Legend

              pushViewport(top.border.vp)
              grid.rect(gp=gpar(fill=sgPlot.header.footer.color, col=sgPlot.header.footer.color))
              grid.text(x=0.025, y=0.5, paste(FIRST_NAME, " ", LAST_NAME, sep="") , 
                        gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", col="white", cex=1.65), just="left", default.units="native")
              grid.text(x=0.975, y=0.5, tmp_school_name, gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", col="white", cex=1.65), just="right", default.units="native")
              popViewport()


              ## Bottom Legend

              pushViewport(bottom.border.vp)
              grid.rect(gp=gpar(fill=sgPlot.header.footer.color, col=sgPlot.header.footer.color))
              grid.text(x=0.02, y=0.70, paste("For more information please visit the", tmp.organization$Name, "at", tmp.organization$URL, "or contact", tmp.organization$Contact), 
                        gp=gpar(cex=0.8, col="white"), default.units="native", just="left")
              copyright.text <- paste("Cooperatively developed by the ", tmp.organization$Name, " & the Center for Assessment, Inc.", sep="")
              grid.text(x=0.02, y=0.30, paste(copyright.text, " Distributed by the ", tmp.organization$Name, ".", sep=""), 
                        gp=gpar(cex=0.8, col="white"), default.units="native", just="left")

#              grid.text(x=0.995, y=0.18, copyright.text, gp=gpar(col="white", cex=0.45), default.units="native", just="right")
#              grid.text(x=unit(0.992, "native")-convertWidth(grobWidth(textGrob(copyright.text, gp=gpar(cex=0.45))), "native"), y=0.19, "\\co", 
#                        gp=gpar(col="white", cex=0.55, fontfamily="HersheySymbol"), default.units="native", just="right")
              popViewport()


              ## Left Legend (Only with two content areas depicted)

              if (length(content_areas)==2) {

		pushViewport(left.legend.vp)

		# Interpretation

                interpretation.y <- 0.93
                achievement.level.region.colors <- paste("grey", round(seq(62, 91, length=number.achievement.level.regions)), sep="")

                grid.roundrect(x=unit(0.5, "native"), y=unit(interpretation.y, "native"), width=unit(0.9, "native"), height=unit(0.06, "native"), 
                               gp=gpar(fill=sgPlot.header.footer.color, col="black"))
                grid.text(x=0.5, y=interpretation.y+0.012, "How to interpret this student", gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", cex=1.05, col="white"))
                grid.text(x=0.5, y=interpretation.y-0.012, "growth & achievement report", gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", cex=1.05, col="white"))

                grid.roundrect(x=unit(0.2, "native"), y=unit(interpretation.y-0.08, "native"), width=unit(0.1, "native"), height=unit(0.05, "native"), r=unit(0.02, "inches"), 
                               gp=gpar(fill=achievement.level.region.colors[1], lwd=1))
                grid.circle(x=0.2, y=interpretation.y-0.08, r=0.02, default.units="native", gp=gpar(fill="white"))
                grid.text(x=0.325, y=interpretation.y-0.0675, tmp.abbreviation, gp=gpar(cex=0.9), default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.0925, "Scale Score", gp=gpar(cex=0.9), default.units="native", just="left")

                tmp.rect.height <- 0.125/number.achievement.level.regions
                for (i in seq(number.achievement.level.regions)) {
                    grid.rect(x=unit(0.2, "native"), y=unit(interpretation.y-0.125-(i-1)*tmp.rect.height, "native"), width=unit(0.1, "native"), height=unit(tmp.rect.height, "native"),
                               gp=gpar(fill=rev(achievement.level.region.colors)[i], col="white", lwd=1), just=c("center", "top"))
                } 
                grid.roundrect(x=unit(0.2, "native"), y=interpretation.y-0.125, width=unit(0.1, "native"), height=unit(0.125, "native"), r=unit(0.02, "inches"),
                               gp=gpar(col="black", lwd=1.5), just=c("center", "top"))
                grid.text(x=0.325, y=interpretation.y-0.1625, tmp.abbreviation, default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.1875, "Achievement", default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.2125, "Levels", default.units="native", just="left")

                grid.polygon(x=c(0.1875, 0.1875, 0.17, 0.2, 0.23, 0.2125, 0.2125), y=interpretation.y-c(0.35, 0.30, 0.31, 0.27, 0.31, 0.30, 0.35), default.units="native",
                             gp=gpar(fill="grey50"))
                grid.text(x=0.325, y=interpretation.y-0.285, "Student", gp=gpar(cex=0.9), default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.31, "Growth", gp=gpar(cex=0.9), default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.335, "Percentile", gp=gpar(cex=0.9), default.units="native", just="left")

		# Suggested uses

                suggested.y <- 0.52

                grid.roundrect(x=unit(0.5, "native"), y=unit(suggested.y, "native"), width=unit(0.9, "native"), height=unit(0.06, "native"), 
                               gp=gpar(fill=sgPlot.header.footer.color, col="black"))
                grid.text(x=0.5, y=suggested.y, "Suggested Uses", gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", cex=1.05, col="white"))

                grid.circle(x=0.075, y=suggested.y-0.07, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.07, "Review past growth to assess", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.09, "student academic progress toward", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.11, paste(tmp.abbreviation, "achievement goals."), gp=gpar(cex=0.8), default.units="native", just="left")

                grid.circle(x=0.075, y=suggested.y-0.14, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.14, "Develop remediation or enrich-", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.16, "ment plans based on rate of", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.18, "growth needed to reach higher", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.20, paste(tmp.abbreviation, "achievement levels."), gp=gpar(cex=0.8), default.units="native", just="left")

                if (sgPlot.fan) {
                   grid.circle(x=0.075, y=suggested.y-0.23, r=0.01, gp=gpar(fill="black"), default.units="native")
                   grid.text(x=0.12, y=suggested.y-0.23, "Identify the rate of progress", gp=gpar(cex=0.8), default.units="native", just="left")
                   grid.text(x=0.12, y=suggested.y-0.25, "needed in order to reach or", gp=gpar(cex=0.8), default.units="native", just="left")
                   grid.text(x=0.12, y=suggested.y-0.27, "maintain proficient status", gp=gpar(cex=0.8), default.units="native", just="left")
                   grid.text(x=0.12, y=suggested.y-0.29, paste("on the", tmp.abbreviation, "next year."), gp=gpar(cex=0.8), default.units="native", just="left")
                }


		# Extra stuff

                grid.lines(x=1.0, y=c(0.025,0.975), gp=gpar(lwd=1.8), default.units="native")

		popViewport()
                
              } ## END Left legend


              ## Turn pdf device off

              dev.off()

              ## Code to LaTeX document attaching first page/adding meta-data

              system(paste("pdflatex -interaction=batchmode student_report_", j, ".tex", sep=""), ignore.stdout = TRUE)
              file.rename(paste("student_report_", j, ".pdf", sep=""), paste(path.to.pdfs, "/", file_name, sep=""))

            } ## END for loop for STUDENTS (n)
          } ## END for loop for GRADES (k)
	if (!reports.by.student) {
		cat("\\end{document}", file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
		system(paste("pdflatex -interaction=batchmode school_catalog_", j, ".tex", sep=""), ignore.stdout = TRUE)
		system(paste("pdflatex -interaction=batchmode school_catalog_", j, ".tex", sep=""), ignore.stdout = TRUE)
		file.rename(paste("school_catalog_", j, ".pdf", sep=""), file.path(sgPlot.folder, year_folder, district_folder, 
			paste(year_folder, "_", district_folder, "_", school_folder, "_Individual_SGP_Report_Catalog.pdf", sep="")))
	}
	if (sgPlot.cleanup) {
		files.to.remove <- list.files(pattern=as.character(j), all.files=TRUE)
		lapply(files.to.remove, file.remove)
	}

        message(paste("\tStarted", last.year, tmp_school_name, "student growth plots:", started.date))
        message(paste("\tFinished", last.year, tmp_school_name, "student growth plots:", date(), "in", timetaken(started.at), "\n"))

        } ## END for loop for SCHOOLS (j)
        #system(paste("zip -r ", pdf.folder, "/", year_folder, "/", district_folder, "_", last.year, ".zip ", pdf.folder, year_folder, district_folder, sep=""))
      } ## END for loop for DISTRICTS (i)
} ## END studentGrowthPlot_Styles function
