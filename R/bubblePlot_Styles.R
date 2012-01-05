`bubblePlot_Styles` <- 
	function(sgp_object,
		state,
		bPlot.years=NULL,
		bPlot.content_areas=NULL,
		bPlot.districts=NULL,
		bPlot.schools=NULL,
		bPlot.styles=c(1),
		bPlot.levels=NULL,
		bPlot.full.academic.year=TRUE,
		bPlot.minimum.n=10,
		bPlot.anonymize=FALSE,
		bPlot.prior.achievement=TRUE, 
		bPlot.draft=FALSE,
		bPlot.format="print",
		bPlot.folder="Visualizations/bubblePlots") {


	DISTRICT_NUMBER <- DISTRICT_NAME <- SCHOOL_NUMBER <- SCHOOL_NAME <- SCHOOL_ENROLLMENT_STATUS <- YEAR <- CONTENT_AREA <- MEDIAN_SGP_COUNT <- NULL ## To prevent R CMD check warnings
	ID <- YEAR_INTEGER_TMP <- SCALE_SCORE <- SGP <- GRADE <- NULL ## To prevent R CMD check warnings
        ### Define relevant quantities

        # State stuff

        if (state %in% c(state.abb, "DEMO")) {
		state.name.label <- c(state.name, "DEMONSTRATION")[state==c(state.abb, "DEMO")]
		test.abbreviation.label <- SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]
	} else {
		state.name.label <- test.abbreviation.label <- NULL
	}
		state.name.file.label <- gsub("_", " ", state.name.label)

        # draft message

        if (bPlot.draft) {
                bPlot.message <- c("grid.text(x=unit(50, 'native'), y=unit(50, 'native'), 'DRAFT - DO NOT DISTRIBUTE', rot=-30, gp=gpar(col='grey80', cex=2.9, alpha=0.8, fontface=2))",
          "grid.lines(x=unit(50, 'native'), y=c(0,1), gp=gpar(col='grey40', lwd=1.5, lty=2, alpha=0.5))")
        } else {
                bPlot.message <- NULL
        }


	### Utility functions	

	"%w/o%" <- function(x,y) x[!x %in% y]

	pretty_year <- function(x) sub("_", "-", x)

	capwords <- function(x) {
		special.words <- c("ELA", "EMH", "II", "III", "IV")
		if (x %in% special.words) return(x)
		s <- gsub("_", " ", x)
		s <- strsplit(s, split=" ")[[1]]
		s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
		s <- strsplit(s, split="-")[[1]]
		paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
	}

	create.bPlot.labels <- function(year.iter, y.variable.iter, bubblePlot_LEVEL) {
		pretty_year <- function(x) sub("_", "-", x)
		my.labels <- list()
		my.labels$x.year.label <- pretty_year(year.iter)
		if (length(grep("PRIOR", y.variable.iter)) > 0) {
			my.labels$y.year <- paste(as.numeric(unlist(strsplit(as.character(year.iter), "_")))-1, collapse="_")
			if (bubblePlot_LEVEL=="Summary") my.labels$y.year.label <- paste(pretty_year(my.labels$y.year), "Prior Percent at/above Proficient")
			if (bubblePlot_LEVEL=="Individual") my.labels$y.year.label <- paste(pretty_year(my.labels$y.year), "Prior Achievement Level")
			if (bubblePlot_LEVEL=="Summary") my.labels$main.title <- paste(test.abbreviation.label, "Growth and Prior Achievement")
			if (bubblePlot_LEVEL=="Individual") my.labels$main.title <- paste(test.abbreviation.label, "Growth and Prior Achievement")
			if (bubblePlot_LEVEL=="Summary") my.labels$pdf.title <- "Bubble_Plot_(Prior_Achievement)"
			if (bubblePlot_LEVEL=="Individual") my.labels$pdf.title <- "Student_Bubble_Plot_(Prior_Achievement)"
		} else {
			my.labels$y.year <- year.iter
			if (bubblePlot_LEVEL=="Summary") my.labels$y.year.label <- paste(pretty_year(my.labels$y.year), "Percent at/above Proficient")
			if (bubblePlot_LEVEL=="Individual") my.labels$y.year.label <- paste(pretty_year(my.labels$y.year), "Achievement Level")
			if (bubblePlot_LEVEL=="Summary") my.labels$main.title <- paste(test.abbreviation.label, "Growth and Achievement")
			if (bubblePlot_LEVEL=="Individual") my.labels$main.title <- paste(test.abbreviation.label, "Growth and Achievement")
			if (bubblePlot_LEVEL=="Summary") my.labels$pdf.title <- "Bubble_Plot_(Current_Achievement)"
			if (bubblePlot_LEVEL=="Individual") my.labels$pdf.title <- "Student_Bubble_Plot_(Current_Achievement)"
		}
		return(my.labels)
	}

	names.merge <- function(tmp.data, bPlot.anonymize) {
		if ("SCHOOL_NUMBER" %in% names(tmp.data)) {
			tmp.names <- unique(data.table(sgp_object@Data[!is.na(DISTRICT_NUMBER) & !is.na(SCHOOL_NUMBER), 
				list(DISTRICT_NUMBER, DISTRICT_NAME, SCHOOL_NUMBER, SCHOOL_NAME)], key="SCHOOL_NUMBER"))
			if (bPlot.anonymize) {
				tmp.names$SCHOOL_NAME <- paste("Example School", as.numeric(as.factor(tmp.names$SCHOOL_NUMBER)))
				tmp.names$DISTRICT_NAME <- paste("Example District", as.numeric(as.factor(tmp.names$DISTRICT_NUMBER)))
			}
			key(tmp.data) <- "SCHOOL_NUMBER"
		}
		if ("DISTRICT_NUMBER" %in% names(tmp.data)) {
			tmp.names <- unique(data.table(sgp_object@Data[!is.na(DISTRICT_NUMBER), 
				list(DISTRICT_NUMBER, DISTRICT_NAME)], key="DISTRICT_NUMBER"))
			if (bPlot.anonymize) {
				tmp.names$DISTRICT_NAME <- paste("Example District", as.numeric(as.factor(tmp.names$DISTRICT_NUMBER)))
			}
			key(tmp.data) <- "DISTRICT_NUMBER"
		}
		tmp.names[tmp.data, mult="last"]
	}
           

	get.my.iters <- function(tmp.data, bubblePlot_LEVEL, ...) {
		my.iters <- list()

	        # Year Stuff

		if (is.null(bPlot.years)) {
			my.iters$tmp.years <- tail(sort(unique(tmp.data$YEAR)), 1)
		} else {
			my.iters$tmp.years <- bPlot.years
			if (is.factor(tmp.data$YEAR)) my.iters$tmp.years <- as.factor(my.iters$tmp.years)
		}

		# Content Area Stuff

		if (is.null(bPlot.content_areas)) {
			my.iters$tmp.content_areas <- unique(tmp.data$CONTENT_AREA) %w/o% NA
		} else {
			my.iters$tmp.content_areas <- bPlot.content_areas
			if (is.factor(tmp.data$CONTENT_AREA)) my.iters$tmp.content_areas <- as.factor(my.iters$tmp.content_areas)
		}

		# Reconcile choice of District and Schools

		if (is.null(bPlot.schools) & is.null(bPlot.districts)) {
			my.iters$tmp.districts <- sort(unique(tmp.data[YEAR %in% my.iters$tmp.years]$DISTRICT_NUMBER)) %w/o% NA
			my.iters$tmp.schools <- sort(unique(tmp.data[YEAR %in% my.iters$tmp.years]$SCHOOL_NUMBER)) %w/o% NA
		}

		if (is.null(bPlot.schools) & !is.null(bPlot.districts)) {
         		my.iters$tmp.districts <- bPlot.districts
         		if (is.factor(tmp.data$DISTRICT_NUMBER)) my.iters$tmp.districts <- as.factor(my.iters$tmp.districts)
			my.iters$tmp.schools <- unique(tmp.data$SCHOOL_NUMBER[tmp.data$DISTRICT_NUMBER %in% my.iters$tmp.districts]) %w/o% NA
		}

		if (!is.null(bPlot.schools) & is.null(bPlot.districts)) {
         		my.iters$tmp.schools <- bPlot.schools 
			if (is.factor(tmp.data$SCHOOL_NUMBER)) my.iters$tmp.schools <- as.factor(my.iters$tmp.schools)
			my.iters$tmp.districts <- unique(tmp.data$DISTRICT_NUMBER[tmp.data$SCHOOL_NUMBER %in% my.iters$tmp.schools]) %w/o% NA
		}

		if (!is.null(bPlot.schools) & !is.null(bPlot.districts)) {
         		my.iters$tmp.districts <- bPlot.districts
         		my.iters$tmp.schools <- bPlot.schools 
			my.iters$tmp.schools <- unique(c(my.iters$tmp.schools, tmp.data$SCHOOL_NUMBER[tmp.data$DISTRICT_NUMBER %in% my.iters$tmp.districts])) %w/o% NA
			my.iters$tmp.districts <- unique(c(my.iters$tmp.districts, tmp.data$DISTRICT_NUMBER[tmp.data$SCHOOL_NUMBER %in% my.iters$tmp.schools])) %w/o% NA
         		if (is.factor(tmp.data$DISTRICT_NUMBER)) my.iters$tmp.districts <- as.factor(my.iters$tmp.districts)
			if (is.factor(tmp.data$SCHOOL_NUMBER)) my.iters$tmp.schools <- as.factor(my.iters$tmp.schools)
		}

		# y.variable (include/not include prior achievement)

		if (bPlot.prior.achievement & length(grep("PRIOR", names(tmp.data))) > 0) {
			if (bubblePlot_LEVEL=="Summary") my.iters$tmp.y.variable <- c("PERCENT_AT_ABOVE_PROFICIENT", "PERCENT_AT_ABOVE_PROFICIENT_PRIOR")
			if (bubblePlot_LEVEL=="Individual") my.iters$tmp.y.variable <- c("SCALE_SCORE", "SCALE_SCORE_PRIOR")
		} else {
			if (bubblePlot_LEVEL=="Summary") my.iters$tmp.y.variable <- "PERCENT_AT_ABOVE_PROFICIENT"
			if (bubblePlot_LEVEL=="Individual") my.iters$tmp.y.variable <- "SCALE_SCORE"
		}
		return(my.iters)
	} ## END get.my.iters


#################################################################################################################
####
#### Summary Level bubblePlots
####
#################################################################################################################

### < 100 are @Summary level bubblePlots

bubblePlot_LEVEL <- "Summary"
 

###################################################################
### BubblePlot Style 1 (State level bubblePlots by Schools)
###################################################################

if (1 %in% bPlot.styles) {

		started.at <- proc.time()
		message(paste("Started bubblePlot Style 1", date()), "\n")

		### Data sets and relevant quantities used for bubblePlots

		if (bPlot.full.academic.year) {
			tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
				SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
		} else {
			tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR"]]
		}

		# Merge in school and district names and anonymize school names (if requested)

		tmp.bPlot.data <- names.merge(tmp.bPlot.data, bPlot.anonymize)

		### Get tmp.years, tmp.content_areas, and tmp.y.variable

		my.iters <- get.my.iters(tmp.bPlot.data, bubblePlot_LEVEL)

		### Start loops for bubblePlots

		for (year.iter in my.iters$tmp.years) {  ### Loop over year
		for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas
		for (y.variable.iter in my.iters$tmp.y.variable) {  ### Loop over CURRENT and PRIOR achievement (if requested)

		# Subset data

		bPlot.data <- tmp.bPlot.data[YEAR==year.iter & CONTENT_AREA==content_area.iter & MEDIAN_SGP_COUNT >= bPlot.minimum.n]

		# Create labels

		bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL) 


		### Create bubblePlot ###

		bubblePlot(
			bubble_plot_data.X=bPlot.data[["MEDIAN_SGP"]],
			bubble_plot_data.Y=bPlot.data[[y.variable.iter]],
			bubble_plot_data.SUBSET=NULL, 
			bubble_plot_data.INDICATE=NULL,
			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
			bubble_plot_data.SIZE=bPlot.data[["MEDIAN_SGP_COUNT"]],
			bubble_plot_data.LEVELS=NULL, 
			bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(bPlot.data[["MEDIAN_SGP"]], " (", bPlot.data[["MEDIAN_SGP_COUNT"]], ")", sep=""),
				paste(bPlot.data[[y.variable.iter]], " (", bPlot.data[[paste(y.variable.iter, "_COUNT", sep="")]], ")", sep="")),
			bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Median Student Growth Percentile")),
			bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
			bubble_plot_labels.SIZE=c(50, 100, 250, 500),
			bubble_plot_labels.LEVELS=NULL, #levels(bubblePlot[["subset.factor"]]),
			bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Median SGP (Count)"),
				paste(bPlot.labels$y.year.label, " (Count)")),
			bubble_plot_labels.BUBBLE_TITLES=bPlot.data[["SCHOOL_NAME"]],
			bubble_plot_titles.MAIN=bPlot.labels$main.title,
			bubble_plot_titles.SUB1=paste(state.name.label, "School Performance"),
			bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, test.abbreviation.label, capwords(content_area.iter)),
			bubble_plot_titles.LEGEND1="School Size",
			bubble_plot_titles.LEGEND2_P1=NULL,
			bubble_plot_titles.LEGEND2_P2=NULL,

			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
			bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
			bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
			bubble_plot_configs.BUBBLE_COLOR="deeppink2",
			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
			bubble_plot_configs.BUBBLE_TIPS="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
			bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
			bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(state.name.file.label, year.iter, capwords(content_area.iter), "State", bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "State", "Style_1"),
			bubble_plot_pdftk.CREATE_CATALOG=FALSE)

		} ## END loop over y.variable.iter
		} ## End loop over content_area.iter
		} ## End loop over year.iter

		message(paste("Finished bubblePlot Style 1", date(), "in", timetaken(started.at), "\n"))

} ## END bubblePlot style 1


#######################################################################################
### BubblePlot Style 2 (State level bubblePlots with district schools highlighted)
#######################################################################################

if (2 %in% bPlot.styles) {

		started.at <- proc.time()
		message(paste("Started bubblePlot Style 2", date()), "\n")

		### Data sets and relevant quantities used for bubblePlots

		if (!is.null(bPlot.levels)) {
			tmp.bPlot.levels.txt <- parse(text=paste("100*length(grep('Yes',", bPlot.levels, "))/length(grep('Yes|No',", bPlot.levels, "))", sep=""))
		}

		if (bPlot.full.academic.year) {
			tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
				SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
			if (!is.null(bPlot.levels)) {
				tmp.bPlot.levels.data <- sgp_object@Data[SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes", 
					eval(tmp.bPlot.levels.txt), by=list(SCHOOL_NUMBER, CONTENT_AREA, YEAR)]
				key(tmp.bPlot.data) <- key(tmp.bPlot.levels.data) <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
				tmp.bPlot.data <- tmp.bPlot.levels.data[tmp.bPlot.data]
				my.level.labels <- c("Less than 20 percent", "20 to 40 percent", "40 to 60 percent", "60 to 80 percent", "More than 80 percent")
				tmp.bPlot.data$V1 <- cut(tmp.bPlot.data$V1, seq(0,100, by=20), include.lowest=TRUE, labels=my.level.labels)
			}
		} else {
			tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR"]]
			if (!is.null(bPlot.levels)) {
				tmp.bPlot.levels.data <- sgp_object@Data[, eval(tmp.bPlot.levels.txt), by=list(SCHOOL_NUMBER, CONTENT_AREA, YEAR)]
				key(tmp.bPlot.data) <- key(tmp.bPlot.levels.data) <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
				tmp.bPlot.data <- tmp.bPlot.levels.data[tmp.bPlot.data]
				my.level.labels <- c("Less than 20 percent", "20 to 40 percent", "40 to 60 percent", "60 to 80 percent", "More than 80 percent")
				tmp.bPlot.data$V1 <- cut(tmp.bPlot.data$V1, seq(0,100, by=20), include.lowest=TRUE, labels=my.level.labels)
			}
		}

		# Merge in school and district names and anonymize school names (if requested)

		tmp.bPlot.data <- names.merge(tmp.bPlot.data, bPlot.anonymize)

		### Get tmp.years, tmp.content_areas, and tmp.y.variable

		my.iters <- get.my.iters(tmp.bPlot.data, bubblePlot_LEVEL)

		### Start loops for bubblePlots

		for (year.iter in my.iters$tmp.years) {  ### Loop over year
		for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas

		# Subset data

		bPlot.data <- tmp.bPlot.data[YEAR==year.iter & CONTENT_AREA==content_area.iter & MEDIAN_SGP_COUNT >= bPlot.minimum.n]

		# Loop over current & prior and bPlot.levels 

		for (levels.iter in levels(factor(bPlot.data$V1))) {
		for (y.variable.iter in my.iters$tmp.y.variable) { 

		# Create labels

		bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL) 

		### Create bubblePlot ###

		bubblePlot(
			bubble_plot_data.X=bPlot.data[["MEDIAN_SGP"]],
			bubble_plot_data.Y=bPlot.data[[y.variable.iter]],
			bubble_plot_data.SUBSET=which(bPlot.data[["V1"]]==levels.iter), 
			bubble_plot_data.INDICATE=NULL,
			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
			bubble_plot_data.SIZE=bPlot.data[["MEDIAN_SGP_COUNT"]],
			bubble_plot_data.LEVELS=bPlot.data[["V1"]], 
			bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(bPlot.data[["MEDIAN_SGP"]], " (", bPlot.data[["MEDIAN_SGP_COUNT"]], ")", sep=""),
				paste(bPlot.data[[y.variable.iter]], " (", bPlot.data[[paste(y.variable.iter, "_COUNT", sep="")]], ")", sep="")),
			bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Median Student Growth Percentile")),
			bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
			bubble_plot_labels.SIZE=c(50, 100, 250, 500),
			bubble_plot_labels.LEVELS=levels(bPlot.data[["V1"]]),
			bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Median SGP (Count)"),
				paste(bPlot.labels$y.year.label, " (Count)")),
			bubble_plot_labels.BUBBLE_TITLES=bPlot.data[["SCHOOL_NAME"]],
			bubble_plot_titles.MAIN=bPlot.labels$main.title,
			bubble_plot_titles.SUB1=paste(state.name.label, "School Performance"),
			bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, test.abbreviation.label, capwords(content_area.iter)),
			bubble_plot_titles.LEGEND1="School Size",
			bubble_plot_titles.LEGEND2_P1="Percentage Students",
			bubble_plot_titles.LEGEND2_P2=paste(head(unlist(strsplit(capwords(bPlot.levels), " ")), -1), collapse=" "),

			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
			bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
			bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
			bubble_plot_configs.BUBBLE_COLOR=NULL,
			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
			bubble_plot_configs.BUBBLE_TIPS="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
			bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
			bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(state.name.file.label, year.iter, capwords(content_area.iter), capwords(levels.iter), "State", bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "State", "Style_2", bPlot.levels),
			bubble_plot_pdftk.CREATE_CATALOG=FALSE)

		} ## END loop over y.variable.iter
		} ## END loop over levels.iter
		} ## End loop over content_area.iter
		} ## End loop over year.iter

		message(paste("Finished bubblePlot Style 2", date(), "in", timetaken(started.at), "\n"))

} ## END bubblePlot style 2


#######################################################################################
### BubblePlot Style 10 (State level bubblePlots with district schools highlighted)
#######################################################################################

if (10 %in% bPlot.styles) {

                started.at <- proc.time()
                message(paste("Started bubblePlot Style 10", date()), "\n")

                ### Data sets and relevant quantities used for bubblePlots

                if (bPlot.full.academic.year) {
                        tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
                                SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
                } else {
                        tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR"]]
                }

                # Merge in school and district names and anonymize school names (if requested)

                tmp.bPlot.data <- names.merge(tmp.bPlot.data, bPlot.anonymize)

                ### Get tmp.years, tmp.content_areas, and tmp.y.variable

                my.iters <- get.my.iters(tmp.bPlot.data, bubblePlot_LEVEL)

                ### Start loops for bubblePlots

                for (year.iter in my.iters$tmp.years) {  ### Loop over year
                for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas

                # Subset data

                bPlot.data <- tmp.bPlot.data[YEAR==year.iter & CONTENT_AREA==content_area.iter & MEDIAN_SGP_COUNT >= bPlot.minimum.n]

                # Loop over unique districts

                for (district_number.iter in intersect(my.iters$tmp.districts, bPlot.data$DISTRICT_NUMBER)) { ### Loop over DISTRICT NUMBERS   
                for (y.variable.iter in my.iters$tmp.y.variable) {  ### Loop over CURRENT and PRIOR achievement (if requested)

                # Create labels

                bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL)
                district.name.label <- as.character(bPlot.data[DISTRICT_NUMBER==district_number.iter]$DISTRICT_NAME[1])

                ### Create bubblePlot ###

                bubblePlot(
                        bubble_plot_data.X=bPlot.data[["MEDIAN_SGP"]],
                        bubble_plot_data.Y=bPlot.data[[y.variable.iter]],
                        bubble_plot_data.SUBSET=which(bPlot.data[["DISTRICT_NUMBER"]]==district_number.iter),
                        bubble_plot_data.INDICATE=NULL,
                        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
                        bubble_plot_data.SIZE=bPlot.data[["MEDIAN_SGP_COUNT"]],
                        bubble_plot_data.LEVELS=NULL,
                        bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(bPlot.data[["MEDIAN_SGP"]], " (", bPlot.data[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                                paste(bPlot.data[[y.variable.iter]], " (", bPlot.data[[paste(y.variable.iter, "_COUNT", sep="")]], ")", sep="")),
                        bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Median Student Growth Percentile")),
                        bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
                        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
                        bubble_plot_labels.LEVELS=NULL, #levels(bubblePlot[["subset.factor"]]),
                        bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Median SGP (Count)"),
                                paste(bPlot.labels$y.year.label, " (Count)")),
                        bubble_plot_labels.BUBBLE_TITLES=bPlot.data[["SCHOOL_NAME"]],
                        bubble_plot_titles.MAIN=bPlot.labels$main.title,
                        bubble_plot_titles.SUB1=paste(district.name.label, "School Performance"),
                        bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, test.abbreviation.label, capwords(content_area.iter)),
                        bubble_plot_titles.LEGEND1="School Size",
                        bubble_plot_titles.LEGEND2_P1=NULL,
                        bubble_plot_titles.LEGEND2_P2=NULL,

                        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
                        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
                        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
                        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
                        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.01,
                        bubble_plot_configs.BUBBLE_COLOR="deeppink2",
                        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
                        bubble_plot_configs.BUBBLE_TIPS="TRUE",
                        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
                        bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
                        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
                        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
                        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
                        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(district.name.label, year.iter, capwords(content_area.iter), "District", bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
                        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "District", "Style_10"),
                        bubble_plot_pdftk.CREATE_CATALOG=FALSE)

                } ## END loop over y.variable.iter
                } ## End loop over district_number.iter
                } ## End loop over content_area.iter
                } ## End loop over year.iter

                message(paste("Finished bubblePlot Style 10", date(), "in", timetaken(started.at), "\n"))

} ## END bubblePlot style 10


#######################################################################################
### BubblePlot Style 11 (State level bubblePlots with district schools highlighted)
#######################################################################################

if (11 %in% bPlot.styles) {

		started.at <- proc.time()
		message(paste("Started bubblePlot Style 11", date()), "\n")

		### Data sets and relevant quantities used for bubblePlots

		if (!is.null(bPlot.levels)) {
			tmp.bPlot.levels.txt <- parse(text=paste("100*length(grep('Yes',", bPlot.levels, "))/length(grep('Yes|No',", bPlot.levels, "))", sep=""))
		}

		if (bPlot.full.academic.year) {
			tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
				SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
			if (!is.null(bPlot.levels)) {
				tmp.bPlot.levels.data <- sgp_object@Data[SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes", 
					eval(tmp.bPlot.levels.txt), by=list(SCHOOL_NUMBER, CONTENT_AREA, YEAR)]
				key(tmp.bPlot.data) <- key(tmp.bPlot.levels.data) <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
				tmp.bPlot.data <- tmp.bPlot.levels.data[tmp.bPlot.data]
				my.level.labels <- c("Less than 20 percent", "20 to 40 percent", "40 to 60 percent", "60 to 80 percent", "More than 80 percent")
				tmp.bPlot.data$V1 <- cut(tmp.bPlot.data$V1, seq(0,100, by=20), include.lowest=TRUE, labels=my.level.labels)
			}
		} else {
			tmp.bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR"]]
			if (!is.null(bPlot.levels)) {
				tmp.bPlot.levels.data <- sgp_object@Data[, eval(tmp.bPlot.levels.txt), by=list(SCHOOL_NUMBER, CONTENT_AREA, YEAR)]
				key(tmp.bPlot.data) <- key(tmp.bPlot.levels.data) <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
				tmp.bPlot.data <- tmp.bPlot.levels.data[tmp.bPlot.data]
				my.level.labels <- c("Less than 20 percent", "20 to 40 percent", "40 to 60 percent", "60 to 80 percent", "More than 80 percent")
				tmp.bPlot.data$V1 <- cut(tmp.bPlot.data$V1, seq(0,100, by=20), include.lowest=TRUE, labels=my.level.labels)
			}
		}

		# Merge in school and district names and anonymize school names (if requested)

		tmp.bPlot.data <- names.merge(tmp.bPlot.data, bPlot.anonymize)

		### Get tmp.years, tmp.content_areas, and tmp.y.variable

		my.iters <- get.my.iters(tmp.bPlot.data, bubblePlot_LEVEL)

		### Start loops for bubblePlots

		for (year.iter in my.iters$tmp.years) {  ### Loop over year
		for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas

		# Loop over unique districts

		for (district_number.iter in my.iters$tmp.districts) { ### Loop over DISTRICT NUMBERS

		# Subset data

		bPlot.data <- tmp.bPlot.data[YEAR==year.iter & CONTENT_AREA==content_area.iter & DISTRICT_NUMBER==district_number.iter & MEDIAN_SGP_COUNT >= bPlot.minimum.n]

		# Loop over current & prior and bPlot.levels 

		for (levels.iter in levels(factor(bPlot.data$V1))) {
		for (y.variable.iter in my.iters$tmp.y.variable) { 

		# Create labels

		bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL) 
		district.name.label <- as.character(bPlot.data[DISTRICT_NUMBER==district_number.iter]$DISTRICT_NAME[1])

		### Create bubblePlot ###

		bubblePlot(
			bubble_plot_data.X=bPlot.data[["MEDIAN_SGP"]],
			bubble_plot_data.Y=bPlot.data[[y.variable.iter]],
			bubble_plot_data.SUBSET=which(bPlot.data[["V1"]]==levels.iter), 
			bubble_plot_data.INDICATE=NULL,
			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
			bubble_plot_data.SIZE=bPlot.data[["MEDIAN_SGP_COUNT"]],
			bubble_plot_data.LEVELS=bPlot.data[["V1"]], 
			bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(bPlot.data[["MEDIAN_SGP"]], " (", bPlot.data[["MEDIAN_SGP_COUNT"]], ")", sep=""),
				paste(bPlot.data[[y.variable.iter]], " (", bPlot.data[[paste(y.variable.iter, "_COUNT", sep="")]], ")", sep="")),
			bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Median Student Growth Percentile")),
			bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
			bubble_plot_labels.SIZE=c(50, 100, 250, 500),
			bubble_plot_labels.LEVELS=levels(bPlot.data[["V1"]]),
			bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Median SGP (Count)"),
				paste(bPlot.labels$y.year.label, " (Count)")),
			bubble_plot_labels.BUBBLE_TITLES=bPlot.data[["SCHOOL_NAME"]],
			bubble_plot_titles.MAIN=bPlot.labels$main.title,
			bubble_plot_titles.SUB1=paste(district.name.label, "School Performance"),
			bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, test.abbreviation.label, capwords(content_area.iter)),
			bubble_plot_titles.LEGEND1="School Size",
			bubble_plot_titles.LEGEND2_P1="Percentage Students",
			bubble_plot_titles.LEGEND2_P2=paste(head(unlist(strsplit(capwords(bPlot.levels), " ")), -1), collapse=" "),

			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
			bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
			bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
			bubble_plot_configs.BUBBLE_COLOR=NULL,
			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
			bubble_plot_configs.BUBBLE_TIPS="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
			bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
			bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(district.name.label, year.iter, capwords(content_area.iter), capwords(levels.iter), "District", bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "District", "Style_11", bPlot.levels),
			bubble_plot_pdftk.CREATE_CATALOG=FALSE)

		} ## END loop over y.variable.iter
		} ## END loop over levels.iter
		} ## End loop over district_number.iter
		} ## End loop over content_area.iter
		} ## End loop over year.iter

		message(paste("Finished bubblePlot Style 11", date(), "in", timetaken(started.at), "\n"))

} ## END bubblePlot style 11

#################################################################################################################
####
#### Individual Level bubblePlots
####
#################################################################################################################

### >= 100 are @Data level bubblePlots

bubblePlot_LEVEL <- "Individual"
 

###################################################################
### bubblePlot style 100 (Individual Student within Grade Chart)
###################################################################

if (100 %in% bPlot.styles) {

		started.at <- proc.time()
		message(paste("Started bubblePlot Style 100", date()), "\n")

		### Utility functions

		get.my.cutscore.year <- function(state, content_area, year) {
			tmp.cutscore.years <- sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[
				grep(content_area, names(SGPstateData[[state]][["Achievement"]][["Cutscores"]]))], "[.]"), function(x) x[2])
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

		### Create Prior Scale Score (if requested)

		if (bPlot.prior.achievement) {
			sgp_object@Data$YEAR_INTEGER_TMP <- as.integer(sgp_object@Data$YEAR) ## To convert YEAR, when factor, to integer
			key(sgp_object@Data) <- c("ID", "CONTENT_AREA", "YEAR_INTEGER_TMP", "VALID_CASE") ## CRITICAL that VALID_CASE is last in group
			sgp_object@Data$SCALE_SCORE_PRIOR <- sgp_object@Data[SJ(ID, CONTENT_AREA, YEAR_INTEGER_TMP-1), mult="last"][,SCALE_SCORE]
			sgp_object@Data$YEAR_INTEGER_TMP <- NULL
		}

		### Key @Data for fast subsetting

		key(sgp_object@Data) <- c("YEAR", "CONTENT_AREA", "DISTRICT_NUMBER")

		### Get tmp.years, tmp.content_areas, and tmp.y.variable

		my.iters <- get.my.iters(sgp_object@Data, bubblePlot_LEVEL)

		### Start loops for bubblePlots

		for (year.iter in my.iters$tmp.years) {  ### Loop over year
		for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas
		for (district.iter in seq_along(my.iters$tmp.districts)) { ### Loop over districts (seq_along to get integer for anonymize)
		
		# Subset data

		tmp.bPlot.data.1 <- sgp_object@Data[J(year.iter, content_area.iter, my.iters$tmp.districts[district.iter])]

		tmp.unique.schools <- my.iters$tmp.schools[my.iters$tmp.schools %in% unique(tmp.bPlot.data.1$SCHOOL_NUMBER)]
		for (school.iter in seq_along(tmp.unique.schools)) { ### Loop over schools (seq_along to get integer for anonymize)

		# Subset data

		tmp.bPlot.data <- tmp.bPlot.data.1[SCHOOL_NUMBER==tmp.unique.schools[school.iter] & !is.na(SGP)]

		for (grade.iter in sort(unique(tmp.bPlot.data$GRADE))) { ### Loop over unique grades in school
		bPlot.data <- subset(tmp.bPlot.data, GRADE==grade.iter)

		if (dim(bPlot.data)[1] > 0) {

		for (y.variable.iter in my.iters$tmp.y.variable) {  ### Loop over CURRENT and PRIOR achievement (if requested)
		if (y.variable.iter=="SCALE_SCORE") tmp.y.bubble.label <- "ACHIEVEMENT_LEVEL"
		if (y.variable.iter=="SCALE_SCORE_PRIOR") tmp.y.bubble.label <- "ACHIEVEMENT_LEVEL_PRIOR"

		
		# Anonymize district, school and student names (if requested)

		if (bPlot.anonymize) {
			bPlot.data$FIRST_NAME <- "Student"; bPlot.data$LAST_NAME <- seq(dim(bPlot.data)[1])
			bPlot.data$SCHOOL_NAME <- paste("Sample School", school.iter); bPlot.data$DISTRICT_NAME <- paste("Sample District", district.iter)
		}

		# Create labels

		bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL)

		# Create cutscore ranges

		my.content_area <- get.my.cutscore.year(state, content_area.iter, as.character(bPlot.labels$y.year)) 
		if (y.variable.iter=="SCALE_SCORE") {
			tmp.y.ticks <- sort(c(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area.iter]][[paste("loss.hoss", grade.iter, sep="_")]],
				SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE", grade.iter, sep="_")]])) 
		}
		if (y.variable.iter=="SCALE_SCORE_PRIOR") {
			tmp.y.ticks <- sort(c(SGPstateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area.iter]][[paste("loss.hoss", grade.iter-1, sep="_")]],
				SGPstateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE", grade.iter-1, sep="_")]])) 
		}


		### Create bubblePlot ###

		bubblePlot(
			bubble_plot_data.X=bPlot.data$SGP,
			bubble_plot_data.Y=bPlot.data[[y.variable.iter]],
			bubble_plot_data.SUBSET=NULL,
			bubble_plot_data.INDICATE=NULL,
			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
			bubble_plot_data.SIZE=rep(50, length(bPlot.data$SGP)),
			bubble_plot_data.LEVELS=NULL, 
			bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(bPlot.data$SGP, " (", bPlot.data$SGP_TARGET, ")", sep=""),
				paste(bPlot.data[[tmp.y.bubble.label]], " (", bPlot.data[[y.variable.iter]], ")", sep="")),
			bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Student Growth Percentile")),
			bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
			bubble_plot_labels.SIZE=NULL,
			bubble_plot_labels.LEVELS=NULL, #levels(bubblePlot[["subset.factor"]]),
			bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Student Growth Percentile (Target)"),
				paste(bPlot.labels$y.year.label, " (", capwords(y.variable.iter), ")", sep="")),
			bubble_plot_labels.BUBBLE_TITLES=paste(bPlot.data$FIRST_NAME, bPlot.data$LAST_NAME),
			bubble_plot_titles.MAIN=bPlot.labels$main.title,
			bubble_plot_titles.SUB1=paste(bPlot.data$SCHOOL_NAME[1], "Student Performance"),
			bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, test.abbreviation.label, "Grade", grade.iter, capwords(content_area.iter)),
			bubble_plot_titles.LEGEND1="",
			bubble_plot_titles.LEGEND2_P1=NULL,
			bubble_plot_titles.LEGEND2_P2=NULL,

			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.07, 0.07),
			bubble_plot_configs.BUBBLE_X_TICKS=c(1, seq(10,90,10), 99),
			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.7, 5), 1, rep(0.7, 5)),
			bubble_plot_configs.BUBBLE_Y_TICKS=tmp.y.ticks,
			bubble_plot_configs.BUBBLE_Y_BANDS=tmp.y.ticks,
			bubble_plot_configs.BUBBLE_Y_BAND_LABELS=SGPstateData[[state]][["Achievement"]][["Levels"]][["Labels"]],
			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
			bubble_plot_configs.BUBBLE_COLOR="blue",
			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
			bubble_plot_configs.BUBBLE_TIPS="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
			bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
			bubble_plot_configs.BUBBLE_PLOT_LEGEND="FALSE",
			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=NULL,
			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(gsub(" ", "_", bPlot.data$SCHOOL_NAME[1]), "Grade", grade.iter, year.iter, capwords(content_area.iter), bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "Individual", "Style_100", gsub(" ", "_", bPlot.data$DISTRICT_NAME[1])),
			bubble_plot_pdftk.CREATE_CATALOG=FALSE)

		} ## END y.variable.iter loop
		} ## END if dim(bPlot.data)[1] > 0
		} ## END grade.iter loop
		} ## END school.iter loop
		} ## END district.iter loop
		} ## END content_area.iter loop
		} ## END year.iter loop

		message(paste("Finished bubblePlot Style 100", date(), "in", timetaken(started.at), "\n"))

} ## END if bubblePlot style 100


####
#### END bubblePlot_Styles
####

} ## END bubblePlot_Styles function
