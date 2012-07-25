`baselineSGP` <-
function(sgp_object,
	state,
	years,
	content_areas,
	grades,
	sgp.config,
	sgp.baseline.config,	
	sgp.percentiles.baseline.max.order=3,
	return.matrices.only=FALSE,
	calculate.baseline.sgps=TRUE,
	goodness.of.fit.print=TRUE,
	 ...) {

	started.at <- proc.time()
	message(paste("\tStarted baselineSGP", date(), "\n"))

	### Create state (if missing) from sgp_object (if possible)

	if (missing(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(x, tmp.name))==1)) {
			state <- c(state.abb, "DEMO", "AOB")[which(sapply(c(state.name, "Demonstration", "AOB"), function(x) regexpr(x, tmp.name))==1)]
		}
	}

        ### Syncronize "return.matrices.only" and "calculate.baseline.sgps" arguments

        if (return.matrices.only & calculate.baseline.sgps) {
                message("\tArgument 'return.matrices.only=TRUE' obviates need to calculate baseline student growth percentiles. Argument 'calculate.baseline.sgps' will be set to FALSE")
                calculate.baseline.sgps <- FALSE
        }


	############################################
	###
	### Utility Functions
	###
	############################################

        .mergeSGP <- function(list_1, list_2) {
                for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs")) {
                        list_1[[j]] <- c(list_1[[j]], list_2[[j]])[!duplicated(names(c(list_1[[j]], list_2[[j]])))]
                }
                for (j in c("SGPercentiles", "SGProjections", "Simulated_SGPs")) {
                        if (all(names(list_2[[j]]) %in% names(list_1[[j]]))) {
                                for (k in names(list_2[[j]])) { # merging list_2 in with list_1, so use it here
                                        if (!identical(list_1[[j]][[k]], list_2[[j]][[k]])) { # keeps it from copying first set of results
                                               list_1[[j]][[k]] <- rbind.fill(list_1[[j]][[k]], list_2[[j]][[k]])
                                        }
                                }
                        }
                }
                for (j in c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries")) {
                        for (k in names(list_1[[j]])) {
                                list_1[[j]][[k]] <- c(list_1[[j]][[k]], list_2[[j]][[k]])[!duplicated(names(c(list_1[[j]][[k]], list_2[[j]][[k]])))]
                        }
                }
        list_1
        }

	baselineSGP_Internal <- function(sgp_object, state, years, content_areas, grade.sequences) {

	        started.at <- proc.time()
		started.date <- date()

	        test.year.sequence <- function(years, grades) {
	                grade.span <- seq(min(grades), max(grades))
	                index <- match(grades,grade.span)
	                tmp.length <- length(grade.span)
	                tmp <- list()
	                for (i in 1:(length(years)-tmp.length+1)) {
	                        tmp[[i]] <- years[i:(i+tmp.length-1)][index]
	                }
	                tmp
	        }

	        ### Reshape data 

	        setkeyv(sgp_object@Data, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA"))
	        tmp.year.sequence <- test.year.sequence(years, grade.sequences)
	        tmp.list <- list()
	        for (k in seq_along(tmp.year.sequence)) {
	                tmp.lookup <- data.table(CJ("VALID_CASE", tmp.year.sequence[[k]]), grade.sequences, content_areas)
	                setnames(tmp.lookup, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA"))
			setkeyv(tmp.lookup, c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA"))
	                tmp.list[[k]] <- reshape(sgp_object@Data[tmp.lookup, nomatch=0],
	                        idvar="ID",
	                        timevar="GRADE",
	                        direction="wide",
	                        drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'SCALE_SCORE', 'GRADE')])
	                        tmp.list[[k]] <- as.data.frame(tmp.list[[k]][!apply(is.na(tmp.list[[k]]), 1, any)])
	        }
	        tmp.df <- rbind.fill(tmp.list)


	        ## Calculate Coefficient Matrices and return list containing coefficient matrix

	        tmp_sgp_list <- list(Coefficient_Matrices =
	                studentGrowthPercentiles(
	                panel.data=data.frame(tmp.df[,1], matrix(grade.sequences, nrow=1), tmp.df[,-1]),
	                sgp.labels=list(my.year="BASELINE", my.subject=content_areas),
	                use.my.knots.boundaries=state,
	                calculate.sgps=FALSE,
			goodness.of.fit=FALSE,
	                drop.nonsequential.grade.progression.variables=FALSE, # taken care of in data reshape above.
	                grade.progression=grade.sequences,
	                exact.grade.progression.sequence=TRUE,
			print.time.taken=FALSE)[["Coefficient_Matrices"]])

	        message(paste("\tStarted baselineSGP Coefficient Matrix Calculation:", started.date))
		message(paste("\tContent Area: ", content_areas, ", Grade Progression: ", paste(grade.sequences, collapse=", "), ". ", sep=""))
	        message(paste("\tFinished baselineSGP Coefficient Matrix Calculation ", date(), " in ", timetaken(started.at), ".\n", sep=""))

	        return(tmp_sgp_list)

	} ## END baselineSGP Function

        .get.config <- function(content_area, year, grades) {
                tmp.data <- sgp_object@Data[SJ("VALID_CASE", content_area), c("YEAR", "GRADE"), with=FALSE]
                tmp.unique.years <- sort(unique(tmp.data$YEAR))
                .sgp.panel.years <- tmp.unique.years[1:which(tmp.unique.years == year)]
                .sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
                tmp.sgp.grade.sequences <- lapply(grades[-1], function(x) tail(grades[grades <= x], length(tmp.unique.years)))
                .sgp.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1])
                list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences)
        }

        gof.print <- function(sgp_object) {
                if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
                        for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
                                dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE, showWarnings=FALSE)
                                        for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
                                                pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=4.5)
                                                grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
                                                dev.off()
                                        }
                                }
                } else {
                        message("\tNo Goodness of Fit tables available to print. No tables will be produced.")
                }
        }


	#################################################################################
	###
	### Calculate/retrieve baseline coefficient matrices if requested
	###
	#################################################################################

	if (is.null(SGPstateData[[state]][["Baseline_splineMatrix"]])) {
	        if (missing(sgp.baseline.config)) {
 	               sgp.baseline.config <- tmp.sgp.baseline.config <- .content_areas <- .years <- .grades <- .sgp.grade.sequences <- list()
	                       .content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]]) #tail(sgp.iter[["sgp.content.areas"]], 1)
                               .years <- sort(unique(sgp_object@Data[SJ("VALID_CASE", .content_areas)][["YEAR"]]))
                               .grades <- sort(unique(sgp_object@Data[SJ("VALID_CASE", .content_areas)][["GRADE"]]))
                               .baseline.max.order <- length(.years)-2
                               tmp.sgp.grade.sequences <- lapply(.grades[-1], function(x) tail(.grades[.grades <= x], (.baseline.max.order+1)))
                               tmp.sgp.baseline.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= .baseline.max.order])
                               sgp.baseline.grade.sequences <- list()
                               for (a in seq_along(tmp.sgp.baseline.grade.sequences)) {
                                       sgp.baseline.grade.sequences[[a]] <-
                                               eval(parse(text=paste("list(", paste("tail(tmp.sgp.baseline.grade.sequences[[", a, "]],", length(tmp.sgp.baseline.grade.sequences[[a]]):2, ")", collapse=", "), ")")))
                               }
                       sgp.baseline.grade.sequences <- unlist(sgp.baseline.grade.sequences, recursive=FALSE)

                       for (i in .content_areas) {
                                tmp.sgp.baseline.config[[i]] <- list(baseline.content.areas=i, baseline.panel.years=.years,
                                        baseline.grade.sequences=sgp.baseline.grade.sequences)
                       }

                       for (a in seq_along(tmp.sgp.baseline.config)) {
				tmp.length <- length(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]])
	                               for (b in seq(tmp.length)) {
	                                   sgp.baseline.config[[b+(a-1)*tmp.length]] <- tmp.sgp.baseline.config[[a]]
	                                   sgp.baseline.config[[b+(a-1)*tmp.length]][["baseline.grade.sequences"]] <- unlist(tmp.sgp.baseline.config[[a]][["baseline.grade.sequences"]][b])
	                        }
	               }
                } else {
                       if (!all(sapply(lapply(sgp.baseline.config, names), 
                           function(x) identical(x, c("baseline.content.areas", "baseline.panel.years", "baseline.grade.sequences"))))) {
                                  stop("Please specify an appropriate list of SGP function labels (sgp.baseline.config).  See help page for details.")
                        }
                }

                tmp <- list()
                for (sgp.iter in seq_along(sgp.baseline.config)) {
                        tmp[[sgp.iter]] <- baselineSGP_Internal(
                                sgp_object,
                                state=state,
                                years=sgp.baseline.config[[sgp.iter]][["baseline.panel.years"]],
                                content_areas=sgp.baseline.config[[sgp.iter]][["baseline.content.areas"]],
                                grade.sequences=sgp.baseline.config[[sgp.iter]][["baseline.grade.sequences"]])
                }
                for (s in seq_along(tmp)) sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
	} else {
                sgp_object@SGP <- .mergeSGP(sgp_object@SGP, SGPstateData[[state]][["Baseline_splineMatrix"]])
        }


	################################################################
	###
	### Calculate baseline referenced student growth percentiles
	###
	################################################################

	if (calculate.baseline.sgps) {

	        ## If missing sgp.config then determine year(s), content_area(s), and grade(s) if not explicitely provided

	        if (missing(sgp.config)) {
	                sgp.config <- tmp.years <- tmp.grades <- list()
	                if (missing(content_areas)) {
	                        content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
	                }
	                if (missing(years)) {
	                        for (i in content_areas) {
	                                tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[SJ("VALID_CASE", i)][["YEAR"]]), -2), decreasing=TRUE)
	                        }
	                } else {
	                        for (i in content_areas) {
	                                tmp.years[[i]] <- years
	                        }
	                }
	                if (missing(grades)) {
	                        for (i in content_areas) {
	                                for (j in tmp.years[[i]]) {
	                                        tmp.grades[[paste(i,j,sep=".")]] <- sort(unique(sgp_object@Data[SJ("VALID_CASE", i, j)][["GRADE"]]))
	                                }
	                        }
	                } else {
	                        for (i in content_areas) {
	                                for (j in tmp.years[[i]]) {
	                                        tmp.grades[[paste(i,j,sep=".")]] <- grades
	                                }
	                        }
	                }
	                for (i in content_areas) {
	                        for (j in tmp.years[[i]]) {
	                                sgp.config[[paste(i,j,sep=".")]] <- .get.config(i,j,tmp.grades[[paste(i,j,sep=".")]])
	                        }
	                }
	        } ## END if (missing(sgp.config))

                tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]])

                for (sgp.iter in sgp.config) {
                        tmp_sgp_object[["Panel_Data"]] <-
                        as.data.frame(reshape(sgp_object@Data[SJ("VALID_CASE", sgp.iter[["sgp.content.areas"]], sgp.iter[["sgp.panel.years"]])],
                                idvar="ID",
                                timevar="YEAR",
                                drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
                                direction="wide"))
                        suppressMessages(gc())

	                sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."),
		                paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))

	                tmp_sgp_object <- .mergeSGP(tmp_sgp_object, SGPstateData[[state]][["Baseline_splineMatrix"]])

	                mtx.names <- names(tmp_sgp_object[["Coefficient_Matrices"]][[paste(toupper(tail(sgp.iter[["sgp.content.areas"]], 1)), ".BASELINE", sep="")]])

                        for (k in sgp.iter[["sgp.grade.sequences"]]) {
	                        max.order.mtx <- mtx.names[[max(grep(paste("qrmatrix_", tail(k, 1), sep=""), mtx.names))]]
                                max.order <- as.numeric(strsplit(max.order.mtx, "_")[[1]][3])

                                if (length(sgp.iter[["sgp.panel.years"]])-1 < max.order) max.order <- length(sgp.iter[["sgp.panel.years"]])-1

                                if (any(diff(tail(k, 1+max.order)) > 1)) {      # deals with 'holes'
	                                base.gp <- k[tail(k, 1)-k <= max.order]
                                        max.order <- length(base.gp) - 1
                                }       else base.gp <- tail(k, 1+max.order)

                                tmp_sgp_object <- studentGrowthPercentiles(
	                                panel.data=tmp_sgp_object,
                                        sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1),
	                                        my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
                                         use.my.knots.boundaries=state,
                                         use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
                                         growth.levels=state,
                                         panel.data.vnames=sgp.vnames,
                                         grade.progression=base.gp,
                                         num.prior=min(max.order, sgp.percentiles.baseline.max.order),
                                         goodness.of.fit=TRUE,
                                         ...)
                        } ## END k loop
		} ## END sgp.iter loop
                sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp_sgp_object)

	        if (goodness.of.fit.print) gof.print(sgp_object)

	} ## END if (calculate.baseline.sgps)

	
	############################################################
	###
	### Return results
	###
	############################################################

	message(paste("\tFinished baselineSGP", date(), "in", timetaken(started.at), "\n"))
	if (return.matrices.only) {
		tmp.list <- list()
                for (ca in unique(sapply(sgp.baseline.config, function(x) x[["baseline.content.areas"]]))) {
                        tmp.list[[paste(ca, ".BASELINE", sep="")]] <- sgp_object@SGP[["Coefficient_Matrices"]][[paste(ca, ".BASELINE", sep="")]]
                }
		return(tmp.list)
	} else {
		return(sgp_object)
	}
} ## END baselineSGP Function
