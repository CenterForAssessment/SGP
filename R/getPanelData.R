`getPanelData` <-
function(sgp.data,
	sgp.type,
	sgp.iter,
	sgp.csem=NULL,
	sgp.scale.score.equated=NULL,
	sgp.targets=NULL,
	SGPt=NULL,
	fix.duplicates=NULL) {

	YEAR <- CONTENT_AREA <- VALID_CASE <- V3 <- V5 <- ID <- GRADE <- SCALE_SCORE <-
	YEAR_WITHIN <- tmp.timevar <- FIRST_OBSERVATION <- LAST_OBSERVATION <- 
	ACHIEVEMENT_LEVEL <- DATE <- SGP_PROJECTION_GROUP_SCALE_SCORES <- DUPS_FLAG <- NULL

	arrow.tf <- is(sgp.data, "ArrowObject") | is(sgp.data, "arrow_dplyr_query")
	var.names <- names(sgp.data)

	if ("STATE" %in% var.names) state <- "STATE" else state <- NULL


	###
	### sgp.percentiles
	###

	if (sgp.type=="sgp.percentiles") {
		if (!is.null(tmp.exclude.lookup <- sgp.iter$sgp.exclude.sequences)) {
			if (is.data.table(tmp.exclude.lookup)) {
				tmp.exclude.lookup <-
				    tmp.exclude.lookup[, list(VALID_CASE, CONTENT_AREA, YEAR, GRADE)] |>
				        setkey()
			} else {
				stop("Element 'sgp.exclude.sequences' of sgp.config must be a data table with variables 'VALID_CASE', 'CONTENT_AREA', 'YEAR', and 'GRADE'.")
			}
            if (arrow.tf) {
                tmp.exclude.lookup <- arrow::arrow_table(tmp.exclude.lookup)
                tmp.exclude.ids <-
                    sgp.data |>
                        dplyr::semi_join(tmp.exclude.lookup) |>
                        dplyr::pull(ID, as_vector = TRUE) |>
                        unique()
                # tmp.exclude.ids <- NULL
                # for (y in seq.int(nrow(tmp.exclude.lookup))) {
                #     tmp.exclude.ids <-
                #         c(tmp.exclude.ids, 
                #           sgp.data |>
                #             dplyr::filter(
                #                 VALID_CASE == "VALID_CASE",
                #                 CONTENT_AREA == tmp.exclude.lookup[y]$CONTENT_AREA,
                #                 YEAR == tmp.exclude.lookup[y]$YEAR,
                #                 GRADE == tmp.exclude.lookup[y]$GRADE
                #             )  |>
                #         dplyr::pull(ID, as_vector = TRUE)
                #     )
                # }
                # tmp.exclude.ids <- unique(tmp.exclude.ids)
			} else tmp.exclude.ids <- unique(sgp.data[tmp.exclude.lookup][['ID']])
		} else tmp.exclude.ids <- as.character(NULL)

        if ("YEAR_WITHIN" %in% var.names) {
            tmp.lookup <-
                data.table(
                    VALID_CASE = "VALID_CASE",
                    CONTENT_AREA = 
                      tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]])),
                    YEAR = 
                      tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]])),
                    GRADE = sgp.iter[["sgp.grade.sequences"]],
                    YW = 
                      tail(sgp.iter[["sgp.panel.years.within"]], length(sgp.iter[["sgp.grade.sequences"]])),
                    FIRST_OBSERVATION = as.integer(NA),
                    LAST_OBSERVATION = as.integer(NA)
                )
            tmp.lookup[grep("FIRST", YW, ignore.case=TRUE), FIRST_OBSERVATION:=1L]
            tmp.lookup[grep("LAST", YW, ignore.case=TRUE), LAST_OBSERVATION:=1L]
            tmp.lookup[, YW := NULL]

            tmp.lookup.list <- list()
            for (i in unique(sgp.iter[["sgp.panel.years.within"]])) {
                if (arrow.tf) {
                    tmp_ids <- arrow::arrow_table(ID = tmp.exclude.ids)
                    tmp.lookup.i <- arrow::arrow_table(tmp.lookup[get(i)==1])
                    #     arrow::arrow_table(
                    #         tmp.lookup[get(i)==1
                    #         ][, c("FIRST_OBSERVATION", "LAST_OBSERVATION") %w/o% i := NULL]
                    #     )
                    # tmp.lookup.i <-
                    #     tmp.lookup.i$cast(
                    #         target_schema =
                    #             sgp.data$schema[
                    #                 c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i)
                    #             ]
                    #     )
                    tmp.vars.to.get <-
                        c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "YEAR_WITHIN",
                          "ID", "SCALE_SCORE", i, sgp.csem, sgp.scale.score.equated)
                    tmp.vars.to.keep <-
                        c("ID", "GRADE", "SCALE_SCORE", "YEAR_WITHIN", "tmp.timevar",
                          sgp.csem, sgp.scale.score.equated)

                    tmp.lookup.list[[i]] <- sgp.data |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.get)) |>
                        dplyr::semi_join(tmp.lookup.i) |>
                        dplyr::anti_join(tmp_ids) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, i, sep = ".")) |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.keep)) |>
                        data.table::as.data.table()
                } else {
					setkeyv(sgp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
					setkeyv(tmp.lookup, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
					suppressWarnings(tmp.lookup.list[[i]] <- data.table(sgp.data[tmp.lookup[get(i)==1], nomatch=0][!ID %in% tmp.exclude.ids][,
							'tmp.timevar':=paste(YEAR, CONTENT_AREA, i, sep=".")][,
						c("ID", "GRADE", "SCALE_SCORE", "YEAR_WITHIN", "tmp.timevar", sgp.csem, sgp.scale.score.equated), with=FALSE], key="ID")) ### Could be NULL and result in a warning
				}
			}

			if (tail(sgp.iter[['sgp.panel.years']], 1L) ==
			    head(tail(sgp.iter[['sgp.panel.years']], 2L), 1L)
			) {
				setkey(tmp.lookup.list[[1L]], ID)
				setkey(tmp.lookup.list[[2L]], ID)
				tmp.ids <- intersect(tmp.lookup.list[[1L]][['ID']], tmp.lookup.list[[2L]][['ID']])
				tmp.ids <-
				    tmp.ids[
						tmp.lookup.list[[1L]][tmp.ids][['YEAR_WITHIN']] <
					    tmp.lookup.list[[2L]][tmp.ids][['YEAR_WITHIN']]
					]
				tmp.lookup.list <- lapply(tmp.lookup.list, function(x) x[tmp.ids])
			}
			return(ddcast(rbindlist(tmp.lookup.list),
					ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", "YEAR_WITHIN", sgp.csem, sgp.scale.score.equated), sep="."))
		} else {
			tmp.lookup <- SJ("VALID_CASE", tail(sgp.iter[["sgp.content.areas"]], length(sgp.iter[["sgp.grade.sequences"]])),
				tail(sgp.iter[["sgp.panel.years"]], length(sgp.iter[["sgp.grade.sequences"]])), sgp.iter[["sgp.grade.sequences"]])
			# ensure lookup table is ordered by years.  NULL out key after sorted so that it doesn't corrupt the join in ddcast.
			setkey(tmp.lookup, V3)
			setkey(tmp.lookup, NULL)

            if (arrow.tf) {
                setnames(tmp.lookup, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
                tmp.lookup <- arrow::arrow_table(tmp.lookup)
                tmp_ids <- arrow::arrow_table(ID = tmp.exclude.ids)
                tmp.vars.to.get <-
                    c(getKey(sgp.data), "SCALE_SCORE", sgp.csem, sgp.scale.score.equated)

                tmp.data <- sgp.data |>
                    dplyr::select(dplyr::all_of(tmp.vars.to.get)) |>
                    dplyr::semi_join(tmp.lookup) |>
                    # dplyr::filter(!ID %in% tmp.exclude.ids) |>
                    dplyr::anti_join(tmp_ids) |>
                    dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                    data.table::as.data.table() |>
                    data.table::setkeyv(getKey(sgp.data))
                    # |> # future dev - deeper integration of `arrow` into lower-lev functions
                    #     arrow::arrow_table() 


				if (is.character(fix.duplicates)) {
					if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")

					if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
						invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
						tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
					}

					if (is.null(SGPt)) {
						return(ddcast(tmp.data,
								ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated), sep="."))
					} else {
						return(ddcast(tmp.data[, c("TIME", "TIME_LAG"):=list(as.numeric(DATE), as.numeric(DATE-c(NA, DATE[-.N]))), by=ID],
								ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated, "TIME", "TIME_LAG"), sep="."))
					}
				} ### END if (is.character(fix.duplicates)

				if (is.null(SGPt)) {
                    return(
                        ddcast(
                            tmp.data,
                            ID ~ tmp.timevar,
                            value.var = c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated),
                            sep = "."
                    ))
				} else {
					return(ddcast(tmp.data[,
							c("TIME", "TIME_LAG"):=list(as.numeric(DATE), as.numeric(DATE-c(NA, DATE[-.N]))), by=ID],
							ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated, "TIME", "TIME_LAG"), sep="."))
				}
			} else {
				if (is.character(fix.duplicates)) {
					if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
					tmp.data <- sgp.data[tmp.lookup, nomatch=0][!ID %in% tmp.exclude.ids][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")]

					if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
						invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
						tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
					}

					if (is.null(SGPt)) {
						return(ddcast(tmp.data,
								ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated), sep="."))
					} else {
						return(ddcast(tmp.data[, c("TIME", "TIME_LAG"):=list(as.numeric(DATE), as.numeric(DATE-c(NA, DATE[-.N]))), by=ID],
								ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated, "TIME", "TIME_LAG"), sep="."))
					}
				} ### END if (is.character(fix.duplicates)

				if (is.null(SGPt)) {
					return(ddcast(sgp.data[tmp.lookup][!ID %in% tmp.exclude.ids][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")],
							ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated), sep="."))
				} else {
					return(ddcast(sgp.data[tmp.lookup][!ID %in% tmp.exclude.ids][,
							'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")][,
							c("TIME", "TIME_LAG"):=list(as.numeric(DATE), as.numeric(DATE-c(NA, DATE[-.N]))), by=ID],
							ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", sgp.csem, sgp.scale.score.equated, "TIME", "TIME_LAG"), sep="."))
				}
			}
		}
	} ### END if (sgp.type=="sgp.percentiles")


	###
	### sgp.projections & sgp.projections.baseline
	###

	if (sgp.type %in% c("sgp.projections", "sgp.projections.baseline")) {

		if (sgp.type=="sgp.projections") {
			sgp.projection.content.areas.label <- "sgp.projection.content.areas"
			sgp.projection.grade.sequences.label <- "sgp.projection.grade.sequences"
			sgp.projection.panel.years.label <- "sgp.projection.panel.years"
		} else {
			sgp.projection.content.areas.label <- "sgp.projection.baseline.content.areas"
			sgp.projection.grade.sequences.label <- "sgp.projection.baseline.grade.sequences"
			sgp.projection.panel.years.label <- "sgp.projection.baseline.panel.years"
		}

		if ("YEAR_WITHIN" %in% var.names) {
			tmp.lookup <- data.table(V1="VALID_CASE", tail(sgp.iter[[sgp.projection.content.areas.label]], length(sgp.iter[[sgp.projection.grade.sequences.label]])),
				sapply(head(sgp.iter[["sgp.panel.years"]], length(sgp.iter[[sgp.projection.grade.sequences.label]])), yearIncrement, tail(sgp.iter$sgp.panel.years.lags, 1)),
				sgp.iter[[sgp.projection.grade.sequences.label]], head(sgp.iter[["sgp.panel.years.within"]], length(sgp.iter[[sgp.projection.grade.sequences.label]])),
				FIRST_OBSERVATION=as.integer(NA), LAST_OBSERVATION=as.integer(NA))
			tmp.lookup[grep("FIRST", V5, ignore.case=TRUE), FIRST_OBSERVATION:=1L]; tmp.lookup[grep("LAST", V5, ignore.case=TRUE), LAST_OBSERVATION:=1L]; tmp.lookup[,V5:=NULL]
			setnames(tmp.lookup, paste0("V", seq.int(4)), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))

			tmp.lookup.list <- list()
			for (i in unique(sgp.iter[["sgp.panel.years.within"]])) {
                if (arrow.tf) {
                    tmp.lookup.i <- arrow::arrow_table(tmp.lookup[get(i)==1])
                    
                    tmp.vars.to.get <-
                        c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "YEAR_WITHIN",
                          "ID", "SCALE_SCORE", i, sgp.scale.score.equated)
                    tmp.vars.to.keep <-
                        c("ID", "GRADE", "SCALE_SCORE", "YEAR_WITHIN", "tmp.timevar",
                          sgp.scale.score.equated)

                    tmp.lookup.list[[i]] <- sgp.data |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.get)) |>
                        dplyr::semi_join(tmp.lookup.i) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, i, sep = ".")) |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.keep)) |>
                        data.table::as.data.table()

                } else {
					setkeyv(sgp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
					setkeyv(tmp.lookup, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
					suppressWarnings(tmp.lookup.list[[i]] <- data.table(sgp.data[tmp.lookup[get(i)==1], nomatch=0][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, i, sep=".")][,
						c("ID", "GRADE", "SCALE_SCORE", "YEAR_WITHIN", "tmp.timevar", sgp.scale.score.equated), with=FALSE], key="ID")) ### Could be NULL and result in a warning
				}
			}

			if (tail(sgp.iter[['sgp.panel.years']], 1L)==head(tail(sgp.iter[['sgp.panel.years']], 2L), 1L)) {
				tmp.ids <- intersect(tmp.lookup.list[[1L]][['ID']], tmp.lookup.list[[2L]][['ID']])
				tmp.ids <- tmp.ids[tmp.lookup.list[[1L]][tmp.ids][['YEAR_WITHIN']] < tmp.lookup.list[[2L]][tmp.ids][['YEAR_WITHIN']]]
				tmp.lookup.list <- lapply(tmp.lookup.list, function(x) x[tmp.ids])
			}
			if (is.null(sgp.targets)) {
				tmp.data <- ddcast(rbindlist(tmp.lookup.list),
						ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", "YEAR_WITHIN", state, sgp.scale.score.equated, SGPt), sep=".")
				if (dim(tmp.data)[1] > 0) {
					setnames(tmp.data, tail(sort(grep("YEAR_WITHIN", names(tmp.data), value=TRUE)), 1L), "YEAR_WITHIN")
					if (length(setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN")) > 0) {
						tmp.data[,setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN"):=NULL]
					}
					if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
						setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
						if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) {
							tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
						}
					}
				}																																																
				return(tmp.data)
			} else {
				tmp.data <- ddcast(rbindlist(tmp.lookup.list),
					ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", "YEAR_WITHIN", state, sgp.scale.score.equated, SGPt), sep=".")[
					sgp.targets[CONTENT_AREA==tail(sgp.iter[[sgp.projection.content.areas.label]], 1L) & YEAR==tail(sgp.iter[["sgp.panel.years"]], 1L) & GRADE==tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)], nomatch=0][,
					!c("CONTENT_AREA", "YEAR"), with=FALSE]
				if (dim(tmp.data)[1] > 0) {
					setnames(tmp.data, tail(sort(grep("YEAR_WITHIN", names(tmp.data), value=TRUE)), 1L), "YEAR_WITHIN")
					if (length(setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN")) > 0L) {
						tmp.data[,setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN"):=NULL]
					}
					if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
						setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
						if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) {
							tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
						}
					}
				}
				return(tmp.data)
			}
		} ### END if ("YEAR_WITHIN" %in% var.names)

		tmp.lookup <- SJ("VALID_CASE", tail(sgp.iter[[sgp.projection.content.areas.label]], length(sgp.iter[[sgp.projection.grade.sequences.label]])),
			tail(sgp.iter[[sgp.projection.panel.years.label]], length(sgp.iter[[sgp.projection.grade.sequences.label]])), sgp.iter[[sgp.projection.grade.sequences.label]])
		# ensure lookup table is ordered by years.  NULL out key after sorted so that it doesn't corrupt the join in ddcast.
		setkey(tmp.lookup, V3)
		setkey(tmp.lookup, NULL)

		if (is.null(sgp.targets)) {
            if (arrow.tf) {
                setnames(tmp.lookup, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
                tmp.lookup <- arrow::arrow_table(tmp.lookup)

                if (is.character(fix.duplicates)) {
                    if (fix.duplicates!="KEEP.ALL") {
                        stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
                    }
                    tmp.data <- sgp.data |>
                        dplyr::select(dplyr::all_of(c(getKey(sgp.data), "SCALE_SCORE"))) |>
                        dplyr::semi_join(tmp.lookup) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv(getKey(sgp.data))

                    if (any(duplicated(tmp.data, by = getKey(tmp.data)))) {
                        invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
                        tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
                    }
                    tmp.data <-
                        ddcast(tmp.data,
                            formula = ID ~ tmp.timevar,
                            value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt),
                            sep="."
                        )
                } else {### END if (is.character(fix.duplicates)
                    tmp.data <- sgp.data |>
                        dplyr::select(dplyr::all_of(c(getKey(sgp.data), "SCALE_SCORE"))) |>
                        dplyr::semi_join(tmp.lookup) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv(getKey(sgp.data)) |>
                        ddcast(
                            formula = ID ~ tmp.timevar,
                            value.var = c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt),
                            sep = "."
                        )
                }
			} else {
				if (is.character(fix.duplicates)) {
					if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
					tmp.data <- sgp.data[tmp.lookup, nomatch=0][, 'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")]

					if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
						invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
						tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
					}
					tmp.data <- ddcast(tmp.data,
						ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")
				} else {### END if (is.character(fix.duplicates)
					tmp.data <- ddcast(sgp.data[tmp.lookup, nomatch=0][, 'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")],
						ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")
				}
			}

			if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
				setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
				if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
			}
			return(tmp.data)
		} else {  ###  END is.null(sgp.targets)
			if (arrow.tf) {
                setnames(tmp.lookup, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
                tmp.lookup <- arrow::arrow_table(tmp.lookup)

				if (is.character(fix.duplicates)) {
					if (fix.duplicates!="KEEP.ALL") {
						stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
					}
                    tmp.data <- sgp.data |>
                        dplyr::select(dplyr::all_of(c(getKey(sgp.data), "SCALE_SCORE"))) |>
                        dplyr::semi_join(tmp.lookup) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv(getKey(sgp.data))

					if (any(duplicated(tmp.data, by = getKey(tmp.data)))) {
						invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
						tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
					}
					tmp.data <- ddcast(
							tmp.data,
							ID ~ tmp.timevar,
							value.var = c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt),
							sep="."
						)[
							sgp.targets[
								CONTENT_AREA == tail(sgp.iter[[sgp.projection.content.areas.label]], 1L) &
								YEAR == tail(sgp.iter[[sgp.projection.panel.years.label]], 1L) &
								GRADE == tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)
							], nomatch = 0
						][, !c("CONTENT_AREA", "YEAR"), with = FALSE]
				} else {  ###  END if (is.character(fix.duplicates)
                    tmp.data <- sgp.data |>
                        dplyr::select(dplyr::all_of(c(getKey(sgp.data), "SCALE_SCORE"))) |>
                        dplyr::semi_join(tmp.lookup) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv(getKey(sgp.data)) |>
                        ddcast(
                            formula = ID ~ tmp.timevar,
                            value.var = c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt),
                            sep = "."
                        )
                tmp.data <-
                    tmp.data[
                        sgp.targets[
                            CONTENT_AREA == tail(sgp.iter[[sgp.projection.content.areas.label]], 1L) &
                            YEAR == tail(sgp.iter[[sgp.projection.panel.years.label]], 1L) &
                            GRADE == tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)
                        ], nomatch=0
                    ][, !c("CONTENT_AREA", "YEAR"), with=FALSE]
                }
            } else {
				if (is.character(fix.duplicates)) {
					if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
					setnames(tmp.lookup, paste0("V", seq.int(4)), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
					setkeyv(sgp.data, names(tmp.lookup))
					tmp.data <- sgp.data[tmp.lookup, nomatch=0][, 'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")]

					if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
						tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]

						##  Create SCALE_SCORE history vars to merge on
						tmp.data <- ddcast(tmp.data, ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")[,
							GRADE := tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)]
						jExpression <- paste0("paste(", paste(grep("SCALE_SCORE[.]", names(tmp.data), value=TRUE) , collapse = ", "), ", sep = '; ')")
						invisible(tmp.data[, SGP_PROJECTION_GROUP_SCALE_SCORES := eval(parse(text=jExpression))])
						invisible(tmp.data[, SGP_PROJECTION_GROUP_SCALE_SCORES := gsub("NA; ", "", SGP_PROJECTION_GROUP_SCALE_SCORES)])
						invisible(tmp.data[grepl("_DUPS_[0-9]*", ID), DUPS_FLAG := gsub(".*_DUPS_", "", ID)])
						invisible(tmp.data[, ID := gsub("_DUPS_[0-9]*", "", ID)])

						tmp.data <- tmp.data[sgp.targets[CONTENT_AREA==tail(sgp.iter[[sgp.projection.content.areas.label]], 1L) & YEAR==tail(sgp.iter[[sgp.projection.panel.years.label]], 1L) & GRADE==tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)],
							on=c("ID", "GRADE", "SGP_PROJECTION_GROUP_SCALE_SCORES"), nomatch=0][,
							!c("CONTENT_AREA", "YEAR", "GRADE", "SGP_PROJECTION_GROUP_SCALE_SCORES"), with=FALSE]

						invisible(tmp.data[!is.na(DUPS_FLAG), ID := paste0(ID, "_DUPS_", DUPS_FLAG)])
						invisible(tmp.data[, DUPS_FLAG := NULL])
					} else {  ###  END if (any(duplicated(tmp.data, ...)))
						if (all(is.na(sgp.targets[["GRADE"]]))) {
							sgp.targets[, GRADE := NULL]
							tmp.merge.vars <- "ID"
						} else tmp.merge.vars <- c("ID", "GRADE")
						tmp.data <- ddcast(tmp.data, ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")[, GRADE := tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)][
							sgp.targets[CONTENT_AREA==tail(sgp.iter[[sgp.projection.content.areas.label]], 1L) & YEAR==tail(sgp.iter[[sgp.projection.panel.years.label]], 1L) & GRADE==tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)],
								on=tmp.merge.vars, nomatch=0][,!c("CONTENT_AREA", "YEAR", "GRADE"), with=FALSE]
					}
				} else { ### END if (is.character(fix.duplicates)
					setnames(tmp.lookup, paste0("V", seq.int(4)), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
					setkeyv(sgp.data, names(tmp.lookup))
					setkeyv(sgp.targets, intersect(names(sgp.targets), getKey(sgp.data)))
					tmp.data <- ddcast(sgp.data[tmp.lookup, nomatch=0][, 'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")],
						ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")[
							sgp.targets[CONTENT_AREA==tail(sgp.iter[[sgp.projection.content.areas.label]], 1L) &
							YEAR==tail(sgp.iter[[sgp.projection.panel.years.label]], 1L) & GRADE==tail(sgp.iter[[sgp.projection.grade.sequences.label]], 1L)],
							on="ID", nomatch=0][,!c("CONTENT_AREA", "YEAR"), with=FALSE]
				}
			}

			if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
				setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
				if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
			}
			return(tmp.data)
		}
	} ### END if (sgp.type=="sgp.projections")


	###
	### sgp.projections.lagged
	###

	if (sgp.type %in% c("sgp.projections.lagged", "sgp.projections.lagged.baseline")) {

		if (sgp.type=="sgp.projections.lagged") {
			sgp.projection.content.areas.label <- "sgp.projection.content.areas"
			sgp.projection.grade.sequences.label <- "sgp.projection.grade.sequences"
			sgp.projection.panel.years.label <- "sgp.projection.panel.years"
		} else {
			sgp.projection.content.areas.label <- "sgp.projection.baseline.content.areas"
			sgp.projection.grade.sequences.label <- "sgp.projection.baseline.grade.sequences"
			sgp.projection.panel.years.label <- "sgp.projection.baseline.panel.years"
		}

        if ("YEAR_WITHIN" %in% var.names) {
			tmp.lookup <-
			    data.table(
					V1 = "VALID_CASE",
					tail(sgp.iter[[sgp.projection.content.areas.label]], 
						length(sgp.iter[[sgp.projection.grade.sequences.label]])),
					head(sgp.iter[["sgp.panel.years"]],
						length(sgp.iter[[sgp.projection.grade.sequences.label]])),
					sgp.iter[[sgp.projection.grade.sequences.label]],
					head(sgp.iter[["sgp.panel.years.within"]],
						length(sgp.iter[[sgp.projection.grade.sequences.label]])),
					FIRST_OBSERVATION = as.integer(NA),
					LAST_OBSERVATION = as.integer(NA)
				)
			tmp.lookup[grep("FIRST", V5, ignore.case=TRUE), FIRST_OBSERVATION := 1L]
			tmp.lookup[grep("LAST", V5, ignore.case=TRUE), LAST_OBSERVATION := 1L]
			tmp.lookup[, V5 := NULL]
			setnames(tmp.lookup,
			         paste0("V", seq.int(4)),
					 c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")
			)

			tmp.lookup.list <- list()
			for (i in unique(sgp.iter[["sgp.panel.years.within"]])) {
                if (arrow.tf) {
                    tmp.lookup.i <- arrow::arrow_table(tmp.lookup[get(i)==1])
                    tmp.vars.to.get <-
                        c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "YEAR_WITHIN", "ID",
                          "SCALE_SCORE", "ACHIEVEMENT_LEVEL", i, sgp.scale.score.equated)
                    tmp.vars.to.keep <-
                        c("ID", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN",
                          "tmp.timevar", sgp.scale.score.equated)

                    tmp.lookup.list[[i]] <- sgp.data |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.get)) |>
                        dplyr::semi_join(tmp.lookup.i) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, i, sep = ".")) |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.keep)) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv("ID")
                } else {
					setkeyv(sgp.data, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
					setkeyv(tmp.lookup, c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", i))
					suppressWarnings(tmp.lookup.list[[i]] <- data.table(sgp.data[tmp.lookup[get(i)==1], nomatch=0][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, i, sep=".")][,
						c("ID", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", "tmp.timevar", sgp.scale.score.equated), with=FALSE], key="ID")) ### Could be NULL and result in a warning
				}
			}

			achievement.level.prior.vname <- paste("ACHIEVEMENT_LEVEL", tail(head(sgp.iter[["sgp.panel.years"]], -1L), 1L), tail(head(sgp.iter[["sgp.content.areas"]], -1L), 1L), sep=".")
			if (is.null(sgp.targets)) {
				tmp.data <-
				    ddcast(
						rbindlist(tmp.lookup.list),
						ID ~ tmp.timevar,
						value.var = 
						    c("GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", state, sgp.scale.score.equated, SGPt),
						sep="."
					)
				setnames(tmp.data,
				    names(tmp.data)[grep(achievement.level.prior.vname, names(tmp.data))],
					achievement.level.prior.vname
				)
				setnames(tmp.data,
				    tail(sort(grep("YEAR_WITHIN", names(tmp.data), value=TRUE)), 1L),
					"YEAR_WITHIN"
				)
				if (length(setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN")) > 0L) {
					tmp.data[,setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN"):=NULL]
				}

				if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
					setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
					if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) {
						tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
					}
				}
				return(tmp.data)
			} else {
				tmp.data <- ddcast(rbindlist(tmp.lookup.list),
					ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "YEAR_WITHIN", state, sgp.scale.score.equated, SGPt), sep=".")[
						sgp.targets[
							CONTENT_AREA==tail(sgp.iter[["sgp.content.areas"]], 1L) &
							YEAR==tail(sgp.iter[["sgp.panel.years"]], 1L) &
							GRADE==tail(sgp.iter[["sgp.grade.sequences"]], 1L)
						], nomatch=0][,!c("CONTENT_AREA", "YEAR"), with=FALSE]

				setnames(tmp.data,
				         names(tmp.data)[grep(achievement.level.prior.vname, names(tmp.data))],
						 achievement.level.prior.vname
				)
				setnames(tmp.data,
				         tail(sort(grep("YEAR_WITHIN", names(tmp.data), value=TRUE)), 1L),
						 "YEAR_WITHIN"
				)

				if (length(setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN")) > 0L) {
					tmp.data[,setdiff(grep("YEAR_WITHIN", names(tmp.data), value=TRUE), "YEAR_WITHIN"):=NULL]
				}

				if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
					setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
					if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) {
						tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
					}
				}
				return(tmp.data)
			}
		} else {  ###  END "YEAR_WITHIN" %in% var.names
			if (is.null(sgp.targets)) {
                if (arrow.tf) {
                    prior_lookup <- arrow::arrow_table(
                        VALID_CASE = "VALID_CASE",
                        CONTENT_AREA = sgp.iter[[sgp.projection.content.areas.label]],
                        YEAR = tail(
                            head(sgp.iter[["sgp.panel.years"]], -1L),
                            length(sgp.iter[[sgp.projection.grade.sequences.label]])
                        ),
                        GRADE = sgp.iter[[sgp.projection.grade.sequences.label]]
                    )
                    prior_lookup <-
                        prior_lookup$cast(
                            target_schema =
                                sgp.data$schema[c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")]
                        )
                    tmp_ids <- sgp.data |>
                        dplyr::filter(
                            VALID_CASE == "VALID_CASE",
                            CONTENT_AREA == tail(sgp.iter[["sgp.content.areas"]], 1L),
                            YEAR == tail(sgp.iter[["sgp.panel.years"]], 1L),
                            GRADE == tail(sgp.iter[["sgp.grade.sequences"]], 1L)
                        )  |>
                        dplyr::select(ID)
                    
                    tmp.vars.to.get <- c(getKey(sgp.data), "SCALE_SCORE", "ACHIEVEMENT_LEVEL")

                    tmp.data <- sgp.data |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.get)) |>
                        dplyr::semi_join(prior_lookup) |>
                        dplyr::semi_join(tmp_ids) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv(getKey(sgp.data))

                    if (is.character(fix.duplicates)) {
                        if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
                        if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
                            invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
                            tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
                        }
                    }
                    tmp.data <-
                        ddcast(
                            tmp.dt = tmp.data,
                            formula = ID ~ tmp.timevar,
                            value.var = c(
                                "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL",
                                state, sgp.scale.score.equated, SGPt),
                            sep = "."
                        )
                } else {
					if (is.character(fix.duplicates)) {
						if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")

						tmp.data <- data.table(
								data.table(sgp.data, key="ID")[
									sgp.data[SJ("VALID_CASE",
									tail(sgp.iter[["sgp.content.areas"]], 1L),
									tail(sgp.iter[["sgp.panel.years"]], 1L),
									tail(sgp.iter[["sgp.grade.sequences"]], 1L))][,"ID", with=FALSE]], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[
								  SJ("VALID_CASE", sgp.iter[[sgp.projection.content.areas.label]],
									tail(head(sgp.iter[["sgp.panel.years"]], -1L), length(sgp.iter[[sgp.projection.grade.sequences.label]])),
									sgp.iter[[sgp.projection.grade.sequences.label]]), nomatch=0][,
									'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")]

						if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
							invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
							tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
						}
						tmp.data <- ddcast(tmp.data, ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", state, sgp.scale.score.equated, SGPt), sep=".")
					} else {
					tmp.data <- ddcast(
						data.table(
							data.table(sgp.data, key="ID")[
								sgp.data[SJ("VALID_CASE",
								tail(sgp.iter[["sgp.content.areas"]], 1L),
								tail(sgp.iter[["sgp.panel.years"]], 1L),
								tail(sgp.iter[["sgp.grade.sequences"]], 1L))][,"ID", with=FALSE]], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[
							SJ("VALID_CASE", sgp.iter[[sgp.projection.content.areas.label]],
								tail(head(sgp.iter[["sgp.panel.years"]], -1L), length(sgp.iter[[sgp.projection.grade.sequences.label]])),
								sgp.iter[[sgp.projection.grade.sequences.label]]), nomatch=0][,
								'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")],
							ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", state, sgp.scale.score.equated, SGPt), sep=".")
					}
				}

				if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
					setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
					if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) {
						tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
					}
				}
				return(tmp.data)
			} else {  ###  END is.null(sgp.targets)
                if (arrow.tf) {
                    prior_lookup <- 
                        arrow::arrow_table(
                            VALID_CASE = "VALID_CASE",
                            CONTENT_AREA = sgp.iter[[sgp.projection.content.areas.label]],
                            YEAR = tail(
                                head(sgp.iter[["sgp.panel.years"]], -1L),
                                length(sgp.iter[[sgp.projection.grade.sequences.label]])
                            ),
                            GRADE = sgp.iter[[sgp.projection.grade.sequences.label]]
                        )
                    prior_lookup <-
                        prior_lookup$cast(
                            target_schema =
                                sgp.data$schema[c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")]
                        )

                    tmp_ids <- sgp.data |>
                        dplyr::filter(
                            VALID_CASE == "VALID_CASE",
                            CONTENT_AREA == tail(sgp.iter[["sgp.content.areas"]], 1L),
                            YEAR == tail(sgp.iter[["sgp.panel.years"]], 1L),
                            GRADE == tail(sgp.iter[["sgp.grade.sequences"]], 1L)
                        )  |>
                        dplyr::select(ID)
                    
                    tmp.vars.to.get <- c(getKey(sgp.data), "SCALE_SCORE", "ACHIEVEMENT_LEVEL")

                    tmp.data <- sgp.data |>
                        dplyr::select(dplyr::all_of(tmp.vars.to.get)) |>
                        dplyr::semi_join(prior_lookup) |>
                        # dplyr::filter(ID %in% tmp_ids) |> # `%in%` not implimented in `arrow/filter`
                        dplyr::semi_join(tmp_ids) |>
                        dplyr::mutate(tmp.timevar = paste(YEAR, CONTENT_AREA, sep = ".")) |>
                        data.table::as.data.table() |>
                        data.table::setkeyv(getKey(sgp.data))

                    if (is.character(fix.duplicates)) {
                        if (fix.duplicates!="KEEP.ALL") {
                            stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")
                        }
                        if (any(duplicated(tmp.data, by = getKey(tmp.data)))) {
                            invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
                            tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]
                        }
                    }

                    tmp.data <-
                        ddcast(
                            tmp.dt = tmp.data,
                            formula = ID ~ tmp.timevar,
                            value.var = c(
                                "GRADE", "SCALE_SCORE",
                                state, sgp.scale.score.equated, SGPt),
                            sep = "."
                        )[sgp.targets[
                            CONTENT_AREA == tail(sgp.iter[["sgp.content.areas"]], 1L) &
                            YEAR == tail(sgp.iter[["sgp.panel.years"]], 1L) &
                            GRADE == tail(sgp.iter[["sgp.grade.sequences"]], 1L)
                        ], nomatch = 0
                        ][, !c("CONTENT_AREA", "YEAR"), with = FALSE]
                } else {
					tmp.lookup1 <- SJ("VALID_CASE",
						 	tail(sgp.iter[["sgp.content.areas"]], 1L),
						 	tail(sgp.iter[["sgp.panel.years"]], 1L),
							tail(sgp.iter[["sgp.grade.sequences"]], 1L))

					tmp.lookup2 <- SJ("VALID_CASE",
							sgp.iter[[sgp.projection.content.areas.label]],
							tail(head(sgp.iter[["sgp.panel.years"]], -1L), length(sgp.iter[[sgp.projection.grade.sequences.label]])),
							sgp.iter[[sgp.projection.grade.sequences.label]])

					setkey(tmp.lookup2, V3)
					setkey(tmp.lookup2, NULL)
					setnames(tmp.lookup1, paste0("V", seq.int(4)), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
					setnames(tmp.lookup2, paste0("V", seq.int(4)), c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))
					setkeyv(sgp.data, names(tmp.lookup1))

					if (is.character(fix.duplicates)) {
						if (fix.duplicates!="KEEP.ALL") stop("Invalid character for 'fix.duplicates' argument/SGPstateData > SGP_Configuration value.  Should be 'KEEP.ALL'.")

						tmp.data <- data.table(data.table(sgp.data, key="ID")[sgp.data[tmp.lookup1][,"ID", with=FALSE]],
							key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[tmp.lookup2, nomatch=0][,'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")]

						if (any(duplicated(tmp.data, by=getKey(tmp.data)))) {
							invisible(tmp.data[, CONTENT_AREA := "TEMP_CONTENT_AREA"])
							tmp.data <- createUniqueLongData(tmp.data)[!is.na(CONTENT_AREA)]

							##  Create SCALE_SCORE history vars to merge on
							tmp.data <- ddcast(tmp.data, ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")[,
								GRADE := tmp.lookup1[["GRADE"]]]
							jExpression <- paste0("paste(", paste(grep("SCALE_SCORE[.]", names(tmp.data), value=TRUE) , collapse = ", "), ", sep = '; ')")
							invisible(tmp.data[, SGP_PROJECTION_GROUP_SCALE_SCORES := eval(parse(text=jExpression))])
							invisible(tmp.data[, SGP_PROJECTION_GROUP_SCALE_SCORES := gsub("NA; ", "", SGP_PROJECTION_GROUP_SCALE_SCORES)])
							invisible(tmp.data[grepl("_DUPS_[0-9]*", ID), DUPS_FLAG := gsub(".*_DUPS_", "", ID)])
							invisible(tmp.data[, ID := gsub("_DUPS_[0-9]*", "", ID)])

							tmp.data <- tmp.data[sgp.targets[CONTENT_AREA == tmp.lookup1[["CONTENT_AREA"]] & YEAR == tmp.lookup1[["YEAR"]] & GRADE == tmp.lookup1[["GRADE"]]],
								on=c("ID", "GRADE", "SGP_PROJECTION_GROUP_SCALE_SCORES"), nomatch=0][,
								!c("CONTENT_AREA", "YEAR", "GRADE", "SGP_PROJECTION_GROUP_SCALE_SCORES"), with=FALSE]

							invisible(tmp.data[!is.na(DUPS_FLAG), ID := paste0(ID, "_DUPS_", seq.int(.N)), by="ID"])
							invisible(tmp.data[, DUPS_FLAG := NULL])
						} else {  ###  END if (any(duplicated(tmp.data, ...)))
							tmp.data <- ddcast(tmp.data, ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")[, GRADE := tmp.lookup1[["GRADE"]]][
								sgp.targets[CONTENT_AREA == tmp.lookup1[["CONTENT_AREA"]] & YEAR == tmp.lookup1[["YEAR"]] & GRADE == tmp.lookup1[["GRADE"]]], on=intersect(names(tmp.data), c("ID", "GRADE")), nomatch=0][,
								!c("CONTENT_AREA", "YEAR", "GRADE"), with=FALSE]
						}
					} else {  ###  END if (is.character(fix.duplicates)
						tmp.data <- ddcast(data.table(data.table(sgp.data, key="ID")[
									sgp.data[tmp.lookup1][,"ID", with=FALSE]], key=c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE"))[
									tmp.lookup2, nomatch=0][,
									'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")],
								ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE", state, sgp.scale.score.equated, SGPt), sep=".")[
									sgp.targets[CONTENT_AREA == tmp.lookup1[["CONTENT_AREA"]] & YEAR == tmp.lookup1[["YEAR"]] & GRADE == tmp.lookup1[["GRADE"]]], nomatch=0][,
									!c("CONTENT_AREA", "YEAR"), with=FALSE]
					}
				}

				if ("STATE" %in% var.names && dim(tmp.data)[1L]!=0L) {
					setnames(tmp.data, tail(sort(grep("STATE", names(tmp.data), value=TRUE)), 1L), "STATE")
					if (length(setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE")) > 0L) {
						tmp.data[,setdiff(grep("STATE", names(tmp.data), value=TRUE), "STATE"):=NULL]
					}
				}
				return(tmp.data)
			}
		}
	} ### END if (sgp.type=="sgp.projections.lagged")
} ## END getPanelData
