`studentGrowthProjections` <-
function(panel.data,	## REQUIRED
	sgp.labels,	## REQUIRED  
	grade.progression,	## REQUIRED
	content_area.progression=NULL,
	year_lags.progression=NULL,
	grade.projection.sequence=NULL,
	content_area.projection.sequence=NULL,
	year_lags.projection.sequence=NULL,
	max.forward.progression.years=NULL,
	max.forward.progression.grade=NULL,
	max.order.for.progression=NULL,
	use.my.knots.boundaries,
	use.my.coefficient.matrices,
	panel.data.vnames,
	achievement.level.prior.vname=NULL,
	performance.level.cutscores,
        calculate.sgps=TRUE,
	convert.0and100=TRUE,
	projection.unit="YEAR",
	percentile.trajectory.values=NULL,
	isotonize=TRUE,
	lag.increment=0,
	sgp.exact.grade.progression=FALSE,
	projcuts.digits=NULL,
	print.time.taken=TRUE) {

	started.at=proc.time()
	started.date <- date()

	##########################################################
	###
	### Utility functions
	###
	##########################################################

	.smooth.bound.iso.row <- function(x, grade, tmp.year, tmp.content_area, iso=isotonize, missing.taus, na.replace) {
		bnd <- eval(parse(text=paste("panel.data[['Knots_Boundaries']]", get.my.knots.boundaries.path(tmp.content_area, tmp.year), "[['loss.hoss_", grade, "']]", sep="")))
		x[x < bnd[1]] <- bnd[1] ; x[x > bnd[2]] <- bnd[2]
		if (!iso) return(round(x, digits=5)) # Results are the same whether NAs present or not...
		if (iso & missing.taus) {
			na.row <- rep(NA,100)
			na.row[na.replace] <- round(sort(x[!is.na(x)]), digits=5)
			return(na.row)
		} else {
			x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
			return(round(sort(x), digits=5))
		}
	}

        .create.path <- function(labels, pieces=c("my.subject", "my.year", "my.extra.label")) {
                sub(' ', '_', toupper(sub('\\.+$', '', paste(unlist(sapply(labels[pieces], as.character)), collapse="."))))
        }

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.path.knots.boundaries <- paste(sgp.labels[['my.subject']], sgp.labels[['my.year']], sep=".")
		tmp.knots.boundaries.names <-
			names(panel.data[['Knots_Boundaries']][[tmp.path.knots.boundaries]])[content_area==sapply(strsplit(names(panel.data[['Knots_Boundaries']][[tmp.path.knots.boundaries]]), "[.]"), '[', 1)]
		if (length(tmp.knots.boundaries.names)==0) {
			return(paste("[['", tmp.path.knots.boundaries, "']]", sep=""))
		} else {
			tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), function(x) x[2])
			if (any(!is.na(tmp.knots.boundaries.years))) {
				if (year %in% tmp.knots.boundaries.years) {
					return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", year, "']]", sep=""))
				} else {
					if (year==sort(c(year, tmp.knots.boundaries.years))[1]) {
						return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, "']]", sep=""))
					} else {
						return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", rev(sort(tmp.knots.boundaries.years))[1], "']]", sep=""))
					}
				}
			} else {
				return(paste("[['", tmp.path.knots.boundaries, "']][['", content_area, "']]", sep=""))
			}
		}
	}

	.get.panel.data <- function(tmp.data, grade.progression, content_area.progression, num.prior=NULL, subset.tf=NULL, bound.data=TRUE) {
		str1 <- str2 <- str3 <- NULL
		if (is.null(num.prior)) num.prior <- length(grade.progression)
		for (i in 1:num.prior-1) {
			str1 <- paste(str1, " & !is.na(tmp.data[[", 1+2*num.panels-i, "]])", sep="")
			str2 <- paste(str2, " & tmp.data[[", 1+num.panels-i, "]]=='", rev(as.character(grade.progression))[i+1], "'", sep="")
			str3 <- c(1+2*num.panels-i, str3)
		} 
		if (!is.null(subset.tf)) str1 <- paste(str1, " & subset.tf", sep="")
		tmp.data <- tmp.data[eval(parse(text=paste(substring(str1, 4), str2, sep="")))][, c(1, str3), with=FALSE]
		if (bound.data) {
			for (i in seq(dim(tmp.data)[2]-1)) {
				bnd <- eval(parse(text=paste("panel.data[['Knots_Boundaries']]", get.my.knots.boundaries.path(content_area.progression[i], as.character(sgp.labels$my.year)), 
					"[['loss.hoss_", grade.progression[i], "']]", sep="")))
				tmp.data[tmp.data[[i+1]]<bnd[1], names(tmp.data)[i+1] := bnd[1], with=FALSE]
				tmp.data[tmp.data[[i+1]]>bnd[2], names(tmp.data)[i+1] := bnd[2], with=FALSE]
			}
		}
		return(tmp.data)
	}

	get.my.cutscore.year.sgprojection <- function(Cutscores, content_area, year) {
		tmp.cutscore.years <- sapply(strsplit(names(Cutscores)[grep(content_area, names(Cutscores))], "[.]"), function(x) x[2])
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

	get.grade.projection.sequence.matrices <- function(
							grade.progression, 
							content_area.progression, 
							year_lags.progression, 
							grade.projection.sequence, 
							content_area.projection.sequence, 
							year_lags.projection.sequence,
							sgp.exact.grade.progression) {

		add.missing.taus.to.matrix <- function(my.matrix) {
			augmented.mtx <- matrix(NA, nrow=nrow(my.matrix), ncol=100)
			tau.num <- ceiling(as.numeric(substr(colnames(my.matrix), 6, nchar(colnames(my.matrix))))*100)
			na.replace <- 1:100 %in% tau.num
			augmented.mtx[,na.replace] <- my.matrix
			return(augmented.mtx)
		}

		if (sgp.exact.grade.progression) grade.progression.index <- length(grade.progression) else grade.progression.index <- seq_along(grade.progression)

		tmp.list <- list()
		for (i in seq_along(grade.progression.index)) {
			tmp.list[[i]] <- list()
			for (j in seq_along(grade.projection.sequence)) {
				tmp.years_lags <- c(tail(year_lags.progression, grade.progression.index[i]-1), head(year_lags.projection.sequence, j))
				if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
					tmp.years <- rep("BASELINE", length(tmp.years_lags)+1) 
				} else {
					tmp.years <- yearIncrement(sgp.labels$my.year, rev(c(0, -cumsum(rev(tmp.years_lags)))))
				}
				tmp.list[[i]][[j]] <- getsplineMatrices(
						tmp.matrices,
						c(tail(content_area.progression, grade.progression.index[i]), head(content_area.projection.sequence, j)),
						c(tail(grade.progression, grade.progression.index[i]), head(grade.projection.sequence, j)),
						tmp.years,
						tmp.years_lags,
						return.highest.order.matrix=TRUE,
						my.matrix.highest.order=max.order.for.progression)[[1]]
				if (dim(tmp.list[[i]][[j]]@.Data)[2] != 100) {
					tmp.list[[i]][[j]]@.Data <- add.missing.taus.to.matrix(tmp.list[[i]][[j]]@.Data)
					missing.taus <- TRUE
				}
			}
		}
		return(rev(tmp.list)) ### rev gives highest orders first
	}

	.get.percentile.trajectories <- function(ss.data, projection.matrices) {

		tmp.percentile.trajectories <- list()
		completed.ids <- TEMP_1 <- TEMP_2 <-NULL

		for (i in seq_along(projection.matrices)) {
			if (any(!ss.data[[1]] %in% completed.ids)) {
				tmp.dt <- .get.panel.data(
							ss.data, 
							head(projection.matrices[[i]][[1]]@Grade_Progression[[1]], -1), 
							head(projection.matrices[[i]][[1]]@Content_Areas[[1]], -1), 
							subset.tf=!(ss.data[[1]] %in% completed.ids))
				if (dim(tmp.dt)[1] > 0) {
					completed.ids <- c(unique(tmp.dt[[1]]), completed.ids)
					tmp.dt <- tmp.dt[list(rep(tmp.dt[[1]], each=100))]
					missing.taus <- FALSE; na.replace <- NULL # put these outside of j loop so that stays true/non-null if only SOME of coef matrices have missing column/taus.

					for (j in seq_along(projection.matrices[[i]])) {
						tmp.matrix <- projection.matrices[[i]][[j]]
						mod <- character()
						int <- "cbind(rep(1, dim(tmp.dt)[1]),"
						for (k in seq_along(projection.matrices[[i]][[j]]@Time_Lags[[1]])) {
							knt <- paste("tmp.matrix@Knots[[", k, "]]", sep="")
							bnd <- paste("tmp.matrix@Boundaries[[", k, "]]", sep="")
							mod <- paste(mod, ", bs(tmp.dt[[", dim(tmp.dt)[2]-k+1, "]], knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
						}

						tmp.scores <- eval(parse(text=paste(int, substring(mod, 2), ")", sep="")))

						for (m in seq(100)) {
							tmp.dt[m+100*(seq(dim(tmp.dt)[1]/100)-1), TEMP_1:=tmp.scores[m+100*(seq(dim(tmp.dt)[1]/100)-1),] %*% tmp.matrix@.Data[,m]]
						}

						tmp.dt[,TEMP_2:=.smooth.bound.iso.row(
										TEMP_1, 
										grade.projection.sequence[j], 
										yearIncrement(sgp.labels[['my.year']], j, lag.increment),
										content_area.projection.sequence[j],
										missing.taus=missing.taus, 
										na.replace=na.replace), 
									by=eval(names(tmp.dt)[1])]
						setnames(tmp.dt, "TEMP_2", paste("SS", grade.projection.sequence[j], content_area.projection.sequence[j], sep="."))
						tmp.dt[,TEMP_1:=NULL]
					} ## END j loop
					tmp.percentile.trajectories[[i]] <- tmp.dt[,c("ID", paste("SS", grade.projection.sequence, content_area.projection.sequence, sep=".")), with=FALSE]
					rm(tmp.dt); suppressMessages(gc())
				} ## END if (dim(tmp.dt)[1] > 0)
			} ## END if statement
		} ## END i loop
		rbindlist(tmp.percentile.trajectories)
	} ## END function

	.sgp.targets <- function(data, cut, convert.0and100) {
		if (is.na(cut)) {
			return(rep(NA, length(data)))
		} else {
			tmp <- which.min(c(data < cut, FALSE))
			tmp[tmp==101] <- 100
			if (convert.0and100) {tmp[tmp==0] <- 1; tmp[tmp==100] <- 99}
			return(as.integer(tmp))
		}
	}

	.get.trajectories.and.cuts <- function(percentile.trajectories, trajectories.tf, cuts.tf, projection.unit=projection.unit) {
		CUT <- NULL
		if (trajectories.tf) {
			if (is.numeric(percentile.trajectory.values)) {
				tmp.name.prefix <- "P"
				tmp.traj <- percentile.trajectories[rep(1:100, dim(percentile.trajectories)[1]/100) %in% percentile.trajectory.values]
				tmp.num.years.forward <- length(grade.projection.sequence)
			}
			if (is.character(percentile.trajectory.values)) {
				tmp.name.prefix <- "SCALE_SCORE_"
				tmp.num.years.forward <- min(length(grade.projection.sequence), 
					lapply(strsplit(percentile.trajectory.values, "_")[[1]], type.convert)[sapply(lapply(strsplit(percentile.trajectory.values, "_")[[1]], type.convert), is.numeric)][[1]])
				if (length(grep("CURRENT", percentile.trajectory.values))==0) tmp.num.years.forward <- min(length(grade.projection.sequence), tmp.num.years.forward+1)

				tmp.indices <- as.integer(rep(dim(percentile.trajectories)[1]/length(unique(percentile.trajectories$ID))*(seq(length(unique(percentile.trajectories$ID)))-1), 
					each=length(percentile.trajectory.values)) + as.numeric(t(as.matrix(data.table(panel.data[["Panel_Data"]], 
					key="ID")[list(unique(percentile.trajectories[['ID']]))][,percentile.trajectory.values, with=FALSE]))))
				tmp.traj <- percentile.trajectories[tmp.indices, 1:(2+tmp.num.years.forward-1), with=FALSE][,ID:=rep(unique(percentile.trajectories$ID), each=length(percentile.trajectory.values))]
				if (tmp.num.years.forward==1) {
					my.cutscore.year <- get.my.cutscore.year.sgprojection(Cutscores, sgp.labels$my.subject, yearIncrement(sgp.labels$my.year, 1, lag.increment))
					tmp.cutscores.by.grade <- tmp.cutscores[[my.cutscore.year]][[paste("GRADE_", grade.projection.sequence[1], sep="")]]
					tmp.target.name <- tail(names(tmp.traj), 1)
					if (length(percentile.trajectory.values)==1) {
						cuku.level.to.get <- which.max(SGPstateData[[performance.level.cutscores]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1
						tmp.target.scores <- rep(tmp.cutscores.by.grade[cuku.level.to.get], length(unique(tmp.traj[['ID']])))

					}
					if (length(percentile.trajectory.values)==2) {
						cuku.level.to.get <- which.max(SGPstateData[[performance.level.cutscores]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1
						musu.level.to.get <- which.max(SGPstateData[[performance.level.cutscores]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")
						tmp.target.scores <- rep(c(tmp.cutscores.by.grade[cuku.level.to.get], tmp.cutscores.by.grade[musu.level.to.get]), length(unique(tmp.traj[['ID']])))
					}
					tmp.target.scores[is.na(tmp.traj[[tmp.target.name]])] <- NA
					tmp.traj[,tmp.target.name:=tmp.target.scores, with=FALSE]
				}
			}
			tmp.traj[,2:dim(tmp.traj)[2] := round(tmp.traj[,2:dim(tmp.traj)[2], with=FALSE], digits=projcuts.digits), with=FALSE]
			trajectories <- data.table(reshape(tmp.traj[, CUT:=rep(percentile.trajectory.values, dim(tmp.traj)[1]/length(percentile.trajectory.values))], 
				idvar="ID", timevar="CUT", direction="wide"), key="ID")

			if (length(grep("CURRENT", percentile.trajectory.values))!=0) percentile.trajectory.values <- unlist(strsplit(percentile.trajectory.values, "_CURRENT"))
			if (projection.unit=="GRADE") {
				tmp.vec <- expand.grid(tmp.name.prefix, percentile.trajectory.values, "_PROJ_GRADE_", grade.projection.sequence, lag.increment.label)[1:(length(percentile.trajectory.values)*tmp.num.years.forward),]
			} else {
				tmp.vec <- expand.grid(tmp.name.prefix, percentile.trajectory.values, "_PROJ_YEAR_", seq_along(grade.projection.sequence), lag.increment.label)[1:(length(percentile.trajectory.values)*tmp.num.years.forward),]
			}
			tmp.vec <- tmp.vec[order(tmp.vec$Var2),]
			setnames(trajectories, c("ID", do.call(paste, c(tmp.vec, sep=""))))
			if (!cuts.tf) return(trajectories)
		}
		if (cuts.tf) {
			setkey(percentile.trajectories, ID)
			k <- 1
			cuts.arg <- names.arg <- character()

			for (i in seq_along(grade.projection.sequence)) {
				my.cutscore.year <- get.my.cutscore.year.sgprojection(Cutscores, content_area.projection.sequence[i], yearIncrement(sgp.labels[['my.year']], i, lag.increment))
				tmp.cutscores.by.grade <- tmp.cutscores[[my.cutscore.year]][[paste("GRADE_", grade.projection.sequence[i], sep="")]]

				if (!is.null(tmp.cutscores.by.grade)) {
					for (j in seq_along(tmp.cutscores.by.grade)) {
						cuts.arg[k] <- paste(".sgp.targets(SS", ".", grade.projection.sequence[i], ".", content_area.projection.sequence[i], ", ", tmp.cutscores.by.grade[j], ", ", convert.0and100, ")", sep="")
						if (projection.unit=="GRADE") {
							names.arg[k] <- paste("LEVEL_", j, "_SGP_TARGET_GRADE_", grade.projection.sequence[i], lag.increment.label, sep="")
						} else {
							names.arg[k] <- paste("LEVEL_", j, "_SGP_TARGET_YEAR_", i, lag.increment.label, sep="")
						}
						k <- k+1
					}
				}
			}
			arg <- paste("list(", paste(cuts.arg, collapse=", "), ")", sep="")
			tmp.cuts <- eval(parse(text=paste("percentile.trajectories[,", arg, ", by=ID]", sep="")))
			setnames(tmp.cuts, c("ID", names.arg))
			setkey(tmp.cuts, ID)
			if (!trajectories.tf) {
				return(tmp.cuts)
			} else {
				return(merge(tmp.cuts, trajectories))
			}
		}
	}


	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	ID <- tmp.messages <- NULL

        if (!calculate.sgps) {
                tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Student growth projections not calculated for", sgp.labels$my.year, sgp.labels$my.subject, "due to argument calculate.sgps=FALSE.\n"))
                return(panel.data)
        }

	if (missing(panel.data)) {
		stop("User must supply student achievement data for student growth percentile calculations. See help page for details.")
	}

	if (!is.list(panel.data)) {
		stop("Supplied panel.data not of a supported class. See help for details of supported classes")
	} else {
		if (!(all(c("Panel_Data", "Coefficient_Matrices", "Knots_Boundaries") %in% names(panel.data)))) {
			stop("Supplied panel.data missing Panel_Data, Coefficient_Matrices, and/or Knots_Boundaries. See help page for details")
		}
		if (!identical(class(panel.data[["Panel_Data"]]), "data.frame")) {
			stop("Supplied panel.data$Panel_Data is not a data.frame")	 
	}}

	if (missing(sgp.labels)) {
		stop("User must supply a list of SGP function labels (sgp.labels). See help page for details.")
	} else {
		if (!is.list(sgp.labels)) {
			stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
		}
		if (!identical(names(sgp.labels), c("my.year", "my.subject")) & 
			!identical(names(sgp.labels), c("my.year", "my.subject", "my.extra.label"))) {
			stop("Please specify an appropriate list for sgp.labels. See help page for details.")
			}
		sgp.labels <- lapply(sgp.labels, toupper)
		tmp.path <- .create.path(sgp.labels)
	}

	if (missing(grade.progression)) {
		stop("User must supply a grade progression from which projections/trajectories will be derived. See help page for details.")
	}
	grade.progression <- as.character(grade.progression)

	if (!missing(use.my.knots.boundaries)) {
		if (!is.list(use.my.knots.boundaries) & !is.character(use.my.knots.boundaries)) {
			stop("use.my.knots.boundaries must be supplied as a list or character abbreviation. See help page for details.")
		}
		if (is.list(use.my.knots.boundaries)) {
			if (!is.list(panel.data)) {
				stop("use.my.knots.boundaries is only appropriate when panel data is of class list. See help page for details.")
			}
			if (!identical(names(use.my.knots.boundaries), c("my.year", "my.subject")) & !identical(names(use.my.knots.boundaries), c("my.year", "my.subject", "my.extra.label"))) {
				stop("Please specify an appropriate list for use.my.knots.boundaries. See help page for details.")
			}
			if (is.null(panel.data[["Knots_Boundaries"]]) | is.null(panel.data[["Knots_Boundaries"]][[.create.path(use.my.knots.boundaries, pieces=c("my.subject", "my.year"))]])) {
				stop("Knots and Boundaries indicated by use.my.knots.boundaries are not included.")
			}
		}
		if (is.character(use.my.knots.boundaries)) {
			if (!use.my.knots.boundaries %in% objects(SGPstateData)) {
				stop(paste("Knots and Boundaries are currently not implemented for the state (", use.my.knots.boundaries, ") indicated. Please contact the SGP package administrator to have your Knots and Boundaries included in the package", sep=""))
			}
		}
	} 

	if (!missing(use.my.coefficient.matrices) & is.null(content_area.projection.sequence)) {
		if (!is.list(use.my.coefficient.matrices)) {
			stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		if (!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject")) & !identical(names(use.my.coefficient.matrices), c("my.year", "my.subject", "my.extra.label"))) {
			stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices)
		if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
			if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0 & !is.null(SGPstateData[[performance.level.cutscores]][["Baseline_splineMatrix"]])) {
				panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]] <- SGPstateData[[performance.level.cutscores]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]]
			} else {
				message("\tNOTE: Coefficient matrices indicated by argument use.my.coefficient.matrices are not included.")
				return(NULL)
			}
		}
	}
	if (missing(use.my.coefficient.matrices) & is.null(content_area.projection.sequence)) {
		if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
			tmp.path.coefficient.matrices <- paste(sgp.labels$my.subject, "BASELINE", sep=".")
			if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
				stop(paste("\tNOTE: Coefficient matrices indicated by argument sgp.labels, '", tmp.path.coefficient.matrices, "', are not included. Please check supplied list to make sure appropriate coefficient matrices are included.", sep=""))
				return(NULL)
			}
		} else {
			tmp.path.coefficient.matrices <- tmp.path
			if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
				message(paste("\tNOTE: Coefficient matrices indicated by argument sgp.labels, '", tmp.path.coefficient.matrices, "', are not included. Bypassing plot production.", sep=""))
				return(NULL)
			}
		}
	}
	if (!is.null(content_area.projection.sequence)) {
		if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
			tmp.path.coefficient.matrices <- paste(unique(content_area.projection.sequence), "BASELINE", sep=".")
		} else {
			tmp.path.coefficient.matrices <- paste(unique(content_area.projection.sequence), sgp.labels$my.year, sep=".")
		}
	}

	if (!missing(performance.level.cutscores)) {
		if (is.character(performance.level.cutscores)) {
			if (!(performance.level.cutscores %in% objects(SGPstateData))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: To use state cutscores, supply an appropriate two letter state abbreviation. \nRequested state may not be included. See help page for details.\n")
				tf.cutscores <- FALSE
			}
			if (is.null(names(SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Cutscores are currently not implemented for the state indicated. \nPlease contact the SGP package administrator to have your cutscores included in the package.\n")
				tf.cutscores <- FALSE
			}
			if (!sgp.labels$my.subject %in% names(SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]])) {
				stop("\nCutscores provided in SGPstateData does not include a subject name that matches my.subject in sgp.labels (CASE SENSITIVE). See help page for details.\n\n")
			} else {
				tmp.cutscores <- SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]
				if (!is.character(percentile.trajectory.values)) tf.cutscores <- TRUE else tf.cutscores <- FALSE
		}}
		if (is.list(performance.level.cutscores)) {
			if (any(names(performance.level.cutscores) %in% sgp.labels$my.subject)) {
				tmp.cutscores <- performance.level.cutscores
				tf.cutscores <- TRUE
			} else {
				stop("\nList of cutscores provided in performance.level.cutscores must include a subject name that matches my.subject in sgp.labels (CASE SENSITIVE). See help page for details.\n\n")
				tf.cutscores <- FALSE
	}}} else {
		tf.cutscores <- FALSE
	}

	if (!(toupper(projection.unit)=="YEAR" | toupper(projection.unit)=="GRADE")) {
		stop("Projection unit must be specified as either YEAR or GRADE. See help page for details.")
	}

	if (is.null(percentile.trajectory.values) & !tf.cutscores) {
		stop("\tNOTE: Either percentile trajectories and/or performance level cutscores must be supplied for the analyses.")
	}

	if (!is.null(percentile.trajectory.values) && is.numeric(percentile.trajectory.values) && !all(percentile.trajectory.values %in% 1:100)) {
		message("\tNOTE: Integer supplied 'percentile.trajectory.values' must be between 1 and 100. Only supplied values in that range will be used.")
		percentile.trajectory.values <- intersect(percentile.trajectory.values, 1:100)
	}

	if (!is.null(percentile.trajectory.values) && is.character(percentile.trajectory.values) && !all(percentile.trajectory.values %in% names(panel.data[["Panel_Data"]]))) {
		message("\tNOTE: Character 'percentile.trajectory.values' must correspond to individual specific variable in panel.data[['Panel_Data']]. Please check for appropriate variables.")
	}

	if (!is.null(achievement.level.prior.vname)) {
		if (!achievement.level.prior.vname %in% names(panel.data[["Panel_Data"]])) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied achievement.level.prior.vname is not in supplied panel.data. No ACHIEVEMENT_LEVEL_PRIOR variable will be produced.\n")
			achievement.level.prior.vname <- NULL
		}
	}

	if (lag.increment==0) lag.increment.label <- "_CURRENT" else lag.increment.label <- ""

	if (!is.null(grade.projection.sequence) & !is.null(content_area.projection.sequence) && length(grade.projection.sequence) != length(content_area.projection.sequence)) {
		stop("\tNOTE: Supplied 'grade.projection.sequence' and 'content_area.projection.sequence' must be of the same length")
	}

	if (!is.null(grade.projection.sequence) & !is.null(year_lags.projection.sequence) && length(grade.projection.sequence)-1 != length(year_lags.projection.sequence)) {
		stop("\tNOTE: Supplied 'year_lags.projection.sequence' must have length 1 less than 'grade.projection.sequence'.")
	}

	if (is.null(projcuts.digits)) {
		projcuts.digits <- 0
	}


	########################################################
	###
	### Calculate Student Growth Projections/Trajectories
	###
	########################################################

	tmp.objects <- c("SGProjections", "Cutscores") 
	for (i in tmp.objects) {
		if (!is.null(panel.data[[i]])) {
			assign(i, panel.data[[i]])
		} else {
			assign(i, list())
		}
	} 

	if (tf.cutscores | is.character(percentile.trajectory.values)) {
		Cutscores <- tmp.cutscores
	}


	### Create ss.data from Panel_Data and rename variables based upon grade.progression

        if (!missing(panel.data.vnames)) {
                if (!all(panel.data.vnames %in% names(panel.data[["Panel_Data"]]))) {
                        tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'panel.data.vnames' are not all in the supplied 'Panel_Data'.\n\t\tAnalyses will continue with the variables contained in both Panel_Data and those provided in the supplied argument 'panel.data.vnames'.\n")
                }
                ss.data <- as.data.table(panel.data[["Panel_Data"]][,intersect(panel.data.vnames, names(panel.data[["Panel_Data"]]))])
        } else {
                ss.data <- as.data.table(panel.data[["Panel_Data"]])
        }

	if (dim(ss.data)[2] %% 2 != 1) {
		stop(paste("Number of columns of supplied panel data (", dim(ss.data)[2], ") does not conform to data requirements. See help page for details."))
	}
	num.panels <- (dim(ss.data)[2]-1)/2

	if (length(grade.progression) > num.panels) {
		tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Supplied grade progression, grade.progress=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.\n\t\t Analyses will utilize maximum number of priors supplied by the data.\n", sep=""))
		grade.progression <- tail(grade.progression, num.panels)
		if (!is.null(content_area.progression)) content_area.progression <- tail(content_area.progression, length(grade.progression))
		if (!is.null(year_lags.progression)) year_lags.progression <- tail(year_lags.progression, length(grade.progression)-1)
	}

	if (!is.null(max.order.for.progression)) {
		grade.progression <- tail(grade.progression, max.order.for.progression)
		if (!is.null(content_area.progression)) content_area.progression <- tail(content_area.progression, length(grade.progression))
		if (!is.null(year_lags.progression)) year_lags.progression <- tail(year_lags.progression, length(grade.progression)-1)
	}

	tmp.last <- tail(grade.progression, 1)
	ss.data <- data.table(ss.data[,c(1, (1+num.panels-(length(grade.progression)-1)):(1+num.panels), (1+2*num.panels-(length(grade.progression)-1)):(1+2*num.panels)), with=FALSE], 
			key=names(ss.data)[1])
	num.panels <- (dim(ss.data)[2]-1)/2
	setnames(ss.data, c(1, (1+num.panels-length(grade.progression)+1):(1+num.panels), (1+2*num.panels-length(grade.progression)+1):(1+2*num.panels)), 
		c("ID", paste("GD", grade.progression, content_area.progression, sep="."), paste("SS", grade.progression, content_area.progression, sep=".")))


	### Get relevant matrices for projections

	tmp.matrices <- unlist(panel.data[["Coefficient_Matrices"]][match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]]))], recursive=FALSE)

	### PROGRESSION SEQUENCES: content_area.progression, & year_lags.progression if not supplied

	if (is.null(content_area.progression)) {
		content_area.progression <- rep(sgp.labels[['my.subject']], length(grade.progression))
	} else {
		if (!identical(class(content_area.progression), "character")) {
			stop("content_area.progression should be a character vector. See help page for details.")
		}
		if (length(content_area.progression) != length(grade.progression)) {
			tmp.messages <- c(tmp.messages, "\tNOTE: The content_area.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (is.null(year_lags.progression)) {
		year_lags.progression <- rep(1, length(grade.progression)-1)
	}


	### PROJECTION SEQUENCES: Calculate grade.projection.sequence, content_area.projection.sequence, and year_lags.projection.sequence if not supplied

	if (is.null(grade.projection.sequence)) {
		grade.projection.sequence <- as.character(unique(sort(type.convert(sapply(tmp.matrices, function(x) tail(slot(x, "Grade_Progression")[[1]], 2)), as.is=TRUE))))
	}

	if (identical(grade.projection.sequence, numeric(0))) {
		stop("Supplied grade.progression and coefficient matrices do not allow projection. See help page for details.")
	}

	if (is.null(content_area.projection.sequence)) {
		content_area.projection.sequence <- rep(tail(content_area.progression, 1), length(grade.projection.sequence))
	}

	grade.content_area.progression <- paste(content_area.progression, paste("GRADE", grade.progression, sep="_"), sep=".")
	grade.content_area.projection.sequence <- paste(content_area.projection.sequence, paste("GRADE", grade.projection.sequence, sep="_"), sep=".")
	tmp.index <- seq(which(tail(grade.content_area.progression, 1)==grade.content_area.projection.sequence)+1, length(grade.projection.sequence))
	if (!is.null(max.forward.progression.grade)) {
		tmp.index <- intersect(tmp.index, which(sapply(grade.projection.sequence, function(x) type.convert(x, as.is=TRUE) <= type.convert(as.character(max.forward.progression.grade), as.is=TRUE))))
	}
	if (!is.null(max.forward.progression.years)) tmp.index <- head(tmp.index, max.forward.progression.years)
	grade.projection.sequence <- grade.projection.sequence[tmp.index]
	content_area.projection.sequence <- content_area.projection.sequence[tmp.index]
	if (is.null(year_lags.projection.sequence)) { ### NOTE same length as grade.projection.sequence for lag between progression and projection sequence
		if (is.numeric(type.convert(grade.projection.sequence))) {
			year_lags.projection.sequence <- diff(as.numeric(c(tail(grade.progression, 1), grade.projection.sequence)))
		} else {
			year_lags.projection.sequence <- rep(1, length(grade.projection.sequence))
		}
	}
	grade.content_area.projection.sequence <- grade.content_area.projection.sequence[tmp.index]

	### Test to see if ss.data has cases to analyze and configuration has elements to answer

	if (dim(.get.panel.data(ss.data, grade.progression, content_area.progression, 1, bound.data=FALSE))[1] == 0 | length(tmp.index)==0) {
                tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied data together with grade progression contains no data for analysis. Check data, function arguments and see help page for details.\n")
                message(paste("\tStarted studentGrowthProjections", started.date))
                message(paste("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
                message(paste(tmp.messages, "\tFinished studentGrowthProjections: SGP Percentile Growth Trajectory/Projection Analysis", date(), "in", timetaken(started.at), "\n"))

                return(
                list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
                        Cutscores=panel.data[["Cutscores"]],
                        Goodness_of_Fit=panel.data[["Goodness_of_Fit"]],
                        Knots_Boundaries=panel.data[["Knots_Bounadries"]],
                        Panel_Data=NULL,
                        SGPercentiles=panel.data[["SGPercentiles"]],
                        SGProjections=panel.data[["SGProjections"]],
                        Simulated_SGPs=panel.data[["Simulated_SGPs"]]))
        } 

	### Calculate grade.projection.sequence.priors 

	grade.projection.sequence.matrices <- get.grade.projection.sequence.matrices(
							grade.progression, 
							content_area.progression,
							year_lags.progression,
							grade.projection.sequence, 
							content_area.projection.sequence,
							year_lags.projection.sequence,
							sgp.exact.grade.progression) 


	### Calculate percentile trajectories

	percentile.trajectories <- .get.percentile.trajectories(ss.data, grade.projection.sequence.matrices)


	### Select specific percentile trajectories and calculate cutscores

	if (tf.cutscores) {
		tmp.cutscore.grade.content_area <- unlist(sapply(seq_along(tmp.cutscores), function(x) paste(names(tmp.cutscores)[x], names(tmp.cutscores[[x]]), sep=".")))
		if (!all(grade.content_area.projection.sequence %in% tmp.cutscore.grade.content_area)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Cutscores provided do not include cutscores for all grades/content areas in projection. Projections to grades/content areas without cutscores will be missing.\n")
	}} 

	trajectories.and.cuts <- .get.trajectories.and.cuts(percentile.trajectories, !is.null(percentile.trajectory.values), tf.cutscores, toupper(projection.unit))

	if (!is.null(achievement.level.prior.vname)) {
		trajectories.and.cuts <- data.table(panel.data[["Panel_Data"]][,c("ID", achievement.level.prior.vname)], key="ID")[trajectories.and.cuts]
		setnames(trajectories.and.cuts, achievement.level.prior.vname, "ACHIEVEMENT_LEVEL_PRIOR")
	}

	SGProjections[[tmp.path]] <- rbind.fill(as.data.frame(SGProjections[[tmp.path]]), trajectories.and.cuts)


	### Announce Completion & Return SGP Object

	if (print.time.taken) {
	        message(paste("\tStarted studentGrowthProjections:", started.date))
		message(paste("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, " (N=", format(dim(trajectories.and.cuts)[1], big.mark=","), ")", sep="")) 
		message(c(tmp.messages, "\tFinished studentGrowthProjections: ", date(), " in ", timetaken(started.at), "\n"))
	} 

	list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
		Cutscores=Cutscores,
		Goodness_of_Fit=panel.data[["Goodness_of_Fit"]], 
		Knots_Boundaries=panel.data[["Knots_Boundaries"]], 
		Panel_Data=panel.data[["Panel_Data"]],
		SGPercentiles=panel.data[["SGPercentiles"]],
		SGProjections=SGProjections,
		Simulated_SGPs=panel.data[["Simulated_SGPs"]])

} ## END studentGrowthProjections Function
