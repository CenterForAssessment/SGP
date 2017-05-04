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
	trajectories.chunk.size=50000L,
	sgp.projections.equated=NULL,
	projection.unit="YEAR",
	projection.unit.label=NULL,
	percentile.trajectory.values=NULL,
	return.percentile.trajectory.values=NULL,
	return.projection.group.identifier=NULL,
	return.projection.group.scale.scores=NULL,
	return.projection.group.dates=NULL,
	isotonize=TRUE,
	lag.increment=0L,
	sgp.exact.grade.progression=FALSE,
	projcuts.digits=NULL,
	sgp.projections.use.only.complete.matrices=NULL,
	SGPt=NULL,
	print.time.taken=TRUE) {

	started.at=proc.time()
	started.date <- prettyDate()

	GRADE <- NULL ## To avoid R CMD check warnings

	##########################################################
	###
	### Utility functions
	###
	##########################################################

	.smooth.bound.iso.row <- function(tmp.dt, grade, tmp.year, tmp.content_area, iso=isotonize, missing.taus, na.replace, equated.year) {
		X <- NULL
		if (!is.null(equated.year)) tmp.year <- equated.year
		bnd <- eval(parse(text=paste0("panel.data[['Knots_Boundaries']]", get.my.knots.boundaries.path(tmp.content_area, tmp.year), "[['loss.hoss_", grade, "']]")))
		tmp.dt[X < bnd[1L], X:=bnd[1L]]
		tmp.dt[X > bnd[2L], X:=bnd[2L]]
		if (!iso) return(round(tmp.dt[['X']], digits=5)) # Results are the same whether NAs present or not...
		if (iso & missing.taus) {
			na.row <- rep(NA,length(tmp.dt[['X']]))
			na.row[na.replace] <- round(data.table(tmp.dt[!is.na(X)], key=c("ID", "X"))[['X']], digits=5)
			return(na.row)
		} else {
			setkey(tmp.dt, ID, X)
			return(round(tmp.dt[c(matrix(seq.int(dim(tmp.dt)[1L]), ncol=100L, byrow=TRUE))][['X']], digits=5))
		}
	}

	.create.path <- function(labels, pieces=c("my.subject", "my.year", "my.extra.label")) {
		sub(' ', '_', toupper(sub('\\.+$', '', paste(unlist(lapply(labels[pieces], as.character)), collapse="."))))
	}

	.get.trajectory.chunks <- function(seq.for.data) {
		split(seq.for.data, ceiling(seq.for.data/trajectories.chunk.size))
	}

	get.my.knots.boundaries.path <- function(content_area, year) {
		tmp.path.knots.boundaries <- paste(sgp.labels[['my.subject']], sgp.labels[['my.year']], sep=".")
		if (is.null(sgp.projections.equated)) {
			tmp.knots.boundaries.names <-
				names(panel.data[['Knots_Boundaries']][[tmp.path.knots.boundaries]])[content_area==sapply(strsplit(names(panel.data[['Knots_Boundaries']][[tmp.path.knots.boundaries]]), "[.]"), '[', 1L)]
			if (length(tmp.knots.boundaries.names)==0L) {
				return(paste0("[['", tmp.path.knots.boundaries, "']]"))
			} else {
				tmp.knots.boundaries.years <- sapply(strsplit(tmp.knots.boundaries.names, "[.]"), function(x) x[2L])
				tmp.sum <- sum(year >= sort(tmp.knots.boundaries.years), na.rm=TRUE)
				return(paste0("[['", tmp.path.knots.boundaries, "']][['", paste(c(content_area, sort(tmp.knots.boundaries.years)[tmp.sum]), collapse="."), "']]"))
			}
		} else {
			return(paste0("[['", tmp.path.knots.boundaries, "']][['", content_area, ".", sgp.projections.equated[['Year']], "']]"))
		}
	}

	.get.panel.data <- function(tmp.data, grade.progression, content_area.progression, num.prior=NULL, completed.ids=NULL, bound.data=TRUE, equated.year=NULL) {
		if (is.null(num.prior)) num.prior <- length(grade.progression)
		if (is.character(tmp.data[[1L+num.panels]])) {
			tmp.data <- eval(parse(text=paste0("na.omit(tmp.data[.(", paste(rev(paste0("'", grade.progression, "'"))[seq(num.prior)], collapse=", "), "), on=names(tmp.data)[c(", paste(1+num.panels-(1:num.prior-1L), collapse=", ") , ")]], cols=names(tmp.data)[c(",paste(1L+2*num.panels-(1:num.prior-1L), collapse=", "), ")])[,c(1, ", paste(rev(1+2*num.panels-(1:num.prior-1L)), collapse=", "),  ")]")))
		} else {
			tmp.data <- eval(parse(text=paste0("na.omit(tmp.data[.(", paste(rev(grade.progression)[seq(num.prior)], collapse=", "), "), on=names(tmp.data)[c(", paste(1+num.panels-(1:num.prior-1L), collapse=", ") , ")]], cols=names(tmp.data)[c(",paste(1+2*num.panels-(1:num.prior-1L), collapse=", "), ")])[,c(1, ", paste(rev(1+2*num.panels-(1:num.prior-1L)), collapse=", "),  ")]")))
		}
		if (!is.null(completed.ids)) tmp.data <- tmp.data[!ID %in% completed.ids]

		if (bound.data) {
			if (!is.null(equated.year)) tmp.year <- equated.year else tmp.year <- as.character(sgp.labels$my.year)
			for (i in seq(dim(tmp.data)[2L]-1L)) {
				bnd <- eval(parse(text=paste0("panel.data[['Knots_Boundaries']]", get.my.knots.boundaries.path(content_area.progression[i], tmp.year),
					"[['loss.hoss_", grade.progression[i], "']]")))
				eval(parse(text=paste0("tmp.data[", names(tmp.data)[i+1L], "<bnd[1L], names(tmp.data)[i+1L] := bnd[1L]]")))
				eval(parse(text=paste0("tmp.data[", names(tmp.data)[i+1L], ">bnd[2L], names(tmp.data)[i+1L] := bnd[2L]]")))
			}
		}
		return(tmp.data)
	}

	get.my.cutscore.state.year.sgprojection <- function(Cutscores, content_area, year, my.state) {
		if (!is.na(my.state)) {
			tmp.cutscore.state <- sapply(strsplit(names(Cutscores)[grep(content_area, names(Cutscores))], "[.]"), function(x) x[2L])
			if (my.state %in% tmp.cutscore.state) {
				content_area <- paste(content_area, my.state, sep=".")
				year.split.index <- -2L
			} else year.split.index <- -1L
		} else year.split.index <- -1L

		tmp.cutscore.years <- sapply(sapply(strsplit(names(Cutscores)[grep(content_area, names(Cutscores))], "[.]"), function(x) tail(x, year.split.index)), paste, collapse=".")
		tmp.cutscore.years[tmp.cutscore.years==""] <- NA
		if (any(!is.na(tmp.cutscore.years))) {
			if (year %in% tmp.cutscore.years) {
				return(paste(content_area, year, sep="."))
			} else {
				if (year==sort(c(year, tmp.cutscore.years))[1L]) {
					return(content_area)
				} else {
					return(paste(content_area, sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1L], sep="."))
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
							sgp.exact.grade.progression,
							SGPt) {

		add.missing.taus.to.matrix <- function(my.matrix) {
			augmented.mtx <- matrix(NA, nrow=dim(my.matrix)[1L], ncol=100L)
			tau.num <- ceiling(as.numeric(substr(colnames(my.matrix), 6, nchar(colnames(my.matrix))))*100L)
			na.replace <- 1:100 %in% tau.num
			augmented.mtx[,na.replace] <- my.matrix
			return(augmented.mtx)
		}

		if (sgp.exact.grade.progression) grade.progression.index <- length(grade.progression) else grade.progression.index <- seq_along(grade.progression)

		tmp.list <- list()
		for (i in seq_along(grade.progression.index)) {
			tmp.list[[i]] <- list()
			for (j in seq_along(grade.projection.sequence)) {
				tmp.years_lags <- c(tail(year_lags.progression, grade.progression.index[i]-1L), head(year_lags.projection.sequence, j))
				if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
					tmp.years <- rep("BASELINE", length(tmp.years_lags)+1L)
				} else {
					tmp.years <- yearIncrement(sgp.labels$my.year, rev(c(0L, -cumsum(rev(tmp.years_lags)))))
				}
				tmp.matrix <- getsplineMatrices(
						tmp.matrices,
						c(tail(content_area.progression, grade.progression.index[i]), head(content_area.projection.sequence, j)),
						c(tail(grade.progression, grade.progression.index[i]), head(grade.projection.sequence, j)),
						tmp.years,
						tmp.years_lags,
						return.highest.order.matrix=TRUE,
						my.matrix.highest.order=max.order.for.progression,
						my.matrix.time.dependency=if (is.null(SGPt)) NULL else list(TIME="TIME", TIME_LAG="TIME_LAG"))
				if (length(tmp.matrix)==0L) {
					# Reverse tmp.years to find COHORT or BASELINE analog
					if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) == 0) {
						tmp.years2 <- yearIncrement(sgp.labels$my.year, rev(c(0L, -cumsum(rev(tmp.years_lags)))))
					} else {
						tmp.years2 <- rep("BASELINE", length(tmp.years_lags)+1L)
					}

					tmp.matrix <- getsplineMatrices(
						tmp.matrices,
						c(tail(content_area.progression, grade.progression.index[i]), head(content_area.projection.sequence, j)),
						c(tail(grade.progression, grade.progression.index[i]), head(grade.projection.sequence, j)),
						tmp.years2,
						tmp.years_lags,
						return.highest.order.matrix=TRUE,
						my.matrix.highest.order=max.order.for.progression,
						my.matrix.time.dependency=if (is.null(SGPt)) NULL else list(TIME="TIME", TIME_LAG="TIME_LAG"))
					if (length(tmp.matrix)==0L) next
					tmp.matrix[[1L]]@Time[[1L]] <- tmp.years  # Overwrite @Time slot to make function think the type is consistent
				}
				tmp.list[[i]][[j]] <- tmp.matrix[[1L]]
				names(tmp.list[[i]])[j] <- content_area.projection.sequence[j] # use named list to verify all matrices later
				if (dim(tmp.list[[i]][[j]]@.Data)[2L] != 100L) {
					tmp.list[[i]][[j]]@.Data <- add.missing.taus.to.matrix(tmp.list[[i]][[j]]@.Data)
					missing.taus <- TRUE
				}
			}
		}
		for (f in seq_along(tmp.list)) tmp.list[[f]] <- tmp.list[[f]][which(!sapply(tmp.list[[f]], is.null))]
		return(rev(tmp.list)) ### rev gives highest orders first
	}

	.get.percentile.trajectories <- function(ss.data, projection.matrices) {

		tmp.percentile.trajectories <- list()
		completed.ids <- TEMP_1 <- TEMP_2 <- TIME <- TIME_LAG <- TMP_KEY <- NULL

		for (i in seq_along(projection.matrices)) {
			if (any(!ss.data[[1L]] %in% completed.ids)) {
				tmp.dt <- .get.panel.data(
							ss.data,
							head(projection.matrices[[i]][[1L]]@Grade_Progression[[1L]], -1L),
							head(projection.matrices[[i]][[1L]]@Content_Areas[[1L]], -1L),
							completed.ids=completed.ids,
							equated.year=yearIncrement(sgp.projections.equated[['Year']], -1L))

				if (dim(tmp.dt)[1L] > 0L) {
					completed.ids <- c(unique(tmp.dt, by=(1L))[[1L]], completed.ids)
					tmp.dt <- tmp.dt[list(rep(tmp.dt[[1L]], 100L))]
					missing.taus <- FALSE; na.replace <- NULL # put these outside of j loop so that stays true/non-null if only SOME of coef matrices have missing column/taus.
					label.iter <- 1L

					for (j in seq_along(projection.matrices[[i]])) {
						tmp.matrix <- projection.matrices[[i]][[j]]
						mod <- character()
						int <- "data.table(ID=tmp.dt[[1L]], INT=1L,"
						for (k in seq_along(projection.matrices[[i]][[j]]@Time_Lags[[1L]])) {
							knt <- paste0("tmp.matrix@Knots[[", k, "]]")
							bnd <- paste0("tmp.matrix@Boundaries[[", k, "]]")
							mod <- paste0(mod, ", bs(tmp.dt[[", dim(tmp.dt)[2L]-k+1L, "]], knots=", knt, ", Boundary.knots=", bnd, ")")
						}

						tmp.scores <- eval(parse(text=paste0(int, substring(mod, 2L), ", key='ID')")))

						if (!is.null(SGPt)) {
							grade.projection.sequence.labels <- c(paste(tail(grade.progression, 1L), "EOW", sep="."), grade.projection.sequence)
							content_area.projection.sequence.labels <- c(tail(content_area.progression, 1L), content_area.projection.sequence)
							if (j==1L) {
								tmp.scores <- panel.data$Panel_Data[,c("ID", SGPt), with=FALSE][tmp.scores, on="ID"]
								for (k in unlist(tmp.matrix@Version[['Matrix_Information']][['SGPt']][c("MAX_TIME_PRIOR", "MAX_TIME")])) {
									tmp.scores[,TIME:=k]
									tmp.time.shift.index <- getTimeShiftIndex(as.numeric(max(tmp.scores[[SGPt]])), tmp.matrix)
									tmp.scores[,TIME_LAG:=(k+365*tmp.time.shift.index)-as.numeric(get(SGPt))]
									tmp.scores[,TMP_KEY:=1:100]
									tmp.dt[,TEMP_1:=tmp.scores[, as.matrix(.SD) %*% tmp.matrix@.Data[,TMP_KEY], by=TMP_KEY, .SDcols=3:(dim(tmp.scores)[2L]-1L)][['V1']]]

									tmp.dt[,TEMP_2:=.smooth.bound.iso.row(
											data.table(ID=tmp.dt[[1L]], X=TEMP_1),
											tail(grade.progression, 1L),
											yearIncrement(sgp.labels[['my.year']], j, lag.increment),
											content_area.projection.sequence[j],
											missing.taus=missing.taus,
											na.replace=na.replace,
											equated.year=yearIncrement(sgp.projections.equated[['Year']], -1L))]

									setnames(tmp.dt, "TEMP_2",
										paste("SS", grade.projection.sequence.labels[label.iter], content_area.projection.sequence.labels[label.iter], sep="."))
									tmp.dt[,TEMP_1:=NULL]
									label.iter <- label.iter + 1
								}
								tmp.scores[,(SGPt):=NULL]
								tmp.max.time <- k
							} else {
								tmp.scores[,TIME:=tmp.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]]
								tmp.time.shift.index <- getTimeShiftIndex(as.numeric(tmp.max.time), tmp.matrix)
								tmp.scores[,TIME_LAG:=(tmp.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]+365L*tmp.time.shift.index)-tmp.max.time]
								tmp.scores[,TMP_KEY:=1:100]
								tmp.max.time <- tmp.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]
								tmp.dt[,TEMP_1:=tmp.scores[, as.matrix(.SD) %*% tmp.matrix@.Data[,TMP_KEY], by=TMP_KEY, .SDcols=2:(dim(tmp.scores)[2L]-1L)][['V1']]]

								tmp.dt[,TEMP_2:=.smooth.bound.iso.row(
											data.table(ID=tmp.dt[[1L]], X=TEMP_1),
											grade.projection.sequence[j],
											yearIncrement(sgp.labels[['my.year']], j, lag.increment),
											content_area.projection.sequence[j],
											missing.taus=missing.taus,
											na.replace=na.replace,
											equated.year=yearIncrement(sgp.projections.equated[['Year']], -1L))]

								setnames(tmp.dt, "TEMP_2", paste("SS", grade.projection.sequence.labels[label.iter], content_area.projection.sequence.labels[label.iter], sep="."))
								tmp.dt[,TEMP_1:=NULL]
								label.iter <- label.iter + 1L
							}
						} else {
							grade.projection.sequence.labels <- grade.projection.sequence
							content_area.projection.sequence.labels <- content_area.projection.sequence
							tmp.scores[,TMP_KEY:=1:100]
							tmp.dt[,TEMP_1:=tmp.scores[, as.matrix(.SD) %*% tmp.matrix@.Data[,TMP_KEY], by=TMP_KEY, .SDcols=2:(dim(tmp.scores)[2L]-1L)][['V1']]]

							tmp.dt[,TEMP_2:=.smooth.bound.iso.row(
											data.table(ID=tmp.dt[[1L]], X=TEMP_1),
											grade.projection.sequence[j],
											yearIncrement(sgp.labels[['my.year']], j, lag.increment),
											content_area.projection.sequence[j],
											missing.taus=missing.taus,
											na.replace=na.replace,
											equated.year=yearIncrement(sgp.projections.equated[['Year']], -1L))]

							setnames(tmp.dt, "TEMP_2", paste("SS", grade.projection.sequence.labels[label.iter], content_area.projection.sequence.labels[label.iter], sep="."))
							tmp.dt[,TEMP_1:=NULL]
							label.iter <- label.iter + 1L
						}
					} ## END j loop
					setkeyv(tmp.dt, names(tmp.dt)[1L])
					tmp.percentile.trajectories[[i]] <-
						tmp.dt[,c("ID", intersect(names(tmp.dt), paste("SS", grade.projection.sequence.labels, content_area.projection.sequence.labels, sep="."))), with=FALSE]
				} ## END if (dim(tmp.dt)[1L] > 0)
			} ## END if statement
		} ## END i loop

		return(rbindlist(tmp.percentile.trajectories))
	} ## END function

	.sgp.targets <- function(data, cut, convert.0and100) {
		if (is.na(cut)) {
			return(as.integer(NA))
		} else {
			tmp <- which.min(c(data < cut, FALSE))
			if (tmp==101L) tmp <- 100L
			if (convert.0and100 && tmp==0L) return(1L)
			if (convert.0and100 && tmp==100L) return(99L)
			return(tmp)
		}
	}

	.get.trajectories.and.cuts <- function(percentile.trajectories, trajectories.tf, cuts.tf, projection.unit=projection.unit) {
		CUT <- STATE <- NULL

		if (!is.null(SGPt)) {
			content_area.projection.sequence <- c(tail(content_area.progression, 1L), content_area.projection.sequence)
			grade.projection.sequence.labels <- c(paste(tail(grade.progression, 1L), "EOW", sep="."), grade.projection.sequence)
			grade.projection.sequence <- c(tail(grade.progression, 1L), grade.projection.sequence)
		} else {
			grade.projection.sequence.labels <- grade.projection.sequence
		}

		### Trajectories

		if (trajectories.tf) {
			if (is.numeric(percentile.trajectory.values)) {
				tmp.name.prefix <- "P"
				tmp.traj <- percentile.trajectories[rep(1:100, dim(percentile.trajectories)[1L]/100L) %in% percentile.trajectory.values]
				tmp.num.years.forward <- length(grade.projection.sequence)
			}
			if (is.character(percentile.trajectory.values)) {
				tmp.name.prefix <- "SCALE_SCORE_"
				tmp.num.years.forward <- min(length(grade.projection.sequence),
					lapply(strsplit(percentile.trajectory.values, "_")[[1L]], type.convert)[sapply(lapply(strsplit(percentile.trajectory.values, "_")[[1L]], type.convert), is.numeric)][[1]])
				if (!any(grepl("CURRENT", percentile.trajectory.values))) tmp.num.years.forward <- min(length(grade.projection.sequence), tmp.num.years.forward+1L)

				tmp.indices <- as.integer(rep(dim(percentile.trajectories)[1L]/uniqueN(percentile.trajectories[['ID']])*(seq(uniqueN(percentile.trajectories[['ID']]))-1L),
					each=length(percentile.trajectory.values)) + c(t(as.matrix(data.table(panel.data[["Panel_Data"]],
					key="ID")[list(unique(percentile.trajectories, by='ID')[['ID']])][,percentile.trajectory.values, with=FALSE]))))
				tmp.traj <- percentile.trajectories[tmp.indices, 1:(2L+tmp.num.years.forward-1L), with=FALSE][,ID:=rep(unique(percentile.trajectories, by='ID')[['ID']], each=length(percentile.trajectory.values))]
				if (tmp.num.years.forward==1L) {
					tmp.target.name <- tail(names(tmp.traj), 1L)
					if ("STATE" %in% names(panel.data[["Panel_Data"]])) {
						included.states <- unique(panel.data[["Panel_Data"]][['STATE']])
						content_area.index <- grep(sgp.labels$my.subject, sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]),
							function(x) strsplit(x, "[.]")[[1L]][1L], USE.NAMES=FALSE))
						available.states <- unique(sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]),
							function(x) strsplit(x, "[.]")[[1L]][2L], USE.NAMES=FALSE)[content_area.index])
						unavailable.states <- included.states[!included.states %in% available.states]
						percentile.trajectories <- na.omit(data.table(panel.data[["Panel_Data"]][,c("ID", "STATE"), with=FALSE], key="ID")[STATE %in% available.states][percentile.trajectories], cols="STATE")
						tmp.traj <- percentile.trajectories[which(!duplicated(percentile.trajectories[['ID']]))]
						if (length(percentile.trajectory.values)==2L) tmp.traj <- data.table(rbindlist(list(tmp.traj, tmp.traj)), key="ID")

						for (state.iter in unique(tmp.traj$STATE)) {
							my.cutscore.year <- get.my.cutscore.state.year.sgprojection(Cutscores, content_area.projection.sequence[1L], yearIncrement(sgp.labels$my.year, 1L, lag.increment), my.state=state.iter)
							tmp.cutscores.by.grade <- tmp.cutscores[[my.cutscore.year]][[paste0("GRADE_", grade.projection.sequence[1L])]]
							if (length(percentile.trajectory.values)==1L) {
								tmp.state.level <- which(sapply(lapply(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscore_Information"]][['State_Levels']],
									 '[[', 1L), function(x) state.iter %in% x))
								cuku.level.to.get <- which.max(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscore_Information"]][[
									'State_Levels']][[tmp.state.level]][["Levels"]]=="Proficient")-1L
								tmp.traj[which(STATE==state.iter), (tmp.target.name):=tmp.cutscores.by.grade[cuku.level.to.get]]
							}
							if (length(percentile.trajectory.values)==2L) {
								tmp.state.level <- which(sapply(lapply(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscore_Information"]][['State_Levels']],
									 '[[', 1L), function(x) state.iter %in% x))
								cuku.level.to.get <- which.max(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscore_Information"]][[
									'State_Levels']][[tmp.state.level]][["Levels"]]=="Proficient")-1L
								musu.level.to.get <- which.max(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscore_Information"]][[
									'State_Levels']][[tmp.state.level]][["Levels"]]=="Proficient")
								tmp.traj[which(STATE==state.iter),
									(tmp.target.name):=c(tmp.cutscores.by.grade[cuku.level.to.get], tmp.cutscores.by.grade[musu.level.to.get])]
							}
						}
						tmp.traj[,STATE:=NULL]
					} else {
						my.cutscore.year <- get.my.cutscore.state.year.sgprojection(Cutscores, content_area.projection.sequence[1L], yearIncrement(sgp.labels$my.year, 1L, lag.increment), my.state=NA)
						tmp.cutscores.by.grade <- tmp.cutscores[[my.cutscore.year]][[paste0("GRADE_", grade.projection.sequence[1L])]]
						if (length(percentile.trajectory.values)==1L) {
							cuku.level.to.get <- which.max(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1L
							tmp.target.scores <- rep(tmp.cutscores.by.grade[cuku.level.to.get], uniqueN(tmp.traj[['ID']]))
						}
						if (length(percentile.trajectory.values)==2L) {
							cuku.level.to.get <- which.max(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1L
							musu.level.to.get <- which.max(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")
							tmp.target.scores <- rep(c(tmp.cutscores.by.grade[cuku.level.to.get], tmp.cutscores.by.grade[musu.level.to.get]), uniqueN(tmp.traj[['ID']]))
						}
						tmp.target.scores[is.na(tmp.traj[[tmp.target.name]])] <- NA
						tmp.traj[,(tmp.target.name):=tmp.target.scores]
					}
				}
			}
			tmp.traj[,(2:dim(tmp.traj)[2L]):=round(tmp.traj[,2:dim(tmp.traj)[2L], with=FALSE], digits=projcuts.digits)]
			trajectories <- ddcast(tmp.traj[, CUT:=rep(percentile.trajectory.values, dim(tmp.traj)[1L]/length(percentile.trajectory.values))],
						ID ~ CUT, value.var=setdiff(names(tmp.traj), c("ID", "CUT")), sep=".")
			if (any(grepl("CURRENT", percentile.trajectory.values))) percentile.trajectory.values <- unlist(strsplit(percentile.trajectory.values, "_CURRENT"))
			if (projection.unit=="GRADE") {
				tmp.vec <- expand.grid(tmp.name.prefix, percentile.trajectory.values, paste0("_PROJ_", projection.unit.label, "_"), paste(grade.projection.sequence.labels, content_area.projection.sequence, sep="_"), lag.increment.label)[1:(length(percentile.trajectory.values)*tmp.num.years.forward),]
			} else {
				tmp.vec <- expand.grid(tmp.name.prefix, percentile.trajectory.values, paste0("_PROJ_", projection.unit.label, "_"), seq_along(grade.projection.sequence.labels), lag.increment.label)[1:(length(percentile.trajectory.values)*tmp.num.years.forward),]
			}
			setnames(trajectories, c("ID", do.call(paste0, tmp.vec)))
			if (!cuts.tf) return(trajectories)
		}

		### Cuts

		if (cuts.tf) {
			setkey(percentile.trajectories, ID)
			tmp.cuts.list <- list()

			if ("STATE" %in% names(panel.data[["Panel_Data"]])) {
				included.states <- unique(panel.data[["Panel_Data"]][['STATE']]); state.arg <- "STATE == states[n.state]"
				content_area.index <- grep(sgp.labels$my.subject, sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]), function(x) strsplit(x, "[.]")[[1L]][1L], USE.NAMES=FALSE))
				available.states <- unique(sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]), function(x) strsplit(x, "[.]")[[1L]][2L], USE.NAMES=FALSE)[content_area.index])
				unavailable.states <- included.states[!included.states %in% available.states]
				percentile.trajectories <- na.omit(data.table(panel.data[["Panel_Data"]][,c("ID", "STATE"), with=FALSE], key="ID")[STATE %in% available.states][percentile.trajectories], cols="STATE")
				states <- included.states[included.states %in% available.states]
			} else {
				states <- NA; state.arg <- "is.na(STATE)"
				percentile.trajectories[, STATE:=NA]
			}

			for (n.state in seq(states)) {
				k <- 1L
				cuts.arg <- names.arg <- character()
				for (i in seq_along(grade.projection.sequence)) {
 					my.cutscore.state.year <- get.my.cutscore.state.year.sgprojection(Cutscores, content_area.projection.sequence[i], yearIncrement(sgp.labels[['my.year']], i, lag.increment), my.state=states[n.state])
 					tmp.cutscores.by.grade <- tmp.cutscores[[my.cutscore.state.year]][[paste0("GRADE_", grade.projection.sequence[i])]]

 					if (!is.null(tmp.cutscores.by.grade)) {
 						for (j in seq_along(tmp.cutscores.by.grade)) {
 							cuts.arg[k] <- paste0(".sgp.targets(SS", ".", grade.projection.sequence.labels[i], ".", content_area.projection.sequence[i], ", ", tmp.cutscores.by.grade[j], ", ", convert.0and100, ")")
							if (projection.unit=="GRADE") {
								names.arg[k] <- paste0("LEVEL_", j, "_SGP_TARGET_", projection.unit.label, "_", grade.projection.sequence.labels[i], lag.increment.label)
							} else {
								names.arg[k] <- paste0("LEVEL_", j, "_SGP_TARGET_", projection.unit.label, "_", i, lag.increment.label)
							}
							k <- k+1L
						}
					}
				}
				arg <- paste0("list(", paste(cuts.arg, collapse=", "), ")")
				tmp.cuts.list[[n.state]] <- eval(parse(text=paste0("percentile.trajectories[which(", state.arg, "),", arg, ", by=ID]")))
				setnames(tmp.cuts.list[[n.state]], c("ID", names.arg))
				if (!is.na(states[n.state])) {
					tmp.cuts.list[[n.state]][,STATE:=states[n.state]]
					setcolorder(tmp.cuts.list[[n.state]], c("ID", "STATE", names.arg))
				}
			} # End loop over states (if they exist)
			tmp.cuts <- rbindlist(tmp.cuts.list, fill=TRUE)
			if (dim(tmp.cuts)[1L]==0L) {
				if (!trajectories.tf) {
					return(NULL)
				} else {
					return(trajectories)
				}
			} else {
				setcolorder(tmp.cuts, names(tmp.cuts.list[[which.max(sapply(tmp.cuts.list, ncol))]])) # Reorder based on state with the most cutscores/proficiency levels
				setkey(tmp.cuts, ID)
				if (!trajectories.tf) {
					return(tmp.cuts)
				} else {
					return(merge(tmp.cuts, trajectories, all=TRUE))
				}
			}
		}
	}


	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	ID <- tmp.messages <- SGP_PROJECTION_GROUP <- SGP_PROJECTION_GROUP_SCALE_SCORES <- SGP_PROJECTION_GROUP_DATES <- index <- NULL

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
		if (identical(class(panel.data[["Panel_Data"]]), "data.frame")) {
			panel.data[["Panel_Data"]] <- as.data.table(panel.data[["Panel_Data"]])
	}}

	if (missing(sgp.labels)) {
		stop("User must supply a list of SGP function labels (sgp.labels). See help page for details.")
	} else {
		if (!is.list(sgp.labels)) {
			stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
		}
		if (!all(names(sgp.labels) %in% c("my.year", "my.subject")) &
				!all(names(sgp.labels) %in% c("my.year", "my.subject", "my.grade")) &
				!all(names(sgp.labels) %in% c("my.year", "my.subject", "my.extra.label")) &
				!all(names(sgp.labels) %in% c("my.year", "my.subject", "my.grade", "my.extra.label"))) {
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
			if (!use.my.knots.boundaries %in% objects(SGP::SGPstateData)) {
				stop(paste0("Knots and Boundaries are currently not implemented for the state (", use.my.knots.boundaries, ") indicated. Please contact the SGP package administrator to have your Knots and Boundaries included in the package"))
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
			if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0 & !is.null(SGP::SGPstateData[[performance.level.cutscores]][["Baseline_splineMatrix"]])) {
				panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]] <- SGP::SGPstateData[[performance.level.cutscores]][["Baseline_splineMatrix"]][["Coefficient_Matrices"]]
			} else {
				messageSGP("\t\tNOTE: Coefficient matrices indicated by argument use.my.coefficient.matrices are not included.")
				return(NULL)
			}
		}
	}
	if (missing(use.my.coefficient.matrices) & is.null(content_area.projection.sequence)) {
		if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
			tmp.path.coefficient.matrices <- paste(sgp.labels$my.subject, "BASELINE", sep=".")
			if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
				stop(paste0("\t\tNOTE: Coefficient matrices indicated by argument sgp.labels, '", tmp.path.coefficient.matrices, "', are not included. Please check supplied list to make sure appropriate coefficient matrices are included.\n"))
				return(NULL)
			}
		} else {
			tmp.path.coefficient.matrices <- tmp.path
			if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
				messageSGP(paste0("\t\tNOTE: Coefficient matrices indicated by argument sgp.labels, '", tmp.path.coefficient.matrices, "', are not included. Bypassing plot production.\n"))
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
			if (!(performance.level.cutscores %in% objects(SGP::SGPstateData))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: To use state cutscores, supply an appropriate two letter state abbreviation. \nRequested state may not be included. See help page for details.\n")
				tf.cutscores <- FALSE
			}
			if (is.null(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Cutscores are currently not implemented for the state indicated.\n\t\t\tPlease contact the SGP package administrator to have your cutscores included in the package.\n")
				tf.cutscores <- FALSE
			}
			if (!sgp.labels$my.subject %in% unique(sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]), function(x) strsplit(x, "[.]")[[1L]][1L], USE.NAMES=FALSE))) {
				tmp.messages <- c(tmp.messages, paste("\t\tNOTE: Cutscores provided in SGPstateData does not include", sgp.labels$my.subject, "(CASE SENSITIVE). See help page for details.\n"))
				tf.cutscores <- FALSE
			} else {
				tmp.cutscores <- SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]
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
		stop("\t\tNOTE: Either percentile trajectories and/or performance level cutscores must be supplied for the analyses.\n")
	}

	if (!is.null(percentile.trajectory.values) && is.numeric(percentile.trajectory.values) && !all(percentile.trajectory.values %in% 1:100)) {
		messageSGP("\t\tNOTE: Integer supplied 'percentile.trajectory.values' must be between 1 and 100. Only supplied values in that range will be used.\n")
		percentile.trajectory.values <- intersect(percentile.trajectory.values, 1:100)
	}

	if (!is.null(percentile.trajectory.values) && is.character(percentile.trajectory.values) && !all(percentile.trajectory.values %in% names(panel.data[["Panel_Data"]]))) {
		messageSGP("\t\tNOTE: Character 'percentile.trajectory.values' must correspond to individual specific variable in panel.data[['Panel_Data']]. Please check for appropriate variables.\n")
	}

	if (!is.null(achievement.level.prior.vname)) {
		if (!achievement.level.prior.vname %in% names(panel.data[["Panel_Data"]])) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied achievement.level.prior.vname is not in supplied panel.data. No ACHIEVEMENT_LEVEL_PRIOR variable will be produced.\n")
			achievement.level.prior.vname <- NULL
		}
	}

	if (lag.increment==0) lag.increment.label <- "_CURRENT" else lag.increment.label <- ""

	if (!is.null(grade.projection.sequence) & !is.null(content_area.projection.sequence) && length(grade.projection.sequence) != length(content_area.projection.sequence)) {
		stop("\t\tNOTE: Supplied 'grade.projection.sequence' and 'content_area.projection.sequence' must be of the same length.\n")
	}

	if (!is.null(grade.projection.sequence) & !is.null(year_lags.projection.sequence) && length(grade.projection.sequence)-1L != length(year_lags.projection.sequence)) {
		stop("\t\tNOTE: Supplied 'year_lags.projection.sequence' must have length 1 less than 'grade.projection.sequence'.\n")
	}

	if (is.null(projcuts.digits)) {
		projcuts.digits <- 3L
	}

	if (is.null(projection.unit.label)) {
		projection.unit.label <- projection.unit
	}

	if (!is.null(return.projection.group.dates) && is.null(SGPt)) {
		return.projection.group.dates <- NULL
	}

	if (identical(return.projection.group.dates, TRUE)) {
		return.projection.group.dates <- "DATE[.]"
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

	if ((tf.cutscores || is.character(percentile.trajectory.values)) && exists("tmp.cutscores")) {
		Cutscores <- tmp.cutscores
	}


	### Create ss.data from Panel_Data and rename variables based upon grade.progression

	if (dim(panel.data[['Panel_Data']])[1L] == 0L) { ### Check needed for getTargetScaleScore when running straight projections for final subject in progression
		tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied data together with grade progression contains no data for analysis. Check data, function arguments and see help page for details.\n")
		messageSGP(paste("\tStarted studentGrowthProjections", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label))
		messageSGP(c(tmp.messages, "\tFinished studentGrowthProjections: ", prettyDate(), " in ", convertTime(timetaken(started.at)), "\n"))

		return(
			list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
				Cutscores=panel.data[["Cutscores"]],
				Goodness_of_Fit=panel.data[["Goodness_of_Fit"]],
				Knots_Boundaries=panel.data[["Knots_Boundaries"]],
				Panel_Data=NULL,
				SGPercentiles=panel.data[["SGPercentiles"]],
				SGProjections=panel.data[["SGProjections"]],
				Simulated_SGPs=panel.data[["Simulated_SGPs"]]))
	}

	if (!missing(panel.data.vnames)) {
		if (!all(panel.data.vnames %in% names(panel.data[["Panel_Data"]]))) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied 'panel.data.vnames' are not all in the supplied 'Panel_Data'.\n\t\t\tAnalyses will utilize the variables contained in both Panel_Data and those provided in the supplied argument 'panel.data.vnames'.\n")
		}
		ss.data <- panel.data[["Panel_Data"]][,intersect(panel.data.vnames, names(panel.data[["Panel_Data"]])), with=FALSE]
	} else {
		ss.data <- panel.data[["Panel_Data"]]
	}

	if (dim(ss.data)[2L] %% 2L != 1L) {
		stop(paste("Number of columns of supplied panel data (", dim(ss.data)[2L], ") does not conform to data requirements. See help page for details."))
	}
	num.panels <- (dim(ss.data)[2L]-1L)/2L

	if (length(grade.progression) > num.panels) {
		tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Supplied 'grade progression', grade.progression=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.\n\t\t\tAnalyses will utilize maximum number of priors supplied by the data.\n"))
		grade.progression <- tail(grade.progression, num.panels)
		if (!is.null(content_area.progression)) content_area.progression <- tail(content_area.progression, length(grade.progression))
		if (!is.null(year_lags.progression)) year_lags.progression <- tail(year_lags.progression, length(grade.progression)-1L)
	}

	if (!is.null(max.order.for.progression)) {
		grade.progression <- tail(grade.progression, max.order.for.progression)
		if (!is.null(content_area.progression)) content_area.progression <- tail(content_area.progression, length(grade.progression))
		if (!is.null(year_lags.progression)) year_lags.progression <- tail(year_lags.progression, length(grade.progression)-1L)
	}

	tmp.last <- tail(grade.progression, 1L)
	ss.data <- data.table(ss.data[,c(1L, (1L+num.panels-(length(grade.progression)-1L)):(1L+num.panels), (1L+2L*num.panels-(length(grade.progression)-1L)):(1L+2L*num.panels)), with=FALSE],
			key=names(ss.data)[1L])
	num.panels <- (dim(ss.data)[2L]-1L)/2L
	setnames(ss.data, c(1L, (1L+num.panels-length(grade.progression)+1L):(1L+num.panels), (1L+2L*num.panels-length(grade.progression)+1L):(1L+2L*num.panels)),
		c("ID", paste("GD", grade.progression, content_area.progression, sep="."), paste("SS", grade.progression, content_area.progression, sep=".")))


	### Get relevant matrices for projections

	# Check to see if ALL relevant matrices exist
	if (is.logical(sgp.projections.use.only.complete.matrices)) { # if sgp.projections.use.only.complete.matrices is TRUE, change to NULL
		if (sgp.projections.use.only.complete.matrices) sgp.projections.use.only.complete.matrices <- NULL
	}
	if (is.null(sgp.projections.use.only.complete.matrices)) {
		if (any(is.na(match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]]))))) {
			tmp.fix.index <- which(is.na(match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]]))))
			# Reverse tmp.path.coefficient.matrices use of my.year/BASELINE and try that
			if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
				tmp.path.coefficient.matrices2 <- paste(unique(content_area.projection.sequence), sgp.labels$my.year, sep=".")[tmp.fix.index]
			} else {
				tmp.path.coefficient.matrices2 <- paste(unique(content_area.projection.sequence), "BASELINE", sep=".")[tmp.fix.index]
			}
			if (any(is.na(match(tmp.path.coefficient.matrices2, names(panel.data[["Coefficient_Matrices"]]))))) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Requested grade & content area progression are missing one or more coefficient matrices.\n")
				messageSGP(paste("\tStarted studentGrowthProjections", started.date))
				messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, " ", return.projection.group.identifier))
				messageSGP(paste(tmp.messages, "\tStudent Growth Projections NOT RUN", prettyDate(), "\n"))

				return(
					list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
					Cutscores=panel.data[["Cutscores"]],
					Goodness_of_Fit=panel.data[["Goodness_of_Fit"]],
					Knots_Boundaries=panel.data[["Knots_Boundaries"]],
					Panel_Data=NULL,
					SGPercentiles=panel.data[["SGPercentiles"]],
					SGProjections=panel.data[["SGProjections"]],
					Simulated_SGPs=panel.data[["Simulated_SGPs"]]))
			} else {
				if (length(grep("BASELINE", sgp.labels[['my.extra.label']])) > 0) {
					tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Not all CONTENT_AREA values in content_area.progression have associated BASELINE referenced coefficient matrices.\n\tCOHORT referenced matrices for missing content areas (",
						paste(gsub(paste0(".", sgp.labels$my.year), "", tmp.path.coefficient.matrices2), collapse=", "),
						") have been found and will be used.\n\t\tPlease note the inconsistency and ensure this is correct!\n"))
				} else {
					tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Not all CONTENT_AREA values in content_area.progression have associated COHORT referenced coefficient matrices.\n\tBASELINE referenced matrices for missing content areas (",
						paste(gsub(".BASELINE", "", tmp.path.coefficient.matrices2), collapse=", "),
						") have been found and will be used.\n\t\tPlease note the inconsistency and ensure this is correct!\n"))
				}
			}
			tmp.matrices <- unlist(panel.data[["Coefficient_Matrices"]][c(match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]])),
				match(tmp.path.coefficient.matrices2, names(panel.data[["Coefficient_Matrices"]])))], recursive=FALSE)
		} else {
			tmp.matrices <- unlist(panel.data[["Coefficient_Matrices"]][match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]]))], recursive=FALSE)
		}
	} else {
		if (any(is.na(match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]]))))) {
			tmp.fix.index <- which(is.na(match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]]))))
			tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Not all CONTENT_AREA values in content_area.progression have associated COHORT referenced coefficient matrices:\n\t\t", paste(tmp.path.coefficient.matrices[tmp.fix.index], collapse=", "), ".\n"))
		}
		tmp.match <- sort(match(tmp.path.coefficient.matrices, names(panel.data[["Coefficient_Matrices"]])))
		tmp.matrices <- unlist(panel.data[["Coefficient_Matrices"]][tmp.match], recursive=FALSE)
		tmp.tf <- content_area.projection.sequence %in% unique(unlist(lapply(tmp.matrices, function(x) x@Content_Areas))) & grade.projection.sequence %in% unique(unlist(lapply(tmp.matrices, function(x) x@Grade_Progression)))
		grade.projection.sequence <- grade.projection.sequence[tmp.tf]
		content_area.projection.sequence <- content_area.projection.sequence[tmp.tf]
	}

	### PROGRESSION SEQUENCES: content_area.progression, & year_lags.progression if not supplied

	if (is.null(content_area.progression)) {
		content_area.progression <- rep(sgp.labels[['my.subject']], length(grade.progression))
	} else {
		if (!identical(class(content_area.progression), "character")) {
			stop("content_area.progression should be a character vector. See help page for details.")
		}
		if (length(content_area.progression) != length(grade.progression)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: The content_area.progression vector does not have the same number of elements as the grade.progression vector.\n")
		}
	}

	if (is.null(year_lags.progression)) {
		year_lags.progression <- rep(1L, length(grade.progression)-1L)
	}


	### PROJECTION SEQUENCES: Calculate grade.projection.sequence, content_area.projection.sequence, and year_lags.projection.sequence if not supplied

	if (is.null(grade.projection.sequence)) {
		grade.projection.sequence <- as.character(unique(sort(type.convert(sapply(tmp.matrices, function(x) tail(slot(x, "Grade_Progression")[[1L]], 2L)), as.is=TRUE))))
	}

	if (identical(grade.projection.sequence, numeric(0))) {
		stop("Supplied grade.progression and coefficient matrices do not allow projection. See help page for details.")
	}

	if (is.null(content_area.projection.sequence)) {
		content_area.projection.sequence <- rep(tail(content_area.progression, 1L), length(grade.projection.sequence))
	}

	grade.content_area.progression <- paste(content_area.progression, paste("GRADE", grade.progression, sep="_"), sep=".")
	grade.content_area.projection.sequence <- paste(content_area.projection.sequence, paste("GRADE", grade.projection.sequence, sep="_"), sep=".")

	tmp.index <- seq(which(tail(grade.content_area.progression, 1L)==grade.content_area.projection.sequence)+1L, length(grade.projection.sequence))

	if (!is.null(max.forward.progression.grade)) {
		tmp.index <- intersect(tmp.index, which(sapply(grade.projection.sequence, function(x) type.convert(x, as.is=TRUE) <= type.convert(as.character(max.forward.progression.grade), as.is=TRUE))))
	}

	if (!is.null(max.forward.progression.years)) tmp.index <- head(tmp.index, max.forward.progression.years)

	grade.projection.sequence <- grade.projection.sequence[tmp.index]
	content_area.projection.sequence <- content_area.projection.sequence[tmp.index]

	if (is.null(year_lags.projection.sequence)) { ### NOTE same length as grade.projection.sequence for lag between progression and projection sequence
		if (is.numeric(type.convert(grade.projection.sequence))) {
			year_lags.projection.sequence <- diff(as.numeric(c(tail(grade.progression, 1L), grade.projection.sequence)))
		} else {
			year_lags.projection.sequence <- rep(1L, length(grade.projection.sequence))
		}
	} else {
		year_lags.projection.sequence <- year_lags.projection.sequence[tmp.index-1L]
	}
	grade.content_area.projection.sequence <- grade.content_area.projection.sequence[tmp.index]

	### Test to see if ss.data has cases to analyze and configuration has elements to answer

	if (dim(.get.panel.data(ss.data, grade.progression, content_area.progression, 1L, bound.data=FALSE))[1L] == 0L | length(tmp.index)==0L) {
		tmp.messages <- c(tmp.messages, "\t\tNOTE: Supplied data together with grade progression contains no data for analysis. Check data, function arguments and see help page for details.\n")
		messageSGP(paste("\tStarted studentGrowthProjections", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label))
		messageSGP(c(tmp.messages, "\tFinished studentGrowthProjections: ", prettyDate(), " in ", convertTime(timetaken(started.at)), "\n"))

		return(
			list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
				Cutscores=panel.data[["Cutscores"]],
				Goodness_of_Fit=panel.data[["Goodness_of_Fit"]],
				Knots_Boundaries=panel.data[["Knots_Boundaries"]],
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
							sgp.exact.grade.progression,
							SGPt)

	if (length(grade.projection.sequence.matrices[[1L]]) < 1L) {
		tmp.messages <- c(tmp.messages, "\t\tNOTE: Requested grade & content area progression are missing one or more coefficient matrices.\n")
		messageSGP(paste("\tStarted studentGrowthProjections", started.date))
		messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, " ", return.projection.group.identifier))
		messageSGP(paste(tmp.messages, "\tStudent Growth Projections NOT RUN", prettyDate(), "\n"))

		return(
			list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
				Cutscores=panel.data[["Cutscores"]],
				Goodness_of_Fit=panel.data[["Goodness_of_Fit"]],
				Knots_Boundaries=panel.data[["Knots_Boundaries"]],
				Panel_Data=NULL,
				SGPercentiles=panel.data[["SGPercentiles"]],
				SGProjections=panel.data[["SGProjections"]],
				Simulated_SGPs=panel.data[["Simulated_SGPs"]]))
	}

	##  Secondary check of available matrices using those selected in grade.projection.sequence.matrices
	num.matrices <- max(sapply(grade.projection.sequence.matrices, length))
	if (num.matrices < length(grade.projection.sequence)) {
		if (is.null(sgp.projections.use.only.complete.matrices)) {
			tmp.messages <- c(tmp.messages, "\t\tNOTE: Requested grade & content area progression are missing one or more coefficient matrices.\n")
			messageSGP(paste("\tStarted studentGrowthProjections", started.date))
			messageSGP(paste0("\t\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, " ", return.projection.group.identifier))
			messageSGP(paste(tmp.messages, "\tStudent Growth Projections NOT RUN", prettyDate(), "\n"))

			return(
				list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
				Cutscores=panel.data[["Cutscores"]],
				Goodness_of_Fit=panel.data[["Goodness_of_Fit"]],
				Knots_Boundaries=panel.data[["Knots_Boundaries"]],
				Panel_Data=NULL,
				SGPercentiles=panel.data[["SGPercentiles"]],
				SGProjections=panel.data[["SGProjections"]],
				Simulated_SGPs=panel.data[["Simulated_SGPs"]]))
		}
		tmp.matrices.tf <- content_area.projection.sequence %in% names(grade.projection.sequence.matrices[[1L]])
		tmp.messages <- c(tmp.messages, paste0("\t\tNOTE: Not all CONTENT_AREA values in content_area.progression have the appropriate coefficient matrices - MISSING:\n\t\t\t", paste(content_area.projection.sequence[!tmp.matrices.tf], collapse=", "), ".\n"))

		grade.projection.sequence <- grade.projection.sequence[tmp.matrices.tf]
		content_area.projection.sequence <- content_area.projection.sequence[tmp.matrices.tf]
		grade.content_area.projection.sequence <- grade.content_area.projection.sequence[tmp.matrices.tf]
	}

	### Calculate percentile trajectories

	if (dim(ss.data)[1L]/trajectories.chunk.size > 1.5) {
		percentile.trajectories <- rbindlist(lapply(.get.trajectory.chunks(seq.int(dim(ss.data)[1L])), function(index) .get.percentile.trajectories(ss.data[index], grade.projection.sequence.matrices)))
	} else {
		percentile.trajectories <- .get.percentile.trajectories(ss.data, grade.projection.sequence.matrices)
	}


	### Select specific percentile trajectories and calculate cutscores

	if (tf.cutscores) {
		tmp.cutscore.grade.content_area <- unlist(lapply(seq_along(tmp.cutscores), function(x) paste(unlist(strsplit(names(tmp.cutscores)[x], '[.]'))[1L], names(tmp.cutscores[[x]]), sep=".")))
		if ("STATE" %in% names(panel.data[["Panel_Data"]])) {
			included.states <- unique(panel.data[["Panel_Data"]][['STATE']]); state.arg <- "STATE == states[n.state]"
			content_area.index <- grep(sgp.labels$my.subject, sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]), function(x) strsplit(x, "[.]")[[1L]][1L], USE.NAMES=FALSE))
			available.states <- unique(sapply(names(SGP::SGPstateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]), function(x) strsplit(x, "[.]")[[1L]][2L], USE.NAMES=FALSE)[content_area.index])
			unavailable.states <- included.states[!included.states %in% available.states]
			if (length(unavailable.states) > 0L) {
				tmp.messages <- c(tmp.messages, paste("\t\tNOTE: The required state specific cutscores for ", sgp.labels$my.subject, " provided in SGPstateData do not include:\n\t\t\t",
					paste(unavailable.states[order(unavailable.states)], collapse = ", "), ".\n\t\t\tTarget projections will not be produced for students in these states.\n", sep = ""))
			}
			tmp.grade.content_area.projection.sequence <- sapply(available.states, function(x) paste(content_area.projection.sequence, x, paste("GRADE", grade.projection.sequence, sep="_"), sep="."))
			if (!all(tmp.grade.content_area.projection.sequence %in% tmp.cutscore.grade.content_area)) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Cutscores provided do not include cutscores for all grades/content areas in projection.\n\t\tProjections to grades/content areas without cutscores will be missing.\n")
			}
		} else {
			if (!all(grade.content_area.projection.sequence %in% tmp.cutscore.grade.content_area)) {
				tmp.messages <- c(tmp.messages, "\t\tNOTE: Cutscores provided do not include cutscores for all grades/content areas in projection.\n\t\tProjections to grades/content areas without cutscores will be missing.\n")
	}}}

	trajectories.and.cuts <- .get.trajectories.and.cuts(percentile.trajectories, !is.null(percentile.trajectory.values), tf.cutscores, toupper(projection.unit))

	if (!is.null(achievement.level.prior.vname)) {
		trajectories.and.cuts <- panel.data[["Panel_Data"]][,c("ID", achievement.level.prior.vname), with=FALSE][trajectories.and.cuts, on="ID"]
		setnames(trajectories.and.cuts, achievement.level.prior.vname, "ACHIEVEMENT_LEVEL_PRIOR")
	}

	if (!is.null(return.percentile.trajectory.values) && percentile.trajectory.values %in% names(panel.data$Panel_Data)) {
		trajectories.and.cuts <- panel.data[["Panel_Data"]][,c("ID", percentile.trajectory.values), with=FALSE][trajectories.and.cuts, on="ID"]
	}

	if (!is.null(return.projection.group.identifier)) {
		trajectories.and.cuts[,SGP_PROJECTION_GROUP:=return.projection.group.identifier]
	}

	if (!is.null(return.projection.group.scale.scores)) {
		my.tmp <- ss.data[,c("ID", grep("SS[.]", names(ss.data), value=TRUE)), with=FALSE][list(trajectories.and.cuts$ID),-1L,with=FALSE,on="ID"]
		trajectories.and.cuts[,SGP_PROJECTION_GROUP_SCALE_SCORES:=gsub("NA; ", "", do.call(paste, c(my.tmp, list(sep="; "))))]
	}

	if (!is.null(return.projection.group.dates)) {
		my.tmp <- panel.data$Panel_Data[,c("ID", grep(return.projection.group.dates, names(panel.data$Panel_Data), value=TRUE)), with=FALSE][list(trajectories.and.cuts$ID),-1L,with=FALSE,on="ID"]
		trajectories.and.cuts[,SGP_PROJECTION_GROUP_DATES:=gsub("NA; ", "", do.call(paste, c(my.tmp, list(sep="; "))))]
	}

	if ("YEAR_WITHIN" %in% names(panel.data[["Panel_Data"]])) {
		trajectories.and.cuts <- panel.data[["Panel_Data"]][,c("ID", "YEAR_WITHIN"), with=FALSE][trajectories.and.cuts, on="ID"]
	}

	##  Return GRADE value for SGP Key
	if (!is.null(sgp.labels$my.grade)) {
		trajectories.and.cuts[, GRADE := sgp.labels$my.grade]
	}

	SGProjections[[tmp.path]] <- rbindlist(list(SGProjections[[tmp.path]], trajectories.and.cuts), fill=TRUE)


	### Announce Completion & Return SGP Object

	if (print.time.taken) {
		messageSGP(paste("\tStarted studentGrowthProjections:", started.date))
		messageSGP(paste0("\t\tContent Area: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, " (N=", format(dim(trajectories.and.cuts)[1L], big.mark=","), ")"))
		messageSGP(c(tmp.messages, "\tFinished studentGrowthProjections: ", prettyDate(), " in ", convertTime(timetaken(started.at)), "\n"))
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
