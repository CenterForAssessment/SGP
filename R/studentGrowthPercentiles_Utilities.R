###   Utility Functions for studentGrowthPercentiles

getSIMEXdata <- function(dbase, B, k=NULL, predictions=FALSE) {
	if (predictions) {
		readRDS(file.path(dbase, paste0("simex_data_", B, ".rds")))[,paste(c("ID", paste0('prior_', k:1), "final_yr")), with=FALSE]
	} else {
		readRDS(file.path(dbase, paste0("simex_data_", B, ".rds")))
	}
}

rq_sgp <- function(formula, tau, data, qr_method) {
	if (qr_method == "br") {
		tmp.res <- quantreg::rq(formula, tau, data, method="br")[['coefficients']]
	} else {
		tmp.res <- try(quantreg::rq(formula, tau, data, method=qr_method)[['coefficients']], silent=TRUE)
		if(class(tmp.res) == "try-error") {
			tmp.res <- quantreg::rq(formula, tau, data, method="br")[['coefficients']]
		}
	}
	return(tmp.res)
}

# if (is.null(rqdata)) {
# 	if (!is.null(sample.index)) {
# 		rqdata <- SGP:::getSIMEXdata(dbase=tmp.dbname, z)[sample.index]
# 	} else rqdata <- SGP:::getSIMEXdata(dbase=tmp.dbname, z)
# }

rq_mtx <- function(tmp.gp.iter, k, lam, z, rqdata=NULL, sample.index=NULL, tmp.dbname, knots.boundaries.path, SGPt, rq.method, taus, Knots_Boundaries, matrix.info) {
	suppressPackageStartupMessages(require(SGP))

	mod <- character()
	s4Ks <- "Knots=list("
	s4Bs <- "Boundaries=list("
	for (i in seq_along(tmp.gp.iter)) {
		knt <- paste0("Knots_Boundaries", knots.boundaries.path, "[['Lambda_", lam, "']][['knots_", tmp.gp.iter[i], "']]")
		bnd <- paste0("Knots_Boundaries", knots.boundaries.path, "[['Lambda_", lam, "']][['boundaries_", tmp.gp.iter[i], "']]")
		mod <- paste0(mod, " + splines::bs(prior_", i, ", knots=", knt, ", Boundary.knots=", bnd, ")")
		s4Ks <- paste0(s4Ks, "knots_", tmp.gp.iter[i], "=", knt, ",")
		s4Bs <- paste0(s4Bs, "boundaries_", tmp.gp.iter[i], "=", bnd, ",")
	}

	tmp.mtx.list <- list()
	for (zz in z) {
		if (!is.null(sample.index)) {
			rqdata <- SGP:::getSIMEXdata(dbase=tmp.dbname, B=zz)[sample.index[[which(zz == z)]]]
		} else rqdata <- SGP:::getSIMEXdata(dbase=tmp.dbname, B=zz)

		tmp.mtx <- eval(parse(text=paste0("SGP:::rq_sgp(final_yr ~", substring(mod,4), ", taus, rqdata, rq.method)")))

		tmp.version <- list(
				SGP_Package_Version=as.character(packageVersion("SGP")),
				Date_Prepared=SGP:::prettyDate(),
				Matrix_Information=list(
					N=dim(rqdata)[1L],
					data_digest = digest::digest(rqdata),
					matrix_digest = digest::digest(as.numeric(tmp.mtx)),
					sample_digest = digest::digest(sample.index[[which(zz == z)]]),
					B=zz,
					Model=paste0("rq_sgp(formula=final_yr ~", substring(mod,4), ", tau = taus, data = rqdata, qr_method = ", rq.method, ")"),
					SGPt=if (is.null(SGPt)) NULL else list(VARIABLES=unlist(SGPt), MAX_TIME=max(rqdata$TIME, na.rm=TRUE), MAX_TIME_PRIOR=max(rqdata$TIME-rqdata$TIME_LAG, na.rm=TRUE), RANGE_TIME_LAG=range(rqdata$TIME_LAG))))

		tmp.mtx.list[[which(zz == z)]] <- eval(parse(text=paste0("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1L, nchar(s4Ks)-1L), "), ", substring(s4Bs, 1L, nchar(s4Bs)-1L), "), ",
			"Content_Areas=list(as.character(tail(matrix.info[['cap']], k+1L))), ",
			"Grade_Progression=list(as.character(tail(matrix.info[['gp']], k+1L))), ",
			"Time=list(as.character(tail(matrix.info[['yp']], k+1L))), ",
			"Time_Lags=list(as.numeric(tail(matrix.info[['ypl']], k))), ",
			"Version=tmp.version)")))
	}
	if (length(z) > 1) return(tmp.mtx.list) else return(unlist(tmp.mtx.list[[zz]]))
}

chunk_list <- function(workers, values.to.chunk, big.trim=TRUE)  {
	if (workers > 3) {
		if (workers %in% 4:10) {
			if (big.trim) tmp.trim <- 0.75 else tmp.trim <- 0.95
			tmp.sml <- ceiling((length(values.to.chunk) / workers)*tmp.trim)
			tmp.lrg <- ceiling((length(values.to.chunk)-(2*tmp.sml))/(workers-2))
			chunk.size <- c(tmp.sml, rep(tmp.lrg, (workers-2)), tmp.sml)
			if (sum(chunk.size) > length(values.to.chunk)) {
				over <- (sum(chunk.size) - length(values.to.chunk)); index <- 0
				while(over != 0) {
					if (over %% 2 == 0) {
						index <- index + 1
						chunk.size[(length(chunk.size)-(index))] <- chunk.size[(length(chunk.size)-(index))]-1
					} else chunk.size[(index + 1)] <- chunk.size[(index + 1)]-1
					over <- over - 1
				}
			}
		}
		if (workers > 10) {
			if (big.trim) {tmp.trim1 <- 0.334; tmp.trim2 <- 0.666} else {tmp.trim1 <- 0.85; tmp.trim2 <- 0.95}
			tmp.sml.a <- ceiling((length(values.to.chunk) / workers)*tmp.trim1)
			tmp.sml.b <- ceiling((length(values.to.chunk) / workers)*tmp.trim2)
			tmp.lrg <- ceiling((length(values.to.chunk)-(2*sum(tmp.sml.a, tmp.sml.b)))/(workers-4))
			chunk.size <- c(tmp.sml.a, tmp.sml.b, rep(tmp.lrg, (workers-4)), tmp.sml.b, tmp.sml.a)
			if (sum(chunk.size) > length(values.to.chunk)) {
				over <- (sum(chunk.size) - length(values.to.chunk)); index <- 0
				while(over != 0) {
					if (over %% 2 != 0) {
						index <- index + 1
						chunk.size[(length(chunk.size)-(index + 1))] <- chunk.size[(length(chunk.size)-(index + 1))]-1
					} else chunk.size[(index + 2)] <- chunk.size[(index + 2)]-1
					over <- over -1
				}
			}
		}
		if (workers > length(values.to.chunk)) chunk.size <- rep(1, length(values.to.chunk))
	}	else chunk.size <- rep(ceiling(length(values.to.chunk) / workers), workers)

	CHUNK.LIST <- vector("list", workers)
	count <- index <- 1
	for (ch in chunk.size) {
		CHUNK.LIST[[index]] <- values.to.chunk[count:(count+ch-1)]
		count <- (count+ch); index <- index + 1
	}
	if (sum(chunk.size) > length(values.to.chunk)) for(l in 1:length(CHUNK.LIST))  CHUNK.LIST[[l]] <- CHUNK.LIST[[l]][!is.na(CHUNK.LIST[[l]])]
	return(CHUNK.LIST)
}

# my.list <- chunk_list(8, sim.iters, FALSE)

smooth_bound_iso_row <- function(tmp.dt, iso, sgp.loss.hoss.adjustment, knots.boundaries.path, Knots_Boundaries, final.grade) {
	X <- ID <- NULL
	if (!is.null(sgp.loss.hoss.adjustment)) {
		# knots.boundaries.path <- get.my.knots.boundaries.path(sgp.labels[['my.subject']], as.character(sgp.labels[['my.year']]))
		bnd <- eval(parse(text=paste0("Knots_Boundaries", knots.boundaries.path, "[['loss.hoss_", final.grade, "']]")))
		tmp.dt[X < bnd[1L], X:=bnd[1L]]
		tmp.dt[X > bnd[2L], X:=bnd[2L]]
	}
	if (iso) setkey(tmp.dt, ID, X)
	return(tmp.dt[['X']])
}

get_percentile_predictions <- function(my.matrix=NULL, my.data=NULL, dbname, Bz, k, taus, isotonize, sgp.loss.hoss.adjustment, knots.boundaries.path, Knots_Boundaries, final.grade, SGPt=NULL, SGPt.max.time=NULL, Panel_Data=NULL) {

	if (is.null(my.matrix)) my.matrix <- readRDS(file.path(dbname, paste0("simex_matrix_", Bz, ".rds")))
	if (is.null(my.data)) my.data <- SGP:::getSIMEXdata(dbase=dbname, B=Bz, k=k, predictions=TRUE)

	SCORE <- TIME <- TIME_LAG <- NULL
	mod <- character()
	for (k in seq_along(my.matrix@Time_Lags[[1L]])) {
		knt <- paste0("my.matrix@Knots[[", k, "]]")
		bnd <- paste0("my.matrix@Boundaries[[", k, "]]")
		mod <- paste0(mod, ", splines::bs(my.data[[", dim(my.data)[2L]-k, "]], knots=", knt, ", Boundary.knots=", bnd, ")")
	}
	if (!is.null(SGPt)) {
		my.data <- Panel_Data[,c("ID", "TIME", "TIME_LAG"), with=FALSE][my.data, on="ID"][,c(names(my.data), "TIME", "TIME_LAG"), with=FALSE]
		tmp.time.shift.index <- getTimeShiftIndex(max(as.numeric(my.data[['TIME']])), my.matrix)
		if (max(my.data$TIME+365*-tmp.time.shift.index, na.rm=TRUE) > my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]+30) stop("Matrix Misfit with TIME data!!!")
		if (is.null(SGPt.max.time) && tmp.time.shift.index != 0) my.data[,TIME:=TIME+365*-tmp.time.shift.index]
		if (!is.null(SGPt.max.time)) {
		    my.data[,TIME_LAG:=TIME_LAG+my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]-(TIME+365*-tmp.time.shift.index)]
		    my.data[,TIME:=my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]]
		}
		mod <- paste0(mod, ", my.data[['TIME']], my.data[['TIME_LAG']]")
	}
  tmp <- eval(parse(text=paste0("cbind(1L, ", substring(mod, 2L), ") %*% my.matrix")))
  return(round(matrix(SGP:::smooth_bound_iso_row(data.table::data.table(ID=rep(seq.int(dim(tmp)[1L]), each=length(taus)), X=c(t(tmp))), isotonize, sgp.loss.hoss.adjustment, knots.boundaries.path, Knots_Boundaries, final.grade), ncol=length(taus), byrow=TRUE), digits=5L))
}
