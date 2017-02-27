`createUniqueLongData` <-
function(long.data,
	  dups.years=NULL) {

	### Set variable to NULL to prevent R CMD Check warnings

<<<<<<< HEAD
	YEAR <- ID <- VALID_CASE <- CONTENT_AREA <- NULL
=======
	YEAR <- ID <- VALID_CASE <- CONTENT_AREA <- GRADE <- NULL
>>>>>>> 3e3506ad28ca1313da6fabe9087aad32d9ad06d1


	### Initialize some settings/variables

	dups.list <- dups.extended <- list()
	tmp.key <- getKey(long.data)
	all.years <- sort(unique(long.data, by="YEAR")[['YEAR']])
	if (is.null(dups.years)) dups.years <- sort(unique(long.data[['YEAR']][duplicated(long.data, by=tmp.key)]))
<<<<<<< HEAD
=======
	tmp.last.year <- tail(all.years, 1)
>>>>>>> 3e3506ad28ca1313da6fabe9087aad32d9ad06d1


	### Create a list of duplicates for each year of the data

	for (year.iter in dups.years) {
		this.year.data <- long.data[VALID_CASE=="VALID_CASE" & YEAR==year.iter]
<<<<<<< HEAD
		dups.list[[year.iter]] <- data.table(unique(this.year.data[duplicated(this.year.data, by=tmp.key)][, tmp.key, with=FALSE], by=tmp.key)[this.year.data, nomatch=0][,setdiff(tmp.key, "YEAR"), with=FALSE], key=setdiff(tmp.key, "YEAR"))
	}


	### For each year of data, extend that data based upon duplicates in other years created in previous step

	setkey(long.data, VALID_CASE, CONTENT_AREA, ID)
=======
		if (year.iter == tmp.last.year) tmp.year.key <- tmp.key else tmp.year.key <- c(tmp.key, "GRADE") # tmp.year.key <- c("VALID_CASE", "CONTENT_AREA", "GRADE", "YEAR", "ID")
		setkeyv(this.year.data, tmp.year.key)
		dups.list[[year.iter]] <- data.table(unique(this.year.data[duplicated(this.year.data, by=tmp.year.key)][, tmp.year.key, with=FALSE], by=tmp.year.key)[this.year.data, nomatch=0][,setdiff(tmp.year.key, "YEAR"), with=FALSE], key=setdiff(tmp.year.key, "YEAR"))
		if("GRADE" %in% names(dups.list[[year.iter]])) {
			invisible(dups.list[[year.iter]][, GRADE := NULL])
			setkeyv(dups.list[[year.iter]], setdiff(tmp.key, "YEAR"))
		}
	}

	### For each year of data, extend that data based upon duplicates in other years created in previous step

	setkeyv(long.data, setdiff(tmp.key, "YEAR"))
>>>>>>> 3e3506ad28ca1313da6fabe9087aad32d9ad06d1
	for (year.iter in sort(unique(unlist(lapply(dups.years, function(x) setdiff(all.years, x)))))) {
		dups.extended[[year.iter]] <- data.table(long.data[VALID_CASE=="VALID_CASE" & YEAR==year.iter][Reduce(function(...) merge(..., all = TRUE), dups.list[intersect(setdiff(all.years, year.iter), names(dups.list))]), nomatch=0], key=c(tmp.key, "SCALE_SCORE"))
	}
	dups.all <- rbindlist(dups.extended)


	### Merge extended duplicates with modified ID together with unique cases

	all.data <- rbindlist(list(long.data[!dups.all, on=tmp.key], dups.all))
	tmp.unique.index <- all.data[!all.data[duplicated(all.data, by=tmp.key)], on=tmp.key, which=TRUE]
<<<<<<< HEAD
	all.data[, ID:=paste(ID, "DUPS", seq.int(.N), sep="_"), by=list(VALID_CASE, CONTENT_AREA, YEAR, ID)]
	all.data[tmp.unique.index, ID:=gsub("_DUPS_[0-9]*", "", ID)]
	setkeyv(all.data, tmp.key)

#### OLD FUNCTION
#YEAR <- ID <- NULL

#tmp.key <- key(long.data)
#tmp.last.year <- tail(sort(unique(long.data[['YEAR']])), 1)
#tmp.last.year.data <- long.data[YEAR==tmp.last.year]
#tmp.dups.index <- data.table(unique(tmp.last.year.data[duplicated(tmp.last.year.data, by=tmp.key)][, tmp.key, with=FALSE], by=tmp.key)[tmp.last.year.data, nomatch=0][,setdiff(tmp.key, "YEAR"), with=FALSE], key=setdiff(tmp.key, "YEAR"))
#tmp.dups.index <- data.table(unique(long.data[duplicated(long.data, by=tmp.key)][, tmp.key, with=FALSE], by=tmp.key)[long.data, nomatch=0][,setdiff(tmp.key, "YEAR"), with=FALSE], key=setdiff(tmp.key, "YEAR"))
#setkeyv(long.data, setdiff(tmp.key, "YEAR"))
#tmp.unique.data <- long.data[!tmp.dups.index]
#tmp.past.dups.extended <- long.data[YEAR!=tmp.last.year][tmp.dups.index, allow.cartesian=TRUE, nomatch=0]
#tmp.current.dups <- data.table(long.data[unique(tmp.dups.index, by=key(tmp.dups.index))][YEAR==tmp.last.year], key=setdiff(tmp.key, "YEAR"))
#tmp.all.dups.extended <- data.table(rbindlist(list(tmp.past.dups.extended, tmp.current.dups)), key=tmp.key)
#tmp.all.dups.extended[,ID:=paste(ID, "DUPS", tmp.all.dups.extended[,seq.int(.N), by=eval(tmp.key)][['V1']], sep="_")]
#return(data.table(rbindlist(list(tmp.unique.data, tmp.all.dups.extended)), key=tmp.key))

=======
	# all.data[, ID:=paste(ID, "DUPS", seq.int(.N), sep="_"), by=list(VALID_CASE, CONTENT_AREA, YEAR, ID)]
	all.data[YEAR==tmp.last.year, TEMP_ID := paste(ID, "DUPS", seq.int(.N), sep="_"), by=list(VALID_CASE, CONTENT_AREA, YEAR, ID)]
	all.data[YEAR!=tmp.last.year, TEMP_ID := paste(ID, "DUPS", seq.int(.N), sep="_"), by=list(VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)]
	all.data[tmp.unique.index, TEMP_ID := gsub("_DUPS_[0-9]*", "", ID)]
	all.data[, ID := TEMP_ID]
	all.data[, TEMP_ID := NULL]
	setkeyv(all.data, tmp.key)
	return(all.data)
>>>>>>> 3e3506ad28ca1313da6fabe9087aad32d9ad06d1
} ### END createUniqueLongData
