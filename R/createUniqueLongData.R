`createUniqueLongData` <-
function(long.data,
	  dups.years=NULL) {

	### Set variable to NULL to prevent R CMD Check warnings

	YEAR <- ID <- VALID_CASE <- CONTENT_AREA <- NULL


	### Initialize some settings/variables

	dups.list <- dups.extended <- list()
	tmp.key <- getKey(long.data)
	all.years <- sort(unique(long.data, by="YEAR")[['YEAR']])
	if (is.null(dups.years)) dups.years <- sort(unique(long.data[['YEAR']][duplicated(long.data, by=tmp.key)]))


	### Create a list of duplicates for each year of the data

	for (year.iter in dups.years) {
		this.year.data <- long.data[VALID_CASE=="VALID_CASE" & YEAR==year.iter]
		dups.list[[year.iter]] <- data.table(unique(this.year.data[duplicated(this.year.data, by=tmp.key)][, tmp.key, with=FALSE], by=tmp.key)[this.year.data, nomatch=0][,setdiff(tmp.key, "YEAR"), with=FALSE], key=setdiff(tmp.key, "YEAR"))
	}


	### For each year of data, extend that data based upon duplicates in other years created in previous step

	setkey(long.data, VALID_CASE, CONTENT_AREA, ID)
	for (year.iter in sort(unique(unlist(lapply(dups.years, function(x) setdiff(all.years, x)))))) {
		dups.extended[[year.iter]] <- data.table(long.data[VALID_CASE=="VALID_CASE" & YEAR==year.iter][Reduce(function(...) merge(..., all = TRUE), dups.list[intersect(setdiff(all.years, year.iter), names(dups.list))]), nomatch=0], key=c(tmp.key, "SCALE_SCORE"))
	}
	dups.all <- rbindlist(dups.extended)


	### Merge extended duplicates with modified ID together with unique cases

	all.data <- rbindlist(list(long.data[!dups.all, on=tmp.key], dups.all))
	tmp.unique.index <- all.data[!all.data[duplicated(all.data, by=tmp.key)], on=tmp.key, which=TRUE]
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

} ### END createUniqueLongData
