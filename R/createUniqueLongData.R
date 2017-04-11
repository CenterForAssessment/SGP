`createUniqueLongData` <- function(long.data, wide.output=FALSE) {

	### Set variable to NULL to prevent R CMD Check warnings

	YEAR <- ID <- VALID_CASE <- CONTENT_AREA <- TEMP_ID <- DUP_COUNT <- EXTENDED <- NULL

	###  Utility function

	extendLongData <- function(my.tmp.data) {
	  ###   permutations function from gtools package
	  permutations <- function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) {
	    if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 0) stop("bad value of n")
	    if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 0) stop("bad value of r")
	    if (!is.atomic(v) || length(v) < n)
	      stop("v is either non-atomic or too short")
	    if ((r > n) & repeats.allowed == FALSE)
	      stop("r > n and repeats.allowed=FALSE")
	    if (set) {
	      v <- unique(sort(v))
	      if (length(v) < n) stop("too few different elements")
	    }
	    v0 <- vector(mode(v), 0)
	    if (repeats.allowed)
	    sub <- function(n, r, v) {
	      if (r == 1)
	        matrix(v, n, 1)
	      else if (n == 1)
	        matrix(v, 1, r)
	      else {
	        inner <- Recall(n, r - 1, v)
	        cbind(rep(v, rep(nrow(inner), n)), matrix(t(inner),
	          ncol = ncol(inner), nrow = nrow(inner) * n,
	          byrow = TRUE))
	      }
	    }
	    else sub <- function(n, r, v) {
	      if (r == 1)
	          matrix(v, n, 1)
	      else if (n == 1)
	          matrix(v, 1, r)
	      else {
	          X <- NULL
	          for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n - 1, r - 1, v[-i])))
	          X
	      }
	    }
	    sub(n, r, v[1:n])
	  } ### END permutations function

		if (wide.output) {
			my.tmp.data <- unique(my.tmp.data, by=c(getKey(my.tmp.data), "SCALE_SCORE"))
		}  else  my.tmp.data <- unique(my.tmp.data) # Remove EXACT duplicates & data extended in previous years/analyses
		key.vars <- intersect(names(my.tmp.data), c("YEAR", "SCALE_SCORE"))
	  setkeyv(my.tmp.data, key.vars)
	  invisible(my.tmp.data[, DUP_COUNT := seq.int(.N), by="YEAR"])
	  dups.by.yr <- my.tmp.data[, list(N = .N), by="YEAR"]
	  max.dups <- max(dups.by.yr[["N"]])
	  all.years <- sort(unique(my.tmp.data, by="YEAR")[['YEAR']])

	  tmp.perms <- data.table(permutations(n = max.dups, r = length(all.years), repeats.allowed=T))
		setkeyv(tmp.perms, rev(names(tmp.perms))) # setkeyv(tmp.perms, names(tmp.perms)[ncol(tmp.perms)]) # Make last year pretty'r
	  tmp.perms <- tmp.perms[eval(parse(text=paste0("V", seq_along(all.years), "<=", dups.by.yr[["N"]], collapse=" & ")))]
	  tmp.perms.long <- data.table(YEAR = all.years, t(tmp.perms[eval(parse(text=paste0("V", seq_along(all.years), "<=", dups.by.yr[["N"]], collapse=" & ")))])) # ID = my.tmp.data[["ID"]][1],
	  tmp.perms.long <- melt(tmp.perms.long, id.vars='YEAR', measure.vars=grep("V", names(tmp.perms.long), value=TRUE), variable.name = "EXTENDED", value.name="DUP_COUNT")
	  setkeyv(tmp.perms.long, c("YEAR", "DUP_COUNT"))
	  setkeyv(my.tmp.data, c("YEAR", "DUP_COUNT"))
	  my.tmp.data <- my.tmp.data[tmp.perms.long, allow.cartesian=TRUE]
	  invisible(my.tmp.data[, ID := paste0(ID, "_DUPS_", gsub("V", "", EXTENDED))])
	  invisible(my.tmp.data[, c('EXTENDED', 'DUP_COUNT') := NULL])
	  return(my.tmp.data)
	} # END extendLongData function


	###  Identify duplicated cases and extend long data accordingly
	tmp.key <- getKey(long.data)
	if (!"VALID_CASE" %in% names(long.data)) { # sgPlot
		invisible(long.data[, VALID_CASE := "VALID_CASE"])
		wide.output <- TRUE
	}
	dup.ids <- unique(long.data[duplicated(long.data, by=tmp.key), ID])
	dups.extended <- rbindlist(lapply(dup.ids, function(f) extendLongData(long.data[ID==f])))

	###  Combine extended and non-duplicated data into single long data object ->> return it
	all.data <- rbindlist(list(long.data[!grepl(paste(dup.ids, collapse="|"), ID),], dups.extended))

	setkeyv(all.data, tmp.key)
	return(all.data)
} ### END createUniqueLongData
