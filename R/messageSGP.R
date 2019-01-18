`messageSGP` <-
function(tmp.message,
	domain=NULL,
	appendLF=TRUE) {

	PrintLogMessage <- function(tmp.message, domain=NULL) {
		# print log message to file
		dir.create("Logs", showWarnings = FALSE)
		logfile <- paste0("Logs/SGP-", packageVersion("SGP"), "_", Sys.Date(), ".txt")

		if (is.call(tmp.message)) {
			tmp.message2 <- c(paste0("\n\n\t", as.character(tmp.message)[1], "(\n\t\t"), paste(names(tmp.message)[-1], as.character(tmp.message)[-1], sep=" = ", collapse="\n\t\t"), ")\n\n")
			cat(tmp.message2, file = logfile, append=TRUE)
		} else cat(tmp.message, "\n", file=logfile, sep="", append=TRUE)
	}

	if (!is.call(tmp.message)) {
		base::message(tmp.message)
	}
	PrintLogMessage(tmp.message)
	invisible()
}

timetaken <- function(started.at)
{
   if (!inherits(started.at,"proc_time")) stop("Use started.at=proc.time() (faster) not Sys.time() (POSIXt and slow)")
   secs = proc.time()[3L] - started.at[3L]
   mins = as.integer(secs) %/% 60L
   hrs = mins %/% 60L
   days = hrs %/% 24L
   mins = mins - hrs * 60L
   hrs = hrs - days * 24L
   if (secs > 60.0) {
     res = if (days>=1L) paste0(days," day", if (days>1L) "s " else " ") else ""
     paste0(res,sprintf("%02d:%02d:%02d", hrs, mins, as.integer(secs) %% 60L))
   } else {
     sprintf(if (secs >= 10.0) "%.1fsec" else "%.3fsec", secs)
   }
}
