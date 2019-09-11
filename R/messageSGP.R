`messageSGP` <-
function(tmp.message,
	domain=NULL,
	appendLF=TRUE) {

	PrintLogMessage <- function(tmp.message, domain=NULL) {
		# print log message to file
		dir.create("Logs", showWarnings = FALSE)
		logfile <- paste0("Logs/SGP-", packageVersion("SGP"), "_", Sys.Date(), ".txt")

		if (is.call(tmp.message)) {
			tmp.message2 <- c(paste0("\n\n\t", as.character(tmp.message)[1L], "(\n\t\t"), paste(names(tmp.message)[-1L], as.character(tmp.message)[-1L], sep=" = ", collapse="\n\t\t"), ")\n\n")
			cat(tmp.message2, file = logfile, append=TRUE)
		} else cat(tmp.message, "\n", file=logfile, sep="", append=TRUE)
	}

	if (!is.call(tmp.message)) {
		base::message(tmp.message)
	}
	PrintLogMessage(tmp.message)
	invisible()
}
