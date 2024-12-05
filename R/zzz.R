`.onLoad` <-
function(libname, pkgname) {
	available_threads <- data.table::getDTthreads()
	data.table::setDTthreads(available_threads)
	utils::globalVariables(c("."))
}

`.onAttach` <- function(libname, pkgname) {
    if (interactive()) {

        get_dev_version <- function(package) {
            url <- paste0("https://raw.githubusercontent.com/CenterForAssessment/", package, "/refs/heads/master/DESCRIPTION")
            lines <- readLines(url, warn = FALSE)
            return(strsplit(grep("^Version:", lines, value = TRUE), ": ")[[1]][2])
        }

        # Extract version information
        installed.version <- utils::packageDescription("SGP")[['Version']]
        cran.version <- pkgsearch::cran_package("SGP")[['Version']]
        dev.version <- get_dev_version("SGP")

        # Define a friendly startup message
	message_text <- paste0(
	    magenta(bold("\uD83C\uDF89 SGP v", installed.version, sep="")), " - ", toOrdinal::toOrdinalDate("2024-12-4"), "\n",
		strrep("━", 40), "\n",
        bold("\U1F4E6 CRAN Version: "), green(cran.version), "\n",
        bold("\U1F527 Dev Version: "), cyan(dev.version), "\n",
		strrep("━", 40), "\n",
	    "\U1F4A1 Tip: ", magenta(bold("> help(package=\"SGP\")")), "\n",
	    "\U1F310 Docs: ", magenta(bold("https://sgp.io/")), "\n",
		strrep("━", 40), "\n",
	    "\u2728 Happy SGPing!"
	)

        # Display the startup message
        packageStartupMessage(message_text)
    }
}