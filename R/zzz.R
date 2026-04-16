`.onLoad` <-
function(libname, pkgname) {
	available_threads <- data.table::getDTthreads()
	data.table::setDTthreads(available_threads)
	utils::globalVariables(c("."))
}

`.onAttach` <- function(libname, pkgname) {
    if (interactive()) {
		# Utility function with timeout
        get_dev_version <- function(package, timeout = 2) {
            url <- paste0("https://raw.githubusercontent.com/CenterForAssessment/", package, "/refs/heads/master/DESCRIPTION")
            tryCatch({
                # Use curl with timeout
                response <- curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout))
                if (response$status_code == 200) {
                    lines <- strsplit(rawToChar(response$content), "\n")[[1]]
                    version_line <- grep("^Version:", lines, value = TRUE)
                    if (length(version_line) > 0) {
                        return(cyan("v", strsplit(version_line, ": ")[[1]][2], sep=""))
                    }
                }
                return(red("Not Available"))
            }, error = function(e) {
                return(red("Not Available"))
            }, warning = function(w) {
                return(red("Not Available"))
            })
        }

        # Extract version information with timeout
        installed.version <- utils::packageDescription("SGP")[['Version']]
        cran.version <- tryCatch({
            # Add timeout to CRAN check
            curl::curl_fetch_memory("https://cran.r-project.org/web/packages/SGP/index.html", 
                                  handle = curl::new_handle(timeout = 2))
            green("v", pkgsearch::cran_package("SGP")[['Version']], sep="")
        }, error = function(e) red("Not Available"),
           warning = function(w) red("Not Available"))
        dev.version <- get_dev_version("SGP")

        # Define a friendly startup message
		message_text <- paste0(
		    magenta(bold("\uD83C\uDF89 SGP v", installed.version, sep="")), " - ", toOrdinal::toOrdinalDate("2026-2-3"), "\n",
			strrep("\u2501", 40), "\n",
    	    bold("\U1F4E6 CRAN: "), cran.version, "\n",
    	    bold("\U1F527 Dev: "), dev.version, "\n",
			strrep("\u2501", 40), "\n",
		    "\U1F4A1 Tip: ", magenta(bold("> help(package=\"SGP\")")), "\n",
		    "\U1F310 Docs: ", magenta(bold("https://sgp.io")), "\n",
			strrep("\u2501", 40), "\n",
		    "\u2728 Happy SGPing!", "\n")

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
