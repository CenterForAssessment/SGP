`.onLoad` <-
function(libname, pkgname) {
	available_threads <- data.table::getDTthreads()
	data.table::setDTthreads(available_threads)
	utils::globalVariables(c("."))
}

.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        # Extract version information
        version <- utils::packageVersion("SGP")

        # Define a friendly startup message
	message_text <- paste0(
	    magenta(bold("\uD83C\uDF89 SGP v", version)), " - ", toOrdinal::toOrdinalDate(as.character(Sys.Date())), "\n",
	    "\U1F4A1 Tip: ", magenta(bold("> help(\"SGP\")")), "\n",
	    "\U1F310 Docs: ", magenta(bold("https://sgp.io/")), "\n",
	    "\u2728 Happy SGPing!"
	)

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
