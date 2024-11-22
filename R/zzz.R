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
            magenta(bold("🎉 SGP v", version)), " - ", toOrdinal::toOrdinalDate(as.character(Sys.Date())), "\n",
            "💡 Tip: ", magenta(bold("> help(\"SGP\")")), "\n",
            "🌐 Docs: ", magenta(bold("https://sgp.io/")), "\n",
            "✨ Happy SGPing!"
        )

        # Display the startup message
        packageStartupMessage(message_text)
    }
}
