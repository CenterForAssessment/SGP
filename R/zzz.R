`.onLoad` <-
function(libname, pkgname) {
	setDTthreads(1)
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (11-21-2017). For help: >help("SGP") or visit https://sgp.io'))
	}
}
