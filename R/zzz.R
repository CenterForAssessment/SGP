`.onLoad` <-
function(libname, pkgname) {
	setDTthreads(1)
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (10-23-2023). For help: >help("SGP") or visit sgp.io'))
	}
}
