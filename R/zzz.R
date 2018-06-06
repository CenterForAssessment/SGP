`.onLoad` <-
function(libname, pkgname) {
	setDTthreads(1)
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
<<<<<<< HEAD
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (6-6-2018). For help: >help("SGP") or visit sgp.io'))
=======
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (6-5-2018). For help: >help("SGP") or visit https://sgp.io'))
>>>>>>> d1f7cb00f36a0fcc4df0e5c7a97e4ff847f1f1c1
	}
}
