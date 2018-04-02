`.onLoad` <-
function(libname, pkgname) {
	setDTthreads(1)
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
<<<<<<< HEAD
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (2-15-2018). For help: >help("SGP") or visit https://sgp.io'))
=======
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (3-31-2018). For help: >help("SGP") or visit https://sgp.io'))
>>>>>>> dcc12574f9e284cb86bddb2e4f59e759135644cc
	}
}
