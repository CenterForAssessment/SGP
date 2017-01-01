`.onLoad` <-
function(libname, pkgname) {
	if (packageVersion('data.table') >= '1.9.7') eval(parse(text="invisible(setDTthreads(1))"))
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('SGP',paste(paste0(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", "")), collapse=""),' (1-1-2017). For help: >help("SGP") or visit https://github.com/CenterForAssessment/SGP/wiki'))
	}
}
