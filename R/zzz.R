`.onLoad` <-
function(libname, pkgname) {
	if (packageVersion('data.table') >= '1.9.7') eval(parse(text="invisible(setthreads(1))"))
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('SGP',paste(paste(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", ""), sep=""), collapse=""),' (8-3-2016).  For help: >help("SGP") or visit https://github.com/CenterForAssessment/SGP/wiki'))
	}
}
