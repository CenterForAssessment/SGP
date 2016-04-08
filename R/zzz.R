`.onLoad` <-
function(libname, pkgname) {
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('SGP',paste(paste(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", ""), sep=""), collapse=""),' (4-8-2016).  For help: >help("SGP") or visit https://github.com/CenterForAssessment/SGP/wiki'))
	}
}
