`.onLoad` <- 
function(libname, pkgname) {
}


`.onAttach` <- 
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage('SGP ',paste(paste(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", ""), sep=""), collapse=""),' (6-24-2019).  For help: >help("SGP") or visit https://github.com/CenterForAssessment/SGP/wiki')
	}
}
