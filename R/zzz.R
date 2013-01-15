`.onLoad` <- 
function(libname, pkgname="SparseM") {
}


`.onAttach` <- 
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage('SGP ',paste(paste(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", ""), sep=""), collapse=""),'  For help type: help("SGP")')
	}
}
