`.onLoad` <- 
function(libname, pkgname="SparseM") {
}


`.onAttach` <- 
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage('SGP ',as.character(packageVersion("SGP")),'  For help type: help("SGP")')
	}
}
