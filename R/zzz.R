`.onLoad` <-
function(libname, pkgname) {
	if (packageVersion('data.table') >= '1.9.7') eval(parse(text="invisible(setthreads(1))"))
}


`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
<<<<<<< HEAD
		packageStartupMessage(magenta$bold('SGP',paste(paste(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", ""), sep=""), collapse=""),' (8-12-2016).  For help: >help("SGP") or visit https://github.com/CenterForAssessment/SGP/wiki'))
=======
		packageStartupMessage(magenta$bold('SGP',paste(paste(unlist(strsplit(as.character(packageVersion("SGP")), "[.]")), c(".", "-", ".", ""), sep=""), collapse=""),' (8-12-2016).  For help: >help("SGP") or visit https://github.com/CenterForAssessment/SGP/wiki'))
>>>>>>> e7ca43e18d48a7f0a7fb849b9ed76a1998c68e7e
	}
}
