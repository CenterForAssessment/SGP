`as.splineMatrix` <- 
	function(matrix, sgp_object, knots, boundaries) {
		if (class(matrix) != "matrix") stop("Supplied object must be of class 'matrix'.")
	
		if (!missing(knots) & !missing(boundaries) & !missing(sgp_object)) stop("Only supply either the sgp_object or the list of knots and boundaries.")
	
		if (!missing(knots) & !missing(boundaries)){
			if (class(knots) != "list") stop("Supplied knots must be of class 'list'.")
			if (class(boundaries) != "list") stop("Supplied boundaries must be of class 'list'.")
			if (length(knots) != length(boundaries)) stop("Supplied lists of knots and boundaries must have equal lengths.")
			if (length(knots) != (dim(matrix)[1]-1)/7) warning("Supplied number of knots and boundaries do not match the default SGP dimensions.  Check to ensure all knots and boundaries used to produce matrices are included.")
		}
	
		if (!missing(sgp_object)) {
			rn<-rownames(matrix)[-1]
			rn <- gsub("\"", "'", rn) 
	
			rn.knots <- strsplit(rn, "knots = ",)
			rn.knots <- unique(sapply(rn.knots, function(x) strsplit(x[2], ", Boundary.")))
			rn.knots2 <- sapply(rn.knots, function(x) strsplit(x, "knots_"))
			rn.knots2 <- sapply(rn.knots2, function(x) strsplit(x[2], "'"))
			knots <- list()
			for (i in seq_along(rn.knots)) {
				knots[[i]] <- eval(parse(text=paste("sgp_object@SGP$", rn.knots[[i]], sep="")))
			}
			names(knots) <- paste("knots", sapply(rn.knots2, function(x) x[1]), sep="_")

			rn.bounds <- strsplit(rn, "Boundary.knots = ",)
			rn.bounds <- sapply(rn.bounds, function(x) strsplit(x[2], ")"))
			rn.bounds <- unique(sapply(rn.bounds, function(x) x[1]))
			rn.bounds2 <- sapply(rn.bounds, function(x) strsplit(x, "boundaries_"))
			rn.bounds2 <- sapply(rn.bounds2, function(x) strsplit(x[2], "'"))
			boundaries <- list()
                        for (i in seq_along(rn.bounds)) {
                                boundaries[[i]] <- eval(parse(text=paste("sgp_object@SGP$", rn.bounds[i], sep="")))
                        }
			names(boundaries) <- paste("boundaries", sapply(rn.bounds2, function(x) x[1]), sep="_")
		}
		version <- list(SGP_Package_Version=as.character(packageVersion("SGP")), Date_Prepared=date())
		new("splineMatrix", matrix, Knots=knots, Boundaries=boundaries, Version=version)
}
