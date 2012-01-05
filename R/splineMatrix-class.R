setClassUnion("list.null", c("list","NULL"))
setClass("splineMatrix", contains='matrix', representation(Knots="list", Boundaries="list", Version="list.null"))
.Valid.splineMatrix <- function(object) {
       out <- NULL
       if (is.null(out)) out <- TRUE
       return(out)
}
setValidity("splineMatrix", .Valid.splineMatrix)
