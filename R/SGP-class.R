setClass("ArrowSGP") # "the member classes must be defined" for `setClassUnion`
setClass("FileSystemDataset") # "the member classes must be defined" for `setClassUnion`
setClass("Table") # "the member classes must be defined" for `setClassUnion`

setClassUnion("list.null", c("list", "NULL"))
setClassUnion("sgp.data", c("data.table", "ArrowSGP", "Table", "FileSystemDataset"))

setOldClass(c("data.frame"))
setOldClass(c("data.table", "data.frame"))
setClass(
	"SGP",
	representation(
		Data = "sgp.data",
		Data_Supplementary = "list.null",
		Names = "list.null",
		SGP = "list.null",
		Summary = "list.null",
		Version = "list.null"
	)
)

.Valid.SGP <- function(object) {
	out <- NULL
	
	## RUN A SET OF CHECKS ON THE SPECIFIED VARIABLES TO ENSURE THAT THE
	## SCORES ARE WITHIN ACCEPTABLE LIMITS AND THE VARIABLES HAVE THE
	## PROPER VARIABLE TYPE

	if (is.null(out)) out <- TRUE
	return(out)
	
}
setValidity("SGP", .Valid.SGP)
