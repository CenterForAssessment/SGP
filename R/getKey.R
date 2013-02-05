`getKey` <- 
function(sgp_object) {
	if (is.SGP(sgp_object)) {
		if ("YEAR_WITHIN" %in% names(sgp_object@Data)) c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "YEAR_WITHIN") else c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
	}
	if (is.data.table(sgp_object)) {
		if ("YEAR_WITHIN" %in% names(sgp_object)) c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "YEAR_WITHIN") else c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
	}
} ### END getKey
