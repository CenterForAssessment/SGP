`setNamesSGP` <-
function(
  data,
  state=NULL) {

  ### Get state (if possible)
  if (is.null(state)) {
    tmp.name <- toupper(gsub("_", " ", deparse(substitute(data))))
    state <- getStateAbbreviation(tmp.name, "prepareSGP")
  }

  variable.name.lookup <- SGP::SGPstateData[[state]][["Variable_Name_Lookup"]]
  if (is.null(variable.name.lookup)) stop("Variable name lookup must be in SGPstateData for ", state, " to use setNamesSGP")
  names.in.data <- which(variable.name.lookup[["names.provided"]] %in% names(data))
  setnames(data, variable.name.lookup[['names.provided']][names.in.data], variable.name.lookup[['names.sgp']][names.in.data])
} ### END setNamesSGP
