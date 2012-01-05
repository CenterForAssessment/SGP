\name{combineSGP}
\alias{combineSGP}
\title{Combine student data and SGP results}
\description{
  Utility function/exemplar used to merge student long data with results from student growth percentiles and/or student growth projections calculations. Default values of this
  function are designed to be used following use of other utility functions: \code{\link{prepareSGP}} and \code{\link{analyzeSGP}}.
}

\usage{
combineSGP(sgp_object,
	state,
	years,
	content_areas,
	sgp.percentiles=TRUE,
        sgp.percentiles.baseline=TRUE,
	sgp.projections.lagged=TRUE,
        sgp.projections.lagged.baseline=TRUE,
	max.lagged.sgp.target.years.forward=4,
	sgp.target.to.NA=TRUE)
}

\arguments{
  \item{sgp_object}{A list containing both \code{Student} (from \code{prepareSGP}) and \code{SGP} (from \code{analyzeSGP}) slots. 
  }
  \item{state}{Acronym for which state is to be used for the lagged projections and growth to standard analyses. Function will try to guess state name from passed \code{sgp_object} is missing.
  }
  \item{years}{A vector of years indicating years of data to merge with \code{@Data}. If missing, merge will use all available years of student growth percentile results.
  }
  \item{content_areas}{A vector of content areas indicating content areas of student growth percentile data to merge with \code{@Data}. If missing, merge will use all available content areas of
student growth percentile results.
  }
  \item{sgp.percentiles}{A boolean variable indicating whether to combine student growth percentiles.
  }
  \item{sgp.percentiles.baseline}{A boolean variable indicating whether to combine baseline student growth percentiles.
  }
  \item{sgp.projections.lagged}{A boolean variable indicating whether to combine lagged student growth projections and calculate catch-up/keep-up values.
  }
  \item{sgp.projections.lagged.baseline}{A boolean variable indicating whether to combine lagged baseline student growth projections and calculate catch-up/keep-up values.
  }
  \item{max.lagged.sgp.target.years.forward}{A integer indicating the number of years forward from the lagged (last year's) score to project forward for growth to standard calculations. Default is 4 years from last year or 3 years from present, which is the standard in most growth to standard calculations used by state departments of education.
  }
  \item{sgp.target.to.NA}{A boolean variable (defaults to TRUE) indicating whether to change SGP_TARGET and SGP_TARGET_BASELINE to NA in instances where a target has been calculated but no SGP is calculated (for example, with retained students). It is generally recommended to change such values to NA so that summary comparisions of median SGP to median SGP_TARGET are based upon the same students.
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a \code{data.table} keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} merged with student growth percentiles and/or lagged projection targets and catch up/keep up status.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}}

\examples{
\dontrun{
## combineSGP is Step 3 of 5 of abcSGP
Demonstration_Data <- sgpData_LONG
Demonstration_Data <- prepareSGP(Demonstration_Data)
Demonstration_Data <- analyzeSGP(Demonstration_Data)
Demonstration_Data <- combineSGP(Demonstration_Data)
}  
}

\keyword{documentation}
