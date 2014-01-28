\name{createKnotsBoundaries}
\alias{createKnotsBoundaries}
\title{Function to create Knots and Boundaries from supplied data in LONG format.}
\description{
The function createKnotsBoundaries creates Knots, Boundaries and Loss/Hoss scores for subsequent use and embedding in SGPstateData. Function can be called separately but is usually called as part of 
\code{\link{prepareSGP}}. See examples below.
}

\usage{
createKnotsBoundaries(tmp.data,
	 knot.cut.percentiles=c(0.2,0.4,0.6,0.8))
}

\arguments{
  \item{tmp.data}{Data supplied to function in LONG format. See sgpData_LONG for an exemplar. tmp.data must contain, at least, variables 'VALID_CASE', 'CONTENT_AREA', 'SCALE_SCORE', 'GRADE'. 
  }
  \item{knot.cut.percentiles}{A numeric vector of quantiles of 'SCALE_SCORE' to be used as the cut points. Default is to use the 20th, 40th, 60th, and 80th percentiles (i.e., c(0.2,0.4,0.6,0.8)).  
  }
}

\value{Returns a list containing Knots, Boundaries and Loss/Hoss scores.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
\dontrun{
### Run on supplied long data

DEMO_Knots_Boundaries <- createKnotsBoundaries(sgpData_LONG)

### Run as part of prepareSGP

### First NULL out knots and boundaries embedded in SGPstateData

SGPstateData[["DEMO"]][["Achievement"]][["Knots_Boundaries"]] <- NULL
Demonstration_SGP <- prepareSGP(sgpData_LONG)
}  
}

\keyword{documentation}
