\name{outputSGP}
\alias{outputSGP}
\title{Output student data and SGP results for a variety of purposes}
\description{
  Utility function used to export student data and SGP results for a variety of purposes. Current functionality exports data in wide format for data visualization purposes. See source
  code for detailed functionality. 
}

\usage{
outputSGP(sgp_object,
	state,
        output.type="SchoolView_WIDE",
        baseline.sgps=FALSE,
        outputSGP.years=NULL,
        outputSGP.content_areas=NULL,
        outputSGP.anonymize=FALSE) 
}

\arguments{
  \item{sgp_object}{An object of class SGP containing data to be exported. 
  }
  \item{state}{Acronym for which state is to be used for the lagged projections and growth to standard analyses. Function will try to guess state name from passed \code{sgp_object} is missing.
  }
  \item{output.type}{A character vector indicating what output type is requested. See source code for options.
  }
  \item{baseline.sgps}{Boolean vector indicating whether to output baseline SGPs for cohorts referenced SGPs.
  }
  \item{outputSGP.years}{A vector indicating the year to be used for output file construction.
  }
  \item{outputSGP.content_areas}{A vector indicating the content areas to be used for output file construction.
  }
  \item{outputSGP.anonymize}{A boolean variable indicating whether to anonymize output files.
  }
}

\value{Function returns an R object with data to be exported per user's desire (e.g., as a pipe delimited file). 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}}

\examples{
\dontrun{
outputData <- outputSGP(Demonstration_Data)
}  
}

\keyword{documentation}
