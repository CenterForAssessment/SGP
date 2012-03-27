\name{outputSGP}
\alias{outputSGP}
\title{Output student data and SGP results for a variety of purposes}
\description{
  Utility function used to export student data and SGP results for a variety of purposes. Current functionality exports data in wide format for data visualization purposes. See source
  code for detailed functionality. 
}

\usage{
outputSGP(sgp_object,
	state=NULL,
        output.type="SchoolView",
        baseline.sgps=FALSE,
        outputSGP_SUMMARY.years=NULL,
        outputSGP_SUMMARY.content_areas=NULL,
        outputSGP_INDIVIDUAL.years=NULL,
        outputSGP_INDIVIDUAL.content_areas=NULL,
        outputSGP.anonymize=FALSE,
        outputSGP.student.groups=NULL,
        outputSGP.directory=file.path("Data", "SchoolView"))
}

\arguments{
  \item{sgp_object}{An object of class SGP containing data to be exported. 
  }
  \item{state}{Acronym for which state is to be used for the lagged projections and growth to standard analyses. Function will try to guess state name from passed \code{sgp_object} is missing.
  }
  \item{years}{A vector indicating the years to be included in the output. Default is to use all years.
  }
  \item{content_areas}{A vector indicating the content areas to be included in the output. Default is to use all content areas.
  }
  \item{output.type}{A character vector indicating what output type is requested. See source code for options.
  }
  \item{baseline.sgps}{Boolean vector indicating whether to output baseline SGPs for cohorts referenced SGPs.
  }
  \item{outputSGP_SUMMARY.years}{A vector indicating the year to be used for output file construction for summary tables.
  }
  \item{outputSGP_SUMMARY.content_areas}{A vector indicating the content areas to be used for output file construction for summary tables.
  }
  \item{outputSGP_INDIVIDUAL.years}{A vector indicating the year to be used for output file construction for individual level file.
  }
  \item{outputSGP_INDIVIDUAL.content_areas}{A vector indicating the content areas to be used for output file construction for individual level file.
  }
  \item{outputSGP.anonymize}{A boolean variable indicating whether to anonymize output files.
  }
  \item{outputSGP.student.groups}{A list of variables to be used for student groups in individual and summary tables.
  }
  \item{outputSGP.directory}{A a file path indicating where to save output files.
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
