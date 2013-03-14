\name{testSGP}
\alias{testSGP}
\title{Test SGP Package functions}
\description{
testSGP runs large scale tests of the SGP package to test for consistent performance across releases.
}

\usage{
testSGP(TEST_NUMBER,
	save.results=FALSE,
	memory.profile=FALSE)
}

\arguments{
  \item{TEST_NUMBER}{An integer indicating the test to be run. Type 'testSGP()' to see list and description of available tests. 
  }
  \item{save.results}{A boolean variable, defaulting to FALSE, indicating whether the results of the analysis is saved to the working directory. 
  }
  \item{memory.profile}{A boolean variable indicating whether to use memory profiling via \code{Rprof}.  Experimental. Defaults to FALSE. 
  }
}

\value{Returns output associated with functions being run.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\seealso{\code{\link{abcSGP}}}

\examples{
\dontrun{
## testSGP(1) runs abcSGP for all years in sgpData_LONG
testSGP(1)

## testSGP(2) runs abcSGP for all years in sgpData_LONG except most recent year than adds 
## in most recent year and runs current year analyses
testSGP(2)
}  
}

\keyword{documentation}
