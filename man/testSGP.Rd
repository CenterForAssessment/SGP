\name{testSGP}
\alias{testSGP}
\title{Test SGP Package functions}
\description{
testSGP runs large scale tests of the SGP package to test for consistent performance across releases.
}

\usage{
testSGP(TEST_NUMBER,
	save.results=TRUE,
	memory.profile=FALSE)
}

\arguments{
  \item{TEST_NUMBER}{An integer indicating the test to be run. Type 'testSGP()' to see list and description of available tests. 
  }
  \item{save.results}{A Boolean variable, defaulting to FALSE, indicating whether the results of the analysis is saved to the working directory. 
  }
  \item{memory.profile}{A Boolean variable indicating whether to use memory profiling via \code{Rprof}.  Experimental. Defaults to FALSE. 
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
## in most recent year and runs current year analyses using updateSGP
testSGP(2)

## testSGP(3) runs abcSGP on grade progressions including End of Course Tests in Mathematics 
## (Algebra I and Algebra II) and Reading (Grade 9 Literature and American Literature)
testSGP(3)

## testSGP(4) runs prepareSGP and analyzeSGP with simex adjustment for measurement error
testSGP(4)
}  
}

\keyword{documentation}
