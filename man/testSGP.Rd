\name{testSGP}
\alias{testSGP}
\title{Test SGP Package functions}
\description{
testSGP runs large scale tests of the SGP package to test for consistent performance across releases.
}

\usage{
testSGP(TEST_NUMBER)
}

\arguments{
  \item{TEST_NUMBER}{An integer indicating the test to be run. Type 'testSGP()' to see list and descritpion of available tests. 
  }
}

\value{Returns output associated with functions being run.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{abcSGP}}}

\examples{
\dontrun{
## testSGP(1) runs abcSGP for all years in sgpData_LONG
testSGP(1)

## testSGP(2) runs abcSGP for all years in sgpData_LONG except most recent than adds 
## in most recent year and runs curent year analyses
testSGP(2)
}  
}

\keyword{documentation}
