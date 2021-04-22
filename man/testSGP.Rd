\name{testSGP}
\alias{testSGP}
\title{Test SGP Package functions}
\description{
testSGP runs large scale tests of the SGP package to test for consistent performance across releases.
}

\usage{
testSGP(TEST_NUMBER,
	save.results=TRUE,
	test.option=list(),
	memory.profile=FALSE,
	stop.fail=TRUE)
}

\arguments{
  \item{TEST_NUMBER}{An integer indicating the test to be run. Type 'testSGP()' to see list and description of available tests.
  }
  \item{save.results}{A Boolean variable, defaulting to FALSE, indicating whether the results of the analysis is saved to the working directory.
  }
  \item{test.option}{A character string (defaults to NULL) supplying a test option to the given test specified by TEST_NUMBER. Argument is test specific. See source code for testSGP for possible
  arguments.}
  \item{memory.profile}{A Boolean variable indicating whether to use memory profiling via \code{Rprof}.  Experimental. Defaults to FALSE.
  }
	\item{stop.fail}{A Boolean variable indicating whether to stop the function if a test fails Defaults to TRUE.
  }
}

\value{Returns output associated with functions being run.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{avaniwaarden@nciea.org}}

\seealso{\code{\link{abcSGP}}}

\examples{
\dontrun{
## testSGP(0): Test of studentGrowthPercentiles, studentGrowthProjections, and sgpData
testSGP(0)

## testSGP(1) & testSGP('1b') runs abcSGP for all years in sgpData_LONG with/without sqliteSGP
testSGP(1)
testSGP('1b')

## testSGP(2): Various tests of updateSGP functionality.
## testSGP('2a'): Test of updateSGP performing SGP analyses in two steps:
##	Create what_sgp_object: 2010-2011 to 2013-2014 then add with_sgp_data_LONG 2014-2015 using
##	overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=FALSE.
## testSGP('2b'): Test of updateSGP performing SGP analyses in two steps:
## 	Create what_sgp_object: 2010-2011 to 2013-2014 then add with_sgp_data_LONG 2014-2015 using
##	overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=FALSE.
## testSGP('2c'): Test of updateSGP performing SGP analyses in two steps:
##	Create what_sgp_object: 2010-2011 to 2013-2014 then add with_sgp_data_LONG 2014-2015 using
##	overwrite.existing.data=TRUE and sgp.use.my.coefficient.matrices=TRUE.
## testSGP('2d'): Test of updateSGP performing SGP analyses in two steps:
##	Create what_sgp_object: 2010-2011 to 2013-2014 then add with_sgp_data_LONG 2014-2015 using
##	overwrite.existing.data=FALSE and sgp.use.my.coefficient.matrices=TRUE.
testSGP('2a')
testSGP('2b')
testSGP('2c')
testSGP('2d')

## testSGP(3) runs abcSGP on grade progressions including End of Course Tests in Mathematics
## (Algebra I and Algebra II) and Reading (Grade 9 Literature and American Literature)
testSGP(3)

## testSGP(4) runs prepareSGP and analyzeSGP with simex adjustment for measurement error
testSGP(4)

## testSGP(5) runs abcSGP assuming at test transition in the most recent year. NOTE YET COMPLETED
testSGP(5)

## testSGP(6) runs a basic baseline SGP analysis including the construction of baseline matrices.
testSGP(6)
}
}

\keyword{documentation}
