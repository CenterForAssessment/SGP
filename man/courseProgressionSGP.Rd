\name{courseProgressionSGP}
\alias{courseProgressionSGP}
\title{Identify potential course progressions for SGP analyses}
\description{
  Utility function used to analyze supplied long data or an existing SGP class object to identify potential course progressions suitable for analysis with \code{\link{analyzeSGP}}, \code{\link{studentGrowthPercentiles}}, etc. See examples for more information.
}

\usage{
courseProgressionSGP(
	sgp_object,
	lag.direction=c("FORWARD", "BACKWARD"),
	year)
}

\arguments{
  \item{sgp_object}{Either a panel data set in long form or an object of class SGP. See embedded \code{\link[SGPdata]{sgpData_LONG}} data set for an exemplar.
  }
  \item{lag.direction}{Character string indicating whether the progressions should be produced prospectively or retrospectively.
  }
  \item{year}{Character indicating the value of YEAR that is the focus of analysis.
  }
}

\value{Function returns a nested \code{list} class object. The final node of each nested list is a  \code{data.table} which summarizes the number of students with a particular GRADE by CONTENT_AREA by YEAR course progression
}

\author{Adam Van Iwaarden \email{avaniwaarden@nciea.org} and Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link[SGPdata]{sgpData_LONG}}}

\examples{
\dontrun{
## Run courseProgressionSGP on the subset of the long data that contains
## ONLY mathematics related records (would realistically also contain EOCT math courses)
Math_Data <- subset(SGPdata::sgpData_LONG, CONTENT_AREA == "MATHEMATICS")
Math_Progressions <- courseProgressionSGP(Math_Data, lag.direction= "BACKWARD", year="2015_2016")

##  Examine results for Adcademic Year 2015-2016, 5th grade Mathematics.
Math_Progressions[['BACKWARD']][['2015_2016']][['MATHEMATICS.05']]
Math_Progressions[['BACKWARD']][['2015_2016']][['MATHEMATICS.05']][COUNT>100]
}
}

\keyword{documentation}
