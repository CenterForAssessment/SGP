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
        output.type=c("LONG_Data", "LONG_FINAL_YEAR_Data", "WIDE_Data", 
			"INSTRUCTOR_Data"),
        baseline.sgps=FALSE,
        outputSGP_SUMMARY.years=NULL,
        outputSGP_SUMMARY.content_areas=NULL,
        outputSGP_INDIVIDUAL.years=NULL,
        outputSGP_INDIVIDUAL.content_areas=NULL,
        outputSGP.anonymize=FALSE,
        outputSGP.student.groups=NULL,
        outputSGP.directory="Data",
	outputSGP.translate.names=TRUE,
	outputSGP.projection.years.for.target=3)
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
  \item{output.type}{A character vector indicating what output type is requested. Currently \code{LONG_Data}, \code{WIDE_Data}, \code{INSTRUCTOR_Data} and \code{SchoolView} are supported 
  modes of output.  \code{LONG_Data} exports the contents of the @Data slot in a pipe delimited format. \code{LONG_FINAL_YEAR_Data} exports the contents of the last year of the @Data slot in a pipe
  delimited format. \code{WIDE_Data} exports a reshaped version of the @Data slot where each row is a unique student record. \code{INSTRUCTOR_Data} uses the 
  \code{@Data_Supplementary$INSTRUCTOR_NUMBER} table to export a long student by instructor number table. \code{SchoolView} exports tables used for representation in SchoolView. The 
  default exports \code{LONG_Data} and \code{WIDE_Data}. 
  }
  \item{baseline.sgps}{Boolean vector indicating whether to output baseline SGPs for cohort referenced SGPs.
  }
  \item{outputSGP_SUMMARY.years}{A character vector indicating the year to be used for output file construction for summary tables.
  }
  \item{outputSGP_SUMMARY.content_areas}{A character vector indicating the content areas to be used for output file construction for summary tables.
  }
  \item{outputSGP_INDIVIDUAL.years}{A character vector indicating the year to be used for output file construction for individual level file.
  }
  \item{outputSGP_INDIVIDUAL.content_areas}{A character vector indicating the content areas to be used for output file construction for individual level file.
  }
  \item{outputSGP.anonymize}{A Boolean variable indicating whether to anonymize output files.
  }
  \item{outputSGP.student.groups}{A list of variables to be used for student groups in individual and summary tables.
  }
  \item{outputSGP.directory}{A a file path indicating where to save output files. Defaults to \code{Data}.
  }
  \item{outputSGP.translate.names}{A Boolean argument, defaults to TRUE, indicating whether data output should refer to 'names.provided' or 'names.sgp' in @Names slot of supplied SGP object.
  This argument allows for the the conversion of variable naming conventions from the SGP package back to that used by the state/organization.
  }
  \item{outputSGP.projection.years.for.target}{An integer argument indicating what projection to supply with regard to the number of years projected forward.
  }
}

\value{Function writes data in multiple formats including .Rdata, .txt (pipe delimited) and zipped versions of .txt.  
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{abcSGP}}, \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}}

\examples{
\dontrun{
Demonstration_SGP <- prepareSGP(sgpData_LONG)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP)
Demonstration_SGP <- combineSGP(Demonstration_SGP)
outputSGP(Demonstration_SGP)

## Output current year

outputSGP(Demonstration_SGP, output.type="LONG_FINAL_YEAR_Data")
}  
}

\keyword{documentation}
