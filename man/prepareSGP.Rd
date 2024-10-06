\name{prepareSGP}
\alias{prepareSGP}
\title{Prepare data for SGP analyses}
\description{
  Utility function/exemplar used to embed supplied long data into a list object as a keyed data.table. NOTE: This function also serves the purposes of running many checks on the SGP object you
  construct to make sure it is up to date and in the best shape possible. If you have an older object that you wish to make sure is up to date with the latest version of the SGP package, running
  \code{\link{prepareSGP}} on an object is never bad thing to do. See examples for more information.
}

\usage{
prepareSGP(data,
	data_supplementary=NULL,
	state=NULL,
	var.names=NULL,
	create.additional.variables=TRUE,
	fix.duplicates=NULL,
  create.achievement.level=TRUE)
}

\arguments{
  \item{data}{A panel data set in long form or an object of class SGP. See embedded \code{\link[SGPdata]{sgpData_LONG}} data set for an exemplar.
  }
  \item{data_supplementary}{Supplementary data (e.g., student teacher lookup tables) to be embedded in SGP object in slot @Data_Supplementary. Data must be embedded in a list. Default is no data supplied.
  }
  \item{state}{A two letter acronym indicating the state associated with the data. If not supplied, the function will try to infer what the state is from the data object name supplied.
  }
  \item{var.names}{A list or a data.frame that includes all required columns that do not match the SGP conventions, as well as all secondary columns needed
for summarizing and reporting.
  }
  \item{create.additional.variables}{Boolean argument indicating whether prepareSGP should create additional variables often used in analyses. For example, the function can create a variable
  \code{HIGH_NEED_STATUS} identifying the top and bottom quartile of students in each school by year by content area by grade grouping.
  }
  \item{fix.duplicates}{Argument to control how \code{prepareSGP} deals with duplicate records based upon the key of VALID_CASE, CONTENT_AREA, YEAR, and ID.
  The function currently warns of duplicate records and doesn't modify data. If set to TRUE, \code{prepareSGP} tries to
  fix the duplicate individual records by adding a '_DUP_***' suffix to the duplicate ID in order to create unique records based upon the key.
  }
  \item{create.achievement.level}{Boolean argument indicating whether prepareSGP should create the ACHIEVEMENT_LEVEL variable if it is missing. Defaults to TRUE.}
}

\value{Function returns an object of class \code{SGP}. The long data is
  in the \code{data} slot.
}

\author{Adam Van Iwaarden \email{avaniwaarden@nciea.org}, Damian W. Betebenner \email{dbetebenner@nciea.org}, and Ben Domingue \email{ben.domingue@gmail.com}}

\seealso{\code{\link[SGPdata]{sgpData_LONG}}}

\examples{
\dontrun{
## prepareSGP is Step 1 of 5 of abcSGP
Demonstration_SGP <- prepareSGP(sgpData_LONG)

## Running prepareSGP on an already create SGP object as part of a annual update

Demonstration_SGP <- prepareSGP(Demonstration_SGP)


## Running prepareSGP on a long data set without creating addition variables

Demonstration_SGP <- prepareSGP(sgpData_LONG, create.additional.variables=FALSE)
}
}

\keyword{documentation}
