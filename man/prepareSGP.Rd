\name{prepareSGP}
\alias{prepareSGP}
\title{Prepare data for SGP analyses}
\description{
  Utility function/exemplar used to embed supplied long data into a list object as a keyed data.table.
}

\usage{
prepareSGP(data,
	state=NULL,
	var.names=NULL,
	create.additional.variables=TRUE,
	fix.duplicates="keep.all")
}

\arguments{
  \item{data}{A panel data set in long form. See embedded \code{\link{sgpData_LONG}} data set for an exemplar. 
  }
  \item{state}{A two letter acronymn indicating the state associated with the data. If not supplied, the function will try to infer what the state is from the data object name supplied.
  }
  \item{var.names}{A list or a dataframe that includes all required columns that do not match the SGP conventions, as well as all secondary columns needed 
for summarization and reporting.
  }
  \item{create.additional.variables}{Boolean argument indicating whether prepareSGP should create additional variables often used in analyses. For example, the function can create a variable
  \code{HIGH_NEED_STATUS} identifying the top and bottom quartile of students in each school by year by content area by grade grouping.
  }
  \item{fix.duplicates}{Argument for future functionality intended to deal with duplicate cases in supplied long data.
  }
}

\value{Function returns an object of class \code{SGP}. The long data is
  in the \code{data} slot.
}

\author{Ben Domgingue \email{ben.domingue@gmail.com} and Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{sgpData_LONG}}}

\examples{
\dontrun{
## prepareSGP is Step 1 of 5 of abcSGP
Demonstration_SGP <- prepareSGP(sgpData_LONG)
}  
}

\keyword{documentation}
