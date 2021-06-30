\name{setNamesSGP}
\alias{setNamesSGP}
\title{Function for renaming data (typically from \code{\link{outputSGP}}) from
a state/organization naming conventions to those used in the SGP package}
\description{
setNamesSGP renames a dataset (without copying or returning the data).
}

\usage{
setNamesSGP(
	data,
	state=NULL)
}

\arguments{
  \item{data}{The dataset to be renamed. For example longitudinal data exported
	from \code{\link{outputSGP}} that uses a state's naming conventions.
  }
  \item{state}{The 'state' for the \code{data} Derive from \code{data} name if not explicitly supplied.
  }
}

\value{NULL. Simply renames the data. Note that the \code{state} must be included
in the \code{SGPstateData} and have a \code{Variable_Name_Lookup} entry to work.
}

\author{Adam R. Van Iwaarden \email{avaniwaarden@nciea.org} }

\seealso{\code{\link{outputSGP}}}

\examples{
\dontrun{
load("Data/Demonstration_SGP_LONG_Data.Rdata")
setNamesSGP(Demonstration_SGP_LONG_Data)
}
}

\keyword{documentation}
