\name{getStateAbbreviation}
\alias{getStateAbbreviation}
\title{Function for converting state/organization abbreviations to long form and back.}
\description{
The function getStateAbbreviation converts state/organization abbreviations to their long form and back. For example ID gets converted to Idaho.
}

\usage{
getStateAbbreviation(
                    supplied.name,
	                SGPfunction=NULL,
	                type="Abbreviation")
}

\arguments{
  \item{supplied.name}{A character string state/organization abbreviation or long form name.
  }
  \item{SGPfunction}{SGP package function from which \code{getStateAbbreviation} is being called. Defaults to NULL.
  }
  \item{type}{Either \code{Abbreviation} or \code{Long} indicating whether the desired output is an Abbreviation (long form name is supplied) or the long form name (abbreviation is supplied).
  The default is \code{Abbreviation}.
  }
}

\value{Returns a character string.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
getStateAbbreviation("IDAHO") ## ID
getStateAbbreviation("ID", type="Long") ## Idaho
}

\keyword{documentation}
