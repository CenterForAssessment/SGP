\name{capwords}
\alias{capwords}
\title{Function for converting all caps to mixed case. Useful in data cleaning.}
\description{
The function capwords converts characters to mixed case character as intelligently as possible and leading/trailing spaces.
}

\usage{
capwords(x,
    special.words = c("ELA","I", "II", "III", "IV", "DCYF", "EMH", "HS", "MS", "ES",
          "SES", "IEP", "ELL", "MAD", "PARCC", "SBAC", "SD", "SWD", "US", "SGP", "SIMEX",
          "SS", "SAT", "PSAT", "WIDA", "ACCESS", "WIDA-ACCESS"))
}

\arguments{
  \item{x}{A character string to be converted to mixed case.
  }
  \item{special.words}{A character vector (see default above), specifying words to not convert to mixed case.
  }
}

\value{Returns a mixed case character string.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
capwords("TEST") ## Test
capwords("TEST1 TEST2") ## Test1 Test2
capwords("O'NEIL") ## O'Neil
capwords("JOHN'S") ## John's

## Use sapply for converting character vectors

test.vector <- paste("TEST", 1:10, sep="")
sapply(test.vector, capwords)


## With factors, convert levels instead of the entire vector

test.factor <- factor(paste("TEST", rep(letters[1:10], each=50)))
levels(test.factor) <- sapply(levels(test.factor), capwords)
levels(test.factor)
}

\keyword{documentation}
