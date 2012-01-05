\name{splineMatrix-class}
\docType{class}
\alias{splineMatrix-class}
\alias{is.splineMatrix}
\alias{as.splineMatrix}
\title{Class "splineMatrix"}
\description{The formal S4 class for coefficient matrices produced from the \code{\link{studentGrowthPercentiles}} function. This class stores the B-spline knots and boundaries used the coefficient matrix object for the production of student growth percentiles and projections.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("splineMatrix", ...)}, but this is not encouraged.  Previously produced coefficient matrices MUST be bound to the IDENTICAL knots and boundaries used to create them. Use the function \code{\link{studentGrowthPercentiles}} instead.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{A coefficient matrix derived from \code{\link{studentGrowthPercentiles}}.}
    \item{\code{Knots}:}{A list(s) of numeric values used as the \code{knots} to generate the B-spline basis matrix in \code{\link{studentGrowthPercentiles}}.}
    \item{\code{Boundaries}:}{A list(s) of numeric values as the \code{Boundary.knots}to generate the B-spline basis matrix in \code{\link{studentGrowthPercentiles}}.}
    \item{\code{Version}:}{A list including the version of the SGP package used to construct the splineMatrix object and the date the object was created.}
  }
}

\details{
  \describe{
    \item{splineMatrix:}{This class contains the S3 \code{matrix} class, inheriting its methods.  The slot \code{Knots} should be one or more lists of numeric vector(s) used in the internal call to \code{bs}, which generates a B-spline basis matrix from student scores. There are typically with 4 values for the knots. Similarly, \code{Boundaries} are used in \code{bs} for the \code{Boundary.knots} argument. This is always two values which are at or slightly beyond the lowest and highest observed student scores.
  }
 }
}


\author{Adam Van Iwaarden \email{Vaniwaarden@colorado.edu}, Ben Domingue \email{ben.domingue@gmail.com} and Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{
\code{\link{studentGrowthPercentiles}}
}
\keyword{classes}
