\name{SGP-package}
\alias{SGP-package}
\alias{SGP}
\docType{package}
\title{SGP: Student Growth Percentiles & Percentile Growth Trajectories}
\description{
SGP contains classes and functions to calculate student growth percentiles and percentile growth projections/trajectories following methodology found in Betebenner (2008, 2009).
The package contains two primary functions, \code{\link{studentGrowthPercentiles}} and \code{\link{studentGrowthProjections}}, and numerous higher level functions
that make use of them including: \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}, \code{\link{visualizeSGP}} and
\code{\link{outputSGP}}. These functions are used to calculate and visualize student growth percentiles and percentile growth projections/trajectories for students using large scale,
longitudinal assessment data.  These norm- and criterion-referenced growth values are currently used in a number of states for many purposes including diagnostic and accountability.
The functions employ quantile regression (using the \code{quantreg} package) to estimate the conditional density for current achievement using each student's achievement history.
Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the student growth percentile analyses. These quantities
are summarized in a variety of ways to describe student growth. Beginning with version 1.4-0.0, the SGP package also calculate time dependent SGPs (SGPt) and allows for student
growth projections to be calculated across assessment transitions by equating the two tests.
}

\details{
\tabular{ll}{
Package: \tab SGP\cr
Type: \tab Package\cr
Version: \tab 2.0-0.0\cr
Date: \tab 2022-5-25\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
Calculation of student growth percentiles and percentile growth trajectories/projections is typically performed by grade and subject. Data for growth percentile
calculation must be specifically formatted.  See \code{\link[SGPdata]{sgpData}} for an example data set. Batch R syntax for performing analyses across all grades and years is
provided in the examples of the \code{\link{studentGrowthPercentiles}} and \code{\link{studentGrowthProjections}} using the higher level functions
\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}, and \code{\link{visualizeSGP}}.}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}, Adam Van Iwaarden \email{avaniwaarden@nciea.org}, Ben Domingue \email{ben.domingue@gmail.com} and Yi Shang \email{shangyi@gmail.com}}

\references{
Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan &
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.

Betebenner, D. W. (2009). Norm- and criterion-referenced student growth.
Educational Measurement: Issues and Practice, 28(4):42-51.

Betebenner, D. W. (2012). Growth, standards, and accountability. In G. J. Cizek,
\emph{Setting Performance Standards: Foundations, Methods & Innovations. 2nd Edition} (pp. 439-450).
New York: Routledge.

Castellano, K. E. & McCaffrey, D. F. (2017). The Accuracy of Aggregate Student Growth Percentiles as
Indicators of Educator Performance. Educational Measurement: Issues and Practice, 36(1):14-27.

Koenker, R. (2005). \emph{Quantile regression}. Cambridge: Cambridge University Press.

Shang, Y., VanIwaarden, A., & Betebenner, D. W. (2015). Covariate measurement error correction for Student Growth
Percentiles using the SIMEX method. Educational Measurement: Issues and Practice, 34(1):4-14.
}

\keyword{package}
