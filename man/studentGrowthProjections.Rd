\name{studentGrowthProjections}
\alias{studentGrowthProjections}
\title{Student Growth Projections}
\description{
  Function to calculate percentile growth projections/trajectories using large scale assessment data and results derived from student growth percentile calculation.  Function can produce percentile growth trajectories, as well as growth percentiles, sufficient for each student to reach a set of predefined scale score cut.
}

\usage{
studentGrowthProjections(panel.data, 
                         sgp.labels,               
                         grade.progression,
                         content_area.progression=NULL,
                         grade.projection.sequence=NULL,
                         content_area.projection.sequence=NULL,
                         max.forward.progression.years,
                         max.forward.progression.grade, 
                         max.order.for.progression,  
                         use.my.knots.boundaries,   
                         use.my.coefficient.matrices,  
                         panel.data.vnames,
                         achievement.level.prior.vname=NULL,
                         performance.level.cutscores, 
                         calculate.sgps=TRUE,
                         convert.0and100=TRUE,    
                         projection.unit="YEAR",       
                         percentile.trajectory.values=NULL, 
                         isotonize=TRUE,
                         lag.increment=0,
                         projcuts.digits=NULL,
                         print.time.taken=TRUE)
}

\arguments{
  \item{panel.data}{Object of class \code{list} containing longitudinal student data in wide format in \code{panel.data$Panel_Data}.  See \code{\link{studentGrowthPercentiles}}
    for data requirements.  List object must also contain \code{panel.data$Knots_Boundaries} and \code{panel.data$Coefficient_Matrices}.  See \code{\link{sgpData}} for an 
    exemplar data set. NOTE: The column position of the variables IS IMPORTANT, NOT the names of the variables. 
  }
  \item{sgp.labels}{REQUIRED. A list, \code{sgp.labels}, of the form  \code{list(my.year= ,} \cr
    \code{my.subject= )}. The user-specified values are used to save the percentile growth projections/trajectories and identify coefficient matrices and knots & boundaries for calculation if
    \code{use.my.coefficient.matrices} or \cr
    \code{use.my.knots.boundaries} is missing. Partly replaces previous argument \code{proj.function.labels}. 
  }
  \item{grade.progression}{REQUIRED. Argument to specify a student grade/time progression in the data to be used for percentile growth projection/trajectory calculation. 
    This argument helps in replacing previous arguments \code{num.panels},  \code{max.num.scores}, and \code{num.prior.scores}.
  }
  \item{content_area.progression}{Argument to specify a student content area progression in the data supplied for percentile growth projection/trajectory calculation. 
  }
  \item{grade.projection.sequence}{Argument to manually supply grade sequence over which projection is made. Defaults to NULL and is calculated from available data.
  }
  \item{content_area.projection.sequence}{Argument to manually supply content area sequence over which projection is made. Defaults to NULL and is calculated from available data.
  }
  \item{max.forward.progression.years}{The MAXIMUM number of years/grades/time periods to project forward conditional upon available coefficient matrices. If missing, function
  will project forward as far as allowed by available coefficient matrices. 
  }
  \item{max.forward.progression.grade}{The MAXIMUM grade to project forward based upon available coefficient matrices. If missing, function
  will project forward as far as allowed by available coefficient matrices. 
  }
  \item{max.order.for.progression}{Argument to specify the maximum coefficient matrix order to be used for percentile growth projection/trajectory calculation. 
  If missing, the function utilizes the highest matrix order available. 
  }
  \item{use.my.knots.boundaries}{A list of the form  \code{list(my.year= , my.subject= )} specifying the set of pre-calculated
   knots and boundaries for B-spline calculations. Knot and boundaries are stored (and must be made available) with \code{panel.data} supplied as a 
  list in \code{panel.data$Knots_Boundaries$my.year.my.subject}. As of SGP_0.0-6.9 user can also supply a two letter state acronym to utilize knots and boundaries 
  within the \code{SGPstateData} data set supplied with the SGP package. If missing, function tries to retrieve knots and boundaries from \cr
   \code{panel.data$Knots_Boundaries$my.year.my.subject} where \cr \code{my.year} and \code{my.subject} are provided by \code{sgp.labels}.  
  }
  \item{use.my.coefficient.matrices}{A list of the form \code{list(my.year= , my.subject= )} specifying the set of pre-calculated
   coefficient matrices to use for percentile growth projection/trajectory calculations. Coefficient matrices are stores (and must be available) with \code{panel.data}
   supplied as a list in \code{panel.data$Coefficient_Matrices} \cr
   \code{$my.year.my.subject}. If missing, function tries to retrieve coefficient matrices from
   \code{panel.data$Coefficient_Matrices$my.year.my.subject} where \code{my.year} and \code{my.subject} are provided by \code{sgp.labels}.
  }
  \item{panel.data.vnames}{Vector of variables to use in percentile growth projection/trajectory calculations. If not specified, function attempts to use all available 
   variables.
  }
  \item{achievement.level.prior.vname}{Character vector indicating variable is supplied panel data corresponding to the prior achievement level to be added to the output. Used
   in the production of growth to standard analyses.
  }
  \item{performance.level.cutscores}{Argument for supplying performance level cutscores to be used for determining growth-to-standard percentile growth trajectory calculations. Argument accepts a two letter state acronym (e.g., "CO") that retrieves cutscores that are automatically embedded in a data set contained in the SGP package. Argument also accepts a subject specific list of the form:
  \preformatted{
  performance.level.cutscores <- list(
         Reading=list(GRADE_3=c(cut1, cut2, cut3),
                      GRADE_4=c(cut1, cut2, cut3),
                       . . .
                      GRADE_8=c(cut1, cut2, cut3)),
            Math=list(GRADE_3=c(cut1, cut2, cut3),
                       . . .
                      GRADE_7=c(cut1, cut2, cut3),
                      GRADE_8=c(cut1, cut2, cut3)))}
  Note that the subject name must match that provided by \code{sgp.labels}. If cuts are not desired leave the cutscore unspecified, which is the default. 
  If your state's cutscores are not included in the \code{SGPstateData} data set or are incorrect, please contact 
  \email{dbetebenner@nciea.org} to have them added or corrected!
  }
  \item{calculate.sgps}{Boolean argument (defaults to TRUE) indicating whether to calculate student growth projections. Currently used to bypass calculations in \code{\link{analyzeSGP}} when scale changes occur.
  }
  \item{convert.0and100}{Boolean argument (defaults to TRUE) indicating whether conversion of growth percentiles of 0 and 100 to growth percentiles
    of 1 and 99, respectively, occurs. The default produces growth percentiles ranging from 1 to 99.
  }
  \item{projection.unit}{Argument specifying the units in which the projections/trajectories and cuts are reported. Either \code{"GRADE"}, the default, or 
  \code{"YEAR"}.
  }       
  \item{percentile.trajectory.values}{An integer argument with values ranging from 1 to 100 that returns a vector of percentile trajectory cuts (default is NULL, 
  no percentile trajectory values returned).  The returned values are the lower bound for the interval associated with the percentile(s) requested. For example, by specifying
  'percentile.trajectory.values=1' the user would receive the conditional .005 quantile associated with each student's distribution.
  }
  \item{isotonize}{Boolean argument (defaults to TRUE) indicating whether quantile regression results are isotonized to prevent quantile crossing following 
  the methods derived by Dette & Volgushev (2008).
  }
  \item{lag.increment}{A non-negative integer (defaults to 0) indicating the lag associated with the data supplied for projections. Only relevant if Cutscores or Knots and Boundaries are year dependent. 
  }
  \item{projcuts.digits}{The number of digits (defaults to NULL/0) percentile trajectories (if requested) are formatted.
  }
  \item{print.time.taken}{Boolean argument (defaults to TRUE) indicating whether to print message indicating information on studentGrowthProjection analysis and time taken.
  }
}

\value{Function returns the input \code{panel.data} list object with the additional percentile growth trajectories/percentiles stored in 
\code{panel.data$SGProjections$my.year.my.subject} consisting of student IDs and the associated percentile growth projections/trajectories and cuts.
The data frame contains projections/trajectories for each performance level cut-point supplied and each percentile cut the user specifies.
}

\references{
Betebenner, D. W. (2012). Growth, standards, and accountability. In G. J. Cizek,
\emph{Setting Performance Standards: Foundations, Methods & Innovations. 2nd Edition} (pp. 439-450).
New York: Routledge.

Betebenner, D. W. (2009). Norm- and criterion-referenced student growth. 
Educational Measurement: Issues and Practice, 28(4):42-51.

Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.

Dette, H. & Volgushev, S. (2008). Non-crossing non-parametric estimates of quantile curves. Journal of the Royal Statistical Society B, 
70(3), 609-627.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\note{Use of this function assumes prior calculation of student growth percentiles, making the coefficient matrices available within the  \code{panel.data$Coefficient_Matrices} 
  list object.  Additionally, if cutscores are desired they must be supplied explicitly by the user (as detailed above in
  \code{performance.level.cutscores}) or included in the \code{SGPstateData} data set.  If your state's cutscores are not included or are incorrect, please contact \email{dbetebenner@nciea.org} to have cutscores added or corrected!
}

\seealso{\code{\link{studentGrowthPercentiles}}, \code{\link{sgpData}}}

\examples{
\dontrun{
## First calculate SGPs for 2013
my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)
my.sgpData <- list(Panel_Data = sgpData)

for (i in seq_along(my.grade.sequences)) {
     my.sgpData <- studentGrowthPercentiles(panel.data=my.sgpData,
                     sgp.labels=list(my.year=2013, my.subject="Reading"),
                     use.my.knots.boundaries="DEMO",
                     grade.progression=my.grade.sequences[[i]])
}

## Calculate Growth Projections

my.grade.progressions <- list(3, 3:4, 3:5, 3:6, 4:7)

for (i in seq_along(my.grade.progressions)) {
     my.sgpData <- studentGrowthProjections(panel.data=my.sgpData,
                      sgp.labels=list(my.year=2013, my.subject="Reading"),
                      projcuts.digits=0,
                      projection.unit="GRADE",
                      performance.level.cutscores="DEMO",
                      percentile.trajectory.values=c(25, 50, 75),
                      grade.progression=my.grade.progressions[[i]])
}

##  Save the Student Growth Projections Results to a .csv file:
write.csv(my.sgpData$SGProjections$READING.2013, 
file= "2013_Reading_SGProjections.csv", row.names=FALSE, quote=FALSE)
}  
}

\keyword{misc}
\keyword{models}
