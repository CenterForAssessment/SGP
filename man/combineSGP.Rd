\name{combineSGP}
\alias{combineSGP}
\title{Combine student data and SGP results}
\description{
  Utility function that merges student long data in the @Data slot with results from student growth percentiles and/or student growth projections calculations.
Default values of this function are designed to be used following use of other utility functions: \code{\link{prepareSGP}} and \code{\link{analyzeSGP}}.
Function is integrated with cutscores embedded in \code{\link{SGPstateData}} to calculate growth-to-standard SGP targets and their associated scale scores
with catch-up/keep-up to proficient status and/or move-up/stay-up to advanced status as well as the scale scores associated with these targets.
}

\usage{
combineSGP(sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	sgp.percentiles=TRUE,
        sgp.percentiles.baseline=TRUE,
	sgp.projections=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged=TRUE,
        sgp.projections.lagged.baseline=TRUE,
	sgp.target.scale.scores=FALSE,
	sgp.target.scale.scores.only=FALSE,
	sgp.target.content_areas=NULL,
	max.sgp.target.years.forward=3,
	update.all.years=FALSE,
	sgp.config=NULL,
	sgp.percentiles.equated=FALSE,
	SGPt=NULL,
  fix.duplicates=NULL,
	parallel.config=NULL)
}

\arguments{
  \item{sgp_object}{An object of class SGP containing slots \code{@Data} (from \code{prepareSGP}) and \code{@SGP} (from \code{analyzeSGP}).
  }
  \item{state}{Acronym for which state is to be used for the lagged projections and growth to standard analyses. Function will try to guess state name from passed \code{sgp_object} is missing.
  }
  \item{years}{A vector of years indicating years of data to merge with \code{@Data}. If missing, merge will use all available years of student growth percentile results.
  }
  \item{content_areas}{A vector of content areas indicating content areas of student growth percentile data to merge with \code{@Data}. If missing, merge will use all available content areas of
student growth percentile results.
  }
  \item{sgp.percentiles}{A Boolean variable indicating whether to combine student growth percentiles.
  }
  \item{sgp.percentiles.baseline}{A Boolean variable indicating whether to combine baseline student growth percentiles.
  }
  \item{sgp.projections}{A Boolean variable indicating whether to combine current year student growth projections and calculate catch-up/keep-up and move-up/stay-up values.
  }
  \item{sgp.projections.baseline}{A Boolean variable indicating whether to combine current year baseline student growth projections and calculate catch-up/keep-up and move-up/stay-up values.
  }
  \item{sgp.projections.lagged}{A Boolean variable indicating whether to combine lagged student growth projections and calculate catch-up/keep-up and move-up/stay-up values.
  }
  \item{sgp.projections.lagged.baseline}{A Boolean variable indicating whether to combine lagged baseline student growth projections and calculate catch-up/keep-up move-up/stay-up values.
  }
  \item{sgp.target.scale.scores}{A Boolean variable indicating whether scale scores from calculated SGP targets should be calculated.
  }
  \item{sgp.target.scale.scores.only}{A Boolean variable indicating whether combineSGP should skip merging and only calculate scale scores from calculated SGP targets. Default is FALSE.
  }
  \item{sgp.target.content_areas}{A Boolean variable indicating whether content area associated with SGP targets should be calculated.
  }
  \item{max.sgp.target.years.forward}{A integer indicating the number of years forward from the lagged (last year's) score to project forward for growth to standard calculations. Default is 3 years from the present, 4 years from the lagged year, which is the standard in most growth to standard calculations used by state departments of education.
  }
  \item{update.all.years}{A Boolean argument defaulting to FALSE indicating whether \code{\link{combineSGP}} should delete previously merged variables calculated in \code{\link{analyzeSGP}} and
re-merge all available data.
  }
  \item{sgp.config}{Argument (defaults to NULL) passed utilized only for target scale score calculation. If targets for end of course tests are required, user must specify configurations
  directly. See code from \code{\link{testSGP}} number 3 for an example.
  }
  \item{sgp.percentiles.equated}{Boolean variable indicating whether equated percentiles are being merged. If scale score targets are being calculated, linkages will be passed to scale score targets
for calculation.
  }
  \item{SGPt}{Argument used to pass SGPt configuration to \code{getTargetScaleScore} for \code{\link{studentGrowthProjections}} calculation of scale score targets.
  }
  \item{fix.duplicates}{Argument to control how duplicate records based upon the key of VALID_CASE, CONTENT_AREA, YEAR, and ID are dealt with.
  If set to 'KEEP.ALL', the function tries to fix the duplicate individual records. If present and a '_DUP_***' suffix has been added to the
  duplicate ID before running \code{\link{studentGrowthPercentiles}}, then the \code{@Data} slot will be extended as necessary to accomodate
  additional student records and SGP results.
  }
  \item{parallel.config}{Parallel configuration only used when 'sgp.target.scale.scores' is set to TRUE. Default is NULL consistent with no targets being calculated. To utilize parallel processing
  in the calculation of SGP scale score targets user must specify a list designating a backend (e.g, BACKEND='PARALLEL') and a number of workers (e.g., 'WORKERS=list(SGP_SCALE_SCORE_TARGETS=4)').
  }
}

\value{Function returns a list containing the input long data set in the \code{@Data} slot as a \code{data.table} keyed using \code{VALID_CASE}, \code{CONTENT_AREA},
\code{YEAR}, \code{ID} merged with student growth percentiles and/or straight/lagged projection targets and catch-up/keep-up and move-up/stay-up status with, if requested, the scale scores
associated with such targets.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}}

\examples{
\dontrun{
## combineSGP is Step 3 of 5 of abcSGP
Demonstration_SGP <- sgpData_LONG
Demonstration_SGP <- prepareSGP(Demonstration_SGP)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP)
Demonstration_SGP <- combineSGP(Demonstration_SGP)
}
}

\keyword{documentation}
