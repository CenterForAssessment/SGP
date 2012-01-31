\name{abcSGP}
\alias{abcSGP}
\title{Perform 5 step sequence: prepareSGP, analyzeSGP, combineSGP, summarizeSGP, and visualizeSGP}
\description{
  Utility function to perform sequence of 5 steps going from data preparation, \code{\link{prepareSGP}}, SGP data analysis, \code{\link{analyzeSGP}},
  data combining, \code{\link{combineSGP}}, data summarization, \code{\link{summarizeSGP}}, and data visualization \code{\link{visualizeSGP}}. Calculating and using
  student growth percentiles is as easy as ABC.
}

\usage{
abcSGP(sgp_object,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP"),
	years=NULL,
	content_areas=NULL,
	grades=NULL,
	prepareSGP.var.names=NULL,
	sgp.percentiles=TRUE,
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
        sgp.percentiles.baseline=TRUE,
        sgp.projections.baseline=TRUE,
        sgp.projections.lagged.baseline=TRUE,
	simulate.sgps=TRUE,
	parallel.config,
	save.intermediate.results=FALSE,
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_TARGET="median_na(SGP_TARGET)",
		PERCENT_CATCHING_UP_KEEPING_UP=
			"percent_in_category(CATCH_UP_KEEP_UP_STATUS, list(c('Catch Up: Yes', 'Keep Up: Yes')), list(c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No')))",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)"),
	summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS_INITIAL"),
		institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
        confidence.interval.groups=list(TYPE="Bootstrap",
                VARIABLES=c("SGP"),
                QUANTILES=c(0.025, 0.975),
                GROUPS=list(institution="SCHOOL_NUMBER",
                content="CONTENT_AREA",
                time="YEAR",
		institution_level= NULL,
                demographic=NULL,
                institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS"))),
	plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot")) 
}

\arguments{
  \item{sgp_object}{A list containing LONG formatted data. See \code{\link{sgpData_LONG}} for an exemplar. NOTE: Data preparation must be meticulous to utilize this enhanced functionality.
  }
  \item{state}{Acronym indicating state associated with the data for access to embedded knot and boundaries,  cutscores, CSEMs, and other relevant state level data.
  }
  \item{steps}{Vector containing all or some subset of \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}, \code{\link{visualizeSGP}} 
indicating what steps the user wants accomplished. Default is to perform all steps.
  }
  \item{years}{A vector indicating year(s) in which to produce student growth percentiles and/or student growth projections/trajectories. If missing the 
function will use the data to infer the year(s) in which to perform growth percentile analyses  based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content area(s) in which to produce student growth percentiles and/or student growth projections/trajectories. 
If missing the function will use the data to infer the content area(s) available for analyses.
  }
  \item{grades}{A vector indicating grades for which to calculate student growth percentiles and/or student growth projections/trajectories. 
If missing the function will use the data to infer all the grade progressions for student growth percentile and student growth projections/trajectories analyses.
  }
  \item{prepareSGP.var.names}{list supplied to prepareSGP mapping provided variable names to variable names required as part of the SGP package. See \code{\link{prepareSGP}} for more details. Defaults to NULL.
  }
  \item{sgp.percentiles}{Boolean variable indicating whether to calculate student growth percentiles. Defaults to TRUE.
  }
  \item{sgp.projections}{Boolean variable indicating whether to calculate student growth projections. Defaults to TRUE.
  }
  \item{sgp.projections.lagged}{Boolean variable indicating whether to calculate lagged student growth projections often used for growth to standard analyses. Defaults to TRUE.
  }
  \item{sgp.percentiles.baseline}{Boolean variable indicating whether to calculate baseline student growth percentiles and/or coefficient matrices. Defaults to TRUE.
  }
  \item{sgp.projections.baseline}{Boolean variable indicating whether to calculate baseline student growth projections. Defaults to TRUE.
  }
  \item{sgp.projections.lagged.baseline}{Boolean variable indicating whether to calculate lagged baseline student growth projections. Defaults to TRUE.
  }
  \item{simulate.sgps}{Boolean variable indicating whether to simulate SGP values for students based on test-specific Conditional Standard Errors of Measurement (CSEM).  Test CSEM data must be available for simulation.  Must be set to TRUE for confidence interval construction. Defaults to TRUE in \code{abcSGP} only.
  }
  \item{parallel.config}{A named list indicating, at a minimum, the TYPE of the back end to be used for parallel computation.  TYPE = one of \code{FOREACH}, \code{SNOW}, \code{MULTICORE}.  List may also contain WORKERS for the number of cores or nodes used in \code{foreach} or \code{multicore} packages, or the name of the CLUSTER.OBJECT created for use with \code{snow}.
  }
  \item{save.intermediate.results}{Should intermediate results of \code{abcSGP} be saved after each of \code{\link{prepareSGP}}, \code{\link{analyzeSGP}},
  \code{\link{combineSGP}}, and \code{\link{summarizeSGP}}. Default is FALSE.
  }
  \item{sgp.summaries}{A list giving the summaries requested for each group analyzed based upon the \code{summary.group} argument.
  }
  \item{summary.groups}{A list consisting of 5 types of groups across which all summaries are taken: \code{institution}, \code{content}, \code{time}, \code{institution_level},
\code{demographic}, and \code{institution_inclusion}. Summaries generated in summarizeSGP are for all possible combinations of the 5 types of group. See documentation for \code{\link{summarizeSGP}} and \code{\link{viewSummaryGroups}} for more detail. 
  }
  \item{confidence.interval.groups}{A subset of the groups provided in the \code{summary.groups} argument indicating which groups to provide confidence intervals for. See documentation for \code{\link{summarizeSGP}} and \code{\link{viewSummaryGroups}} for more detail.
  }
  \item{plot.types}{A character vector passed to \code{\link{visualizeSGP}} indicating the types of plots to produce. Currently supported plots include \code{\link{bubblePlot}}s, 
\code{\link{studentGrowthPlot}}s, and \code{\link{growthAchievementPlot}}s. 
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID}, SGP results including student growth percentile and student growth projections/trajectories in the SGP slot, and summary results in the 
Summary slot. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}},  \cr
\code{\link{studentGrowthPercentiles}}, and \code{\link{studentGrowthProjections}}}

\examples{
\dontrun{

## Runs all 5 steps
Demonstration_Data <- abcSGP(sgp_object=sgpData_LONG, state="DEMO")


## Or letting the function detect the state

Demonstration_Data <- sgpData_LONG
Demonstration_Data <- abcSGP(Demonstration_Data)
}
}

\keyword{documentation}
