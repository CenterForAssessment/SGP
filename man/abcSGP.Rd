\name{abcSGP}
\alias{abcSGP}
\title{Perform 5 step sequence: prepareSGP, analyzeSGP, combineSGP, summarizeSGP, and visualizeSGP}
\description{
  Utility function to perform sequence of 6 steps going from data preparation, \code{\link{prepareSGP}}, SGP data analysis, \code{\link{analyzeSGP}},
  data combining, \code{\link{combineSGP}}, data summarization, \code{\link{summarizeSGP}}, data visualization \code{\link{visualizeSGP}} and data output
  \code{\link{outputSGP}}.
}

\usage{
abcSGP(sgp_object,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
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
	sgp.use.my.coefficient.matrices=NULL,
	simulate.sgps=TRUE,
	calculate.simex=NULL,
	parallel.config=NULL,
	save.intermediate.results=FALSE,
	save.old.summaries=FALSE,
	sgPlot.demo.report=FALSE,
	sgp.summaries=NULL,
	summary.groups=NULL,
	data_supplementary=NULL,
	confidence.interval.groups=NULL,
	plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
	verbose.output=FALSE) 
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
  \item{sgp.use.my.coefficient.matrices}{Boolean variable indicating whether to use embedded coefficient matrices to calculate SGPs. One should be careful to remove previously calculated SGPs prior
  to recalculating SGPs.
  }
  \item{simulate.sgps}{Boolean variable indicating whether to simulate SGP values for students based on test-specific Conditional Standard Errors of Measurement (CSEM). 
  Test CSEM data must be available for simulation.  Must be set to TRUE for confidence interval construction. Defaults to TRUE in \code{abcSGP} only.
  }
  \item{calculate.simex}{A character state acronymn or list including state/csem variable, simulation.iterations, lambda and extrapolation method. Default to NULL, no simex calculations done.
  }
 \item{parallel.config}{A named list with, at a minimum, two elements indicating 1) the BACKEND package to be used for parallel computation and 2) the WORKERS list to specify the number of processors to be used in each major analysis.  The BACKEND element can be set = to \code{FOREACH} or \code{PARALLEL}.  Please consult the manuals and vignettes for information of these packages!  The \code{\link{analyzeSGP}} help page contains more thorough explanation and examples of the \code{parallel.config} setup. 
 
 The parallel.config list is passed to  \code{\link{analyzeSGP}},  \code{\link{summarizeSGP}} and  \code{\link{visualizeSGP}}.  The WORKERS list can accordingly contain elements for PERCENTILES, PROJECTIONS, LAGGED_PROJECTIONS, BASELINE_MATRICES, BASELINE_PERCENTILES for \code{analyzeSGP}, SUMMARY for \code{summarizeSGP} and GA_PLOTS and SG_PLOTS for \code{visualizeSGP}.  See those functions help pages for details.
  }
  \item{save.intermediate.results}{Should intermediate results of \code{abcSGP} be saved after each of \code{\link{prepareSGP}}, \code{\link{analyzeSGP}},
  \code{\link{combineSGP}}, and \code{\link{summarizeSGP}}. Default is FALSE.
  }
  \item{save.old.summaries}{A boolean argument (defaults to FALSE which will delete the @Summary slot before creating new summaries) 
  indicating whether the call to \code{\link{summarizeSGP}} should save existing summaries in the @Summary slot. 
  }
  \item{sgPlot.demo.report}{A boolean variable (defaults to FALSE) indicating whether to produce only the demonstration student report catalog. Default is to produce reports for entire current 
  year data.
  }
  \item{sgp.summaries}{A list giving the summaries requested for each group analyzed based upon the \code{summary.group} argument. Default is NULL allowing the \code{\link{summarizeSGP}} 
  function to produce the list of summaries automatically.
  }
  \item{summary.groups}{A list consisting of 8 types of groups across which all summaries are taken: \code{institution}, \code{content}, \code{time}, \code{institution_type}, 
  \code{institution_level}, \code{demographic}, and \code{institution_inclusion}. Summaries generated in summarizeSGP are for all possible combinations of the 8 types of group. See documentation for 
  \code{\link{summarizeSGP}} for more detail. 
  }
  \item{data_supplementary}{A list argument (or NULL, the default) providing additional multiple membership lookup tables for \code{\link{summarizeSGP}}. See \code{\link{sgpData_INSTRUCTOR_NUMBER}}
  for an example. Supplied data is embedded in the @Data_Supplementary slot.
  }
  \item{confidence.interval.groups}{A subset of the groups provided in the \code{summary.groups} argument indicating which groups to provide confidence intervals for. 
  See documentation for \code{\link{summarizeSGP}} for more detail.
  }
  \item{plot.types}{A character vector passed to \code{\link{visualizeSGP}} indicating the types of plots to produce. Currently supported plots include \code{\link{bubblePlot}}s, 
  \code{\link{studentGrowthPlot}}s, and \code{\link{growthAchievementPlot}}s. 
  }
  \item{verbose.output}{A boolean argument indicating whether the function should output verbose diagnostic messages.
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID}, SGP results including student growth percentile and student growth projections/trajectories in the SGP slot, and summary results in the 
Summary slot. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}},  \cr
\code{\link{studentGrowthPercentiles}}, and \code{\link{studentGrowthProjections}}}

\examples{
\dontrun{

## Runs all 5 steps
Demonstration_SGP <- abcSGP(sgp_object=sgpData_LONG, state="DEMO")


## Or letting the function detect the state

Demonstration_SGP <- sgpData_LONG
Demonstration_SGP <- abcSGP(Demonstration_SGP)


###
###  Example uses of the parallel.config argument
###

Demonstration_SGP <- sgpData_LONG
Demonstration_SGP <- abcSGP(Demonstration_SGP,
	parallel.config=list(
	    BACKEND="PARALLEL", TYPE="SOCK",
	    WORKERS=list(
	       PERCENTILES=8, BASELINE_PERCENTILES=8, PROJECTIONS=7, LAGGED_PROJECTIONS=6,
	       SUMMARY=8, 
	       GA_PLOTS=8, SG_PLOTS=8)
	)
)

}
}

\keyword{documentation}
