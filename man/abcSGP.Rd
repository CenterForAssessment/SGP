\name{abcSGP}
\alias{abcSGP}
\title{Perform 6 step sequence: prepareSGP, analyzeSGP, combineSGP, summarizeSGP, visualizeSGP, and outputSGP}
\description{
  Utility function to perform sequence of 6 steps going from data preparation, \code{\link{prepareSGP}}, SGP data analysis, \code{\link{analyzeSGP}},
  data combining, \code{\link{combineSGP}}, data summary, \code{\link{summarizeSGP}}, data visualization \code{\link{visualizeSGP}} and data output
  \code{\link{outputSGP}}.
}

\usage{
abcSGP(sgp_object,
	state=NULL,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP",
		"summarizeSGP", "visualizeSGP", "outputSGP"),
	years=NULL,
	content_areas=NULL,
	grades=NULL,
	prepareSGP.var.names=NULL,
	prepareSGP.create.additional.variables=FALSE,
	sgp.percentiles=TRUE,
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	sgp.use.my.coefficient.matrices=NULL,
	sgp.minimum.default.panel.years=NULL,
	sgp.target.scale.scores=FALSE,
	sgp.target.scale.scores.only=FALSE,
	sgp.test.cohort.size=NULL,
	return.sgp.test.results=FALSE,
	simulate.sgps=TRUE,
	calculate.simex=NULL,
	calculate.simex.baseline=NULL,
	goodness.of.fit.print=TRUE,
	parallel.config=NULL,
	save.intermediate.results=FALSE,
	save.old.summaries=FALSE,
	sgPlot.demo.report=FALSE,
	sgp.config=NULL,
	sgp.summaries=NULL,
	summary.groups=NULL,
	data_supplementary=NULL,
	confidence.interval.groups=NULL,
	plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
	outputSGP.output.type=c("LONG_Data",
				"LONG_FINAL_YEAR_Data",
				"WIDE_Data",
				"INSTRUCTOR_Data"),
	outputSGP.directory="Data",
  verbose.output=FALSE,
	sgp.sqlite=FALSE,
	sgp.percentiles.equated=NULL,
    sgp.percentiles.equating.method=NULL,
	sgp.percentiles.calculate.sgps=TRUE,
    get.cohort.data.info=FALSE,
	SGPt=NULL,
  fix.duplicates=NULL)
}

\arguments{
  \item{sgp_object}{A list containing LONG formatted data. See \code{\link[SGPdata]{sgpData_LONG}} for an exemplar. By including the name of the state in the object name (e.g., Idaho_SGP), the function
  will detect what state is associated with the data and supply that to the 'state' argument of the function so that state meta-data located in the SGPstateData object can be utilized.
  NOTE: Data preparation must be meticulous to utilize this enhanced functionality.
  }
  \item{state}{Acronym indicating state associated with the data for access to embedded knot and boundaries,  cutscores, CSEMs, and other relevant state level data. This can be supplied to the functional
  automatically by including the full state name (e.g, New_Hampshire_SGP) in the name of the object supplied to \code{sgp_object}.
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
  \item{prepareSGP.var.names}{list supplied to prepareSGP mapping provided variable names to variable names required as part of the SGP package. See \code{\link{prepareSGP}} for more details. Defaults to NULL.}
  \item{prepareSGP.create.additional.variables}{Boolean variable indicating whether prepareSGP should create addition variables (e.g., ACHIEVEMENT_LEVEL) if they are missing.}
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
  \item{sgp.minimum.default.panel.years}{Integer indicating the minimum number of panel years to begin with in the calculation of student growth percentiles. The default is NULL (converted to 3 years).
  }
  \item{sgp.target.scale.scores}{Boolean variable passed to \code{\link{combineSGP}} indicating whether target scale scores associated with SGP_TARGETs should be calculated as
  part of the \code{\link{combineSGP}} run. Defaults to FALSE.
  }
  \item{sgp.target.scale.scores.only}{Boolean variable passed to \code{\link{combineSGP}} indicating whether ONLY target scale scores associated with SGP_TARGETs should be calculated as
  part of the \code{\link{combineSGP}} run. Defaults to FALSE.
  }
  \item{sgp.test.cohort.size}{Integer indicating the maximum number of students sampled from the full cohort to use in the calculation of student growth percentiles.  Intended to be used
  as a test of the desired analyses to be run. The default, NULL, uses no restrictions (no tests are performed, and analyses use the entire cohort of students).
  }
  \item{return.sgp.test.results}{Boolean variable passed to \code{\link{analyzeSGP}} and \code{\link{studentGrowthPercentiles}} indicating whether the results from the cohort sample subset (if specified using the above argument) should be returned for inspection. Defaults to FALSE. If TRUE, only the sample subset of the data used will be returned in the SGP object's @Data slot. Alternatively, user can supply the character "ALL_DATA" to the argument to return the entire original data.
  }
  \item{simulate.sgps}{Boolean variable indicating whether to simulate SGP values for students based on test-specific Conditional Standard Errors of Measurement (CSEM).
  Test CSEM data must be available for simulation.  Must be set to TRUE for confidence interval construction. Defaults to TRUE in \code{abcSGP} only.
  }
  \item{calculate.simex}{A character state acronym or list including state/csem variable, csem.data.vnames, csem.loss.hoss, simulation.iterations, lambda and extrapolation method.
   Returns both SIMEX adjusted SGP (\code{SGP_SIMEX}) as well as the percentile ranked SIMEX SGP (\code{RANK_SIMEX}) values as suggested by Castellano and McCaffrey (2017).
   Defaults to NULL, no simex calculations performed.  Alternatively, setting the argument to TRUE sets the list up with state=state, lambda=seq(0,2,0.5), simulation.iterations=50, simex.sample.size=25000, extrapolation="linear" and save.matrices=TRUE.
  }
  \item{calculate.simex.baseline}{A character state acronym or list including state/csem variable, csem.data.vnames, csem.loss.hoss, simulation.iterations, lambda and extrapolation method. Defaults to NULL, no simex calculations performed.
  Alternatively, setting the argument to TRUE uses the same defaults as above along with \code{simex.use.my.coefficient.matrices = TRUE}. This argument is passed to \code{\link{analyzeSGP}}.
  }
  \item{goodness.of.fit.print}{Boolean variable passed to \code{\link{analyzeSGP}} indicating whether to print goodness of fit results.
  }
 \item{parallel.config}{A named list with, at a minimum, two elements indicating 1) the BACKEND package to be used for parallel computation and 2) the WORKERS list to specify the number of processors to be used in each major analysis.  The BACKEND element can be set = to \code{FOREACH} or \code{PARALLEL}.  Please consult the manuals and vignettes for information of these packages!  The \code{\link{analyzeSGP}} help page contains more thorough explanation and examples of the \code{parallel.config} setup.

 The parallel.config list is passed to  \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}} and \code{\link{visualizeSGP}}.  The WORKERS list can accordingly contain
 elements for PERCENTILES, PROJECTIONS, LAGGED_PROJECTIONS, BASELINE_MATRICES, BASELINE_PERCENTILES for \code{analyzeSGP}, SUMMARY for \code{summarizeSGP} and GA_PLOTS and SG_PLOTS for
 \code{visualizeSGP}.  See those functions help pages for details.
  }
  \item{save.intermediate.results}{Should intermediate results of \code{abcSGP} be saved after each of \code{\link{prepareSGP}}, \code{\link{analyzeSGP}},
  \code{\link{combineSGP}}, and \code{\link{summarizeSGP}}. Default is FALSE.
  }
  \item{save.old.summaries}{A Boolean argument (defaults to FALSE which will delete the \code{@Summary} slot before creating new summaries)
  indicating whether the call to \code{\link{summarizeSGP}} should save existing summaries in the \code{@Summary} slot.
  }
  \item{sgPlot.demo.report}{A Boolean variable (defaults to FALSE) indicating whether to produce only the demonstration student report catalog. Default is to produce reports for entire current
  year data.
  }
  \item{sgp.config}{Configuration passed to \code{\link{analyzeSGP}} and \code{\link{combineSGP}} for user specified SGP analyses. See \code{\link{analyzeSGP}} documentation for details on format of configuration argument.
  }
  \item{sgp.summaries}{A list giving the summaries requested for each group analyzed based upon the \code{summary.group} argument. Default is NULL allowing the \code{\link{summarizeSGP}}
  function to produce the list of summaries automatically.
  }
  \item{summary.groups}{A list consisting of 8 types of groups across which all summaries are taken: \code{institution}, \code{content}, \code{time}, \code{institution_type},
  \code{institution_level}, \code{demographic}, and \code{institution_inclusion}. Summaries generated in summarizeSGP are for all possible combinations of the 8 types of group. See documentation for
  \code{\link{summarizeSGP}} for more detail.
  }
  \item{data_supplementary}{A list argument (or NULL, the default) providing additional multiple membership lookup tables for \code{\link{summarizeSGP}}. See \code{\link[SGPdata]{sgpData_INSTRUCTOR_NUMBER}}
  for an example. Supplied data is embedded in the @Data_Supplementary slot.
  }
  \item{confidence.interval.groups}{A subset of the groups provided in the \code{summary.groups} argument indicating which groups to provide confidence intervals for.
  See documentation for \code{\link{summarizeSGP}} for more detail.
  }
  \item{plot.types}{A character vector passed to \code{\link{visualizeSGP}} indicating the types of plots to produce. Currently supported plots include \code{\link{bubblePlot}}s,
  \code{\link{studentGrowthPlot}}s, and \code{\link{growthAchievementPlot}}s.
  }
  \item{outputSGP.output.type}{An argument passed to outputSGP indicating the output types to be produced. Defaults to \code{LONG_Data}, \code{LONG_FINAL_YEAR_Data}, \code{WIDE_Data},
  and \code{INSTRUCTOR_Data}.
  }
  \item{outputSGP.directory}{A a file path indicating where to save output files. Defaults to \code{Data}.
  }
  \item{verbose.output}{A Boolean argument indicating whether the function should output verbose diagnostic messages.
  }
  \item{sgp.sqlite}{A Boolean argument (defaults to FALSE) indicating whether a SQLite database file of the essential SGP data should be created from the \code{@Data} slot and subsequently
  used to extract data subsets for analyses conducted in order to reduce the amount of RAM memory required.  See full argument description in \code{\link{analyzeSGP}}.
  }
  \item{sgp.percentiles.equated}{A Boolean argument (defaults to NULL, which calculates equated results if in the equating year) passed to \code{\link{analyzeSGP}}
  indicating whether equating should be used on the most recent year of test data provided. Equating allows for student growth projections to be calculated in
  across assessment transitions where the scale for the assessment changes.
  }
  \item{sgp.percentiles.equating.method}{Character vector argument passed to \code{\link{analyzeSGP}} indicating type(s) of equating method to used if \code{sgp.percentiles.equated=TRUE}.
  Default is NULL indicating 'equipercentile' equating. Options include 'identity', 'mean', 'linear', and 'equipercentile'.
  }
  \item{sgp.percentiles.calculate.sgps}{Boolean argument passed to \code{\link{analyzeSGP}} indicating whether student growth percentiles are
produced as part of calls to the \code{\link{studentGrowthPercentiles}} function. Default is TRUE. Setting to FALSE produces only coefficient
matrices.
  }
  \item{get.cohort.data.info}{Boolean argument passd to \code{\link{analyzeSGP}} indicating whether to create norm group cohort
  information based upon sgp configurations provided or calculated in \code{\link{analyzeSGP}}.
  }
  \item{SGPt}{Argument (defaults to NULL) indicating whether to perform time dependent SGP analyses (SGPt).
  }
  \item{fix.duplicates}{Argument to control how \code{analyzeSGP} and \code{combineSGP} deal with duplicate records based upon the key of VALID_CASE, CONTENT_AREA, YEAR, and ID.
  The function currently warns of duplicate records and doesn't modify data.  If set to 'KEEP.ALL', \code{analyzeSGP} tries to fix the duplicate individual records by
  adding a '_DUP_***' suffix to the duplicate ID before running \code{\link{studentGrowthPercentiles}} in order to create unique records based upon the key.  If needed, the
	\code{@Data} slot will be extended as necessary to accomodate additional student records and SGP results in \code{combineSGP}.
  }
}

\value{Function returns a list containing the input long data set in the \code{@Data} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA},
\code{YEAR}, \code{ID}, SGP results including student growth percentile and student growth projections/trajectories in the SGP slot, and summary results in the
\code{@Summary} slot.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}},  \cr
\code{\link{studentGrowthPercentiles}}, and \code{\link{studentGrowthProjections}}}

\examples{
\dontrun{
## Runs all 5 steps

Demonstration_SGP <- abcSGP(sgp_object=sgpData_LONG, state="DEMO")


## Or letting the function detect the state.

Demonstration_SGP <- abcSGP(sgpData_LONG)


###
###  Example uses of the parallel.config argument
###

Demonstration_SGP <- abcSGP(sgpData_LONG,
	parallel.config=list(
            BACKEND="PARALLEL", TYPE="PSOCK",
            WORKERS=list(
               PERCENTILES=8, BASELINE_PERCENTILES=8, PROJECTIONS=7, LAGGED_PROJECTIONS=6,
               SUMMARY=8,
               GA_PLOTS=8, SG_PLOTS=8)
	)
)

}
}

\keyword{documentation}
