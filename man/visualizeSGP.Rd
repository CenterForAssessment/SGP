\name{visualizeSGP}
\alias{visualizeSGP}
\title{Visualize data from SGP analyses}
\description{
Utility function to produce a variety of graphical displays associated with student growth percentile/percentile growth trajectory results.
Function currently includes facility to produce individual student growth and achievement plots, interactive bubble plots depicting summary growth
and achievement data, and growth and achievement charts showing system level growth and achievement data as shown on the cover of \emph{Educational Measurement:
Issues and Practice} as part of Betebenner (2009) and Betebenner (2012).
}

\usage{
visualizeSGP(
        sgp_object,
        plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
        state,
        bPlot.years=NULL,
        bPlot.content_areas=NULL,
        bPlot.districts=NULL,
        bPlot.schools=NULL,
        bPlot.instructors=NULL,
        bPlot.styles=c(1),
        bPlot.levels=NULL,
        bPlot.level.cuts=NULL,
        bPlot.full.academic.year=TRUE,
        bPlot.minimum.n=10,
        bPlot.anonymize=FALSE,
        bPlot.prior.achievement=TRUE,
        bPlot.draft=FALSE,
        bPlot.demo=FALSE,
        bPlot.output="PDF",
        bPlot.format="print",
        bPlot.folder="Visualizations/bubblePlots",
        sgPlot.save.sgPlot.data=FALSE,
        sgPlot.years=NULL,
        sgPlot.content_areas=NULL,
        sgPlot.districts=NULL,
        sgPlot.schools=NULL,
        sgPlot.reports.by.school=TRUE,
        sgPlot.instructors=NULL,
        sgPlot.reports.by.instructor=FALSE,
        sgPlot.students=NULL,
        sgPlot.reports.by.student=FALSE,
        sgPlot.reports.group.vars=
            list(DISTRICT_NUMBER="DISTRICT_NUMBER", SCHOOL_NUMBER="SCHOOL_NUMBER"),
        sgPlot.header.footer.color="#4CB9CC",
        sgPlot.front.page=NULL,
        sgPlot.folder="Visualizations/studentGrowthPlots",
        sgPlot.folder.names="number",
        sgPlot.fan=TRUE,
        sgPlot.custom.trajectory=FALSE,
        sgPlot.sgp.targets=FALSE,
        sgPlot.sgp.targets.timeframe=3,
        sgPlot.anonymize=FALSE,
        sgPlot.cleanup=TRUE,
        sgPlot.demo.report=FALSE,
        sgPlot.produce.plots=TRUE,
        sgPlot.baseline=NULL,
        sgPlot.zip=TRUE,
        sgPlot.output.format="PDF",
        sgPlot.year.span=5,
        sgPlot.plot.test.transition=TRUE,
        gaPlot.years=NULL,
        gaPlot.content_areas=NULL,
        gaPlot.students=NULL,
        gaPlot.format="print",
        gaPlot.baseline=NULL,
        gaPlot.max.order.for.progression=NULL,
        gaPlot.start.points="Achievement Level Cuts",
        gaPlot.back.extrapolated.cuts=NULL,
        gaPlot.SGPt=NULL,
        gaPlot.folder="Visualizations/growthAchievementPlots",
        parallel.config=NULL)
}

\arguments{
  \item{sgp_object}{An object of class SGP containing long formatted data in the \code{@Data} slot that will be used for the production of student growth
  and achievement plots and system growth and achievement plots, summary data from \code{\link{summarizeSGP}} in the \code{Summary} slot for bubble plots.  Alternatively, a properly formatted WIDE data set can be provided for production of student growth plots.  Such data sets are produced from an SGP object when the sgPlot.save.sgPlot.data argument is set to TRUE.
  }
  \item{plot.types}{A character vector indicating what types of plots to construct. Currently available plots include \code{\link{bubblePlot}},
  \code{\link{studentGrowthPlot}}, and \code{\link{growthAchievementPlot}}.
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{SGPstateData}.
  }
  \item{bPlot.years}{A vector indicating year(s) in which to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
  function will use the last year available in the data to produce \code{\link{bubblePlot}}s.
  }
  \item{bPlot.content_areas}{A vector indicating content area(s) to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
  function will produce plots for all available content areas provided in the data.
  }
  \item{bPlot.districts}{A vector indicating districts to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. Consult \code{\link{bubblePlot}}
  styles to determine which \code{\link{bubblePlot}}s styles accept specification for districts. Default is to produce plots for all available districts in the data.
  }
  \item{bPlot.schools}{A vector indicating schools to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. Consult \code{\link{bubblePlot}}
  styles to determine which \code{\link{bubblePlot}} styles accept specification for schools. Default is to produce plots for all available schools in the data.
  }
  \item{bPlot.instructors}{A vector indicating instructors to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
function will produce plots for all available instructors provided in the data where schools and districts represent relevant units to be represented by the specific \code{bubblePlot} style.
  }
  \item{bPlot.styles}{A vector of positive integers indicating the types of \code{\link{bubblePlot}}s to produce using data available in \code{sgp_object}.
  See associated documentation for example plots.
  }
  \item{bPlot.levels}{A character vector of levels to be used to color bubbles in the \code{\link{bubblePlot}}. See associated documentation for example plots.
  }
  \item{bPlot.level.cuts}{A vector of cuts to be used to distinguish levels used to color bubbles in the \code{\link{bubblePlot}}. See associated
  documentation for example plots.
  }
  \item{bPlot.full.academic.year}{A Boolean argument (defaults to TRUE) indicating whether \code{\link{bubblePlot}}s should use full academic year results if available.
  }
  \item{bPlot.minimum.n}{A positive integer (defaults to 10) indicating the minimum size for summary values to be displayed in the \code{\link{bubblePlot}}s.
  }
  \item{bPlot.anonymize}{A Boolean argument (defaults to FALSE) indicating whether to anonymize \code{\link{bubblePlot}}s school and district names that appear in the
  plots and data tips of the plots.  For student level anonymization, the function utilizes the randomNames package to produce gender and ethnic correct names based
  upon gender and ethnicity codes available in \code{sgp_object@Data}.
  }
  \item{bPlot.prior.achievement}{A Boolean argument (defaults to TRUE) indicating whether to produce \code{\link{bubblePlot}}s using prior achievement as well as current
  achievement as the vertical dimension of the \code{\link{bubblePlot}}.
  }
  \item{bPlot.draft}{A Boolean argument (defaults to FALSE) indicating whether to put an indicator on the chart noting that the results are draft and to not distribute.
  }
  \item{bPlot.demo}{A Boolean argument (defaults to FALSE) indicating whether to produce demo student level plots (styles 150 and/or 153) for instructors.
  }
  \item{bPlot.output}{Argument indicating the desired type of output format for bubble plots. Either 'PDF' (default) or 'PNG'.
  }
  \item{bPlot.format}{Either "print" or "presentation" indicating whether to optimize the plot for print form (light background) or presentation form (dark background).
  }
  \item{bPlot.folder}{Character vector indicating where \code{\link{bubblePlot}}s should be placed. Default folder is "Visualizations/bubblePlots".
  }
  \item{sgPlot.save.sgPlot.data}{A Boolean argument indicating whether wide formatted data should be save as part of individual student report production. For operational
  student report production construction, using a wide formatted file for a large state can save in having to reshape the same long file multiple times.
  }
  \item{sgPlot.years}{A vector indicating year for which to produce \code{\link{studentGrowthPlot}}s. The supplied year indicates the \emph{final} year associated with each
  student's \code{\link{studentGrowthPlot}}. If missing the function will use the last year available in the data to produce \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.content_areas}{A vector indicating the content areas for which to produce \code{\link{studentGrowthPlot}}s. If missing, the function will utilize all available
  content areas.
  }
  \item{sgPlot.districts}{A vector indicating which districts to produce \code{\link{studentGrowthPlot}}s for. If missing the function will use \emph{all} available
  districts in the data to produce \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.schools}{A vector indicating which schools to produce \code{\link{studentGrowthPlot}}s for. If missing the function will use \emph{all} available
  schools in the data to produce \code{\link{studentGrowthPlot}}s. If both \code{sgPlot.districts} and \code{sgPlot.schools} are provided the function produces
  \code{\link{studentGrowthPlot}}s for ALL students in the districts and schools provided.
  }
  \item{sgPlot.reports.by.school}{A Boolean variable indicating whether the reports should be collated as single reports in a single folder by school and deposited into a
  \code{district/school/grade folder} hierarchy. The default is TRUE which puts the reports into their appropriate \code{district/school/grade} folder.
  }
  \item{sgPlot.instructors}{A vector indicating which instructors to produce \code{\link{studentGrowthPlot}}s for. If NULL and the argument \code{sgPlot.reports.by.instructor} is TRUE,
the argument function will use \emph{all} available instructors in the data to produce \code{\link{studentGrowthPlot}}s. If \code{sgPlot.districts} and/or \code{sgPlot.schools} are
provided the function produces \code{\link{studentGrowthPlot}}s for ALL students in the districts and/or schools provided.
  }
  \item{sgPlot.reports.by.instructor}{A Boolean variable indicating whether the reports should be collated as single reports in a single folder by school and deposited into a
  \code{district/school/grade folder} hierarchy. The default is TRUE which puts the reports into their appropriate \code{district/school/grade} folder.
  }
  \item{sgPlot.students}{A vector of student \code{ID}s indicating which students to produce \code{\link{studentGrowthPlot}}s for. If missing the function will
  use \emph{all} available students in the data to produce \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.reports.group.vars}{A list of variables used to group student reports. Default is \code{list(DISTRICT_NUMBER='DISTRICT_NUMBER', SCHOOL_NUMBER='SCHOOL_NUMBER')}
  }
  \item{sgPlot.reports.by.student}{A Boolean variable indicating whether the reports should be collated as single reports in a single folder or deposited into a
  \code{district/school/grade folder} hierarchy. The default is FALSE which puts the reports into their appropriate \code{district/school/grade} slot.
  }
  \item{sgPlot.header.footer.color}{Character vector (default is blue) indicating the color of the header/footer associated with the
  \code{\link{studentGrowthPlot}}. Another good color is \code{goldenrod2}.
  }
  \item{sgPlot.front.page}{A path to a PDF to be used as the front page to the \code{\link{studentGrowthPlot}}. The default is missing so that no front page is
  attached to the \code{\link{studentGrowthPlot}}.
  }
  \item{sgPlot.folder}{Character vector indicating where \code{\link{studentGrowthPlot}}s should be placed. Note that \code{\link{studentGrowthPlot}}s are placed
  within nested folders within this folder. Default folder is "Visualizations/studentGrowthPlots".
  }
  \item{sgPlot.folder.names}{Either "name" or "number" (the default) indicating how the nested folder structure will be labeled that holds the
\code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.fan}{A Boolean argument (defaults to TRUE) indicating whether to produce projection fan on \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.custom.trajectory}{A Boolean argument (defaults to FALSE) or a character vector indicating the variable name containing the custom trajectory coordinates to be plotted.
  }
  \item{sgPlot.sgp.targets}{A Boolean argument (defaults to TRUE) indicating whether to indicate SGP growth targets on \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.sgp.targets.timeframe}{An integer argument specifying the number of years forward associated with targets to be added to the student growth plots.
  }
  \item{sgPlot.anonymize}{A Boolean argument (defaults to FALSE) indicating whether to anonymize \code{\link{studentGrowthPlot}}s student, school and district names.
  For student level anonymization, the function utilizes the \code{randomNames} package to produce gender and ethnicity based names based upon gender and ethnicity
  codes available in \code{sgp_object@Data}.
  }
  \item{sgPlot.cleanup}{A Boolean argument (defaults to TRUE) indicating whether to remove files produced by pdfLaTeX to produce \code{\link{studentGrowthPlot}} catalogs.
  }
  \item{sgPlot.demo.report}{A Boolean argument (defaults to TRUE) indicating whether to just produce a sample \code{\link{studentGrowthPlot}} catalogs. Note: When producing
  \code{\link{studentGrowthPlot}}s for an entire state, considerable resources are required to produce this many reports. We are actively working on parallelizing this
  functionality to reduce report production time by two orders of magnitude.
  }
  \item{sgPlot.produce.plots}{A Boolean argument (defaults to TRUE) indicating whether to produce \code{\link{studentGrowthPlot}}s. Useful when one just wants to produce
  wide formatted data without the actual student growth plots.
  }
  \item{sgPlot.baseline}{Argument (defaults to NULL) indicating whether to use baseline referenced SGPs for student growth plot construction.  If not set by user,
  argument will be set using \code{SGPstateData} which contains information on whether state is a cohort or baseline referenced system.
  }
  \item{sgPlot.zip}{A Boolean argument (defaults to TRUE) indicating whether to zip school folders containing \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.output.format}{Argument indicating the desired type of output format for student growth plots. Either 'PDF' (default), 'PNG', 'PDF_PIECES', or 'JSON'.
  }
  \item{sgPlot.year.span}{Integer argument (defaults to 5) indicating the number of years to display for the pdf student growth plot.
  }
  \item{sgPlot.plot.test.transition}{Boolean argument (defaults to TRUE) indicating whether test transition data will be used IF it's available. Setting to FALSE ignore previous
  test transition data, which would indicate a desire to 'forget the past'.
  }
  \item{gaPlot.years}{A vector indicating the year(s) for which to produce \code{\link{growthAchievementPlot}}s. If missing the function will use the last year available
  in the data to produce the \code{\link{growthAchievementPlot}}s.
  }
  \item{gaPlot.content_areas}{A vector indicating content area(s) to produce \code{\link{growthAchievementPlot}}s using data available in \code{sgp_object}.
  If missing the function will produce plots for all available content areas provided in the data.
  }
  \item{gaPlot.students}{A vector of student \code{ID}s indicating which students to produce \code{\link{growthAchievementPlot}}s for. If missing the function will
  use \emph{all} available students in the data to produce \code{\link{growthAchievementPlot}}s.
  }
  \item{gaPlot.format}{Either 'print' or 'presentation' indicating whether to optimize the plot for print form (light background) or presentation form (dark background).
  }
  \item{gaPlot.baseline}{Argument (defaults to NULL) indicating whether to calculate growth and achievement plots using percentile trajectories derived from baseline
  referenced coefficient matrices. If not set by user, argument will be set using \code{SGPstateData} which contains information on whether state is a cohort or
  baseline referenced system.
  }
  \item{gaPlot.max.order.for.progression}{The maximum coefficient matrix order to use for each progression. Default is NULL which utilizes the maximum order available
    with the coefficient matrices.
  }
  \item{gaPlot.start.points}{Either 'Achievement Level Cuts' or 'Achievement Percentiles' defining where the percentile trajectories of the growth achievement plot will start from.
  }
  \item{gaPlot.folder}{Character vector indicating where \code{\link{growthAchievementPlot}}s should be placed. The default folder is 'Visualizations/growthAchievementPlots'.
  }
  \item{gaPlot.back.extrapolated.cuts}{A numeric value or Boolean vector indicating whether to create back extrapolated cutscores based upon nth percentile growth.
  }
  \item{gaPlot.SGPt}{Boolean indicator for growth achievement plot indicating whether time dependent SGPs are to be used for student growth projections \code{SGPt}
  }
  \item{parallel.config}{A named list with, at a minimum, two elements indicating 1) the BACKEND package to be used for parallel computation and 2) the WORKERS list to specify the number of processors to be used in each major analysis.  The BACKEND element can be set = to \code{FOREACH} or \code{PARALLEL}.  Please consult the manuals and vignettes for information of these packages!  The \code{\link{analyzeSGP}} help page contains more thorough explanation and examples of the \code{parallel.config} setup.

TYPE is a third element of the \code{parallel.config} list that provides necessary information when using FOREACH or PARALLEL packages as the backend. With BACKEND="FOREACH",  the TYPE element specifies the flavor of 'foreach' backend.  As of version 1.0-1.0, only "doParallel" is supported.  TYPE=NA (default) produces summaries sequentially.  If BACKEND = "PARALLEL", the \code{parallel} package will be used.  This package combines deprecated parallel packages \code{snow} and \code{multicore}.  Using the "snow" implementation of \code{parallel} the function will create a cluster object based on the TYPE element specified and the number of workers requested (see WORKERS list description below).  The TYPE element indicates the users preferred cluster type (either "PSOCK" for socket cluster of "MPI" for an OpenMPI cluster). If Windows is the operating system, this "snow" implementation must be used and the TYPE element must = "PSOCK".  Defaults are assigned based on operating system if TYPE is missing based on system OS.  Unix/Mac OS defaults to the "multicore" to avoid worker node pre-scheduling and appears to be more efficient in these operating systems.

The WORKERS element is a list with GA_PLOTS (growth achievement plots) and SG_PLOTS (student growth plots) specifying the number of processors to be used.  NOTE: choice of the number of cores is a balance between the number of processors available and the amount of RAM a system has; each system will be different and may require some adjustment.

Default is FOREACH as the back end, TYPE=NA and both plot WORKERS=1, which produces plots sequentially: 'list(BACKEND="FOREACH", TYPE=NA, WORKERS=list(GA_PLOTS=1, SG_PLOTS=1))'

Examples of various parallel configurations can be found in the examples for \code{\link{analyzeSGP}} and \code{\link{summarizeSGP}}.
  }
}

\value{Function produces \emph{numerous} (potentially hundreds of thousands) of pdf files in a folder structure specified by the user and supplied through arguments
to the function.
}


\references{
Betebenner, D. W. (2012). Growth, standards, and accountability. In G. J. Cizek,
\emph{Setting Performance Standards: Foundations, Methods & Innovations. 2nd Edition} (pp. 439-450).
New York: Routledge.

Betebenner, D. W. (2009). Norm- and criterion-referenced student growth.
Educational Measurement: Issues and Practice, 28(4):42-51.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{avaniwaarden@nciea.org}}

\seealso{\code{\link{bubblePlot}}, \code{\link{bubblePlot_Styles}}, \code{\link{studentGrowthPlot}}, \code{\link{growthAchievementPlot}}}

\examples{
\dontrun{
## visualizeSGP is Step 5 of 5 of abcSGP
Demonstration_SGP <- sgpData_LONG
Demonstration_SGP <- prepareSGP(Demonstration_SGP)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP)
Demonstration_SGP <- combineSGP(Demonstration_SGP)
Demonstration_SGP <- summarizeSGP(Demonstration_SGP)
visualizeSGP(Demonstration_SGP)

## Produce a DEMO catalog of student growth plots

visualizeSGP(
	sgp_object=Demonstration_SGP,
	plot.types="studentGrowthPlot",
	state="DEMO",
	sgPlot.demo.report=TRUE)

## Production of sample student growth and achievement plots

visualizeSGP(
	sgp_object=Demonstration_SGP,
	plot.types="studentGrowthPlot",
	state="DEMO",
	sgPlot.districts=470,
	sgPlot.schools=c(6418, 8008),
	sgPlot.header.footer.color="#4CB9CC")
}
}

\keyword{documentation}
