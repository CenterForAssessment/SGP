\name{studentGrowthPercentiles}
\alias{studentGrowthPercentiles}
\title{Student Growth Percentiles}
\description{
Function to calculate student growth percentiles using large scale assessment data.
Outputs growth percentiles for each student and supplies various options as function arguments.
Results from this function are utilized to calculate percentile growth projections/trajectories
using the \cr
\code{\link{studentGrowthProjections}} function.
}
\usage{
studentGrowthPercentiles(panel.data,
                         sgp.labels,
                         panel.data.vnames=NULL,
                         additional.vnames.to.return=NULL,
                         grade.progression,
                         content_area.progression,
                         year.progression,
                         year_lags.progression,
                         num.prior,
                         max.order.for.percentile=NULL,
                         return.additional.max.order.sgp=NULL,
                         subset.grade,
                         percentile.cuts,
                         growth.levels,
                         use.my.knots.boundaries,
                         use.my.coefficient.matrices,
                         calculate.confidence.intervals,
                         print.other.gp=FALSE,
                         print.sgp.order=FALSE,
                         calculate.sgps=TRUE,
                         rq.method="br",
                         rq.method.for.large.n="fn",
                         max.n.for.coefficient.matrices=NULL,
                         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),
                         knots.boundaries.by.panel=FALSE,
                         exact.grade.progression.sequence=FALSE,
                         drop.nonsequential.grade.progression.variables=TRUE,
                         convert.0and100=TRUE,
                         sgp.quantiles="Percentiles",
                         sgp.quantiles.labels=NULL,
                         sgp.loss.hoss.adjustment=NULL,
                         sgp.cohort.size=NULL,
                         sgp.less.than.sgp.cohort.size.return=NULL,
                         sgp.test.cohort.size=NULL,
                         percuts.digits=0L,
                         isotonize=TRUE,
                         convert.using.loss.hoss=TRUE,
                         goodness.of.fit=TRUE,
                         goodness.of.fit.minimum.n=NULL,
                         goodness.of.fit.output.format="GROB",
                         return.prior.scale.score=TRUE,
                         return.prior.scale.score.standardized=TRUE,
                         return.norm.group.identifier=TRUE,
                         return.norm.group.scale.scores=NULL,
                         return.norm.group.dates=NULL,
                         return.panel.data=identical(parent.frame(), .GlobalEnv),
                         print.time.taken=TRUE,
                         parallel.config=NULL,
                         calculate.simex=NULL,
                         sgp.percentiles.set.seed=314159,
                         sgp.percentiles.equated=NULL,
                         SGPt=NULL,
                         SGPt.max.time=NULL,
                         verbose.output=FALSE)
}

\arguments{
  \item{panel.data}{REQUIRED. Object of class list, data.frame, or matrix containing longitudinal student data in wide format. If supplied as part of a list, data should be
  contained in \code{panel.data$Panel_Data}. Data must be formatted so that student ID is the first variable/column, student grade/time variables for each time period,
  from earliest to most recent, are the next variables/columns, and student scale score variables for each year, from earliest to latest, are the remaining
  variables/columns. See \code{\link[SGPdata]{sgpData}} for an exemplar data set. NOTE: The column position of the variables IS IMPORTANT, NOT the names of the variables.
  }
  \item{sgp.labels}{REQUIRED. A list, \code{sgp.labels}, of the form  \code{list(my.year= ,} \cr
  \code{my.subject= )} or \code{list(my.year= , my.subject= , my.extra.label)}. The user-specified values are used to save the student growth percentiles,
  coefficient matrices, knots/boundaries, and goodness of fit results in an orderly fashion using an appropriate combination of year &
  subject & grade. Except in special circumstances, supplying \code{my.year} and \code{my.subject} are sufficient to uniquely label derivative output.
  }
  \item{panel.data.vnames}{Vector of variables to use in student growth percentile calculations. If not specified, function attempts to use all available variables.
  }
  \item{additional.vnames.to.return}{A list of the form list(VARIABLE_NAME_SUPPLIED=VARIABLE_NAME_TO_BE_RETURNED) indicating data to be returned with results
  from \code{\link{studentGrowthPercentiles}} analyses.
  }
  \item{grade.progression}{Preferred argument to specify a student grade/time progression in the data. For example, \code{3:4} would indicate to subset the
  data where the two most recent grades for which data are available are 3 and 4, respectively. The argument allows for non-sequential grade progressions to be analyzed with automatic
  removal of columns where "holes" occur in the supplied grade.progression. For example, for the grade.progression \code{c(7,8,10)}, the penultimate GRADE and SCALE_SCORE column
  in the supplied panel.data would be removed.  The argument can also be combined with an appropriate \code{panel.data.vnames} argument to remove a year of data would analyze students
  progressing from 7 to 8 to 10.
  }
  \item{content_area.progression}{Character vector of content area names of same length as grade.progression to be provided if not all identical to 'my.subject' in sgp.labels list.  Vector will be used to populate the @Content_Areas slot of the splineMatrix class coefficient matrices.  If missing, 'sgp.labels$my.subject' is repeated in a vector length equal to grade.progression.
  }
  \item{year.progression}{Character vector of years associated with grade and content area progressions. If missing then the year.progression is assumed to end in 'my.year' provided in
sgp.labels and be of the same length as grade.progression.  Vector will be used to populate the @Years slot of the splineMatrix class coefficient matrices.
  }
  \item{year_lags.progression}{A numeric vector indicating the time lags/span between observations in the columns supplied to \code{studentGrowthPercentiles}. The default, NULL, allows the function
to calculate the lags/differences based upon the supplied years.
  }
  \item{num.prior}{Number of prior scores one wishes to use in the analysis. Defaults to \code{num.panels-1}.
  If \code{num.prior=1}, then only 1st order growth percentiles are computed, if \code{num.prior=2}, then 1st and 2nd order are computed,
  if \code{num.prior=3}, 1st, 2nd, and 3rd ... NOTE: specifying \code{num.prior} is necessary in some situations (in early grades for example)
  where the number of prior data points is small compared to the number of panels of data.
  }
  \item{max.order.for.percentile}{A positive integer indicating the maximum order for percentiles desired. Similar limiting of number of priors used can be accomplished using the \code{grade.progression} argument.
  }
  \item{return.additional.max.order.sgp}{A positive integer (defaults to NULL) indicating the order of an additional SGP to be returned: \code{SGP_MAX_ORDER_N}.
  }
  \item{subset.grade}{Student grade level for sub-setting. If the data fed into the function contains multiple
  grades, setting \code{subset.grade=5} selects out those students in grade five in the most recent year of the data. If no sub-setting is desired,
  argument do not include the \code{subset.grade} argument. If \code{grade.progression} is supplied, then a subset grade is implicitly specified.
  }
  \item{percentile.cuts}{Additional percentile cuts (supplied as a vector) between 1 and 99 associated with each student's conditional distribution.
   Default is to provide NO growth percentile cuts (scale scores associated with those growth percentiles) for each student.
  }
  \item{growth.levels}{A two letter state acronym or a list of the form \code{list(my.cuts= , my.levels= )} specifying a vector of cuts between 1 and 99 (e.g., 35, 65)
   and the associated qualitative levels associated with the cuts (e.g., low, typical, and high). Note that the length of my.levels should be one more than the
   length of my.cuts. To add your growth levels to the \code{SGPstateData} data set, please contact the package administrator.
  }
  \item{use.my.knots.boundaries}{A list of the form  \code{list(my.year= , my.subject= )} specifying a set of pre-calculated
   knots and boundaries for B-spline calculations. Most often used to utilize knots and boundaries calculated from a previous analysis. Knots and boundaries are stored
   (and must be made available) with  \code{panel.data} supplied as a list in \code{panel.data$Knots_Boundaries$my.subject.my.year}. As of SGP_0.0-6 user can also supply
   a two letter state acronym to utilize knots and boundaries within the \code{SGPstateData} data set supplied with the SGP package. To add your knots and boundaries to the
   \code{SGPstateData} data set, please contact the package administrator. If missing, function automatically calculates knots, boundaries, and loss.hoss values and stores them
   in \code{panel.data$Knots_Boundaries} \cr
   \code{$my.subject.my.year} where \code{my.subject} and \code{my.year} are provided by \code{sgp.labels}.
  }
  \item{use.my.coefficient.matrices}{A list of the form  \code{list(my.year= , my.subject= )} specifying a set of pre-calculated
   coefficient matrices to use for student growth percentile calculations. Can be used to calculate baseline referenced student growth percentiles or to calculate student growth percentiles for small groups of excluded students without recalculating an entire set of data. If missing, coefficient matrices are calculated based upon the provided data and stores them in \cr
 \code{panel.data$Coefficient_Matrices$my.subject.my.year} where \code{my.subject} and \code{my.year} are provided by \code{sgp.labels}.
  }
 \item{calculate.confidence.intervals}{A character vector providing either a state acronym or a variable name from the supplied panel data. If a state acronym, CSEM tables from the embedded
   \code{\link{SGPstateData}} (note: CSEM data must be embedded in the \code{SGPstateData} set. To have your state CSEMs embed in the \code{SGPstateData} set, please contact the package
   administrator) will be used. If a variable name, the supplied panel data must contain a variable providing student level CSEMs (e.g., with adaptive testing). NOTE: If a variable
   name is supplied, the user must also use the argument \code{panel.data.vnames} indicating what variables in the supplied \code{panel.data} will be used for the \code{studentGrowthPercentiles}
   analysis. For greater control, the user can also supply a list of the form \code{list(state= , confidence.quantiles= , simulation.iterations= , distribution= , round= )} or
   \code{list(variable= , confidence.quantiles= , simulation.iterations= , distribution= , round= )} specifying the \code{state} or \code{variable} to use,
   \code{confidence.quantiles} to report from the simulated SGPs calculated for each student, \code{simulation.iterations} indicating the number of simulated SGPs to calculate,
   \code{distribution} indicating whether to the the \code{Normal} or \code{Skew-Normal} to calculate SGPs, and \code{round} (defaults to 1, which is an integer - see \code{round_any}
   from \code{plyr} package for details) giving the level to round to. If requested, simulations are calculated and simulated SGPs are stored in \code{panel.data$Simulated_SGPs}.
  }
  \item{print.other.gp}{Boolean argument (defaults to FALSE) indicating whether growth percentiles of all orders should be returned. The default returns only the highest order growth percentile for each student.
  }
  \item{print.sgp.order}{Boolean argument (defaults to FALSE) indicating whether the order of the growth percentile should be provided in addition to the SGP itself.
  }
  \item{calculate.sgps}{Boolean argument (defaults to TRUE) indicating whether student growth percentiles should be calculated following coefficient matrix calculation.
  }
  \item{rq.method}{Argument defining the estimation method used in the quantile regression calculations. The default is the \code{"br"} method referring to the Barrodale and Robert's L1 estimation detailed in Koenker (2005) and in the help for the quantile regression (\code{quantreg}) package.
  }
  \item{rq.method.for.large.n}{Argument defining the estimation method used in the quantile regression calculations when norm group cohort size exceeds 300,000 students. The default is the \code{"fn"} method referring to the Frisch-Newton estimation detailed in Koenker (2005) and in the help for the quantile regression (\code{quantreg}) package.
  }
  \item{max.n.for.coefficient.matrices}{Argument the defines a size threshold above which a subset of data is taken with a number of cases equal to the sgp.subset.size.threshold argument. Default is NULL,
  no subset is taken.
  }
  \item{knot.cut.percentiles}{Argument that specifies the quantiles to be used for calculation of B-spline knots. Default is to place knots at the 0.2, 0.4, 0.6, and 0.8 quantiles.
  }
  \item{knots.boundaries.by.panel}{Boolean argument (defaults to FALSE) indicating whether knots and boundaries should be calculated by panel in supplied panel data instead of aggregating across panel. If panels are on different scales, then different knots and boundaries may be required to accommodate quantile regression analyses.
  }
  \item{exact.grade.progression.sequence}{Boolean argument indicating whether the grade.progression supplied is used exactly (TRUE) as supplied or whether lower order analyses are run as part of the whole analysis (FALSE--the default).
  }
  \item{drop.nonsequential.grade.progression.variables}{Boolean argument indicating whether to drop variables that do not occur with a non-sequential grade progress. For example, if the grade progression 7, 8, 10 is provided, the penultimate variable in \code{panel.data} is dropped. Default is TRUE.
  }
  \item{convert.0and100}{Boolean argument (defaults to TRUE) indicating whether conversion of growth percentiles of 0 and 100 to growth percentiles of 1 and 99, respectively, occurs. The default produces growth percentiles ranging from 1 to 99.
  }
  \item{sgp.quantiles}{Argument to specify quantiles for quantile regression estimation. Default is Percentiles. User can additionally submit a vector of quantiles (between 0 and 1). Goodness of fit output only available currently for PERCENTILES.
  }
  \item{sgp.quantiles.labels}{Argument to specify integer labels associated with provided 'sgp.quantiles'. Integer labels must a vector of length 1 longer than the length of 'sgp.quantiles'.
  }
  \item{sgp.loss.hoss.adjustment}{Argument to control whether SGP is calculated using which.max for values associated with the hoss embedded in SGPstateData. Providing two letter state acronym utilizes this adjustment whereas supply NULL (the default) uses no adjustment.
  }
  \item{sgp.cohort.size}{Argument to control the minimum cohort size used to calculate SGPs and associated coefficient matrices. NULL (the default) uses no restriction.  If not NULL, argument should be an integer value.
  }
  \item{sgp.less.than.sgp.cohort.size.return}{If non-NULL, indicates whether a data set should be returned with the indicated character string in place of the SGP
	  that would be calculated. If set to TRUE, then character string: \code{< sgp.cohort.size students in cohort. No SGP Calculated}.
  }
  \item{sgp.test.cohort.size}{Integer indicating the maximum number of students sampled from the full cohort to use in the calculation of student growth percentiles.  Intended to be used
  as a test of the desired analyses to be run. The default, NULL, uses no restrictions (no tests are performed, and analyses use the entire cohort of students).
  }
  \item{percuts.digits}{Argument specifying how many digits (defaults to 2) to print percentile cuts (if asked for) with.
  }
  \item{isotonize}{Boolean argument (defaults to TRUE) indicating whether quantile regression results are isotonized to prevent quantile crossing following the
  methods derived by Chernozhukov, Fernandez-Val and Glichon (2010).
  }
  \item{convert.using.loss.hoss}{Boolean argument (defaults to TRUE) indicating whether requested percentile cuts are adjusted using the lowest obtainable scale
   score (LOSS) and highest obtainable scale score (HOSS). Those percentile cuts above the HOSS are replaced with the HOSS and those percentile cuts below the
   LOSS are replaced with the LOSS. The LOSS and HOSS are obtained from the loss and hoss calculated with the knots and boundaries used for spline calculations.
  }
  \item{goodness.of.fit}{Boolean argument (defaults to TRUE) indicating whether to produce goodness of fit results associated with produced student growth percentiles.
   Goodness of fit results are grid.grobs stored in \code{panel.data$Goodness_of_Fit} \cr
   \code{$my.subject.my.year} where \code{my.subject} and \code{my.year} are provided by \code{sgp.labels}.
  }
  \item{goodness.of.fit.minimum.n}{Integer argument (defaults to 250) indicating the minimum number of observations necessary before goodness of fit plots are constructed."
  }
  \item{goodness.of.fit.output.format}{Character argument (defaults to graphical object 'GROB') indicating output format for goodness of fit plots. Options include:
  'GROB', 'PDF', 'PNG', 'SVG'.
  }
  \item{return.prior.scale.score}{Boolean argument (defaults to TRUE) indicating whether to include the prior scale score in the SGP data output. Useful for examining relationship between prior
   achievement and student growth.
  }
  \item{return.prior.scale.score.standardized}{Boolean argument (defaults to TRUE) indicating whether to include the standardized prior scale score in the SGP data output.
Useful for examining relationship between prior achievement and student growth.
  }
  \item{return.norm.group.identifier}{Boolean argument (defaults to TRUE) indicating whether to include the content areas and years that form students' specific norm group in the SGP data output.
  }
  \item{return.norm.group.scale.scores}{Boolean argument (defaults to NULL) indicating whether to return a semi-colon separated character vector of the scores associated with the SGP_NORM_GROUP to
  which the student belongs.
  }
  \item{return.norm.group.dates}{Boolean argument or character string (defaults. to NULL) indicating whether to return a semi-colon separated character vector of the dates associated
  with time dependent SGPt calculations. If TRUE is supplied, 'DATE' is the assumed name for the date variable.
  }
  \item{return.panel.data}{Boolean argument indicating whether to return the original data provided in \code{panel.data$Panel_Data} in the SGP list of results.
  Defaults to 'identical(parent.frame(), .GlobalEnv)': If the parent environment from which the function is called is .GlobalEnv, then FALSE, otherwise TRUE.
  }
  \item{print.time.taken}{Boolean argument (defaults to TRUE) indicating whether to print message indicating information on \code{studentGrowthPercentiles} analysis and time taken.
  }
  \item{parallel.config}{parallel configuration argument allowing for parallel analysis by 'tau'. Defaults to NULL.
  }
  \item{calculate.simex}{A character state acronym or list including state/csem variable, csem.data.vnames, csem.loss.hoss, simulation.iterations, simulation.sample.size, lambda and extrapolation method.
  Returns both SIMEX adjusted SGP (\code{SGP_SIMEX}) as well as the percentile ranked SIMEX SGP (\code{RANK_SIMEX}) values as suggested by Castellano and McCaffrey (2017). Defaults to NULL, no simex calculations performed.
  }
  \item{sgp.percentiles.set.seed}{An integer (or NULL) argument indicating whether to set.seed to make analyses fully reproducible. To turn off, set argument to NULL. Default is 314159.
  }
  \item{sgp.percentiles.equated}{An object containing information (linkages, year, ...) on equating done for calculating student growth percentiles.
  }
  \item{SGPt}{An argument supplied to implement time-dependent SGP analyses (SGPt). Default is NULL giving standard, non-time dependent argument. If set to TRUE, the function assumes the
  variables 'TIME' and 'TIME_LAG' are supplied as part of the panel.data. To specify other names, supply a list of the form: list(TIME='my_time_name', TIME_LAG='my_time_lag_name'), substituting
  your variable names.
  }
  \item{SGPt.max.time}{Boolean argument (defaults to NULL/FALSE) indicating whether cuts/trajectories should be calculated based upon the maximum Time value in the matrices. Such cuts
  are sometimes used to provide within window trajectories.
  }
  \item{verbose.output}{A Boolean argument indicating whether the function should output verbose diagnostic messages.
  }
}


\details{Typical use of the function is to submit a data frame to the function containing records of all students across all grades, allowing the function to subset
out specific grade progressions using \code{grade.progression}. Additional uses include using pre-calculated results to recalculate SGPs for baseline referencing.
\code{\link{studentGrowthPercentiles}} examples provide code for use in analyzing assessment data across multiple grades.
}

\value{Function returns an object of class list containing objects: Coefficient_Matrices, Goodness_of_Fit, Knots_Boundaries, Panel_Data, SGPercentiles, Simulated_SGPs.
}

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

Chernozhukov, V., Fernandez-Val, I. and Galichon, A. (2010),
Quantile and Probability Curves Without Crossing. Econometrica, 78: 1093-1125.

Koenker, R. (2005). \emph{Quantile regression}. Cambridge: Cambridge University Press.

Shang, Y., VanIwaarden, A., & Betebenner, D. W. (2015). Covariate measurement error correction for Student Growth
Percentiles using the SIMEX method. Educational Measurement: Issues and Practice, 34(1):4-14.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\seealso{\code{\link{studentGrowthProjections}}, \code{\link[SGPdata]{sgpData}}, \code{\link[SGPdata]{sgpData_LONG}}, \code{\link{SGPstateData}}}


\examples{
\dontrun{
## Calculate 4th grade student growth percentiles using included sgpData

require(SGPdata)
sgp_g4 <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		percentile.cuts=c(1,35,65,99),
		subset.grade=4,
		num.prior=1)

## NOTE: "grade.progression" can be used in place of "subset.grade" and "num.prior"

sgp_g4_v2 <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		percentile.cuts=c(1,35,65,99),
		grade.progression=c(3,4))

identical(sgp_g4$SGPercentiles, sgp_g4_v2$SGPercentiles)

## Established state Knots and Boundaries are available in the supplied SGPstateData
## file and used by supplying the appropriate two letter state acronym.

sgp_g4_DEMO <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		use.my.knots.boundaries="DEMO",
		grade.progression=c(3,4))


## Sample code for running non-sequential grade progression analysis.

sgp_g8_DEMO <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		use.my.knots.boundaries="DEMO",
		grade.progression=c(5,6,8))


## NOTE: Unless specified with 'goodness.of.fit.output.format'
## Goodness of Fit results are stored as graphical objects in the
## Goodness_of_Fit slot. To view or save (using any R output device) try:
## Load 'grid' package to access grid.draw function

require(grid)
grid.draw(sgp_g4$Goodness_of_Fit$READING.2054[[1]])

require(grid)
pdf(file="Grade_4_Reading_2015_GOF.pdf", width=8.5, height=4.5)
grid.draw(sgp_g4$Goodness_of_Fit$READING.2015[[1]])
dev.off()

# Other grades

sgp_g5 <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		percentile.cuts=c(1,35,65,99),
		grade.progression=3:5)

sgp_g6 <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		percentile.cuts=c(1,35,65,99),
		grade.progression=3:6)

sgp_g7 <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		percentile.cuts=c(1,35,65,99),
		grade.progression=3:7)

sgp_g8 <- studentGrowthPercentiles(
		panel.data=sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		percentile.cuts=c(1,35,65,99),
		grade.progression=4:8)

## All output of studentGrowthPercentiles (e.g., coefficient matrices) is contained
## in the object.  See, for example, names(sgp_g8), for all included objects.
## Results are stored in the slot SGPercentiles.

# Combine all results

sgp_all <- rbind(
		sgp_g4$SGPercentiles$READING.2015,
		sgp_g5$SGPercentiles$READING.2015,
		sgp_g6$SGPercentiles$READING.2015,
		sgp_g7$SGPercentiles$READING.2015,
		sgp_g8$SGPercentiles$READING.2015)

# Save SGP results to .csv file

write.csv(sgp_all, file="sgp_all.csv", row.names=FALSE, quote=FALSE, na="")


## NOTE: studentGrowthPercentiles ADDs results to the current SGP object.
## This allows one to "recycle" the object for multiple grades and subjects as desired.

# Loop to calculate all SGPs for all grades without percentile cuts
# but with growth levels and goodness of fit plots exported automatically as PDFs, PNGs, and SVGs

my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)
my.sgpData <- list(Panel_Data=sgpData)   ### Put sgpData into Panel_Data slot

for (i in seq_along(my.grade.sequences)) {
	my.sgpData <- studentGrowthPercentiles(panel.data=my.sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		growth.levels="DEMO",
		goodness.of.fit="DEMO",
        goodness.of.fit.output.format=c("PDF", "PNG", "SVG"),
		grade.progression=my.grade.sequences[[i]])
}

#  Save Student Growth Percentiles results to a .csv file:

write.csv(my.sgpData$SGPercentiles$READING.2015,
file="2015_Reading_SGPercentiles.csv", row.names=FALSE, quote=FALSE, na="")

## Loop to calculate all SGPs for all grades using 2010 to 2013 data

my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)

for (i in seq_along(my.grade.sequences)) {
	my.sgpData_2009 <- studentGrowthPercentiles(panel.data=my.sgpData,
		panel.data.vnames=c("ID", "GRADE_2010",
			"GRADE_2011", "GRADE_2012", "GRADE_2013",
			"SS_2010", "SS_2011", "SS_2012", "SS_2013"),
		sgp.labels=list(my.year=2013, my.subject="Reading"),
		grade.progression=my.grade.sequences[[i]])
}


## Loop to calculate all SGPs for all grades WITH 80% confidence intervals

my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)

for (i in seq_along(my.grade.sequences)) {
	my.sgpData <- studentGrowthPercentiles(panel.data=my.sgpData,
	sgp.labels=list(my.year=2015, my.subject="Reading"),
	calculate.confidence.intervals=list(state="DEMO",
	confidence.quantiles=c(0.1, 0.9), simulation.iterations=100,
		distribution="Normal", round=1),
	grade.progression=my.grade.sequences[[i]])
}

### Example showing how to use pre-calculated coefficient
### matrices to calculate student growth percentiles

my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)
my.sgpData <- list(Panel_Data=sgpData)   ### Put sgpData into Panel_Data slot

for (i in seq_along(my.grade.sequences)) {
	my.sgpData <- studentGrowthPercentiles(panel.data=my.sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		growth.levels="DEMO",
		grade.progression=my.grade.sequences[[i]])
}

percentiles.1st.run <- my.sgpData$SGPercentiles$READING.2015

### my.sgpData has as full set of coefficient matrices for Reading, 2015. To view these

names(my.sgpData$Coefficient_Matrices$READING.2015)

## Let's NULL out the SGPercentiles slot and recreate the percentiles
## using the embedded coefficient matrices

my.sgpData$SGPercentiles$READING.2015 <- NULL

for (i in seq_along(my.grade.sequences)) {
	my.sgpData <- studentGrowthPercentiles(panel.data=my.sgpData,
		sgp.labels=list(my.year=2015, my.subject="Reading"),
		use.my.knots.boundaries=list(my.year=2015, my.subject="Reading"),
		use.my.coefficient.matrices=list(my.year=2015, my.subject="Reading"),
		growth.levels="DEMO",
		grade.progression=my.grade.sequences[[i]])
}

percentiles.2nd.run <- my.sgpData$SGPercentiles$READING.2015

identical(percentiles.1st.run, percentiles.2nd.run)

}
}

\keyword{misc}
\keyword{models}
