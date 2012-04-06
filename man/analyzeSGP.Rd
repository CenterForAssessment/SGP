\name{analyzeSGP}
\alias{analyzeSGP}
\title{Analyze student data to produce student growth percentiles and student growth projections}
\description{
  Utility function/exemplar used to produce student growth percentiles and student growth projections using long formatted data like that provided by \code{\link{prepareSGP}}.
}

\usage{
analyzeSGP(sgp_object,
         state=NULL,
         years=NULL,
         content_areas=NULL,
         grades=NULL,
         sgp.percentiles=TRUE,
         sgp.projections=TRUE,
         sgp.projections.lagged=TRUE,
         sgp.percentiles.baseline=TRUE,
         sgp.projections.baseline=TRUE,
         sgp.projections.lagged.baseline=TRUE,
         sgp.percentiles.baseline.max.order=3,
         sgp.projections.baseline.max.order=3,
         sgp.projections.lagged.baseline.max.order=3,
         simulate.sgps=TRUE,
         goodness.of.fit.print=TRUE,
         sgp.config=NULL,
         sgp.config.drop.nonsequential.grade.progression.variables=TRUE,
         sgp.baseline.config=NULL,
         parallel.config=NULL,
         ...)
}

\arguments{
  \item{sgp_object}{An object of class \code{SGP} containing long formatted data in the \code{code} (from \code{\link{prepareSGP}}) slot. 
  }
  \item{state}{Acronym indicating state associated with the data for access to embedded knot and boundaries, cutscores, CSEMs, and other state related assessment data.
  }
  \item{years}{A vector indicating year(s) in which to produce student growth percentiles and/or student growth projections/trajectories. If missing the 
function will use the data to infer the year(s) based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content area(s) in which to produce student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer the content area(s) available for analyses.
  }
  \item{grades}{A vector indicating grades for which to calculate student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer all the grade progressions for student growth percentile and student growth projections/trajectories analyses.
  }
  \item{sgp.percentiles}{Boolean variable indicating whether to calculate student growth percentiles. Defaults to TRUE.
  }
  \item{sgp.projections}{Boolean variable indicating whether to calculate student growth projections. Defaults to TRUE.
  }
  \item{sgp.projections.lagged}{Boolean variable indicating whether to calculate lagged student growth projections often used for growth to standard analyses. Defaults to TRUE.
  }
  \item{sgp.percentiles.baseline}{Boolean variable indicating whether to calculate baseline student growth percentiles and/or coefficient matrices. Defaults to FALSE.
  }
  \item{sgp.projections.baseline}{Boolean variable indicating whether to calculate baseline student growth projections. Defaults to FALSE.
  }
  \item{sgp.projections.lagged.baseline}{Boolean variable indicating whether to calculate lagged baseline student growth projections. Defaults to FALSE.
  }
  \item{sgp.percentiles.baseline.max.order}{Integer indicating the maximum order to calculate baseline student growth percentiles (regardless of maximum coefficient matrix order). Default is 3.
To utilize the maximum matrix order, set to NULL.
  }
  \item{sgp.projections.baseline.max.order}{Integer indicating the maximum order to calculate baseline student growth projections (regardless of maximum coefficient matrix order). Default is 3.
To utilize the maximum matrix order, set to NULL.
  }
  \item{sgp.projections.lagged.baseline.max.order}{Integer indicating the maximum order to calculate lagged baseline student growth projections (regardless of maximum coefficient matrix order). 
Default is 3.  To utilize the maximum matrix order, set to NULL.
  }
  \item{simulate.sgps}{Boolean variable indicating whether to simulate SGP values for students based on test-specific Conditional Standard Errors of Measurement (CSEM).  Test CSEM data must be available for simulation and included in \code{SGPstateData}.  This argument must be set to TRUE for confidence interval construction. Defaults to TRUE.
  }
  \item{goodness.of.fit.print}{Boolean variable indicating whether to print out Goodness of Fit figures as pdf into a directory labeled Goodness of Fit. Defaults to TRUE.
  }
  \item{sgp.config}{If \code{years}, \code{content_areas}, and \code{grades} are missing, user can directly specify a list containing three vectors: \code{baseline.content.areas}, \code{baseline.panel.years}, and \code{baseline.grade.sequences}. This advanced option is helpful for analysis of non-traditional grade progressions and other special cases. See examples for use cases.
  }
  \item{sgp.config.drop.nonsequential.grade.progression.variables}{Boolean variable (defaults to TRUE) indicating whether non-sequential grade progression variables should be dropped when sgp.config is processed. For example, if a grade progression of c(3,4,6) is provided, the data configuration will assume (default is TRUE) that data for a missing year needs to be dropped prior to applying \code{\link{studentGrowthPercentiles}} or \code{\link{studentGrowthProjections}} to the data. 
  }
  \item{sgp.baseline.config}{A list containing three vectors: \code{sgp.content.areas}, \code{sgp.panel.years}, \code{sgp.grade.sequences} indicating how baseline student growth percentile analyses
 are to be conducted. In almost all cases this value is calculated by default within the function but can be specified directly for advanced use cases. See source code for more detail on this configuration option.
  }
\item{parallel.config}{A named list with, at a minimum, two elements indicating 1) the BACKEND package to be used for parallel computation and 2) the WORKERS list to specify the number of processors to be used in each major analysis.  The BACKEND element can be set = to \code{FOREACH}, \code{SNOW}, \code{MULTICORE}, or \code{PARALLEL}.  Please consult the manuals and vignettes for information of these packages!

TYPE is a third element of the \code{parallel.config} list that provides necessary information when using FOREACH, SNOW or PARALLEL packages as the backend. With BACKEND="FOREACH", the TYPE element specifies the "doMC", "doMPI", "doSNOW", "doRedis" or "doParallel" flavor of foreach backends.   If BACKEND = "SNOW", the TYPE element specifies the users preferred cluster type (either "SOCK" for socket cluster of "MPI" for an OpenMPI cluster).  The function will create a cluster object based on these parameters and the number of workers requested (see WORKERS list description below).  Alternatively, the name of an external CLUSTER.OBJECT set up by the user outside of the function can be used.   If BACKEND = "PARALLEL", the \code{parallel} package will be used, and the TYPE element specifies the users preferred cluster type if a SNOW type cluster is used.  If Windows is the operating system, this element must = "SOCK".  Defaults are assigned based on operating system if TYPE is missing based on system OS.  Unix/Mac OS defaults to MULTICORE to avoid presheduling.  SNOW can be chosen by specifying "SOCK" or "SNOW" in the TYPE element.

The WORKERS list must contain, at a minimum, a single number of processors (nodes) desired or available.  If WORKERS is specified in this manner, then the same number of processors will be used for each analysis type (sgp.percentiles, sgp.projections, ...  sgp.projections.lagged.baseline).  Alternatively, the user may specify the numbers of processors used for each analysis.  This allows for better memory management in systems that do not have enough RAM available per core.  The choice of the number of cores is a balance between the number of processors available, the amount of RAM a system has and the size of the data (sgp_object).  Each system will be different and will require some tailoring.  One rule of thumb used by the authors is to allow for 4GB of memory per core used for running large state data.  The SGP Demonstration (and data that size) requires more like 1-2GB per core.  As an example, PERCENTILES=4 and  PROJECTIONS=2 might be used on a quad core machine with 4 GB of RAM.  This will use all 4 cores available for the sgp.percentiles analysis and 2 cores for the sgp.projections analysis (which requires more memory than available).  The WORKERS list accepts these elements:   PERCENTILES, PROJECTIONS (for both cohort and baseline referenced projections), LAGGED_PROJECTIONS (for both cohort and baseline referenced lagged projections), BASELINE_MATRICES (used to produce the baseline coefficient matrices when not available in SGPstateData - very computationally intensive), BASELINE_PERCENTILES (SGP calculation only when baseline coefficient matrices have already been produced and are available - NOT very computationally intensive).

Example use cases are provided below.
  }
\item{...}{Arguments to be passed to \code{\link{studentGrowthPercentiles}} or \code{\link{studentGrowthProjections}} for finer control over SGP calculations. NOTE: arguments can only be passed to one lower level function at a time, and only student growth percentiles OR projections can be created but not both at the same time.
  }
}

\value{Function returns a list containing the long data set in the \code{Data} slot as a \code{data.table} keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} and the student growth percentile and/or student growth projection/trajectory results in the \code{SGP} slot. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{vaniwaarden@colorado.edu}}

\seealso{\code{\link{prepareSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
## analyzeSGP is Step 2 of 5 of abcSGP
Demonstration_SGP <- sgpData_LONG
Demonstration_SGP <- prepareSGP(Demonstration_SGP)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP)

## Or (explicitly pass state argument)

Demonstration_SGP <- prepareSGP(sgpData_LONG)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP, state="DEMO")

###
###  Example uses of the sgp.config argument
###

#  Use only 3 years of Data, for grades 3 to 6
#  and only perform analyses for most recent year (2011)

my.custom.config <- list(
MATHEMATICS.2010_2011 = list(
	sgp.content.areas=rep("MATHEMATICS", 3), # Note, must be same length as sgp.panel.years
	sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011'),
	sgp.grade.sequences=list(3:4, 3:5, 4:6)),
READING.2010_2011 = list(
	sgp.content.areas=rep("READING", 3),
	sgp.panel.years=c('2008_2009', '2009_2010', '2010_2011'),
	sgp.grade.sequences=list(3:4, 3:5, 4:6)))

Demonstration_SGP <- prepareSGP(sgpData_LONG)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP,
	sgp.config=my.custom.config,
	sgp.percentiles.baseline = FALSE,
	sgp.projections.baseline = FALSE,
	sgp.projections.lagged.baseline = FALSE,
	simulate.sgps=FALSE)


##  Another example sgp.config list:

#  Use different CONTENT_AREA priors, and only 1 year of prior data
my.custom.config <- list(
MATHEMATICS.2010_2011.READ_PRIOR = list(
	sgp.content.areas=c("READING", "MATHEMATICS"),
	sgp.panel.years=c('2009_2010', '2010_2011'),
	sgp.grade.sequences=list(3:4, 4:5, 5:6)),
READING.2010_2011.MATH_PRIOR = list(
	sgp.content.areas=c("MATHEMATICS", "READING"),
	sgp.panel.years=c('2009_2010', '2010_2011'),
	sgp.grade.sequences=list(3:4, 4:5, 5:6)))


###
###  Example uses of the parallel.config argument
###

##  Windows users must use snow:
#  possibly a quad core machine with low RAM Memory
#  4 workers for percentiles, 2 workers for projections.
#  Note the SOCK type cluster is used for single machines.

Demonstration_SGP <- prepareSGP(sgpData_LONG)
Demonstration_SGP <- analyzeSGP(Demonstration_SGP,
	parallel.config=list(
		BACKEND="SNOW", TYPE="SOCK",
		WORKERS=list(PERCENTILES=4, 
		    PROJECTIONS=2, 
		    LAGGED_PROJECTIONS=2,
		    BASELINE_PERCENTILES=4))

##  Windows users with R version >= 2.14.0  may prefer the parallel package:
#  This example is would be good for a single workstation with 8 cores
#  and enough RAM to use 8 workers for ALL analyses.
	...
	parallel.config=list(
		BACKEND="PARALLEL", TYPE="SOCK"),
		WORKERS=8)
	...

#  A similar specification for R versions pre 2.13 using SNOW:
	...
	parallel.config=list(
		BACKEND="SNOW", TYPE="SOCK"),
		WORKERS=8)
	...

##  Linux/Mac may use the multicore package directly:
	...
	parallel.config=list(
		BACKEND="MULTICORE",
		WORKERS=4)
	...

## FOREACH use cases:
# doMC - only available on Linux or Mac OSX
	...
	parallel.config=list(
		BACKEND="FOREACH", TYPE="doMC",
		WORKERS=3)
	...

#  doMPI package -  the number of workers is 100, 
#  suggesting this example is for a HPC cluster usage.
	...
	parallel.config=list(
		BACKEND="FOREACH", TYPE="doMPI",
		WORKERS=100)
	...


##  New parallel package - only available with R 2.13 or newer
#  Note there are up to 16 workers, and MPI is used, 
#  suggesting this example is for a HPC cluster, possibly Windows OS.
	...
	parallel.config=list(
		BACKEND="PARALLEL", TYPE="MPI",
		WORKERS=list(PERCENTILES=16, 
		    PROJECTIONS=8, 
		    LAGGED_PROJECTIONS=6,
		    BASELINE_PERCENTILES=12))
	...

#  NOTE:  This list of parallel.config specifications is NOT exhaustive.  
#  See examples in analyzeSGP documentation for some others.0

###
###  Advanced Example: restrict years, recalculate baseline SGP
###  coefficient matrices, and use parallel processing
###

#  Remove existing DEMO baseline coefficient matrices from
#  the SGPstateData object so that new ones will be computed.

SGPstateData$DEMO$Baseline_splineMatrix <- NULL

#  set up a customized sgp.config list

	. . .

#  set up a customized sgp.baseline.config list

	. . .

#  to be completed

}  
}

\keyword{documentation}
