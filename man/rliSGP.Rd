\name{rliSGP}
\alias{rliSGP}
\title{Wrapper function associated with SGP analyses for Renaissance Learning Incorporated (RLI) interim STAR assessments.}
\description{
rliSGP is a wrapper function design to expedite SGP analyses on Renaissance Learning Incorporated (RLI) interim STAR assessments.
}

\usage{
rliSGP(
	sgp_object,
  additional.data=NULL,
	state=NULL,
	content_areas=c("MATHEMATICS", "MATHEMATICS_SPANISH", "READING", 
			"READING_SPANISH", "READING_UNIFIED", "EARLY_LITERACY", 
			"EARLY_LITERACY_SPANISH"),
  testing.window,
  eow.or.update="UPDATE",
	update.save.shell.only=FALSE,
  configuration.year,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=FALSE,
	sgp.target.scale.scores=TRUE,
  update.ids=NULL,
	SGPt=TRUE,
	simulate.sgps=FALSE,
	save.intermediate.results=FALSE,
	coefficient.matrices=NULL,
	goodness.of.fit.print=FALSE,
	return.updated.shell=FALSE,
	fix.duplicates="KEEP.ALL",
	eow.calculate.sgps=FALSE,
	score.type="RASCH",
	cutscore.file.name="Cutscores.csv",
	get.cohort.data.info=FALSE,
  use.latest.rliMatrices=TRUE,
  parallel.config=NULL)
}

\arguments{
  \item{sgp_object}{An object of class SGP or a \code{data.frame} from which an SGP object can be produced. If data.frame is supplied, the current
  window's data will be created from the last window present in the data.frame.
  }
  \item{additional.data}{Current window's data for UPDATE or EOW (end of window) processing. Can be NULL if new data all fed into argument
  'sgp_object' as data.frame.
  }
  \item{state}{Abbreviation for STATE/ORGANIZATION being analyzed. Currently only available for 'RLI' or 'RLI_UK'.
  }
  \item{content_areas}{Character vector indicating which content areas \code{rliSGP} should be run for ('MATHEMATICS', 'READING', 'READING_UNIFIED', and/or 'EARLY_LITERACY'). Default is all.
  }
  \item{testing.window}{Argument indicating testing window being analyzed. Either 'FALL', 'WINTER', 'SPRING' indicating what testing window's data is being analyzed.
  }
  \item{eow.or.update}{Argument indicating whether end-of-window or within-window updates are being done. Either 'EOW' (end of window) or 'UPDATE' (within window).
  }
  \item{update.save.shell.only}{Boolean argument indicating whether End of Window processing should only produce/save RLI_SGP_UPDATE_SHELL and bypass coefficient matrix production.
  }
  \item{configuration.year}{Year associated with SGP analyses (configuration year) to be used.
  }
  \item{sgp.percentiles.baseline}{Boolean variable passed to \code{\link{updateSGP}} to control whether baseline student growth percentiles are calculated. Default is TRUE.
  }
  \item{sgp.projections.baseline}{Boolean variable passed to \code{\link{updateSGP}} to control whether baseline student growth projections are calculated. Default is TRUE.
  }
  \item{sgp.projections.lagged.baseline}{Boolean variable passed to \code{\link{updateSGP}} to control whether lagged baseline student growth projections are calculated. Default is FALSE.
  }
  \item{sgp.target.scale.scores}{Boolean variable passed to \code{\link{updateSGP}} to control whether scale score targets are calculated. Default is TRUE.
  }
  \item{update.ids}{Argument that allows ID to be updated based upon a supplied two column data frame where the first column represents the ORIGINAL/OLD ID and the second column the NEW ID that
is to replace it. NOTE that ID needn't be replaced in either 'sgp_object' nor 'additional.data' that are provided as part of the 'rliSGP' call. Substitution will occur on entire data set before
analysis. Default is NULL, no ID replacement is performed.
  }
  \item{SGPt}{Argument indicating whether to perform time dependent SGP (SGPt) analyses. Current default is TRUE based upon transition to SGPt in Summer 2015.
  }
  \item{simulate.sgps}{A Boolean argument indicating whether to calculate SGP standard errors based upon CSEMs provided by either an embedded CSEM-scale score lookup or an embedded variable.
  Default is FALSE.
  }
  \item{save.intermediate.results}{Boolean variable passed to \code{\link{updateSGP}} indicating whether to save intermediate results from \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, and
  \code{\link{combineSGP}} as part of the \code{rliSGP} analyses.
  }
  \item{coefficient.matrices}{List of baseline matrices to be used for analyses (overrides what exists in \code{SGPstateData}). Defaults to NULL (use those in \code{SGPstateData}).
  }
  \item{goodness.of.fit.print}{Boolean argument passed to \code{\link{updateSGP}} indicating whether to print goodness of fit plots associated with outcome. Default is FALSE.
  }
  \item{return.updated.shell}{Boolean argument (defaults to FALSE) indicating whether to return \code{sgp_object} as part of call to \code{rliSGP}.
  }
  \item{fix.duplicates}{Character argument (defaults to 'KEEP.ALL') indicating how to deal with duplicates submitted as part of data for analysis. Setting 'fix.duplicates' to null
  sets no modification of duplicates.
  }
  \item{eow.calculate.sgps}{Boolean argument passed to \code{\link{updateSGP}} further passed to \code{\link{analyzeSGP}} indicating whether student
growth percentiles are calculated as part of EOW analyses. Default is FALSE which results in just coefficient matrices being calculated.
  }
  \item{score.type}{Either \code{RASCH} (default) or \code{STAR} indicating what type of score is being supplied for analysis. The selection ensures the correct cutscores,
  	matrices, and knots/boundaries are used for analyses.
  }
  \item{cutscore.file.name}{Cutscore file name and path for input and creation of RLI Cutscores. Default is \code{Cutscores.csv}.
  }
  \item{get.cohort.data.info}{Boolean argument passed to \code{analyzeSGP} indicating whether to calculate norm group cohort counts.
  }
  \item{use.latest.rliMatrices}{Boolean argument indicating whether to ensure use of latest version of RLImatrices (defaults to TRUE).
  }
  \item{parallel.config}{Argument passed to \code{\link{abcSGP}} or \code{\link{updateSGP}} to activate parallel processing with an appropriate backend.
  }
}

\value{Saves relevant results to disc for use in subsequent analyses.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
\dontrun{
####### For Within Window processing

testSGP('RLI')

####### For End-of-Window processing


}
}

\keyword{documentation}
