\name{rliSGP}
\alias{rliSGP}
\title{Wrapper function associated with SGP analyses for Renaissance Learning Incorporated (RLI) interim STAR assessments.}
\description{
rliSGP is a wrapper function design to expediate SGP analyses on Renaissance Learning Incorporated (RLI) interim STAR assessments.
}

\usage{
rliSGP(
	sgp_object,
        additional.data,
        testing.window,
        eow.or.update="UPDATE",
	update.save.shell.only=FALSE,
        configuration.year,
        parallel.config=NULL)
}

\arguments{
  \item{sgp_object}{The SGP object from which the goodness-of-fit data will be used. 
  }
  \item{additional.data}{Current window's data for UPDATE or EOW (end of window) processing. 
  }
  \item{testing.window}{Argument indicating testing window being analyzed. Either 'FALL', 'WINTER', 'SPRING', 'EARLY_SPRING', 'LATE_SPRING' indicating what testing window's data is being analyzed.
  }
  \item{eow.or.update}{Argument indicating whether end-of-window or within-window updates are being done. Either 'EOW' (end of window) or 'UPDATE' (within window). 
  }
  \item{update.save.shell.only}{Boolean argument indicating whether End of Window processing should only produce/save RLI_SGP_UPDATE_SHELL and bypass coefficient matrix production.
  }
  \item{configuration.year}{Year associated with SGP analyses (configuration year) to be used.
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



####### For End-of-Window processing 


}  
}

\keyword{documentation}
