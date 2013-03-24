\name{updateSGP}
\alias{updateSGP}
\title{Function to update SGP object with additional year's analyses}
\description{
updateSGP takes an object of class SGP and adds in additional data (usually an additional year's data) and runs analyses on that additional year's data including the results in the supplied
SGP object.
}

\usage{
updateSGP(
	what_sgp_object=NULL,
	with_sgp_data_LONG=NULL,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	save.old.summaries=TRUE,
	save.intermediate.results=FALSE,
	...)
}

\arguments{
  \item{what_sgp_object}{The SGP object to which the additional data will be added and analyzed. This object must be specified.
  }
  \item{with_sgp_data_LONG}{The additional data in LONG for to be added to the supplied object of class SGP analyzed. The additional data must be in the same form as the data in the @Data slot. If with_sgp_data_LONG is not supplied, the function with update the sgp_object supplied in 'what_sgp_object' using the embedded coefficient matrices, essentially re-doing the analyses.
  }
  \item{state}{The 'state' for the sgp_object. Derived from sgp_object name if not explicitly supplied. 
  }
  \item{years}{If only 'what_sgp_object' is supplied, years specifies the years to be run amongst those in the provided sgp_object.
  }
  \item{content_areas}{If only 'what_sgp_object' is supplied, content_areas specifies the content areas to be run amonst those provided by the coefficient matrices in the sgp_object. Default is to run all analyses associated with the coefficient matrices.
  }
  \item{save.old.summaries}{A boolean argument (defaults to FALSE which will delete the @Summary slot before creating new summaries)
  indicating whether the call to \code{\link{summarizeSGP}} should save existing summaries in the @Summary slot.
  }
  \item{save.intermediate.results}{A boolean argument (defaults to FALSE) indicating whether results should be save to the current directory after each step of the analysis.
  }
  \item{...}{Arguments to be passed to \code{\link{abcSGP}} for finer control over SGP calculations. For example, 'parallel.config' can be supplied to abcSGP for parallel computation.
  }

}

\value{Returns and object of class SGP including additional analyses.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}} and \code{\link{abcSGP}}}

\examples{
\dontrun{
### Run analyses on all but final year's of data

Demonstration_Data_LONG <- subset(sgpData_LONG, YEAR %in% c("2008_2009", "2009_2010", "2010_2011", "2011_2012"))
Demonstration_Data_LONG_2012_2013 <- subset(sgpData_LONG, YEAR %in% c("2012_2013"))

Demonstration_SGP <- abcSGP(
        sgp_object=Demonstration_Data_LONG,
        sgPlot.demo.report=TRUE)


### Run updateSGP on Demonstration_SGP and the 2012_2013 data

Demonstration_SGP <- updateSGP(
        what_sgp_object=Demonstration_SGP,
        with_sgp_data_LONG=Demonstration_Data_LONG_2012_2013)
}  
}

\keyword{documentation}
