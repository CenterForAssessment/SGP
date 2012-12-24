\name{updateSGP}
\alias{updateSGP}
\title{Function to update SGP object with additional year's analyses}
\description{
updateSGP takes an object of class SGP and adds in additional data (usually an additional year's data) and runs analyses on that additional year's data including the results in the supplied
SGP object.
}

\usage{
updateSGP(
	what_sgp_object,
	with_sgp_data_LONG,
	state=NULL,
	save.old.summaries=TRUE,
	...)
}

\arguments{
  \item{what_sgp_object}{The SGP object to which the additional data will be added and analyzed. 
  }
  \item{with_sgp_data_LONG}{The additional data in LONG for to be added to the supplied object of class SGP analyzed. The additional data must be in the same form as the data in the @Data slot. 
  }
  \item{state}{The 'state' for the sgp_object. Derived from sgp_object name if not explicitly supplied. 
  }
  \item{save.old.summaries}{A boolean argument (defaults to FALSE which will delete the @Summary slot before creating new summaries)
  indicating whether the call to \code{\link{summarizeSGP}} should save existing summaries in the @Summary slot.
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

Demonstration_Data_LONG <- subset(sgpData_LONG, YEAR %in% c("2007_2008", "2008_2009", "2009_2010", "2010_2011"))
Demonstration_Data_LONG_2011_2012 <- subset(sgpData_LONG, YEAR %in% c("2011_2012"))

Demonstration_SGP <- abcSGP(
        sgp_object=Demonstration_Data_LONG,
        sgPlot.demo.report=TRUE)


### Run updateSGP on Demonstration_SGP and the 2011_2012 data

Demonstration_SGP <- updateSGP(
        what_sgp_object=Demonstration_SGP,
        with_sgp_data_LONG=Demonstration_Data_LONG_2011_2012)
}  
}

\keyword{documentation}
