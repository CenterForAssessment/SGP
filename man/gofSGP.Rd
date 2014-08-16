\name{gofSGP}
\alias{gofSGP}
\title{Function for producing goodness of fit plots using existing SGP object}
\description{
gofSGP creates goodness-of-fit plots in either PDF or PNG for showing SGP distribution by prior achievement level and prior scale score decile. These plots expand upon the plots currently produced
with the \code{\link{studentGrowthPercentiles}} function.
}

\usage{
gofSGP(
	sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	content_areas_prior=NULL,
	grades=NULL,
	use.sgp="SGP",
	output.format="PDF",
	color.scale="reds.and.blues")
}

\arguments{
  \item{sgp_object}{The SGP object from which the goodness-of-fit data will be used. 
  }
  \item{state}{The 'state' for the sgp_object. Derive from sgp_object name if not explicitly supplied. 
  }
  \item{years}{The years that goodness-of-fit plots are requested. Default is to use all years available.
  }
  \item{content_areas}{The content area(s) that goodness-of-fit plots are requested. Default is to use all content areas available.
  }
  \item{content_areas_prior}{The content area(s) of the prior year which growth by achievement level is being produced that goodness-of-fit plots are requested. 
  Default is to use all content areas available.
  }
  \item{grades}{The grade(s) that goodness-of-fit plots are requested. Default is to use all grade available.
  }
  \item{use.sgp}{Character vectors (defaults to 'SGP') indicating what student growth percentile variable to calculate goodness-of-fit plots for. 
  }
  \item{output.format}{Character vectors (defaults to 'PDF') indicating what driver to use to output results. Options currently include 'PDF', 'PNG' and 'SVG'.  'SVG' is currently experimental.
  }
  \item{color.scale}{Character vectors (defaults to 'red') indicating what color palette to use for creating percentile distribution table.
  }
}

\value{Returns output ('PDF' , 'PNG' or 'SVG') associated with goodness-of-fit analyses.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{studentGrowthPercentiles}}}

\examples{
\dontrun{
Demonstration_SGP <- abcSGP(sgpData_LONG)
gofSGP(Demonstration_SGP)
}  
}

\keyword{documentation}
