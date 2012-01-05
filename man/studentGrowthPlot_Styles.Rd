\name{studentGrowthPlot_Styles}
\alias{studentGrowthPlot_Styles}
\title{studentGrowth_Styles providing base templates for the core studentGrowthPlot function}
\description{
Function includes two "styles" associated with \code{\link{studentGrowthPlot}} to create PDF student growth plots depicting the growth and achievement for an individual
student over time. One styles displayed two content area student growth plots on a single page and the other three content areas. These charts are currently being used in 
multiple states to report student growth results. This function is called by \code{\link{visualizeSGP}} to generate individual student reports. This function may expand 
in the future to incorporate other possible individual student reports.
}
\usage{
studentGrowthPlot_Styles(
           sgPlot.data,
           state,
           last.year,
           content_areas,
           districts,
           schools,
           reports.by.student,
           sgPlot.years,
           sgPlot.demo.report,
           sgPlot.folder,
           sgPlot.folder.names,
           sgPlot.anonymize,
           sgPlot.front.page,
           sgPlot.header.footer.color,
           sgPlot.fan,
           sgPlot.cleanup,
           sgPlot.baseline)
}

\arguments{
  \item{sgPlot.data}{Wide formatted individual student report data used to produce student growth plots. To view structure of wide formatted data, utilize the 
\code{sgPlot.save.sgPlot.data} option with \code{\link{visualizeSGP}} to save wide formatted student growth plot data.
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{SGPstateData}.
  }
  \item{last.year}{Argument indicating the final year represented in the student growth plots.
  }
  \item{content_areas}{Argument providing the content areas depicted in the student growth plots.
  }
  \item{districts}{A vector of district numbers indicating which districts student growth plots should be produced for.
  }
  \item{schools}{A vector of school numbers indicating which schools student growth plots should be produce for.
  }
  \item{reports.by.student}{A boolean variables passed to \code{studentGrowthPlot_Styles} indicating whether separate individual plots will be produced or separate reports
and a summary catalog containing those reports will be produced. 
  }
  \item{sgPlot.years}{A vector of all years over which student growth plots are being produced.  
  }
  \item{sgPlot.demo.report}{A boolean argument indicating whether a demonstration report catalog (with anonymized individual, school, and district names) is to be produced. 
  }
  \item{sgPlot.folder}{A character argument specifying the folder into which the student growth reports will be placed. 
  }
  \item{sgPlot.folder.names}{Either \code{names} or \code{number} indicating whether names or numbers should be used as folder names.
  }
  \item{sgPlot.anonymize}{A boolean argument indicating whether individual, school, and district names should be anonymized.
  }
  \item{sgPlot.front.page}{A character vector indicating the file, the the base directory, that should be used as the front page for the student growth plots. 
  }
  \item{sgPlot.header.footer.color}{A color (as a character) specifying the header/footer color of the report.
  }
  \item{sgPlot.fan}{A boolean argument indicating whether the projection fan indicating growth rates necessary to reach 1 years targets be displayed.
  }
  \item{sgPlot.cleanup}{A boolean argument indicating whether to cleanup/remove files produced as part of pdfLaTeX build.
  }
  \item{sgPlot.baseline}{A boolean argument indicating whether to use baseline referenced student growth percentiles in student growth plots.
  }
}

\details{\code{studentGrowthPlot_Styles} is a utility function containing a two templates for displaying \code{\link{studentGrowthPlot}}s. The templates display two or three
content areas depending upon the availability of test data for the state. This function will expand in the future to allow for other types of individual growth and
achievement reports.
}
\value{Function produces \emph{numerous} (potentially hundreds of thousands of) PDF student growth plots. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\keyword{misc}
\keyword{models}
