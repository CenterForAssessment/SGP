\name{bubblePlot_Styles}
\alias{bubblePlot_Styles}
\title{bubblePlot_Styles providing various uses of the core bubblePlot function}
\description{
Function includes a number of "styles" associated with \code{\link{bubblePlot}} to create bubble plots depicting a variety of relationships often of interest
to stakeholders. The \code{\link{bubblePlot}} function itself is adaptable to many representations but is most often used in conjunction with results derived
from \code{\link{summarizeSGP}} to represent summary level results of growth against achievement (usually, median student growth percentile against percentage
at/above proficient).
}
\usage{
bubblePlot_Styles(
                sgp_object,
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
		bPlot.folder="Visualizations/bubblePlots")
}

\arguments{
  \item{sgp_object}{An object of class SGP containing long formatted data in the \code{@Data} slot that will be used for the production of student growth
and achievement plots and system growth and achievement plots, summary data from \code{\link{summarizeSGP}} in the \code{@Summary} slot for bubble plots.
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{SGPstateData}.
  }
  \item{bPlot.years}{A vector indicating year(s) in which to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
function will use the last year available in the data to produce \code{\link{bubblePlot}}s.
  }
  \item{bPlot.content_areas}{A vector indicating content area(s) to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
function will produce plots for all available content areas provided in the data.
  }
  \item{bPlot.districts}{A vector indicating districts to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
function will produce plots for all available districts provided in the data where districts represent a relevant unit to be represented by the specific \code{bubblePlot} style.
  }
  \item{bPlot.schools}{A vector indicating schools to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
function will produce plots for all available schools provided in the data where districts represent a relevant unit to be represented by the specific \code{bubblePlot} style.
  }
  \item{bPlot.instructors}{A vector indicating instructors to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the
function will produce plots for all available instructors provided in the data where schools and districts represent relevant units to be represented by the specific \code{bubblePlot} style.
  }
  \item{bPlot.styles}{A vector of positive integers indicating the types of \code{\link{bubblePlot}}s to produce using data available in \code{sgp_object}. See
associated documentation for example plots.
  }
  \item{bPlot.levels}{A character vector of levels to be used to color bubbles in the \code{\link{bubblePlot}}. See  associated documentation for example plots.
  }
  \item{bPlot.level.cuts}{A vector of cuts to be used to distinguish levels used to color bubbles in the \code{\link{bubblePlot}}. See associated documentation for example plots.
  }
  \item{bPlot.full.academic.year}{A Boolean argument (defaults to TRUE) indicating whether \code{\link{bubblePlot}}s should use full academic year results if available.
  }
  \item{bPlot.minimum.n}{A positive integer (defaults to 10) indicating the minimum size for summary values to be displayed in the \code{\link{bubblePlot}}s.
  }
  \item{bPlot.anonymize}{A Boolean argument (defaults to FALSE) indicating whether to anonymize \code{\link{bubblePlot}}s school and district names that appear in the
plots and data tips of the plots.  For student level anonymization, the function utilizes the randomNames package to produce gender and ethnic correct names based
upon gender and ethnicity codes available in \code{@Data}.
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
}

\details{\code{bubblePlot_Styles} is an omnibus function containing a number of applications of the \code{bubblePlot} function with data output from \code{SGP} analyses. As added styles are
constructed they will be added to the function allowing user to construct plots of their liking. User wishing to participate and provide or suggest specific styles of their own should
contact the package maintainer. Styles representing summary level data (e.g., growth and achievement for schools) are currently assigned numbers from 1 to 99 and styles representing
individual level data (e.g., growth and achievement for students within a single grade) are currently assigned numbers from 100 to 199.
\tabular{ll}{
\code{1}: \tab Growth and achievement \code{\link{bubblePlot}}s for the entire state with bubbles representing schools.\cr
\code{100}: \tab Growth and achievement \code{\link{bubblePlot}}s by grade for students in selected schools and districts.\cr
}
}
\value{Function produces \emph{numerous} PDF bubble charts in the styles specified by the function.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{avaniwaarden@nciea.org}}

\keyword{misc}
\keyword{models}
