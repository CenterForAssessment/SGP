\name{growthAchievementPlot}
\alias{growthAchievementPlot}
\title{growthAchievementPlot for SGP}
\description{
Function to create growth and achievement plots depicting system level results associated with student growth percentile results. The charts show, simultaneously,
norm- and criterion-referenced student achievement (i.e., status) as well as norm- and criterion-referenced student growth. These charts are those shown on the
cover of the December 2009 Issue of \emph{Educational Measurement: Issues and Practice}. See Betebenner (2009) and Betebenner (2012) for more details
}
\usage{
growthAchievementPlot(
    gaPlot.sgp_object,
    gaPlot.students,
    gaPlot.percentile_trajectories,
    gaPlot.achievement_percentiles=c(.01, seq(.05, .95, by=.05), .99),
    gaPlot.show.scale.transformations=TRUE,
    gaPlot.grade_range,
    gaPlot.max.order.for.progression=NULL,
	gaPlot.start.points="Achievement Level Cuts",
	gaPlot.subtitle = TRUE,
    state,
    content_area,
    year,
    format="print",
    baseline=FALSE,
    equated=NULL,
	output.format="PDF",
    output.folder,
    assessment.name)
}

\arguments{
	\item{gaPlot.sgp_object}{
        The sgp_object containing system information for constructing the growth and achievement plot. Object is calculated using \code{\link{abcSGP}} or (at least) \code{\link{prepareSGP}}
        followed by \code{\link{analyzeSGP}}. The function requires coefficient matrices in order to display percentile growth trajectories.
	}
	\item{gaPlot.students}{
        Either NULL (the default) or a list of student IDs for whom one wishes to generate growth and achievement plots.
	}
	\item{gaPlot.percentile_trajectories}{
	A vector indicating the growth percentile trajectories to be depicted on the plot. If missing, the percentile trajectories will be the trajectories associated with the state supplied. If no
        state is supplied, the percentile trajectories will be 10, 35, 50, 65, 90.
	}
	\item{gaPlot.achievement_percentiles}{
        A vector of percentiles that achievement (i.e., status) percentiles will be depicted across the range of grades.
        }
        \item{gaPlot.show.scale.transformations}{
        A Boolean arguments (defaults to TRUE) indicating whether to show the scale as a vertical axis if a scale transformation is applied to the supplied data.
        }
	\item{gaPlot.grade_range}{
	The grade range for which to demonstrate plot. If missing, function uses supplied state to derive grade range.
	}
	\item{gaPlot.max.order.for.progression}{
	The maximum coefficient matrix order to use for each progression. Default is NULL which utilizes the maximum order available with the coefficient matrices.
	}
	\item{gaPlot.start.points}{
	Either 'Achievement Level Cuts' or 'Achievement Percentiles' defining where the growth percentiles trajectories will start in the growth achievement plots.
	}
	\item{gaPlot.subtitle}{Boolean variable (defaults to TRUE) indicating whether subtitle is printed on the growth achievement chart indicating student starting point.
	}
        \item{state}{
        A two letter acronym for the state associated with the supplied data.
        }
        \item{content_area}{
        A character vector indicating the content area to produce the growth and achievement chart. Note that the supplied content area must match that used in the internal labeling of content
	area for the sgp_object.
	}
	\item{year}{
	The year for which to produce the growth and achievement plots.
	}
	\item{format}{
	Either "print" (the default) for light background or "presentation" for dark background slides.
	}
	\item{baseline}{
	Boolean variable (defaults to FALSE) indicating whether to create percentile trajectories using baseline referenced coefficient matrices.
	}
    \item{equated}{
    Boolean variable (defaults to FALSE) indicating whether to utilize linked matrices to generate growth and achievement plots.
    }
	\item{output.format}{
	A character vector indicating which output format to use. Currently support 'PDF' (the default) and 'PNG'. The call to growthAchievementPlot in \code{\link{visualizeSGP}} outputs in both
	formats.
	}
	\item{output.folder}{
	A character vector indicating where to put the produced growth and achievement plot.
	}
	\item{assessment.name}{
	A character vector indicating the assessment.name. If missing, the supplied state is used to determine the assessment.name.
	}
}

\details{Typical use of the function is as part of \code{visualizeSGP} function. However, function can be used to produce growth and achievement charts.
}

\value{Function creates a growth and achievement chart and writes the result as a PDF to \code{output.folder}.}

\references{
Betebenner, D. W. (2012). Growth, standards, and accountability. In G. J. Cizek,
\emph{Setting Performance Standards: Foundations, Methods & Innovations. 2nd Edition} (pp. 439-450).
New York: Routledge.

Betebenner, D. W. (2009). Norm- and criterion-referenced student growth.
Educational Measurement: Issues and Practice, 28(4):42-51.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\keyword{misc}
\keyword{models}
