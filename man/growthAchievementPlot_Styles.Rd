\name{growthAchievementPlot_Styles}
\alias{growthAchievementPlot_Styles}
\title{growthAchievementPlot Styles for SGP}
\description{
Function that contains styles associated with the use of the \code{\link{growthAchievementPlot}} to create growth and achievement plots depicting system level results 
associated with student growth percentile results. The charts show, simultaneously, norm- and criterion-referneced student achievement (i.e., status) as well as 
norm- and criterion-referenced student growth. These charts are those shown on the cover of the December 2009 Issue of \emph{Educational Measurement: Issues and Practice}. 
See Betebenner (2009) and Betebenner (2012) for details.
}
\usage{
growthAchievementPlot_Styles(
        gaPlot.sgp_object,
        gaPlot.students,
        gaPlot.percentile_trajectories,
        gaPlot.achievement_percentiles,
        gaPlot.show.scale.transformations,
        gaPlot.grade_range,
        gaPlot.max.order.for.progression,
        state,
        content_area,
        year,
        format,
        baseline,
        pdf.folder,
        assessment.name)
}

\arguments{
	\item{gaPlot.sgp_object}{
        The sgp_object containing system information for constructing the growth and achievement plot. Object is calculated using 
        \code{\link{abcSGP}} or (at least) \code{\link{prepareSGP}}
        followed by \code{\link{analyzeSGP}}.
	}
	\item{gaPlot.students}{
        Either NULL or a list of student IDs for whom one wishes to generate growth and achievement plots. 
	}
	\item{gaPlot.percentile_trajectories}{
	A vector indicating the growth percentile trajectories to be depicted on the plot. If missing, the percentile trajectories will be the trajectories associated 
        with the state supplied.
	}
	\item{gaPlot.achievement_percentiles}{
        A vector of percentiles that achievement (i.e., status) percentiles will be depicted across the range of grades.
        }
        \item{gaPlot.show.scale.transformations}{
        A boolean argument indicating whether to show the scale as a vertical axis if a scale transformation is applied to the supplied data.
        }
	\item{gaPlot.grade_range}{
	The grade range for which to demonstrate plot. If missing, function uses supplied state to derive grade range.	
	}
	\item{gaPlot.max.order.for.progression}{
	The maximum coefficient matrix order to use for each progression. Default is utilizes the maximum order available with the coefficient matrices.
	}
        \item{state}{
        A two letter acronym for the state associated with the supplied data.
        }
        \item{content_area}{
        A character vector indicating the content area to produce the growth and achievement chart. Note that the supplied content area must match that used in the 
        internal labelling of content area for the sgp_object.
	}
	\item{year}{
	The year for which to produce the growth and achievement plots.
	}
	\item{format}{
	Either "print" for light background or "presentation" for dark background slides.
	}
	\item{baseline}{
	Boolean variable indicating whether to create percentile trajectories using baseline referenced coefficient matrices. 
	}
	\item{pdf.folder}{
	A character vector indicating where to put the produced growth and achievement plot.
	}
	\item{assessment.name}{
	A character vector indicating the assessment.name. If missing, the supplied state is used to determine the assessment.name.
	} 

}

\details{Utility function used by \code{\link{visualizeSGP}} to call \code{\link{growthAchievementPlot}}.  
}

\value{Function calls \code{\link{growthAchievementPlot}} and creates a growth and achievement chart and writes the result as a PDF to folder supplied by argument
\code{pdf.folder}.
}

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
