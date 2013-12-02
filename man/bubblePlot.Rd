\name{bubblePlot}
\alias{bubblePlot}
\title{Core bubblePlot function for SGP}
\description{
Function to create bubble plots associated with student growth percentile and percentile growth trajectory results. The function is adaptable to many representations but 
is used in conjunction with results derived from \code{\link{summarizeSGP}} to represent summary level results of growth against achievement 
(usually, median student growth percentile against percentage at/above proficient). The function has MANY options and users are advised to 
read this documentation thoroughly as well as investigate the source code for the function itself to see what the many different representations 
that are possible. The function has the ability to produce interactive data tips using the \code{pdf2} package available on R-Forge. This package is NOT
installed as part of the SGP package and must be installed separately from the package to take advantage of this functionality of the \code{bubblePlot} function.
To install \code{pdf2} from the \code{R} prompt type: \code{install.packages("pdf2",repos="http://R-Forge.R-project.org")}. The use of the pdf2 package is 
scheduled to be deprecated as it is no longer maintained by the creator and requires use of a pre 2.14 version of R.
}
\usage{
bubblePlot(
	bubble_plot_data.X, 
	bubble_plot_data.Y,
	bubble_plot_data.SUBSET=NULL,
	bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL, 
	bubble_plot_data.SIZE, 
	bubble_plot_data.LEVELS=NULL, 
	bubble_plot_data.BUBBLE_TIPS_LINES,
	bubble_plot_labels.X=c("Growth", "Median Student Growth Percentile"),
	bubble_plot_labels.Y=c("Achievement", "Percent at/above Proficient"),
	bubble_plot_labels.SIZE=c(50, 100, 500, 1000),
	bubble_plot_labels.LEVELS=NULL,
	bubble_plot_labels.BUBBLE_TIPS_LINES=list("Median SGP (Count)",
		"Percent at/above Proficient"),
	bubble_plot_labels.BUBBLE_TITLES,
	bubble_plot_titles.MAIN="Growth and Achievement",
	bubble_plot_titles.SUB1="State School Performance",
	bubble_plot_titles.SUB2="Growth & Current Achievement",
	bubble_plot_titles.LEGEND1="School Size",
	bubble_plot_titles.LEGEND2_P1=NULL,
	bubble_plot_titles.LEGEND2_P2=NULL,
	bubble_plot_titles.NOTE=NULL,
	bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.03),
	bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
	bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_X_BANDS=NULL,
        bubble_plot_configs.BUBBLE_X_BAND_LABELS=NULL,
	bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
	bubble_plot_configs.BUBBLE_Y_TICKS_SIZE=rep(0.6, 11),
        bubble_plot_configs.BUBBLE_Y_BANDS=NULL,
        bubble_plot_configs.BUBBLE_Y_BAND_LABELS=NULL,
	bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0,
	bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.95),
	bubble_plot_configs.BUBBLE_COLOR="deeppink2",
        bubble_plot_configs.BUBBLE_COLOR_GRADIENT_REVERSE=FALSE,
	bubble_plot_configs.BUBBLE_TIPS=TRUE,
	bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
	bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",
	bubble_plot_configs.BUBBLE_PLOT_LEGEND=FALSE,
	bubble_plot_configs.BUBBLE_PLOT_TITLE=TRUE,
	bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=c("Growth", "Achievement"),
	bubble_plot_configs.BUBBLE_PLOT_EXTRAS=NULL,
        bubble_plot_configs.BUBBLE_PLOT_DIMENSION=NULL, ## List of WIDTH and HEIGHT
	bubble_plot_configs.BUBBLE_PLOT_NAME="bubblePlot.pdf",
	bubble_plot_configs.BUBBLE_PLOT_PATH=paste("Figures", sep=""),
	bubble_plot_pdftk.CREATE_CATALOG=FALSE) 
}

\arguments{
	\item{bubble_plot_data.X}{
	A vector of X coordinates for the bubbles to be plotted.
	}
	\item{bubble_plot_data.Y}{
	A vector of Y coordinates for the bubbles to be plotted.
	}
	\item{bubble_plot_data.SUBSET}{
	A Boolean vector indicating a subset of the bubbles to be highlighted and plotted. When BUBBLE_TIPS are indicated, only subsetted bubbles will show bubble tips.
	To further accentuate highlight bubbles, their radius can be altered using the bubble_plot_configs.BUBBLE_SUBSET_INCREASE argument. 
	Default value is \code{bubble_plot_data.SUBSET=NULL}.
	}
	\item{bubble_plot_data.INDICATE}{
	A Boolean vector indicating whether to attached a label to to further highlight in a manner suitable for printing. Usually done for few bubbles.
	Default value is \code{bubble_plot_data.INDICATE=NULL}.
	}
	\item{bubble_plot_data.BUBBLE_CENTER_LABEL}{
        A character vector to label the interior of the bubbles with. Usually a vector of singleton characters. 
	Default value is \code{bubble_plot_data.BUBBLE_CENTER_LABEL=NULL}.
	}
	\item{bubble_plot_data.SIZE}{
	A vector indicating the size of each of the bubbles plotted.
	}
	\item{bubble_plot_data.LEVELS}{
	A vector (usually a factor) indicating categories to which the bubbles belong. Default value is \code{bubble_plot_data.LEVELS=NULL}.
	}
	\item{bubble_plot_data.BUBBLE_TIPS_LINES}{
	A list of arbitrary length indicating the different values supplied when bubble tips are requested. 
	Default value is \code{bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(MEDIAN_SGP, ' (', MEDIAN_SGP_COUNT, ')', sep=''), round(PERCENT_AT_ABOVE_PROFICIENT))}.
	}
	\item{bubble_plot_labels.X}{
	A vector of length 2 where the 1st component is, usually a one word summary for the axis (e.g., Growth) and the 2nd component of the vector is a label for the
	axis (e.g., Median Student Growth Percentile).  Default value is \code{bubble_plot_labels.X=c('Growth', 'Median Student Growth Percentile')}.
	}
	\item{bubble_plot_labels.Y}{
	A vector of length 2 where the 1st component is, usually a one word summary for the axis (e.g., Achievement) and the 2nd component of the vector is a label for the
	axis (e.g., Percent at/above Proficient).  Default value is \code{bubble_plot_labels.Y=c('Achievement', 'Percent at/above Proficient')}.
	}
	\item{bubble_plot_labels.SIZE}{
	A vector of quantities giving breaking points for the size bubbles indicated in the legend of the plot. 
	Default value is \code{bubble_plot_labels.SIZE=c(50, 100, 500, 1000)}.
	}
	\item{bubble_plot_labels.LEVELS}{
	A vector of quantities giving level labels associated with \code{bubble_plot_data.LEVELS}. These labels will appear in the right legend of the plot. 
	Default value is \code{bubble_plot_labels.LEVELS=NULL}.
	}
	\item{bubble_plot_labels.BUBBLE_TIPS_LINES}{
	A list of labels that appear in the mouse over data tips. Should be of same length as the list from \code{bubble_plot_data.BUBBLE_TIPS_LINES}. 
	Default value is \code{bubble_plot_labels.BUBBLE_TIPS_LINES=list('Median SGP (Count)', 'Percent at/above Proficient')}.
	}
	\item{bubble_plot_labels.BUBBLE_TITLES}{
	A character vector with of the same length as number of points plotted indicated what name should appear on each mouse over bubble tip (e.g., the school name associated 
	with the bubble.  Default value is \code{bubble_plot_labels.BUBBLE_TITLES=SCHOOL_NAME},
	}
	\item{bubble_plot_titles.MAIN}{
	The main title of the bubble plot. Default value is \code{bubble_plot_titles.MAIN='Growth and Achievement'}.
	}
	\item{bubble_plot_titles.SUB1}{
	The right upper title of the bubble plot. Default value is \code{bubble_plot_titles.SUB1='State School Performance'}.
	}
	\item{bubble_plot_titles.SUB2}{
	The right lower title of the bubble plot. Default value is \code{bubble_plot_titles.SUB2='Growth & Current Achievement'}.
	}
	\item{bubble_plot_titles.LEGEND1}{
	The title of the upper legend to the right of the bubble plot. Default value is \code{bubble_plot_titles.LEGEND1='School Size'}.
	}
	\item{bubble_plot_titles.LEGEND2_P1}{
	The 1st line of the title of the lower legend of the bubble plot. Default value is \code{bubble_plot_titles.LEGEND2_P1=NULL}.
	}
	\item{bubble_plot_titles.LEGEND2_P2}{
	The 2nd line of the title of the lower legend of the bubble plot. Default value is \code{bubble_plot_titles.LEGEND2_P2=NULL}.
	}
	\item{bubble_plot_titles.NOTE}{
	A note, message, description, etc to be placed in lower half of the legend. Default value is \code{bubble_plot_titles.NOTE=NULL}.  
	Note that this can only be used if there are not LEVELS (and therefore no second legend).
	}
	\item{bubble_plot_configs.BUBBLE_MIN_MAX}{
	A vector of length two indicating min and max values for the bubbles in inches. Default value is \code{bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.03)}.
	}
	\item{bubble_plot_configs.BUBBLE_X_TICKS}{
	A vector indicating what x coordinates to display for the x axis of the bubble plot. Default value is \code{bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10)}.
	}
	\item{bubble_plot_configs.BUBBLE_X_TICKS_SIZE}{
	A vector, the same length as \code{bubble_plot_configs.BUBBLE_X_TICKS} indicating the character expansion (i.e., cex) associated with the characters.
	Default value is \code{bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5))}.
	}
	\item{bubble_plot_configs.BUBBLE_X_BANDS}{
	A vector of cutpoints used to separate the plot background into horizontal rectangular regions.
	}
	\item{bubble_plot_configs.BUBBLE_X_BAND_LABELS}{
	A character vector of labels used to labels the rectangles produced with \code{bubble_plot_configs.BUBBLE_X_BANDS}.
	}
	\item{bubble_plot_configs.BUBBLE_Y_TICKS}{
	A vector indicating what x coordinates to display for the x axis of the bubble plot. Default value is \code{bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10)}.
	}
	\item{bubble_plot_configs.BUBBLE_Y_TICKS_SIZE}{
	A vector, the same length as \code{bubble_plot_configs.BUBBLE_Y_TICKS} indicating the character expansion (i.e., cex) associated with the characters.
	Default value is \code{bubble_plot_configs.BUBBLE_Y_TICKS_SIZE=rep(0.6, 11)}.
	}
	\item{bubble_plot_configs.BUBBLE_Y_BANDS}{
	A vector of cutpoints used to separate the plot background into horizontal rectangular regions.
	}
	\item{bubble_plot_configs.BUBBLE_Y_BAND_LABELS}{
	A character vector of labels used to labels the rectangles produced with \code{bubble_plot_configs.BUBBLE_X_BANDS}.
	}
	\item{bubble_plot_configs.BUBBLE_SUBSET_INCREASE}{
	Default value is \code{bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0}.
	}
	\item{bubble_plot_configs.BUBBLE_SUBSET_ALPHA}{
	Default value is \code{bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.95)}.
	}
	\item{bubble_plot_configs.BUBBLE_COLOR}{
	Default value is \code{bubble_plot_configs.BUBBLE_COLOR="deeppink2"}.
	}
	\item{bubble_plot_configs.BUBBLE_COLOR_GRADIENT_REVERSE}{
	Boolean argument (defaults to FALSE) indicating whether to reverse color gradient associated with bubbles.
	}
	\item{bubble_plot_configs.BUBBLE_TIPS}{
	Default value is \code{bubble_plot_configs.BUBBLE_TIPS=TRUE}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_DEVICE}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_DEVICE='PDF'}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_FORMAT}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_FORMAT='print'}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_LEGEND}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_LEGEND=FALSE}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_TITLE}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_TITLE=TRUE}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=c('Growth', 'Achievement')}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_EXTRAS}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_EXTRAS=NULL}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_DIMENSION}{
        Default value is \code{bubble_plot_configs.BUBBLE_PLOT_DIMENSION=NULL}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_NAME}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_NAME='bubblePlot.pdf'}.
	}
	\item{bubble_plot_configs.BUBBLE_PLOT_PATH}{
	Default value is \code{bubble_plot_configs.BUBBLE_PLOT_PATH=paste('Figures', sep='')}.
	}
	\item{bubble_plot_pdftk.CREATE_CATALOG}{
	Default value is \code{bubble_plot_pdftk.CREATE_CATALOG=FALSE}. 
	}
}

\details{Typical use of the function is as part of \code{visualizeSGP} function. However, function can be used more generically for diverse plots showing many dimensions of data simultaneously. 
}

\value{Function creates a bubble chart and writes the result as a PDF to \code{bubble_plot_configs.BUBBLE_PATH}.} 

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\keyword{misc}
\keyword{models}
