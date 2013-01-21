\name{studentGrowthPlot}
\alias{studentGrowthPlot}
\title{Create a student growth and achievement chart}
\description{Function used to produce individual student growth and achievement time lines/plots for based upon output from student growth percentile
analyses. Function is integrated with \code{SGPstateData} to accommodate state specific scales and nomenclature including performance
level names. See Betebenner (2012) for discussion}
\usage{
studentGrowthPlot(Scale_Scores,
	Plotting_Scale_Scores, 
	Achievement_Levels, 
	SGP, 
	SGP_Levels, 
	Grades, 
	Cuts_NY1,
	Connect_Points="Arrows",
	Cutscores,
	Report_Parameters) 
}
\arguments{
	\item{Scale_Scores}{
		A vector of historical scale scores.
	}
	\item{Plotting_Scale_Scores}{
		A vector of scale scores used as the vertical coordinates for plotting. If supplied, \code{Scale_Scores} are used for text and Plotting_Scale_Scores
		are used for the actual coordinates. 
	}
	\item{Achievement_Levels}{
		A vector of historical performance levels.
	}
	\item{SGP}{
		A vector of historical student growth percentiles.
	}
	\item{SGP_Levels}{
		A vector of historical growth (SGP) levels (e.g., low, typical, high).
	}
	\item{Grades}{
		A vector of historical grades student was tested in.
	}
	\item{Cuts_NY1}{
		A vector of cuts scores for next year.
	}
	\item{Connect_Points}{
		Argument specifying how historical achievement scores are connected in the studentGrowthPlot. Currently accepts either \code{Arrows}, the default, or 
		\code{None}.
	}
	\item{Cutscores}{
		A data.frame of long formatted achievement level cutscores.
	}
	\item{Report_Parameters}{
		A list containing arguments: \code{Current_Year}, \code{Content_Area}, \code{State}.
	}
}
\details{Function currently used as part of SGP package to produce student growth charts for states. More documentation to follow.}
\value{Returns a studentGrowthPlot graphical object for the content area specified. More documentation to follow.}
\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}
\references{
Betebenner, D. W. (2012). Growth, standards, and accountability. In G. J. Cizek,
\emph{Setting Performance Standards: Foundations, Methods & Innovations. 2nd Edition} (pp. 439-450).
New York: Routledge.
}
