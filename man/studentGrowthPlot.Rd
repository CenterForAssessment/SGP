\name{studentGrowthPlot}
\alias{studentGrowthPlot}
\title{Create a student growth and achievement chart}
\description{Function used to produce individual student growth and achievement chart (an achievement time lines indicating student growth) based upon output from student growth percentile
and student growth projection analyses. Function is integrated with \code{SGPstateData} to accommodate state specific scales and nomenclature including performance
level names. See Betebenner (2012) for discussion}
\usage{
studentGrowthPlot(Scale_Scores,
	Plotting_Scale_Scores, 
	Achievement_Levels, 
	SGP, 
	SGP_Levels, 
	Grades,
	Content_Areas,
	Cuts,
	Plotting_Cuts,
	SGP_Targets,
	SGP_Scale_Score_Targets,
	Plotting_SGP_Scale_Score_Targets,
	Cutscores,
	Years,
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
	\item{Content_Areas}{
		A vector of historical content areas student was tested in.
	}
	\item{Cuts}{
		A list of cuts scores for NY1, NY2, and NY3.
	}
	\item{Plotting_Cuts}{
		A list of plotting cuts scores for NY1, NY2, and NY3. Plotting cuts are identical to Cuts for states with a vertical scale and are transformed for non-vertical scale states.
	}
	\item{SGP_Targets}{
		A list of CUKU, CUKU_Current, MUSU, MUSU_Current targets.
	}
	\item{SGP_Scale_Score_Targets}{
		A list of CUKU, CUKU_Current, MUSU, MUSU_Current scale score targets.
	}
	\item{Plotting_SGP_Scale_Score_Targets}{
		A list of CUKU, CUKU_Current, MUSU, MUSU_Current scale score targets for plotting that are transformed when no vertical scale exists.
	}
	\item{Cutscores}{
		A data.frame of long formatted achievement level cutscores.
	}
	\item{Years}{
		A vector of years corresponding to supplied scale scores.
	}
	\item{Report_Parameters}{
		A list containing arguments: \code{Current_Year}, \code{Content_Area}, \code{State}, \code{Denote_Content_Area}, \code{SGP_Targets}, and \code{Configuration}.
	}
}
\details{Function currently used as part of SGP package to produce student growth charts for states. Function is usually called from the higher level function \code{\link{visualizeSGP}} which
allows for the creation of a student growth plot catalog for each school with student reports organized by grade and student name.} 
\value{Returns a student growth plot graphical object that is usually exported in either PDF or PNG format.}
\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}
\references{
Betebenner, D. W. (2012). Growth, standards, and accountability. In G. J. Cizek,
\emph{Setting Performance Standards: Foundations, Methods & Innovations. 2nd Edition} (pp. 439-450).
New York: Routledge.
}
