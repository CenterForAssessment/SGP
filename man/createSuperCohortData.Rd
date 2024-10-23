\name{createSuperCohortData}
\alias{createSuperCohortData}
\title{Function to create longitudinal data set merging together individual years into a super-cohort of data.}
\description{
The function createSuperCohortData creates a longitudinal data set from another longitudinal data set in which individual years 
are merged together in order to create a multi-year, super-cohort of longitudinal data. See examples below.
}

\usage{
createSuperCohortData(
		base_data,
		sgp.config,
		supercohort_base_years,
		indicate_cohort=FALSE)
}

\arguments{
  \item{base_data}{Longitudinal data from which to create the super-cohort. Longitudinal data is long formatted data where each row represents a unique student by grade by year by content area record. For SGP and sgpFlow analyses, data should, at a minimum provide the variables: 'VALID_CASE', 'CONTENT_AREA', 'YEAR', 'GRADE', 'SCALE_SCORE'. 
  }
  \item{sgp.config}{Configurations that detail the super-cohorts of student assessment records to be produced.  
  }
  \item{supercohort_base_years}{Subset of years in the supplied \code{base_data} to use for super-cohort construction.
  }
  \item{indicate_cohort}{Boolean variable (defaults to FALSE) indicating whether to include a variable in the super-cohort data set indicating from which cohort the data record emanates.
  }
}

\value{Returns a data.table consisting of the multi-year, super-cohort data.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
\dontrun{

### super-cohort configurations
MATHEMATICS.config <- list(
	list(
		sgp.content.areas=rep("MATHEMATICS", 2),
		sgp.panel.years=c("2022_2023", "2023_2024"),
		sgp.grade.sequences=c("3", "4"),
		sgp.grade.sequences.lags=1),
	list(
		sgp.content.areas=rep("MATHEMATICS", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("3", "4", "5"),
		sgp.grade.sequences.lags=c(1,1)),
	list(
		sgp.content.areas=rep("MATHEMATICS", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("4", "5", "6"),
		sgp.grade.sequences.lags=c(1,1)),
	list(
		sgp.content.areas=rep("MATHEMATICS", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("5", "6", "7"),
		sgp.grade.sequences.lags=c(1,1)),
	list(
		sgp.content.areas=rep("MATHEMATICS", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("6", "7", "8"),
		sgp.grade.sequences.lags=c(1,1))
)

READING.config <- list(
	list(
		sgp.content.areas=rep("READING", 2),
		sgp.panel.years=c("2022_2023", "2023_2024"),
		sgp.grade.sequences=c("3", "4"),
		sgp.grade.sequences.lags=1),
	list(
		sgp.content.areas=rep("READING", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("3", "4", "5"),
		sgp.grade.sequences.lags=c(1,1)),
	list(
		sgp.content.areas=rep("READING", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("4", "5", "6"),
		sgp.grade.sequences.lags=c(1,1)),
	list(
		sgp.content.areas=rep("READING", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("5", "6", "7"),
		sgp.grade.sequences.lags=c(1,1)),
	list(
		sgp.content.areas=rep("READING", 3),
		sgp.panel.years=c("2021_2022", "2022_2023", "2023_2024"),
		sgp.grade.sequences=c("6", "7", "8"),
		sgp.grade.sequences.lags=c(1,1))
)

super_cohort_config <- c(MATHEMATICS.config, READING.config)

super_cohort_data <- createSuperCohortData(
					base_data=SGPdata::sgpData_LONG,
					sgp.config=super_cohort_config)


}  
}

\keyword{documentation}
