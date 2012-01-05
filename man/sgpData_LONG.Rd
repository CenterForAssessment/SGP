\name{sgpData_LONG}
\alias{sgpData_LONG}
\docType{data}
\title{Longitudinal Student Assessment Data in LONG format}
\description{
A dataset comprising a subset of five years of state assessment reading and mathematics data suitable for student growth percentile
and percentile growth projection/trajectory analyses. The dataset is in LONG format with each record representing a unique student, by 
year by content area combination. The example is used examples and illustration in enclosed documentation to show how SGPs can be operationalized
across years with the results being used to produce a variety of high quality visualizations both at the aggregate (e.g., school) and individual level.
}
\usage{data(sgpData_LONG)}
\format{
  A data frame of student level observations for five years and two content areas for the following variables.
  \describe{
    \item{\code{ID}}{Unique Student Identification Number}
    \item{\code{LAST_NAME}}{Student last name}
    \item{\code{FIRST_NAME}}{Student first name}
    \item{\code{CONTENT_AREA}}{Content area for student observation (Reading or Mathematics)}
    \item{\code{YEAR}}{Year for student observation}
    \item{\code{GRADE}}{Grade level of student observation}
    \item{\code{SCALE_SCORE}}{Student Scale Score}
    \item{\code{ACHIEVEMENT_LEVEL}}{Achievement level associated with student scale score}
    \item{\code{GENDER}}{Student gender}
    \item{\code{ETHNICITY}}{Student ethnicity}
    \item{\code{FREE_REDUCED_LUNCH_STATUS}}{Student free/reduced lunch status}
    \item{\code{ELL_STATUS}}{Student English Language Learner status}
    \item{\code{IEP_STATUS}}{Student Individual Education Plan status}
    \item{\code{GIFTED_AND_TALENTED_PROGRAM_STATUS}}{Student Gifted and Talented Program status}
    \item{\code{INSTRUCTOR_NUMBER_1}}{First instructor number associated with student record}
    \item{\code{INSTRUCTOR_NUMBER_2}}{Second instructor number associated with student record}
    \item{\code{SCHOOL_NUMBER}}{School number associated with student record}
    \item{\code{SCHOOL_NAME}}{School name associated with school number and student record}
    \item{\code{EMH_LEVEL}}{Elementary, Middle, High school indicator for school attended}
    \item{\code{DISTRICT_NUMBER}}{District number associated with student record}
    \item{\code{DISTRICT_NAME}}{District name associated with school number and student record}
    \item{\code{INSTRUCTOR_1_ENROLLMENT_STATUS}}{Indicator of enrollment status for instructor 1}
    \item{\code{INSTRUCTOR_2_ENROLLMENT_STATUS}}{Indicator of enrollment status for instructor 2}
    \item{\code{INSTRUCTOR_1_WEIGHT}}{Proporational weight associated with instructor 1}
    \item{\code{INSTRUCTOR_2_WEIGHT}}{Proporational weight associated with instructor 2}
    \item{\code{SCHOOL_ENROLLMENT_STATUS}}{Indicator of full academic year student enrollment in school}
    \item{\code{DISTRICT_ENROLLMENT_STATUS}}{Indicator of full academic year student enrollment in school}
    \item{\code{STATE_ENROLLMENT_STATUS}}{Indicator of full academic year student enrollment in school}
    \item{\code{VALID_CASE}}{Indicator of whether the case is valid or invalid}
  }
}

\note{All datasets used with \code{studentGrowthPercentiles} and \code{studentGrowthProjections} must be specifically formatted as \emph{wide} format files. The first 
variable/column is the student ID variable. The next set of columns provide the grade of the student across all the years provided in the dataset (possibly missing). 
The last set of columns provide the scales scores of the student in the respective grades. Operational users of this package are advised to use data formated similar to the sgpData_LONG
with the higher level functions \code{prepareSGP}, \code{analyzeSGP}, \code{combineSGP}, \code{summarizeSGP}} 


\seealso{\code{\link{studentGrowthPercentiles}}, \code{\link{studentGrowthProjections}}, \code{\link{sgpData}}, \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}},
\code{\link{summarizeSGP}}}

\source{Anonymized student level state assessment data in LONG format}
\keyword{datasets}
