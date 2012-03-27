\name{summarizeSGP}
\alias{summarizeSGP}
\title{Summarize student scale scores, proficiency levels and student growth percentiles according to user specified summary group variables}
\description{
 Utility function used to produce summary tables using long formatted data that contain student growth percentiles.  An exemplar is provided from the successive execution of \code{\link{prepareSGP}}, \code{\link{analyzeSGP}} and \code{\link{combineSGP}}.
}

\usage{
summarizeSGP(sgp_object,
           state,
           years,
           content_areas,
           sgp.summaries=NULL,
           summary.groups=NULL,
           confidence.interval.groups=NULL) 
}

\arguments{
  \item{sgp_object}{A list containing long formatted data in the \code{Student} slot. If summaries of student growth percentiles are requested, those quantities must first be produced (possibly by first using \code{\link{analyzeSGP}}) and subsequently combined with the \code{Student} data (possibly with \code{\link{combineSGP}}).
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{SGPstateData}.
  }
  \item{years}{A vector indicating year(s) in which to produce summary tables associated with student growth percentile and percentile growth trajectory/projection analyses. If missing the
function will use the data to calculate years and produce summaries for the most recent three years.
  }
  \item{content_areas}{A vector indicating content area(s) in which to produce student growth percentiles and/or student growth projections/trajectories. 
If missing the function will use the data to infer the content area(s) available for analyses.
  }
  \item{sgp.summaries}{A list giving the summaries requested for each group analyzed based upon the \code{summary.group} argument. The default (produced internal to summarizeSGP) summaries include:
\tabular{ll}{
\code{MEDIAN_SGP} \tab The group level median student growth percentile.\cr 
\code{MEDIAN_SGP_COUNT} \tab The number of students used to compute the median.\cr 
\code{PERCENT_AT_ABOVE_PROFICIENT} \tab The percentage of students at or above proficient.\cr 
\code{PERCENT_AT_ABOVE_PROFICIENT_COUNT} \tab The number of students used to compute the percentage at/above proficient.\cr 
\code{PERCENT_AT_ABOVE_PROFICIENT_PRIOR} \tab The percentage of students at or above proficient in the prior year.\cr 
\code{PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT} \tab The number of students used to compute the percentage at/above proficient in the prior year.\cr 
}
NOTE: The internal function \code{percent_in_category()} summary function requires a variable that MUST be a factor with proficiency categories as levels. The function utilizes the SGPstateData
with the provided state name in an attempt to identify achievement levels and whether or not they are considered proficient. 
  }
  \item{summary.groups}{A list consisting of 8 elements indicating the types of groups across which all summaries are taken (Inclusion means that summaries will be calculated for levels of the 
associated variable). For state data, if the list is not explicitly provided, the function will attempt to determine levels based upon meta data supplied in the \code{@Names} slot of the provided
SGP object. See \code{\link{prepareSGP}} for more information on supplied meta-data.  
\tabular{ll}{
\code{institution}: \tab State, District and/or School.\cr 
\code{content area}: \tab Variable indicating content area (default is CONTENT_AREA) if content area summaries are of interest.\cr 
\code{time}: \tab Variable indicating time (default is YEAR) if time summaries are of interest. NOTE: Cross year (i.e., multi-year) summaries default to 3 years.\cr 
\code{institution_type}: \tab Variable(s) indicating the type of institution institution (default EMH_LEVEL) if summaries by institution type is of interest.\cr
\code{institution_level}: \tab Variable(s) indicating levels within the institution (default GRADE) if summaries by institution level is of interest.\cr
\code{demographic}: \tab  Demographics variables if summaries by demographic subgroup are of interest.\cr 
\code{institution_inclusion}: \tab Variables indicating inclusion for institutional calculations.\cr 
\code{growth_only_summary}: \tab Variables indicating whether to calculate summaries only for those students with growth in addition to other analyses.\cr
}
All group slots MUST be included in the list, although \code{NULL} can be provided if a grouping subset is not desired. All possible combinations of the group variables are produced. Specification of \code{summary.groups} can be tested prior to use of \code{summarizeSGP} with the \code{\link{viewSummaryGroups}} function.  See that \code{\link{viewSummaryGroups}} documentation for details.
  }
  \item{confidence.interval.groups}{A list consisting of information used to calculate group confidence intervals: 
\tabular{ll}{
\code{TYPE}: \tab Either \code{Bootstrap} (default) or \code{CSEM} indicating Bootstrap confidence interval calculation (the default) or conditional standard error of measurement based confidence interval calculation (experimental).\cr 
\code{VARIABLES}: \tab The variables on which to calculate confidence intervals (default is SGP).\cr 
\code{QUANTILES} \tab The desired confidence quantiles.\cr 
\code{GROUP} \tab The group summaries for which confidence intervals should be constructed.\cr
\code{content} \tab The content area variable if confidence intervals by content area are desired.\cr
\code{time} \tab The time variable (default is YEAR) if confidence intervals by time period are desired.\cr
\code{institution_type} \tab The insitution type variables (e.g., EMH_LEVEL, default is EMH_LEVEL) if confidence intervals by institution level are desired.\cr
\code{institution_level} \tab The insitution level variables (e.g., GRADE, default is NULL) if confidence intervals by institution level are desired.\cr
\code{demographic} \tab The demographic variables if confidence intervals by demographic subgroups are desired.\cr
\code{institution_inclusion} \tab The institution inclusion variables if confidence intervals by institution inclusion subgroups are desired.\cr
\code{growth_only_summary} The growth only summary variables if confidence intervals by growth only summary group are desired.\cr
}
For \code{CSEM} analysis this argument requires that simulated SGPs have been produced (see \code{\link{analyzeSGP}} for more information). List slots set to \code{NULL} will not produce confidence intervals. NOTE: This is currently an experimental functionality and is very memory intensive. Groups to be included should be identified selectively! The default 95\% confidence intervals are provided in the selected summary tables as two additional columns named \code{LOWER_MEDIAN_SGP_95_CONF_BOUND} and \code{UPPER_MEDIAN_SGP_95_CONF_BOUND}. Specification of \code{confidence.interval.groups} can be tested prior to use of \code{summarizeSGP} with the \code{\link{viewSummaryGroups}} function.
  }
}

\details{Function makes use of the \code{foreach} package to parallel process summary tables of student data. Currently, it is the user's responsibility to register a parallel back end of their choice before running the \code{summarizeSGP} function. The proper choice may be dependent upon the user's operating system, software and system memory capacity. Please see the \code{foreach} documentation for details. If no parallel back end is specified, the function will process the summary tables sequentially.
}

\value{Function returns lists containing the summary tables as data.table objects in the \code{Summary} slot of the SGP data object. Each \code{institution} has a slot in the \code{Summary} list. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam VanIwaarden \email{Vaniwaarden@Colorado.edu}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
## summarizeSGP is Step 4 of 5 of abcSGP
Demonstration_Data <- sgpData_LONG
Demonstration_Data <- prepareSGP(Demonstration_Data)
Demonstration_Data <- analyzeSGP(Demonstration_Data)
Demonstration_Data <- combineSGP(Demonstration_Data)
Demonstration_Data <- summarizeSGP(Demonstration_Data)
}  
}

\keyword{documentation}
