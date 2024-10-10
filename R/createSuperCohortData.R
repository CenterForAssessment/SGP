`createSuperCohortData` <- 
function(
        base_data, 
        sgp.config,
        target_years, ## Provide either target_years OR num_priors. length(target_years) = num_priors + 1
        num_priors, ## Provide either num_priors OR target_years. num_priors = length(target_years) - 1
        indicate_cohort=FALSE
) {

  ### Parameters
  data.years <- sort(unique(base_data$YEAR))
  tmp.cohort.list <- list()

  ### Test parameters
  if (missing(target_years) & missing(num_priors)) stop("Provide either targets years for super-cohort or number of priors of desired SGP analyses.")
  if (missing(target_years) & !missing(num_priors)) target_years <- tail(data.years, num_priors)
  if (length(target_years) >= length(data.years)) stop("Super-cohort construction is possible only when number of target_years is less than number of years in supplied data.")

  ### Loop over configurations
  for (sgp.config.iter in sgp.config) {
    tmp.list <- list()
    for (data.years.iter in 1:(length(data.years) - sum(sgp.config.iter[['sgp.grade.sequences.lags']]))) {
      grade_year_content_area_map <- data.table(
        CONTENT_AREA = sgp.config.iter[['sgp.content.areas']],
        YEAR = data.years[c(data.years.iter, data.years.iter + (cumsum(sgp.config.iter[['sgp.grade.sequences.lags']])))],
        GRADE = sgp.config.iter[['sgp.grade.sequences']],
        YEAR_NEW = tail(data.years, length(sgp.config.iter[['sgp.content.areas']])))
 
      tmp.list[[data.years.iter]] <- base_data[grade_year_content_area_map, on=list(CONTENT_AREA, YEAR, GRADE)][,YEAR:=YEAR_NEW][,YEAR_NEW:=NULL]

      if (indicate_cohort) {
        tmp.list[[data.years.iter]][,COHORT:=paste(unlist(grade_year_content_area_map[,1:3]), collapse="_")]
      }

      tmp.dt <- rbindlist(tmp.list)
      setkey(tmp.dt, YEAR, GRADE, ID)
      tmp.dt <- tmp.dt[unique(tmp.dt[,.(YEAR, GRADE, ID)]), mult="last"] ### Remove duplicates created by collapsing data into YEAR_NEW taking LAST (most recent) case
    }
    tmp.cohort.list[[paste(sgp.config.iter[['sgp.content.areas']][1], paste(sgp.config.iter[['sgp.grade.sequences']], collapse=""), sep="_")]] <- tmp.dt
  }
  return(rbindlist(tmp.cohort.list))
} ### END createSuperCohortData