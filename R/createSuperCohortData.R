`createSuperCohortData` <- 
function(
        base_data, 
        sgp.config,
        supercohort_base_years, ## Subset of years of supplied_base data to use for super-cohort construction
        indicate_cohort=FALSE
) {

  YEAR <- CONTENT_AREA <- GRADE <- YEAR_NEW <- COHORT <- ID <- NULL

  ### Parameters
  data.years <- sort(unique(base_data$YEAR))
  tmp.cohort.list <- list()

  ### Test parameters
  if (!missing(supercohort_base_years) && !all(supercohort_base_years %in% data.years)) stop("Note: supercohort_base_years supplied not all in years provided in base_data.")

  ### Use supercohort_base_years to filter data if it is provided.
  if (!missing(supercohort_base_years)) base_data <- base_data[YEAR %in% supercohort_base_years]

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
      tmp.dt <- tmp.dt[unique(tmp.dt[, .(YEAR, GRADE, ID)]), mult="last"] ### Remove duplicates created by collapsing data into YEAR_NEW taking LAST (most recent) case
    }

    tmp.cohort.list[[paste(sgp.config.iter[['sgp.content.areas']][1], paste(sgp.config.iter[['sgp.grade.sequences']], collapse=""), sep="_")]] <- tmp.dt
  }
  return(rbindlist(tmp.cohort.list))
} ### END createSuperCohortData
