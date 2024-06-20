ArrowSGP <- R6::R6Class(
    "ArrowSGP",
    cloneable = FALSE,
    portable = FALSE,
    public = list(
        source_path = NULL,
        years = NULL,
        grades = NULL,
        content_areas = NULL,
        data = NULL,
        initialize =
          function(
            source_path = file.path("Data", "Arrow_SGP", "LONG"),
            years = NULL,
            grades = NULL,
            content_areas = NULL
          ) {
            source_path <<- source_path
            years <<- years
            grades <<- grades
            content_areas <<- content_areas
            invisible(self$set)
          }
    ),
    active = list(
        set = function() {
            data <<-
                arrow::open_dataset(
                    sources = source_path,
                    unify_schemas = TRUE
                )
            data.names <- names(data)

            ###   Variable Class Enforcement

            ##   `character` class (i.e. "strings")
            if ("ID" %in% data.names &&
                !grepl(": string", data$schema["ID"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(ID = cast(ID, arrow::string())) |>
                    dplyr::compute()
            }
            if ("VALID_CASE" %in% data.names &&
                !grepl(": string", data$schema["VALID_CASE"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(VALID_CASE = cast(VALID_CASE, arrow::string())) |>
                    dplyr::compute()
            }
            if ("CONTENT_AREA" %in% data.names &&
                !grepl(": string", data$schema["CONTENT_AREA"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(CONTENT_AREA = cast(CONTENT_AREA, arrow::string())) |>
                    dplyr::compute()
            }
            if ("YEAR" %in% data.names &&
                !grepl(": string", data$schema["YEAR"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(YEAR = cast(YEAR, arrow::string())) |>
                    dplyr::compute()
            }
            if ("GRADE" %in% data.names &&
                !grepl(": string", data$schema["GRADE"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(GRADE = cast(GRADE, arrow::string())) |>
                    dplyr::compute()
            }
            if ("ACHIEVEMENT_LEVEL" %in% data.names &&
                !grepl(": string", data$schema["ACHIEVEMENT_LEVEL"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(
                        ACHIEVEMENT_LEVEL = cast(ACHIEVEMENT_LEVEL, arrow::string())
                    ) |>
                    dplyr::compute()
            }
            if ("ACHIEVEMENT_LEVEL_PRIOR" %in% data.names &&
                !grepl(": string", data$schema["ACHIEVEMENT_LEVEL_PRIOR"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(
                        ACHIEVEMENT_LEVEL_PRIOR = cast(ACHIEVEMENT_LEVEL_PRIOR, arrow::string())
                    ) |>
                    dplyr::compute()
            }

            ##  `numeric` class (i.e. "float64")
            if ("SCALE_SCORE" %in% data.names &&
                !grepl(": double", data$schema["SCALE_SCORE"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(SCALE_SCORE = cast(SCALE_SCORE, arrow::float64())) |>
                    dplyr::compute()
            }
            if ("SCALE_SCORE_PRIOR" %in% data.names &&
                !grepl(": double", data$schema["SCALE_SCORE_PRIOR"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(SCALE_SCORE_PRIOR = cast(SCALE_SCORE_PRIOR, arrow::float64())) |>
                    dplyr::compute()
            }
            if ("SCALE_SCORE_CSEM" %in% data.names &&
                !grepl(": double", data$schema["SCALE_SCORE_CSEM"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(SCALE_SCORE_CSEM = cast(SCALE_SCORE_CSEM, arrow::float64())) |>
                    dplyr::compute()
            }

            ##  `Date` class (i.e. "date32")
            if ("DATE" %in% data.names &&
                !grepl(": date", data$schema["DATE"]$ToString())
            ) {
                data <<- data |>
                    dplyr::mutate(DATE = cast(DATE, arrow::date32())) |>
                    dplyr::compute()
            }

            ###   Filter/Subset YEAR/GRADE/CONTENT_AREA
            # if (!is.null(years))) {
            if (!is.null(years)) {
                data <<- data |>
                    dplyr::filter(YEAR %in% {{ years }})
            }
            if (!is.null(grades)) {
                data <<- data |>
                    dplyr::filter(GRADE %in% {{ grades }})
            }
            if (!is.null(content_areas)) {
                data <<- data |>
                    dplyr::filter(CONTENT_AREA %in% {{ content_areas }})
            }

            ###   Return FileSystemDataset/Table
            data <<- dplyr::compute(data)
            invisible(self)
        },
        get = function() {
            dplyr::collect(self$data)
        }
    )
)

###   S3 methods extensions for `ArrowSGP` class
names.ArrowSGP <- function(obj) {
    names(obj$data)
}

dim.ArrowSGP <- function(obj) {
    dim(obj$data)
}

nrow.ArrowSGP <- function(obj) {
    nrow(obj$data)
}

ncol.ArrowSGP <- function(obj) {
    ncol(obj$data)
}

summary.ArrowSGP <- function(obj) {
    tmp_data <- obj$data$get
    summary(tmp_data)
}
