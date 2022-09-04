`gofPrint` = function(
    sgp_object,
    years = NULL,
    content_areas = NULL,
    grades = NULL,
    sgp_types = NULL,
    norm_group = NULL,
    output_format = c("PDF", "SVG", "DECILE_TABLES"),
    output_path = "Goodness_of_Fit",
    ...
  ) {

    CONTENT_AREA <- YEAR <- SGP_TYPE <- NULL

    ### Utility functions
    `%w/o%` = function(x, y) x[!x %in% y]

    gof.slots <- names(sgp_object@SGP[["Goodness_of_Fit"]])

    if (is.null(gof.slots)) {
      print("No goodness of fit plots found in sgp_object@SGP[['Goodness_of_Fit']], skipping gofPrint.")
      return()
    }
    
    if (is.null(years)) {
      # Put this first for YEAR with `.` in it (e.g. "2021_2022.2")
      years <- unique(sgp_object@Data[, YEAR])
      years <- years[unlist(lapply(years, function(f) any(grepl(f, gof.slots))))]
    } else {
      tmp.years <- years[unlist(lapply(years, function(f) any(grepl(f, gof.slots))))]
      if (!all(years %in% tmp.years)) {
        messageSGP(paste("\tFit plots not available for", years %w/o% tmp.years, collapse = "\n"))
        years <- tmp.years
      }
    }
    gof.slots <-
      unlist(
        sapply(X = gof.slots, USE.NAMES = FALSE,
               function(f) {
                  tmp.year <-
                    years[unlist(lapply(years, function(ff) any(grepl(ff, f))))]
                  if (length(tmp.year)) f[grepl(tmp.year, f)]
                })
            )

    if (!is.null(norm_group)) {
      for (gs in gof.slots) {
        tmp.ng <- names(sgp_object@SGP[["Goodness_of_Fit"]][[gs]])
        if (!any(norm_group %in% tmp.ng)) {
          gof.slots <- gof.slots %w/o% gs
        }
      }
      if (!is.null(sgp_types)) {
        gof.slots <- gof.slots[grepl(paste0(sgp_types, "$", collapse = "|"), gof.slots)]
      }
      ##  Base these arguments on `norm_group` if specified:
      years <- years[unlist(lapply(years, function(f) any(grepl(f, gof.slots))))]
      content_areas <- sgp_types <- grades <- NULL
    }

    if (!is.null(content_areas) || !is.null(sgp_types)) {
      # `strsplit` by `years` instead of `.` (used originally in `Literasee`)
      all.slots <-
        lapply(X = gof.slots,
               FUN = function(f) {
                 tmp.year <-
                   years[unlist(lapply(years, function(ff) any(grepl(ff, f))))]
                 data.table(matrix(c(
                    tmp.year,
                    gsub("(^[.]+)|([.]+$)", "", strsplit(f, tmp.year)[[1]])), nrow = 1))
                })
      all.slots <- rbindlist(all.slots, fill = TRUE)
      if (ncol(all.slots) < 3) {
        # only cohort referenced `SGP_TYPE` available
        all.slots[, "V3" := as.character(NA)]
      }
      setnames(all.slots,
               c("V1", "V2", "V3"),
               c("YEAR", "CONTENT_AREA", "SGP_TYPE"))
      setcolorder(all.slots, c(2, 1, 3))
      if (is.null(content_areas)) {
        content_areas <-
          unique(all.slots[YEAR %in% years, CONTENT_AREA])
      }
      if (is.null(sgp_types)) {
        sgp_types <-
          unique(all.slots[YEAR %in% years & CONTENT_AREA %in% content_areas, SGP_TYPE])
      }

      gof.slots <- all.slots[CONTENT_AREA %in% content_areas &
                             YEAR %in% years &
                             SGP_TYPE %in% sgp_types]
      gof.slots <- gsub(".NA$", "", apply(gof.slots, 1, paste, collapse = "."))
    }

    for (i in gof.slots) {
      if (length(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
        if ("DECILE_TABLES" %in% toupper(output_format)) {
          tmp.path <- file.path(output_path, i, "Decile_Tables")
        } else {
          tmp.path <- file.path(output_path, i)
        }
        if (!dir.exists(tmp.path)) dir.create(tmp.path, recursive = TRUE)

        grobs.to.print <- names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])
        if (!is.null(norm_group)) {
          grobs.to.print <-
            grep(paste(norm_group, collapse = "|"), grobs.to.print, value = TRUE)
        }
        if (!is.null(grades)) {
          grobs.to.print <- 
            unlist(lapply(
              grobs.to.print,
              function(f) {
                tmp.tf <- grepl(paste(grades, collapse = "|"),
                                strsplit(f, ";")[[1]][1])
                f[tmp.tf]
              }))
        }

        if (!length(grobs.to.print)) next

        for (j in grobs.to.print) {
          tmp.path <- file.path(output_path, i, j)
          if (!identical(.Platform$OS.type, "unix") & nchar(tmp.path) > 250L) {
            tmp.content_area <- unlist(strsplit(j, "[.]"))[1L]
            tmp.path <- gsub(tmp.content_area, substr(tmp.content_area, 1, 1), tmp.path)
          }
          if ("PDF" %in% toupper(output_format)) {
            grDevices::pdf(file = paste0(tmp.path, ".pdf"), width = 8.5, height = 11)
            if ("PLOT" %in% names(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])) {
              grid::grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["PLOT"]])
            } else {
              grid::grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
            }
            dev.off()
          }
          if ("PNG" %in% toupper(output_format)) {
            Cairo::Cairo(file = paste0(tmp.path, ".png"),
                         width = 8.5, height = 11, units = "in",
                         dpi = 144, pointsize = 10.5, bg = "transparent")
            if ("PLOT" %in% names(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])) {
                grid::grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["PLOT"]])
            } else {
              grid::grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
            }
            dev.off()
          }
          if ("SVG" %in% toupper(output_format)) {
            svglite::svglite(filename = paste0(tmp.path, ".svg"),
                             pointsize = 11.5, bg = "transparent",
                             width = 8.5, height = 11, ...)
            if ("PLOT" %in% names(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])) {
              grid::grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["PLOT"]])
            } else {
              grid::grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
            }
            dev.off()
          }
          if ("DECILE_TABLES" %in% toupper(output_format)) {
            decile.table <- sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["TABLE"]]
            save(decile.table,
                 file = file.path(output_path, i, "Decile_Tables",
                                  paste0(j, "_Decile_Table.Rdata")))
          }
        }
      } else {
        messageSGP(paste0(
            "\tNOTE: Goodness of Fit plots are not available to print for ", i,
            ". No plots will be produced."))
      }
    }
  }
