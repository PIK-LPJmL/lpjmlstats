# ------ reduction of space and time dimensions ------------------------------

#' @title GlobSumTimeAvgTable
#' @description
#' GlobSumTimeAvgTable metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobSumTimeAvgTable <- # nolint: object_name_linter.
  R6::R6Class(
    "GlobSumTimeAvgTable",
    inherit = Metric,
    public = list(
      #' @description
      #' First take global weighted sum, then average over all time steps
      #' of the simulation period. The result is a scalar for each band.
      #' @param data LPJmLDataCalc object to be summarized
      #' @return A summarized \link{LPJmLDataCalc} object
      summarize = function(data) {
        data %>%
          aggregate(cell = list(to = "global", stat = "weighted_sum")) %>%
          aggregate(time = list(to = "sim_period", stat = "mean"))
      },

      #' @description
      #' Calculate difference and relative difference to the baseline.
      #' @param var_grp variable group
      compare = function(var_grp) {
        var_grp$compare <-
          list(
            `diff2base` = lapply(var_grp$under_test, function(x) {
              x - var_grp$baseline
            }),
            `diff_%` = lapply(var_grp$under_test, function(x) {
              (x - var_grp$baseline) / var_grp$baseline * 100
            })
          )
      },

      #' @description
      #' Create a table of the results.
      #' @return A tibble with the results
      plot = function() {
        create_table_plot(
          self$var_grp_list,
          self$m_options
        )
      },

      #' @description
      #' Style the table to be displayed in the report.
      #' @param table A tibble with the results
      arrange_plots = function(table) {
        arrange_table_plot(table, self$m_options)
      },

      #' @field m_options
      #' List of metric options specific to this metric:
      #' - `font_size`: integer, font size of the table (default 7)
      #' - `disp_digits`: integer, number of significant digits to display (default 4)
      #' - `year_subset`: character vector, defines which calendar years the metric considers,
      #' i.e., a data subset that the metric works with; e.g., c("1995", "1996") (default 1991:2000).
      #' - `cell_subset`: character vector, defines which cells to subset (default NULL)
      m_options = list(
        font_size = 7,
        disp_digits = 4,
        year_subset = as.character(1991:2000),
        cell_subset = NULL
      ),

      #' @field title
      #' Section header used in the report
      title = "Global Sum Time Average Table",

      #' @field description
      #' Description used in the report
      description = "The cell-time values of each variable are weighted by
             reference area, summed globally
             and then averaged over time. \n"

    )
  )

#' @title GlobAvgTimeAvgTable
#' @description
#' GlobAvgTimeAvgTable metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobAvgTimeAvgTable <- R6::R6Class( # nolint: object_name_linter.
  "GlobAvgTimeAvgTable",
  inherit = GlobSumTimeAvgTable,
  public = list(
    #' @description
    #' First take global weighted mean, then average over all time steps.
    #' @param data LPJmLDataCalc object to be summarized
    #' @return A summarized \link{LPJmLDataCalc} object
    summarize = function(data) {
      data %>%
        aggregate(cell = list(to = "global", stat = "weighted_mean")) %>%
        aggregate(time = list(to = "sim_period", stat = "mean"))
    },

    #' @field title
    #' Section header used in the report
    title = "Global Average Time Average Table",

    #' @field description
    #' Description used in the report
    description = "The cell-time values of each variable are weighted by
             reference area, averaged globally
             and then averaged over time. \n"
  )
)



# ------- reduction of space dimension ---------------------------------------

#' @title GlobSumTimeseries
#' @description
#' GlobSumTimeseries metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobSumTimeseries <- R6::R6Class( # nolint: object_name_linter.
  "GlobSumTimeseries",
  inherit = Metric,
  public = list(
    #' @description
    #' Take a global weighted sum of the output.
    #' @param data LPJmLDataCalc object to be summarized
    #' @return A summarized \link{LPJmLDataCalc} object
    summarize = function(data) {
      data %>%
        aggregate(cell = list(to = "global", stat = "weighted_sum"))
    },

    #' @description
    #' Create a time series plot of the results.
    #' @return A list of time series ggplots
    plot = function() {
      create_time_series_plots(
        self$var_grp_list,
        self$m_options
      )
    },

    #' @description
    #' Arrange the time series plots side by side
    #' with legends pooled together in the top left
    #' @param plotlist List of time series ggplots
    arrange_plots = function(plotlist) {
      arrange_timeseries_plots(plotlist, self$m_options)
    },

    #' @field m_options
    #' List of metric options specific to this metric:
    #' - `font_size`: integer, font size of the plot (default 6)
    #' - `year_subset`: character vector, defines which calendar years the metric considers,
    #' i.e., a data subset that the metric works with; e.g., c("1995", "1996") (default "1901" - "2019").
    #' - `cell_subset`: character vector, defines which cells to subset (default NULL)
    #' - `num_cols`: integer, number of columns in the plot grid in the report (default 2)
    #' - `var_subheading`: boolean, if TRUE, before plots for a new variable are added to the report, 
    #' a linebreak will be inserted and a subheading
    #' with the name of the variable will be added. Both things are intended to visually seperate
    #' the plots of different variables and to better organize the report,
    #' especially if the metric generates many plots for each variable. (default FALSE)
    #' - `band_subheading`: analogous to var_subheading but for bands (default FALSE)
    m_options = list(font_size = 6,
                     num_cols = 2,
                     var_subheading = FALSE,
                     band_subheading = FALSE,
                     year_subset = as.character(1901:2019),
                     cell_subset = NULL),

    #' @field title
    #' Section header used in the report
    title = "Global Sum Time Series",

    #' @field description
    #' Description used in the report
    description = "The cell-time values of each variable are weighted by
             reference area, summed globally
             and then plotted over time. \n"
  )
)

#' @title GlobAvgTimeseries
#' @description
#' GlobAvgTimeseries metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobAvgTimeseries <- R6::R6Class( # nolint: object_name_linter.
  "GlobAvgTimeseries",
  inherit = GlobSumTimeseries,
  public = list(
    #' @description
    #' Take the global weighted mean
    #' over the cells.
    #' @param data LPJmLDataCalc object to be summarized
    #' @return A summarized \link{LPJmLDataCalc} object
    summarize = function(data) {
      data %>%
        aggregate(cell = list(to = "global", stat = "weighted_mean"))
    },

    #' @field title
    #' Section header used in the report
    title = "Global Average Time Series",

    #' @field description
    #' Description used in the report
    description = "The cell-time values of each variable are weighted by
             reference area and averaged globally. \n"
  )
)

#' @title GlobSumAnnAvgTimeseries
#' @description
#' GlobSumAnnAvgTimeseries metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobSumAnnAvgTimeseries <- # nolint: object_name_linter.
  R6::R6Class(
    "GlobSumAnnAvgTimeseries",
    inherit = GlobSumTimeseries,
    public = list(
      #' @description
      #' Take the mean for each year and then the global weighted sum
      #' over the cells.
      #' @param data LPJmLDataCalc object to be summarized
      #' @return A summarized \link{LPJmLDataCalc} object
      summarize = function(data) {
        data %>%
          aggregate(time = list(to = "years", stat = "mean")) %>%
          aggregate(cell = list(to = "global", stat = "weighted_sum"))
      },

      #' @field title
      #' Section header used in the report
      title = "Global Sum Annual Average Time Series ",

      #' @field description
      #' Description used in the report
      description = "The cell-time values of each variable are weighted by
             reference area, summed globally, averaged annually,
             and then plotted over time. \n"
    )
  )

#' @title GlobAvgAnnAvgTimeseries
#' @description
#' GlobAvgAnnAvgTimeseries metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobAvgAnnAvgTimeseries <- R6::R6Class( # nolint: object_name_linter.
  "GlobAvgAnnAvgTimeseries",
  inherit = GlobSumTimeseries,
  public = list(
    #' @description
    #' Take the mean for each year and then the global weighted mean
    #' over the cells.
    #' @param data LPJmLDataCalc object to be summarized
    #' @return A summarized \link{LPJmLDataCalc} object
    summarize = function(data) {
      data %>%
        aggregate(time = list(to = "years", stat = "mean")) %>%
        aggregate(cell = list(to = "global", stat = "weighted_mean"))
    },

    #' @field title
    #' Section header used in the report
    title = "Global Average Annual Average Time Series",

    #' @field description
    #' Description used in the report
    description = "The cell-time values of each variable are weighted by
             reference area, averaged globally and
             averaged annually. \n"
  )
)

#' @title CellSubsetAnnAvgTimeseries
#' @description
#' CellSubsetAnnAvgTimeseries metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
CellSubsetAnnAvgTimeseries <- # nolint: object_name_linter.
  R6::R6Class(
    "CellSubsetAnnAvgTimeseries",
    inherit = GlobAvgTimeseries,
    public = list(
      #' @description
      #' Subset the cells and compute an annual average.
      #' @param lpjml_data LPJmLDataCalc object to be summarized
      #' @return A summarized \link{LPJmLDataCalc} object
      summarize = function(lpjml_data) {
        subset(lpjml_data, cell = self$m_options$cell) %>%
          aggregate(time = list(to = "years", stat = "mean"))
      },

      #' @field m_options
      #' List of metric options specific to this metric:
      #' - `font_size`: integer, font size of the plot (default 6)
      #' - `year_subset`: character vector, defines which calendar years the metric considers,
      #' i.e., a data subset that the metric works with; e.g., c("1995", "1996") (default "1901" - "2019").
      #' - `cell`: cells to be subsetted (default 10000)
      #' - `num_cols`: integer, number of columns in the plot grid in the report (default 2)
      #' - `var_subheading`: boolean, if TRUE, before plots for a new variable are added to the report, 
      #' a linebreak will be inserted and a subheading
      #' with the name of the variable will be added. Both things are intended to visually seperate
      #' the plots of different variables and to better organize the report,
      #' especially if the metric generates many plots for each variable. (default FALSE)
      #' - `band_subheading`: analogous to var_subheading but for bands (default FALSE)
      m_options = list(
        font_size = 6,
        year_subset = as.character(1901:2019),
        num_cols = 2,
        var_subheading = FALSE,
        band_subheading = FALSE,
        cell = 10000
      ),

      #' @field description
      #' Description used in the report
      description =
        paste0(
          "The metric subsets one or several cells ",
          "and averages annually. \n"
        ),

      #' @field title
      #' Section header used in the report
      title = "Cell Subset Annual Average Timeseries"
    )
  )

#' @title CellSubsetTimeseries
#' @description
#' CellSubsetTimeseries metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
CellSubsetTimeseries <- # nolint: object_name_linter.
  R6::R6Class(
    "CellSubsetTimeseries",
    inherit = CellSubsetAnnAvgTimeseries,
    public = list(
      #' @description
      #' Subset the cells.
      #' @param lpjml_data LPJmLDataCalc object to be summarized
      #' @return A summarized \link{LPJmLDataCalc} object
      summarize = function(lpjml_data) {
        subset(lpjml_data, cell = self$m_options$cell)
      },

      #' @field description
      #' Description used in the report
      description =
        paste0("The metric subsets one or several cells. \n"),

      #' @field title
      #' Section header used in the report
      title = "Cell Subset Timeseries"
    )
  )


# ------- reduction of time dimension -----------------------------------------

#' @title TimeAvgMap
#' @description
#' TimeAvgMap metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
TimeAvgMap <- # nolint: object_name_linter.
  R6::R6Class(
    "TimeAvgMap",
    inherit = Metric,
    public = list(
      #' @description
      #' Take the mean over the simulation period for each cell.
      #' @param data LPJmLDataCalc object to be summarized
      #' @return A summarized \link{LPJmLDataCalc} object
      summarize = function(data) {
        data %>%
          aggregate(time = list(to = "sim_period", stat = "mean"))
      },

      #' @description
      #' Compare the baseline summary with the under test summaries by
      #' subtracting the baseline from the under test.
      #' @param var_grp variable group
      compare = function(var_grp) {

        var_grp$compare <-
          list(diff2base = lapply(var_grp$under_test, function(x) {
            x - var_grp$baseline
          }))

        # add grids for to all diff2bases
        lapply(var_grp$compare$diff2base, function(x) x$add_grid())

        var_grp$baseline <- NULL
        var_grp$under_test <- NULL
      },

      #' @description
      #' Create a map plot with country border overlay.
      #' @return A list of map ggplots
      plot = function() {
        create_map_plots(self$var_grp_list,
                         m_options = self$m_options)
      },

      #' @description
      #' Arrange the map plots side by side
      #' @param plotlist List of map ggplots
      arrange_plots = function(plotlist) {
        arrange_map_plots(plotlist, self$m_options)
      },

      #' @field m_options
      #' List of metric options specific to this metric:
      #' - `font_size`: integer, font size of the map plot (default 6)
      #' - `highlight`: vector of strings, indicating which variables
      #' should be highlighted in the report, that is receive a larger plot at the
      #' beginning of report content of the metric.
      #' All variables with a name that contains at least one these strings as a substring,
      #' will not be plotted in the plotgrid (see `num_cols`) but before the plot grid starts.
      #' These plots are allowed to extent to full page width. (default NULL)
      #' - `quantiles`: quantiles used to determine the lower and upper
      #' limits for the values in the map plot (default c(0.05, 0.95))
      #' - `n_breaks`: number of breaks for each arm of the diverging
      #' color scale (default 3)
      #' - `year_subset`: character vector, defines which calendar years the metric considers,
      #' i.e., a data subset that the metric works with; e.g., c("1995", "1996") (default 1991:2000).
      #' - `cell_subset`: character vector, defines which cells to subset (default NULL)
      #' - `num_cols`: integer, number of columns in the plot grid in the report (default 2)
      #' - `var_subheading`: boolean, if TRUE, before plots for a new variable are added to the report, 
      #' a linebreak will be inserted and a subheading
      #' with the name of the variable will be added. Both things are intended to visually seperate
      #' the plots of different variables and to better organize the report,
      #' especially if the metric generates many plots for each variable. (default FALSE)
      #' - `band_subheading`: analogous to var_subheading but for bands (default FALSE)
      m_options = list(
        font_size = 6,
        highlight = NULL,
        quantiles = c(0.05, 0.95),
        year_subset = as.character(1991:2000),
        cell_subset = NULL,
        n_breaks = 3,
        num_cols = 2,
        var_subheading = FALSE,
        band_subheading = FALSE
      ),

      #' @field title
      #' Section header used in the report
      title = "Time Average Maps",

      #' @field description
      #' Description used in the report
      description = "The cell-time values of each variable are averaged over
             time. The difference of unter test outputs to baseline outputs
             is then plotted on a map. \n"
    )
  )

#' @title TimeAvgMapWithAbs
#' @description
#' TimeAvgMapWithAbs metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
TimeAvgMapWithAbs <- # nolint: object_name_linter.
  R6::R6Class(
    "TimeAvgMapWithAbs",
    inherit = TimeAvgMap,
    public = list(
      #' @description
      #' Compare the baseline summary with the under test summaries by
      #' subtracting the baseline from the under test.
      #' @param var_grp variable group
      compare = function(var_grp) {

        var_grp$compare <-
          list(diff2base = lapply(var_grp$under_test, function(x) {
            x - var_grp$baseline
          }))

        # add grids for to all diff2bases
        lapply(var_grp$compare$diff2base, function(x) x$add_grid())
      },

      #' @description
      #' Create a map plot with country border overlay.
      #' @return A list of map ggplots
      plot = function() {
        create_map_plots(self$var_grp_list,
                         self$m_options,
                         colorbar_length = 0.8)
      },

      #' @field m_options
      #' List of metric options specific to this metric:
      #' - `font_size`: integer, font size of the map plot (default 6)
      #' - `highlight`: vector of strings, indicating which variables
      #' should be highlighted in the report, that is receive a larger plot at the 
      #' beginning of report content of the metric. 
      #' All variables with a name that contains at least one these strings as a substring, 
      #' will not be plotted in the plotgrid (see `num_cols`) but before the plot grid starts. 
      #' These plots are allowed to extent to full page width. (default NULL)
      #' - `quantiles`: quantiles used to determine the lower and upper
      #' limits for the values in the map plot (default c(0.05, 0.95))
      #' - `sep_cmp_lims`: logical, if TRUE not all plots of a var_grp
      #' will have the same limits anymore, but the compare plots
      #' have their own separate limits (default TRUE)
      #' - `n_breaks`: number of breaks for each arm of the diverging
      #' color scale (default 3)
      #' - `year_subset`: character vector, defines which calendar years the metric considers,
      #' i.e., a data subset that the metric works with; e.g., c("1995", "1996") (default 1991:2000).
      #' - `cell_subset`: character vector, defines which cells to subset (default NULL)
      #' - `num_cols`: integer, number of columns in the plot grid in the report (default 3)
      #' - `var_subheading`: boolean, if TRUE, before plots for a new variable are added to the report, 
      #' a linebreak will be inserted and a subheading
      #' with the name of the variable will be added. Both things are intended to visually seperate
      #' the plots of different variables and to better organize the report,
      #' especially if the metric generates many plots for each variable. (default FALSE)
      #' - `band_subheading`: analogous to var_subheading but for bands (default FALSE)
      m_options = list(
        font_size = 6,
        highlight = NULL,
        quantiles = c(0.05, 0.95),
        sep_cmp_lims = TRUE,
        year_subset = as.character(1991:2000),
        cell_subset = NULL,
        n_breaks = 3,
        num_cols = 3,
        var_subheading = FALSE,
        band_subheading = FALSE
      ),

      #' @description
      #' Arrange the map plots side by side
      #' @param plotlist List of map ggplots
      arrange_plots = function(plotlist) {
        arrange_map_plots(plotlist, self$m_options)
      },

      #' @field title
      #' Section header used in the report
      title = "Time Average Maps With Absolute Values",

      #' @field description
      #' Description used in the report
      description = "The cell time values are averaged
                     over time and plotted on a map. The difference
                     to the baseline is also plotted. \n"
    )
  )

# ------------ metrics for special outputs -------------------------------------

#' @title GlobSumTimeAvgTablePFT_harvest
#' @description
#' GlobSumTimeAvgTablePFT_harvest metric
#' @export
GlobSumTimeAvgTablePFT_harvest <- # nolint: object_name_linter.
  R6::R6Class(
    "GlobSumTimeAvgTablePFT_harvest",
    inherit = GlobSumTimeAvgTable,
    public = list(
      #' @description
      #' Weigh by cft_frac and then do the same as GlobSumTimeAvgTable
      #' @param data LPJmLDataCalc object to be summarized
      summarize = function(data) {
        cft_frac <- read_file(data$meta$.__enclos_env__$private$.data_dir,
                              "cftfrac",
                              subset = list(year = as.character(data$meta$firstyear:data$meta$lastyear)))
        cft_frac <- subset(cft_frac,
                           band = data$meta$band_names,
                           time = dimnames(data$data)[[2]])
        super$summarize(data * cft_frac)
      },

      #' @field title
      #' Section header used in the report
      title = "Global Sum Time Average Table PFT harvest",

      #' @description
      #' initialize with an extended description
      initialize = function() {
        self$description <- paste0(self$description, " In addition to terr_area
                           the output is also weighted by cftfrac.")
      }
    )
  )

#' @title GlobSumTimeAvgTableFPC
#' @description
#' GlobSumTimeAvgTableFPC metric
#' @export
GlobSumTimeAvgTableFPC <- R6::R6Class( # nolint: object_name_linter.
  "GlobSumTimeAvgTableFPC",
  inherit = GlobSumTimeAvgTable,
  public = list(
    #' @description
    #' Weigh by natural stand fraction and then do the same as GlobSumTimeAvgTable
    #' @param data LPJmLDataCalc object to be summarized
    summarize = function(data) {
      nat_stand_frac <- subset(data, band = "natural stand fraction")
      bands <- data$meta$band_names
      data_s <- subset(data, band = bands[!bands %in% "natural stand fraction"])
      super$summarize(data_s * nat_stand_frac)
    },

    #' @field title
    #' Section header used in the report
    title = "Global Sum Time Average Table FPC",

    #' @description
    #' initialize with an extended description
    initialize = function() {
      self$description <- paste0(self$description, " In addition to terr_area
                           the output is also weighted by the natural stand
                                 fraction.")
    }
  )
)

#' @title GlobSumAnnTimeseriesPFT_harvest
#' @description
#' GlobSumAnnTimeseriesPFT_harvest metric
#' @export
GlobSumAnnTimeseriesPFT_harvest <- # nolint: object_name_linter.
  R6::R6Class(
    "GlobSumAnnTimeseriesPFT_harvest",
    inherit = GlobSumAnnAvgTimeseries,
    public = list(
      #' @description
      #' Weigh by cft_frac and then do the same as GlobSumAnnAvgTimeseries
      #' @param data LPJmLDataCalc object to be summarized
      summarize = function(data) {
        cft_frac <- read_file(data$meta$.__enclos_env__$private$.data_dir,
                              "cftfrac")
        cft_frac <- subset(cft_frac,
                           band = data$meta$band_names,
                           time = dimnames(data$data)[[2]])
        super$summarize(data * cft_frac)
      },

      #' @field title
      #' Section header used in the report
      title = "Global Sum Annual Time Series PFT harvest",

      #' @description
      #' initialize with an extended description
      initialize = function() {
        self$description <- paste0(self$description, " In addition to terr_area
                           the output is also weighted by cftfrac.")
      }
    )
  )

#' @title GlobSumAnnTimeseriesFPC
#' @description
#' GlobSumAnnTimeseriesFPC metric
#' @export
GlobSumAnnTimeseriesFPC <- R6::R6Class( # nolint: object_name_linter.
  "GlobSumAnnTimeseriesFPC",
  inherit = GlobSumAnnAvgTimeseries,
  public = list(
    #' @description
    #' Weigh by natural stand fraction and then do the same as
    #' GlobSumAnnAvgTimeseries
    #' @param data LPJmLDataCalc object to be summarized
    summarize = function(data) {
      nat_stand_frac <- subset(data, band = "natural stand fraction")
      bands <- data$meta$band_names
      data_s <- subset(data, band = bands[!bands %in% "natural stand fraction"])
      super$summarize(data_s * nat_stand_frac)
    },

    #' @field title
    #' Section header used in the report
    title = "Global Sum Annual Time Series FPC",

    #' @description
    #' initialize with an extended description
    initialize = function() {
      self$description <- paste0(self$description, " In addition to terr_area
                           the output is also weighted by the natural stand
                                 fraction.")
    }
  )
)
