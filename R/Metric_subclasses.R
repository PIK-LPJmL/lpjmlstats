# ------ reduction of space and time dimensions ------------------------------

#' @title GlobSumTimeAvgTable
#' @description
#' GlobSumTimeAvgTable metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
GlobSumTimeAvgTable <- # nolint:object_linter_name
  R6::R6Class(
    "GlobSumTimeAvgTable",
    inherit = Metric,
    public = list(
      #' @description
      #' First take global weighted sum, then average over all time steps
      #' of the simulation period. The result is a scalar for each band.
      #' @param data LpjmlDataCalc object to be summarized
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
            `diff to baseline` = lapply(var_grp$under_test, function(x) {
              x - var_grp$baseline
            }),
            `diff \\%` = lapply(var_grp$under_test, function(x) {
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
      #' List of metric options specific to this metric
      #' - `font_size`: integer, font size of the table
      #' - `name_trunc`: integer, number of characters to display in the table
      #' band names
      #' - `decimal_places`: integer, number of decimal places to display
      #'
      #' band names
      m_options = list(
        font_size = 8,
        name_trunc = 1,
        decimal_places = 3
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
GlobAvgTimeAvgTable <- R6::R6Class( # nolint:object_linter_name
  "GlobAvgTimeAvgTable",
  inherit = GlobSumTimeAvgTable,
  public = list(
    #' @description
    #' First take global weighted mean, then average over all time steps.
    #' @param data LpjmlDataCalc object to be summarized
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
GlobSumTimeseries <- R6::R6Class( # nolint:object_linter_name
  "GlobSumTimeseries",
  inherit = Metric,
  public = list(
    #' @description
    #' Take a global weighted sum of the output.
    #' @param data LpjmlDataCalc object to be summarized
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
      arrange_timeseries_plots(plotlist)
    },

    #' @field m_options
    #' List of metric options specific to this metric
    #' - `font_size` integer, font size of the table
    #' - `name_trunc` integer, indicating when to truncate the band names
    #' band names
    m_options = list(font_size = 7,
                     name_trunc = 1),

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
GlobAvgTimeseries <- R6::R6Class( # nolint:object_linter_name
  "GlobAvgTimeseries",
  inherit = GlobSumTimeseries,
  public = list(
    #' @description
    #' Take the global weighted mean
    #' over the cells.
    #' @param data LpjmlDataCalc object to be summarized
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
GlobSumAnnAvgTimeseries <- # nolint:object_linter_name
  R6::R6Class(
    "GlobSumAnnAvgTimeseries",
    inherit = GlobSumTimeseries,
    public = list(
      #' @description
      #' Take the mean for each year and then the global weighted sum
      #' over the cells.
      #' @param data LpjmlDataCalc object to be summarized
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
GlobAvgAnnAvgTimeseries <- R6::R6Class( # nolint:object_linter_name
  "GlobAvgAnnAvgTimeseries",
  inherit = GlobSumTimeseries,
  public = list(
    #' @description
    #' Take the mean for each year and then the global weighted mean
    #' over the cells.
    #' @param data LpjmlDataCalc object to be summarized
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



# ------- reduction of time dimension -----------------------------------------

#' @title TimeAvgMap
#' @description
#' TimeAvgMap metric.
#' See \link{Metric} for the documentation of metrics in general.
#' @export
TimeAvgMap <- # nolint:object_linter_name
  R6::R6Class(
    "TimeAvgMap",
    inherit = Metric,
    public = list(
      #' @description
      #' Take the mean over the simulation period for each cell.
      #' @param data LpjmlDataCalc object to be summarized
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
          list(difference = lapply(var_grp$under_test, function(x) {
            x - var_grp$baseline
          }))

        # add grids for to all differences
        lapply(var_grp$compare$difference, function(x) x$add_grid())
      },

      #' @description
      #' Create a map plot with country border overlay.
      #' @return A list of map ggplots
      plot = function() {
        # get identifier of baseline run
        baseline_ident <- self$var_grp_list[[1]]$baseline$get_sim_identifier()
        # describe calculation that was applied to the data
        mod_descr <- paste0("- ", baseline_ident)
        create_map_plots(
          self$var_grp_list,
          self$m_options,
          mod_descr
        )
      },

      #' @description
      #' Arrange the map plots side by side
      #' @param plotlist List of map ggplots
      arrange_plots = function(plotlist) {
        arrange_map_plots(plotlist, self$m_options)
      },

      #' @field m_options
      #' List of metric options specific to this metric
      #' - `m_options$font_size` integer, font size of the map plot
      #' - `m_options$name_trunc` integer, indicating when to truncate the
      #' band names
      #' - `m_options$highlight` vector of strings, indicating which variables
      #' should receive a larger full width plot
      #'
      m_options = list(
        font_size = 7,
        name_trunc = 1,
        highlight = NULL,
        quantiles = c(0.05, 0.95)
      ),

      #' @field title
      #' Section header used in the report
      title = "Time Average Maps",

      #' @field description
      #' Description used in the report
      description = "The cell-time values of each variable are averaged over
             time. The difference of unter test outputs  to baseline outputs
             is then plotted on a map. \n"
    )
  )



# ------------ metrics for special outputs -------------------------------------

#' @title GlobSumTimeAvgTablePFT_harvest
#' @description
#' GlobSumTimeAvgTablePFT_harvest metric
#' @export
GlobSumTimeAvgTablePFT_harvest <- # nolint:object_linter_name
  R6::R6Class(
    "GlobSumTimeAvgTablePFT_harvest",
    inherit = GlobSumTimeAvgTable,
    public = list(
      #' @description
      #' Weigh by cft_frac and then do the same as GlobSumTimeAvgTable
      #' @param data LpjmlDataCalc object to be summarized
      summarize = function(data) {
        cft_frac <- read_cft_frac(data$meta$.__enclos_env__$private$.data_dir)
        cft_frac <- subset(cft_frac, band = data$meta$band_names)
        super$summarize(data * cft_frac)
      },

      #' @field title
      #' Section header used in the report
      title = "Global Sum Time Average Table PFT harvest",

      #' @description
      #' initialize with an extended description
      initialize = function() {
        self$description <- paste0(self$description, " In addition to terr_area
                           the output is also weighted by cft_frac.")
      }
    )
  )

#' @title GlobSumTimeAvgTableFPC
#' @description
#' GlobSumTimeAvgTableFPC metric
#' @export
GlobSumTimeAvgTableFPC <- R6::R6Class( # nolint:object_linter_name
  "GlobSumTimeAvgTableFPC",
  inherit = GlobSumTimeAvgTable,
  public = list(
    #' @description
    #' Weigh by natural stand fraction and then do the same as GlobSumTimeAvgTable
    #' @param data LpjmlDataCalc object to be summarized
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
GlobSumAnnTimeseriesPFT_harvest <- # nolint:object_linter_name
  R6::R6Class(
    "GlobSumAnnTimeseriesPFT_harvest",
    inherit = GlobSumAnnAvgTimeseries,
    public = list(
      #' @description
      #' Weigh by cft_frac and then do the same as GlobSumAnnAvgTimeseries
      #' @param data LpjmlDataCalc object to be summarized
      summarize = function(data) {
        cft_frac <- read_cft_frac(data$meta$.__enclos_env__$private$.data_dir)
        cft_frac <- subset(cft_frac, band = data$meta$band_names)
        super$summarize(data * cft_frac)
      },

      #' @field title
      #' Section header used in the report
      title = "Global Sum Annual Time Series PFT harvest",

      #' @description
      #' initialize with an extended description
      initialize = function() {
        self$description <- paste0(self$description, " In addition to terr_area
                           the output is also weighted by cft_frac.")
      }
    )
  )

#' @title GlobSumAnnTimeseriesFPC
#' @description
#' GlobSumAnnTimeseriesFPC metric
#' @export
GlobSumAnnTimeseriesFPC <- R6::R6Class( # nolint:object_linter_name
  "GlobSumAnnTimeseriesFPC",
  inherit = GlobSumAnnAvgTimeseries,
  public = list(
    #' @description
    #' Weigh by natural stand fraction and then do the same as
    #' GlobSumAnnAvgTimeseries
    #' @param data LpjmlDataCalc object to be summarized
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
