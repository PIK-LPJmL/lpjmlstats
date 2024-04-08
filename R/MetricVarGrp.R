#' @title Metric super class
#'
#' @description
#' A metric is
#' a) a structured, generic processing pipeline to calculate
#' numerical indicators for sets of lpjml outputs as well as
#' to display these indicators in a report, and
#' b) a structured data container that stores the numerical indicators resulting
#' from a).
#'
#' **a)** A metric defines a procedure to
#'
#' 1. **summarize** a complex, multidimensional LPJmL
#' output in a meaningfull, potentially opinionated way,
#' typically involving the reduction of its time and or space dimension,
#' 2. **compare** how this summary statistic of an output variable changes,
#' when going from an established baseline LPJmL version or configuration to
#' a new version or configuration currently under test,
#' 3. **plot** the results of 1. and 2. as a figure or table and
#' 4. **arrange** these plots in a report,
#' (e.g. styling, side by side arrangement)
#'
#' See \link{benchmark} on how these steps come into play in the
#' benchmarking process.
#'
#'
#' **b)**  The summarized outputs as well as comparisons that are stored
#' a metric are grouped by the different lpjml variables. A so called variable
#' group (var_grp) contains
#' 1. the summary of the baseline output of that variable
#' 2. a list of all summarized under test outputs of that variable
#' 3. a list of compare items (e.g. difference, relative difference). Each
#' compare item is a list of comparisons of the baseline summary with
#' each under test summary using the specific method of that item.
#'
#' All variable groups are stored in the var_grp_list attribute of the metric.
#'
#' As the cornerstone of the benchmarking process, all metrics illuminate
#' the change of LPJmL results from different angles,
#' and should together provide a comprehensive picture of the effects of
#' modifications in code or settings.
#'
#' See \link{GlobSumTimeAvgTable} for a typical example of a metric.
#'
#' @export
#'

Metric <- R6::R6Class( # nolint: cyclocomp_linter object_linter_name
  classname = "Metric",
  public = list(

    #' @description
    #' Pipeline to summarize the raw data.
    #' Will be overwritten by the individual metric subclasses.
    #' @param data Raw data to be summarized
    summarize = function(data) {
      NULL
    },

    #' @description
    #' Pipeline to compare the baseline summary with each under test summary
    #' stored in the metric.
    #' Will be overwritten by the individual metric subclasses.
    #' @param var_grp variable group
    compare = function(var_grp) {
      NULL
    },

    #' @description
    #' Function to plot the results of the metric.
    #' Will be overwritten by the individual metric subclasses.
    #' @param var_grp variable group
    plot = function(var_grp) {
      NULL
    },

    #' @description
    #' Function to arrange all plots of the metric in the respective
    #' section of the report.
    #' Will be overwritten by the individual metric subclasses.
    #' @param var_grp variable group
    arrange_plots = function(var_grp) {
      NULL
    },

    #' @field m_options
    #' List of metric options
    #' Will be overwritten by the individual metric subclasses.
    m_options = list(),

    #' @field var_grp_list
    #' List of variable groups. Each variable group contains the summaries
    #' and the comparisons for one variable.
    var_grp_list = list(),


    # ---- package internal methods ----
    #' @description
    #' !Package internal method!
    #' @param lpjml_calc Raw data to be summarized
    #' @param var Variable name
    #' @param type Type of data ("baseline" or "under_test")
    capture_summary = function(lpjml_calc, var, type) {
      if (!is.null(self$m_options$year_range)) {
        subset_years <- function(lpjml_calc, years) {
          lpjml_calc %>%
            transform("year_month_day") %>%
            subset(year = years) %>%
            transform("time")
        }
        lpjml_calc <-
          keep_units_lpjml_calc(lpjml_calc,
                                function(x) subset_years(x, self$m_options$year_range))
      }
      summary <- self$summarize(lpjml_calc)
      self$store_summary(summary, var, type)
    },

    #' @description
    #' !Package internal method!
    #' Store the summary in the variable group
    #' @param summary Summary to be stored
    #' @param var Variable name
    #' @param type Type of data ("baseline" or "under_test")
    store_summary = function(summary, var, type) {
      # create new variable group if it doesn't exists
      if (is.null(self$var_grp_list[[var]])) {
        var_grp <- VarGrp$new()
        self$var_grp_list[[var]] <- var_grp
      }

      # store data
      if (type == "baseline") {
        self$var_grp_list[[var]]$baseline <- summary
      } else if (type == "under_test") {
        sim_identifier <- summary$get_sim_identifier()
        self$var_grp_list[[var]]$under_test[[sim_identifier]] <-
          summary
      } else {
        stop("type must be either 'baseline' or 'under_test'")
      }
    },

    #' @description
    #' !Package internal method!
    #' Compare and store the comparison in the variable group
    #' @param var Variable name
    add_comparison = function() {
      # get the group for this variable
      for (var_grp in self$var_grp_list) {
        self$compare(var_grp)
      }
    },

    #' @description
    #' !Package internal method!
    #' Apply function to all lpjml_calcs in all eval groups and lists
    #' @param fun Function to apply
    #' @param ... Additional arguments passed to fun
    apply_to_all_lpjml_calcs = function(fun, ...) {
      apply_fun_to_data_only <- function(x, fun, ...) {
        # skip if x is NULL
        if (!is.null(x)) {
          fun(x, ...)
        }
      }
      for (var_grp in self$var_grp_list) {
        apply_fun_to_data_only(var_grp$baseline, fun, ...)
        for (under_test in var_grp$under_test) {
          apply_fun_to_data_only(under_test, fun, ...)
        }
        for (compare in var_grp$compare) {
          for (value in compare) {
            apply_fun_to_data_only(value, fun, ...)
          }
        }
      }
    },

    #' @description
    #' !Package internal method!
    #' Generate the full report content of the metric.
    generate_report_content = function() {
      self$print_metric_header()
      self$print_metric_description()
      plotlist <- self$plot()
      self$arrange_plots(plotlist)
    },

    #' @description
    #' !Package internal method!
    #' Function to print the metric header.
    print_metric_header = function() {
      cat("# ", self$title, "\n")
    },

    #' @description
    #' !Package internal method!
    #' Function to print the metric description.
    print_metric_description = function() {
      cat(self$description, "\n")
    }
  )
)

# An variable group contains all under test and baseline summaries and
# comparisons for one variable.
VarGrp <- # nolint:object_linter_name
  R6::R6Class(
    "VarGrp",
    public = list(
      # Function to retrieve the minimum and maximum of contained data
      # for the different types of data contained
      get_limits = function(type, quantiles = c(0, 1)) {
        get_min_max_of_lpjml_calc <- function(lpjml_calc) {
          return(c(min(lpjml_calc$data), max(lpjml_calc$data)))
        }

        get_min_max_of_lpjml_calc_list <- function(list) {
          vec <- unlist(list)
          max <- -Inf
          min <- Inf
          for (run in vec) {
            limits <- get_min_max_of_lpjml_calc(run)
            if (limits[1] < min) {
              min <- limits[1]
            }
            if (limits[2] > max) {
              max <- limits[2]
            }
          }
          return(c(min, max))
        }

        get_quantiles_of_lpjml_calc_list <- function(list, quantiles) {
          data <- sapply(c(unlist(list)), function(x) x$data) # nolint
          lower_lim <- quantile(data, quantiles[1])
          upper_lim <- quantile(data, quantiles[2])
          return(c(lower_lim, upper_lim))
        }

        limits <- get_quantiles_of_lpjml_calc_list(self[[type]], quantiles)


        return(limits)
      },

      under_test = NULL,
      # list of under test summaries
      baseline = NULL,
      # baseline summary
      compare = NULL  # list of comparisons under test against baseline
    ),

    active = list(
      var_name = function() {
        if (!is.null(self$baseline)) {
          return(self$baseline$meta$variable)
        } else if (!is.null(self$under_test[[1]])) {
          return(self$under_test[[1]]$meta$variable)
        } else if (!is.null(self$compare[[1]])) {
          return(self$compare[[1]]$meta$variable)
        } else {
          stop("No data in var_grp")
        }
      }
    )
  )
