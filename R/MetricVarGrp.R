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
      if (!is.null(self$m_options$year_subset)) {
        subset_years <- function(lpjml_calc, years) {
          lpjml_calc %>% subset_time_pattern(paste0("^", years, "-")) # match e.g. 2023-02-01
        }
        lpjml_calc <-
          keep_units_lpjml_calc(lpjml_calc,
                                function(x) subset_years(x, self$m_options$year_subset))
        if (!is.null(self$m_options$cell_subset))
          lpjml_calc <- subset(lpjml_calc, cell = self$m_options$cell_subset)
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
        sim_ident <- summary$meta$sim_ident
        self$var_grp_list[[var]]$under_test[[sim_ident]] <-
          summary
      } else {
        stop("type must be either 'baseline' or 'under_test'")
      }
    },

    #' @description
    #' !Package internal method!
    #' Compare and store the comparison in all variable groups
    add_comparisons = function() {
      for (var_grp in self$var_grp_list) {
        self$compare(var_grp)
        self$add_compare_meta(var_grp)
      }
    },

    #' @description
    #' !Package internal method!
    #' Add the position of the comparisons within the var_grp to meta
    #' @param var_grp variable group
    add_compare_meta = function(var_grp) {
      for (compare_item_num in seq_along(var_grp$compare)) {
        compare_item_name <- names(var_grp$compare[compare_item_num])
        compare_item <- var_grp$compare[[compare_item_num]]
        for (lpjml_calc in compare_item) {
          lpjml_calc$.meta$.__set_pos_in_var_grp__(
            list(type = "compare",
                 compare_item = compare_item_name)
          )
        }
      }
    },

    #' @description
    #' !Package internal method!
    #' Apply function to all lpjml_calcs in all eval groups and lists
    #' @param fun Function to apply
    #' @param ... Additional arguments passed to fun
    transform_lpjml_calcs = function(fun, ...) {
      for (var_grp in self$var_grp_list) {
        var_grp$transform_lpjml_calcs(fun, ...)
      }
    },

    #' @description
    #' !Package internal method!
    #' Generate the full report content of the metric.
    generate_report_content = function() {
      self$print_metric_header()
      self$print_metric_description()
      self$print_year_subset()
      plotlist <- tryCatch(self$plot())
      try(self$arrange_plots(plotlist))
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
    },

    #' @description
    #' !Package internal method!
    #' Function to print the year_subset metric option.
    print_year_subset = function() {
      if (!is.null(self$m_options$year_subset)) {
        pretty_print_year_subset(self$m_options$year_subset)
      }
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
      get_limits = function(type = "all", quantiles = c(0, 1)) {
        get_quantiles_of_lpjml_calc <- function(lpjml_calc, quantiles) {
          lower_lim <- Inf
          upper_lim <- -Inf
          for (band in dimnames(lpjml_calc$data)[["band"]]) {
            data <- subset(lpjml_calc, band = band)$data
            data[data == 0] <- NA # we assume that a zero means "no exisitng data" for the quantiles
            if (all(is.na(data))) {
              low <- 0
              up <- 0
            } else {
              low <- unlist(quantile(data, quantiles[1], na.rm = TRUE))
              up <- unlist(quantile(data, quantiles[2], na.rm = TRUE))
            }
            if (low < lower_lim) lower_lim <- low
            if (up > upper_lim) upper_lim <- up
          }
          return(c(lower_lim, upper_lim))
        }
        var_grp_lims <- unlist(self$apply_to_lpjml_calcs(get_quantiles_of_lpjml_calc, quantiles))
        return(c(min(var_grp_lims), max(var_grp_lims)))
      },

      get_band_names = function() {
        self$apply_to_any_lpjml_calc(function(x) dimnames(x$data)[["band"]])
      },

      get_var_name = function() {
        self$apply_to_any_lpjml_calc(function(x) x$meta$name)
      },

      # Function applies the function `fun`
      # to the first lpjml_calc it can find in the var_grp and returns the
      # result.
      apply_to_any_lpjml_calc = function(fun, ...) {
        if (!is.null(self$baseline)) {
          return(fun(self$baseline, ...))
        } else if (!is.null(self$under_test[[1]])) {
          return(fun(self$under_test[[1]], ...))
        } else if (!is.null(self$compare[[1]][[1]])) {
          return(fun(self$compare[[1]][[1]], ...))
        } else {
          stop("No data in var_grp")
        }
      },

      # Function applies the function `fun`
      # to all lpjml_calcs of a var_grp
      # (baseline, all under_test, all compare with all items)
      # and saves the results in a non-nested list.
      # Additional arguments ... will be passed to `fun`
      # in addition to each lpjml_calc.
      apply_to_lpjml_calcs = function(fun, ...) {
        add_to_list <- function(list, lpjml_calc) {
          result <- fun(lpjml_calc, ...)
          if (!is.null(attr(result, "listname"))) {
            list[[attr(result, "listname")]] <- result
          } else {
            list <- c(list, list(result))
          }
          return(list)
        }
        list <- list()
        # process baseline with fun if it exists
        if (!is.null(self$baseline)) {
          list <- add_to_list(list, self$baseline)
        }
        # process under_test(s) with fun if any exist
        if (!is.null(self$under_test)) {
          for (lpjml_calc in self$under_test) {
            list <- add_to_list(list, lpjml_calc)
          }
        }
        # process compare(s) with fun if any exist
        if (!is.null(self$compare)) {
          for (item in seq_along(self$compare)) {
            item_name <- names(self$compare[item])
            for (lpjml_calc in self$compare[[item]]) {
              list <- add_to_list(list, lpjml_calc)
            }
          }
        }
        return(list)
      },

      # Function applies the function `fun`
      # to all lpjml_calcs of a var_grp
      # (baseline, all under_test, all compare with all items).
      # `fun` is expected to return lpjml_calc objects.
      # The lpjml_calcs of the var_grp are overwritten by
      # those objects. (They are transformed by `fun`)
      # Additional arguments ... will be passed to `fun`
      # in addition to each lpjml_calc.
      transform_lpjml_calcs = function(fun, ...) {
        fun_skip_null <- function(x, fun, ...) {
          # skip if x is NULL
          if (!is.null(x))
            return(fun(x, ...))
          else
            return(NULL)
        }
        self$baseline <- fun_skip_null(self$baseline, fun, ...)
        for (i in seq_along(self$under_test)) {
          self$under_test[[i]] <- fun_skip_null(self$under_test[[i]], fun, ...)
        }
        for (i in seq_along(self$compare)) {
          compare <- self$compare[[i]]
          for (j in seq_along(compare)) {
            self$compare[[i]][[j]] <-
              fun_skip_null(self$compare[[i]][[j]], fun, ...)
          }
        }
      },

      deep_clone = function(deep = FALSE) {
        new_var_grp <- VarGrp$new()
        new_var_grp$under_test <- lapply(self$under_test, function(x) {
          x$clone(deep = TRUE)
        })
        new_var_grp$compare <- lapply(self$compare, function(comp) {
          lapply(comp, function(x) {
            x$clone(deep = TRUE)
          })
        })
        if (!is.null(self$baseline)) {
          new_var_grp$baseline <- self$baseline$clone(deep = TRUE)
        }
        return(new_var_grp)
      },

      under_test = NULL,
      baseline = NULL,
      compare = NULL
    )
  )

# Utility functions
pretty_print_year_subset <- function(years) {
  numeric_years <- as.numeric(years)
  vec_range <- range(numeric_years)
  cat("Data subset: ")
  if (identical(as.integer(numeric_years), as.integer(vec_range[1]:vec_range[2])))
    cat(vec_range[1], "-", vec_range[2])
  else
    cat(numeric_years)
  cat(" (years)")
  cat("\n\n")
}
