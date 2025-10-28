#' Validate input arguments for get_plot function
#'
#' Internal helper function that validates the input arguments passed to
#' \code{get_plot}. Checks that the benchmark object is valid, and that
#' metric, variables, and data_only arguments have the correct types and values.
#'
#' @param bm Benchmark result object (must be a non-empty list)
#' @param metric Character vector of metric names or NULL
#' @param variables Character vector of variable names or NULL
#' @param data_only Logical flag indicating whether to return data or plots
#'
#' @return NULL (function throws errors if validation fails)
#'
#' @noRd
validate_inputs <- function(bm, metric, variables, data_only) {
  # Validate benchmark object
  if (is.null(bm) || !is.list(bm) || length(bm) == 0) {
    stop("Argument 'bm' must be a non-empty benchmark object (list).")
  }

  # Type checks
  if (!is.null(metric) && !is.character(metric)) {
    stop("Argument 'metric' must be a character vector containing strings.")
  }
  if (!is.null(variables) && !is.character(variables)) {
    stop("Argument 'variables' must be a character vector containing strings.")
  }
  if (!is.logical(data_only) || length(data_only) != 1 || is.na(data_only)) {
    stop("Argument 'data_only' must be a single logical value (TRUE or FALSE).")
  }

  # Check for empty vectors
  if (!is.null(metric) && length(metric) == 0) {
    stop("Argument 'metric' cannot be an empty character vector.")
  }
  if (!is.null(variables) && length(variables) == 0) {
    stop("Argument 'variables' cannot be an empty character vector.")
  }
}

#' Clean and normalize character vectors
#'
#' Internal helper function that removes whitespace, NA values, and empty
#' strings from a character vector, then returns unique values.
#'
#' @param x Character vector to clean, or NULL
#' @param arg_name Name of the argument (used in error messages)
#'
#' @return Cleaned character vector with unique values, or NULL if input is NULL
#'
#' @noRd
clean_char_vector <- function(x, arg_name) {
  if (is.null(x)) return(NULL)

  x <- trimws(x)
  x <- x[!is.na(x) & x != ""]

  if (length(x) == 0) {
    stop(paste0("Argument '", arg_name, "' contains only NA or empty strings."))
  }

  unique(x)
}

#' Validate metric and get variable list
#'
#' Internal helper function that validates a metric exists in the benchmark
#' object and has the required structure. Returns the list of variables
#' available for that metric.
#'
#' @param bm Benchmark result object
#' @param metric Character string with the name of a single metric
#'
#' @return Character vector of variable names available for the metric
#'
#' @noRd
validate_metric <- function(bm, metric) {
  if (is.null(bm[[metric]])) {
    stop(paste0(
      "Metric '", metric, "' not found in benchmark object. ",
      "Available metrics are: ", paste(names(bm), collapse = ", ")
    ))
  }

  if (is.null(bm[[metric]]$var_grp_list)) {
    stop(paste0(
      "Metric '", metric, "' does not contain a valid 'var_grp_list' component."
    ))
  }

  if (!is.list(bm[[metric]]$var_grp_list) ||
        is.null(names(bm[[metric]]$var_grp_list))) {
    stop(paste0(
      "Metric '", metric,
      "' has an invalid 'var_grp_list' structure (must be a named list)."
    ))
  }

  names(bm[[metric]]$var_grp_list)
}

#' Get valid variable indices
#'
#' Internal helper function that identifies which requested variables are
#' available in the metric's variable list and returns their indices.
#' Issues warnings for missing variables.
#'
#' @param available_vars Character vector of available variable names
#' @param requested_vars Character vector of requested variable names
#' @param metric Character string with metric name (used in error messages)
#'
#' @return Integer vector of indices for valid variables
#'
#' @noRd
get_variable_indices <- function(available_vars, requested_vars, metric) {
  missing_vars <- requested_vars[!requested_vars %in% available_vars]
  if (length(missing_vars) > 0) {
    warning(paste0(
      "Variable(s) not found in metric '", metric, "': ",
      paste(missing_vars, collapse = ", "), ". ",
      "Available variables are: ", paste(available_vars, collapse = ", ")
    ))
  }

  var_index <- which(available_vars %in% requested_vars)
  if (length(var_index) == 0) {
    stop(paste0(
      "None of the specified variables were found in metric '", metric, "'. ",
      "Available variables are: ", paste(available_vars, collapse = ", ")
    ))
  }

  var_index
}

#' Generate and validate plots from benchmark metric
#'
#' Internal helper function that calls the plot method on a benchmark metric
#' and validates the returned plot list. Assigns variable names to the plots.
#'
#' @param bm Benchmark result object
#' @param metric Character string with the name of a single metric
#' @param available_vars Character vector of variable names for the metric
#'
#' @return Named list of plot objects
#'
#' @noRd
generate_plots <- function(bm, metric, available_vars) {
  plots <- tryCatch({
    bm[[metric]]$plot()
  }, error = function(e) {
    stop(paste0("Failed to generate plots for metric '", metric, "': ", conditionMessage(e)))
  })

  if (is.null(plots)) {
    stop(paste0("Plot generation for metric '", metric, "' returned NULL."))
  }
  if (!is.list(plots)) {
    stop(paste0("Plot generation for metric '", metric, "' did not return a list."))
  }

  if (length(plots) != length(available_vars)) {
    warning(paste0(
      "Number of generated plots (", length(plots),
      ") does not match number of variables (", length(available_vars), ")."
    ))
  }

  names(plots) <- available_vars[seq_along(plots)]
  plots
}

#' Extract data from ggplot objects
#'
#' Internal helper function that extracts the underlying data from a ggplot
#' object. Returns the plot object as-is if it's not a ggplot.
#'
#' @param plot_obj A plot object (typically ggplot)
#'
#' @return Data frame with plot data (if ggplot), or the original object
#'
#' @noRd
extract_plot_data <- function(plot_obj) {
  tryCatch({
    if (inherits(plot_obj, "ggplot")) {
      return(plot_obj$data)
    } else {
      warning(paste0(
        "Plot object is not a ggplot object (class: ",
        paste(class(plot_obj), collapse = ", "),
        "). Returning the object as-is."
      ))
      return(plot_obj)
    }
  }, error = function(e) {
    warning(paste0("Failed to extract data from plot: ", conditionMessage(e)))
    return(NULL)
  })
}

#' Extract plots from a benchmarkResult object
#'
#' Retrieves plots or underlying plot data from a benchmarkResult object
#' for specific metrics and variables.
#'
#' @param bm A benchmarkResult object created by the \code{\link{benchmark}}
#'   function.
#' @param metric Character vector specifying which metric(s) to retrieve plots
#'   for. If \code{NULL}, plots for all available metrics will be returned.
#'   Examples: \code{"TimeAvgMap"}, \code{"GlobSumTimeseries"}.
#' @param variables Character vector specifying which variable(s) to retrieve
#'   plots for. If \code{NULL}, plots for all variables associated with the
#'   specified metric(s) will be returned. Variable names should match those
#'   used in the benchmark settings (e.g., \code{"vegc"}, \code{"soilc"}).
#' @param data_only Logical. If \code{TRUE}, returns the underlying data used
#'   to generate the plots instead of the plot objects themselves. Default is
#'   \code{FALSE}.
#'
#' @return
#' \itemize{
#'   \item If a single metric is specified: A list of plot objects (or data
#'     frames if \code{data_only = TRUE}) for the requested variables.
#'   \item If multiple metrics are specified: A named list where each element
#'     corresponds to a metric and contains a list of plots (or data frames)
#'     for that metric's variables.
#' }
#'
#' @details
#' This function provides a convenient way to extract and inspect individual
#' plots from a benchmarkResult object. This is useful when you want to:
#' \itemize{
#'   \item Display specific plots interactively
#'   \item Modify plot aesthetics
#'   \item Export plots individually
#'   \item Access the underlying data for custom visualizations
#' }
#'
#' The function validates all inputs and will issue warnings or errors if:
#' \itemize{
#'   \item The specified metric does not exist in the benchmark results
#'   \item The specified variables are not available for the given metric
#'   \item Invalid argument types are provided
#' }
#'
#' @examples
#' \dontrun{
#' # Create a benchmark result
#' bm_result <- benchmark("path_to_baseline", "path_to_under_test")
#'
#' # Get all plots for a specific metric
#' plots <- get_plot(bm_result, metric = "TimeAvgMap")
#'
#' # Get plots for specific variables from a metric
#' vegc_plot <- get_plot(bm_result, metric = "TimeAvgMap", variables = "vegc")
#'
#' # Get plots for multiple metrics
#' plots <- get_plot(bm_result, metric = c("TimeAvgMap", "GlobSumTimeseries"))
#'
#' # Get underlying data instead of plots
#' plot_data <- get_plot(bm_result, metric = "TimeAvgMap",
#'                       variables = "vegc", data_only = TRUE)
#'
#' # Display a specific plot
#' plots <- get_plot(bm_result, metric = "TimeAvgMap", variables = "vegc")
#' print(plots[[1]])
#' }
#'
#' @seealso \code{\link{benchmark}}, \code{\link{create_pdf_report}}
#'
#' @md
#' @export
#'
get_plot <- function(bm, metric = NULL, variables = NULL, data_only = FALSE) {
  # Validate inputs
  validate_inputs(bm, metric, variables, data_only)

  # Clean inputs
  metric <- clean_char_vector(metric, "metric")
  variables <- clean_char_vector(variables, "variables")

  # Default to all metrics if none specified
  if (is.null(metric)) {
    warning("No metric specified. Returning plots for all available metrics.")
    metric <- names(bm)
  }

  # Handle multiple metrics
  if (length(metric) > 1) {
    plots <- lapply(metric, function(m) {
      tryCatch({
        get_plot(bm, m, variables, data_only = data_only)
      }, error = function(e) {
        warning(paste0("Failed to get plots for metric '", m, "': ", conditionMessage(e)))
        NULL
      })
    })
    names(plots) <- metric
    plots <- Filter(Negate(is.null), plots)

    if (length(plots) == 0) {
      stop("Failed to retrieve plots for all specified metrics.")
    }
    return(plots)
  }

  # Single metric case
  available_vars <- validate_metric(bm, metric)

  # Default to all variables if none specified
  if (is.null(variables)) {
    warning("No variables specified. Returning all available variables.")
    variables <- available_vars
  }

  # Get valid variable indices
  var_index <- get_variable_indices(available_vars, variables, metric)

  # Generate plots
  plots <- generate_plots(bm, metric, available_vars)

  # Extract data if requested
  if (data_only) {
    return(lapply(plots[var_index], extract_plot_data))
  }

  plots[var_index]
}
