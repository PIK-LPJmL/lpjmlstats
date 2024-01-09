#' Benchmark one or several LPJmL runs
#'
#' Function to benchmark one or several under test LPJmL runs
#' against a baseline run.
#'
#' @param bsl_dir Path to directory containing the baseline run.
#' @param ut_dirs List of paths to directories containing the under test run
#' results.
#' @param file_type File type of the raw data to be read in (e.g. ".bin.json")
#' @param settings List that defines for each output which metrics to use.
#'  The list has to have the following structure:
#'  \itemize{
#'  \item \code{var1}: List of metric classes to use for variable \code{var1}
#'  \item \code{var2}: List of metric classes to use for variable \code{var2}
#'  \item ...
#'  }
#' @return List of metric objects which store the results of the evaluations.
#'
#' @md
#' @export
#'
benchmark <-
  function(bsl_dir, ut_dirs, settings, file_type = ".bin.json") {
    paths <- list(bsl_dir = bsl_dir, ut_dirs = ut_dirs, suffix = file_type)

    all_metrics <- initialize_metrics(settings)

    for (var in names(settings)) {
      metric_names_of_var <- sapply(settings[[var]], function(x) x$classname)
      metrics_of_var <- all_metrics[metric_names_of_var]

      retrieve_summaries(metrics_of_var, var, paths)

      compare_summaries(metrics_of_var, var)
    }

    return(all_metrics)
  }

initialize_metrics <- function(settings) {
  metric_classes <- unique(unname(unlist(settings)))

  metric_objs <- list()
  for (m_class in metric_classes) {
    metric_objs[[m_class$classname]] <- m_class$new()
  }

  return(metric_objs)
}

retrieve_summaries <- function(metric_list, var, paths) {
  # the summaries will be retrieved and stored in the metric objects
  # file by file to avoid reading the same file multiple times
  # or having several raw files in memory at the same time

  # function to read raw file and extract all needed summaries of that file
  process_file <- function(metric_list, dir, filename, type) {
    # load potentially large raw dataset into memory
    print(paste0("Read in ", type, " ", var, " ..."))
    raw_data <- read_io(file.path(dir, filename))
    for (metric in metric_list) {
      # allow each metric to capture an individual summary of this raw data
      print(paste0("Summarize with metric ", class(metric)[1], " ..."))
      metric$capture_summary(raw_data, var, type)
    }
    # remove raw data from memory
    rm(raw_data)
    gc()
  }

  filename <- paste0(var, paths$suffix)

  # process baseline file
  process_file(metric_list, paths$bsl_dir, filename, "bsl")

  # process under test files
  for (ut_dir in paths$ut_dirs) {
    process_file(metric_list, ut_dir, filename, "ut")
  }

  # remove memoised cache of aggregate function
  memoise::forget(aggregate)
}

compare_summaries <- function(metric_list, var) {
  for (metric in metric_list) {
    metric$add_comparison(var)
  }
}
