#' Benchmark one or several LPJmL runs
#'
#' Function to benchmark one or several under test LPJmL runs
#' against a baseline run.
#'
#' @param baseline_dir Path to directory containing the baseline run.
#' @param under_test_dirs List of paths to directories containing the under
#' test run results.
#' @param settings List that defines for each output which metrics to use.
#'  The list has to have the following structure:
#'  \itemize{
#'  \item \code{var1} = Vector of metric classes to use for variable \code{var1}
#'  \item \code{var2} = Vector of metric classes to use for variable \code{var2}
#'  \item ...
#'  }
#' @param metric_options List that defines options for the metrics.
#'  The list has to have the following structure:
#'  \itemize{
#'  \item \code{metric1} = List of options for metric \code{metric1}
#'  \item \code{metric2} = List of options for metric \code{metric2}
#'  }
#' @param author Name of the author of the benchmark.
#' @param description Description of the purpose of the benchmark.
#' @param pdf_report Logical, if TRUE a pdf report will be created with
#'  the \link{create_pdf_report} function.
#' @param ilamb_report Logical, if TRUE a basic ILAMB report will be created
#' @param ... additional arguments to be passed to \link{create_pdf_report} like
#'  `output_file` for the directory and filename of the pdf report.
#'
#' @return A benchmarkResult object containing the numerical results of the
#' benchmarking. This data object is basically a list of all metrics
#' used in the benchmarking. See \link{Metric} for the way a metric structures
#' benchmarking results. In addition the benchmarkResult object contains meta
#' information. Of particular importance is the
#' simulation table, which contains the simulation names, paths and
#' the short simulation identifier that are used in the benchmarkResult object.
#'
#' The function \link{get_benchmark_meta_data} can be used to retrieve the meta
#' information.
#'
#' The data structure of the benchmarkResult object is depicted here:
#' \if{html}{
#'   \out{<div style="text-align: center">}\figure{benchmark_obj_struc.png}{options: style="width:500px;max-width:50\%;"}\out{</div>} #nolint
#' }
#'
#' @details
#'
#' In order for the benchmarking to work, all the output files specified
#' in the settings have to be present in the baseline and all under test
#' directories. All output files need to be with ".bin" extension
#' and with meta files of ".bin.json" format. All output paths given to
#' the function need to be distinct. In each output directory there
#' must be a grid and a terr_area file corresponding to the outputs.
#' For each variable the structure of the output files has to be same
#' in each directory (i.e. same cells, same time steps, same bands).
#'
#'
#' The internal benchmarking process is structured as follows:
#' 1. Create simulation table with meta information
#' of all considered simulations and the short simulation identifiers.
#'
#' 2. Retrieve all summaries of outputs from the baseline and under test runs
#' of the variable by applying the summary method of each metric to all lpjml
#' outputs of variables that are designated to be evaluated with this metric,
#' as specified in the settings. The results are organized in
#' variable groups and stored in the var_grp_list attributes of the metrics.
#' See \link{Metric} for details.
#'
#' 3. Add the comparison items to the variable groups, by applying the
#' compare method of each metric to the combination of baseline summary
#' with each under test summary of the variable groups stored in that
#' metric.
#'
#' 4. Apply unit conversions to all data objects of the metrics, as specified
#' in the unit conversion table.
#' See \link{set_lpjmlstats_settings}.
#'
#' @examples
#' \dontrun{
#' # Example 1
#' # Most basic benchmarking with default settings
#' benchmark("path_to_baseline_results", "path_to_under_test_results")
#'
#' # Example 2
#' # Specifying author and description, as well as filename for pdf report
#' # is recommended. Also, it can make sense to store the benchmarkResult object
#' # for later analysis.
#' BM_resu <- benchmark("path_to_baseline_results",
#'                      "path_to_under_test_results",
#'                      author = "anonymous",
#'                      description = "This is a test",
#'                      output_file = "myBenchmark.pdf")
#'
#' saveRDS(BM_resu, "bm_results.rds")
#'
#' # Example 3
#' # Quick benchmarking that only looks at specific outputs with
#' # specific metrics and doesn't generate pdf report.
#' # In addition only the first 10 years are considered
#' # which gives another significant speedup.
#' settings <- list(
#'  vegc = c(GlobSumTimeAvgTable),
#'  soilc = c(GlobSumTimeAvgTable),
#'  # this give an aggregation to a single value for baseline and under test
#'  # and their comparison, displayed in a table
#'  mgpp = c(GlobSumTimeseries),
#'  # this gives a time series for baseline and under test
#'  # displayed as line plots
#'  mnpp = c(TimeAvgMap)
#'  # this gives a time average for baseline and under test
#'  # displayed as maps
#' )
#' BM_data <- benchmark("path_to_baseline_results",
#'                      "path_to_under_test_results",
#'                      settings = settings,
#'                      pdf_report = FALSE)
#'
#' # Example 4
#' # Benchmark soiltemp in addition to default settings
#' # with a special metric
#' settings <- c(default_settings, # use default settings
#'               list(msoiltemp1 = c(GlobAvgTimeAvgTable, TimeAvgMap))
#'               # GlobAvgTimeAvgTable uses a weighted average over space
#'               # instead of the standard weighted sum
#'               )
#' BM_data <- benchmark("path_to_baseline_results",
#'                      "path_to_under_test_results",
#'                      settings = settings)
#'
#' # Example 5
#' # Benchmark multiple under test runs against the baseline
#' BM_data <- benchmark("path_to_baseline_results",
#'                     list("path_to_under_test_results1",
#'                     "path_to_under_test_results2")
#'                     )
#'
#' # Example 6
#' # Benchmark with custom metric options
#' metric_options <- list(
#'   GlobSumTimeAvgTable = list(font_size = 12), # use larger font size in table
#'   TimeAvgMap = list(highlight = "soilc")      # plots a larger map for soilc
#' )
#' BM_data <- benchmark("path_to_baseline_results",
#'                      "path_to_under_test_results",
#'                      metric_options = metric_options)
#'
#' # Example 7
#' # Benchmark only maize harvest
#' # The benchmarking allows to select only specific bands of an output
#  # with the following semantics:
#' settings <- list(`pft_harvest.pft$rainfed maize; irrigated maize`
#'                   = c(GlobSumTimeAvgTable))
#' benchmark("path_to_baseline_results", "path_to_under_test_results",
#'           settings)
#'
#' # Example 8
#' # Benchmark only a single run
#' # It is also possible to benchmark only a single run, by setting the
#' # under_test_dirs argument to NULL
#' benchmark("path_to_baseline_results", NULL)
#'
#' # Example 9
#' # Custom simulation names and ILAMB report
#' # Custom simulation names to under test
#' # and baseline runs can be provided by passing a list.
#' # This only has an effect if custom names are provided
#' # for all simulations.
#' # The maximum number of characters is 7.
#' # In addition an ILAMB report is created, which is a webpage
#' # that will appear as a folder in the driectory specified
#' # by the `output_file` argument.
#' benchmark(list(sim1 = "path_to_baseline_results"),
#'           list(sim2 = "path_to_under_test_results"),
#'           ILAMB_report = TRUE)
#'
#' }
#'
#' @seealso \code{\link{create_pdf_report}}
#'
#' @md
#'
#' @importFrom dplyr %>%
#' @importFrom lpjmlstats read_io_calc
#' @importFrom utils getFromNamespace
#' @export
#'
benchmark <-
  function(baseline_dir,
           under_test_dirs,
           settings = default_settings,
           metric_options = NULL,
           author = "",
           description = "",
           pdf_report = TRUE,
           ilamb_report = FALSE,
           ...) {

    cat(cli::col_blue("Start the core numerical benchmarking...\n"))

    paths <-
      list(
        baseline_dir = baseline_dir,
        under_test_dirs = under_test_dirs,
        suffix = getOption("lpjmlstats.file_extension", default = ".bin.json")
      )

    sim_table <- create_simulation_table(paths)

    all_metrics <- initialize_metrics(settings)

    set_options(all_metrics, metric_options)

    for (var in names(settings)) {
      metric_names_of_var <- vapply(settings[[var]],
                                    function(x) x$classname,
                                    character(1))
      metrics_of_var <- all_metrics[metric_names_of_var]

      retrieve_summaries(metrics_of_var, var, paths, sim_table)

    }

    compare_summaries(all_metrics)

    empty_cache()

    apply_unit_conversion_table(all_metrics)

    # put together metric data and meta data into a benchmarkResult object
    benchmark_result <- create_benchmarkResult_obj(all_metrics,
                                                   baseline_dir,
                                                   under_test_dirs,
                                                   author,
                                                   description,
                                                   sim_table)

    cat(cli::col_green("Core numerical benchmarking completed. \n"))

    if (pdf_report) {
      cat(cli::col_blue("Start report generation ...\n"))
      tryCatch({
        create_pdf_report(benchmark_result, ...)
        cat(cli::col_green("Pdf report generation completed.\n"))
      }, error = function(e) {
        cat(cli::col_red("Error during pdf report generation: "), e$message, "\n")
      }, warning = function(w) {
        cat(cli::col_yellow("Warning during pdf report generation: "), w$message, "\n")
      })
    }

    # create ILAMB report if requested
    if (ilamb_report) {
      cat(cli::col_blue("Start ILAMB evaluation ... \n"))
      create_ilamb_report(baseline_dir, under_test_dirs, sim_table, ...)
    }

    return(benchmark_result)
  }

#' Generate a pdf report from a benchmarkResult object.
#'
#'
#' @param benchmark_result benchmarkResult object created by the \code{\link{benchmark}}
#' function
#' @param output_file file of the output pdf, including filename and directory
#' \code{\link[rmarkdown]{render}}
#' @param ... additional arguments passed to \code{\link[rmarkdown]{render}}
#'
#' @details
#' Each metric has its own section in the report. The content of the section
#' is generated by the plot and plot_arrange function of the metric.
#' The metric results are displayed in the same order as they were specified in
#' the benchmark settings.
#'
#' @examples
#' \dontrun{
#'   create_pdf_report(BM_data, "myBenchmark.pdf")
#' }
#'
#'
#'
#' @md
#' @export
#'
create_pdf_report <- function(benchmark_result,
                              output_file = "benchmark.pdf",
                              ...) {
  # this variable is used in the Rmd file
  # it is assigned to the current environment, which will be passed to the .Rmd
  bench_data <- benchmark_result #nolint

  # copy input Rmd to output and processing dirctory
  path_to_rmd <- system.file("Benchmark_markdown.Rmd", package = "lpjmlstats")
  process_and_out_dir <- dirname(output_file)
  if (!dir.exists(process_and_out_dir))
    stop("Given directory does not exist")
  path_to_rmd_copy <- tempfile(fileext = ".Rmd")
  file.copy(path_to_rmd, path_to_rmd_copy)

  # render markdown
  rmarkdown::render(
    path_to_rmd_copy,
    output_file = basename(output_file),
    # pass over current environment
    envir = environment(),
    output_dir = process_and_out_dir,
    ...
  )

  # remove input Rmd
  unlink(path_to_rmd_copy)
}

# ----- utitily functions -----

# Function to create simulation table
# including unique simulation identifiers
create_simulation_table <- function(paths) {
  sim_paths <- c(paths$baseline_dir, unlist(paths$under_test_dirs))
  sim_names <- c()
  lpjml_version <- c()

  for (path in sim_paths) {
    # read any file with the correct file extension in the directory
    # and extract the meta data

    # get filename of any fitting file
    file <- list.files(path, pattern = paths$suffix, full.names = TRUE)[1]

    # get relevant meta information from that file
    meta <- lpjmlkit::read_meta(file)
    sim_names <- c(sim_names, meta$sim_name)
    lpjml_version <- c(lpjml_version, meta$source)
  }

  if (!is.null(names(paths$baseline_dir)) && !is.null(names(paths$under_test_dirs))) {
    # use user specified names for the simulations if existing
    sim_ident <- c(names(paths$baseline_dir), names(paths$under_test_dirs))
  } else if (length(unique(sim_names)) != length(sim_names)) {
    # use file paths as sim identifier, if sim names are not unique
    # check if file paths are unique
    if (length(unique(sim_paths)) != length(sim_paths)) {
      stop("Neither simulation names nor given simulation paths are unique.
           The Benchmarking cannot differentiate between the simulations.")
    }

    # create identifier from file paths
    sim_ident <- unlist(create_unique_short_sim_paths(sim_paths))
  } else {
    sim_ident <- sim_names
  }

  # remove underscores if it doesnt inflict uniqueness
  if (length(sim_ident) == length(unique(gsub("_", "", sim_ident)))) {
    sim_ident <- gsub("_", " ", sim_ident)
  }

  sim_ident <- abbreviate(sim_ident, minlength = 7, method = "both.sides")

  lpjml_version <- gsub("LPJmL C Version", "", lpjml_version)

  # first entry is always the baseline, based on sim_paths struct
  sim_type <- c("baseline", rep("under test", length(paths$under_test_dirs)))

  # create simulation itentification table
  sim_table <-
    tibble::tibble(sim_paths,
                   lpjml_version,
                   sim_names,
                   sim_ident,
                   sim_type = sim_type)

  return(sim_table)
}


# Function to create unique short versions for the simulation paths
# that can be used to differentiate between
# different simulations used in the benchmarking
create_unique_short_sim_paths <- function(sim_paths) {
  # function to split a path into a vector of strings
  split_path <- function(x) {
    if (dirname(x) == x) x
    else c(basename(x), split_path(dirname(x)))
  }

  # function to collapse the top i most folders of a path given as a a vector
  # of strings to a single string
  collapse_until_i <- function(splitted_paths, i) {
    lapply(splitted_paths, function(x) {
      ifelse(i <= length(x),
             paste(x[1:i] %>% rev, collapse = "/"),
             paste(x[1:length(x)] %>% rev, collapse = "/")) # nolint
    })
  }

  splitted_paths <- lapply(sim_paths, split_path)

  continue <- TRUE
  i <- 1
  while (continue) {
    collapsed_paths <- collapse_until_i(splitted_paths, i)

    # check if all paths are unique
    if (length(unique(collapsed_paths)) == length(collapsed_paths)) {
      # break if unique
      continue <- FALSE
    } else {
      i <- i + 1
    }

    # also break loop if i is very large
    if (i > 100) {
      continue <- FALSE
    }
  }

  return(collapsed_paths)
}

# Function to assign metric options to metric objects
set_options <- function(metrics, m_options) {
  if (!is.null(m_options)) {
    for (metric_names in names(m_options)) {
      metric <- metrics[[metric_names]]
      if (is.null(metric))
        stop(paste0("An option is specified for ", metric_names, " but this metric is not used."))
      metric_opt <- m_options[[metric_names]]
      for (opt in names(metric_opt)) {
        if (!opt %in% names(metric$m_options))
          stop(paste0("The option ", opt, " does not exist for the metric ", metric_names))
        metric$m_options[[opt]] <- metric_opt[[opt]]
      }
    }
  }
}



# Function applies unit conversions as specified in package conversion
# table to all metrics
apply_unit_conversion_table <- function(metrics) {
  for (metric in metrics) {
    metric$transform_lpjml_calcs(function(x) x$apply_unit_conversion_table())
  }
}

# Function to consolidate metrics and meta information in a comprehensive
# benchmark results object, storing the result of the benchmarking
create_benchmarkResult_obj <- function(metrics, # nolint: object_name_linter.
                                       baseline_dir,
                                       under_test_dirs,
                                       author,
                                       description,
                                       sim_table) {
  attr(metrics, "class") <- "benchmarkResult"
  attr(metrics, "baseline_dir") <- baseline_dir
  attr(metrics, "ut_dir") <- under_test_dirs
  attr(metrics, "author") <- author
  attr(metrics, "description") <- description
  attr(metrics, "sim_table") <- sim_table

  metrics <- escape_latex_special_chars_in_meta(metrics)
  return(metrics)
}

# Function to escape all latex special characters
escape_latex_special_chars <- function(string) {
  getFromNamespace("escape_latex", "knitr")(string) # nolint
}

# Function to escape latex special characters in description and author as well metric descriptions
escape_latex_special_chars_in_meta <- function(metrics) {
  attr(metrics, "author") <- escape_latex_special_chars(attr(metrics, "author"))
  attr(metrics, "description") <- escape_latex_special_chars(attr(metrics, "description"))
  return(metrics)
}

#' Function that returns the meta data of a benchmarkResult object
#' @param benchmark_result A benchmarkResult object
#' @return A list with the meta data of the benchmarkResult object.
#' The list contains the author, the description and a simulation
#' identification table. The latter is a tibble with the columns
#' sim_paths, lpjml_version, sim_names, sim_ident and sim_type.
#'
#' @export
get_benchmark_meta_data <- function(benchmark_result) {
  return(list(author = attr(benchmark_result, "author"),
              description = attr(benchmark_result, "description"),
              sim_table = attr(benchmark_result, "sim_table")))
}


# Function to empty the cache of functions tht read in files used
# throughout the benchmarking
empty_cache <- function() {
  memoise::forget(read_file)
}

# Function to make instances, that is objects of all metric classes
# that the user specified
initialize_metrics <- function(settings) {
  metric_classes <- unique(unname(unlist(settings)))

  metric_objs <- list()
  for (m_class in metric_classes) {
    metric_objs[[m_class$classname]] <- m_class$new()
  }

  metric_objs <- reorder_metrics(metric_objs)

  return(metric_objs)
}

# Function to reorder the metrics based on user-specified prioritization vector
# with regex matching
reorder_metrics <- function(all_metrics) {
  priority_patterns <- getOption("lpjmlstats.metrics_at_start")

  # Initialize an empty vector to store indices for metrics matching the
  # priority patterns
  priority_indices <- integer(0)

  # Loop through each pattern and find indices of names in all_metrics that match
  for (pattern in priority_patterns) {
    matched_indices <- grep(pattern, names(all_metrics), ignore.case = TRUE)
    # Append matched indices while avoiding duplicates
    priority_indices <- unique(c(priority_indices, matched_indices))
  }

  # Find indices of all_metrics not covered by the priority patterns
  non_priority_indices <- setdiff(seq_along(all_metrics), priority_indices)

  # Concatenate the indices to create the new order
  new_order <- c(priority_indices, non_priority_indices)

  # Subset all_metrics based on new order
  ordered_metrics <- all_metrics[new_order]
  return(ordered_metrics)
}

# Function to create and store the summaries that all the metrics need
retrieve_summaries <-
  function(metrics_of_var, var, paths, sim_table) {
    # the summaries will be retrieved and stored in the metric objects
    # datafile by datafile to avoid reading the same raw data file multiple
    # times or having several raw files in memory at the same time

    # Function to read raw file, add some meta information
    # and capture all needed summaries of that file
    process_file <-
      function(metrics_of_var,
               dir,
               var,
               type,
               suffix,
               sim_table) {

        # get varname, bandname and filename
        var_band <- split_variable_and_band(var)
        filename <- paste0(var_band$var_pure, suffix)

        # load potentially large raw dataset into memory
        cat(paste0("Process ", type, " ", cli::col_blue(var), " ...\n"))
        raw_data <- read_in_time_subsetted(dir, filename, metrics_of_var)
        raw_data$add_grid()
        if (!is.null(var_band$band)) {
          raw_data <- subset(raw_data, band = var_band$band)
        }

        # add some context of the data object to the meta
        raw_data$.meta$.__set_sim_ident__(
          sim_table$sim_ident[sim_table$sim_paths == dir][1]
        )
        raw_data$.meta$.__set_pos_in_var_grp__(list(type = type))

        # allow each metric to capture an individual summary of this raw data
        for (metric in metrics_of_var) {
          metric$capture_summary(raw_data, var, type)
        }

        # remove raw data from memory
        rm(raw_data)
        gc()
      }


    # process baseline file
    process_file(metrics_of_var,
                 unlist(paths$baseline_dir),
                 var,
                 "baseline",
                 paths$suffix,
                 sim_table)

    # process under test files
    for (ut_dir in paths$under_test_dirs) {
      process_file(metrics_of_var,
                   ut_dir,
                   var,
                   "under_test",
                   paths$suffix,
                   sim_table)
    }

    # remove memoised cache of aggregate function
    memoise::forget(aggregate)
  }

# Function to split a variable into its pure name and the given bands
split_variable_and_band <- function(var) {
  # extract the band and variable name from the variable string
  # get rid of newlines and spaces
  var <- gsub("\n", "", var)
  var <- stringr::str_replace_all(var, ";\\s*", ";")
  # check if bands are given
  if (stringr::str_detect(var, "\\$")) {
    # split the variable string into the variable name and band string
    band <- stringr::str_split_1(var, "\\$")[2]
    # split the band string into a vector of bands
    if (stringr::str_detect(band, ";")) {
      band <- stringr::str_split_1(band, ";")
    }
    # get the pure variable name
    var_pure <- stringr::str_split_1(var, "\\$")[1]
  } else {
    var_pure <- var
    band <- NULL
  }
  return(list(var_pure = var_pure, band = band))
}

# Function to read in a file and subset it based on year_subsets of metrics
read_in_time_subsetted <- function(dir, filename, metrics_of_var) {
  # get the smallest and largest year that need to be read to fulfill needs of all metrics
  min_year <- Inf
  max_year <- -Inf
  for (metric in metrics_of_var) {
    years <- as.numeric(metric$m_options$year_subset)
    if (min(years) < min_year) min_year <- min(years)
    if (max(years) > max_year) max_year <- max(years)
  }

  # add directory if provided
  if (!is.null(dir))
    path <- file.path(dir, filename)
  else
    path <- filename

  # read the data
  raw_data <- read_io_calc(path, subset = list(year = as.character(min_year:max_year)))

  return(raw_data)
}


# Function to create all the comparisons of the metrics by calling the
# internal comparison function of all metrics
compare_summaries <- function(metric_list) {
  for (metric in metric_list) {
    metric$add_comparisons()
  }
}

#' Function to create a pdf with a table with literature values
#' @export
#' @param output_file filename of the output pdf, can include directory
#' @param ... additional parameters passed to rmarkdown::render
create_literature_pdf <- function(output_file = "literature_values.pdf", ...) {
  path_to_rmd <- system.file("Literature_table.Rmd", package = "lpjmlstats")
  dir <- dirname(output_file)
  filename <- basename(output_file)
  path_to_rmd_copy <- tempfile(fileext = ".Rmd")
  file.copy(path_to_rmd, path_to_rmd_copy)
  if (!dir.exists(dir))
    stop("Given directory does not exist")

  # render markdown
  rmarkdown::render(
    path_to_rmd_copy,
    output_file = filename,
    output_dir = dir,
    ...
  )

  unlink(path_to_rmd_copy)
}
