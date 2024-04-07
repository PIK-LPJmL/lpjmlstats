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
#' @param ... additional arguments to be passed to \link{create_pdf_report}
#'
#' @return A benchmark object containing the numerical results of the
#' benchmarking. This data object is basically a list of all metrics
#' used in the benchmarking. See \link{Metric} for the way a metric structures
#' benchmarking results. In addition the benchmark object contains meta
#' information. Of particular importance is the
#' simulation table, which contains the simulation names, paths and
#' the short simulation identifier that are used in the benchmark object.
#'
#' The function \link{get_benchmark_meta_data} can be used to retrieve the meta
#' information.
#'
#' The data structure of the benchmark object is depicted here:
#' \if{html}{
#'   \out{<div style="text-align: center">}\figure{benchmark_obj_struc.png}{options: style="width:500px;max-width:50\%;"}\out{</div>} #nolint
#' }
#'
#' @details
#'
#' In order for the benchmark to work, all the output files specified
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
#' # is recommended. Also, it can make sense to store the benchmark object
#' # for later analysis.
#' BM_data <- benchmark("path_to_baseline_results",
#'                      "path_to_under_test_results",
#'                      author = "anonymous",
#'                      description = "This is a test",
#'                      output_file = "myBenchmark.pdf")
#'
#' # Example 3
#' # Quick benchmarking that only looks at specific outputs with
#' # specific metrics and doesn't generate pdf report.
#' # In addition only the first 10 years are considered
#' # which gives another significant speedup.
#' set_lpjmlstats_settings(year_subset = 1:10)
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
#' set_lpjmlstats_settings(year_subset = NULL) # restore default settings
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
#' }
#'
#' @seealso \code{\link{create_pdf_report}}
#'
#' @md
#'
#' @importFrom dplyr %>%
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
           ...) {

    cat(cli::col_blue("Start the core numerical benchmarking...\n"))

    paths <-
      list(
        baseline_dir = baseline_dir,
        under_test_dirs = under_test_dirs,
        suffix = getOption("lpjmlstats.file_extension")
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

    benchmark <- create_benchmark_object(all_metrics,
                                         baseline_dir,
                                         under_test_dirs,
                                         author,
                                         description,
                                         sim_table)

    cat(cli::col_green("Core numerical benchmarking completed. \n"))

    if (pdf_report) {
      cat(cli::col_blue("Start report generation ...\n"))
      create_pdf_report(benchmark, ...)
      cat(cli::col_green("Report generation completed.\n"))
    }

    return(benchmark)
  }

#' Generate a pdf report from a benchmark object.
#'
#'
#' @param data benchmark data object created by the \code{\link{benchmark}}
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
create_pdf_report <- function(data, output_file = "benchmark.pdf", ...) {
  # this variable is used in the Rmd file
  # it is assigned to the current environment, which will be passed to the .Rmd
  bench_data <- data #nolint

  # copy input Rmd to output and processing dirctory
  path_to_rmd <- system.file("Benchmark_markdown.Rmd", package = "lpjmlstats")
  process_and_out_dir <- dirname(output_file)
  path_to_rmd_copy <- file.path(process_and_out_dir, "Benchmark_markdown.Rmd")
  file.copy(path_to_rmd, path_to_rmd_copy)

  # render markdown
  rmarkdown::render(
    path_to_rmd_copy,
    output_file = basename(output_file),
    # pass over current environment
    envir = environment(),
    output_dir = process_and_out_dir,
    knit_root_dir = process_and_out_dir,
    intermediates_dir = process_and_out_dir,
    ...
  )

  # remove input Rmd
  unlink(path_to_rmd_copy)
}

# ----- utitly functions -----
# Function to create unique sim identifier

create_simulation_table <- function(paths) {
  sim_paths <- c(paths$baseline_dir, unlist(paths$under_test_dirs))
  sim_names <- c()
  lpjml_version <- c()

  for (path in sim_paths) {
    # read any file with a .bin.json extension in the directory
    # and extract the meta data

    # get filename of any file with .bin.json extension in the directory
    file <- list.files(path, pattern = ".bin.json", full.names = TRUE)[1]

    # get relevant meta information from that file
    meta <- lpjmlkit::read_meta(file)
    sim_names <- c(sim_names, meta$sim_name)
    lpjml_version <- c(lpjml_version, meta$source)
  }

  if (length(unique(sim_names)) != length(sim_names)) {
    # use file paths as sim identifier, if sim names are not unique
    # check if file paths are unique
    if (length(unique(sim_paths)) != length(sim_paths)) {
      stop("Neither simulation names nor given simulation paths are unique.
           The Benchmarking cannot differentiate between the simulations.")
    }

    # create identifier from file paths
    sim_identifier <- unlist(create_unique_short_sim_paths(sim_paths))
  } else {
    sim_identifier <- sim_names
  }

  # remove underscores if it doesnt inflict uniqueness
  if (length(sim_identifier) == length(unique(gsub("_", "", sim_identifier)))) {
    sim_identifier <- gsub("_", " ", sim_identifier)
  }

  sim_identifier <- abbreviate(sim_identifier, minlength = 4, method = "both.sides")

  lpjml_version <- gsub("LPJmL C Version", "", lpjml_version)

  # first entry is always the baseline, based on sim_paths struct
  sim_type <- c("baseline", rep("under test", length(paths$under_test_dirs)))

  # create simulation itentification table
  sim_table <-
    tibble::tibble(sim_paths,
                   lpjml_version,
                   sim_names,
                   sim_identifier,
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
      metric_opt <- m_options[[metric_names]]
      for (opt in names(metric_opt)) {
        metric$m_options[[opt]] <- metric_opt[[opt]]
      }
    }
  }
}



# Function applies unit conversions ass specified in package conversion
# table to all metrics
apply_unit_conversion_table <- function(metrics) {
  for (metric in metrics) {
    metric$apply_to_all_lpjml_calcs(function(x) x$apply_unit_conversion_table())
  }
}

# Function to consolidate metrics and meta information in a comprehensive
# benchmark object, storing the result of the benchmarking
create_benchmark_object <- function(metrics,
                                    baseline_dir,
                                    under_test_dirs,
                                    author,
                                    description,
                                    sim_table) {
  attr(metrics, "class") <- "benchmark"
  attr(metrics, "baseline_dir") <- baseline_dir
  attr(metrics, "ut_dir") <- under_test_dirs
  attr(metrics, "author") <- author
  attr(metrics, "description") <- description
  attr(metrics, "sim_table") <- sim_table
  return(metrics)
}

#' Function that returns the meta data of a benchmark object
#' @param benchmark A benchmark object
#' @return A list with the meta data of the benchmark object.
#' The list contains the author, the description and a simulation
#' identification table. The latter is a tibble with the columns
#' sim_paths, lpjml_version, sim_names, sim_identifier and sim_type.
#'
#' @export
get_benchmark_meta_data <- function(benchmark) {
  return(list(author = attr(benchmark, "author"),
              description = attr(benchmark, "description"),
              sim_table = attr(benchmark, "sim_table")))
}


# Function to empty the cache of functions tht read in files used
# throughout the benchmarking
empty_cache <- function() {
  # empty the grid and terrarrea cache
  memoise::forget(read_grid)
  memoise::forget(read_terr_area)
}

# Function to make instances, that is objects of all metric classes
# that the user specified
initialize_metrics <- function(settings) {
  metric_classes <- unique(unname(unlist(settings)))

  metric_objs <- list()
  for (m_class in metric_classes) {
    metric_objs[[m_class$classname]] <- m_class$new()
  }

  return(metric_objs)
}

# Function to create and store the summaries that all the metrics need
retrieve_summaries <-
  function(metric_list, var, paths, sim_table) {
    # the summaries will be retrieved and stored in the metric objects
    # datafile by datafile to avoid reading the same raw data file multiple
    # times or having several raw files in memory at the same time

    # Function to read raw file and extract all needed summaries of that file
    process_file <-
      function(metric_list,
               dir,
               var,
               type,
               suffix,
               sim_table) {

        var_band <- split_variable_and_band(var)

        filename <- paste0(var_band$var_pure, suffix)

        # load potentially large raw dataset into memory
        cat(paste0("Process ", type, " ", cli::col_blue(var), " ...\n"))
        raw_data <- read_in_time_subsetted(dir, filename)
        if (!is.null(var_band$band)) {
          raw_data <- subset(raw_data, band = var_band$band)
        }
        raw_data$set_sim_identifier(
          sim_table$sim_identifier[sim_table$sim_paths == dir][1]
        )
        for (metric in metric_list) {
          # allow each metric to capture an individual summary of this raw data
          # cat(paste0("Summarize with metric ", class(metric)[1], " ...\n")) # nolint
          metric$capture_summary(raw_data, var, type)
        }
        # remove raw data from memory
        rm(raw_data)
        gc()
      }


    # process baseline file
    process_file(metric_list,
                 paths$baseline_dir,
                 var,
                 "baseline",
                 paths$suffix,
                 sim_table)

    # process under test files
    for (ut_dir in paths$under_test_dirs) {
      process_file(metric_list,
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

# Function to read in a file and subset it if a year subset is given
read_in_time_subsetted <- function(dir = dir, filename = filename) {
  if (!is.null(dir))
    path <- file.path(dir, filename)
  else
    path <- filename

  if (is.null(getOption("lpjmlstats.year_subset"))) {
    raw_data <- read_io(path)
  } else {
    year_subset <-  list(year = getOption("lpjmlstats.year_subset"))
    raw_data <-
      read_io(path, subset = year_subset)
  }

  return(raw_data)
}


# Function to create all the comparisons of the metrics by calling the
# internal comparison function of all metrics
compare_summaries <- function(metric_list) {
  for (metric in metric_list) {
    metric$add_comparison()
  }
}
