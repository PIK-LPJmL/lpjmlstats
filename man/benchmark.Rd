% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark.R
\name{benchmark}
\alias{benchmark}
\title{Benchmark one or several LPJmL runs}
\usage{
benchmark(
  baseline_dir,
  under_test_dirs,
  settings = default_settings,
  metric_options = NULL,
  author = "",
  description = "",
  pdf_report = TRUE,
  ...
)
}
\arguments{
\item{baseline_dir}{Path to directory containing the baseline run.}

\item{under_test_dirs}{List of paths to directories containing the under
test run results.}

\item{settings}{List that defines for each output which metrics to use.
The list has to have the following structure:
\itemize{
\item \code{var1} = Vector of metric classes to use for variable \code{var1}
\item \code{var2} = Vector of metric classes to use for variable \code{var2}
\item ...
}}

\item{metric_options}{List that defines options for the metrics.
The list has to have the following structure:
\itemize{
\item \code{metric1} = List of options for metric \code{metric1}
\item \code{metric2} = List of options for metric \code{metric2}
}}

\item{author}{Name of the author of the benchmark.}

\item{description}{Description of the purpose of the benchmark.}

\item{pdf_report}{Logical, if TRUE a pdf report will be created with
the \link{create_pdf_report} function.}

\item{...}{additional arguments to be passed to \link{create_pdf_report}}
}
\value{
A benchmarkResult object containing the numerical results of the
benchmarking. This data object is basically a list of all metrics
used in the benchmarking. See \link{Metric} for the way a metric structures
benchmarking results. In addition the benchmarkResult object contains meta
information. Of particular importance is the
simulation table, which contains the simulation names, paths and
the short simulation identifier that are used in the benchmarkResult object.

The function \link{get_benchmark_meta_data} can be used to retrieve the meta
information.

The data structure of the benchmarkResult object is depicted here:
\if{html}{
  \out{<div style="text-align: center">}\figure{benchmark_obj_struc.png}{options: style="width:500px;max-width:50\%;"}\out{</div>} #nolint
}
}
\description{
Function to benchmark one or several under test LPJmL runs
against a baseline run.
}
\details{
In order for the benchmarking to work, all the output files specified
in the settings have to be present in the baseline and all under test
directories. All output files need to be with ".bin" extension
and with meta files of ".bin.json" format. All output paths given to
the function need to be distinct. In each output directory there
must be a grid and a terr_area file corresponding to the outputs.
For each variable the structure of the output files has to be same
in each directory (i.e. same cells, same time steps, same bands).

The internal benchmarking process is structured as follows:
\enumerate{
\item Create simulation table with meta information
of all considered simulations and the short simulation identifiers.
\item Retrieve all summaries of outputs from the baseline and under test runs
of the variable by applying the summary method of each metric to all lpjml
outputs of variables that are designated to be evaluated with this metric,
as specified in the settings. The results are organized in
variable groups and stored in the var_grp_list attributes of the metrics.
See \link{Metric} for details.
\item Add the comparison items to the variable groups, by applying the
compare method of each metric to the combination of baseline summary
with each under test summary of the variable groups stored in that
metric.
\item Apply unit conversions to all data objects of the metrics, as specified
in the unit conversion table.
See \link{set_lpjmlstats_settings}.
}
}
\examples{
\dontrun{
# Example 1
# Most basic benchmarking with default settings
benchmark("path_to_baseline_results", "path_to_under_test_results")

# Example 2
# Specifying author and description, as well as filename for pdf report
# is recommended. Also, it can make sense to store the benchmarkResult object
# for later analysis.
BM_resu <- benchmark("path_to_baseline_results",
                     "path_to_under_test_results",
                     author = "anonymous",
                     description = "This is a test",
                     output_file = "myBenchmark.pdf")

saveRDS(BM_resu, "bm_results.rds")

# Example 3
# Quick benchmarking that only looks at specific outputs with
# specific metrics and doesn't generate pdf report.
# In addition only the first 10 years are considered
# which gives another significant speedup.
settings <- list(
 vegc = c(GlobSumTimeAvgTable),
 soilc = c(GlobSumTimeAvgTable),
 # this give an aggregation to a single value for baseline and under test
 # and their comparison, displayed in a table
 mgpp = c(GlobSumTimeseries),
 # this gives a time series for baseline and under test
 # displayed as line plots
 mnpp = c(TimeAvgMap)
 # this gives a time average for baseline and under test
 # displayed as maps
)
BM_data <- benchmark("path_to_baseline_results",
                     "path_to_under_test_results",
                     settings = settings,
                     pdf_report = FALSE)

# Example 4
# Benchmark soiltemp in addition to default settings
# with a special metric
settings <- c(default_settings, # use default settings
              list(msoiltemp1 = c(GlobAvgTimeAvgTable, TimeAvgMap))
              # GlobAvgTimeAvgTable uses a weighted average over space
              # instead of the standard weighted sum
              )
BM_data <- benchmark("path_to_baseline_results",
                     "path_to_under_test_results",
                     settings = settings)

# Example 5
# Benchmark multiple under test runs against the baseline
BM_data <- benchmark("path_to_baseline_results",
                    list("path_to_under_test_results1",
                    "path_to_under_test_results2")
                    )

# Example 6
# Benchmark with custom metric options
metric_options <- list(
  GlobSumTimeAvgTable = list(font_size = 12), # use larger font size in table
  TimeAvgMap = list(highlight = "soilc")      # plots a larger map for soilc
)
BM_data <- benchmark("path_to_baseline_results",
                     "path_to_under_test_results",
                     metric_options = metric_options)

# Example 7
# Benchmark only maize harvest
# The benchmarking allows to select only specific bands of an output
settings <- list(`pft_harvest.pft$rainfed maize; irrigated maize`
                  = c(GlobSumTimeAvgTable))
benchmark("path_to_baseline_results", "path_to_under_test_results",
          settings)
}

}
\seealso{
\code{\link{create_pdf_report}}
}
