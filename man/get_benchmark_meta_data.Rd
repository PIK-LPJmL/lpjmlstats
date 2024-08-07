% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark.R
\name{get_benchmark_meta_data}
\alias{get_benchmark_meta_data}
\title{Function that returns the meta data of a benchmarkResult object}
\usage{
get_benchmark_meta_data(benchmark_result)
}
\arguments{
\item{benchmark_result}{A benchmarkResult object}
}
\value{
A list with the meta data of the benchmarkResult object.
The list contains the author, the description and a simulation
identification table. The latter is a tibble with the columns
sim_paths, lpjml_version, sim_names, sim_ident and sim_type.
}
\description{
Function that returns the meta data of a benchmarkResult object
}
