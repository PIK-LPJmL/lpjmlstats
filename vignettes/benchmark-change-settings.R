## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----load_package-------------------------------------------------------------
#  library(lpjmlstats)

## ----set_year_subset----------------------------------------------------------
#  set_lpjmlstats_settings(year_subset = 1:5)

## ----eval=FALSE---------------------------------------------------------------
#  settings <- list(
#    output_var1  = c(metric1, metric2, metric3),
#    output_var2  = c(metric2),
#    output_var3  = c(metric1, metric4),
#    ...
#  )

## ----create_settings_extension------------------------------------------------
#  soil_temp_settings <- list(
#    # first top layer is evaluated by all mentioned metrics
#    msoiltemp1  = c(
#      GlobAvgTimeAvgTable,
#      GlobAvgAnnAvgTimeseries,
#      GlobAvgTimeseries,
#      TimeAvgMap
#    ),
#    # third layer is also evaulated but only by an annual time series
#    msoiltemp3  = c(GlobAvgAnnAvgTimeseries)
#  )

## ----extend_settings----------------------------------------------------------
#  extended_default <- c(default_settings, soil_temp_settings)

## ----create_metric_options----------------------------------------------------
#  metric_opt <- list(TimeAvgMap = list(highlight = c("soiltemp1")))

## ----start_adjusted_benchmark-------------------------------------------------
#  benchmark_results <-
#    benchmark(
#      "/p/projects/lpjml/benchmark_run_outputs/5.7.1/output",
#      "/p/projects/lpjml/benchmark_run_outputs/5.8.6/output",
#      settings = extended_default,
#      metric_options = metric_opt,
#      author = "David",
#      description = "Evaluate changes to soil temperature",
#      output_file = "benchmark_soiltemp.pdf"
#    )

## -----------------------------------------------------------------------------
#  set_lpjmlstats_settings(year_subset = NULL)

