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

## ----perform-first-benchmark--------------------------------------------------
#  benchmark_obj <-
#    benchmark(
#      baseline_dir = "/p/projects/lpjml/benchmark_run_outputs/5.7.1/output",
#      # it is recommended to use absolute paths
#      under_test_dirs = "/p/projects/lpjml/benchmark_run_outputs/5.8.6/output",
#      author = "David",
#      settings = list(vegc = c(GlobAvgAnnAvgTimeseries)),
#      description = "Evaluate the changes from version 5.7.1 to 5.8.6",
#      output_file = "benchmark_vignette.pdf"
#    )

## ----eval=FALSE---------------------------------------------------------------
#  default_settings <- list(
#    vegc  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
#    soilc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
#    litc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
#    ...
#  )

## ----bechmark_multiple_under_test, eval = F-----------------------------------
#  benchmark(
#    "/p/projects/lpjml/benchmark_run_outputs/5.8.1/output",
#    list(
#      "/p/projects/lpjml/benchmark_run_outputs/5.7.7/output",
#      # note that older versions than 5.7.1 can at the moment not be benchmarked due
#      # to missing terr_area output
#      "/p/projects/lpjml/benchmark_run_outputs/5.7.1/output"
#    ),
#    author = "David",
#    description = "Check how newer lpjml versions compare to older versions",
#    output_file = "benchmark_vignette_multiple_ut.pdf"
#  )

## ----acess_baseline_vegc------------------------------------------------------
#  vegc_old_ts <- benchmark_obj$GlobSumAnnAvgTimeseries$var_grp_list$vegc$baseline
#  vegc_old_ts

## ----subset_vegc--------------------------------------------------------------
#  subset(vegc_old_ts, time = 1:5)$data

## ----get_metadata-------------------------------------------------------------
#  get_benchmark_meta_data(benchmark_obj)

## ----access_under_test_metadata-----------------------------------------------
#  vegc_new_ts <- benchmark_obj$GlobSumAnnAvgTimeseries$var_grp_list$vegc$under_test$`5.8.`

## ----calculate_vegc_difference------------------------------------------------
#  subset(vegc_new_ts, time = 1:5)$data - subset(vegc_old_ts, time = 1:5)$data

## ----convert_to_terra---------------------------------------------------------
#  soiln_lpjmldata <- benchmark_obj$TimeAvgMap$var_grp_list$soiln$under_test$`5.8.`
#  
#  soiln_terra <- lpjmlkit::as_terra(soiln_lpjmldata)
#  
#  soiln_terra
#  
#  # read in validation data terra
#  
#  # ...

## ----output_table-------------------------------------------------------------
#  benchmark_obj$GlobSumTimeAvgTable$plot()

## ----plot_vegc, fig.width=8, fig.height=6-------------------------------------
#  benchmark_obj$TimeAvgMap$plot()[[1]] # vegc is the first output defined in the settings

## ----plot_litc, fig.width=8, fig.height=6-------------------------------------
#  vegc_map <- benchmark_obj$TimeAvgMap$plot()[[3]] # litc is the third output of the settings
#  
#  vegc_map + ggplot2::ggtitle("Difference in litter carbon; lpjml 5.8.6 - lpjml 5.7.1")

## ----save_benchmark_object, eval = FALSE--------------------------------------
#  saveRDS(benchmark_obj, file = "benchmark_obj_vignette.rds")

## -----------------------------------------------------------------------------
#  set_lpjmlstats_settings(year_subset = NULL)

