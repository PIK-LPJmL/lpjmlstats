test_that("benchmark produces correct results", {
  baseline_dir <- testthat::test_path("../testdata")
  under_test_dir <- testthat::test_path("../testdata")
  settings <-
    list(soiln = list(GlobalSumTimeAvg, GlobalSumTimeSer, TimeAvg))

  out <- benchmark(baseline_dir, under_test_dir, settings)

  # check that global sum of soiln is still the same
  expect_equal(
    out$GlobalSumTimeAvg$evalList$soiln$ut$actual_fert_landuse$data,
    196505302521977632,
    ignore_attr = TRUE
  )

  # mean of time series should be equal to the global sum time avg
  soiln_time_ser <-
    out$GlobalSumTimeSer$evalList$soiln$ut$actual_fert_landuse$data
  soiln_time_mean <-
    out$GlobalSumTimeAvg$evalList$soiln$ut$actual_fert_landuse$data
  expect_equal(mean(soiln_time_ser), soiln_time_mean, ignore_attr = TRUE)

  # weighted mean of time average should be equal to the global sum time avg
  soiln_time_avg <- out$TimeAvg$evalList$soiln$ut$actual_fert_landuse
  soiln_time_avg$aggregate(cell = "global")

  expect_equal(soiln_time_avg$data,
               soiln_time_mean,
               ignore_attr = TRUE)

  # compare of time avg should be zero since files are identical
  soiln_time_avg_cmp <- out$TimeAvg$evalList$soiln$cmp$actual_fert_landuse$data
  expect_equal(sum(soiln_time_avg_cmp), 0)

})

test_that("retrieve summaries produces correct result for global sum", {
  # prepare paths
  bsl_dir <- testthat::test_path("../testdata")
  ut_dir <- list(testthat::test_path("../testdata"))
  paths <- list(bsl_dir = bsl_dir, ut_dirs = ut_dir, suffix = ".bin.json")

  # prepare settings
  settings <- list(soiln = list(GlobalSumTimeAvg, GlobalSumTimeSer, TimeAvg))

  # initialize metrics
  init_metrics <- initialize_metrics(settings)

  retrieve_summaries(init_metrics, "soiln", paths)

  # check if results for global sum are as expected
  expect_equal(
    init_metrics$GlobalSumTimeAvg$evalList$soiln$ut$actual_fert_landuse$data,
    196505302521977632,
    ignore_attr = TRUE
  )
  expect_equal(
    init_metrics$GlobalSumTimeAvg$evalList$soiln$bsl$data,
    196505302521977632,
    ignore_attr = TRUE
  )
})
