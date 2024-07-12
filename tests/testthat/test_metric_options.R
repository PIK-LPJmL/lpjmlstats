test_that("metric option year_subset works with strings", {

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeseries))

  metric_options <- list(GlobSumTimeseries = list(year_subset = c("2010")))

  out <-
    benchmark(
      baseline_dir,
      under_test_dir,
      settings,
      pdf_report = FALSE,
      metric_options = metric_options
    )

  under_test_data <-
    out$GlobSumTimeseries$var_grp_list$soiln$under_test$pth2$data

  expect_match(dimnames(under_test_data)$time[1], "2010")
})
