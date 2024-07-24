test_that("metric reordering works", {

  # priority is given to maps to appear at the start of the report
  set_lpjmlstats_settings(metrics_at_start = "Map")

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap))


  out <-
    benchmark(
      baseline_dir,
      under_test_dir,
      settings,
      pdf_report = FALSE,
      metric_options = test_m_options[-4]
    )

  expect_true(stringr::str_detect(names(out[1]), "Map"))

  # restore default
  set_lpjmlstats_settings(metrics_at_start = NULL)
})
