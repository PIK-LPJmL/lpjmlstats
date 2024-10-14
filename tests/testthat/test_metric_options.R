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
    out$GlobSumTimeseries$var_grp_list$soiln$under_test$path2$data

  expect_match(dimnames(under_test_data)$time[1], "2010")
})

test_that("metric option cell_subset produces correct subset", {
  soiln <- load_soiln_calc()
  soiln <- subset(soiln, cell = c("1000"), time = c("2010-12-31"))
  expected_val <- aggregate(soiln, cell = "global")$.data_with_unit

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeseries))

  metric_options <- list(GlobSumTimeseries = list(cell_subset = c("1000"),
                                                  year_subset = c("2010")))

  out <-
    benchmark(
      baseline_dir,
      under_test_dir,
      settings,
      pdf_report = FALSE,
      metric_options = metric_options
    )

  under_test_data <-
    out$GlobSumTimeseries$var_grp_list$soiln$under_test$path2$.data_with_unit

  expect_true(expected_val == under_test_data)
})

test_that("metric option sep_cmp_lims still works when FALSE", {
  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMapWithAbs))

  metric_options <- list(TimeAvgMapWithAbs = list(sep_cmp_lims = FALSE, year_subset = c("2010")))

  out <- benchmark(
        baseline_dir,
        under_test_dir,
        settings,
        pdf_report = FALSE,
        metric_options = metric_options
      )
  pdf(file = NULL)
  expect_no_error(out$TimeAvgMapWithAbs$generate_report_content())
  dev.off()
})
