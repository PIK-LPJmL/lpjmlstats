testthat::test_that("plot functions of metric run through", {
  skip("Two benchmarkable files are needed to run this test,
       blows up the package size")

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")

  settings <-
    list(soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap))

  out <-
    benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE)

  #TODO: deal with warnings
  suppressWarnings({
    expect_no_error(out$GlobSumTimeAvgTable$plot())
    expect_no_error(out$GlobSumTimeseries$plot())
    expect_no_error(out$TimeAvgMap$plot())
  })
})
