test_that("subset cell produces correct output", {

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(CellSubsetAnnAvgTimeseries))

  metric_opt <- test_m_options
  metric_opt$CellSubsetAnnAvgTimeseries <- c(list(cell = c("10000", "10002")), m_option)
  out <- benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE,
                   metric_options = metric_opt["CellSubsetAnnAvgTimeseries"])

  soiln <- read_io(testthat::test_path("../testdata/path1/soiln.bin.json"))
  expected <- subset(soiln, cell = c("10000", "10002"))$.data_with_unit
  expected <- units::set_units(expected, "kgN/m^2")
  # check that global sum of soiln is still the same
  expect_equal(out$CellSubsetAnnAvgTimeseries$var_grp_list$soiln$baseline$data,
               expected,
               ignore_attr = TRUE)

})
