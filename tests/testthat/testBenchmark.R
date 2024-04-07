test_that("benchmark produces correct results", {

  set_lpjmlstats_settings(year_subset = NULL) # restore default settings

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap),
         terr_area = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap))

  out <- benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE)

  # check that global sum of soiln is still the same
  expect_equal(
    out$GlobSumTimeAvgTable$var_grp_list$soiln$under_test$pth2$data,
    196.5053,
    ignore_attr = TRUE
  )

  # mean of time series should be equal to the global sum time avg
  soiln_time_ser <-
    out$GlobSumTimeseries$var_grp_list$soiln$under_test$pth2$data
  soiln_time_mean <-
    out$GlobSumTimeAvgTable$var_grp_list$soiln$under_test$pth2$.data_with_unit
  expect_equal(mean(soiln_time_ser), soiln_time_mean, ignore_attr = TRUE)

  # weighted mean of time average should be equal to the global sum time avg
  soiln_time_avg <- out$TimeAvgMap$var_grp_list$soiln$under_test$pth2
  soiln_time_avg$aggregate(cell = "global")

  expect_equal(units::set_units(soiln_time_avg$.data_with_unit, "GtN"),
               units::set_units(soiln_time_mean, "GtN"),
               ignore_attr = TRUE)

  # compare of time avg should be zero since files are identical
  soiln_time_avg_compare <- out$TimeAvgMap$var_grp_list$soiln$compare$pth2$data
  expect_equal(sum(soiln_time_avg_compare), 0)

})

test_that("benchmark report generation runs through without warnings", {
  skip("NTODO: Currently crahses testing of devtools::check()")
  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap))

  out <- benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE)

  report_path <- testthat::test_path("../testdata/benchmark_report.pdf")
  expect_no_warning(create_pdf_report(out, report_path))

  # remove the report file benchmark.pdf
  file.remove(report_path)

})

test_that("benchmark works for davids personal directory", {

  skip("only works at davids personal directory")
  baseline_dir <- "C:/Users/davidho/Desktop/LPJmLG/example_outputs_BM/master" #nolint
  under_test_dir <- "C:/Users/davidho/Desktop/LPJmLG/example_outputs_BM/new_soil_energy" # nolint

  settings <- list(
    `pft_harvest.pft$rainfed rice;
    rainfed maize;
    rainfed oil crops soybean;
    rainfed grassland`  = c(GlobSumTimeAvgPFT_harvest, TimeAvgMap),
    fpc = c(GlobSumTimeAvgFPC, GlobSumTimeAvgTable, TimeAvgMap, GlobSumTimeseries)
  )

  metric_options <- list(
    GlobSumTimeAvgTable = list(
      font_size = 10
    ),
    TimeAvgMap = list(
      highlight = "soilc",
      font_size = 9
    )
  )

  set_lpjmlstats_settings(year_subset = c(1:2))

  bench_data <-
    benchmark(
      baseline_dir,
      list(under_test_dir),
      author = "David",
      description = "test benchmarking"
    )

  create_pdf_report(data = bench_data)

  prepare_tibble_for_table(bench_data$GlobSumTimeAvgTable$var_grp_list)

})





# ----- test benchmark utitlity functions

test_that("create_unique_short_sim_path produces unique names", {
  # create test sim pathes
  paths <- c(
    "C:/Users/davidho/Desktop/LPJmLG/example_outputs_BM/master",           # nolint
    "C:/Users/davidho/Desktop/LPJmLG/example_outputs_BM/new_soil_energy",  # nolint
    "C:/Users/davidho/Desktop/LPJmLG/example_outputs_BM/new_soil_energy2", # nolint
    "C:/Users/davidho/Desktop/LPJmLG/example_outputs_BM2/new_soil_energy"  # nolint
  )

  # create unique names
  unique_names <- create_unique_short_sim_paths(paths)

  # check if names are unique
  expect_equal(unique(unique_names), unname(unique_names))

  # check if names are short
  expect_true(all(nchar(unique_names) <= 40))

})
