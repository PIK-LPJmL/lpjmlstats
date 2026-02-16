test_that("benchmark produces correct results for global data", {
  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap))
  out <- benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE, metric_options = test_m_options[-4])

  # check that global sum of soiln is still the same
  expect_equal(
    out$GlobSumTimeAvgTable$var_grp_list$soiln$under_test$path2$data,
    196.5053,
    ignore_attr = TRUE
  )

  # mean of time series should be equal to the global sum time avg
  soiln_time_ser <-
    out$GlobSumTimeseries$var_grp_list$soiln$under_test$path2$data
  soiln_time_mean <-
    out$GlobSumTimeAvgTable$var_grp_list$soiln$under_test$path2$.data_with_unit
  expect_equal(mean(soiln_time_ser), soiln_time_mean, ignore_attr = TRUE)

  # compare of time avg should be zero since files are identical
  soiln_time_avg_compare <- out$TimeAvgMap$var_grp_list$soiln$compare$path2$data

  expect_equal(sum(soiln_time_avg_compare), 0)
})

test_that("benchmark runs through for single cell data", {
  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln_layer = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap))

  expect_no_error(benchmark(baseline_dir,
                            under_test_dir,
                            settings,
                            pdf_report = FALSE,
                            metric_options = test_m_options[-4]))
})

test_that("correct meta information ends up in lpjml_calc", {
  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln_layer = list(.DoNothing))

  out <- benchmark(baseline_dir,
                   under_test_dir,
                   settings,
                   pdf_report = FALSE,
                   metric_options = test_m_options[".DoNothing"])

  compare_lpjml_calc <-
    out$.DoNothing$var_grp_list$soiln_layer$compare$nodiff$sim1

  expect_equal(compare_lpjml_calc$meta$pos_in_var_grp,
               list(type = "compare", compare_item = "nodiff"))
})

test_that("benchmark report generation runs through without errors", {
  skip("X11 is not available when running buildLibrary()")
  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeAvgTable))

  out <- benchmark(baseline_dir, under_test_dir, settings, metric_options = test_m_options[1], pdf_report = FALSE)

  report_path <- testthat::test_path("../testdata/benchmark_report.pdf")

  # expect that only one warning is thrown
  expect_no_error(create_pdf_report(out, report_path))

  # remove the report file benchmark.pdf
  file.remove(report_path)
})

test_that("correct error message is thrown when no LPJmL files where found in directory", {
  baseline_dir <- testthat::test_path("../testdata/")
  under_test_dir <- testthat::test_path("../testdata/path2")
  settings <-
    list(soiln = list(GlobSumTimeAvgTable))

  expect_error(
    benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE),
    "Please check"
  )
})


# ----- test benchmark utitlity functions

test_that("create_unique_short_sim_path produces unique names", {
  # create test sim pathes
  paths <- c(
    "C:/Users/test/Desktop/test2/something/master",  # nolint
    "C:/Users/test/Desktop/test2/something/other1",  # nolint
    "C:/Users/test/Desktop/test2/something/other2",  # nolint
    "C:/Users/test/Desktop/test2/anything/other4"    # nolint
  )

  # create unique names
  unique_names <- create_unique_short_sim_paths(paths)

  # check if names are unique
  expect_equal(unique(unique_names), unname(unique_names))

  # check if names are short
  expect_true(all(nchar(unique_names) <= 40))
})
