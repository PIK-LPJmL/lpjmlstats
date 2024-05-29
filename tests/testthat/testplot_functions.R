testthat::test_that("plot functions of metric run through", {

  baseline_dir <- testthat::test_path("../testdata/path1")
  under_test_dir <- testthat::test_path("../testdata/path2")

  settings <-
    list(soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap),
         terr_area = list(GlobSumTimeAvgTable))

  out <-
    benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE)

  # NTODO: deal with warnings
  suppressWarnings({
    expect_no_error(out$GlobSumTimeAvgTable$plot())
    expect_no_error(out$GlobSumTimeseries$plot())
    expect_no_error(out$TimeAvgMap$plot())
  })
})

testthat::test_that("lpjml_calc_to_table returns correct table", {
  soiln_dir <- testthat::test_path("../testdata/path1/soiln_layer.bin.json")
  soiln <- read_io(soiln_dir)

  soiln <- aggregate(soiln, time = "sim_period")

  table <- lpjml_calc_to_table(soiln, "baseline")

  expect_equal(
    table$variable,
    c(
      "soiln_layer$200",
      "soiln_layer$500",
      "soiln_layer$1000",
      "soiln_layer$2000",
      "soiln_layer$3000"
    )
  )
  expect_equal(
    table$`baseline:\n notset`,
    c(308.4811, 136.942, 113.6034, 133.9228, 124.4054),
    tolerance = 0.001
  )
})


testthat::test_that("var_grp_apply returns correct list", {
  var_grp <- create_soiln_var_grp()

  var_grp$compare <- list(
    diff = list(
      sim1 = soiln_under_test_1 - soiln_baseline,
      sim2 = soiln_under_test_2 - soiln_baseline
    )
  )

  fun <- function(lpjml_calc, name, compare_item = "") {
    table <- tibble::as_tibble(lpjml_calc$data)
    tibble::add_column(table, name = paste0(name, "", compare_item))
  }
  table <- var_grp_apply(var_grp, fun)

  expect_equal(table[[1]]$name, "baseline")
  expect_equal(table[[1]]$`sim_period.200`, 308.481, tolerance = 0.01)
  expect_equal(table[[5]]$`sim_period.200`, -288.481, tolerance = 0.01)
})

testthat::test_that("var_grp_to_table returns correct table", {
  var_grp <- create_soiln_var_grp()

  table <- var_grp_to_table(var_grp)

  expect_equal(
    table$variable,
    c(
      "soiln_layer$200",
      "soiln_layer$500",
      "soiln_layer$1000",
      "soiln_layer$2000",
      "soiln_layer$3000"
    )
  )
})

testthat::test_that("var_grpList_to_table returns correct table", {
  var_grp <- create_soiln_var_grp()
  var_grp2 <- create_soiln_var_grp()

  var_grp_list <- list(var_grp, var_grp2)

  table <- var_grp_list_to_table(var_grp_list)

  expect_equal(table$variable, rep(
    c(
      "soiln_layer$200",
      "soiln_layer$500",
      "soiln_layer$1000",
      "soiln_layer$2000",
      "soiln_layer$3000"
    ),
    2
  ))
})

