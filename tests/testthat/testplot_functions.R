test_that("plot functions of metric run through", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <-
    list(
      soiln = list(GlobSumTimeAvgTable, GlobSumTimeseries, TimeAvgMap)
    )

  out <-
    benchmark(baseline_dir, under_test_dir, settings, pdf_report = FALSE, metric_options = test_m_options[-4])

  # NTODO: deal with warnings
  suppressWarnings({
    expect_no_error(out$GlobSumTimeAvgTable$plot())
    expect_no_error(out$GlobSumTimeseries$plot())
    expect_no_error(out$TimeAvgMap$plot())
  })
})

test_that("lpjml_calc_to_table returns correct table", {
  soiln_dir <- test_path("../testdata/path1/soiln_layer.bin.json")
  soiln <- read_io_calc(soiln_dir)

  soiln <- aggregate(soiln, time = "sim_period")
  soiln$.meta$.__set_pos_in_var_grp__(list(type = "baseline"))

  table <- lpjml_calc_to_table(soiln)

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
    table$`baseline:\n undefined simulation`,
    c(308.4811, 136.942, 113.6034, 133.9228, 124.4054),
    tolerance = 0.001
  )
})


test_that("apply_to_lpjml_calcs returns correct list", {
  dir <- test_path("../testdata/path1/soiln_layer.bin.json")
  lpjml_calc <- read_io_calc(dir)
  lpjml_calc <- aggregate(lpjml_calc, time = "sim_period")
  var_grp <- create_var_grp(lpjml_calc)

  var_grp$compare <- list(
    diff = list(
      sim1 = var_grp$under_test$sim1 - var_grp$baseline,
      sim2 = var_grp$under_test$sim2 - var_grp$baseline
    )
  )

  fun <- function(lpjml_calc) {
    table <- tibble::as_tibble(lpjml_calc$data)
    result <- tibble::add_column(
      table,
      name = paste0(
        lpjml_calc$meta$pos_in_var_grp$type,
        lpjml_calc$meta$pos_in_var_grp$compare_item
      )
    )
    return(result)
  }
  table <- var_grp$apply_to_lpjml_calcs(fun)

  expect_equal(table[[1]]$name, "baseline")
  expect_equal(table[[1]]$`sim_period.200`, 308.481, tolerance = 0.01)
  expect_equal(table[[5]]$`sim_period.200`, -288.481, tolerance = 0.01)
})

test_that("var_grp_to_table returns correct table", {
  dir <- test_path("../testdata/path1/soiln_layer.bin.json")
  lpjml_calc <- read_io_calc(dir)
  lpjml_calc <- aggregate(lpjml_calc, time = "sim_period")
  var_grp <- create_var_grp(lpjml_calc)

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

test_that("var_grp_list_to_table returns correct table", {
  dir <- test_path("../testdata/path1/soiln_layer.bin.json")
  lpjml_calc <- read_io_calc(dir)
  lpjml_calc <- aggregate(lpjml_calc, time = "sim_period")
  var_grp <- create_var_grp(lpjml_calc)
  var_grp2 <- var_grp$clone(deep = TRUE)

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

test_that("lpjml_calc_to_map returns map", {
  soiln <- load_soiln_calc()
  soiln$aggregate(time = "sim_period")

  soiln$.meta$.__set_pos_in_var_grp__(list(type = "compare",
                                           compare_item = "diff"))
  m_options <- get_test_m_options()
  map <- lpjml_calc_to_map(soiln, m_options, list(compare = c(-100, 100)))
  expect_equal(class(map)[2], "ggplot")
})

test_that("create_map_plots returns map list", {
  dir <- test_path("../testdata/path1/soiln.bin.json")
  lpjml_calc <- read_io_calc(dir)
  lpjml_calc <- aggregate(lpjml_calc, time = "sim_period")
  lpjml_calc$add_grid()
  var_grp1 <- create_var_grp(lpjml_calc)
  var_grp2 <- var_grp1$clone(deep = TRUE)
  var_grp_list <- list(var_grp1, var_grp2)
  m_options <- get_test_m_options()
  plot_list <- create_map_plots(var_grp_list, m_options)
  expect_equal(class(plot_list[[1]])[2], "ggplot")
  pdf(file = NULL)
  expect_no_error(suppressWarnings(plot(plot_list[[1]])))
  dev.off()
})

test_that("lpjml_calc_to_timeseries_tibble returns correct tibble", {
  soiln <- load_soiln_calc()
  soiln$aggregate(cell = "global")
  soiln$subset(band = 1)
  soiln$.meta$.__set_pos_in_var_grp__(list(type = "compare",
                                           compare_item = "diff"))
  m_options <- get_test_m_options()
  expect_equal(
    lpjml_calc_to_timeseries_tibble(soiln)$value[1],
    c(`2009-12-31` = 196694835877974528)
  )
})

test_that("create_timeseries_plots returns timeseries plotlist
                    for multibanded output",
          {
            dir <- test_path("../testdata/path1/soiln_layer.bin.json")
            lpjml_calc <- read_io_calc(dir)
            var_grp1 <- create_var_grp(lpjml_calc)
            var_grp2 <- var_grp1$deep_clone()
            var_grp2$compare <- NULL
            var_grp_list <- list(var_grp1, var_grp2)
            m_options <- get_test_m_options()
            plot_list <- create_time_series_plots(var_grp_list, m_options)
            expect_equal(class(plot_list[[1]])[2], "ggplot")
            expect_equal(class(plot_list[[2]])[2], "ggplot")
            pdf(file = NULL)
            expect_no_error(suppressWarnings(plot(plot_list[[1]])))
            dev.off()
          })

test_that("create_timeseries_plots returns timeseries plotlist
                    for multicell output",
          {
            dir <- test_path("../testdata/path1/soiln.bin.json")
            lpjml_calc <- read_io_calc(dir)
            lpjml_calc <- subset(lpjml_calc, cell = c(1, 10000))
            var_grp1 <- create_var_grp(lpjml_calc)
            var_grp1$compare <- NULL
            var_grp2 <- var_grp1$deep_clone()
            var_grp2$compare <- NULL
            var_grp_list <- list(var_grp1, var_grp2)
            m_options <- get_test_m_options()
            plot_list <- create_time_series_plots(var_grp_list, m_options)
            expect_equal(class(plot_list[[1]])[2], "ggplot")
            expect_equal(class(plot_list[[2]])[2], "ggplot")
            pdf(file = NULL)
            expect_no_error(suppressWarnings(plot(plot_list[[1]])))
            dev.off()
          })
