# Test validation through get_plot: bm argument
test_that("get_plot validates bm argument correctly", {
  # NULL bm
  expect_error(
    get_plot(NULL),
    "Argument 'bm' must be a non-empty benchmark object"
  )

  # Non-list bm
  expect_error(
    get_plot("not a list"),
    "Argument 'bm' must be a non-empty benchmark object"
  )

  # Empty list bm
  expect_error(
    get_plot(list()),
    "Argument 'bm' must be a non-empty benchmark object"
  )
})

test_that("get_plot validates metric argument correctly", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Non-character metric
  expect_error(
    get_plot(bm, metric = 123),
    "Argument 'metric' must be a character vector"
  )

  # Empty character vector
  expect_error(
    get_plot(bm, metric = character(0)),
    "Argument 'metric' cannot be an empty character vector"
  )
})

test_that("get_plot validates variables argument correctly", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Non-character variables
  expect_error(
    get_plot(bm, metric = "TimeAvgMap", variables = 123),
    "Argument 'variables' must be a character vector"
  )

  # Empty character vector
  expect_error(
    get_plot(bm, metric = "TimeAvgMap", variables = character(0)),
    "Argument 'variables' cannot be an empty character vector"
  )
})

test_that("get_plot validates data_only argument correctly", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Non-logical data_only
  expect_error(
    get_plot(bm, metric = "TimeAvgMap", data_only = "yes"),
    "Argument 'data_only' must be a single logical value"
  )

  # NA data_only
  expect_error(
    get_plot(bm, metric = "TimeAvgMap", data_only = NA),
    "Argument 'data_only' must be a single logical value"
  )

  # Vector of logicals
  expect_error(
    get_plot(bm, metric = "TimeAvgMap", data_only = c(TRUE, FALSE)),
    "Argument 'data_only' must be a single logical value"
  )
})

# Test character vector cleaning through variables
test_that("get_plot handles variables with whitespace and duplicates", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Variables with whitespace should be trimmed
  suppressWarnings({
    plots <- get_plot(
      bm,
      metric = "TimeAvgMap",
      variables = c("  soiln  ", "soiln")  # Duplicate after trimming
    )
  })

  expect_true(is.list(plots))
  expect_equal(length(plots), 1)  # Duplicates removed
})

test_that("get_plot errors on variables with only NA or empty strings", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Variables with only NA/empty
  expect_error(
    get_plot(bm, metric = "TimeAvgMap", variables = c(NA, "", "  ")),
    "contains only NA or empty strings"
  )
})

# Test metric validation
test_that("get_plot errors when metric not found", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  expect_error(
    get_plot(bm, metric = "NonExistentMetric"),
    "Metric 'NonExistentMetric' not found"
  )
})

# Test variable index validation
test_that("get_plot warns about missing variables", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  expect_warning(
    get_plot(bm, metric = "TimeAvgMap", variables = c("soiln", "nonexistent")),
    "Variable\\(s\\) not found.*nonexistent"
  )
})

test_that("get_plot errors when no variables match", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(soiln = list(TimeAvgMap))

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  expect_error(
    suppressWarnings(
      get_plot(bm, metric = "TimeAvgMap", variables = "nonexistent")
    ),
    "None of the specified variables were found"
  )
})

# Test main functionality
test_that("get_plot works with valid benchmark object", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(GlobSumTimeAvgTable, TimeAvgMap)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[c(1, 3)]
  )

  # Get plots for single metric
  suppressWarnings({
    plots <- get_plot(bm, metric = "TimeAvgMap")
  })

  expect_true(is.list(plots))
  expect_equal(length(plots), 1)
})

test_that("get_plot works with specific variables", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(GlobSumTimeAvgTable, TimeAvgMap)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[c(1, 3)]
  )

  # Get plots for specific variable
  suppressWarnings({
    plots <- get_plot(bm, metric = "TimeAvgMap", variables = "soiln")
  })

  expect_true(is.list(plots))
  expect_equal(names(plots), "soiln")
})

test_that("get_plot works when no metric specified (defaults to all metrics)", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(GlobSumTimeAvgTable)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[1]
  )

  # When no metric specified, should use all available metrics
  # Suppress warnings about defaults
  suppressWarnings(
    result <- get_plot(bm)
  )
  # Should return results for all metrics in the benchmark
  expect_true(is.list(result))
  # Result should have at least one metric
  expect_true(length(result) > 0)
})

test_that("get_plot works when no variables specified (defaults to all variables)", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(TimeAvgMap)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # When no variables specified, should use all available variables
  # Suppress warnings about defaults
  suppressWarnings(
    plots <- get_plot(bm, metric = "TimeAvgMap")
  )
  expect_equal(length(plots), 1)
  expect_true("soiln" %in% names(plots))
})

test_that("get_plot handles multiple metrics", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(GlobSumTimeAvgTable, TimeAvgMap, GlobSumTimeseries)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[c(1, 2, 3)]
  )

  suppressWarnings({
    plots <- get_plot(
      bm,
      metric = c("TimeAvgMap", "GlobSumTimeseries")
    )
  })

  expect_true(is.list(plots))
  expect_equal(length(plots), 2)
  expect_equal(names(plots), c("TimeAvgMap", "GlobSumTimeseries"))
})

test_that("get_plot handles errors in multiple metrics gracefully", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(TimeAvgMap)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Request valid and invalid metrics - should handle gracefully
  # Suppress warnings to avoid pipeline failures
  suppressWarnings(
    plots <- get_plot(
      bm,
      metric = c("TimeAvgMap", "NonExistentMetric")
    )
  )

  # Should still return the valid one
  expect_true(is.list(plots))
  expect_equal(length(plots), 1)
  expect_equal(names(plots), "TimeAvgMap")
})

test_that("get_plot errors when all metrics fail", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(TimeAvgMap)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  # Request only invalid metrics
  expect_error(
    suppressWarnings(
      get_plot(bm, metric = c("NonExistent1", "NonExistent2"))
    ),
    "Failed to retrieve plots for all specified metrics"
  )
})

test_that("get_plot returns data when data_only = TRUE", {
  skip_if_not_installed("ggplot2")

  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(TimeAvgMap)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[3]
  )

  suppressWarnings({
    data <- get_plot(
      bm,
      metric = "TimeAvgMap",
      variables = "soiln",
      data_only = TRUE
    )
  })

  expect_true(is.list(data))
  # Data should be extracted from ggplot
  expect_true(is.data.frame(data[[1]]) || is.list(data[[1]]))
})

test_that("get_plot works with GlobSumTimeseries", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(GlobSumTimeseries)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[2]
  )

  suppressWarnings({
    plots <- get_plot(bm, metric = "GlobSumTimeseries")
  })

  expect_true(is.list(plots))
  expect_equal(length(plots), 1)
})

test_that("get_plot works with GlobSumTimeAvgTable", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(GlobSumTimeAvgTable)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[1]
  )

  suppressWarnings({
    plots <- get_plot(bm, metric = "GlobSumTimeAvgTable")
  })

  expect_true(is.list(plots))
  expect_equal(length(plots), 1)
})

test_that("get_plot returns plots with correct structure", {
  baseline_dir <- test_path("../testdata/path1")
  under_test_dir <- test_path("../testdata/path2")

  settings <- list(
    soiln = list(TimeAvgMap, GlobSumTimeseries)
  )

  bm <- benchmark(
    baseline_dir,
    under_test_dir,
    settings,
    pdf_report = FALSE,
    metric_options = test_m_options[c(2, 3)]
  )

  suppressWarnings({
    plots <- get_plot(bm, metric = c("TimeAvgMap", "GlobSumTimeseries"))
  })

  # Should return nested list: first level = metrics, second level = variables
  expect_true(is.list(plots))
  expect_equal(length(plots), 2)
  expect_true(all(sapply(plots, is.list)))
})
