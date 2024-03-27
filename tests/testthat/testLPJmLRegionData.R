

test_that("LPJmLRegionData can be initialized", {
  grid <- load_test_grid()
  region_matrix <-
    generate_random_region_matrix(3, 10, grid$meta$ncell)
  lpjml_region_data <- LPJmLRegionData$new(grid, region_matrix)

  expect_true(inherits(lpjml_region_data, "LPJmLRegionData"))
})

test_that("consistency check throws error/warning when needed", {
  # experiment 1: inconsistent region matrix
  grid <- load_test_grid()
  region_matrix <-
    generate_random_region_matrix(3, 10, grid$meta$ncell + 10)
  expect_error(LPJmLRegionData$new(grid, region_matrix), "inconsistent")

  # experiment 2: consistent region matrix
  grid <- load_test_grid()
  region_matrix <-
    generate_random_region_matrix(3, 10, grid$meta$ncell)

  expect_silent(LPJmLRegionData$new(grid, region_matrix))

  # experiment 3: sum of cell fractions above 1
  grid <- load_test_grid()
  region_matrix <-
    generate_random_region_matrix(3, 10, grid$meta$ncell)
  region_matrix[1, 1] <- 2


  expect_warning(LPJmLRegionData$new(grid, region_matrix), "above 1")
})


test_that("initialization only accepts dgCMatrix", {
  grid <- load_test_grid()
  region_matrix <-
    generate_random_region_matrix(3, 10, grid$meta$ncell)
  region_matrix <- as(region_matrix, "matrix")

  expect_error(LPJmLRegionData$new(grid, region_matrix), "dgCMatrix")
})

test_that("initialization only accepts grid", {
  region_matrix <- generate_random_region_matrix(3, 10, 10)

  expect_error(LPJmLRegionData$new(region_matrix, region_matrix), "grid")
})
