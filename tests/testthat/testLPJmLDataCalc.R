library(testthat)
library(lpjmlkit)
library(LPJmLDataCalc)
library(units)

# utility functions ------------------------

create_LPJmLDataCalc <- function(data, my_unit, ...) {
  header <- create_header(ncell = 1, ...)
  lpj_meta <- lpjmlkit:::LPJmLMetaData$new(header, list(unit = my_unit))
  lpj_dat <- lpjmlkit:::LPJmLData$new(data, lpj_meta)
  return(LPJmLDataCalc$new(lpj_dat))
}

# tests  ------------------------

test_that("when initializing with non LPJmLData object an error is thrown", {
  expect_error(LPJmLDataCalc$new(1), regexp = "LPJmLData")
})

test_that("when initializing with LPJmLData the content arrives", {
  header <- lpjmlkit::create_header(ncell = 6)
  lpj_meta <- lpjmlkit:::LPJmLMetaData$new(header)
  lpj_dat <- lpjmlkit:::LPJmLData$new(1, meta_data = lpj_meta)

  lpj_calc <- LPJmLDataCalc$new(lpj_dat)

  expect_equal(lpj_calc$meta$ncell, 6)
  expect_equal(lpj_calc$data, 1)
})

test_that("the object returned when calling obj$data doesn't have class", {
  lpj_calc <- create_LPJmLDataCalc(1, "gN")

  expect_false(inherits(lpj_calc$data, "units"))
})

test_that("as_LPJmLDataCalc returns LPJmLDataCalc object", {
  header <- create_header(ncell = 1)
  lpj_meta <- lpjmlkit:::LPJmLMetaData$new(header, list(unit = "g"))

  lpj_dat <- lpjmlkit:::LPJmLData$new(c(1), lpj_meta)
  lpj_calc <- as_LPJmLDataCalc(lpj_dat)

  expect_true(inherits(lpj_calc, "LPJmLDataCalc"))
})

test_that("conversion LPJ unit of the wild to format of units package works", {
  path_to_data <- test_path("../testdata", "soiln.rds")
  soil_n <- readRDS(path_to_data)

  soil_n_calc <- as_LPJmLDataCalc(soil_n)
  x <- soil_n_calc$.__data_with_unit__

  expect_equal(attr(x, "unit")$numerator, "gN")
})


test_that("two LPJmLDataCalc objects with different units cannot be added", {
  lpj_calc1 <- create_LPJmLDataCalc(1, "gN")
  lpj_calc2 <- create_LPJmLDataCalc(2, "gC")

  expect_error(lpj_calc1 + lpj_calc2, regexp = "cannot convert")
})

test_that("correct units and value results from addition", {
  ## experiment 1: 1 g/m2 + 1 kg/m2 = 1001 g/m2
  lpj_calc1 <- create_LPJmLDataCalc(1, "g/m2")
  lpj_calc2 <- create_LPJmLDataCalc(1, "kg/m2")

  sum <- lpj_calc1 + lpj_calc2

  expect_equal(sum$meta$unit, "g/m2")
  expect_equal(sum$data, 1001)


  ## experiment 2: array addition
  array1 <- c(1, 0, 0, 1)
  dim(array1) <- c(2, 2)
  array2 <- c(3, 3, 3, 3)
  dim(array2) <- c(2, 2)
  lpj_calc1 <- create_LPJmLDataCalc(array1, "")
  lpj_calc2 <- create_LPJmLDataCalc(array2, "")

  z <- lpj_calc1 + lpj_calc2

  expect_equal(z$data, array1 + array2)
}
)

test_that("correct units and value results from addition", {
  ## experiment 1: 1 g/m2 + 1 kg/m2 = 1001 g/m2
  lpj_calc1 <- create_LPJmLDataCalc(1, "g/m2")
  lpj_calc2 <- create_LPJmLDataCalc(1, "kg/m2")

  sum <- lpj_calc1 - lpj_calc2

  expect_equal(sum$meta$unit, "g/m2")
  expect_equal(sum$data, -999)
}
)

test_that("correct units and value results from multiplication", {
  ## experiment 1: gN/m^2 * 1/gN = 1/m^2
  lpj_calc1 <- create_LPJmLDataCalc(1, "gN/m2")
  lpj_calc2 <- create_LPJmLDataCalc(1, "1/gN")

  product <- lpj_calc1 * lpj_calc2

  x <- product$.__data_with_unit__
  expect_equal(attr(x, "unit")$numerator, character(0))
  expect_equal(attr(x, "unit")$denominator, "m2")
  expect_equal(product$meta$unit, "1/m2")

  ## experiment 2: gN * 1/gN = 1
  lpj_calc1 <- create_LPJmLDataCalc(2, "gN")
  lpj_calc2 <- create_LPJmLDataCalc(1 / 2, "1/gN")

  product <- lpj_calc1 * lpj_calc2

  expect_equal(product$meta$unit, "1") # TODO: how to handle no unit case?
  expect_equal(product$data, 1)

  ## experiment 3: gC * 1/gN = gC/gN
  lpj_calc1 <- create_LPJmLDataCalc(2, "gC")
  lpj_calc2 <- create_LPJmLDataCalc(1 / 2, "1/gN")

  product <- lpj_calc1 * lpj_calc2

  expect_equal(product$meta$unit, "gC/gN")
  expect_equal(product$data, 1)

  ## experiment 4: array multiplication
  array1 <- c(1, 0, 0, 1)
  dim(array1) <- c(2, 2)
  array2 <- c(3, 3, 3, 3)
  dim(array2) <- c(2, 2)
  lpj_calc1 <- create_LPJmLDataCalc(array1, "")
  lpj_calc2 <- create_LPJmLDataCalc(array2, "")

  product <- lpj_calc1 * lpj_calc2

  expect_equal(product$data, array1 * array2)
}
)

test_that("correct units and value results from division", {
  lpj_calc1 <- create_LPJmLDataCalc(2, "gN")
  lpj_calc2 <- create_LPJmLDataCalc(1 / 2, "gN")

  ratio <- lpj_calc1 / lpj_calc2

  expect_equal(ratio$data, 4)
  expect_equal(ratio$meta$unit, "1")
})

test_that("meta from multiplicand is used", {
  lpj_calc1 <- create_LPJmLDataCalc(1, "gN", nyear = 5)
  lpj_calc2 <- create_LPJmLDataCalc(1, "", nyear = 1)

  product <- lpj_calc1 * lpj_calc2

  expect_equal(product$meta, lpj_calc1$meta)
})

test_that("multiplication with scalar units object works", {
  lpj_calc1 <- create_LPJmLDataCalc(array(c(1,2,3,4) ,c(2,2)), "gN")
  z <- create_LPJmLDataCalc(2, "")

  product <- lpj_calc1 * z

  expect_equal(product$data, 8)
})
