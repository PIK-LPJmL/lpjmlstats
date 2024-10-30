


test_that("when initializing with non LPJmLData object an error is thrown", {
  expect_error(LPJmLDataCalc$new(1), regexp = "LPJmLData")
})

test_that("when initializing with LPJmLData the content arrives", {
  header <- lpjmlkit::create_header(ncell = 6, verbose = FALSE)
  lpjml_meta <- lpjmlkit::LPJmLMetaData$new(header)
  lpjml_meta$.__set_attribute__("unit", "")
  lpjml_dat <- lpjmlkit::LPJmLData$new(1,
                                       meta_data = lpjml_meta)

  lpjml_calc <- LPJmLDataCalc$new(lpjml_dat)

  expect_equal(lpjml_calc$meta$ncell, 6)
  expect_equal(as.numeric(lpjml_calc$data), 1, ignore_attr = TRUE)
})

test_that("the object returned when calling obj$data doesn't have class", {
  lpjml_calc <- create_LPJmLDataCalc(1, "gN")

  expect_false(inherits(lpjml_calc$data, "units"))
})

test_that(".as_LPJmLDataCalc returns LPJmLDataCalc object", {
  header <- create_header(ncell = 1, verbose = FALSE)
  lpjml_meta <-
    lpjmlkit::LPJmLMetaData$new(header, list(unit = "g"))

  lpjml_dat <- lpjmlkit::LPJmLData$new(c(1), lpjml_meta)
  lpjml_calc <- .as_LPJmLDataCalc(lpjml_dat)

  expect_true(inherits(lpjml_calc, "LPJmLDataCalc"))
})

test_that("conversion LPJ unit of the wild to format of units package works", {
  path_to_data <- test_path("../testdata/path1", "soiln.rds")
  soil_n <- readRDS(path_to_data)

  soil_n_calc <- .as_LPJmLDataCalc(soil_n)
  x <- soil_n_calc$.data_with_unit

  expect_equal(attr(x, "units")$numerator, "gN")
})


test_that("two LPJmLDataCalc objects with different units cannot be added", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "gN")
  lpjml_calc2 <- create_LPJmLDataCalc(2, "gC")

  expect_error(lpjml_calc1 + lpjml_calc2, regexp = "cannot convert")
})

test_that("correct units and value results from addition", {
  ## experiment 1: 1 g/m2 + 1 kg/m2 = 1001 g/m2
  lpjml_calc1 <- create_LPJmLDataCalc(1, "g/m2")
  lpjml_calc2 <- create_LPJmLDataCalc(1, "kg/m2")

  sum <- lpjml_calc1 + lpjml_calc2

  expect_equal(sum$meta$unit, "g m-2")
  expect_equal(sum$data[[1]], 1001, ignore_attr = TRUE)


  ## experiment 2: array addition
  array1 <- c(1, 0, 0, 1)
  dim(array1) <- c(2, 2, 1)
  array2 <- c(3, 3, 3, 3)
  dim(array2) <- c(2, 2, 1)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  lpjml_calc2 <- create_LPJmLDataCalc(array2, "")

  z <- lpjml_calc1 + lpjml_calc2

  expect_equal(z$data, array1 + array2, ignore_attr = TRUE)
})

test_that("broadcasting second operator works for addition", {
  ## experiment 1: 1 g/m2 + 1 kg/m2 = 1001 g/m2
  lpjml_calc1 <- create_LPJmLDataCalc(c(1, 1), "g/m2")
  lpjml_calc2 <- create_LPJmLDataCalc(1, "kg/m2")

  sum <- lpjml_calc1 + lpjml_calc2

  expect_equal(sum$meta$unit, "g m-2")
  expect_equal(sum$data[[1]], 1001, ignore_attr = TRUE)
})

test_that("addition with scalar works", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "g/m2")

  scalar <- 1

  sum <- lpjml_calc1 + scalar

  expect_equal(sum$data[[1]], 2, ignore_attr = TRUE)
})


test_that("correct units and value results from subtraction", {
  ## experiment 1: 1 g/m2 + 1 kg/m2 = 1001 g/m2
  lpjml_calc1 <- create_LPJmLDataCalc(1, "g/m2")
  lpjml_calc2 <- create_LPJmLDataCalc(1, "kg/m2")

  difference <- lpjml_calc1 - lpjml_calc2

  expect_equal(difference$meta$unit, "g m-2")
  expect_equal(difference$data[[1]], -999, ignore_attr = TRUE)
})

test_that("broadcasting second operator works for subtraction", {
  ## experiment 1: 1 g/m2 + 1 kg/m2 = 1001 g/m2
  lpjml_calc1 <- create_LPJmLDataCalc(c(1, 1), "g/m2")
  lpjml_calc2 <- create_LPJmLDataCalc(1, "kg/m2")

  difference <- lpjml_calc1 - lpjml_calc2

  expect_equal(difference$meta$unit, "g m-2")
  expect_equal(difference$data[[1]], -999, ignore_attr = TRUE)
})

test_that("subtraction with scalar works", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "g/m2")

  scalar <- 1

  difference <- lpjml_calc1 - scalar

  expect_equal(difference$data[[1]], 0, ignore_attr = TRUE)
})


test_that("correct units and value results from multiplication", {
  ## experiment 1: gN/m^2 * 1/gN = 1/m^2
  lpjml_calc1 <- create_LPJmLDataCalc(1, "gN/m2")
  lpjml_calc2 <- create_LPJmLDataCalc(1, "1/gN")

  product <- lpjml_calc1 * lpjml_calc2

  x <- product$.data_with_unit
  expect_equal(product$meta$unit, "m-2")

  ## experiment 2: gN * 1/gN = 1
  lpjml_calc1 <- create_LPJmLDataCalc(2, "gN")
  lpjml_calc2 <- create_LPJmLDataCalc(1 / 2, "1/gN")

  product <- lpjml_calc1 * lpjml_calc2

  expect_equal(product$meta$unit, "") # NTODO: how to handle no unit case?
  expect_equal(product$data[[1]], 1, ignore_attr = TRUE)

  ## experiment 3: gC * 1/gN = gC/gN
  lpjml_calc1 <- create_LPJmLDataCalc(2, "gC")
  lpjml_calc2 <- create_LPJmLDataCalc(1 / 2, "1/gN")

  product <- lpjml_calc1 * lpjml_calc2

  expect_equal(product$meta$unit, "gC gN-1")
  expect_equal(product$data[[1]], 1, ignore_attr = TRUE)

  ## experiment 4: array multiplication
  array1 <- c(1, 0, 0, 1)
  dim(array1) <- c(2, 2, 1)
  array2 <- c(3, 3, 3, 3)
  dim(array2) <- c(2, 2, 1)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  lpjml_calc2 <- create_LPJmLDataCalc(array2, "")

  product <- lpjml_calc1 * lpjml_calc2

  expect_equal(product$data, array1 * array2, ignore_attr = TRUE)

  ## experiment 5: 1/m2 * m2 = 1
  lpjml_calc1 <- create_LPJmLDataCalc(1, "1/m2")
  lpjml_calc2 <- create_LPJmLDataCalc(1, "m2")

  product <- lpjml_calc1 * lpjml_calc2

  expect_equal(product$meta$unit, "")
})

test_that("broadcasting second operator works for multiplication", {
  array1 <- rep(c(1, 0, 0, 1), 3)
  dim(array1) <- c(2, 2, 3)
  array2 <- c(3, 2, 1)
  dim(array2) <- c(1, 1, 3)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  lpjml_calc2 <- create_LPJmLDataCalc(array2, "")

  product <- lpjml_calc1 * lpjml_calc2

  expect_equal(product$data[1, 1, ], c(3, 2, 1), ignore_attr = TRUE) #nolint
})

test_that("multiplication with non matchin dimnames fails", {
  array1 <- rep(c(1, 0, 0, 1), 3)
  dim(array1) <- c(2, 2, 3)
  dimnames(array1) <- list(cell = c("1", "2"), time = c("1", "2"), band = c("1", "2", "3"))
  array2 <- c(3, 2, 1)
  dim(array2) <- c(1, 1, 3)
  dimnames(array2) <- list(cell = c("1"), time = c("1"), band = c("1", "2", "4"))
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  lpjml_calc2 <- create_LPJmLDataCalc(array2, "")

  expect_error(lpjml_calc1 * lpjml_calc2, "match")

})

test_that("multiplication with scalar unit object works", {
  array1 <- c(1, 0, 0, 1)
  dim(array1) <- c(2, 2, 1)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")

  multiplier <- 2

  lpjml_calc2 <- lpjml_calc1 * multiplier
  expect_equal(lpjml_calc2$data, array1 * 2, ignore_attr = TRUE)
})

test_that("multiplication with vector works", {
  array1 <- c(1, 1, 1, 1)
  dim(array1) <- c(2, 2, 1)

  # test 1 apply to cells
  array2 <- c(3, 4)
  dim(array2) <- c(2, 1, 1)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  product <- lpjml_calc1 * array2
  result <- array(c(3, 4, 3, 4), dim = c(2, 2, 1))
  expect_equal(product$data, result, ignore_attr = TRUE)

  # test 2 apply to time
  array2 <- c(3, 4)
  dim(array2) <- c(1, 2, 1)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  product <- lpjml_calc1 * array2
  result <- array(c(3, 3, 4, 4), dim = c(2, 2, 1))
  expect_equal(product$data, result, ignore_attr = TRUE)

  # test 3 apply to bands with not matching dims
  array2 <- c(3, 4)
  dim(array2) <- c(1, 1, 2)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  expect_error(lpjml_calc1 * array2, "match")

  # test 4 apply to bands with matching dims
  array1 <- c(1, 1, 1, 1, 1, 1, 1, 1)
  dim(array1) <- c(2, 2, 2)
  array2 <- c(3, 4)
  dim(array2) <- c(1, 1, 2)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  product <- lpjml_calc1 * array2
  result <- array(c(3, 3, 3, 3, 4, 4, 4, 4), dim = c(2, 2, 2))
  expect_equal(product$data, result, ignore_attr = TRUE)

})

test_that("band name order doesn't matter for multiplication", {
  lpjml_calc1 <- read_io(test_path("../testdata/path1/soiln_layer.bin.json"))

  lpjml_calc1_reordered <- subset(lpjml_calc1, band = c("500", "200", "1000", "2000", "3000"))

  expect_equal((lpjml_calc1 * lpjml_calc1_reordered)$data, (lpjml_calc1 * lpjml_calc1)$data)
})

test_that("correct units and value results from division", {
  lpjml_calc1 <- create_LPJmLDataCalc(2, "gN")
  lpjml_calc2 <- create_LPJmLDataCalc(1 / 2, "gN")

  ratio <- lpjml_calc1 / lpjml_calc2

  expect_equal(ratio$data[[1]], 4, ignore_attr = TRUE)
  expect_equal(ratio$meta$unit, "")
})

test_that("broadcasting second operator works for division", {
  array1 <- rep(c(1, 0, 0, 1), 3)
  dim(array1) <- c(2, 2, 3)
  array2 <- c(3, 2, 1)
  dim(array2) <- c(1, 1, 3)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")
  lpjml_calc2 <- create_LPJmLDataCalc(array2, "")

  ratio <- lpjml_calc1 / lpjml_calc2

  expect_equal(ratio$data[1, 1, ], c(1 / 3, 1 / 2, 1), ignore_attr = TRUE) #nolint
})

test_that("division with scalar unit object works", {
  array1 <- c(1, 0, 0, 1)
  dim(array1) <- c(2, 2, 1)
  lpjml_calc1 <- create_LPJmLDataCalc(array1, "")

  divisor <- 2

  lpjml_calc2 <- lpjml_calc1 / divisor
  expect_equal(lpjml_calc2$data, array1 / 2, ignore_attr = TRUE)
})

test_that("meta from multiplicand is used", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "gN", nyear = 1)
  lpjml_calc2 <- create_LPJmLDataCalc(1, "", nyear = 1)

  product <- lpjml_calc1 * lpjml_calc2

  expect_equal(product$meta, lpjml_calc1$meta)
})

test_that("consistency check fails if band number is not consistent", {
  expect_error(create_LPJmLDataCalc(1, "gN", nyear = 1, nband = 2),
               "inconsistent")
})

# ------ test unit conversion ------

test_that("conversion from gN to GtN works", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "gN", nyear = 1)

  lpjml_calc1$.convert_unit("GtN")

  expect_equal(lpjml_calc1$meta$unit, "GtN")
  expect_equal(lpjml_calc1$data, 1 / 1e9, ignore_attr = TRUE)
})

test_that("applying conversions of conversion table does correct converion", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "gN", nyear = 1)

  lpjml_calc1$apply_unit_conversion_table()

  expect_equal(lpjml_calc1$meta$unit, "GtN")
  expect_equal(lpjml_calc1$data, 1 / 1e9, ignore_attr = TRUE)
})

test_that("applying conversion table does nothing for unspecified conversion", {
  lpjml_calc1 <- create_LPJmLDataCalc(1, "g", nyear = 1)

  lpjml_calc1$apply_unit_conversion_table()

  expect_equal(lpjml_calc1$meta$unit, "g")
  expect_equal(lpjml_calc1$data, 1, ignore_attr = TRUE)
})

test_that("keep dimnames works as expected", {
  array1 <- c(1, 1, 1, 1)
  dim(array1) <- c(x = 2, y = 2, z = 1)
  dimnames(array1) <- list(c("a", "b"), c("c", "d"), NULL)

  array1 <- keep_dimnames_and_dims(array1, units::set_units, "gN")

  expect_equal(dimnames(array1), list(c("a", "b"), c("c", "d"), NULL))
  expect_equal(deparse_unit(array1), "gN")
  expect_equal(dim(array1), c(x = 2, y = 2, z = 1))
})

test_that("add band returns correct data and meta data for mean", {
  soiln_calc <- load_soiln_layer_calc()
  soiln_calc$add_band("mean", function(x) sum(x) / length(x))

  # meta data checks
  expect_equal(soiln_calc$meta$nbands, 6)
  expect_equal(soiln_calc$meta$band_names[6], "mean")
  expect_equal(dim(soiln_calc$data), c(cell = 1, time = 10, band = 6))

  # data checks
  expect_equal(soiln_calc$data[1, 1, 6], mean(soiln_calc$data[1, 1, ]))
  expect_equal(units::deparse_unit(soiln_calc$.data_with_unit), "gN m-2")
  expect_equal(dimnames(soiln_calc$data)$band, c("200", "500", "1000", "2000", "3000", "mean"))
})
