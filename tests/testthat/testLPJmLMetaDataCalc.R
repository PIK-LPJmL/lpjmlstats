test_that("meta of LPJmLDataCalc is LPJmLMetaDataCalc", {
  soiln <- load_soiln_calc()
  soiln_calc <- .as_LPJmLDataCalc(soiln)

  expect_true(inherits(soiln_calc$meta, "LPJmLMetaDataCalc"))
})

test_that("print of LPJmLMetaDataCalc works correctly for soiln", {
  soiln <- load_soiln_calc()
  soiln_calc <- .as_LPJmLDataCalc(soiln)

  expect_output(
    print(soiln_calc$meta),
    "cellsize_lon 0.5"
  )
  expect_output(
    print(soiln_calc$meta),
    "aggregation FALSE"
  )
})

test_that("subset_time_pattern works correctly for soiln", {
  soiln <- load_soiln_calc()

  soiln_calc_subset <- subset_time_pattern(soiln, c("2011-", "2012-", "2013-"))

  expect_equal(soiln_calc_subset$meta$firstyear, 2011)
  expect_equal(soiln_calc_subset$meta$lastyear, 2013)
  expect_equal(dimnames(soiln_calc_subset$data)[[2]], c("2011-12-31", "2012-12-31", "2013-12-31"))
})
