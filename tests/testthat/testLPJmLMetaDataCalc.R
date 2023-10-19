test_that("meta of LPJmLDataCalc is LPJmLMetaDataCalc", {
  soiln <- load_soiln_calc()
  soiln_calc <- as_LPJmLDataCalc(soiln)

  expect_true(inherits(soiln_calc$meta, "LPJmLMetaDataCalc"))
})

test_that("print of LPJmLMetaDataCalc works correctly for soiln", {
  soiln <- load_soiln_calc()
  soiln_calc <- as_LPJmLDataCalc(soiln)

  expect_output(
    print(soiln_calc$meta),
    "cellsize_lon 0.5"
  )
  expect_output(
    print(soiln_calc$meta),
    "aggregated FALSE"
  )
})
