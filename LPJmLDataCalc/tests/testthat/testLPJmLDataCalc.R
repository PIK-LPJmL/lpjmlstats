library(testthat)
library(lpjmlkit)
library(LPJmLDataCalc)

test_that("when feeding a non LPJmLData an error is thrown",{
  myLPJmLDataCalc <- LPJmLDataCalc$new(data = 0)

  expect_error(myLPJmLDataCalc$feed_LPJmLData(1))
})

test_that("when feeding a LPJmLData its content arrives in the LPJmLDataCalc",{
  # prepare lpjmlCalcData
  header <- lpjmlkit::create_header(ncell = 11)
  myLPJmLMetaData <- lpjmlkit:::LPJmLMetaData$new(header)
  myLPJmLDataCalc <- LPJmLDataCalc$new(data = 0, meta_data = myLPJmLMetaData)

  # prepare lpjmlData to feed into lpjmlCalcData
  header2 <- lpjmlkit::create_header(ncell = 6)
  myLPJmLMetaData2 <- lpjmlkit:::LPJmLMetaData$new(header2)
  myLPJmLData <- lpjmlkit:::LPJmLData$new(1, meta_data = myLPJmLMetaData2)

  #feed lpjmlData -> lpjmlCalcData
  myLPJmLDataCalc$feed_LPJmLData(myLPJmLData)

  expect_equal(myLPJmLDataCalc$meta$ncell , 6)
  expect_equal(myLPJmLDataCalc$data , 1)
})

test_that("adding two LPJmLDataCalc objects with single value results in LPJmLDataCalc object containing the sum", {
  obj1 <- LPJmLDataCalc$new(c(1))
  obj2 <- LPJmLDataCalc$new(c(1))
  objSum <- obj1+obj2

  expect_equal(objSum$data, 2 )
})


test_that("two LPJmLDataCalc objects with different units cannot be added", {
  header <- create_header(ncell = 1)
  LPJmLMetaUnitA <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes = list(unit="g") )
  LPJmLMetaUnitB <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes = list(unit="m") )
  objA <- LPJmLDataCalc$new(c(1), LPJmLMetaUnitA)
  objB <- LPJmLDataCalc$new(c(1), LPJmLMetaUnitB)

  expect_error(objA+objB, regexp = "cannot convert")
})

test_that("LPJmLDataCalc fed by LPJmLData with unit A and LPJmLDataCalc with unit B cannot be added", {
  header <- create_header(ncell = 1)
  LPJmLMetaUnitA <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes = list(unit="g") )
  LPJmLMetaUnitB <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes = list(unit="m") )
  objA <- lpjmlkit:::LPJmLData$new(c(1), LPJmLMetaUnitA)
  objAC <- LPJmLDataCalc$new(c(1))
  objAC$feed_LPJmLData(objA)
  objB <- LPJmLDataCalc$new(c(1), LPJmLMetaUnitB)

  expect_error(objAC+objB, "cannot convert")
})

test_that("The object returned when calling obj$data should have no class attribute", {
  header <- create_header(ncell = 1)
  LPJmLMetaUnitA <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes = list(unit="g") )
  objA <- LPJmLDataCalc$new(c(1), LPJmLMetaUnitA)

  expect_false(inherits(objA$data, "units"))
})
