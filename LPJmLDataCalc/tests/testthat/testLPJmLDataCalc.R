library(testthat)
library(lpjmlkit)
library(LPJmLDataCalc)
library(units)

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

test_that("as_LPJmLDataCalc returns LPJmLDataCalc", {
  #soilnLPJDat <- readRDS("tests/data/soiln.rds")
  header <- create_header(ncell = 1)
  LPJmLMetaUnitA <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes = list(unit="g") )
  objA <- lpjmlkit:::LPJmLData$new(c(1), LPJmLMetaUnitA)
  expect_true(inherits(as_LPJmLDataCalc(objA), "LPJmLDataCalc"))
})

test_that("conversion of LPJ unit to format of units package works",{
  path_to_data <- test_path("../data", "soiln.rds")
  soilnLPJDat <- readRDS(path_to_data)
  soilnLPJDatCalc <- as_LPJmLDataCalc(soilnLPJDat)
  x <- soilnLPJDatCalc$.__data_with_unit__

  expect_equal(attr(x, "unit")$numerator, "gN")
})

create_LPJmLDataCalc_with_unit <- function(data, myUnit, ...){
  header <- create_header(ncell = 1, ...)
  LPJmLMeta <- lpjmlkit:::LPJmLMetaData$new(header, additional_attributes =list(unit=myUnit) )
  return(LPJmLDataCalc$new(data,LPJmLMeta) )
}


test_that("correct units in meta and value results from multiplication of LPJmLDataCalc objects",{

  ## experiment 1: gN/m^2 * 1/gN = 1/m^2
  LPJCalcU1 <- create_LPJmLDataCalc_with_unit(1, "gN/m2")
  LPJCalcU2 <- create_LPJmLDataCalc_with_unit(1, "1/gN")

  product <- LPJCalcU1*LPJCalcU2
  x <- product$.__data_with_unit__

  expect_equal(attr(x, "unit")$numerator , character(0))
  expect_equal(attr(x, "unit")$denominator , "m2")
  expect_equal(product$meta$unit, "1/m2")

  ## experiment 2: gN * 1/gN = 1
  LPJCalcU1 <- create_LPJmLDataCalc_with_unit(2, "gN")
  LPJCalcU2 <- create_LPJmLDataCalc_with_unit(1/2, "1/gN")
  product <- LPJCalcU1*LPJCalcU2

  expect_equal(product$meta$unit, "1") # TODO: Question: how to handle no unit case?
  expect_equal(product$data, 1)

  ## experiment 3: gC * 1/gN = gC/gN
  LPJCalcU1 <- create_LPJmLDataCalc_with_unit(2, "gC")
  LPJCalcU2 <- create_LPJmLDataCalc_with_unit(1/2, "1/gN")
  product <- LPJCalcU1*LPJCalcU2

  expect_equal(product$meta$unit, "gC/gN")
  expect_equal(product$data, 1)

  ## experiment 4: array multiplication
  array <- c(1,0,0,1)
  dim(array) <- c(2,2)
  array2 <- c(3,3,3,3)
  dim(array2) <- c(2,2)
  LPJCalcU1 <- create_LPJmLDataCalc_with_unit(array, "")
  LPJCalcU2 <- create_LPJmLDataCalc_with_unit(array2, "")
  z<- LPJCalcU1*LPJCalcU2
  expect_equal(z$data, array*array2)
}
)

test_that("meta from multiplicand is used",{
  obj1 <- create_LPJmLDataCalc_with_unit(1, "gN", nyear = 5)
  obj2 <- create_LPJmLDataCalc_with_unit(1, "", nyear = 1)

  objProd <- obj1*obj2

  expect_equal(objProd$meta, obj1$meta)

})

test_that("correct units in meta and value results from division of LPJmLDataCalc objects",{

  LPJCalcU1 <- create_LPJmLDataCalc_with_unit(2, "gN")
  LPJCalcU2 <- create_LPJmLDataCalc_with_unit(1/2, "gN")
  ratio <- LPJCalcU1/LPJCalcU2

  expect_equal(ratio$data, 4)
  expect_equal(ratio$meta$unit, "1")

})

# test_that("multiplication with scalar units object works",{
#
#   LPJCalcU1 <- create_LPJmLDataCalc_with_unit(2, "gN")
#   z <- set_units(4, "1/gN")
#   res <- LPJCalcU1 *z
#
#   expect_equal(res$data, 8)
#
# })


