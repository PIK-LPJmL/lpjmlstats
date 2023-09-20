library(testthat)
library(lpjmlkit)
source("LPJmLDataCalc.R")
test_that("when feeding a LPJmLData its content is stored in the object",{
  # prepare lpjmlCalcData
  header <- lpjmlkit::create_header(ncell = 11)
  myLPJmLMetaData <- lpjmlkit:::LPJmLMetaData$new(header)
  myLPJmLDataCalc <- LPJmLDataCalc$new(data = 0, meta_data = myLPJmLMetaData)
  
  # prepare lpjmlData to feed into lpjmlCalcData
  header2 <- lpjmlkit::create_header(ncell = 6)
  myLPJmLMetaData2 <- lpjmlkit:::LPJmLMetaData$new(header2)
  myLPJmLData <- lpjmlkit:::LPJmLData$new(1, meta_data = myLPJmLMetaData2)
  
  myLPJmLDataCalc$feed_LPJmLData(myLPJmLData)
  
  expect_equal(myLPJmLDataCalc$meta$ncell , 6)
  expect_equal(myLPJmLDataCalc$data , 1)
})


# test_that("adding two LPJmLDataCalc objects with single value results in LPJmLDataCalc object containing the sum", {
#   obj1 <- lpjmlkit:::LPJmLData$new(c(1))
#   obj2 <- lpjmlkit:::LPJmLData$new(c(1))
#   objSum <- obj1+obj2
#   expect_equal(objSum$data, 2 )
# })
