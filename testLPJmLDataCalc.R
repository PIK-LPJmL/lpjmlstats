library(testthat)
library(lpjmlkit)
source("LPJmLDataCalc.R")
test_that("when feeding an LPJmLData its content is stored in the object",{
  header <- lpjmlkit::create_header(ncell = 11)
  myLPJmLMetaData <- lpjmlkit:::LPJmLMetaData$new(header)
  myLPJmLDataCalc <- LPJmLDataCalc$new(data = c(5), meta_data = myLPJmLMetaData)
  
  header2 <- lpjmlkit::create_header(ncell = 6)
  myLPJmLMetaData2 <- lpjmlkit:::LPJmLMetaData$new(header2)
  myLPJmLData <- lpjmlkit:::LPJmLData$new(c(2), meta_data = myLPJmLMetaData2)
  
  myLPJmLDataCalc$feed_LPJmLData(myLPJmLData)
  
  meta <- myLPJmLDataCalc$meta
  expect_equal(meta$ncell , 6)
})


# test_that("adding two LPJmLDataCalc objects with single value results in LPJmLDataCalc object containing the sum", {
#   obj1 <- lpjmlkit:::LPJmLData$new(c(1))
#   obj2 <- lpjmlkit:::LPJmLData$new(c(1))
#   objSum <- obj1+obj2
#   expect_equal(objSum$data, 2 )
# })
