#' @title LPJmLDataCalc
#'
#' @description
#' An extended LPJmLData class that enables calculations and statistics. 
#' 

LPJmLDataCalc <- R6::R6Class(
  
  classname = "LPJmLDataCalc",
  
  inherit = lpjmlkit:::LPJmLData,
  
  public = list(
    initialize = function(data, meta_data=NULL) {
      super$initialize(data = data, meta_data=meta_data)
    },
    feed_LPJmLData = function(lpjml_data_obj) {
      # Ensure the passed object is of class LPJmLData
      if (!inherits(lpjml_data_obj, "LPJmLData")) {
        stop("Expected an LPJmLData object")
      }
      # Feed the data from the provided LPJmLData object
      private$.data = lpjml_data_obj$data
      private$.meta = lpjml_data_obj$meta
      private$.grid = lpjml_data_obj$grid
    }
  )
  
  
)
