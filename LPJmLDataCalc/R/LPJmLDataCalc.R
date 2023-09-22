#' @title LPJmLDataCalc
#'
#' @importFrom units set_units as_units drop_units deparse_unit
#' @import lpjmlkit
#'
#' @description
#' An extended LPJmLData class that enables calculations and statistics.
#'

#' @export

LPJmLDataCalc <- R6::R6Class(

  classname = "LPJmLDataCalc",

  inherit = lpjmlkit:::LPJmLData,

  public = list(
    initialize = function(data, meta_data=NULL) {
      units::units_options(set_units_mode = "standard") # TODO: put this into apckage installation?
      unitDataBasePath <- test_path("../../units", "udunits2.xml")
      units::load_units_xml(unitDataBasePath) # TODO: install more LPJmL units? is this the right place to do this?
      super$initialize(data = data, meta_data=meta_data)
      if(methods::is(private$.meta, "LPJmLMetaData")){
        private$add_unit_from_meta_to_array()
      }
    },
    feed_LPJmLData = function(lpjml_data_obj) {
      # Ensure the passed object is of class LPJmLData
      if (!inherits(lpjml_data_obj, "LPJmLData")) {
        stop("Expected an LPJmLDataCalc object")
      }
      # Copy the data from the provided LPJmLData object
      private$.data = lpjml_data_obj$data
      private$.meta = lpjml_data_obj$meta
      private$.grid = lpjml_data_obj$grid
      if(methods::is(private$.meta, "LPJmLMetaData")){
        private$add_unit_from_meta_to_array()
      }
    },
    add_LPJmLDataCalc = function(lpj_calc_obj){
      if(!inherits(lpj_calc_obj, "LPJmLDataCalc")){
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data = private$.data+lpj_calc_obj$.__data_with_unit__
    },
    mult_LPJmLDataCalc = function(lpj_calc_obj){
      if(!inherits(lpj_calc_obj, "LPJmLDataCalc")){
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data = private$.data*lpj_calc_obj$.__data_with_unit__
      private$copy_unit_from_array_to_meta()
    },
    divBy_LPJmLDataCalc = function(lpj_calc_obj){
      if(!inherits(lpj_calc_obj, "LPJmLDataCalc")){
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data = private$.data/lpj_calc_obj$.__data_with_unit__
      private$copy_unit_from_array_to_meta()
    }
  ),

  private = list(
    add_unit_from_meta_to_array = function(){
      unit <- private$.meta$unit
      private$.data <- set_units(private$.data, as_units(unit))
    },
    copy_unit_from_array_to_meta = function(){
      unit <- attr(self$.__data_with_unit__,"unit")
      if(identical(unit$numerator , character(0)) ){ # TODO: use of identical correct?
        numerator = 1
      } else{
        numerator = unit$numerator
      }
      denominator = unit$denominator
      if(!identical(unit$denominator , character(0)) ){ # TODO: use of identical correct?
        denominator = paste0("/", denominator)
      }
      unit_str <- paste0(numerator,denominator)
      private$.meta$.__set_attribute__("unit", unit_str)
    }
  ),

  active = list(
    data = function() {
      if(inherits(private$.data, "units")){
        return(drop_units(private$.data))
      } else {
        return(private$.data)
      }
    },
    .__data_with_unit__ = function() {
      return(private$.data)
    }
  )


)

#' @export
`+.LPJmLDataCalc` = function(obj1,obj2) {
  objSum <- obj1$clone(deep=TRUE)
  objSum$add_LPJmLDataCalc(obj2)
  return(objSum)
}

#' @export
`*.LPJmLDataCalc` = function(obj1,obj2) {
  objSum <- obj1$clone(deep=TRUE)
  objSum$mult_LPJmLDataCalc(obj2)
  return(objSum)
}

#' @export
`/.LPJmLDataCalc` = function(obj1,obj2) {
  objSum <- obj1$clone(deep=TRUE)
  objSum$divBy_LPJmLDataCalc(obj2)
  return(objSum)
}


#' @export
as_LPJmLDataCalc = function(obj) {
  objCalc <- LPJmLDataCalc$new(NULL)
  objCalc$feed_LPJmLData(obj)
  return(objCalc)
}


