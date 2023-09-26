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
    initialize = function(lpj_dat) {
      # TODO: unit configuration into package installation?
      units::units_options(set_units_mode = "standard")
      unit_database_path <- test_path("../../inst/lpj_units", "udunits2.xml")
      # TODO: install more LPJmL units next to gC gN?
      units::load_units_xml(unit_database_path)

      # Ensure the passed object is of class LPJmLData
      if (!inherits(lpj_dat, "LPJmLData")) {
        stop("Expected an LPJmLData object")
      }
      if (!methods::is(lpj_dat$meta, "LPJmLMetaData")) {
        stop("Meta data is missing")
      }
      # Copy the data from the provided LPJmLData object
      private$.data <- lpj_dat$data
      private$.meta <- lpj_dat$meta
      private$.grid <- lpj_dat$grid
      private$copy_unit_meta2array()
    },
    add = function(lpj_calc_obj) {
      if (!inherits(lpj_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data + lpj_calc_obj$.__data_with_unit__
    },
    subtract = function(lpj_calc_obj) {
      if (!inherits(lpj_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data - lpj_calc_obj$.__data_with_unit__
    },
    multiply = function(lpj_calc_obj) {
      if (!inherits(lpj_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data * lpj_calc_obj$.__data_with_unit__
      private$copy_unit_array2meta()
    },
    divide = function(lpj_calc_obj) {
      if (!inherits(lpj_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data / lpj_calc_obj$.__data_with_unit__
      private$copy_unit_array2meta()
    }
  ),

  private = list(
    copy_unit_meta2array = function() {
      unit <- private$.meta$unit
      private$.data <- set_units(private$.data, as_units(unit))
    },
    copy_unit_array2meta = function() {
      unit <- attr(self$.__data_with_unit__, "unit")
      if (identical(unit$numerator, character(0))) {
        numerator <- 1
      } else {
        numerator <- unit$numerator
      }
      denominator <- unit$denominator
      if (!identical(unit$denominator, character(0))) {
        denominator <- paste0("/", denominator)
      }
      unit_str <- paste0(numerator, denominator)
      private$.meta$.__set_attribute__("unit", unit_str)
    }
  ),

  active = list(
    data = function() {
      if (inherits(private$.data, "units")) {
        return(drop_units(private$.data))
      } else {
        return(private$.data)
      }
    },
    # TODO: is this the correct way to indicate function not meant for end user?
    .__data_with_unit__ = function() {
      return(private$.data)
    }
  )
)

#' @export
`+.LPJmLDataCalc` <- function(o1, o2) {
  sum <- o1$clone(deep = TRUE)
  sum$add(o2)
  return(sum)
}

#' @export
`-.LPJmLDataCalc` <- function(o1, o2) {
  sum <- o1$clone(deep = TRUE)
  sum$subtract(o2)
  return(sum)
}

#' @export
`*.LPJmLDataCalc` <- function(o1, o2) {
  product <- o1$clone(deep = TRUE)
  product$multiply(o2)
  return(product)
}

#' @export
`/.LPJmLDataCalc` <- function(o1, o2) {
  quotient <- o1$clone(deep = TRUE)
  quotient$divide(o2)
  return(quotient)
}

#' @export
as_LPJmLDataCalc <- function(obj) {
  calc <- LPJmLDataCalc$new(obj)
  return(calc)
}
