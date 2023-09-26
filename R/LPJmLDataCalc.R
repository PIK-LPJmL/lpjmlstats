#' @title LPJmLDataCalc
#'
#' @importFrom units set_units as_units drop_units deparse_unit
#' @import lpjmlkit
#'
#' @description
#' An extended LPJmLData class that enables calculations and statistics.
#'

#' @export

LPJmLDataCalc <- R6::R6Class( # nolint:object_linter_name

  classname = "LPJmLDataCalc",

  inherit = lpjmlkit:::LPJmLData,

  public = list(
    # Create a new LPJmLDataCalc object; to be used internally or explicitly
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_data an LPJmLData object.
    initialize = function(lpjml_data) {
      # TODO: unit configuration into package installation?
      units::units_options(set_units_mode = "standard")
      unit_database_path <- system.file(
        "lpjml_units",
        "udunits2.xml",
        package = "lpjmlstats"
      )
      # TODO: install more LPJmL units next to gC gN?
      units::load_units_xml(unit_database_path)

      # Ensure the passed object is of class LPJmLData
      if (!inherits(lpjml_data, "LPJmLData")) {
        stop("Expected an LPJmLData object")
      }
      if (!methods::is(lpjml_data$meta, "LPJmLMetaData")) {
        stop("Meta data is missing")
      }
      # Copy the data from the provided LPJmLData object
      private$.data <- lpjml_data$data
      private$.meta <- lpjml_data$meta
      private$.grid <- lpjml_data$grid
      private$copy_unit_meta2array()
    },


    #' Addition of two LPJmLDataCalc objects
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_calc_obj An `LPJmLData` object.
    add = function(lpjml_calc_obj) {
      if (!inherits(lpjml_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data + lpjml_calc_obj$.__data_with_unit__
    },


    #' Subtraction of two LPJmLDataCalc objects
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_calc_obj An `LPJmLData` object.
    subtract = function(lpjml_calc_obj) {
      if (!inherits(lpjml_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data - lpjml_calc_obj$.__data_with_unit__
    },


    #' Multiplication of two LPJmLDataCalc objects
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_calc_obj An `LPJmLData` object.
    multiply = function(lpjml_calc_obj) {
      if (!inherits(lpjml_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data * lpjml_calc_obj$.__data_with_unit__
      private$copy_unit_array2meta()
    },

    #' Division of two LPJmLDataCalc objects
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_calc_obj An `LPJmLData` object.
    divide = function(lpjml_calc_obj) {
      if (!inherits(lpjml_calc_obj, "LPJmLDataCalc")) {
        stop("Expected an LPJmLDataCalc object")
      }
      private$.data <- private$.data / lpjml_calc_obj$.__data_with_unit__
      private$copy_unit_array2meta()
    }
  ),

  private = list(

    # Copy the unit attribute from the meta data to the units data array
    copy_unit_meta2array = function() {
      unit <- private$.meta$unit
      private$.data <- set_units(private$.data, as_units(unit))
    },

    # Copy the unit attribute from the units data array to the meta data
    copy_unit_array2meta = function() {
      unit <- attr(self$.__data_with_unit__, "units")
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


#' Addition of two LPJmLDataCalc objects
#' Add an LPJmLDataCalc object to another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`+.LPJmLDataCalc` <- function(o1, o2) {
  sum <- o1$clone(deep = TRUE)
  sum$add(o2)
  return(sum)
}


#' Subtraction of two LPJmLDataCalc objects
#' Subtract an LPJmLDataCalc object from another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`-.LPJmLDataCalc` <- function(o1, o2) {
  sum <- o1$clone(deep = TRUE)
  sum$subtract(o2)
  return(sum)
}


#' Multiplication of two LPJmLDataCalc objects
#' Multiply an LPJmLDataCalc object by another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`*.LPJmLDataCalc` <- function(o1, o2) {
  product <- o1$clone(deep = TRUE)
  product$multiply(o2)
  return(product)
}


#' Division of two LPJmLDataCalc objects
#' Divide an LPJmLDataCalc object by another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`/.LPJmLDataCalc` <- function(o1, o2) {
  quotient <- o1$clone(deep = TRUE)
  quotient$divide(o2)
  return(quotient)
}


#' Coerce an LPJmLData object into an LPJmLDataCalc object
#'
#' Function to coerce (convert) an [`LPJmLData`] object into an
#' LPJmLDataCalc object with extended functionality.
#'
#' @param obj LPJmLData object.
#'
#' @return An LPJmLDataCalc object.
#'
#' @md
#' @export
as_LPJmLDataCalc <- function(obj) { # nolint:object_linter_name
  calc <- LPJmLDataCalc$new(obj)
  return(calc)
}
