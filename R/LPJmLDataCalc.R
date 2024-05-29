#' @title LPJmLDataCalc
#'
#' @importFrom units set_units as_units drop_units deparse_unit
#' @import lpjmlkit
#' @importFrom abind abind
#' @importFrom Matrix sparseMatrix Matrix colSums
#'
#' @description
#' An extended LPJmLData class that enables arithmetic and statistics.
#'
#' @export

LPJmLDataCalc <- R6::R6Class( # nolint:object_linter_name

  classname = "LPJmLDataCalc",

  inherit = lpjmlkit::LPJmLData,

  public = list(
    #' @description
    #' Create a new LPJmLDataCalc object; to be used internally or explicitly
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_data an LPJmLData object.
    initialize = function(lpjml_data) {
      private$.initialize(lpjml_data)
    },

    #' @description
    #' See \link[lpjmlstats]{aggregate}.
    #' @param ref_area See \link[lpjmlstats]{aggregate}.
    #' @param ... See \link[lpjmlstats]{aggregate}.
    aggregate = function(ref_area = "terr_area", ...) {
      private$.__aggregate__(ref_area, ...)
    },

    #' @description
    #' Get the reference area of the LPJmLDataCalc object.
    #' For an area density variable the reference area should
    #' be the area of each cell on which the variable is defined.
    #' @param ref_area A string that can be
    #' - `terr_area` terrestrial area (land area including inland water bodies)
    #' - `cell_area` full area of each cell
    #' @return An [`LPJmLDataCalc`] object with the reference area as variable.
    get_ref_area = function(ref_area) {
      private$.__get_ref_area__(ref_area)
    },

    #' @description
    #' Plot an LPJmLDataCalc object
    #'
    #' The function acts a wrapper of \link[lpjmlkit]{plot.LPJmLData} from
    #' lpjmlkit, but allows for plotting data in more formats.
    #'
    #' In case of non-aggregated data
    #' \link[lpjmlkit]{plot.LPJmLData}
    #' is directly called. In case of aggregated data the
    #' value for each region is assigned to all pixels that belong
    #' to the region.
    #' If a pixel belong to a region only partially, the value is
    #' multiplied by the
    #' fraction of that pixel belonging to the region. If a pixel belongs to
    #' multiple regions, the sum of all respective region values
    #' (multiplied by the fractions) is taken.
    #' The pixel values are then again plotted with
    #' \link[lpjmlkit]{plot.LPJmLData}.
    #' @param ... Arguments passed to \link[lpjmlkit]{LPJmLData} plot method.
    #'
    plot = function(...) {
      private$.__plot__(...)
    },



    #' @description
    #' Check consistency of data and meta data
    #' !Internal method only to be used for package development!
    .check_internal_integrity = function() {
      private$.__check_internal_integrity__()
    },

    #' @description
    #' Plot aggregated data.
    #' Performs a very simple disaggregation to create LPJmLData obj
    #' that can be plotted with plot.LPJmLData.
    #' For each pixel the values of all regions that contain
    #' the pixel are multiplied by the fractions and summed up.
    #' @param ... Arguments to be passed to plot.LPJmLData
    .plot_aggregated = function(...) {
      private$.__plot_aggregated__(...)
    },

    #' @description
    #' Addition of two LPJmLDataCalc objects
    #' !Internal method only to be used for package development!
    #' @param lpjml_calc_obj An `LPJmLData` object.
    .add = function(lpjml_calc_obj) {
      private$.__add__(lpjml_calc_obj)
    },

    #' @description
    #' Subtraction of two LPJmLDataCalc objects
    #' !Internal method only to be used for package development!
    #' @param lpjml_calc_obj An `LPJmLData` object.
    .subtract = function(lpjml_calc_obj) {
      private$.__subtract__(lpjml_calc_obj)
    },

    #' @description
    #' Multiplication of two LPJmLDataCalc objects
    #' !Internal method only to be used for package development!
    #' @param lpjml_calc_obj An `LPJmLData` object.
    .multiply = function(lpjml_calc_obj) {
      private$.__multiply__(lpjml_calc_obj)
    },

    #' @description
    #' Division of two LPJmLDataCalc objects
    #' !Internal method only to be used for package development!
    #' @param lpjml_calc_obj An `LPJmLData` object.
    .divide = function(lpjml_calc_obj) {
      private$.__divide__(lpjml_calc_obj)
    },

    #' @description
    #' Unit conversion of LPJmLDataCalc object
    #' !Internal method only to be used for package development!
    #' @param unit A string with the unit to convert to.
    .convert_unit = function(unit) {
      private$.__convert_unit__(unit)
    },

    #' @description
    #' Set unit of LPJmLDataCalc object
    #' !Internal method only to be used for package development!
    #' @param unit_str A string with the unit to be set.
    .set_unit = function(unit_str) {
      private$.data <- units::set_units(private$.data, unit_str)
      private$copy_unit_array2meta()
    },

    #' @description
    #' Apply unit conversion from conversion table
    #' @param path_to_table A string with the path to the conversion table.
    apply_unit_conversion_table = function(path_to_table = NULL) {
      private$.__apply_unit_conversion_table__(path_to_table)
    },

    #' @description
    #' !Internal method only to be used for package development!
    #' Set the simulation identifier to the LPJmLDataCalc object.
    #' @param identifier A string with the identifier to set.
    set_sim_identifier = function(identifier) {
      private$.meta$.__set_sim_identifier__(identifier)
    },

    #' @description
    #' !Internal method only to be used for package development!
    #' Get the simulation identifier of the LPJmLDataCalc object.
    #' @return A string with the simulation identifier.
    get_sim_identifier = function() {
      private$.meta$.__get_sim_identifier__()
    },

    #' @description
    #' Add a grid to the LPJmLDataCalc object
    #' Wrapper for the `add_grid` method of the `LPJmLData` class.
    add_grid = function() {
      private$.__add_grid__()
    }
  ),

  active = list(
    #' @field data the data array
    data = function() {
      if (inherits(private$.data, "units")) {
        return(units::drop_units(private$.data))
      } else {
        return(private$.data)
      }
    },

    #' @field .data_with_unit
    #' Returns the internal enclosed unit object
    #' !Internal method only to be used for package development!
    .data_with_unit = function() {
      # NTODO: is this the correct way to indicate function not for end user?
      return(private$.data)
    }
  )
)

# ---------------------- internal integrity checking ------------------------- #
# NTODO: account for time transformed lpjmldatacalc objects
LPJmLDataCalc$set( # nolint: object_name_linter.
  "private",
  ".__check_internal_integrity__",
  function() {
    # check if data and meta number of bands is consistent
    nbands_meta <- private$.meta$nbands
    nbands_array <- dim(private$.data)["band"]
    if (!(nbands_meta == nbands_array)) {
      stop("Number of bands in meta data is inconsistent with data array")
    }

    #check internal consistency of grid or region data object
    ncells_meta <- private$.meta$ncell
    if (inherits(private$.grid, "LPJmLGridData")) {
      if (!(private$.grid$meta$ncell == ncells_meta)) {
        stop("Number of cells in grid meta data
             is inconsistent withLPJmLDataCalc meta data")
      }
      if (!(private$.grid$meta$ncell == dim(private$.grid$data)[1])) {
        stop("Number of cells in grid meta data
             is inconsistent grid data array")
      }
    }
  }
)

# ----------------------------- unit handling  ------------------------------- #
keep_units_lpjml_calc <- function(lpjml_calc, fun) {
  # save units
  unit <- deparse_unit(lpjml_calc$.data_with_unit)
  # apply fun
  lpjml_calc <- fun(lpjml_calc)
  # restore unit
  lpjml_calc$.set_unit(unit)
  return(lpjml_calc)
}

# Copy the unit attribute from the meta data to the units data array
LPJmLDataCalc$set("private", "copy_unit_meta2array",
                  function() {
                    insert_caret <- function(input_string) {
                      # matches any number directly preceded by a letter i.e. m2
                      pattern <- "(?<=[a-zA-Z])([1-9]+)"
                      replacement <- "^\\1"
                      # e.g. m2 -> m^2
                      x <- gsub(pattern, replacement, input_string, perl = TRUE)
                      if (is.null(input_string)) {
                        x <- NULL
                      }
                      return(x)
                    }
                    set_minussign_to_nounit <- function(input_string) {
                      # i.e. "-" -> ""
                      x <- gsub("^-$", "", input_string)
                      if (is.null(input_string)) {
                        x <- NULL
                      }
                      return(x)
                    }
                    unit <- insert_caret(private$.meta$unit)
                    unit <- set_minussign_to_nounit(unit)
                    private$.data <- units::set_units(private$.data,
                                                      as_units(unit))
                  })


# Copy the unit attribute from the units data array to the meta data
LPJmLDataCalc$set("private", "copy_unit_array2meta",
                  function() {
                    deparsed_unit <- units::deparse_unit(private$.data)
                    private$.meta$.__set_attribute__("unit", deparsed_unit)
                  })

# Convert the units of the data array
LPJmLDataCalc$set("private", ".__convert_unit__",
                  function(unit) {
                    private$.data <- keep_dimnames_and_dims(private$.data,
                                                            units::set_units,
                                                            as_units(unit))
                    private$copy_unit_array2meta()
                  })

# function that allow to apply a function to array while keeping the
# dimnames and dims
keep_dimnames_and_dims <- function(x, fun, ...) {
  dimnames <- dimnames(x)
  dims <- dim(x)
  x <- fun(x, ...)
  dim(x) <- dims
  dimnames(x) <- dimnames
  return(x)
}

# Apply unit conversions of conversion table to data array
LPJmLDataCalc$set(
  "private",
  ".__apply_unit_conversion_table__",
  function(path_to_table) {

    # if no path to table is given, use the default table
    if (is.null(path_to_table)) {
      path_to_table <-
        getOption(
          "LPJmLDataCalc.unit_conversion_table",
          system.file("unit_conversions.csv",
                      package = "lpjmlstats")
        )
    }

    # Read the conversion table
    conversions <- read.csv(path_to_table, stringsAsFactors = FALSE)

    # Get the current unit of the data array
    current_unit <- units::deparse_unit(private$.data)
    # Check if current unit is in the table
    if (current_unit %in% conversions$original_unit) {
      # Get the corresponding converted unit
      converted_unit <-
        conversions$converted_unit[conversions$original_unit == current_unit]
      # Convert the unit
      private$.__convert_unit__(converted_unit)
    }
  }
)



# ----------------- basic arithmetic operations ------------------------------ #

# ----- utility functions ----- #

LPJmLDataCalc$set(
  "private",
  ".__apply_operator__",
  function(sec_operand, operator) {
    tar_dim <- dim(private$.data)

    # this function is used to expand the second operand
    # to the same dimension as the first operand
    # the idea comes from the base::sweep function
    expand <- function(x) {
      cur_dim <- dim(x) # current dimension of second operand
      keep <- which(tar_dim == cur_dim) # which dimensions are the same
      # check if dimensions are incompatible
      # this is the case if there is a non matching dimension that
      # has more than one element (i.e. it is not clear how to expand it).
      # Also, an extra check is needed for the case that no dimensions are
      # the same which is only allowed if the second operand is a scalar.
      if (any(cur_dim[-keep] != 1) || (length(keep) == 0 && any(cur_dim != 1)))
        stop("Dimensions of second operand do not
              match dimensions of first operand")
      if (length(keep) > 0)
        # put the dimensions to keep in front and append the rest
        perm <- c(keep, seq_along(tar_dim)[-keep])
      else
        # if there are only dimensions to expand, just append everything
        perm <- seq_along(tar_dim)

      # reverse permutation
      reverse_perm <- order(perm)

      # this will look at the array as a vector and
      # automatically recycle it to the
      # required length as well as
      # collapse dimensions with only one element
      x <- array(x, tar_dim[perm])

      # reverse permutation to get back to original order
      aperm(x, reverse_perm)
    }

    # expand second operand while keeping units if present
    if (inherits(sec_operand, "units"))
      exp_sec_op <- units::keep_units(expand, sec_operand)
    else
      exp_sec_op <- expand(sec_operand)

    # apply operator
    private$.data <-
      do.call(operator,
              list(private$.data,
                   exp_sec_op))
  }
)

# ------ addition ------
#' Addition of two LPJmLDataCalc objects
#' @description
#'
#' Add an LPJmLDataCalc object to another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`+.LPJmLDataCalc` <- function(o1, o2) {
  sum <- o1$clone(deep = TRUE)
  sum$.add(o2)
  return(sum)
}

LPJmLDataCalc$set(
  "private",
  ".__add__",
  function(sec_operand) {
    if (is.numeric(sec_operand)) {
      sec_operand <- units::set_units(sec_operand,
                                      units(private$.data))
      print("The added numeric vector is assumed to have the
             same unit as the LPJmLDataCalc object")
    }
    if (inherits(sec_operand, "LPJmLDataCalc")) {
      sec_operand <- sec_operand$.data_with_unit
    }
    private$.data <- private$.__apply_operator__(sec_operand, `+`)
    private$copy_unit_array2meta()
  }
)

# ------ subtraction ------
#' Subtraction of two LPJmLDataCalc objects
#' @description
#'
#' Subtract an LPJmLDataCalc object from another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`-.LPJmLDataCalc` <- function(o1, o2) {
  sum <- o1$clone(deep = TRUE)
  sum$.subtract(o2)
  return(sum)
}


LPJmLDataCalc$set(
  "private",
  ".__subtract__",
  function(sec_operand) {
    if (is.numeric(sec_operand)) {
      sec_operand <- units::set_units(sec_operand,
                                      units(private$.data))
      print("The subtracted numeric vector is assumed
            to have the same unit as the LPJmLDataCalc object")
    }
    if (inherits(sec_operand, "LPJmLDataCalc")) {
      sec_operand <- sec_operand$.data_with_unit
    }
    private$.data <- private$.__apply_operator__(sec_operand, `-`)
    private$copy_unit_array2meta()
  }
)


# ------ multiplication ------
#' Multiplication of two LPJmLDataCalc objects
#' @description
#' Multiply an LPJmLDataCalc object by another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`*.LPJmLDataCalc` <- function(o1, o2) {
  product <- o1$clone(deep = TRUE)
  product$.multiply(o2)
  return(product)
}

LPJmLDataCalc$set(
  "private",
  ".__multiply__",
  function(sec_operand) {
    if (inherits(sec_operand, "LPJmLDataCalc")) {
      sec_operand <- sec_operand$.data_with_unit
    }
    private$.data <- private$.__apply_operator__(sec_operand, `*`)
    private$copy_unit_array2meta()
  }
)

# ------ division ------
#' Division of two LPJmLDataCalc objects
#' @description
#'
#' Divide an LPJmLDataCalc object by another LPJmLDataCalc object
#'
#' @param o1 An `LPJmLDataCalc` object.
#' @param o2 An `LPJmLDataCalc` object.
#'
#' @return An `LPJmLDataCalc` object.
#' @export
`/.LPJmLDataCalc` <- function(o1, o2) {
  quotient <- o1$clone(deep = TRUE)
  quotient$.divide(o2)
  return(quotient)
}

LPJmLDataCalc$set(
  "private",
  ".__divide__",
  function(sec_operand) {
    if (inherits(sec_operand, "LPJmLDataCalc")) {
      sec_operand <- sec_operand$.data_with_unit
    }
    private$.data <- private$.__apply_operator__(sec_operand, `/`)
    private$copy_unit_array2meta()
  }
)


# -------------- interfaces to LPJmLData objects and functions --------------- #
# ---- initialization -----
LPJmLDataCalc$set("private", ".initialize",  function(lpjml_data) {

  # Ensure the passed object has the correct format
  if (!inherits(lpjml_data, "LPJmLData")) {
    stop("Expected an LPJmLData object")
  }
  if (!methods::is(lpjml_data$meta, "LPJmLMetaData")) {
    stop("Meta data is missing")
  }
  if (!lpjml_data$meta$._space_format_ == "cell") {
    stop("Currently only cell format is supported")
  }

  # Ensure the data has the correct format
  # NTODO: modify tests such that they run through with this
  # if (!names(dim(lpjml_data$data))[1] == "cell" || # nolint start
  #     !names(dim(lpjml_data$data))[2] == "time" ||
  #     !names(dim(lpjml_data$data))[3] == "band"){
  #    stop("The data must have the following order of dimensions: 1. cell, 2. time, 3. band")
  # } # nolint end


  # Create a new meta enhanced LPJmLMetaDataCalc object
  meta_calc <- LPJmLMetaDataCalc$new(lpjml_data$meta)

  # Copy the data from the provided LPJmLData object
  private$.data <- lpjml_data$data
  private$.meta <- meta_calc
  private$.grid <- lpjml_data$grid
  private$copy_unit_meta2array()
  # the following is to consistently have the unit formatting from units package
  private$copy_unit_array2meta()
})



# ---- plottting -----
#' Plot an LPJmLDataCalc object
#'
#' The function acts a wrapper of \link[lpjmlkit]{plot.LPJmLData} from lpjmlkit,
#' but allows for plotting data in more formats.
#'
#' @param x LPJmLDataCalc object. In case of non-aggregated data
#' \link[lpjmlkit]{plot.LPJmLData}
#' is directly called. In case of aggregated data the
#' value for each region is assigned to all pixels that belong to the region.
#' If a pixel belong to a region only partially, the value is multiplied by the
#' fraction of that pixel belonging to the region. If a pixel belongs to
#' multiple regions, the sum of all respective region values
#' (multiplied by the fractions) is taken.
#' The pixel values are then again plotted with \link[lpjmlkit]{plot.LPJmLData}.
#' @param ... Arguments passed to \link[lpjmlkit]{LPJmLData} plot method.
#'
plot.LPJmLDataCalc <- function(x, ...) {
  x$plot(...)
}

LPJmLDataCalc$set("private", ".__plot__",
                  function(...) {
                    if ("region" %in% names(dimnames(self$data))) {
                      # plotting of non-aggregated data
                      self$.plot_aggregated(...)
                    } else {
                      # plotting of aggregated data
                      lpjml_dat <-
                        lpjmlkit::LPJmLData$new(self$data, self$meta)
                      lpjml_dat$.__set_grid__(self$grid)
                      lpjml_dat$plot(...)
                    }
                  })

LPJmLDataCalc$set("private", ".__plot_aggregated__", # nolint: object_name_linter.
                  function(...) {
                    region_matrix <- Matrix::t(private$.grid$region_matrix)
                    list_of_disaggr_bands <-
                      lapply(1:private$.meta$nbands, function(band) {
                        as.array(region_matrix %*% self$data[, , band])
                      })
                    disaggr_data <- abind(list_of_disaggr_bands, along = 3)

                    # recover dims and dimnames
                    dim(disaggr_data) <- c(
                      cell = unname(dim(disaggr_data)[1]),
                      time = unname(dim(disaggr_data)[2]),
                      band = unname(dim(disaggr_data)[3])
                    )
                    dimnames(disaggr_data) <-
                      list(
                        cell = seq_len(dim(disaggr_data)[1]),
                        time = dimnames(self$data)[[2]],
                        band = dimnames(self$data)[[3]]
                      )

                    # create dummy LPJmLData only for purpose of plotting
                    plot_obj <-
                      lpjmlkit::LPJmLData$new(disaggr_data, private$.meta)
                    plot_obj$.__set_grid__(private$.grid$grid)

                    # plot
                    lpjmlkit::plot.LPJmLData(plot_obj, ...)
                  })

# ----- read_io -----
#' Read in LPJmL input and output files as LPJmLDataCalc
#'
#' The function acts a wrapper of \link[lpjmlkit]{read_io} from lpjmlkit,
#' but outputs an [`LPJmLDataCalc`] object.
#'
#' @param ... Parameters that are passed to \link[lpjmlkit]{read_io}
#'
#' @param output_type Can be either `LPJmLDataCalc`or `LPJmLData`.
#'
#' @return An [`LPJmLDataCalc`] object
#'
#' @export

read_io <- function(..., output_type = "LPJmLDataCalc") {
  lpjml_dat <- lpjmlkit::read_io(...)
  if (output_type == "LPJmLDataCalc") {
    lpjml_calc <- .as_LPJmLDataCalc(lpjml_dat)
  } else if (output_type == "LPJmLData") {
    lpjml_calc <- lpjml_dat
  } else {
    stop("Unknown output type")
  }
  return(lpjml_calc)
}

# ----- .as_LPJmLDataCalc -----
#' Coerce an LPJmLData object into an LPJmLDataCalc object
#'
#' Function to coerce (convert) an [`LPJmLData`] object into an
#' LPJmLDataCalc object with extended functionality.
#'
#' @param obj LPJmLData object or an array with the following order of
#' dimensions: 1. space, 2. time, 3. band.
#'
#' @return An LPJmLDataCalc object.
#'
#' @md
#' @export

.as_LPJmLDataCalc <- function(obj) { # nolint:object_linter_name
  if (is.array(obj)) {

    # check if array has the correct dimensions
    if (length(dim(obj)) != 3) {
      stop("Array must have 3 dimensions. 1. space, 2. time, 3. band.")
    }

    header <- lpjmlkit::create_header(ncell = dim(obj)[1],
                                      nstep = dim(obj)[2],
                                      nbands = dim(obj)[3])

    meta <- lpjmlkit::LPJmLMetaData$new(header)

    obj <- lpjmlkit::LPJmLData$new(obj, meta)
  } else if (inherits(obj, "LPJmLData")) {
    obj <- obj
  } else {
    stop("Object must be either an LPJmLData object or an array.")
  }
  calc <- LPJmLDataCalc$new(obj)
  return(calc)
}

# ----- subset -----
#' Subset an LPJmLDataCalc object
#'
#' Function to subset an LPJmLDataCalc object. The function acts as a wrapper
#' of \link[lpjmlkit]{subset.LPJmLData} from lpjmlkit, but outputs an
#' [`LPJmLDataCalc`] object, in particular keeping its unit.

#' @param x LPJmLDataCalc object.
#' @param ... Parameters that are passed to \link[lpjmlkit]{subset.LPJmLData}.

#' @return An [`LPJmLDataCalc`] object.
#' @export

subset.LPJmLDataCalc <- function(x, ...) {
  lpjml_dat <- lpjmlkit::subset.LPJmLData(x, ...)
  return(.as_LPJmLDataCalc(lpjml_dat))
}



# ----- add_grid -----

LPJmLDataCalc$set(
  "private", ".__add_grid__",
  function(...) {

    # Skip if grid is already attached
    if (!is.null(private$.grid)) {
      return(invisible(self))
    }
    if (...length() == 0) {
      grid <- read_file(private$.meta$._data_dir_, "grid", add_grid = FALSE)
      # Add support for cell subsets. This is a rough filter since $subset
      #   does not say if cell is subsetted - but ok for now.
      if (private$.meta$._subset_space_) {
        cells <- self$dimnames()[["cell"]]
        grid <- subset(grid, cell = cells)
      }
    } else {
      # All arguments have to be provided manually to read_io.
      #   Ellipsis (...) does that.

      # Add support for cell subsets. This is a rough filter since $subset
      #   does not say if cell is subsetted - but ok for now.
      if (private$.meta$._subset_space_) {
        grid <- read_io(..., subset = list(cell = self$dimnames()[["cell"]]))
      } else {
        grid <- read_io(...)
      }
    }

    # make a copy to avoid modifying tho original
    grid_clone <- grid$clone(deep = TRUE)

    # Create LPJmLData object and bring together data and meta_data
    lpjml_grid <- lpjmlkit::LPJmLGridData$new(grid_clone)

    # set grid attribute
    self$.__set_grid__(lpjml_grid)
  }
)
