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

  inherit = lpjmlkit:::LPJmLData,

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
    #' Function to aggregate an [`LPJmLDataCalc`] object in space.
    #' Different aggregation methods can be used.
    #'
    #' @param lpjml_calc [`LPJmLDataCalc`] object to be aggregated
    #' @param space The spatial aggregation units. Can be either a string
    #' indicating regions of the earth surface or an [`LPJmLRegionData`] object.
    #' Currently the following strings are available:
    #' - `cow_regions`: The regions defined by the Correlates of War project
    #' - `global`: A dynamically created region that fully contains all cells
    #' of the grid.
    #' @param method String that specifies spatial method of aggregation.
    #' Current only the following options are available:
    #' - `sum`: The values of all cells belonging to each region are summed up.
    #' If a cell belongs to a region only partially, we assume
    #' that the quantity is distributed uniformly over the cell area and
    #' multiply the value by the fraction of the cell that is part of the region
    #' before summing up.
    #' - `integral`: Given an area density, the integral over each region is
    #' calculated. First, the area density is converted to a total per cell by
    #' multiplying the value by the supporting cell areas.
    #' The integral is then calculated
    #' by summing up the values of all cells belonging to each region, as
    #' described for `sum`. This option expects that the stored quantity is an
    #' area density, i.e. `m^2` is par of the denominator of the unit.
    #' - `area_mean`: Calculates the mean value over the supporting areas of
    #' each region. This option expects that the stored quantity is an
    #' area density, i.e. `m^2` is par of the denominator of the unit.
    #' @param support_of_area_dens Specifies the areas of each cell on which
    #' the area density is supported. See [`LPJmLDataCalc`] method
    #' area_dens2cell_values.
    #'
    #' @return An aggregated [`LPJmLDataCalc`] object.
    #'
    aggregate = function(space = "cow_regions",
                         method = "sum",
                         support_of_area_dens = "terr_area") {
      private$.__aggregate__(space, method, support_of_area_dens)
    },

    #' @description
    #' Convert data storing a quantity given as an area density
    #' (per square meter)
    #' to the total amounts of that quantity present in each cell
    #' by multiplying the values with cell areas.
    #' @param support_of_area_dens Specifies the cell areas to be used as a
    #' multiplier. The underlying assumption is that the given area density
    #' "lives" (in mathematical terms "is supported") only on this area.
    #' More precisely the area density is assumed to have the given constant
    #' value on this area but is zero outside of it.
    #' The following options are available:
    #' - `terr_area`: The terrestrial area of the earth including inland water
    #' but excluding oceans. This relies on the output "terr_area" to be
    #' present in the directory of the source file of the LPJmLDataCalc object.
    #' - `full_cell_area` The complete area of each cell including oceans.
    #' @seealso \link[lpjmlkit]{calc_cellarea}
    #'
    area_dens2cell_values = function(support_of_area_dens = "terr_area") {
      private$.__area_dens2cell_totals__(support_of_area_dens)
      return(invisible(self))
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
    #' Sum up cell values in each region
    #' !Internal method only to be used for package development!
    #' @param lpjml_regions An `LPJmLRegionData` object.
    .sum_up_regions = function(lpjml_regions) {
      private$.__sum_up_regions__(lpjml_regions)
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
      # TODO: is this the correct way to indicate function not for end user?
      return(private$.data)
    }
  )
)

# ---------------------- internal integrity checking ------------------------- #
# TODO: account for time transformed lpjmldatacalc objects
LPJmLDataCalc$set("private",
                  ".__check_internal_integrity__",
                  function() {
      # nolint:object_name_linter
      # check if data and meta number of bands is consistent
      nbands_meta <- private$.meta$nbands
      nbands_array <- dim(private$.data)["band"]
      if (!(nbands_meta == nbands_array)) {
        stop("Number of bands in meta data is inconsistent with data array")
      }
      # check if data and meta number of cells is consistent
      ncells_meta <- private$.meta$ncell
      ncells_array <- dim(private$.data)["cell"]
      if (!(ncells_meta == ncells_array)) {
        stop("Number of cells in meta data is inconsistent with data array")
      }
      # check if data and meta number of timesteps is consistent
      ntimesteps_meta <- private$.meta$nyear * private$.meta$nstep
      ntimesteps_array <- dim(private$.data)["time"]
      if (!(ntimesteps_meta == ntimesteps_array)) {
        stop("Total number of timesteps in meta data is inconsistent with data array") #nolint
      }
  }
)

# ----------------------------- unit handling  ------------------------------- #
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
                    unit <- insert_caret(private$.meta$unit)
                    private$.data <- units::set_units(private$.data,
                                                      as_units(unit))
                  })


# Copy the unit attribute from the units data array to the meta data
LPJmLDataCalc$set("private", "copy_unit_array2meta",
                  function() {
                    deparsed_unit <- units::deparse_unit(private$.data)
                    private$.meta$.__set_attribute__("unit", deparsed_unit)
                  })


# ----------------- basic arithmetic operations ------------------------------ #

# ----- utility functions ----- #

# python inspired broadcasting function to make operations of arrays with
# different dimensions possible
broadcast_right_operand <-
  function(left_operand,
           right_operand) {
    left_operand_dims <- dim(left_operand)
    right_operand_dims <- dim(right_operand)
    if (length(left_operand_dims) != 3 || length(right_operand_dims) != 3) {
      stop("both operands must have three dimensions (cell, time, band)")
    }

    # expand first dimension
    if (right_operand_dims[1] == 1) {
      right_operand <-
        right_operand[rep(1, left_operand_dims[1]), , , drop = FALSE]
    }
    # expand second dimension
    if (right_operand_dims[2] == 1) {
      right_operand <-
        right_operand[, rep(1, left_operand_dims[2]), , drop = FALSE]
    }
    # expand third dimension
    if (right_operand_dims[3] == 1) {
      right_operand <-
        right_operand[, , rep(1, left_operand_dims[3]), drop = FALSE]
    }

    return(right_operand)
  }

# check if something is a scalar
is_scalar <- function(x) {
  length(x) == 1 & is.numeric(x)
}

# process second operand of arithmetic operations
process_second_op <- function(first_operand, sec_operand) {
  # check if second operand is a scalar or an LPJmLDataCalc object
  if (!inherits(sec_operand, "LPJmLDataCalc") && !is_scalar(sec_operand)) {
    stop("Expected an LPJmLDataCalc object or scalar value")
  }
  if (inherits(sec_operand, "LPJmLDataCalc")) {
    sec_operand <- sec_operand$.data_with_unit
    sec_operand <-
      broadcast_right_operand(first_operand, sec_operand)
  } else {
    if (!inherits(sec_operand, "numeric")) {
      stop("Only simple numeric values without a class are allowed as scalars.")
    }
  }
  return(sec_operand)
}

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

LPJmLDataCalc$set("private", ".__add__",
                  function(sec_operand) {
                    # nolint:object_linter_name
                    sec_operand <- process_second_op(private$.data, sec_operand)
                    if (is_scalar(sec_operand)) {
                      sec_operand <- units::set_units(sec_operand,
                                                      units(private$.data))
                      print("The added scalar is assumed to have the same unit as the LPJmLDataCalc object") #nolint
                    }
                    private$.data <-
                      private$.data + sec_operand
                    private$copy_unit_array2meta()
                  })

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


LPJmLDataCalc$set("private", ".__subtract__", function(sec_operand) {
  #nolint:object_linter_name
  sec_operand <- process_second_op(private$.data, sec_operand)
  if (is_scalar(sec_operand)) {
    sec_operand <- units::set_units(sec_operand, units(private$.data))
    print("The subtracted scalar is assumed to have the same unit as the LPJmLDataCalc object") # nolint
  }
  private$.data <-
    private$.data - sec_operand
  private$copy_unit_array2meta()
})


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

LPJmLDataCalc$set("private", ".__multiply__",
                  function(sec_operand) {
                    #nolint:object_linter_name
                    sec_operand <- process_second_op(private$.data, sec_operand)
                    private$.data <-
                      private$.data * sec_operand
                    private$copy_unit_array2meta()
                  })

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

LPJmLDataCalc$set("private", ".__divide__", function(sec_operand) {
  #nolint:object_linter_name
  sec_operand <- process_second_op(private$.data, sec_operand)
  private$.data <-
    private$.data / sec_operand
  private$copy_unit_array2meta()
})

# -------------- interfaces to LPJmLData objects and functions --------------- #
# ---- initialization -----
LPJmLDataCalc$set("private", ".initialize",  function(lpjml_data) {
  # TODO: unit configuration into package installation?
  units::units_options(set_units_mode = "standard")
  unit_database_path <- system.file("lpjml_units",
                                    "udunits2.xml",
                                    package = "lpjmlstats")
  # TODO: install more LPJmL units next to gC gN?
  units::load_units_xml(unit_database_path)

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

  # TODO: check if data has the correct dimension order
  # something alogn the lines of:
  # if (!names(dim(lpjml_data$data))[1] == "cell" || #nolint
  #     !names(dim(lpjml_data$data))[2] == "time" || #nolint
  #     !names(dim(lpjml_data$data))[3] == "band"){  #nolint
  #   stop("Currently data must have the following order of dimensions: 1. cell, 2. time, 3. band") #nolint
  # }


  # Create a new meta enhanced LPJmLMetaDataCalc object
  meta_calc <-
    lpjmlstats:::LPJmLMetaDataCalc$new(lpjml_data$meta)

  # Copy the data from the provided LPJmLData object
  private$.data <- lpjml_data$data
  private$.meta <- meta_calc
  private$.grid <- lpjml_data$grid
  private$copy_unit_meta2array()
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
                        lpjmlkit:::LPJmLData$new(self$data, self$meta)
                      lpjml_dat$.__set_grid__(self$grid)
                      lpjml_dat$plot(...)
                    }
                  })

LPJmLDataCalc$set("private", ".__plot_aggregated__", #nolint:object_name_linter
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
                      lpjmlkit:::LPJmLData$new(disaggr_data, private$.meta)
                    plot_obj$.__set_grid__(private$.grid$grid)

                    # plot
                    lpjmlkit:::plot.LPJmLData(plot_obj, ...)
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
    lpjml_calc <- as_LPJmLDataCalc(lpjml_dat)
  } else if (output_type == "LPJmLData") {
    lpjml_calc <- lpjml_dat
  } else {
    stop("Unknown output type")
  }
  return(lpjml_calc)
}

# ----- as_LPJmLDataCalc -----
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

# ----- subset -----
#' Subset an LPJmLDataCalc object
#'
#' Function to subset an LPJmLDataCalc object. The function acts as a wrapper
#' of \link[lpjmlkit]{subset.LPJmLData} from lpjmlkit, but outputs an
#' [`LPJmLDataCalc`] object.

#' @param x LPJmLDataCalc object.
#' @param ... Parameters that are passed to \link[lpjmlkit]{subset.LPJmLData}.

#' @return An [`LPJmLDataCalc`] object.
#' @export

subset.LPJmLDataCalc <- function(x, ...) {
  lpjml_dat <- lpjmlkit:::subset.LPJmLData(x, ...)
  return(as_LPJmLDataCalc(lpjml_dat))
}
