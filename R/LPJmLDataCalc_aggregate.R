#NTODO deal with double documentation of functions (here and method)
#' Aggregate an LPJmLDataCalc object
#'
#' Function to aggregate the full data of an [`LPJmLDataCalc`] object by
#' applying summary statistics along the cell and/or time dimensions.
#'
#' @param x [`LPJmLDataCalc`] object to be aggregated.
#' @param ref_area, string either `terr_area` or `cell_area`.
#'  Specifies the reference area to be used as
#'  a multiplier for the `weighted_sum` and `weighted_mean` aggregation methods.
#'  Should be the area of each cell on which the value "lives",
#'  assuming it has the given value only on that area and the value zero
#'  elsewhere (see mathematical support).
#' @param ... one or several key-value pairs. Keys represent the
#'  dimension to be aggregated and values specify the target aggregation units
#'  and the desired summary statistic.
#'
#'  Aggregation unit and statistic are given in a list,
#'  by the syntax `list(to = [aggregation unit], stat = [summary statistic])`.
#'
#'  If only a string is given instead of a list it is used as
#'  the aggregation unit and the summary statistic defualts to `mean` for time
#'  and `weighted_sum` for cell.
#'
#'  **Options for the cell dimension**
#'
#'  The aggregation units for the cell dimension can be either an
#'  LPJmlRegionData object or a string with
#'  the following options
#'  * `countries`: The regions defined in the countries of the world file.
#'  * `global`: A dynamically created region that fully contains all cells
#'    of the grid.
#'  The aggregation method for space has the following options:
#'  * `sum`: The values of all cells belonging to each region are summed up.
#'  If a cell belongs to a region only partially, we assume
#'  that the quantity is distributed uniformly over the cell area and
#'  multiply the value by the fraction of the cell that is part of the region
#'  before summing up.
#'  * `mean`: First sums up the values of all cells belonging to each region
#'  as described for `sum` and then divides by the number of cells belonging to
#'  the region. Again we account for partial belonging of cells to regions
#'  (if it exists) by only counting the fraction of the cell that is part of
#'  the region in the divisor.
#'  * `weighted_sum`: Similar to the `sum` option but multiplies the value of
#'  each cell by a reference area before summing up. The reference area
#'  default is the `terr_area` output which needs to exist in the same directory
#'  as the output to be aggregated. Other reference areas can be specified
#'  by setting the `reference_area` parameter.
#'  * `weighted_mean`: Similar to the `mean` option but multiplies the value of
#'  each cell by a reference area before summing up. Also,
#'  the resulting sum is then divided by the total reference area of each
#'  region instead of the number of cells.
#'
#'  **Options for the time dimension**
#'
#'  For the time dimension these aggregation units are available:
#'  * `sim_period`: The full simulation period.
#'  * `years`: Aggregates the data to annual values.
#'
#'  The only available aggregation method is `mean`
#'  which takes the unweighted mean of the values.
#'
#' @return An aggregated [`LPJmLDataCalc`] object.
#'
#' @examples
#' \dontrun{
#' # Example 1
#' # Load an example LPJmLDataCalc object
#' soiln <- load_soiln_calc()
#'
#' # Aggregate the data to countries of the world
#' soiln_countries <- aggregate(soiln, cell = "countries")
#'
#' soiln_countries$data # look at country time series
#'
#' # Example 2
#' # Load an example LPJmLDataCalc object
#' soiln <- load_soiln_calc()
#'
#' # Aggregate the to global region
#' soiln <- aggregate(soiln, cell = list(to = "global", stat = "weighted_sum"))
#'
#' soiln$data # look at global time series
#'
#' # Example 3
#' # Load an example LPJmLDataCalc object
#' soiln <- load_soiln_calc()
#'
#' # Take the mean of the data over the full simulation period
#' # and a weighted mean over the cells
#' soiln <- aggregate(soiln, time = "sim_period",
#'                    cell = list(to = "global", stat = "weighted_mean"))
#'
#' # Look at the resulting value
#' soiln$data
#' }
#' @export
aggregate <-
  function(x, ref_area = "terr_area", ...) {
    if (!inherits(x, "LPJmLDataCalc")) {
      stop("Expected LPJmLDataCalc")
    }
    y <- x$clone(deep = TRUE)
    y$aggregate(ref_area, ...)
    return(y)
  }

# NTODO: automatic selection of referencne area base on meta data json
LPJmLDataCalc$set(
  "private",
  ".__aggregate__",
  function(ref_area, ...) {

    subset_list <- list(...)

    if ("cell" %in% names(subset_list)) {
      if (!is.null(private$.meta$space_aggregation)) {
        stop("LPJmLDataCalc object is already aggregated")
      }
      spatial_agg_units <- get_tar_aggregation_unit(subset_list["cell"])
      spatial_agg_method <- get_summary_stat(subset_list["cell"])
    }

    if ("time" %in% names(subset_list)) {
      if (!is.null(private$.meta$time_aggregation)) {
        stop("LPJmLDataCalc object is already aggregated")
      }
      temporal_agg_units <- get_tar_aggregation_unit(subset_list["time"])
      temporal_agg_method <- get_summary_stat(subset_list["time"])
    }

    if (exists("spatial_agg_method")) {
      private$.__aggregate_space__(spatial_agg_units,
                                   spatial_agg_method,
                                   ref_area)
    }

    if (exists("temporal_agg_method")) {
      private$.__aggregate_time__(temporal_agg_units,
                                  temporal_agg_method)
    }
  }
)

# helper function to get the aggregation unit from a key-value pair
# passed to the aggregate method
get_tar_aggregation_unit <- function(argument) {
  dimension <- names(argument) # get the dimension name
  # get the aggregation specification of that dimension
  # which is the value of the key-value pair
  specifier <- argument[[dimension]]
  if (is.character(specifier) || inherits(specifier, "LPJmLRegionData")) {
    to <- specifier
  } else if (is.list(specifier)) {
    if ("to" %in% names(specifier)) {
      to <- specifier$to
    } else {
      stop("Missing 'to' key in the aggregation specifier")
    }
  } else {
    stop("Expected string or list as value for ", dimension, " key")
  }
  if (dimension == "cell") {
    if (!(inherits(to, "LPJmLRegionData") || to %in% c("countries", "global"))) { # nolint
      stop("Invalid aggregation unit for cell dimension")
    }
  } else if (dimension == "time") {
    if (!to %in% c("sim_period", "years")) {
      stop("Invalid aggregation unit for time dimension")
    }
  }
  return(to)
}

# helper function to get the summary statistic from a key-value pair
# passed to the aggregate method
get_summary_stat <- function(argument) {
  dimension <- names(argument)
  # get the aggregation specification of that dimension
  # which is the value of the key-value pair
  specifier <- argument[[dimension]]
  if (is.character(specifier) || inherits(specifier, "LPJmLRegionData")) {
    # the default statistic for cell dimension is weighted_sum
    # and for time dimension it is mean
    stat <- if (dimension == "cell") "weighted_sum" else "mean"
  } else if (is.list(specifier)) {
    if ("stat" %in% names(specifier)) {
      stat <- specifier$stat
    } else {
      stop("Missing 'stat' key in the aggregation specifier")
    }
  } else {
    stop("Expected string or list as value for ", dimension, " key")
  }
  if (dimension == "cell") {
    if (!stat %in% c("sum", "mean", "weighted_sum", "weighted_mean")) {
      stop("Invalid aggregation method for cell dimension")
    }
  } else if (dimension == "time") {
    if (!stat %in% c("mean")) {
      stop("Invalid aggregation method for time dimension")
    }
  }
  return(stat)
}

LPJmLDataCalc$set(
  "private",
  ".__aggregate_space__",
  function(spatial_agg_units,
           spatial_agg_method,
           ref_area) {

    # the grid of the aggregation regions must match the grid of the data
    # the grid is also needed to calculate the supporting cell areas
    # and construct dynamic regions
    self$add_grid()
    private$.grid$subset(cell = dimnames(self$data)[["cell"]]) # subset the grid

    # select the correct LPJmLRegionData object as aggregation unit
    if (inherits(spatial_agg_units, "LPJmLRegionData")) {
      aggregation_regions <- spatial_agg_units
    } else if (is.character(spatial_agg_units)) {
      if (spatial_agg_units == "countries") {
        aggregation_regions <- read_cow_regions()
      } else if (spatial_agg_units == "global") {
        aggregation_regions <- build_global_region(private$.grid)
      }
    }

    # select the aggregation method and perform aggregation
    if (spatial_agg_method == "sum") {
      private$.__sum_up_regions__(aggregation_regions)
    } else if (spatial_agg_method == "mean") {
      private$.__sum_up_regions__(aggregation_regions)
      cell_p_reg <- aggregation_regions$get_ncells_per_region()
      self$.divide(cell_p_reg)
    } else if (spatial_agg_method == "weighted_sum") {
      # weight by cell area
      cell_areas <- private$.__get_ref_area__(ref_area)
      self$.multiply(cell_areas)
      # sum up regions
      private$.__sum_up_regions__(aggregation_regions)
    } else if (spatial_agg_method == "weighted_mean") {
      # weight by cell area
      cell_areas <- private$.__get_ref_area__(ref_area)
      self$.multiply(cell_areas)
      # sum up regions
      private$.__sum_up_regions__(aggregation_regions)
      # sum up supporting area of each region
      cell_areas$aggregate(cell = list(to = aggregation_regions, stat = "sum"))
      # divide by total cell area
      self$.divide(cell_areas)
    } else {
      stop("Unknown aggregation method")
    }

    # record aggregation in meta data
    private$.meta$.__set_space_aggregation__(spatial_agg_method)

  }
)

LPJmLDataCalc$set(
  "private",
  ".__aggregate_time__",
  function(temporal_agg_units,
           temporal_agg_method) {

    if (temporal_agg_units == "sim_period") {

      # save dimnames, dimensions
      dimnames <- dimnames(private$.data)
      dim_space <- dim(private$.data)[1]
      dim_bands <- dim(private$.data)[3]

      # fast temporal mean function
      time_mean <- function(x) {
        # move second dim to last dim
        x <- base::aperm(x, c(1, 3, 2))
        # use highly optimized rowMeans function
        x <- base::rowMeans(x, dims = 2)
        # restore lost dimension
        dim(x) <- c(dim(x)[1], dim(x)[2], 1)
        # restore original order of dimensions
        x <- base::aperm(x, c(1, 3, 2))
        return(x)
      }

      # perform aggregation without loosing units
      private$.data <- units::keep_units(time_mean,
                                         private$.data)

      # restore dimnames, dimensions
      dim(private$.data) <- c(dim_space, time = 1, dim_bands)
      dimnames(private$.data) <-
        c(dimnames[1], list(time = "sim_period"), dimnames[3])
    } else if (temporal_agg_units == "years") {

      if (self$meta$nstep > 1) {
        # convert data array to one with dimension for year, month and day
        self$transform(to = "year_month_day")

        # restore the unit
        private$copy_unit_meta2array()

        data <- private$.data

        # function to average over years
        year_mean <- function(data) {
          # get the dimensions
          dims <- names(dimnames(data))
          # define the dimensions to keep
          dims_to_keep <- which(dims %in% c("region", "cell", "year", "band"))
          # create reordering of dimensions
          # the dimensions to keep should be at the beginning
          dim_order <- c(dims_to_keep, setdiff(1:length(dims), dims_to_keep)) # nolint
          # apply the reordering
          data <- aperm(data, dim_order)
          # use highly optimized rowMeans function to average away the
          # last dimensions not to keep
          data <- base::rowMeans(data, dims = 3)
          return(data)
        }

        # perform aggregation without loosing units
        data_agg <- units::keep_units(year_mean, data)

        # restore dimnames
        index <- which(names(dimnames(data_agg)) == "year")
        names(dimnames(data_agg))[index] <- "time"

        # NTODO: Adjust meta data. In particular adjust nstep, format of the time
        # dimname and the time time_format attribute
        # This will also allow plotting on a map etc

        private$.data <- data_agg
      } else {
        return()
      }

    }

    # record aggregation in meta data
    private$.meta$.__set_time_aggregation__(temporal_agg_method)
  }
)


# ----------------------.__sum_up_regions__----------------------------------- #

LPJmLDataCalc$set(
  "private",
  ".__sum_up_regions__",
  function(lpjml_regions) {
    # check if input is LPJmLRegionData
    if (!inherits(lpjml_regions, "LPJmLRegionData")) {
      stop("Expected an LPJmLRegionData object")
    }

    self$.check_internal_integrity()

    # check if grids of lpjml_regions and self are matching
    if (!identical(self$grid$data, lpjml_regions$grid$data) || # nolint start
        !identical(self$grid$meta$cellsize_lon,
                   lpjml_regions$grid$meta$cellsize_lon) ||
        !identical(self$grid$meta$cellsize_lat,
                   lpjml_regions$grid$meta$cellsize_lat)) { # nolint end
      stop("The grid of the LPJmLDataCalc object and the LPJmLRegionData
               object must match")
    }

    # perform aggregation
    region_matrix <- lpjml_regions$region_matrix
    list_of_aggr_bands <-
      lapply(1:private$.meta$nbands, function(band) {
        as.array(region_matrix %*% self$data[, , band]) # core aggregation step
      })
    aggr_data <- unlist(list_of_aggr_bands)
    first_band <- list_of_aggr_bands[[1]]

    # recover names of dimension vector
    region_names <- dimnames(first_band)[[1]]
    dim(aggr_data) <- c(region = dim(first_band)[1],
                        time = dim(first_band)[2],
                        band = private$.meta$nbands)

    # recover dimnames
    dimnames(aggr_data) <- list(
      region = region_names,
      time = dimnames(self$data)[["time"]],
      band = dimnames(self$data)[["band"]]
    )

    # set new attributes of the object
    units <- units(private$.data) # save units
    private$.data <- aggr_data
    private$.grid <- lpjml_regions
    private$.data <- units::set_units(private$.data, units) # restore units
  }
)

# ------------------- conversion of area density to cell totals -------------- #

LPJmLDataCalc$set(
  "private",
  ".__get_ref_area__",
  function(ref_area) {
    if (ref_area == "terr_area") {
      if (is.null(private$.meta$._data_dir_)) {
        stop("The data directory must be set to read the terr_area")
      }
      terr_area_orig <- read_file(private$.meta$._data_dir_, "terr_area")
      # make a new copy of the original terr_area, such that modifications
      # do not affect the original R6 object
      terr_area <- terr_area_orig$clone(deep = TRUE)
      cell_areas <- terr_area
    } else if (ref_area == "cell_area") {
      if (!inherits(self$grid, "LPJmLGridData")) {
        stop("A grid is needed to convert area density to total per cell")
      }
      cell_areas <- calc_cellarea_wrapper(private$.grid)
    } else {
      stop("The ref_area must be either 'terr_area' or
           'cell_area'")
    }
    cell_areas <- subset(cell_areas, cell = dimnames(self$data)[["cell"]])
    return(cell_areas)
  }
)

read_file <- function(searchdir, name, add_grid = TRUE) {
  # find path of file
  filename <- find_varfile(searchdir, name)

  # read file
  message(paste0(
    cli::col_blue(name),
    " read from ",
    sQuote(basename(filename))
  ))
  lpjml_calc <- read_io(filename)

  if (add_grid)
    lpjml_calc$add_grid()

  return(lpjml_calc)
}


# ----- utility functions -----
# Function calculates the cell areas in m^2 for a number of
# gridcells given in an LPJmLGridData object
# and returns them as an LPJmLDataCalc object.
# The function is a wrapper around the calc_cellarea function
# of the lpjmlkit package.
calc_cellarea_wrapper <- function(lpjml_grid) {

  cell_areas <- lpjmlkit::calc_cellarea(lpjml_grid, return_unit = "m2")

  ncell <- length(cell_areas)

  # set dims and dimnames
  dim(cell_areas) <- c(cell = ncell,
                       time = 1,
                       band = 1)
  dimnames(cell_areas) <- list(cell = dimnames(lpjml_grid$data)[["cell"]],
                               time = 1,
                               band = 1)

  # create meta data
  header <- create_header(
    cellsize_lon = lpjml_grid$meta$cellsize_lon,
    cellsize_lat = lpjml_grid$meta$cellsize_lat,
    ncell = ncell,
    nbands = 1,
    verbose = FALSE
  )
  meta <- lpjmlkit::LPJmLMetaData$new(header, list(unit = "m2",
                                                   variable = "cell area"))

  # combine array and metadata to LPJmLData object
  lpjml_calc <- lpjmlkit::LPJmLData$new(cell_areas, meta)

  # add grid
  lpjml_calc$.__set_grid__(lpjml_grid)

  # coerce to LPJmLDataCalc
  lpjml_calc <- .as_LPJmLDataCalc(lpjml_calc)

  return(lpjml_calc)
}
