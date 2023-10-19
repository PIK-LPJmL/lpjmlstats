#TODO deal with double documentation of functions (here and method)
#' Aggregate an LPJmLDataCalc object
#'
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
#' by summing up the values of all cells belonging to each region, as described
#' for `sum`. This option expects that the stored quantity is an
#' area density, i.e. `m^2` is par of the denominator of the unit.
#' - `area_mean`: Calculates the mean value over the supporting areas of each
#' region. This option expects that the stored quantity is an
#' area density, i.e. `m^2` is par of the denominator of the unit.
#' @param support_of_area_dens Specifies the areas of each cell on which
#' the area density is supported. See [`LPJmLDataCalc`] method
#' area_dens2cell_values.
#'
#' @return An aggregated [`LPJmLDataCalc`] object.
#'
#' @export
aggregate <-
  function(lpjml_calc,
           space = "cow_regions",
           method = "sum",
           support_of_area_dens = "terr_area") {
    if (!inherits(lpjml_calc, "LPJmLDataCalc")) {
      stop("Expected LPJmLDataCalc")
    }
    lpjml_calc_agg <- lpjml_calc$clone(deep = TRUE)
    lpjml_calc_agg$aggregate(space, method, support_of_area_dens)
    return(lpjml_calc_agg)
  }

# TODO: automatic selection of cellarea support
LPJmLDataCalc$set("private",
                  ".__aggregate__",
                  function(space, method, support_of_area_dens) {

    if (private$.meta$aggregated) {
      stop("LPJmLDataCalc object is already aggregated")
    }

    # select the correct LPJmLRegionData object as aggregation unit
    if (inherits(space, "LPJmLRegionData")) {
      aggregation_regions <- space
    } else if (is.character(space)) {
      if (space == "cow_regions") {
        aggregation_regions <- read_lpjml_region_cow()
      } else if (space == "global") {
        aggregation_regions <- construct_lpjml_region_global(private$.grid)
      } else {
        stop("Unknown aggregation unit string")
      }
    } else {
      stop("Expects a string or LPJmLRegionData object as space parameter.")
    }

    # the grid of the aggregation regions must match the grid of the data
    # therefore it is needed
    self$add_grid()

    # select the aggregation method and perform aggregation
    if (method == "sum") {
      self$.sum_up_regions(aggregation_regions)
    } else if (method == "integral") {
      # weight by cell area
      cell_areas <- private$.__get_supporting_cell_areas__(support_of_area_dens)
      self$area_dens2cell_values(support_of_area_dens = cell_areas)
      # sum up regions
      self$.sum_up_regions(aggregation_regions)
    } else if (method == "area_mean") {
      # weight by cell area
      cell_areas <- private$.__get_supporting_cell_areas__(support_of_area_dens)
      self$area_dens2cell_values(support_of_area_dens = cell_areas)
      # sum up regions
      self$.sum_up_regions(aggregation_regions)
      # sum up supporting area of each region
      cell_areas$.sum_up_regions(aggregation_regions)
      # divide by total cell area
      self$.divide(cell_areas)
    } else {
      stop("Unknown aggregation method")
    }
  }
)

# ----------------------.__sum_up_regions__----------------------------------- #

LPJmLDataCalc$set("private",
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
    aggr_data <- abind(list_of_aggr_bands, along = 3)

    # recover names of dimension vector
    region_names <- dimnames(aggr_data)[[1]]
    dim(aggr_data) <- c(region = dim(aggr_data)[1],
                        time = dim(aggr_data)[2],
                        band = dim(aggr_data)[3])

    # recover dimnames
    dimnames(aggr_data) <- list(
      region = region_names,
      time = dimnames(self$data)[["time"]],
      band = dimnames(self$data)[["band"]]
    )

    # set new attributes of the object
    private$.data <- aggr_data
    private$.grid <- lpjml_regions
    private$copy_unit_meta2array()

    # change respective meta entry
    private$.meta$.__set_as_aggregated__()
  }
)

# ------------------- conversion of area density to cell totals -------------- #
find_terr_area <- function(searchdir) {
  grid_files <- list.files(
    path = searchdir,
    pattern = "^terr_area",
    full.names = TRUE
  )
  if (length(grid_files) > 0) {
    grid_types <- sapply(grid_files, lpjmlkit::detect_io_type) # nolint
    # Prefer "meta" file_type if present
    if (length(which(grid_types == "meta")) == 1) {
      filename <- grid_files[match("meta", grid_types)]
    } else if (length(which(grid_types == "clm")) == 1) {
      # Second priority "clm" file_type
      filename <- grid_files[match("clm", grid_types)]
    } else {
      # Stop if either multiple files per file type or not the right type have
      # been detected
      stop(
        "Cannot detect terr_area file automatically",
      )
    }
  } else {
    # Stop if no file name matching pattern detected
    stop(
      "Cannot detect terr_area file automatically",
    )
  }

  filename

}



LPJmLDataCalc$set("private",
                  ".__load_terr_area__",
                  function() {
    terr_area_path <- find_terr_area(private$.meta$._data_dir_)
    terr_area <- read_io(terr_area_path)
    return(terr_area)
  }
)

LPJmLDataCalc$set("private",
                  ".__get_supporting_cell_areas__",
                  function(support_of_area_dens) {
    if (support_of_area_dens == "terr_area") {
      terr_area <- private$.__load_terr_area__()
      terr_area$add_grid()
      cell_areas <- terr_area
    } else if (support_of_area_dens == "full_cell_area") {
      if (!inherits(self$grid, "LPJmLGridData")) {
        stop("A grid is needed to convert area density to total per cell")
      }
      if (!grepl("m-2", units::deparse_unit(self$.data_with_unit))) {
        stop("Data must be given per square meter to convert to total per cell")
      }
      cell_areas <- calc_lpjml_calc_cell_area(private$.grid)
    } else {
      stop("The support_of_area_dens must be either 'terr_area' or
           'full_cell_area'")
    }
    return(cell_areas)
  }
)


LPJmLDataCalc$set("private",
                  ".__area_dens2cell_totals__",
                  function(support_of_area_dens) {
    if (is.character(support_of_area_dens)) {
      cell_area <- private$.__get_supporting_cell_areas__(support_of_area_dens)
    } else if (inherits(support_of_area_dens, "LPJmLDataCalc")) {
      cell_area <- support_of_area_dens
    } else {
      stop("The support_of_area_dens must be either a character string
           or an LPJmLDataCalc object")
    }
    self$.multiply(cell_area)
  }
)



# ----- utility functions -----
# Main utility function
# It calculates the cell areas in m^2 for a number of
# gridcells given in an LPJmLGridData object
# and returns them as an LPJmLDataCalc object.
calc_lpjml_calc_cell_area <- function(lpjml_grid) {

  cell_areas <- lpjmlkit::calc_cellarea(lpjml_grid, return_unit = "m2")

  ncell <- length(cell_areas)

  # set dims and dimnames
  dim(cell_areas) <- c(cell = ncell,
                       time = 1,
                       band = 1)
  dimnames(cell_areas) <- list(cell = 1:ncell,
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
  meta <- lpjmlkit:::LPJmLMetaData$new(header, list(unit = "m2",
                                                    variable = "cell area"))

  # combine array and metadata to LPJmLData object
  lpjml_calc <- lpjmlkit:::LPJmLData$new(cell_areas, meta)

  # add grid
  lpjml_calc$.__set_grid__(lpjml_grid)

  # coerce to LPJmLDataCalc
  lpjml_calc <- as_LPJmLDataCalc(lpjml_calc)

  return(lpjml_calc)
}
