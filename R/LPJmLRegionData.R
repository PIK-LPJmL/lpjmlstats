#' @title LPJmLRegionData
#'
#' @import lpjmlkit
#' @importFrom Matrix sparseMatrix Matrix colSums
#' @importFrom utils read.csv
#' @importFrom methods as
#' @importFrom jsonlite fromJSON
#' @description A class that represents one or several regions in LPJmL.
#' Based on an LPJmL grid, a region is defined as set of grid cells together
#' with fractions. The fractions indicate the share of each grid cell that
#' is part of the region. (e.g. 1 = the cell belongs completely to the region,
#' 0 = the cell does not belong to the region at all).\
#' The underlying data structure is a sparse matrix, where the rows represent
#' the regions, the columns represent the grid cells and the values represent
#' the fractions (cells not belonging to a region do not take memory as only
#' nonzero entries are stored in a sparse matrix).
#'

LPJmLRegionData <- R6::R6Class( # nolint:object_linter_name

  classname = "LPJmLRegionData",
  public = list(
    #' Create a new `LPJmLRegionData` object; only used internally.
    #' @description
    #' !Internal method only to be used by the package itself!
    #'
    #' @param grid `LPJmLGridData` object containing the underlying grid.
    #'
    #' @param region_matrix object stores the region data as a sparse matrix.
    initialize = function(grid, region_matrix) {
      if (!inherits(grid, "LPJmLGridData")) {
        stop("grid must be of class LPJmLGridData")
      }
      if (!is(region_matrix, "dgCMatrix")) {
        stop("region_matrix must be of class dgCMatrix")
      }
      private$.grid <- grid
      private$.region_matrix <- region_matrix
      private$check_consistency()
    },

    #' @description
    #' Get number of cells per region.
    #' @return A vector of length nrow(region_matrix) containing the number of
    #' cells per region.
    #' @details
    #' For partially belonging cells the fraction
    #' of the cell that belongs to the region is counted.
    get_ncells_per_region = function() {
      return(Matrix::rowSums(private$.region_matrix))
    }
  ),

  active = list(
    #' @field region_matrix object stores the region data as a sparse matrix.
    region_matrix = function() {
      return(private$.region_matrix)
    },

    #' @field grid `LPJmLGridData` object containing the underlying grid.
    grid = function() {
      return(private$.grid$clone())
    }
  ),

  private = list(
    .region_matrix = NULL,
    .grid = NULL,

    check_consistency = function() {
      if (ncol(private$.region_matrix) != private$.grid$meta$ncell) {
        stop("inconsistent number of cells in grid and region matrix")
      }
      if (any(colSums(private$.region_matrix) > 1 + 1e-6)) {
        warning("sum of cell fractions above 1")
      }
    }
  )

)

#' Read or create the cow regions as an LPJmLRegionData object
#'
#' @description
#'
#' The COW = countries of the world data contains global country borders.
#'
#' @return An LPJmLRegionData object containing the cow regions.
#' @seealso \code{\link{LPJmLRegionData}}
#' @export
#'

read_cow_regions <- function() {
  path_to_lpjml_cow_regions <- system.file("lpjml_cow_regions.rds",
                                           package = "lpjmlstats")

  # if the required LPJmLRegionData object already exists, read it
  # else, create it
  if (!identical(path_to_lpjml_cow_regions, "")) {
    return(readRDS(path_to_lpjml_cow_regions))
  } else {
    # ------- create cow region matrix
    # read cow region file
    path_to_cow <-
      system.file("cow_full_2018.bin.json", package = "lpjmlstats")
    cow <- read_io_calc(path_to_cow)

    # extract first item of third dimension which contains countries
    cow_mat <- cow$data[, , 1]

    # create one hot encoding for each unique value
    unique_values <- unique(unname(cow_mat))
    unique_values <- unique_values # add 1 to avoid 0 as index
    region_matrix <-
      array(0, c(length(unique_values), length(cow_mat)))
    for (val in unique_values) {
      region_matrix[val + 1, ] <- unname(cow_mat == val)
    }
    region_matrix <- Matrix::Matrix(region_matrix, sparse = TRUE)

    # ------- add country codes as dimnames

    countries <- read_countrymap(path_to_cow) # read data as dataframe
    dimnames(region_matrix) <- list(countries$`alpha-3`, NULL)

    # ------- read grid
    grid <- read_def_grid()

    # ------- combine grid and region matrix to LPJmLRegionData object
    lpjml_cow_regions <- LPJmLRegionData$new(grid, region_matrix)

    # ------- save LPJmLRegionData object
    saveRDS(lpjml_cow_regions, "./inst/lpjml_cow_regions.rds")

    return(lpjml_cow_regions)
  }
}

# helper to extract the countrymap from json meta file
read_countrymap <- function(file_path) {

  # Read the JSON file
  json_data <- jsonlite::fromJSON(file_path)

  # Extract 'countrymap' as a vector
  countrymap_vector <- json_data$countrymap

  return(countrymap_vector)
}



#' Construct global region object that fully contains all cells given in a
#' grid.
#' @param grid An LPJmLGridData object containing the grid.
#' @return An LPJmLRegionData object containing the global region.
#' @seealso \code{\link{LPJmLRegionData}}

build_global_region <- function(grid) {
  region_matrix <- Matrix::Matrix(1, nrow = 1, ncol = grid$meta$ncell,
                                  sparse = TRUE)
  region_matrix <- methods::as(methods::as(region_matrix, "generalMatrix"), "CsparseMatrix")
  dimnames(region_matrix) <- list(c("global"), NULL)
  return(LPJmLRegionData$new(grid, region_matrix))
}


#' Read default grid
#'
#' @description
#' The default grid is the standard global grid used in LPJmL.
#'
#' @return An LPJmLGridData object containing the default grid.
#' @seealso \code{\link{LPJmLGridData}}
#'
#' @export

read_def_grid <- function() {
  path_to_grid <- system.file("grid.bin.json", package = "lpjmlstats")

  if (identical(path_to_grid, "")) {
    stop("grid does not exist as package data")
  }

  return(lpjmlkit::LPJmLGridData$new(lpjmlkit::read_io(path_to_grid)))
}
