#' @importFrom R6 R6Class

#' function to create a sparse region matrix
#' columns = regions; rows = cells
#' values are the fraction of cell area that is contained in a region
#' random numbers between 0 and 1 are assigned for a given number of cells in
#' each region (cells_per_region)
#' only for testing purposes
generate_random_region_matrix <-
  function(cells_per_region, nregions, ncells) {
    # ChatGPT generated

    # Initialize vectors to store the row indices, column indices, and values of
    # the non-zero elements
    rows <- integer(nregions * cells_per_region)
    cols <- integer(nregions * cells_per_region)
    vals <- numeric(nregions * cells_per_region)

    # Fill the vectors with appropriate values
    for (col in 1:nregions) {
      # Randomly select row indices for the non-zero entries in this column
      selected_rows <-
        sample(1:ncells, cells_per_region, replace = FALSE)

      # Assign the selected row indices, column indices, and random values
      # to the vectors
      idx <-
        ((col - 1) * cells_per_region + 1):(col * cells_per_region)
      rows[idx] <- selected_rows
      cols[idx] <- col
      vals[idx] <-
        runif(cells_per_region) * 3 # Random numbers between 0 and 1
    }

    # Create the sparse matrix
    sparse_mat <- sparseMatrix(
      i = rows,
      j = cols,
      x = vals,
      dims = c(ncells, nregions),
      dimnames = list(NULL, paste("region", seq(1:nregions)))
    )

    # normalize rows to sum to 1
    row_sum <- Matrix::rowSums(sparse_mat)
    row_sum[row_sum == 0] <- 1
    sparse_mat <- as(sparse_mat / row_sum, "dgCMatrix")

    return(Matrix::t(sparse_mat))
  }

load_test_grid <- function() {
  soiln_path <- testthat::test_path("../testdata/path1", "soiln.rds")
  soiln <- readRDS(soiln_path)
  soiln$add_grid()
  grid <- soiln$grid
  return(grid)
}

load_soiln_david_local <- function() {
  path_to_soil_layer_n_json <-
    "C:\\Users\\davidho\\Desktop\\LPJmLG\\example_outputs\\soiln_layer.bin.json" # nolint: absolute_path_linter.
  soiln <- lpjmlkit::read_io(path_to_soil_layer_n_json)
  soiln$add_grid()
  return(soiln)
}

create_LPJmLDataCalc <- function(data,  # nolint: object_name_linter.
                                 my_unit,
                                 grid = NULL,
                                 ncell = NULL,
                                 nyear = NULL,
                                 nbands = NULL,
                                 ...) {
  if (!is.array(data)) {
    data <- array(data, dim = c(cell = length(data), time = 1, band = 1))
  }
  if (is.null(ncell)) {
    ncell <- dim(data)[1]
  }
  if (is.null(nyear)) {
    nyear <- dim(data)[2]
  }
  if (is.null(nbands)) {
    nbands <- dim(data)[3]
  }
  if (is.null(names(dim(data)))) {
    dimnames <- dimnames(data)
    dim(data) <- c(cell = dim(data)[1],
                   time = dim(data)[2],
                   band = dim(data)[3])
    dimnames(data) <- dimnames
  }
  header <-
    lpjmlkit::create_header(
      ncell = ncell,
      nbands = nbands,
      nyear = nyear,
      verbose = FALSE,
      ...
    )
  lpjml_meta <-
    lpjmlkit::LPJmLMetaData$new(header, list(unit = my_unit))
  lpjml_dat <- lpjmlkit::LPJmLData$new(data, lpjml_meta)
  if (!is.null(grid)) {
    lpjml_dat$.__set_grid__(grid)
  }
  lpjml_calc <- LPJmLDataCalc$new(lpjml_dat)
  lpjml_calc$.check_internal_integrity()
  return(lpjml_calc)
}

create_LPJmLGridData <- # nolint:object_name_linter
  function(gridarray) {
    header <-
      lpjmlkit::create_header(ncell = dim(gridarray)[1],
                              verbose = FALSE,
                              name = "GRID")
    lpjml_meta <- lpjmlkit::LPJmLMetaData$new(header, 
                                              additional_attributes = list(variable = "grid"))
    dim(gridarray) <- c(cell = dim(gridarray)[1], time = 1, band = 2)
    dimnames(gridarray) <-
      list(cell = as.character(seq_len(dim(gridarray)[1])),
           time = "1",
           band = c("lon", "lat"))
    grid <- lpjmlkit::LPJmLData$new(gridarray, lpjml_meta)
    grid <- lpjmlkit::LPJmLGridData$new(grid)
    return(grid)
  }

create_test_global_LPJmLDataCalc <- function(unit = "gN", value = NULL) { # nolint: object_name_linter.
  if (is.null(value)) {
    data <- array(rep(0, 67420), dim = c(67420, 1, 1))
    data[2011, 1, 1] <- 1 # Canada, completely land
    data[2012, 1, 1] <- 1 # Canada, completely land
    data[2899, 1, 1] <- 1 # USA, completely land
  } else {
    data <- array(rep(value, 67420), dim = c(67420, 1, 1))
  }
  dimnames(data) <- list(cell = as.character(0:67419), time = "1", band = "1")
  grid <- read_def_grid()

  lpjml_calc <- create_LPJmLDataCalc(data, unit, grid)

  return(lpjml_calc)
}

load_soiln_calc <- function() {
  path_to_soil_n_json <-
    testthat::test_path("../testdata/path1", "soiln.bin.json")
  soiln <- lpjmlstats::read_io(path_to_soil_n_json)
  soiln$add_grid()
  return(soiln)
}

load_soiln_layer_calc <- function() {
  path_to_soil_n_json <-
    testthat::test_path("../testdata/path1", "soiln_layer.bin.json")
  soiln <- lpjmlstats::read_io(path_to_soil_n_json)
  soiln$add_grid()
  return(soiln)
}

create_var_grp <-
  function(baseline) {
    baseline$.meta$.__set_sim_ident__("sim1")
    baseline$.meta$.__set_pos_in_var_grp__(list(type = "baseline"))

    under_test_1 <- baseline$clone(deep = TRUE)
    baseline$.meta$.__set_sim_ident__("sim2")
    under_test_1$.meta$.__set_pos_in_var_grp__(list(type = "under_test"))
    under_test_2 <- baseline$clone(deep = TRUE)
    baseline$.meta$.__set_sim_ident__("sim3")
    under_test_2$.meta$.__set_pos_in_var_grp__(list(type = "under_test"))


    under_test_1$.__enclos_env__$private$.data[1, 1, 1] <- 10
    under_test_2$.__enclos_env__$private$.data[1, 1, 1] <- 20

    var_grp <- VarGrp$new()

    var_grp$baseline <- baseline
    var_grp$under_test <- list(sim1 = under_test_1, sim2 = under_test_2)

    var_grp$compare <- list(diff = list(sim1 = under_test_1 - baseline,
                                        sim2 = under_test_2 - baseline))

    compare_pos <- list(type = "compare", compare_item = "diff")
    var_grp$compare$diff$sim1$.meta$.__set_pos_in_var_grp__(compare_pos)
    var_grp$compare$diff$sim2$.meta$.__set_pos_in_var_grp__(compare_pos)

    return(var_grp)
  }

get_test_m_options <- function() {
  m_options <- list(font_size = 8,
                    n_breaks = 3,
                    quantiles = c(0.05, 0.95),
                    var_seperator = NULL,
                    band_seperator = NULL,
                    num_cols = 2)
  return(m_options)
}

m_option <- list(year_subset = as.character(c(2009:2018)))

# metric for testing purposes
.DoNothing <- R6::R6Class( # nolint: object_name_linter.
  ".DoNothing", # nolint: object_name_linter.
  inherit = Metric,
  public = list(
    summarize = function(data) {
      data
    },
    compare = function(var_grp) {
      var_grp$compare <- list("nodiff" = list(sim1 = var_grp$baseline))
    },
    m_options = m_option
  )
)

# test metric options
test_m_options <- list(GlobSumTimeAvgTable = m_option,
                       GlobSumTimeseries = m_option,
                       TimeAvgMap = m_option,
                       .DoNothing = m_option)
