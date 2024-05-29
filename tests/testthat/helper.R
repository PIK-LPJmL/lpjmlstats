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
    lpjml_meta <- lpjmlkit::LPJmLMetaData$new(header)
    dimnames(gridarray) <-
      list(cell = as.character(seq_len(dim(gridarray)[1])),
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

create_soiln_var_grp <- function() {
  soiln_dir <- testthat::test_path("../testdata/path1/soiln_layer.bin.json")
  soiln_baseline <- read_io(soiln_dir)
  soiln_baseline <- aggregate(soiln_baseline, time = "sim_period")
  soiln_baseline$set_sim_identifier("sim1")

  soiln_under_test_1 <- soiln_baseline$clone(deep=TRUE)
  soiln_baseline$set_sim_identifier("sim2")
  soiln_under_test_2 <- soiln_baseline$clone(deep=TRUE)
  soiln_baseline$set_sim_identifier("sim3")

  soiln_under_test_1$.__enclos_env__$private$.data[1,1,1] <- 10
  soiln_under_test_2$.__enclos_env__$private$.data[1,1,1] <- 20

  var_grp <- VarGrp$new()

  var_grp$baseline <- soiln_baseline
  var_grp$under_test <- list(sim1 = soiln_under_test_1,
                             sim2 = soiln_under_test_2)

  var_grp$compare <- list(diff = list(sim1 = soiln_under_test_1 - soiln_baseline,
                                      sim2 = soiln_under_test_2 - soiln_baseline))

  return(var_grp)
}

