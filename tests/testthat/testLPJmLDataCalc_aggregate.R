test_that("aggregate regions via sum produces correct values and metadata", {
  # ------ create LPJmLRegionData object
  # grid
  gridarray <-
    array(c(9.75, 10.25, 10.75, 11.25, 11.75, 12.25), dim = c(3, 2))
  grid <- create_LPJmLGridData(gridarray)
  # region matrix
  region_matrix <-
    Matrix(
      c(0.5, 0, 0.5, 0.5, 0, 0),
      nrow = 2,
      ncol = 3,
      dimnames = list(c("R1", "R2"), NULL),
      sparse = TRUE
    )
  regions <- LPJmLRegionData$new(grid, region_matrix)

  # ------ create LPJmLDataCalc object
  data <- array(rep(c(1, 2, 3, 4, 5, 6), 2),
                dim = c(3, 2, 2),
                dimnames = list(
                  cell = c("1", "2", "3"),
                  time = c("time 1", "time 2"),
                  band = c("band 1", "band 2")
                ))
  lpjml_calc <- create_LPJmLDataCalc(data, "g", grid = grid)

  # ------ sum up regions
  lpjml_calc$aggregate(cell = list(to = regions, stat = "sum"))

  # ------ verify that output is as expected
  # correct values
  expect_equal(lpjml_calc$data[2, 1, 1], 1, ignore_attr = TRUE)
  expect_equal(lpjml_calc$data[1, 2, 1], 4.5, ignore_attr = TRUE)
  expect_equal(names(dim(lpjml_calc$data)), c("region", "time", "band"))
  # correct dimnames
  expect_equal(dimnames(lpjml_calc$data)[["region"]],  c("R1", "R2"))
  expect_equal(dimnames(lpjml_calc$data)[["time"]],  c("time 1", "time 2"))
  expect_equal(dimnames(lpjml_calc$data)[["band"]],  c("band 1", "band 2"))
  # correct unit
  expect_equal(attr(lpjml_calc$.data_with_unit, "units")$numerator,
               "g")
  # correct grid attribute
  expect_equal(lpjml_calc$grid, regions)
})


test_that("aggregation speed is fast enough", {
  path_to_soil_n_json <- test_path("../testdata/path1", "soiln.bin.json")
  soiln <- lpjmlkit::read_io(path_to_soil_n_json)
  soiln$add_grid()

  # generate random LPJmLRegionData
  regions <- generate_random_region_matrix(cells_per_region = 560,
                                           nregions = 237,
                                           ncells = 67420)
  reg_dat <- LPJmLRegionData$new(soiln$grid, regions)
  soiln_calc <- .as_LPJmLDataCalc(soiln)

  # do aggregation
  start_time <- Sys.time()
  soiln_calc$aggregate(cell = list(to = reg_dat, stat = "sum"))
  end_time <- Sys.time()
  time_diff <- end_time - start_time

  # verify that it was fast enough
  # should normally be less than 0.1 sec on a modern pc
  expect_lt(time_diff, 0.5)

})

test_that("if grids do not match an error is thrown", {
  # ------ create LPJmLRegionData object
  # create grid
  gridarray <-
    array(c(9.75, 10.25, 10.75, 11.25, 11.75, 12.25), dim = c(3, 2))
  grid <- create_LPJmLGridData(gridarray)

  # create region matrix
  region_matrix <-
    Matrix(
      c(0.5, 0, 0.5, 0.5, 0, 0),
      nrow = 2,
      ncol = 3,
      dimnames = list(c("R1", "R2"), NULL),
      sparse = TRUE
    )
  regions <- LPJmLRegionData$new(grid, region_matrix)

  # ------ modify grid
  gridarray[1, 1] <- 20.25
  grid_modified <- create_LPJmLGridData(gridarray)

  # ------ create LPJmLDataCalc with modified grid
  data <- array(rep(c(1, 2, 3, 4, 5, 6), 2),
                dim = c(3, 2, 2),
                dimnames = list(
                  cell = c("1", "2", "3"),
                  time = c("time 1", "time 2"),
                  band = c("band 1", "band 2")
                ))
  lpjml_calc <-
    create_LPJmLDataCalc(data, "g", grid = grid_modified)

  # ------ grids are not matching so error should be thrown
  expect_error(lpjml_calc$aggregate(ref_area = "cell_area", cell = regions),
               "grid")
})

test_that("aggregation to cowregions produces correct values in Canada and USA",
          {
            lpjml_calc <- create_test_global_LPJmLDataCalc()

            lpjml_calc <-
              aggregate(lpjml_calc, cell = list(to = "countries", stat = "sum"))

            # verify that values are correct
            expect_equal(unname(lpjml_calc$data["USA", 1, 1]), 1)
            expect_equal(unname(lpjml_calc$data["CAN", 1, 1]), 2)
          })

test_that("aggregation is recorded in meta data", {
  lpjml_calc <- create_test_global_LPJmLDataCalc()

  lpjml_calc <-
    aggregate(lpjml_calc, cell = list(to = "countries", stat = "sum"))

  # correct aggregated attribute of meta data
  expect_equal(lpjml_calc$meta$space_aggregation, "sum")
})


test_that("usecases of aggregation with cow and plotting runs error free", {
  # load test data
  pdf(NULL)
  path_to_soil_n_json <- test_path("../testdata/path1", "soiln.bin.json")
  soiln <- read_io(path_to_soil_n_json)
  soiln$add_grid()

  # usecase 1: subset and plot
  expect_no_error(soiln %>% subset(time = 1) %>% plot())

  # usecase 2: aggregate, subset and plot
  expect_no_error(
    soiln %>%
      aggregate(cell = list(to = "countries", stat = "sum")) %>%
      subset(time = 1) %>%
      plot()
  )
})

test_that("usecases of aggregation with cow integral on multiple timesteps ",
          {
            # load test data
            path_to_soil_n_json <-
              test_path("../testdata/path1", "soiln.bin.json")
            soiln <- read_io(path_to_soil_n_json)
            soiln$add_grid()

            # aggregate multiple time steps #nolint
            expect_no_error(soiln %>%
                              aggregate(cell =
                                          list(to = "countries", stat = "sum")))

          })

test_that("aggregation to global land produces correct values", {
  lpjml_calc <- create_test_global_LPJmLDataCalc()

  lpjml_calc <-
    aggregate(
      lpjml_calc,
      cell = list(to = "global", stat = "sum")
    )

  expect_equal(unname(lpjml_calc$data[1, 1, 1]), 3)
})

test_that("integration over global land produces correct values", {
  lpjml_calc <- create_test_global_LPJmLDataCalc(unit = "gN/m2")

  lpjml_calc <- aggregate(
    lpjml_calc,
    ref_area = "cell_area",
    cell = list(to = "global", stat = "weighted_sum")
  )

  # verify that values equals cell area times values summed up
  expect_equal(unname(lpjml_calc$data[1, 1, 1]), 5286288145)
})

test_that("global integration produces roughly correct global land area",
          {
            # assign value 1 to global land surface
            lpjml_calc <-
              create_test_global_LPJmLDataCalc(unit = "1/m^2", value = 1)

            # integrate over global land
            lpjml_calc <- aggregate(
              lpjml_calc,
              ref_area = "cell_area",
              cell = list(to = "global", stat = "weighted_sum")
            )

            # extract area
            area <- lpjml_calc$data

            # convert unit
            area <- set_units(area, "m^2")
            area <- set_units(area, "km^2")
            area <- drop_units(area)

            # source: FAO global land area,
            # not a precise value since FAO includes inland water bodies
            expect_lt(abs(area - 130000000), 1e8)
          })


test_that("double aggregation throws error", {
  lpjml_calc <- create_test_global_LPJmLDataCalc()

  lpjml_calc <-
    aggregate(lpjml_calc, cell = list(to = "countries", stat = "sum"))

  expect_error(aggregate(lpjml_calc, cell = list(to = "global", stat = "sum")),
               "already aggregated")
})

test_that("convert to absolute cell values produces correct result", {
  # create test data
  data <- array(1, dim = c(1, 1, 1))
  dimnames(data) <- list(cell = "1", time = "1", band = "1")
  lpjml_calc <- create_LPJmLDataCalc(data, "kg/m2")

  # create test grid
  gridarray <- array(c(5, 6), dim = c(1, 2))
  grid <- create_LPJmLGridData(gridarray)

  # add grid to lpjml_calc
  lpjml_calc$.__set_grid__(grid)

  correct_arrea <-
    lpjmlkit::calc_cellarea(c(6))

  # convert to absolute values
  cell_area <- lpjml_calc$get_ref_area("cell_area")
  lpjml_calc$.multiply(cell_area)

  # verify that values and unit are correct
  expect_equal(lpjml_calc$data[[1]], correct_arrea)
  expect_equal(lpjml_calc$meta$unit, "kg")
})

test_that("construction of global region works", {
  # create test grid
  gridarray <- array(c(5, 6, 5.5, 6.5), dim = c(2, 2))
  grid <- create_LPJmLGridData(gridarray)

  # create global region
  global_region <- build_global_region(grid)

  expect_equal(as.array(global_region$region_matrix),
               array(1, dim = c(1, 2)),
               ignore_attr = TRUE)

})

test_that("conversion to cell totals works", {
  soiln <- load_soiln_calc()

  cell_area <- soiln$get_ref_area("terr_area")
  soiln$.multiply(cell_area)

  expect_equal(soiln$meta$unit, "gN")
})

test_that("area mean aggregation produces correct value", {
  test_calc <-
    create_test_global_LPJmLDataCalc(value = 11.11, unit = "gN m-2")
  test_calc_agg <-
    aggregate(
      test_calc,
      ref_area = "cell_area",
      cell = list(to = "global", stat = "weighted_mean")
    )

  expect_equal(test_calc_agg$data[1, 1, 1], 11.11, ignore_attr = TRUE)
  expect_equal(test_calc_agg$meta$unit, "gN m-2")
})

test_that("mean aggregation produces correct value", {
  test_calc <-
    create_test_global_LPJmLDataCalc(value = 11.11, unit = "gN m-2")
  test_calc_agg <-
    aggregate(
      test_calc,
      ref_area = "cell_area",
      cell = list(to = "global", stat = "mean")
    )

  expect_equal(test_calc_agg$data[1, 1, 1], 11.11, ignore_attr = TRUE)
  expect_equal(test_calc_agg$meta$unit, "gN m-2")
})

test_that("area mean aggregation works for edge cases", {
  # Case 1: 0
  test_calc <-
    create_test_global_LPJmLDataCalc(value = 0, unit = "gN m-2")

  test_calc_agg <-
    aggregate(
      test_calc,
      ref_area = "cell_area",
      cell = list(to = "global", stat = "weighted_mean")
    )

  expect_equal(test_calc_agg$data[1, 1, 1], 0, ignore_attr = TRUE)

  # Case 2: NaN
  test_calc <-
    create_test_global_LPJmLDataCalc(value = NaN, unit = "gN m-2")

  test_calc_agg <-
    aggregate(
      test_calc,
      ref_area = "cell_area",
      cell = list(to = "global", stat = "weighted_mean")
    )

  expect_equal(test_calc_agg$data[1, 1, 1], NaN, ignore_attr = TRUE)
})

test_that("grid for aggregation is loaded automatically", {
  path_to_soil_n_json <-
    test_path("../testdata/path1", "soiln.bin.json")
  soiln <- read_io(path_to_soil_n_json)
  expect_no_error(soiln %>% aggregate(cell = list(to = "global", stat = "sum")))
})


# ----- temporal aggregation tests -----

test_that("temporal aggregation over whole simulation period correct result", {
  # create test data
  data <- array(rep(c(2, 4), 2, each = 2) * rep(c(1, 2), each = 4) +
                  rep(c(0, 1), 4), dim = c(2, 2, 2))
  dimnames(data) <- list(
    cell = c("cell1", "cell2"),
    time = c("year1", "year2"),
    band = c("band1", "band2")
  )

  lpjml_calc <- create_LPJmLDataCalc(data, "kg/m2", ncell = 2)

  # create test grid
  gridarray <- array(c(5, 6, 6, 7), dim = c(2, 2))
  grid <- create_LPJmLGridData(gridarray)

  # add grid to lpjml_calc
  lpjml_calc$.__set_grid__(grid)

  # aggregate
  lpjml_calc_agg <-
    aggregate(
      lpjml_calc,
      time = list(to = "sim_period", stat = "mean")
    )

  # check correct values
  expect_equal(lpjml_calc_agg$data, array(c(3, 4, 6, 7), dim = c(2, 1, 2)),
               ignore_attr = TRUE)
  # check correct names of dim vector
  expect_equal(names(dim(lpjml_calc_agg$data)), c("cell", "time", "band"))
  # check correct dimnames
  expect_equal(dimnames(lpjml_calc_agg$data)[["cell"]],  c("cell1", "cell2"))
  expect_equal(dimnames(lpjml_calc_agg$data)[["time"]],  c("sim_period"))
  expect_equal(dimnames(lpjml_calc_agg$data)[["band"]],  c("band1", "band2"))
  #check aggregate_time meta data
  expect_equal(lpjml_calc_agg$meta$time_aggregation, "mean")
})

test_that("weighted sum prouced correct result fo single cell output", {
  soiln_layer <- read_io(test_path("../testdata/path1", "soiln_layer.bin.json"))
  soiln_agg <- aggregate(soiln_layer, cell = "global")

  terr_area <- read_io(test_path("../testdata/path1", "terr_area.bin.json"))
  terr_area <- subset(terr_area, cell = dimnames(soiln_layer$data)[["cell"]])

  expect_equal(soiln_agg$.data_with_unit, (soiln_layer * terr_area)$.data_with_unit, ignore_attr = TRUE)
})
