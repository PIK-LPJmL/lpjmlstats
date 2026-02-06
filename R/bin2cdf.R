#' Convert LPJmL binary output to NetCDF format
#'
#' Converts LPJmL binary output files (.bin with .bin.json metadata) to
#' NetCDF format. This is an R implementation replacing the C bin2cdf utility.
#'
#' @param input_file Path to the input binary file or its JSON metadata file
#'   (.bin.json)
#' @param output_file Path for the output NetCDF file
#' @param varname Optional variable name for the NetCDF file. If NULL, uses
#'   the name from metadata.
#' @param use_days If TRUE, use days since base year for time axis instead of
#'   months/years. Default is FALSE.
#' @param compress Compression level for NetCDF4 (0-9). Default is 0 (no
#'   compression).
#'
#' @return Invisibly returns the path to the created NetCDF file.
#'
#' @details
#' The function reads LPJmL binary output using lpjmlkit and writes it to
#' NetCDF format using the ncdf4 package. It preserves metadata including
#' variable names, units, and band names.
#'
#' Time axis is created based on the temporal resolution of the data:
#' - Annual data: years or days since base year
#' - Monthly data: months or days since base year
#' - Daily data: days since base year
#'
#' @examples
#' \dontrun{
#' # Convert a single file
#' bin2cdf("output/vegc.bin.json", "output/vegc.nc")
#'
#' # With days time axis
#' bin2cdf("output/mgpp.bin.json", "output/mgpp.nc", use_days = TRUE)
#' }
#'
#' @importFrom lpjmlkit read_io read_meta
#' @export
bin2cdf <- function(input_file,
                    output_file,
                    varname = NULL,
                    use_days = FALSE,
                    compress = 0) {

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package 'ncdf4' is required for bin2cdf. ",
         "Please install it with: install.packages('ncdf4')")
  }

  # Validate compress parameter (0 = no compression, 1-9 = compression levels)
  if (compress < 0 || compress > 9) {
    stop("compress must be between 0 and 9.")
  }
  # ncdf4 only accepts 1-9; 0 means no compression (NA)
  compress <- if (compress == 0) NA else compress

  # Validate output directory exists
  out_dir <- dirname(output_file)
  if (out_dir != "" && !dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir)
  }

  # Read the LPJmL data
  lpj_data <- lpjmlkit::read_io(input_file)
  lpj_data$add_grid()
  meta <- lpj_data$meta

  # Validate grid exists
  if (is.null(lpj_data$grid) || is.null(lpj_data$grid$data)) {
    stop("Grid data is missing. Ensure the grid file exists.")
  }

  # Resolve variable name
  if (is.null(varname)) {
    varname <- meta$variable
    if (is.null(varname) || varname == "") {
      varname <- gsub("\\.bin(\\.json)?$", "", basename(input_file))
    }
  }

  # Extract and validate dimensions
  data_array <- lpj_data$data
  dims <- dim(data_array)

  if (dims[1] == 0) stop("No cells in input data.")
  if (dims[2] == 0) stop("No timesteps in input data.")

  # Build grid mapping
  grid_info <- .build_grid_mapping(lpj_data$grid, meta)

  # Get time parameters with defaults
  nstep <- if (is.null(meta$nstep)) 1 else meta$nstep
  firstyear <- if (is.null(meta$firstyear)) 1901 else meta$firstyear

  if (!nstep %in% c(1, 12, 365)) {
    warning("Unusual nstep value: ", nstep,
            ". Expected 1 (annual), 12 (monthly), or 365 (daily).")
  }

  # Build time axis and write file
  time_info <- .build_time_axis(nstep, dims[2], firstyear, use_days)
  .write_netcdf(output_file, grid_info, time_info, dims[3],
                dimnames(data_array), meta, varname, compress, data_array)

  invisible(output_file)
}

# Build regular grid mapping from cell coordinates
.build_grid_mapping <- function(grid, meta) {
  lons <- grid$data[, 1]
  lats <- grid$data[, 2]
  cs_lon <- if (is.null(meta$cellsize_lon)) 0.5 else meta$cellsize_lon
  cs_lat <- if (is.null(meta$cellsize_lat)) 0.5 else meta$cellsize_lat

  lon_rounded <- round(lons / cs_lon) * cs_lon
  lat_rounded <- round(lats / cs_lat) * cs_lat
  lon_unique <- sort(unique(lon_rounded))
  lat_unique <- sort(unique(lat_rounded))

  list(lon = lon_unique, lat = lat_unique,
       lon_idx = match(lon_rounded, lon_unique),
       lat_idx = match(lat_rounded, lat_unique))
}

# Build time axis values and units string
.build_time_axis <- function(nstep, ntimesteps, firstyear, use_days) {
  if (use_days) {
    vals <- if (nstep == 1) {
      (seq_len(ntimesteps) - 1) * 365 + 183  # Annual: mid-year
    } else if (nstep == 12) {
      days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      mid_month <- cumsum(days_in_month) - days_in_month / 2
      idx <- seq_len(ntimesteps) - 1
      (idx %/% 12) * 365 + mid_month[(idx %% 12) + 1]
    } else {
      seq_len(ntimesteps) - 1  # Daily
    }
    list(vals = vals, units = paste0("days since ", firstyear, "-1-1 0:0:0"))
  } else {
    if (nstep == 1) {
      list(vals = seq(firstyear, length.out = ntimesteps), units = "years")
    } else {
      prefix <- if (nstep == 12) "months" else "days"
      list(vals = seq_len(ntimesteps) - 1,
           units = paste0(prefix, " since ", firstyear, "-1-1 0:0:0"))
    }
  }
}

# Write the NetCDF file
.write_netcdf <- function(output_file, grid_info, time_info, nbands,
                          dim_names, meta, varname, compress, data_array) {
  missing_val <- -9999.0
  nlon <- length(grid_info$lon)
  nlat <- length(grid_info$lat)
  ntimesteps <- length(time_info$vals)

  # Define dimensions
  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", grid_info$lon)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", grid_info$lat)
  time_dim <- ncdf4::ncdim_def("time", time_info$units, time_info$vals)

  if (nbands > 1) {
    band_names <- dim_names[["band"]]
    if (is.null(band_names)) band_names <- as.character(seq_len(nbands))
    band_dim <- ncdf4::ncdim_def("band", "", seq_len(nbands),
                                 create_dimvar = FALSE)
    dims <- list(lon_dim, lat_dim, band_dim, time_dim)
  } else {
    band_names <- NULL
    dims <- list(lon_dim, lat_dim, time_dim)
  }

  # Define variable
  var_unit <- if (is.null(meta$unit) || meta$unit == "") "1" else meta$unit
  long_name <- if (is.null(meta$descr)) varname else meta$descr

  var_def <- ncdf4::ncvar_def(varname, var_unit, dims, missing_val,
                              longname = long_name, prec = "float",
                              compression = compress)

  # Create file and ensure cleanup
  nc <- ncdf4::nc_create(output_file, var_def)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  # Rearrange and write data
  out_array <- .rearrange_to_grid(data_array, grid_info$lon_idx,
                                  grid_info$lat_idx, nlon, nlat,
                                  ntimesteps, nbands, missing_val)
  ncdf4::ncvar_put(nc, var_def, out_array)

  # Add attributes
  ncdf4::ncatt_put(nc, 0, "source", paste("lpjmlstats bin2cdf, R version",
                                          utils::packageVersion("lpjmlstats")))
  ncdf4::ncatt_put(nc, 0, "history",
                   paste(Sys.time(), "- converted from LPJmL binary format"))
  if (!is.null(meta$source)) {
    ncdf4::ncatt_put(nc, 0, "LPJmL_version", meta$source)
  }

  # Coordinate attributes (standard_name, long_name, axis)
  .add_coord_attrs(nc, "lon", "longitude", "Longitude", "X")
  .add_coord_attrs(nc, "lat", "latitude", "Latitude", "Y")
  .add_coord_attrs(nc, "time", "time", "Time", "T")
  ncdf4::ncatt_put(nc, "time", "calendar", "standard")

  if (!is.null(band_names)) {
    ncdf4::ncatt_put(nc, varname, "band_names",
                     paste(band_names, collapse = ", "))
  }
}

# Add coordinate attributes helper
.add_coord_attrs <- function(nc, var, std_name, long_name, axis) {
  ncdf4::ncatt_put(nc, var, "standard_name", std_name)
  ncdf4::ncatt_put(nc, var, "long_name", long_name)
  ncdf4::ncatt_put(nc, var, "axis", axis)
}

# Rearrange cell-indexed data to lon/lat grid
.rearrange_to_grid <- function(data_array, lon_idx, lat_idx,
                               nlon, nlat, ntimesteps, nbands, missing_val) {
  if (nbands > 1) {
    out <- array(missing_val, dim = c(nlon, nlat, nbands, ntimesteps))
    for (t in seq_len(ntimesteps)) {
      for (b in seq_len(nbands)) {
        out[cbind(lon_idx, lat_idx, b, t)] <- data_array[, t, b]
      }
    }
  } else {
    out <- array(missing_val, dim = c(nlon, nlat, ntimesteps))
    for (t in seq_len(ntimesteps)) {
      out[cbind(lon_idx, lat_idx, t)] <- data_array[, t, 1]
    }
  }
  out
}
