# ----- modifiable package settings of lpjmlstats

#' Set the package settings for lpjmlstats
#'
#' @param year_subset vector of years passed to the subset argument of the
#' \link{read_io} function to read in the output files for the
#' benchmarking. The default is to read in all years.
#' Note that the benchmarking will run much faster if
#' only a subset of years needs to be read in and processed.
#'
#' @param graphics_device character string, the graphics device to be used for
#' plotting the benchmarking results. The default is "png".
#' Using "pdf" will result in plots using vector graphics, which
#' allow infinite zooming without loss of quality.
#'
#' @param pdf_plot_dpi numeric, the dpi that is used for the graphics device.
#'
#' @param unit_table_path character string, the path to the unit
#' conversion table. The table must be a .csv file. The default is to use
#' the conversion table in the inst folder of the package.
#'
#' @export


set_lpjmlstats_settings <-

  function(year_subset = NULL,
           graphics_device = NULL,
           pdf_plot_dpi = NULL,
           unit_table_path = NULL) {

    # year_subset
    if (!is.null(year_subset)) {
      options(lpjmlstats.year_subset = year_subset)
    }

    # graphics_device
    if (!is.null(graphics_device)) {
      if (!is.character(graphics_device)) {
        stop("dev must be a character string")
      }
      options(lpjmlstats.graphics_device = graphics_device)
    }

    # pdf_plot_dpi
    if (!is.null(pdf_plot_dpi)) {
      if (!is.numeric(pdf_plot_dpi)) {
        stop("dpi must be a numeric value")
      }
      options(lpjmlstats.pdf_plot_dpi = pdf_plot_dpi)
    }

    # unit_table_path
    if (!is.null(unit_table_path)) {
      if (!is.character(unit_table_path)) {
        stop("unit_table must be a string")
      }
      if (!file.exists(unit_table_path)) {
        stop("unit_table_path does not exist")
      }
      # check that format of table is .csv
      if (tools::file_ext(unit_table_path) != "csv") {
        stop("unit_table_path must be a .csv file")
      }
      options(lpjmlstats.unit_conversion_table = unit_table_path)
    }
  }
