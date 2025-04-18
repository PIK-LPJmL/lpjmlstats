# ----- modifiable package settings of lpjmlstats

#' Set the package settings for lpjmlstats
#'
#' This function configures various settings for the lpjmlstats package.
#'
#' @param ... Variable arguments to specify settings. The function accepts
#' the following options:
#' \itemize{
#'   \item \code{graphics_device}:  A character string specifying
#'   the graphics device to be used for plotting the benchmarking results.
#'   Defaults to "png". Use "pdf" for vector graphics.
#'
#'   \item \code{pdf_plot_dpi}:  A numeric value specifying the
#'   DPI for the PDF document.
#'
#'   \item \code{unit_table_path}: A character string specifying
#'   the path to the unit conversion table (a .csv file). Defaults to the
#'   conversion table in the package's \code{inst} folder. The specified
#'   file must exist and be a .csv file.
#'
#'   \item \code{metrics_at_start}: A vector of strings to be
#'   matched against the
#'   names of the metrics. The matched metrics will be prioritized,
#'   that is run first and displayed at the report beginning. The prioritization
#'   will be in the same order as the vector.
#'
#'   \item \code{file_extension}: A string indicating the file extension
#'   to be used by read_io_calc.
#' }
#'
#'
#' @examples
#' \dontrun{
#' set_lpjmlstats_settings(unit_table_path = "path/to/my_table.csv",
#' pdf_plot_dpi = 300)
#' }
#'
#' @export


set_lpjmlstats_settings <- function(...) {
  args_list <- list(...)

  for (option_name in names(args_list)) {
    option_value <- args_list[[option_name]]
    switch(option_name,
      "graphics_device" = {
        if (!(is.character(option_value) | is.null(option_value)))
          stop("graphics_device must be a character string")
        options(lpjmlstats.graphics_device = option_value) # nolint
      },
      "pdf_plot_dpi" = {
        if (!(is.numeric(option_value) | is.null(option_value)))
          stop("pdf_plot_dpi must be a numeric value")
        options(lpjmlstats.pdf_plot_dpi = option_value) # nolint
      },
      "unit_table_path" = {
        if (is.null(option_value)) {
          options(lpjmlstats.unit_conversion_table = NULL) # nolint
        } else {
          if (!is.character(option_value))
            stop("unit_table_path must be a string")
          if (!file.exists(option_value))
            stop("unit_table_path does not exist")
          if (tools::file_ext(option_value) != "csv")
            stop("unit_table_path must be a .csv file")
          options(lpjmlstats.unit_conversion_table = option_value) # nolint
        }
      },
      "metrics_at_start" = {
        if (!(is.character(option_value) | is.null(option_value)))
          stop("metrics_at_start must be a character string")
        options(lpjmlstats.metrics_at_start = option_value) # nolint
      },
      "file_extension" = {
        if (!(is.character(option_value) | is.null(option_value)) | length(option_value) != 1)
          stop("file_extension must be a character string")
        options(lpjmlstats.file_extension = option_value) # nolint
      },
      stop(paste("Invalid option:", option_name))
    )
  }
}
