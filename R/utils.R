# Collection of small utility function applied across the package

# Warn when a deprecated argument is used and fall back to it if new arg is unset
deprecate_arg <- function(new_arg,
                          deprec_arg,
                          version = "1.0.0",
                          ignore_new_arg = FALSE) {
  new_name <- deparse(substitute(new_arg))
  deprec_name <- deparse(substitute(deprec_arg))
  if (!is.null(deprec_arg)) {
    if (is.null(new_arg) || ignore_new_arg) {
      new_arg <- deprec_arg
    } else {
      warning(
        "Argument `", deprec_name, "` will be ignored in favour of argument `",
        new_name, "`."
      )
    }
    message(
      "Argument `", deprec_name, "` is deprecated as of version ", version, ";",
      " use `", new_name, "` instead."
    )
  }
  new_arg
}
