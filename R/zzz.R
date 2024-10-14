.onLoad <- function(libname, pkgname) {
  # NTODO: unit configuration into package installation?
  units::units_options(set_units_mode = "standard")
  unit_database_path <- system.file("lpjml_units",
                                    "udunits2.xml",
                                    package = "lpjmlstats")
  units::load_units_xml(unit_database_path)

  # configure default settings
  # the default needs to be manually set here if it is different
  # from NULL (in the option list)

  # the metrics_at_start default is "Table". NULL means that no reordering
  # of metrics is done.
  set_lpjmlstats_settings(metrics_at_start = c("Table", "Timeseries"))
}
