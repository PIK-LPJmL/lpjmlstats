.onLoad <- function(libname, pkgname) {
  # NTODO: unit configuration into package installation?
  units::units_options(set_units_mode = "standard")
  unit_database_path <- system.file("lpjml_units",
                                    "udunits2.xml",
                                    package = "lpjmlstats")
  # NTODO: install more LPJmL units next to gC gN?
  units::load_units_xml(unit_database_path)

  # configure the default settings
  options(lpjmlstats.file_extension = ".bin.json") # nolint
}
