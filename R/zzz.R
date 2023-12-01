.onLoad <- function(libname, pkgname) {
  # TODO: unit configuration into package installation?
  units::units_options(set_units_mode = "standard")
  unit_database_path <- system.file("lpjml_units",
                                    "udunits2.xml",
                                    package = "lpjmlstats")
  # TODO: install more LPJmL units next to gC gN?
  units::load_units_xml(unit_database_path)
}
