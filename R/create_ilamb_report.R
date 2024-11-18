create_ilamb_report <- function(baseline_dir, under_test_dirs, output_file, sim_table) {
  # create directory
  ilamb_dir <- file.path(dirname(output_file), paste0(gsub(".pdf", "", basename(output_file)), "_ilamb"))
  dir.create(ilamb_dir)

  # add symlink to data
  system(paste("ln -s", file.path("/p/projects/lpjml/reference_data/iLAMB"), file.path(ilamb_dir, "DATA")))

  # create MODELS folder structure
  get_subfolder_name <- function(dir) {
    x <- sim_table$sim_ident[sim_table$sim_paths == unname(unlist(dir))][1]
    gsub("/", "_", x)
  }

  base_subfolder <- get_subfolder_name(baseline_dir)
  dir.create(file.path(ilamb_dir, "MODELS", base_subfolder), recursive = TRUE)
  for (dir in under_test_dirs) {
    subfolder <- get_subfolder_name(dir)
    dir.create(file.path(ilamb_dir, "MODELS", subfolder), recursive = TRUE)
  }

  # convert vars to nc
  bin2cdf_path <- system.file("bin2cdf", package = "lpjmlstats")
  process_var <- function(var) {
    for (dir in c(baseline_dir, under_test_dirs)) {
      meta <- read_meta(file.path(dir, paste0(var, ".bin.json")))
      name <- LPJmLMetaDataCalc$new(meta)$name
      ident <- get_subfolder_name(dir)
      system(
        paste(bin2cdf_path, "-days -metafile", name, 
              file.path(dir, "grid.bin.json"), 
              file.path(dir, paste0(var, ".bin.json")), 
              file.path(ilamb_dir, "MODELS", ident, paste0(name, ".nc"))
        )
      )
    }
  }
  process_var("mgpp")
  process_var("mevap")
  process_var("mtransp")

  # create sample.cfg in ilamb_dir
  writeLines(
    c(
      "[h1: Vegetation]",
      "[h2: gpp]",
      "variable = \"gpp\"",
      "[WECANN]",
      "source  = \"DATA/gpp/WECANN/gpp.nc\"",
      "[h2: et]",
      "variable = \"et\"",
      "[MODIS]",
      "derived = \"transp + evap\"",
      "source = \"DATA/evspsbl/MODIS/et_0.5x0.5.nc\""
    ),
    file.path(ilamb_dir, "sample.cfg")
   )

  # run ilamb
  system(paste0(
  "module use --append /p/projects/poem/davidho/ilamb/modulefile; ",
  "module load ILAMB; ",
  "cd " , ilamb_dir,"; ", "export ILAMB_ROOT=$PWD","; ",
  "ilamb-run --config sample.cfg --model_root $ILAMB_ROOT/MODELS/ --regions global" ))

  # write a start script opening a webserver on a port with random number between 4000 and 5000
  port <- sample(4000:5000, 1)
  writeLines(
    c(
      "cd _build",
      "module load anaconda",
      paste0("python -m http.server ", port)
    ),
    file.path(ilamb_dir, "start.sh")
  )
  system(paste("chmod +x", file.path(ilamb_dir, "start.sh")))
}