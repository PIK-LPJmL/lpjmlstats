create_ilamb_report <- function(baseline_dir, 
                                under_test_dirs, 
                                output_file, 
                                sim_ident,
                                eval_vars = c("mgpp", "mevap", "mtransp"),
                                ilamb_run_script = file.path(ilamb_dir, "ilamb_run_cmd.sh")) {
  
  # 1. Create the iLAMB output directory
  ilamb_dir <- file.path(dirname(output_file), paste0(gsub(".pdf", "", basename(output_file)), "_ilamb"))
  dir.create(ilamb_dir, showWarnings = FALSE, recursive = TRUE)

  # 2. Symlink the data folder
  system(paste(
    "ln -s",
    file.path("/p/projects/lpjml/reference_data/iLAMB"),
    file.path(ilamb_dir, "DATA")
  ))
  
  # 3. Create MODELS folder structure
  get_subfolder_name <- function(dir) {
    x <- sim_ident[dir]
    gsub("/", "_", x)
  }
  
  base_subfolder <- get_subfolder_name(baseline_dir)
  dir.create(file.path(ilamb_dir, "MODELS", base_subfolder), recursive = TRUE)
  
  for (dir in under_test_dirs) {
    subfolder <- get_subfolder_name(dir)
    dir.create(file.path(ilamb_dir, "MODELS", subfolder), recursive = TRUE)
  }
  
  # 4. Copy and set up scripts/config files
  #    Shell scripts are made executable (".sh"), config files remain as-is.
  
  files_to_copy <- c("ilamb_run_cmd.sh", # script to run ILAMB
                     "sample.cfg",       # ILAMB config
                     "start.sh")         # script to start local webserver
  
  for (f in files_to_copy) {
    src <- system.file("ilamb", f, package = "lpjmlstats")
    dst <- file.path(ilamb_dir, f)
    file.copy(src, dst, overwrite = TRUE)
    # If it's a shell script, mark as executable
    if (grepl("\\.sh$", f)) {
      Sys.chmod(dst, mode = "0755")
    }
  }
  
  # 5. Convert vars to NetCDF using the bin2cdf_cmd.sh script
  bin2cdf_path <- system.file("bin2cdf", package = "lpjmlstats")
  process_var <- function(var) {
    for (dir in c(baseline_dir, under_test_dirs)) {
      meta <- read_meta(file.path(dir, paste0(var, ".bin.json")))
      name <- LPJmLMetaDataCalc$new(meta)$name
      ident <- get_subfolder_name(dir)
      system(
        paste(bin2cdf_path, "-days -metafile", 
              name, 
              file.path(dir, "grid.bin.json"), 
              file.path(dir, paste0(var, ".bin.json")), 
              file.path(ilamb_dir, "MODELS", ident, paste0(name, ".nc"))
        )
      )
    }
  }
  
  for (var in eval_vars) {
    process_var(var)
  }
  
  # 6. Run ILAMB by calling ilamb_run_cmd.sh (in ilamb_dir)
  system2("bash", c(ilamb_run_script, shQuote(ilamb_dir)))
  
  # 7. Inform the user how to start the local server
  message(sprintf(
    "To start a local web server on a random port, run:\n  cd %s\n  ./start.sh\n",
    ilamb_dir
  ))
}
