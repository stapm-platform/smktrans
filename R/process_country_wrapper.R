#' Master Process Wrapper
#' @param config A named list containing all country-specific parameters
#' @export
process_country <- function(config) {
  
  message(paste0("\n", paste(rep("=", 60), collapse = "")))
  message(sprintf(" PROCESSING: %s", config$country))
  message(paste0(paste(rep("=", 60), collapse = ""), "\n"))
  
  # 1. Load Population Data
  # -----------------------
  # Assuming pop_file path is relative to root_dir or absolute. 
  # You might need to adjust 'file.path' depending on your folder structure consistency.
  if (file.exists(config$pop_file)) {
    pop_path <- config$pop_file
  } else {
    # Try prepending path if file not found (handling relative paths in config)
    pop_path <- file.path(config$path, config$pop_file)
  }
  
  if (!file.exists(pop_path) && !file.exists(config$pop_file)) {
    stop("Population file not found: ", config$pop_file)
  }
  
  # Load data (supporting both CSV and RDS based on your config snippets)
  if (grepl(".rds$", pop_path, ignore.case = TRUE)) {
    pops <- readRDS(pop_path)
  } else {
    pops <- fread(pop_path) 
    # Ensure standard column names if CSV
    if(!"N" %in% names(pops) && "pop" %in% names(pops)) setnames(pops, "pop", "N")
  }
  
  # 2. Run Estimations
  # ------------------
  # Passing the whole config list to the sub-functions
  
  estimate_initiation(config)
  estimate_quitting(config)
  estimate_relapse(config)
  
  # 3. Net Initiation (Synthetic Cohort)
  # ------------------------------------
  out_dir <- file.path(config$path, "outputs")
  
  # Load the fresh estimates
  init_data    <- readRDS(file.path(out_dir, paste0("init_forecast_data_", config$country, ".rds")))
  quit_data    <- readRDS(file.path(out_dir, paste0("quit_forecast_data_", config$country, ".rds")))
  relapse_data <- readRDS(file.path(out_dir, paste0("relapse_forecast_data_", config$country, ".rds")))
  
  calculate_net_initiation(init_data, quit_data, relapse_data, pops, config)
  
  # 4. Summaries & Plots
  # --------------------
  summarise_smoking_transitions(config, pops)
  
  message(sprintf(">> Completed %s", config$country))
}