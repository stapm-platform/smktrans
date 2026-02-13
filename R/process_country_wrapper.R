#' Master Process Wrapper
#' @param config A named list containing all country-specific parameters
#' @export
process_country <- function(config) {
  
  message(paste0("\n", paste(rep("=", 60), collapse = "")))
  message(sprintf(" PROCESSING: %s", config$country))
  message(paste0(paste(rep("=", 60), collapse = ""), "\n"))
  
  # 1A. Load Population Data
  # -----------------------
  pop_path <- config$pop_file
  
  # Load data
  if (grepl(".rds$", pop_path, ignore.case = TRUE)) {
    pops <- readRDS(pop_path)
  } else {
    pops <- fread(pop_path) 
    # Ensure standard column names if CSV
    if(!"N" %in% names(pops) && "pop" %in% names(pops)) setnames(pops, "pop", "N")
  }
  
  # 1B. Load Survey Data
  # -----------------------
  survey_path <- file.path(config$path, config$survey_file)
  
  # Load data
  if (grepl(".rds$", survey_path, ignore.case = TRUE)) {
    survey_data <- readRDS(survey_path)
  } else {
    survey_data <- fread(survey_path)
  }
  
  # 2. Run Estimations
  # ------------------
  # Passing the whole config list to the sub-functions
  
  estimate_initiation(config, survey_data)
  estimate_relapse(config, survey_data)
  estimate_quitting(config)
  
  
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