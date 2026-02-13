#' Master Process Wrapper
#'
#' @description Orchestrates the estimation of smoking transition probabilities,
#' calculates uncertainty, and exports to a professional Excel report.
#' @param config A named list of country-specific parameters.
#' @return A list containing the final datasets and samples.
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
  
  # 1C. Load mortality data
  # -----------------------
  mort_data_dir <- file.path(config$path, "intermediate_data")
  tob_mort_data_cause <- readRDS(file.path(mort_data_dir, "tob_mort_data_cause.rds"))
  tob_mort_data <- readRDS(file.path(mort_data_dir, "tob_mort_data_trans.rds"))
  
  # 2. Run Estimations
  # ------------------
  # Passing the whole config list to the sub-functions
  
  estimate_initiation(config, survey_data)
  estimate_relapse(config, survey_data)
  estimate_quitting(config, survey_data, tob_mort_data, tob_mort_data_cause)
  
  # 3. Net Initiation (Synthetic Cohort)
  # ------------------------------------
  out_dir <- file.path(config$path, "outputs")
  
  # Load the fresh estimates
  init_data    <- readRDS(file.path(out_dir, paste0("init_forecast_data_", config$country, ".rds")))
  quit_data    <- readRDS(file.path(out_dir, paste0("quit_forecast_data_", config$country, ".rds")))
  quit_data_no_init    <- readRDS(file.path(out_dir, paste0("quit_forecast_data_no_init_", config$country, ".rds")))
  relapse_data <- readRDS(file.path(out_dir, paste0("relapse_forecast_data_", config$country, ".rds")))
  
  net_init_dt <- calculate_net_initiation(init_data, quit_data, relapse_data, pops, config)
  
  # 4. Generate uncertainty intervals
  # ------------------------------------
  message(">> Calculating uncertainty... if kn_samp = 100 -> time to go grab a coffee (and maybe some biscuits).")
  message("    if kn_samp = 1000 -> time to go to bed.")
  
  init_data_uncertainty <- generate_uncertainty(init_data, "p_start", config$kn, config$kn_samp, config$kR)
  quit_data_uncertainty <- generate_uncertainty(data = quit_data, prob_col = "p_quit", n_eff = config$kn, n_samp = config$kn_samp, correlation = config$kR)
  relapse_data_uncertainty <- generate_uncertainty(relapse_data, "p_relapse", config$kn, config$kn_samp, config$kR) # takes the longest to run
  net_init_uncertainty <- generate_uncertainty(net_init_dt, "p_start_net", config$kn, config$kn_samp, config$kR)
  quit_no_init_uncertainty <- generate_uncertainty(quit_data_no_init, "p_quit_no_init", config$kn, config$kn_samp, config$kR)
  
  # 5. Export Report
  write_excel_report(config, 
                     init_data_uncertainty$data, 
                     quit_data_uncertainty$data, 
                     relapse_data_uncertainty$data, 
                     net_init_uncertainty$data,
                     quit_no_init_uncertainty$data)
  
  message(paste(">> Done with", config$country))
  return(invisible(TRUE))
}