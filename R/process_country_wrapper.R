#' Master Process Wrapper
#'
#' @description Orchestrates the estimation of smoking transition probabilities,
#' calculates empirical bootstrap uncertainty, and exports to a professional Excel report.
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
  
  if (grepl(".rds$", pop_path, ignore.case = TRUE)) {
    pops <- readRDS(pop_path)
  } else {
    pops <- fread(pop_path) 
  }
  
  if(!"N" %in% names(pops) && "pop" %in% names(pops)) {
    setnames(pops, "pop", "N")
  }
  
  # 1B. Load Survey Data
  # -----------------------
  survey_path <- file.path(config$path, config$survey_file)
  
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
  
  # 2. Run Baseline Estimations (Point Estimates)
  # ---------------------------------------------
  message(">> Running baseline point estimates...")
  
  # We run these with boot_mode = FALSE so they save to disk normally
  estimate_initiation(config, survey_data, boot_mode = FALSE)
  estimate_relapse(config, survey_data, boot_mode = FALSE)
  estimate_quitting(config, survey_data, tob_mort_data, tob_mort_data_cause, boot_mode = FALSE)
  
  # 3. Baseline Net Initiation 
  # ------------------------------------
  out_dir <- file.path(config$path, "outputs")
  
  # Load the fresh baseline estimates
  base_init         <- readRDS(file.path(out_dir, paste0("init_forecast_data_", config$country, ".rds")))
  base_quit         <- readRDS(file.path(out_dir, paste0("quit_forecast_data_", config$country, ".rds")))
  base_quit_no_init <- readRDS(file.path(out_dir, paste0("quit_forecast_data_no_init_", config$country, ".rds")))
  base_relapse      <- readRDS(file.path(out_dir, paste0("relapse_by_age_imd_timesincequit_", config$country, ".rds")))
  
  # Calculate Baseline Net Initiation
  base_net <- calculate_net_initiation(base_init, base_quit, base_relapse, pops, config, boot_mode = FALSE)
  
  # 4. Generate Empirical Uncertainty Intervals
  # -----------------------------------------
  B_samples <- ifelse(is.null(config$kn_samp), 100, config$kn_samp)
  message(sprintf("\n>> Running empirical bootstrap (%d iterations)...", B_samples))
  
  # Call our new pipeline
  boot_results <- run_bootstrap_pipeline(config, survey_data, pops, tob_mort_data, tob_mort_data_cause, B = B_samples)
  
  message(">> Aggregating uncertainty bounds...")
  init_ci         <- aggregate_uncertainty(boot_results$init, "p_start")
  quit_ci         <- aggregate_uncertainty(boot_results$quit, "p_quit")
  quit_no_init_ci <- aggregate_uncertainty(boot_results$quit_no_init, "p_quit_no_init")
  relapse_ci      <- aggregate_uncertainty(boot_results$relapse, "p_relapse")
  net_ci          <- aggregate_uncertainty(boot_results$net, "p_start_net")
  
  # Helper to safely merge baseline and CIs
  merge_ci <- function(base, ci, prob_col) {
    merge_cols <- setdiff(names(ci), paste0(prob_col, c("_lower", "_upper", "_se")))
    merge(base, ci, by = merge_cols, all.x = TRUE)
  }
  
  init_final         <- merge_ci(base_init, init_ci, "p_start")
  quit_final         <- merge_ci(base_quit, quit_ci, "p_quit")
  quit_no_init_final <- merge_ci(base_quit_no_init, quit_no_init_ci, "p_quit_no_init")
  relapse_final      <- merge_ci(base_relapse, relapse_ci, "p_relapse")
  net_final          <- merge_ci(base_net, net_ci, "p_start_net")
  
  # Wrap in lists to preserve your original downstream data structures
  init_data_uncertainty         <- list(data = init_final)
  quit_data_uncertainty         <- list(data = quit_final)
  quit_no_init_uncertainty      <- list(data = quit_no_init_final)
  relapse_data_uncertainty      <- list(data = relapse_final)
  net_init_data_uncertainty     <- list(data = net_final)
  
  # Save main files
  saveRDS(init_data_uncertainty,     file.path(out_dir, paste0("init_data_", config$country, "_uncertainty.rds")))
  saveRDS(quit_data_uncertainty,     file.path(out_dir, paste0("quit_data_", config$country, "_uncertainty.rds")))
  saveRDS(relapse_data_uncertainty,  file.path(out_dir, paste0("relapse_data_", config$country, "_uncertainty.rds")))
  saveRDS(net_init_data_uncertainty, file.path(out_dir, paste0("net_init_data_", config$country, "_uncertainty.rds")))
  saveRDS(quit_no_init_uncertainty,  file.path(out_dir, paste0("quit_no_init_data_", config$country, "_uncertainty.rds")))
  
  # 5. Export Report
  # ----------------
  write_excel_report(config, 
                     init_data_uncertainty, 
                     quit_data_uncertainty, 
                     relapse_data_uncertainty,
                     net_init_data_uncertainty,
                     quit_no_init_uncertainty)
  
  message(paste(">> Done with", config$country))
  return(invisible(TRUE))
}