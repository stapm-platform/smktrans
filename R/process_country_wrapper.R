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
  
  # 1D. Pin the trend prediction grid
  # ---------------------------------
  # The IMD levels have to be fixed from the FULL survey before any resampling,
  # otherwise a bootstrap sample that happens to miss a quintile would fit a
  # different model and return a smaller grid. Same reasoning for the age range.
  config$trend_grid_imd   <- sort(unique(as.character(survey_data$imd_quintile)))
  config$trend_grid_ages  <- min(survey_data$age):max(survey_data$age)
  if (is.null(config$trend_last_year)) config$trend_last_year <- config$last_year
  
  message(sprintf(">> Trend grid: ages %d-%d, years %d-%d, %d IMD quintiles",
                  min(config$trend_grid_ages), max(config$trend_grid_ages),
                  config$first_year, config$trend_last_year,
                  length(config$trend_grid_imd)))
  if (config$trend_last_year > config$last_year) {
    message(sprintf("   NOTE: years %d-%d are extrapolated beyond the survey data.",
                    config$last_year + 1, config$trend_last_year))
  }
  
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
  base_trend        <- readRDS(file.path(out_dir, paste0("smoking_trends_", config$country, ".rds")))
  
  # Calculate Baseline Net Initiation
  base_net <- calculate_net_initiation(base_init, base_quit, base_relapse, pops, config, boot_mode = FALSE)
  
  # 4. Generate Empirical Uncertainty Intervals
  # -----------------------------------------
  B_samples <- ifelse(is.null(config$kn_samp), 100, config$kn_samp)
  
  # The exported central estimates are bootstrap medians, so they depend on the
  # random draws. Without a seed the delivery cannot be reproduced or diffed
  # against a previous one. Refuse rather than silently produce a one-off.
  if (is.null(config$seed)) {
    stop("process_country: config$seed is not set. The exported estimates are ",
         "bootstrap medians and would not be reproducible. Set a seed in the ",
         "country config in 10_run_smoking_transitions.R.")
  }
  message(sprintf("\n>> Running empirical bootstrap (%d iterations)...", B_samples))
  
  # Call the bootstrap pipeline
  boot_results <- run_bootstrap_pipeline(config, survey_data, pops, tob_mort_data, tob_mort_data_cause,
                                         B = B_samples, seed = config$seed)
  
  message(">> Aggregating uncertainty bounds...")
  init_ci         <- aggregate_uncertainty(boot_results$init, "p_start")
  quit_ci         <- aggregate_uncertainty(boot_results$quit, "p_quit")
  quit_no_init_ci <- aggregate_uncertainty(boot_results$quit_no_init, "p_quit_no_init")
  relapse_ci      <- aggregate_uncertainty(boot_results$relapse, "p_relapse")
  net_ci          <- aggregate_uncertainty(boot_results$net, "p_start_net")
  
  # Trends come out of trend_fit wide (one column per smoking state), so melt to
  # long and group by smk.state as well as the demographics.
  state_cols <- setdiff(names(boot_results$trend),
                        c("boot_id", "age", "year", "sex", "imd_quintile", "cohort"))
  boot_trends <- melt(boot_results$trend,
                      id.vars = c("boot_id", "age", "year", "sex", "imd_quintile"),
                      measure.vars = state_cols,
                      variable.name = "smk.state",
                      value.name = "probability",
                      variable.factor = FALSE)
  
  trend_ci <- aggregate_uncertainty(boot_trends, "probability", extra_keys = "smk.state")
  
  # Helper to safely merge baseline and CIs (Overwrites base estimate with bootstrap median)
  merge_ci <- function(base, ci, prob_col) {
    
    # 1. Define the keys that exist in both tables
    potential_keys <- c("year", "age", "sex", "imd_quintile", "time_since_quit")
    common_keys <- intersect(potential_keys, intersect(names(base), names(ci)))
    
    # 2. Drop the original central estimate from the base table
    # so it doesn't clash with our new median estimate from the CI table.
    if (prob_col %in% names(base)) {
      base[, (prob_col) := NULL]
    }
    
    # 3. Perform the merge. The new median automatically becomes the main estimate!
    result <- merge(base, ci, by = common_keys, all.x = TRUE)
    
    return(result)
  }
  
  init_final         <- merge_ci(base_init, init_ci, "p_start")
  quit_final         <- merge_ci(base_quit, quit_ci, "p_quit")
  quit_no_init_final <- merge_ci(base_quit_no_init, quit_no_init_ci, "p_quit_no_init")
  relapse_final      <- merge_ci(base_relapse, relapse_ci, "p_relapse")
  net_final          <- merge_ci(base_net, net_ci, "p_start_net")
  
  # The trend baseline is wide and the CI table is long, so it does not go
  # through merge_ci. Melt the baseline the same way and join on all five keys.
  base_trend_long <- melt(base_trend,
                          id.vars = c("age", "year", "sex", "imd_quintile"),
                          measure.vars = intersect(state_cols, names(base_trend)),
                          variable.name = "smk.state",
                          value.name = "probability_baseline",
                          variable.factor = FALSE)
  
  trend_final <- merge(trend_ci, base_trend_long,
                       by = c("year", "age", "sex", "imd_quintile", "smk.state"),
                       all.x = TRUE)
  
  if (anyNA(trend_final$probability_baseline)) {
    stop("process_country: ", sum(is.na(trend_final$probability_baseline)),
         " bootstrapped trend cells have no matching baseline estimate.")
  }
  
  # Wrap in lists to preserve your original downstream data structures
  init_data_uncertainty         <- list(data = init_final)
  quit_data_uncertainty         <- list(data = quit_final)
  quit_no_init_uncertainty      <- list(data = quit_no_init_final)
  relapse_data_uncertainty      <- list(data = relapse_final)
  net_init_data_uncertainty     <- list(data = net_final)
  trend_data_uncertainty        <- list(data = trend_final)
  
  # Save main files
  saveRDS(init_data_uncertainty,     file.path(out_dir, paste0("init_data_", config$country, "_uncertainty.rds")))
  saveRDS(quit_data_uncertainty,     file.path(out_dir, paste0("quit_data_", config$country, "_uncertainty.rds")))
  saveRDS(relapse_data_uncertainty,  file.path(out_dir, paste0("relapse_data_", config$country, "_uncertainty.rds")))
  saveRDS(net_init_data_uncertainty, file.path(out_dir, paste0("net_init_data_", config$country, "_uncertainty.rds")))
  saveRDS(quit_no_init_uncertainty,  file.path(out_dir, paste0("quit_no_init_data_", config$country, "_uncertainty.rds")))
  saveRDS(trend_data_uncertainty,    file.path(out_dir, paste0("smoking_trends_", config$country, "_uncertainty.rds")))
  
  # Raw draws. The prevalence targets need the full set of iterations to build
  # a covariance matrix, not just the summarised bounds.
  saveRDS(boot_results$quit,  file.path(out_dir, paste0("raw_boot_quit_data_", config$country, ".rds")))
  saveRDS(boot_results$trend, file.path(out_dir, paste0("raw_boot_smoking_trends_", config$country, ".rds")))

  # The survey-side aggregates for the same draws, so the prevalence targets
  # can be built from either source without re-running the bootstrap.
  if (is.null(boot_results$survey_prev)) {
    stop("process_country: the bootstrap returned no survey aggregates. ",
         "Check that the patched run_bootstrap_pipeline.R is the one being sourced.")
  }
  saveRDS(boot_results$survey_prev, file.path(out_dir, paste0("raw_boot_survey_prev_", config$country, ".rds")))
  
  # Run manifest. When a delivery is queried months later, this is what says
  # whether a difference is a real change or a different set of random draws.
  saveRDS(list(
    country      = config$country,
    seed         = config$seed,
    n_bootstrap  = B_samples,
    run_datetime = Sys.time(),
    package_version = tryCatch(as.character(utils::packageVersion("smktrans")),
                               error = function(e) NA_character_),
    config       = config
  ), file.path(out_dir, paste0("run_manifest_", config$country, ".rds")))
  
  # 5. Export Report
  # ----------------
  write_excel_report(config,
                     init_data_uncertainty,
                     quit_data_uncertainty,
                     relapse_data_uncertainty,
                     net_init_data_uncertainty,
                     quit_no_init_uncertainty)
  
  rm(list = ls(pattern = "boot_|res_|dt"))
  gc()
  
  message(paste(">> Done with", config$country))
  return(invisible(TRUE))
}
