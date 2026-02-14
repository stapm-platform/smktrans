#' Estimate and Forecast Smoking Initiation
#'
#' @description
#' 1. Estimates historical initiation (cumulative -> density).
#' 2. Forecasts future initiation using `quit_forecast` (continuing trend).
#' 3. Saves raw, adjusted, and forecasted outputs.
#'
#' @param config List. Must contain: first_year, last_year, min_age, max_age, ref_age, 
#' smokefree_target_year, age_trend_limit_init, smooth_rate_dim_init, k_smooth_age_init.
#'
#' @export
estimate_initiation <- function(config, survey_data) {
  
  message(">> [Step 1] Estimating & Forecasting Initiation...")
  
  # A. Estimate Raw Initiation (Cohort Cumulative)
  # -------------------------------------------------------------------------
  init_data_raw <- init_est(
    data = survey_data,
    strat_vars = c("sex", "imd_quintile")
  )
  saveRDS(init_data_raw, file.path(config$path, "outputs", paste0("init_data_raw_", config$country, ".rds")))
  
  # B. Estimate 'Ever Smoker' Trends (for adjustment)
  # -------------------------------------------------------------------------
  
  ever_smoke_data <- ever_smoke(
    data = survey_data,
    time_horizon = config$time_horizon,
    num_bins = 7,
    model = config$init_model_choice, 
    min_age = config$min_age,
    min_year = config$first_year,
    age_cats = c("25-34")
  )
  saveRDS(ever_smoke_data, file.path(config$path, "outputs", paste0("ever_smoke_data_", config$country, ".rds")))
  
  # C. Adjust for Recall Bias
  # -------------------------------------------------------------------------
  init_data_adj <- init_adj(
    init_data = copy(init_data_raw),
    ever_smoke_data = copy(ever_smoke_data$predicted_values),
    ref_age = config$ref_age,
    fix_ref_age = FALSE,
    min_ref = 21,
    cohorts = (config$first_year - config$ref_age):config$time_horizon,
    period_start = config$first_year, 
    period_end = config$last_year
  )
  saveRDS(init_data_adj, file.path(config$path, "outputs", paste0("init_data_adj_", config$country, ".rds")))
  
  # D. Convert to Density (Annual Probability)
  # -------------------------------------------------------------------------
  smk_init_data <- p_dense(
    data = copy(init_data_adj),
    cum_func_var = "p_ever_smoker_adj",
    strat_vars = c("cohort", "sex", "imd_quintile"),
    lowest_year = config$first_year, 
    max_year = config$last_year
  )
  saveRDS(smk_init_data, file.path(config$path, "outputs", paste0("smk_init_data_", config$country, ".rds")))
  
  # E. Forecast
  # -------------------------------------------------------------------------
  message("   > Forecasting Initiation Trends...")
  
  init_forecast_data <- quit_forecast(
    data = copy(smk_init_data),
    forecast_var = "p_start",
    forecast_type = "continuing", 
    cont_limit = config$cont_limit,
    oldest_year = config$first_year,
    youngest_age = config$min_age,
    oldest_age = config$ref_age,
    age_cont_limit = config$age_trend_limit_init,
    first_year = config$first_year,       # Assuming forecast base is full data range
    jump_off_year = config$last_year - 1, # As per your script
    time_horizon = config$time_horizon,
    smooth_rate_dim = config$smooth_rate_dim_init,
    k_smooth_age = config$k_smooth_age_init
  )
  
  # Filter Age Range
  init_forecast_data <- init_forecast_data[age >= config$min_age & age <= config$max_age]
  
  # F. Save Final Outputs
  # -------------------------------------------------------------------------
  saveRDS(init_forecast_data, file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".rds")))
  write.csv(init_forecast_data, file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".csv")), row.names = FALSE)
  
  return(invisible(init_forecast_data))
}