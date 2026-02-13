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
  #ggplot(init_data_raw[cohort == 1999]) + geom_line(aes(x = age, y = p_ever_smoker, colour = imd_quintile, linetype = sex))
  saveRDS(init_data_raw, file.path(config$path, "outputs", paste0("init_data_raw_", config$country, ".rds")))
  
  # B. Estimate 'Ever Smoker' Trends (for adjustment)
  # -------------------------------------------------------------------------
  # Select model based on country logic (England = model2, others might differ)
  # Defaulting to model2 as per your script; adjust if Scotland requires different.
  if(config$country == "England") model_choice <- "model2"
  if(config$country == "Scotland") model_choice <- "model2"
  if(config$country == "Wales") model_choice <- "model2"
  
  ever_smoke_data <- ever_smoke(
    data = survey_data,
    time_horizon = 2100,
    num_bins = 7,
    model = model_choice, 
    min_age = config$min_age,
    min_year = config$first_year,
    age_cats = c("25-34")
  )
  #ggplot(ever_smoke_data$predicted_values) + geom_line(aes(x = year, y = fitted_trends, colour = imd_quintile, linetype = sex))
  saveRDS(ever_smoke_data, file.path(config$path, "outputs", paste0("ever_smoke_data_", config$country, ".rds")))
  
  # C. Adjust for Recall Bias
  # -------------------------------------------------------------------------
  init_data_adj <- init_adj(
    init_data = copy(init_data_raw),
    ever_smoke_data = copy(ever_smoke_data$predicted_values),
    ref_age = config$ref_age,
    fix_ref_age = FALSE,
    min_ref = 21,
    cohorts = (config$first_year - config$ref_age):2100,
    period_start = config$first_year, 
    period_end = config$last_year
  )
  #ggplot(init_data_adj[cohort == 1982]) + geom_line(aes(x = age, y = p_ever_smoker_adj, colour = imd_quintile, linetype = sex))
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
  #ggplot(smk_init_data[year == 2018]) + geom_line(aes(x = age, y = p_start, colour = imd_quintile, linetype = sex))
  saveRDS(smk_init_data, file.path(config$path, "outputs", paste0("smk_init_data_", config$country, ".rds")))
  
  # E. Forecast
  # -------------------------------------------------------------------------
  message("   > Forecasting Initiation Trends...")
  
  init_forecast_data <- quit_forecast(
    data = copy(smk_init_data),
    forecast_var = "p_start",
    forecast_type = "continuing", 
    cont_limit = config$smokefree_target_year + 10,
    oldest_year = config$first_year,
    youngest_age = config$min_age,
    oldest_age = config$ref_age,
    age_cont_limit = config$age_trend_limit_init,
    first_year = config$first_year,       # Assuming forecast base is full data range
    jump_off_year = config$last_year - 1, # As per your script
    time_horizon = 2100,
    smooth_rate_dim = config$smooth_rate_dim_init,
    k_smooth_age = config$k_smooth_age_init
  )
  
  # Filter Age Range
  init_forecast_data <- init_forecast_data[age >= config$min_age & age <= config$max_age]
  
  #ggplot(init_forecast_data[year == 2018]) + geom_line(aes(x = age, y = p_start, colour = imd_quintile, linetype = sex))
  
  # F. Save Final Outputs
  # -------------------------------------------------------------------------
  saveRDS(init_forecast_data, file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".rds")))
  write.csv(init_forecast_data, file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".csv")), row.names = FALSE)
  
  return(invisible(init_forecast_data))
}