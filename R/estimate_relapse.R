#' Estimate and Forecast Smoking Relapse
#'
#' @description
#' 1. Prepares base relapse rates (Hawkins).
#' 2. Forecasts the Age/Sex/IMD specific trend using `quit_forecast`.
#' 3. Scales the Time-Since-Quit (TSQ) data using `relapse_forecast`.
#' 4. Imputes data for ages < 18.
#'
#' @export
estimate_relapse <- function(config, survey_data) {
  
  message(">> [Step 2] Estimating & Forecasting Relapse...")
  
  # A. Base Relapse Estimates
  # -------------------------------------------------------------------------
  # Requires hawkins_relapse data to be available (usually in smktrans or loaded raw)
  # If 'hawkins_relapse' is not global, load it here. 
  # Assuming it is passed or available via smktrans package data.
  
  relapse_data <- smktrans::prep_relapse(
    data = survey_data,
    hawkins_relapse = smktrans::hawkins_relapse,
    lowest_year = config$first_year,
    highest_year = config$last_year,
    youngest_age = 18
  )
  saveRDS(relapse_data, file.path(config$path, "outputs", paste0("relapse_data_", config$country, ".rds")))
  
  # B. Forecast Age/Sex/IMD Trend
  # -------------------------------------------------------------------------
  message("   > Forecasting Relapse Trends...")
  
  relapse_forecast_data <- quit_forecast(
    data = copy(relapse_data$relapse_by_age_imd),
    forecast_var = "p_relapse",
    forecast_type = "continuing",
    cont_limit = config$last_year + 3, # Specific logic from your script
    first_year = config$first_year,
    jump_off_year = config$last_year - 1,
    time_horizon = 2100,
    youngest_age = 18,
    oldest_age = config$max_age,
    age_cont_limit = config$age_trend_limit_relapse,
    oldest_year = config$first_year,
    smooth_rate_dim = config$smooth_rate_dim_relapse,
    k_smooth_age = config$k_smooth_age_relapse
  )
  
  saveRDS(relapse_forecast_data, file.path(config$path, "outputs", paste0("relapse_forecast_data_", config$country, ".rds")))
  
  # C. Apply Trend to Time-Since-Quit Data
  # -------------------------------------------------------------------------
  # Note: Assumes relapse_forecast() is loaded from R/relapse_forecast.R
  relapse_by_age_imd_timesincequit <- relapse_forecast(
    relapse_forecast_data = relapse_forecast_data,
    relapse_by_age_imd_timesincequit = relapse_data$relapse_by_age_imd_timesincequit,
    jump_off_year = config$last_year - 1
  )
  
  relapse_by_age_imd_timesincequit <- relapse_by_age_imd_timesincequit[age >= 18 & age <= config$max_age]
  
  # D. Impute Ages < 18 (Loop)
  # -------------------------------------------------------------------------
  # Your workflow manually copies age 18 data to ages min_age..17
  
  if(config$min_age < 18) {
    message("   > Imputing Relapse for ages < 18...")
    temp <- relapse_by_age_imd_timesincequit[age == 18]
    
    for(i in config$min_age:17) {
      relapse_by_age_imd_timesincequit <- rbindlist(list(
        relapse_by_age_imd_timesincequit,
        copy(temp)[, age := i]
      ), use.names = TRUE)
    }
  }
  
  # E. Save Final Outputs
  # -------------------------------------------------------------------------
  out_path_rds <- file.path(config$path, "outputs", paste0("relapse_forecast_data_", config$country, ".rds"))
  out_path_csv <- file.path(config$path, "outputs", paste0("relapse_forecast_data_", config$country, ".csv"))
  
  saveRDS(relapse_by_age_imd_timesincequit, out_path_rds)
  write.csv(relapse_by_age_imd_timesincequit, out_path_csv, row.names = FALSE)
  
  return(invisible(relapse_by_age_imd_timesincequit))
}