#' Forecast relapse probabilities (Time-Since-Quit Stratified)
#'
#' @description
#' Applies the trends forecast by `quit_forecast` (which are only Age/Sex/IMD)
#' to the detailed Time-Since-Quit data. It calculates a scaling factor
#' from the "jump-off" year and applies it to future years.
#'
#' @param relapse_forecast_data Data table - output from quit_forecast() (The trend).
#' @param relapse_by_age_imd_timesincequit Data table - detailed base rates from prep_relapse().
#' @param jump_off_year Integer.
#' @importFrom data.table copy := setnames merge
#' @export
relapse_forecast <- function(
    relapse_forecast_data,
    relapse_by_age_imd_timesincequit,
    jump_off_year = 2018
) {
  
  # 1. Prepare Trend Data (The Scaler)
  # Get Jump-off values (Baseline)
  trend_jo <- relapse_forecast_data[year == jump_off_year, .(age, sex, imd_quintile, p_relapse_jo = p_relapse)]
  
  # Get Future values (Target)
  trend_future <- relapse_forecast_data[year > jump_off_year, .(year, age, sex, imd_quintile, p_relapse)]
  
  # Calculate Scaling Factor (Target / Baseline)
  scaler <- merge(trend_future, trend_jo, by = c("age", "sex", "imd_quintile"), all.x = TRUE)
  
  # Avoid division by zero
  scaler[, scaling := ifelse(p_relapse_jo > 1e-9, p_relapse / p_relapse_jo, 1)]
  
  # Cleanup
  scaler[, `:=`(p_relapse = NULL, p_relapse_jo = NULL)]
  
  # 2. Apply Scaling to Detailed Data
  # We operate on the detailed TSQ dataset
  dt_detailed <- copy(relapse_by_age_imd_timesincequit)
  
  # Merge scaling factors
  dt_detailed <- merge(dt_detailed, scaler, 
                       by = c("year", "age", "sex", "imd_quintile"), 
                       all.x = TRUE)
  
  # 3. Apply Logic
  # For years <= jump_off_year, scaling is NA (keep original). 
  # For future years, apply scaling.
  # For age 89, we force scaling = 1 (stability).
  
  dt_detailed[year <= jump_off_year | age == 89, scaling := 1]
  dt_detailed[is.na(scaling), scaling := 1] # Safety fill
  
  # Apply
  dt_detailed[, p_relapse := p_relapse * scaling]
  dt_detailed[, scaling := NULL]
  
  return(dt_detailed[])
}