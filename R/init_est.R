#' Cohort specific smoking initiation
#'
#' @description
#' Reconstructs longitudinal smoking histories from cross-sectional recall data.
#' It expands each individual's record into a time-series of 0s (non-smoker)
#' and 1s (initiation event).
#'
#' @details
#' This function avoids row-wise looping for speed. It uses an "expansion"
#' approach where we create a grid of all possible ages and join the
#' events to it.
#'
#' @param data Data table of individual characteristics. Must contain 'start_age'.
#' @param strat_vars Character vector of stratification variables (e.g., "sex", "imd_quintile").
#' @importFrom data.table := setDT melt setnames setkeyv
#' @return A summarized data.table of initiation probabilities by age/year/cohort.
#' @export
init_est <- function(data, strat_vars = c("sex", "imd_quintile")) {
  
  # 1. Filter valid data
  # We only want people who have a known start age
  # Standardize start_age and filter
  dt <- copy(data)
  dt <- dt[!is.na(start_age)]
  dt[, start_age := round(start_age, 0)]
  dt <- dt[start_age >= 8]
  
  # Keep only necessary columns to save memory
  cols_to_keep <- c("wt_int", "year", "age", "start_age", "censor_age", strat_vars)
  dt <- dt[, ..cols_to_keep]
  
  # 2. Vectorized Expansion
  # Instead of looping, we create a 'long' template of all relevant ages for everyone.
  # We use a list column approach which is very fast in data.table.
  
  # Create a list of ages for each person (from 8 up to their censoring age or 98)
  # capped at 98 to match original logic
  dt[, age_long := lapply(1:.N, function(i) 8:min(98, censor_age[i]))]
  
  # Unnest this list column (Explode the data)
  dt_long <- dt[, .(age_long = unlist(age_long)), by = setdiff(names(dt), "age_long")]
  
  # 3. Assign Event Status
  # 1 = Initiated at this age
  # 0 = At risk but did not initiate
  dt_long[, start_bin := ifelse(age_long == start_age, 1, 0)]
  
  # Remove years after initiation (they are no longer "at risk" of starting for the first time)
  dt_long <- dt_long[age_long <= start_age]
  
  # 4. Calculate Cohorts and Calendar Years
  dt_long[, year_long := year - age + age_long]
  dt_long[, cohort_long := year_long - age_long]
  
  # Filter for relevant cohorts (born >= 1930) and valid years
  dt_long <- dt_long[cohort_long >= 1930 & year_long <= max(year)]
  
  # 5. Aggregate (The Numerator and Denominator)
  # We calculate the weighted sum of initiators vs total risk pool
  group_cols <- c("age_long", "year_long", strat_vars)
  
  smk_data <- dt_long[age_long < 90, .(
    p_start = sum(start_bin * wt_int, na.rm = TRUE) / sum(wt_int, na.rm = TRUE)
  ), by = group_cols]
  
  # 6. Cumulative Calculations
  smk_data[, cohort_long := year_long - age_long]
  
  # Ensure data is sorted for cumulative product
  setkeyv(smk_data, c("cohort_long", strat_vars, "age_long"))
  
  # Calculate Survival (Never Smoker) then convert to Ever Smoker
  # p_ever = 1 - product(1 - p_start)
  smk_data[, p_ever_smoker := 1 - cumprod(1 - p_start), by = c("cohort_long", strat_vars)]
  smk_data[, p_never_smoker := 1 - p_ever_smoker]
  
  # Final cleanup
  setnames(smk_data, c("age_long", "year_long", "cohort_long"), c("age", "year", "cohort"))
  
  return(smk_data[])
}