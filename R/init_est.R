#' Cohort specific smoking initiation
#'
#' @description
#' Reconstructs longitudinal smoking histories from cross-sectional recall data.
#'
#' @param data Data table of individual characteristics. Must contain 'start_age'.
#' @param strat_vars Character vector of stratification variables.
#' @importFrom data.table := setDT setnames setkeyv CJ
#' @return A summarized data.table of initiation probabilities by age/year/cohort.
#' @export
init_est <- function(data, strat_vars = c("sex", "imd_quintile")) {
  
  # 1. Filter valid data
  dt <- copy(data)
  dt <- dt[!is.na(start_age)]
  dt[, start_age := round(start_age, 0)]
  dt <- dt[start_age >= 8]
  
  # Keep only necessary columns
  cols_to_keep <- c("wt_int", "year", "age", "start_age", "censor_age", strat_vars)
  dt <- dt[, ..cols_to_keep]
  
  # 2. Vectorized Expansion
  # Create a list of ages for each person (from 8 up to censoring age)
  # NOTE: The logic here creates the "Observed Risk Set"
  dt_long <- dt[, .(
    age_long = 8:(min(98, censor_age) - 1)
  ), by = cols_to_keep]
  
  # 3. Assign Event Status (1 = Initiated, 0 = At Risk)
  dt_long[, start_bin := as.integer(age_long == start_age)]
  
  # Remove years after initiation 
  dt_long <- dt_long[age_long <= start_age]
  
  # 4. Calculate Cohorts
  dt_long[, year_long := year - age + age_long]
  dt_long[, cohort_long := year_long - age_long]
  
  # Filter for relevant cohorts (born >= 1930) and max year check
  dt_long <- dt_long[cohort_long >= 1930 & year_long <= max(year)]
  
  # 5. Aggregate (The Numerator and Denominator)
  group_cols <- c("age_long", "cohort_long", strat_vars) # Removed year_long from group to recalculate later
  
  smk_data <- dt_long[age_long <= 30, .(
    p_start = sum(start_bin * wt_int, na.rm = TRUE) / sum(wt_int, na.rm = TRUE)
  ), by = group_cols]
  
  # --- 5.5 Standardization (Grid Expansion) ---
  
  # Define the full grid of required combinations
  # We use data.table::CJ (Cross Join) to create every possible combination
  # This ensures Age 8-30 exists for EVERY cohort and subgroup
  unique_cohorts <- unique(smk_data$cohort_long)
  unique_ages    <- 8:30 
  
  # Helper to get unique levels of stratification variables dynamically
  strat_levels <- lapply(strat_vars, function(v) unique(dt[[v]]))
  names(strat_levels) <- strat_vars
  
  # Create the skeleton table
  grid_args <- c(list(cohort_long = unique_cohorts, age_long = unique_ages), strat_levels)
  full_grid <- do.call(CJ, grid_args)
  
  # Merge observed data onto the full skeleton
  smk_data <- merge(full_grid, smk_data, 
                    by = c("cohort_long", "age_long", strat_vars), 
                    all.x = TRUE)
  
  # FILL GAPS: 
  # If a row is missing (NA), it means no initiation was observed there.
  # We set p_start to 0. When we calculate CumProd later, 
  # a 0 probability of start results in the Cumulative Value staying flat (LOCF).
  smk_data[is.na(p_start), p_start := 0]
  
  # Recalculate 'year' since we may have added rows where it didn't exist
  smk_data[, year_long := cohort_long + age_long]
  
  # ---------------------------------------------------------
  
  # 6. Cumulative Calculations
  
  # Ensure data is sorted strictly for cumprod
  setkeyv(smk_data, c("cohort_long", strat_vars, "age_long"))
  
  # Calculate Survival (Never Smoker) then convert to Ever Smoker
  # If p_start is 0 for imputed rows, p_ever_smoker will effectively "carry forward" 
  # the previous value.
  smk_data[, p_ever_smoker := 1 - cumprod(1 - p_start), by = c("cohort_long", strat_vars)]
  smk_data[, p_never_smoker := 1 - p_ever_smoker]
  
  # Final cleanup
  setnames(smk_data, c("age_long", "year_long", "cohort_long"), c("age", "year", "cohort"))
  
  return(smk_data[])
}