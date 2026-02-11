#' Estimate smoking quit probabilities (The Flow Equation)
#'
#' @description
#' Calculates the probability of quitting required to balance the population 
#' stocks (Current, Former, Never) given the flows (Initiation, Relapse, Mortality).
#' 
#' @details
#' Solves the equation:
#' N(t+1) = N(t) * Survival * (1 - Quit) + (Former * Relapse) + (Never * Init)
#' Rearranged to solve for Quit.
#'
#' @param trend_data Output of trend_fit().
#' @param survivorship_data Output of prep_surv().
#' @param mortality_data Output of smoke_surv().
#' @param relapse_data Output of prep_relapse() (aggregated version).
#' @param initiation_data Output of init_adj().
#' @param min_age,max_age,min_year,max_year Integers.
#' @importFrom data.table setDT := setnames shift merge setkeyv
#' @export
quit_est <- function(
    trend_data,
    survivorship_data,
    mortality_data,
    relapse_data,
    initiation_data,
    min_age = 11,
    max_age = 89,
    min_year = 2003,
    max_year = 2018
) {
  
  # 1. Create Master Grid
  master_data <- data.table(expand.grid(
    sex = c("Male", "Female"),
    age = min_age:max_age,
    year = min_year:max_year,
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  
  # 2. Merge All Inputs
  # We use left joins (all.x=TRUE) to keep the grid structure
  
  # Survivorship (lx)
  master_data <- merge(master_data, survivorship_data, 
                       by = c("sex", "age", "year", "imd_quintile"), all.x = TRUE)
  
  # Prevalence Trends (current, former, never)
  # Note: trend_data might have cohort col, remove if duplicative
  t_data <- copy(trend_data)
  if("cohort" %in% names(t_data)) t_data[, cohort := NULL]
  master_data <- merge(master_data, t_data, 
                       by = c("sex", "age", "year", "imd_quintile"), all.x = TRUE)
  
  # Mortality Differentials (px)
  master_data <- merge(master_data, mortality_data, 
                       by = c("sex", "age", "year", "imd_quintile"), all.x = TRUE)
  
  # Relapse Rates
  master_data <- merge(master_data, relapse_data, 
                       by = c("sex", "age", "year", "imd_quintile"), all.x = TRUE)
  
  # Initiation Rates
  master_data <- merge(master_data, initiation_data[, .(age, year, sex, imd_quintile, p_start)], 
                       by = c("sex", "age", "year", "imd_quintile"), all.x = TRUE)
  
  # Assumption: No initiation after age 30 (clean up NAs)
  master_data[is.na(p_start), p_start := 0]
  master_data[age > 30, p_start := 0]
  
  # 3. The Flow Calculation
  master_data[, cohort := year - age]
  
  # CRITICAL: Sort by cohort and age to ensure 'shift' works along the life-course
  setkeyv(master_data, c("cohort", "sex", "imd_quintile", "age"))
  
  # Get next year's values (Lead)
  # lx1 = Survivorship at t+1
  # current1 = Prevalence at t+1
  master_data[, lx1 := shift(lx, type = "lead"), by = c("cohort", "sex", "imd_quintile")]
  master_data[, current1 := shift(current, type = "lead"), by = c("cohort", "sex", "imd_quintile")]
  
  # Calculate components of the quit equation
  # c1 represents the ratio of smokers surviving to next year vs current smokers surviving this year
  master_data[, c1 := (lx1 * current1) / (lx * current * current_px)]
  
  # c2 represents the inflow of new smokers (Relapse + Initiation)
  # normalized by the surviving current smoker population
  master_data[, c2 := (1 / (current * current_px)) * (former * former_px * p_relapse + never * never_px * p_start)]
  
  # c2_no_init is for the counterfactual (Relapse only)
  master_data[, c2_no_init := (1 / (current * current_px)) * (former * former_px * p_relapse)]
  
  # Solve for Quit Prob
  master_data[, p_quit := 1 - c1 + c2]
  master_data[, p_quit_no_init := 1 - c1 + c2_no_init]
  
  # 4. Cleanup and Clamping
  # Force boundaries [0, 1]
  master_data[age == max_age, `:=`(p_quit = 0, p_quit_no_init = 0)]
  
  clamp <- function(x) pmin(pmax(x, 0), 1)
  master_data[, p_quit := clamp(p_quit)]
  master_data[, p_quit_no_init := clamp(p_quit_no_init)]
  master_data[is.na(p_quit), p_quit := 0]
  master_data[is.na(p_quit_no_init), p_quit_no_init := 0]
  
  # Filter final output
  # We drop the last year because we cannot calculate forward leads for it
  master_data <- master_data[year < max_year]
  master_data <- master_data[age < max_age]
  
  cols_keep <- c("sex", "age", "year", "imd_quintile", "p_quit", "p_quit_no_init")
  return(master_data[, ..cols_keep])
}