#' Adjust probabilities of ever-smoking (Holford Method)
#'
#' @description
#' Calibrates retrospective recall data against observed period trends using 
#' the Holford (2014) method. This ensures that cohort histories sum up to 
#' match the "truth" seen in cross-sectional surveys at specific ages.
#'
#' @param init_data Data table - raw estimates from init_est().
#' @param ever_smoke_data Data table - trend targets from ever_smoke().
#' @param ref_age Integer - the index age for calibration (default 30).
#' @param fix_ref_age Logical. If TRUE, forces ref_age even if data is sparse.
#' @param min_ref Integer - minimum age to allow for reference.
#' @param cohorts Integer vector - cohorts to adjust.
#' @param period_start Integer - first year of data.
#' @param period_end Integer - last year of data.
#' @importFrom data.table setDT := copy
#' @export
init_adj <- function(
    init_data,
    ever_smoke_data,
    ref_age = 30,
    fix_ref_age = FALSE,
    min_ref = 25,
    cohorts = 1973:2020,
    period_start = 2003,
    period_end = 2018
) {
  
  dt <- copy(init_data)
  
  # Filter cohorts
  dt <- dt[cohort %in% cohorts]
  
  # 1. Determine Reference Ages
  # Dynamic reference age allows us to use older cohorts where we only have older data
  if(fix_ref_age == FALSE) {
    dt[cohort <= (period_end - ref_age), ref_ages := ref_age]
    dt[cohort > (period_end - ref_age), ref_ages := period_end - cohort]
    dt[cohort < (period_start - ref_age), ref_ages := period_start - cohort]
    
    # Filter out cohorts where the ref age is too young to be reliable
    dt <- dt[ref_ages >= min_ref]
  } else {
    dt[, ref_ages := ref_age]
  }
  
  # 2. Calculate Adjustment Factors
  # We look at the cumulative prob at the reference age in the raw data
  # vs the modeled trend data.
  
  # Get raw values at reference age
  ref_data <- dt[age == ref_ages]
  
  # Prepare trend data
  evr_smk_ref <- copy(ever_smoke_data)
  evr_smk_ref[, cohort := year - ref_age]
  evr_smk_ref[, year := NULL]
  
  # Merge
  ref_data <- merge(ref_data, evr_smk_ref, by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)
  
  # Calculate scalar: Target / Raw
  ref_data[, adjustment_factor := fitted_trends / p_ever_smoker]
  
  # Merge scalar back to main data
  dt <- merge(dt, ref_data[, .(cohort, sex, imd_quintile, adjustment_factor)], 
              by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)
  
  # Apply adjustment
  dt[, p_ever_smoker_adj := p_ever_smoker * adjustment_factor]
  
  # 3. Standardization and Extrapolation
  # We need a full grid of values.
  domain <- data.table(expand.grid(
    cohort = unique(dt$cohort),
    sex = unique(dt$sex),
    imd_quintile = unique(dt$imd_quintile),
    age = unique(dt$age)
  ))
  
  domain <- merge(domain, dt[, .(cohort, sex, imd_quintile, age, ref_ages, p_ever_smoker_adj)],
                  by = c("cohort", "sex", "imd_quintile", "age"), all.x = TRUE)
  
  # Fill missing ref_ages within groups
  domain[, ref_ages := unique(ref_ages[!is.na(ref_ages)]), by = .(cohort, sex, imd_quintile)]
  
  # LOCF (Last Observation Carried Forward) for ages > ref_age
  # Initiation is assumed to stop/flatten after the reference age (usually 30)
  domain[, last_val := p_ever_smoker_adj[age == ref_ages], by = .(cohort, sex, imd_quintile)]
  domain[age > ref_ages, p_ever_smoker_adj := last_val]
  domain[, `:=`(last_val = NULL, ref_ages = NULL)]
  
  # 4. Handle Future Cohorts (Extrapolation)
  maxc <- max(domain$cohort)
  if(max(cohorts) > maxc) {
    
    # Create extension grid
    cohorts_ext <- min(domain$cohort):max(cohorts)
    domain_ex <- data.table(expand.grid(
      cohort = cohorts_ext,
      sex = unique(dt$sex),
      imd_quintile = unique(dt$imd_quintile),
      age = unique(dt$age)
    ))
    
    domain_ex <- merge(domain_ex, domain, by = c("cohort", "sex", "imd_quintile", "age"), all.x = TRUE)
    
    # Calculate average profile of the last 5 observed cohorts
    data_av <- domain[cohort %in% (maxc - 5):maxc, .(av10 = mean(p_ever_smoker_adj, na.rm = TRUE)), 
                      by = .(age, sex, imd_quintile)]
    
    domain_ex <- merge(domain_ex, data_av, by = c("age", "sex", "imd_quintile"), all.x = TRUE)
    
    # Fill future cohorts with average profile
    domain_ex[cohort > maxc, p_ever_smoker_adj := av10]
    domain_ex[, av10 := NULL]
    
    # Re-apply trend adjustment to these extrapolated cohorts
    # (Same logic as above, but for future cohorts using projected trends)
    domain_ex[cohort > maxc, ref_ages := ref_age]
    
    ref_data_ex <- domain_ex[cohort > maxc & age == ref_ages]
    evr_smk_ref <- copy(ever_smoke_data)
    evr_smk_ref[, cohort := year - ref_age]
    
    ref_data_ex <- merge(ref_data_ex, evr_smk_ref[, .(cohort, sex, imd_quintile, fitted_trends)], 
                         by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)
    
    ref_data_ex[, adjustment_factor := fitted_trends / p_ever_smoker_adj]
    
    domain_ex <- merge(domain_ex, ref_data_ex[, .(cohort, sex, imd_quintile, adjustment_factor)], 
                       by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)
    
    domain_ex[cohort > maxc, p_ever_smoker_adj := p_ever_smoker_adj * adjustment_factor]
    
    # LOCF for future
    domain_ex[cohort > maxc, last_val := p_ever_smoker_adj[age == ref_ages], by = .(cohort, sex, imd_quintile)]
    domain_ex[cohort > maxc & age > ref_ages, p_ever_smoker_adj := last_val]
    
    domain_ex[, `:=`(ref_ages = NULL, adjustment_factor = NULL, last_val = NULL)]
    domain <- domain_ex
  }
  
  domain[, year := cohort + age]
  
  return(domain[])
}