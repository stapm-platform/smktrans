

#' Adjust probabilities of ever-smoking
#'
#' Takes the probabilities of ever-smoking estimated from respondent recall and adjusts them
#' according to the reported proportions of ever-smokers at a reference age.
#'
#' This is based on the method applied by \insertCite{holford2014patterns;textual}{smktrans}.
#'
#' @param init_data Data table - raw estimates of the probabilities of 
#' ever-smoking by age, sex and IMD quintile.
#' @param ever_smoke_data Data table - reference values for the proportion of ever-smokers.
#' @param ref_age Integer - the index age for calibration
#' @param fix_ref_age Logical. If TRUE, ref_age is used for all cohorts
#' @param min_ref Integer - the minimum reference age to use for cohorts with not enough data on older ages
#' @param cohorts Integer vector - the cohorts to be adjusted. 
#' The min cohort might sensibly be period_start - ref_age, 
#' and the max cohort might be the baseline year of the model e.g. 2020.
#' @param period_start Integer - the first year of data (England - 2003, Scotland - 2008)
#' @param period_end Integer - the last year of data
#' 
#' @importFrom data.table setDT := copy
#' @importFrom Rdpack reprompt
#' 
#' @return Returns an updated version of init_data with a new 
#' variable containing the adjusted values for
#' the probabilities of ever smoking. The data has also 
#' been filtered to only include cohorts for which it is possible
#' to make an adjustment.
#' 
#' @references
#' \insertRef{holford2014patterns}{smktrans}
#' 
#' 
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Estimate the raw probabilities of ever-smoking
#' init_data_raw <- init_est(
#'   data = hse_data,
#'   strat_vars = c("sex", "imd_quintile")
#' )
#'
#' # Estimate the reference values - observed proportions of ever-smokers
#' ever_smoke_data <- ever_smoke(
#'   data = hse_data,
#'   time_horizon = 2100
#' )
#'
#' # Adjust the probabilities of ever-smoking
#' init_data_adj <- init_adj(
#'   init_data = copy(init_data_raw),
#'   ever_smoke_data = copy(ever_smoke_data$predicted_values),
#'   ref_age = 30,
#'   cohorts = 1971:2020, # 50 cohorts
#'   period_start = 2001,
#'   period_end = 2016
#' )
#'
#'
#' }
#'
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
  
  # Select cohorts to be adjusted
  init_data <- init_data[cohort %in% cohorts]
  
  if(fix_ref_age == FALSE) {
    # Set ref ages
    # considering that some cohorts will need younger or older ref ages
    # due to the limits of our period data
    init_data[cohort <= (period_end - ref_age), ref_ages := ref_age]
    init_data[cohort > (period_end - ref_age), ref_ages := period_end - cohort]
    init_data[cohort < (period_start - ref_age), ref_ages := period_start - cohort]
    
    # Limit the cohorts covered to those with a ref age of at least 25 years
    init_data <- init_data[ref_ages >= min_ref]
    
  } else {
    
    init_data[ , ref_ages := ref_age]
    
  }
  
  # Take a subset of data in which only the reference ages are retained for each cohort
  ref_data <- copy(init_data[age == ref_ages])
  
  # Assign the cohort to the reference values
  evr_smk_ref <- copy(ever_smoke_data)
  
  evr_smk_ref[ , cohort := year - ref_age]
  evr_smk_ref[ , year := NULL]
  
  # Merge this data subset with the corresponding estimates of ever-smoking
  ref_data <- merge(ref_data, evr_smk_ref, all.x = T, all.y = F, 
                    by = c("cohort", "sex", "imd_quintile"))
  
  # Calculate the factor by which to scale the cumulative curve of ever-smoking
  ref_data[ , adjustment_factor := fitted_trends / p_ever_smoker]
  
  # Retain just the variables needed
  ref_data <- ref_data[ , c("cohort", "sex", "imd_quintile", "adjustment_factor")]
  
  # Merge the adjustment factors back into the cohort data
  init_data <- merge(init_data, ref_data, all.x = T, all.y = F, 
                     by = c("cohort", "sex", "imd_quintile"))
  
  # Adjust the cumulative probabilities of ever-smoking
  init_data[ , p_ever_smoker_adj := p_ever_smoker * adjustment_factor]
  
  # Estimates for some of the later cohorts will stop at ages before 30 years
  # Create a standardised data table and hold the missing age-specific values constant up to age 30
  domain <- data.frame(expand.grid(
    cohort = unique(init_data$cohort),
    sex = unique(init_data$sex),
    imd_quintile = unique(init_data$imd_quintile),
    age = unique(init_data$age)
  ))
  setDT(domain)
  
  domain <- merge(domain, init_data[ , c("cohort", "sex", "imd_quintile", 
                                         "age", "ref_ages", "p_ever_smoker_adj")], 
                  by = c("cohort", "sex", "imd_quintile", "age"), all.x = T)
  
  # Fill NAs in ref ages
  domain[ , ref_ages := unique(ref_ages[!is.na(ref_ages)]),  
          by = c("cohort", "sex", "imd_quintile")]
  
  # Create a column containing the initiation probability at the reference age 
  domain[ , last_val := p_ever_smoker_adj[age == ref_ages], 
          by = c("cohort", "sex", "imd_quintile")]
  
  # Use this value to fill missing initiation probs at ages after the ref age
  domain[age > ref_ages, p_ever_smoker_adj := last_val]
  
  domain[ , `:=`(last_val = NULL, ref_ages = NULL)]
  
  # If extrapolation to later cohorts is required
  maxc <- max(domain$cohort)
  
  if(max(cohorts) > maxc) {
    
    # Creat a new standardised data table that includes all required cohorts
    domain_ex <- data.frame(expand.grid(
      cohort = min(domain$cohort):max(cohorts),
      sex = unique(init_data$sex),
      imd_quintile = unique(init_data$imd_quintile),
      age = unique(init_data$age)
    ))
    setDT(domain_ex)
    
    domain_ex <- merge(domain_ex, domain, 
                       by = c("cohort", "sex", "imd_quintile", "age"), all.x = T)
    
    # Give the cohorts to be extrapolated the average values for the last 5 cohorts
    
    # Calculate the average values
    data_av <- domain[cohort %in% (maxc - 5):maxc, list(av10 = mean(p_ever_smoker_adj, na.rm = T)), 
                      by = c("age", "sex", "imd_quintile")]
    
    # Merge into domain_ex
    domain_ex <- merge(domain_ex, data_av, by = c("sex", "imd_quintile", "age"), all.x = T)
    
    # Use these average values for the later cohorts
    domain_ex[cohort > maxc, p_ever_smoker_adj := av10]
    
    domain_ex[ , av10 := NULL]
    
    # Repeat the adjustment for the ongoing trend for the extrapolated cohorts
    
    # Set ref age
    domain_ex[cohort > maxc, ref_ages := ref_age]
    
    # Take a subset of data in which only the reference ages are retained for each cohort
    ref_data <- copy(domain_ex[cohort > maxc & age == ref_ages])
    
    # Assign the cohort to the reference values
    evr_smk_ref <- copy(ever_smoke_data)
    
    evr_smk_ref[ , cohort := year - ref_age]
    evr_smk_ref[ , year := NULL]
    
    # Merge this data subset with the corresponding estimates of ever-smoking
    ref_data <- merge(ref_data, evr_smk_ref, all.x = T, all.y = F, 
                      by = c("cohort", "sex", "imd_quintile"))
    
    # Calculate the factor by which to scale the cumulative curve of ever-smoking
    ref_data[ , adjustment_factor := fitted_trends / p_ever_smoker_adj]
    
    # Retain just the variables needed
    ref_data <- ref_data[ , c("cohort", "sex", "imd_quintile", "adjustment_factor")]
    
    # Merge the adjustment factors back into the cohort data
    domain_ex <- merge(domain_ex, ref_data, all.x = T, all.y = F, 
                       by = c("cohort", "sex", "imd_quintile"))
    
    # Adjust the cumulative probabilities of ever-smoking
    domain_ex[cohort > maxc, p_ever_smoker_adj := p_ever_smoker_adj * adjustment_factor]
    
    # Create a column containing the initiation probability at the reference age 
    domain_ex[cohort > maxc, last_val := p_ever_smoker_adj[age == ref_ages], 
              by = c("cohort", "sex", "imd_quintile")]
    
    # Use this value to fill missing initiation probs at ages after the ref age
    domain_ex[cohort > maxc & age > ref_ages, p_ever_smoker_adj := last_val]
    
    domain_ex[ , `:=`(ref_ages = NULL, adjustment_factor = NULL, last_val = NULL)]
    
    domain <- copy(domain_ex)
    
  }
  
  domain[ , year := cohort + age]
  
  return(domain[])
}

