#' Generate a Single Bootstrap Sample for Complex Survey Data
#'
#' @param survey_data A data.table or data.frame containing survey data.
#' @return A resampled data.table.
#' @import data.table
generate_bootstrap_sample <- function(survey_data) {
  dt <- as.data.table(survey_data)
  
  has_psu <- "psu" %in% names(dt)
  has_cluster <- "cluster" %in% names(dt)
  has_year <- "year" %in% names(dt)
  
  # Determine stratification variables (usually year and cluster/strata)
  strat_vars <- character(0)
  if (has_year) strat_vars <- c(strat_vars, "year")
  if (has_cluster) strat_vars <- c(strat_vars, "cluster")
  
  if (has_psu) {
    # COMPLEX DESIGN: Resample PSUs within Strata
    # 1. Get unique PSUs per stratum
    psu_list <- unique(dt[, c(strat_vars, "psu"), with = FALSE])
    
    # 2. Sample PSUs with replacement within each stratum
    if (length(strat_vars) > 0) {
      sampled_psus <- psu_list[, .(psu = sample(psu, size = .N, replace = TRUE)), by = strat_vars]
    } else {
      sampled_psus <- psu_list[, .(psu = sample(psu, size = .N, replace = TRUE))]
    }
    
    # 3. Assign a new unique ID to each sampled PSU. 
    # This is CRITICAL because the same PSU might be drawn multiple times, 
    # and we need them to be treated as distinct groups in the resampled data.
    sampled_psus[, boot_psu_id := .I]
    
    # 4. Merge back to get the individuals (allow.cartesian because of duplicate PSUs)
    resampled_data <- merge(sampled_psus, dt, by = c(strat_vars, "psu"), allow.cartesian = TRUE)
    
  } else {
    # SIMPLE DESIGN: Resample individuals within strata
    if (length(strat_vars) > 0) {
      resampled_data <- dt[, .SD[sample(.N, .N, replace = TRUE)], by = strat_vars]
    } else {
      resampled_data <- dt[sample(.N, .N, replace = TRUE)]
    }
  }
  
  return(resampled_data)
}


