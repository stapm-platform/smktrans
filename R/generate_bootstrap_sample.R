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
  
  strat_vars <- character(0)
  if (has_year) strat_vars <- c(strat_vars, "year")
  
  # ============================================================================
  # VIABILITY CHECK: Is 'cluster' safe to use as a stratum?
  # ============================================================================
  if (has_cluster) {
    if (has_psu) {
      # Calculate the maximum number of unique PSUs inside any cluster
      max_psus_per_cluster <- max(dt[, .(n_psu = uniqueN(psu)), by = c(strat_vars, "cluster")]$n_psu)
      
      # If at least one cluster has > 1 PSU, it is a viable grouping variable.
      # If max == 1, cluster and psu are 1-to-1, and stratifying by cluster will break the bootstrap.
      if (max_psus_per_cluster > 1) {
        strat_vars <- c(strat_vars, "cluster")
      }
    } else {
      # If there's no PSU column at all, cluster is safe to use for grouping individuals
      strat_vars <- c(strat_vars, "cluster")
    }
  }
  
  # Also check for a formal 'strata' column, just in case other datasets use that name
  if ("strata" %in% names(dt)) {
    strat_vars <- unique(c(strat_vars, "strata"))
  }
  # ============================================================================
  
  
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


