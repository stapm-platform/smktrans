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
      max_psus_per_cluster <- max(dt[, .(n_psu = uniqueN(psu)), by = c(strat_vars, "cluster")]$n_psu)
      if (max_psus_per_cluster > 1) {
        strat_vars <- c(strat_vars, "cluster")
      }
    } else {
      strat_vars <- c(strat_vars, "cluster")
    }
  }
  
  if ("strata" %in% names(dt)) {
    strat_vars <- unique(c(strat_vars, "strata"))
  }
  
  # ============================================================================
  # VIABILITY CHECK: Is 'psu' actually resamplable?
  # ----------------------------------------------------------------------------
  # Resampling PSUs only introduces variation if at least one stratum contains
  # more than one PSU. If every stratum has a single PSU (as can happen when a
  # survey's imputed file carries a degenerate/placeholder psu column, e.g.
  # Wales), then `sample(psu, size = 1, replace = TRUE)` returns that same PSU
  # every iteration, the bootstrap becomes a no-op, and all downstream
  # uncertainty collapses to zero width (lower == upper == point, se == 0).
  # In that case, drop to individual-level resampling so the bootstrap still
  # captures sampling variation.
  # ============================================================================
  psu_is_viable <- FALSE
  if (has_psu) {
    if (length(strat_vars) > 0) {
      max_psu_per_stratum <- max(dt[, .(n_psu = uniqueN(psu)), by = strat_vars]$n_psu)
    } else {
      max_psu_per_stratum <- dt[, uniqueN(psu)]
    }
    psu_is_viable <- max_psu_per_stratum > 1
    if (!psu_is_viable) {
      warning("PSU resampling is degenerate (<= 1 PSU per stratum); ",
              "falling back to individual-level resampling.")
    }
  }
  
  
  if (has_psu && psu_is_viable) {
    # COMPLEX DESIGN: Resample PSUs within Strata
    psu_list <- unique(dt[, c(strat_vars, "psu"), with = FALSE])
    
    if (length(strat_vars) > 0) {
      sampled_psus <- psu_list[, .(psu = sample(psu, size = .N, replace = TRUE)), by = strat_vars]
    } else {
      sampled_psus <- psu_list[, .(psu = sample(psu, size = .N, replace = TRUE))]
    }
    
    sampled_psus[, boot_psu_id := .I]
    resampled_data <- merge(sampled_psus, dt, by = c(strat_vars, "psu"), allow.cartesian = TRUE)
    
  } else {
    # SIMPLE DESIGN (or non-viable PSU design): Resample individuals within strata
    if (length(strat_vars) > 0) {
      resampled_data <- dt[, .SD[sample(.N, .N, replace = TRUE)], by = strat_vars]
    } else {
      resampled_data <- dt[sample(.N, .N, replace = TRUE)]
    }
  }
  
  return(resampled_data)
}
