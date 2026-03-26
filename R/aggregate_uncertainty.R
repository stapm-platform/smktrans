#' Aggregate Bootstrap Uncertainty
#' 
#' @param boot_dt The combined data.table from all bootstrap iterations
#' @param prob_col The name of the probability column (e.g., "p_quit")
#' @import data.table
aggregate_uncertainty <- function(boot_dt, prob_col) {
  
  # 1. Strictly define the demographic keys
  # This prevents rogue probability columns from being treated as groups
  potential_keys <- c("year", "age", "sex", "imd_quintile", "time_since_quit")
  group_cols <- intersect(potential_keys, names(boot_dt))
  
  # 2. Calculate 95% CI bounds and Standard Error
  agg_dt <- boot_dt[, .(
    lower = quantile(get(prob_col), 0.025, na.rm = TRUE),
    upper = quantile(get(prob_col), 0.975, na.rm = TRUE),
    se    = sd(get(prob_col), na.rm = TRUE)
  ), by = group_cols]
  
  # 3. Rename the output columns dynamically based on the input metric
  setnames(agg_dt, 
           old = c("lower", "upper", "se"), 
           new = paste0(prob_col, c("_lower", "_upper", "_se")))
  
  return(agg_dt)
}