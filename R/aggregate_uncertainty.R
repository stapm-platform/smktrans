#' Aggregate Bootstrap Uncertainty (Now includes Median Central Estimate)
#' 
#' @param boot_dt The combined data.table from all bootstrap iterations
#' @param prob_col The name of the probability column (e.g., "p_quit")
#' @import data.table
aggregate_uncertainty <- function(boot_dt, prob_col) {
  
  # 1. Strictly define the demographic keys
  potential_keys <- c("year", "age", "sex", "imd_quintile", "time_since_quit")
  group_cols <- intersect(potential_keys, names(boot_dt))
  
  # 2. Calculate Median (New Central Estimate), 95% CI bounds, and SE
  agg_dt <- boot_dt[, .(
    median = quantile(get(prob_col), 0.5, na.rm = TRUE),   # NEW: 50th percentile
    lower  = quantile(get(prob_col), 0.025, na.rm = TRUE),
    upper  = quantile(get(prob_col), 0.975, na.rm = TRUE),
    se     = sd(get(prob_col), na.rm = TRUE)
  ), by = group_cols]
  
  # 3. Rename columns. 'median' becomes the primary prob_col (e.g., 'p_quit')
  setnames(agg_dt, 
           old = c("median", "lower", "upper", "se"), 
           new = c(prob_col, paste0(prob_col, c("_lower", "_upper", "_se"))))
  
  return(agg_dt)
}
