#' Aggregate Bootstrap Uncertainty
#' 
#' @param boot_dt The combined data.table from all bootstrap iterations
#' @param prob_col The name of the probability column (e.g., "p_quit")
#' @import data.table
aggregate_uncertainty <- function(boot_dt, prob_col) {
  
  # Identify grouping columns (everything except the value and the boot_id)
  group_cols <- setdiff(names(boot_dt), c(prob_col, "boot_id"))
  
  # Calculate 95% CI bounds and Standard Error
  agg_dt <- boot_dt[, .(
    lower = quantile(get(prob_col), 0.025, na.rm = TRUE),
    upper = quantile(get(prob_col), 0.975, na.rm = TRUE),
    se    = sd(get(prob_col), na.rm = TRUE)
  ), by = group_cols]
  
  # Rename the output columns dynamically based on the input metric
  setnames(agg_dt, 
           old = c("lower", "upper", "se"), 
           new = paste0(prob_col, c("_lower", "_upper", "_se")))
  
  return(agg_dt)
}

