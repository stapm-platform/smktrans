#' Aggregate Bootstrap Uncertainty (median central estimate, CI bounds, SE)
#'
#' @param boot_dt The combined data.table from all bootstrap iterations
#' @param prob_col The name of the probability column (e.g. "p_quit")
#' @param extra_keys Additional grouping columns beyond the standard demographic
#'   keys. The smoking trends are grouped by smk.state as well, because each
#'   age/year/sex/IMD cell carries three probabilities rather than one.
#' @param min_boot The number of iterations each group must have. Groups with
#'   fewer are an error, not something to average over quietly.
#' @import data.table
aggregate_uncertainty <- function(boot_dt, prob_col, extra_keys = character(0), min_boot = NULL) {

  if (!prob_col %in% names(boot_dt)) {
    stop("aggregate_uncertainty: no column called '", prob_col, "'.")
  }

  # 1. Strictly define the demographic keys
  potential_keys <- c("year", "age", "sex", "imd_quintile", "time_since_quit")
  group_cols <- c(intersect(potential_keys, names(boot_dt)), extra_keys)

  missing_extra <- setdiff(extra_keys, names(boot_dt))
  if (length(missing_extra) > 0) {
    stop("aggregate_uncertainty: extra_keys not in the data: ", paste(missing_extra, collapse = ", "))
  }

  # 2. Every cell should have been estimated in every iteration. If some have
  # not, the quantiles below are computed off a different number of draws per
  # cell and the CIs are not comparable. Say so rather than let it through.
  if ("boot_id" %in% names(boot_dt)) {
    if (is.null(min_boot)) min_boot <- uniqueN(boot_dt$boot_id)
    counts <- boot_dt[!is.na(get(prob_col)), .N, by = group_cols]
    short <- counts[N < min_boot]
    if (nrow(short) > 0) {
      stop("aggregate_uncertainty: ", nrow(short), " of ", nrow(counts),
           " groups have fewer than ", min_boot, " non-missing draws of '", prob_col,
           "'. Worst has ", min(short$N), ".")
    }
  }

  # 3. Median (central estimate), 95% CI bounds, and SE
  agg_dt <- boot_dt[, .(
    median = quantile(get(prob_col), 0.5, na.rm = TRUE),
    lower  = quantile(get(prob_col), 0.025, na.rm = TRUE),
    upper  = quantile(get(prob_col), 0.975, na.rm = TRUE),
    se     = sd(get(prob_col), na.rm = TRUE)
  ), by = group_cols]

  # 4. Rename. 'median' becomes the primary prob_col (e.g. 'p_quit')
  setnames(agg_dt,
           old = c("median", "lower", "upper", "se"),
           new = c(prob_col, paste0(prob_col, c("_lower", "_upper", "_se"))))

  return(agg_dt)
}
