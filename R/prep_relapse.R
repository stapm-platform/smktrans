#' Prepare long-term relapse probabilities
#'
#' @description
#' Combines published estimates of long-term relapse (Hawkins 2010) with
#' population survey data. It maps these probabilities onto the survey population's
#' demographic structure (education, mental health, etc.) to derive
#' aggregate relapse rates by Age, Sex, IMD, and Time Since Quit.
#'
#' @param data Data table of individual survey data.
#' @param hawkins_relapse Data table of Hawkins et al. odds ratios/probs.
#' @param lowest_year Integer.
#' @param highest_year Integer.
#' @param youngest_age Integer.
#' @importFrom data.table copy := setDT rbindlist merge CJ setkeyv
#' @export
prep_relapse <- function(
    data,
    hawkins_relapse = smktrans::hawkins_relapse,
    lowest_year = 2003,
    highest_year = 2018,
    youngest_age = 18
) {
  
  # 1. Setup and Filtering
  dt <- copy(data)
  hawkins <- copy(hawkins_relapse)
  
  # Filter for former smokers with valid covariates
  dt <- dt[!is.na(age) & age >= youngest_age & smk.state == "former" &
             !is.na(time_since_quit) & time_since_quit >= 0 &
             !is.na(degree) & !is.na(relationship_status) &
             !is.na(hse_mental) & !is.na(income5cat) &
             !is.na(employ2cat) & !is.na(imd_quintile)]
  
  # Select columns
  cols <- c("wt_int", "year", "age", "sex", "income5cat", "employ2cat",
            "imd_quintile", "degree", "relationship_status", "hse_mental", "time_since_quit")
  dt <- dt[, ..cols]
  
  # Normalize weights within year
  dt[, wt_int := wt_int / sum(wt_int), by = year]
  
  # 2. Create weighted demographic profiles
  group_cols <- c("year", "age", "sex", "imd_quintile")
  covar_cols <- c("income5cat", "employ2cat", "degree", "relationship_status", "hse_mental")
  
  # Aggregation (removing time_since_quit for the profile creation)
  imd_map <- dt[, .(mu = sum(wt_int)), by = c(group_cols, covar_cols)]
  imd_map[, p := mu / sum(mu), by = group_cols]
  imd_map[, mu := NULL]
  
  # 3. Expand for Time Since Quit (0 to 10 years)
  imd_map_expanded <- imd_map[rep(1:.N, each = 11)]
  imd_map_expanded[, time_since_quit := rep(0:10, times = nrow(imd_map))]
  
  # 4. Merge Hawkins Estimates
  merged_data <- merge(imd_map_expanded, hawkins,
                       by = c(covar_cols, "sex", "age", "imd_quintile", "time_since_quit"),
                       all.x = TRUE)
  
  # Handle missing matches
  merged_data[is.na(p), p := 0]
  
  # Safety check: if probability sums to 0, assign a small baseline
  merged_data[, p_sum := sum(p), by = c(group_cols, "time_since_quit")]
  merged_data[p_sum == 0, p := 1/160]
  
  # 5. Calculate Weighted Average Relapse Probability (by TSQ)
  relapse_tsq <- merged_data[, .(
    p_relapse = sum(p_relapse * p, na.rm = TRUE) / sum(p, na.rm = TRUE)
  ), by = c(group_cols, "time_since_quit")]
  
  # Clamp
  relapse_tsq[p_relapse < 0, p_relapse := 0]
  relapse_tsq[p_relapse > 1, p_relapse := 1]
  
  # 6. Fill Missing Combinations (Expand Grid)
  full_grid <- data.table(expand.grid(
    year = lowest_year:highest_year,
    age = youngest_age:89,
    time_since_quit = 0:10,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  
  relapse_tsq <- merge(full_grid, relapse_tsq,
                       by = c("year", "age", "time_since_quit", "sex", "imd_quintile"),
                       all.x = TRUE)
  
  # 7. Smoothing (Vectorized by Group)
  
  # A. Split into rows that need smoothing (<10) vs rows that are fixed to 0 (>=10)
  tsq_smooth <- relapse_tsq[time_since_quit < 10]
  tsq_zero   <- relapse_tsq[time_since_quit >= 10]
  
  # B. Apply smoothing to the <10 group
  tsq_smooth <- tsq_smooth[, p_smooth(.SD, "p_relapse", window_size = 5),
                           by = .(sex, imd_quintile, time_since_quit)]
  
  # C. Set 0 for the >=10 group
  tsq_zero[, p_relapse := 0.0]
  
  # D. Recombine
  relapse_tsq <- rbind(tsq_smooth, tsq_zero)
  
  # 8. Future Extrapolation
  last_yr_data <- relapse_tsq[year == highest_year]
  future_years <- (highest_year + 1):2100
  
  future_list <- lapply(future_years, function(y) {
    d <- copy(last_yr_data)
    d[, year := y]
    return(d)
  })
  
  relapse_tsq_final <- rbindlist(list(relapse_tsq, rbindlist(future_list)))
  
  # 9. Calculate Aggregated Relapse (Age/Sex/IMD only)
  
  # We merge the original individual data (dt) with Hawkins probabilities
  # and calculate the weighted mean.
  dt_agg <- merge(dt, hawkins,
                  by = c(covar_cols, "sex", "age", "imd_quintile", "time_since_quit"),
                  all.x = TRUE)
  
  relapse_agg <- dt_agg[, .(
    p_relapse = sum(p_relapse * wt_int, na.rm = TRUE) / sum(wt_int, na.rm = TRUE)
  ), by = group_cols]
  
  relapse_agg[p_relapse < 0, p_relapse := 0]
  relapse_agg[p_relapse > 1, p_relapse := 1]
  
  # Expand and Smooth Aggregated Data
  agg_grid <- unique(full_grid[, .(year, age, sex, imd_quintile)])
  relapse_agg <- merge(agg_grid, relapse_agg, by = group_cols, all.x = TRUE)
  
  relapse_agg <- relapse_agg[, p_smooth(.SD, "p_relapse", 5),
                             by = .(sex, imd_quintile)]
  
  return(list(
    relapse_by_age_imd_timesincequit = relapse_tsq_final[],
    relapse_by_age_imd = relapse_agg[]
  ))
}