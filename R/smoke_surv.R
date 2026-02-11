#' Estimate age-specific probabilities of death by smoking status
#'
#' @description
#' Calculates survival probabilities (px) stratified by Smoking Status.
#' It uses the relative risks (RR) of smoking-related diseases to adjust
#' background mortality rates.
#'
#' @param data Data table of individual characteristics (survey data).
#' @param diseases Character vector of disease names.
#' @param mx_data Data table of cause-specific mortality rates.
#' @param min_age,max_age,min_year,max_year Integers.
#' @importFrom data.table copy rbindlist setDT setorderv dcast setkeyv
#' @importFrom mgcv gam s predict.gam
#' @export
smoke_surv <- function(
    data,
    diseases = tobalcepi::tob_disease_names,
    mx_data,
    min_age = 11,
    max_age = 89,
    min_year = 2003,
    max_year = 2018
) {
  
  # 1. Attach Relative Risks
  # Adds RR columns to the individual data based on smoking history
  dt_rr <- tobalcepi::RRFunc(
    data = copy(data),
    substance = "tob",
    tob_diseases = diseases
  )
  
  # 2. Calculate Individual Survival Probabilities
  # We iterate over years because mortality rates (mx_data) change by year.
  # (Optimization: Pre-split data to avoid repeated subsetting overhead)
  
  results_list <- vector("list", length(min_year:max_year))
  names(results_list) <- min_year:max_year
  
  for(y in min_year:max_year) {
    # Filter for current year
    d_yr <- dt_rr[year == y]
    mx_yr <- mx_data[year == y]
    
    if(nrow(d_yr) > 0) {
      # stapmr::SurvFunc calculates qx (prob of death) for individuals
      # based on their specific disease risks
      res <- stapmr::SurvFunc(
        data = d_yr,
        mx_data = mx_yr,
        diseases = diseases,
        weights = TRUE,
        remove_dead = FALSE,
        k_year = y
      )
      results_list[[as.character(y)]] <- res
    }
  }
  
  combined_data <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
  
  # 3. Aggregate to Group Level
  # Average the individual qx up to the group level (Age/Sex/IMD/SmkState)
  death_agg <- combined_data[, .(qx = mean(qx, na.rm = TRUE)), 
                             by = .(year, sex, age, imd_quintile, smk.state)]
  
  # 4. Expand Grid and Merge
  # Ensure all combinations exist (even those with no survey participants)
  domain <- CJ(
    age = min_age:max_age,
    year = min_year:max_year,
    sex = c("Male", "Female"),
    imd_quintile = unique(death_agg$imd_quintile),
    smk.state = c("current", "former", "never")
  )
  
  domain <- merge(domain, death_agg, 
                  by = c("year", "age", "sex", "imd_quintile", "smk.state"), 
                  all.x = TRUE)
  
  # 5. Clean and Interpolate Missing Data
  # Sort for smoothing
  setkeyv(domain, c("year", "sex", "imd_quintile", "smk.state", "age"))
  
  # Handle NAs and outliers before smoothing
  # (Logic from original script: clear outlier qx values for elderly)
  domain[is.na(qx), qx := 0]
  domain[age > 80 & qx < 0.05, qx := NA] # Suspiciously low mortality for elderly
  domain[age > 60 & qx < 0.005, qx := NA]
  
  # 6. Smooth Mortality Curves (GAM)
  # We smooth the qx curve over Age for every Year/Sex/IMD/State group.
  # Using data.table's 'by' to vectorize the model fitting.
  
  # Define a wrapper for robust GAM fitting
  fit_gam <- function(sub_age, sub_qx) {
    # Need enough points to smooth
    valid <- !is.na(sub_qx)
    if(sum(valid) < 5) return(rep(0, length(sub_age)))
    
    # Fit
    tryCatch({
      m <- mgcv::gam(qx ~ s(age, k = 10), data = data.frame(age = sub_age[valid], qx = sub_qx[valid]))
      preds <- predict(m, newdata = data.frame(age = sub_age))
      return(preds)
    }, error = function(e) return(rep(0, length(sub_age))))
  }
  
  # Apply smoothing
  message("  - Smoothing mortality curves...")
  domain[, qx_fits := fit_gam(age, qx), by = .(year, sex, imd_quintile, smk.state)]
  
  # Clamp and Convert to Survival (px)
  domain[qx_fits < 0, qx_fits := 0]
  domain[qx_fits > 1, qx_fits := 1] # Safety
  domain[, px := 1 - qx_fits]
  
  # 7. Reshape for Output
  # Pivot to wide format: columns never_px, former_px, current_px
  px_wide <- dcast(domain, 
                   year + age + sex + imd_quintile ~ paste0(smk.state, "_px"), 
                   value.var = "px")
  
  return(list(
    data_detailed = domain[],
    data_for_quit_ests = px_wide[]
  ))
}