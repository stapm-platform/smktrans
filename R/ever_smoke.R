#' Summarise and project trends in ever-smoking
#'
#' @description
#' Fits a weighted GLM (quasibinomial) to the trend in ever-smoking 
#' at age 25-34. This provides the "target" level for the Holford adjustment.
#'
#' @param data Data table of individual characteristics.
#' @param time_horizon Integer - the last year for projection.
#' @param num_bins Integer - bins for the period trend to reduce noise.
#' @param model Character - Model specification (interaction terms).
#' @param min_age Integer - youngest age for prediction.
#' @param min_year Integer - first year of survey data.
#' @param age_cats Character vector - age category for reference (e.g., "25-34").
#' @importFrom data.table := setDT setnames copy
#' @importFrom stats glm predict quasibinomial weighted.mean
#' @export
ever_smoke <- function(
    data,
    time_horizon = 2100,
    num_bins = 7,
    model = "model2", # Default to Model 2 (Sex interaction) as per England standard
    min_age = 15,
    min_year = 2003,
    age_cats = c("25-34")
) {
  
  # Copy to avoid modifying original by reference
  dt <- copy(data)
  
  # Select required variables
  cols <- c("wt_int", "age", "year", "age_cat", "sex", "imd_quintile", "smk.state")
  dt <- dt[, ..cols]
  
  # Create binary ever smoker variable
  dt[, ever_smoker := ifelse(smk.state == "never", 0, 1)]
  dt[, cohort := year - age]
  
  # Filter data to reference age category
  dt <- dt[age_cat %in% age_cats]
  
  # Bin the year variable to smooth out annual survey noise
  # (Assuming bin_var is a helper function defined elsewhere in your package)
  dt[, year_bin := bin_var(year, n_bins = num_bins)]
  
  message("  - Estimating observed proportions...")
  
  # FAST BYPASS: Use data.table for weighted means instead of survey::svyby
  current_prop <- dt[, .(
    ever_smoker = stats::weighted.mean(ever_smoker, w = wt_int, na.rm = TRUE)
  ), by = .(year_bin, sex, imd_quintile)]
  
  setnames(current_prop, "year_bin", "year")
  
  message(paste("  - Fitting trend model:", model))
  
  # Model Selection
  f <- switch(model,
              "model1" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + imd_quintile:year_bin + sex:imd_quintile,
              "model2" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + imd_quintile:year_bin,
              "model3" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + sex:imd_quintile,
              "model4" = ever_smoker ~ sex + imd_quintile + year_bin + imd_quintile:year_bin + sex:imd_quintile,
              "model5" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin,
              "model6" = ever_smoker ~ sex + imd_quintile + year_bin + sex:imd_quintile,
              "model7" = ever_smoker ~ sex + imd_quintile + year_bin + imd_quintile:year_bin,
              "model8" = ever_smoker ~ sex + imd_quintile + year_bin,
  )
  
  if(is.null(f)) stop("Invalid model selection")
  
  # FAST BYPASS: Use standard glm() instead of survey::svyglm()
  m <- stats::glm(
    f, 
    data = dt, 
    family = stats::quasibinomial(link = "logit"), 
    weights = wt_int
  )
  
  # Generate predictions
  newdata <- data.frame(expand.grid(
    year_bin = (min_year - min_age):time_horizon,
    sex = c("Male", "Female"), 
    imd_quintile = unique(dt$imd_quintile)
  ))
  
  newdata$fitted_trends <- as.numeric(stats::predict(m, type = "response", newdata = newdata))
  
  setDT(newdata)
  setnames(newdata, "year_bin", "year")
  
  return(list(
    data_points = current_prop[],
    predicted_values = newdata[]
  ))
}