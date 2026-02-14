#' Summarise and project trends in ever-smoking
#'
#' @description
#' Fits a survey-weighted GLM (quasibinomial) to the trend in ever-smoking 
#' at age 25-34. This provides the "target" level for the Holford adjustment.
#'
#' @param data Data table of individual characteristics.
#' @param time_horizon Integer - the last year for projection.
#' @param num_bins Integer - bins for the period trend to reduce noise.
#' @param model Character - Model specification (interaction terms).
#' @param min_age Integer - youngest age for prediction.
#' @param min_year Integer - first year of survey data.
#' @param age_cats Character vector - age category for reference (e.g., "25-34").
#' @importFrom data.table := setDT setnames
#' @importFrom survey svydesign svyby svymean svyglm
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
  
  message("  - Setting up survey design...")
  
  # Copy to avoid modifying original by reference
  dt <- copy(data)
  
  # Select required variables
  cols <- c("wt_int", "psu", "cluster", "age", "year", "age_cat", "sex", "imd_quintile", "smk.state")
  dt <- dt[, ..cols]
  
  # Create binary ever smoker variable
  dt[, ever_smoker := ifelse(smk.state == "never", 0, 1)]
  dt[, cohort := year - age]
  
  # Lonely PSU adjustment (critical for survey package stability)
  options(survey.lonely.psu = "adjust")
  
  # Filter data to reference age category
  dt <- dt[age_cat %in% age_cats]
  
  # Bin the year variable to smooth out annual survey noise
  dt[, year_bin := bin_var(year, n_bins = num_bins)]
  dt[, cluster := as.factor(cluster)]
  
  # Create survey design object
  srv.int <- survey::svydesign(
    id = ~ psu,
    strata = ~ cluster,
    weights = ~ wt_int,
    nest = TRUE,
    data = dt
  )
  
  message("  - Estimating observed proportions...")
  
  # Estimate observed proportions for comparison
  current_prop <- survey::svyby(
    ~ ever_smoker,
    by = ~ year_bin + sex + imd_quintile,
    design = srv.int,
    survey::svymean
  )
  setDT(current_prop)
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
  
  m <- survey::svyglm(f, design = srv.int, family = "quasibinomial")
  
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