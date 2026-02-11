#' Statistically model trends in smoking status
#'
#' @description
#' Fits a multinomial logistic regression "response surface" to estimate 
#' Current/Former/Never status. The model includes high-order polynomials 
#' for Age and Year, and interactions with Sex and IMD.
#'
#' @param data Data table of survey data.
#' @param max_iterations Integer.
#' @param age_var,year_var,sex_var,smoker_state_var,imd_var,weight_var Column names.
#' @importFrom data.table setDT := copy
#' @importFrom nnet multinom
#' @importFrom stats predict
#' @export
trend_fit <- function(
    data,
    max_iterations = 1e3,
    age_var = "age",
    year_var = "year",
    sex_var = "sex",
    smoker_state_var = "smk.state",
    imd_var = "imd_quintile",
    weight_var = "wt_int"
) {
  
  # 1. Prepare Data
  # Extract relevant columns and standardize names
  model_data <- data.table(
    smk.state = data[[smoker_state_var]],
    age = data[[age_var]],
    year = data[[year_var]],
    sex = data[[sex_var]],
    imd_quintile = data[[imd_var]],
    wt_int = data[[weight_var]]
  )
  
  # Z-score standardization for numerical stability in GLMs
  mu_age <- mean(model_data$age, na.rm = TRUE)
  sd_age <- sqrt(var(model_data$age, na.rm = TRUE))
  mu_year <- mean(model_data$year, na.rm = TRUE)
  sd_year <- sqrt(var(model_data$year, na.rm = TRUE))
  
  model_data[, age.z := (age - mu_age) / (2 * sd_age)]
  model_data[, year.z := (year - mu_year) / (2 * sd_year)]
  
  # 2. Fit Model
  # Structure: 4th order Age * 3rd order Year * Sex * IMD
  # This captures complex cohort effects and demographic shifts.
  
  message("  - Fitting multinomial response surface...")
  
  # Explicit formula equivalent to the cumulative update() approach
  f_final <- smk.state ~ 
    (poly(age.z, 4) * poly(year.z, 3) * sex * imd_quintile) 
  
  # Note: The original code manually constructed interactions (e.g., I(age^2):year).
  # Below is the exact reconstruction of 'm8' from the original script logic
  # if one were to write it out fully, to ensure identical results to the legacy code:
  
  f_legacy <- smk.state ~ 
    age.z + year.z + I(age.z^2) + I(year.z^2) + age.z:year.z + 
    I(age.z^3) + I(age.z^4) +                                  
    I(year.z^3) +                                              
    I(age.z^2):year.z + age.z:I(year.z^2) +                    
    sex + imd_quintile + sex:imd_quintile +                    
    age.z:sex + age.z:imd_quintile + age.z:sex:imd_quintile +  
    year.z:sex + year.z:imd_quintile + year.z:sex:imd_quintile + 
    age.z:year.z:sex + age.z:year.z:imd_quintile + age.z:year.z:sex:imd_quintile 
  
  m_final <- nnet::multinom(f_legacy, data = model_data, weights = wt_int, 
                            maxit = max_iterations, trace = FALSE)
  
  # 3. Predict on Grid
  message("  - Generating predictions...")
  
  newdata <- data.table(expand.grid(
    age = min(model_data$age):max(model_data$age),
    year = min(model_data$year):max(model_data$year),
    sex = c("Male", "Female"),
    imd_quintile = unique(model_data$imd_quintile)
  ))
  
  # Apply same Z-score transformation
  newdata[, age.z := (age - mu_age) / (2 * sd_age)]
  newdata[, year.z := (year - mu_year) / (2 * sd_year)]
  newdata[, cohort := year - age]
  
  # Predict
  probs <- stats::predict(m_final, newdata = newdata, type = "probs")
  
  # Combine
  newdata <- cbind(newdata, probs)
  
  # Cleanup z-scores
  newdata[, `:=`(age.z = NULL, year.z = NULL)]
  
  return(newdata[])
}