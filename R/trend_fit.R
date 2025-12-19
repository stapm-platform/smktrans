

#' Statistically model trends in current, former and never smoking
#'
#' Fits a series of multinomial logistic regression models to estimate smoking 
#' states. The process uses a "response surface" approach, starting with 
#' basic age and period trends and iteratively adding complexity through 
#' higher-order polynomials, birth cohort effects (age-year interactions), 
#' and socio-demographic stratification (sex and deprivation).
#' 
#' @details 
#' The final model structure represents a fully-interacted 
#' response surface. This approach assumes that smoking prevalence is not 
#' static across time or demographics, but rather a fluid "surface" 
#' defined by:
#' 
#' \itemize{
#'   \item \strong{Flexible Age-Period-Cohort Geometry:} By combining 
#'   4th-order age polynomials with 3rd-order year polynomials and their 
#'   interactions, the model captures complex temporal patterns. These 
#'   interactions specifically account for birth cohort effects, where 
#'   generational shifts in smoking behaviour deviate from general period trends.
#'   \item \strong{Demographic "Tilts":} The inclusion of sex and IMD 
#'   (deprivation) quintiles acts as a stratification layer. Rather than 
#'   simply shifting the prevalence up or down, the model allows the 
#'   entire "shape" of the age-year curve to rotate and bend differently 
#'   for each of the 10 subgroups (2 sexes × 5 IMD quintiles).
#'   \item \strong{High-Order Interactions:} The final three-way 
#'   interactions (\code{age:year:sex:imd}) allow the model to recognize 
#'   that the gap in smoking rates between rich and poor may widen or 
#'   narrow differently for men and women as they age over historical time.
#' }
#'
#' @param data Data table containing individual-level survey data over multiple years.
#' @param max_iterations Integer - the maximum number of iterations to try when looking for the
#' multinomial model to converge to a stable model fit.
#' @param age_var Character - the name of the variable containing age in single years.
#' @param year_var Character - the name of the variable containing year in single years.
#' @param sex_var Character - the name of the variable containing sex (m/f).
#' @param smoker_state_var Character - the name of the variable containing smoking status
#' (current, former, never smoker).
#' @param imd_var Character - the name 
#' of the variable containing Index of Multiple Deprivation quintiles.
#' @param weight_var Character - the name of the variable containing the survey weights.
#' @importFrom data.table data.table := setDT
#' @return Returns a data table containing the fitted values by age, year, sex and IMD quintile.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_data <- trend_fit(data = hse_data)
#'
#' }
#'
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
  
  model_data <- data.table(
    smk.state = data[ , get(smoker_state_var)],
    age = data[ , get(age_var)],
    year = data[ , get(year_var)],
    sex = data[ , get(sex_var)],
    imd_quintile = data[ , get(imd_var)],
    wt_int = data[ , get(weight_var)]
  )
  
  # Standardise variables
  mu_age <- mean(model_data$age, na.rm = T)
  sd_age <- sqrt(var(model_data$age, na.rm = T))
  
  mu_year <- mean(model_data$year, na.rm = T)
  sd_year <- sqrt(var(model_data$year, na.rm = T))
  
  model_data[ , age.z := (age - mu_age) / (2 * sd_age)]
  model_data[ , year.z := (year - mu_year) / (2 * sd_year)]
  
  # Fit response surface
  
  # Quadratic response surface, not stratified by sex or IMD quintiles
  m1 <- nnet::multinom(smk.state ~ age.z + year.z + I(age.z ^ 2) + I(year.z ^ 2) + age.z:year.z, 
                       data = model_data, weights = wt_int, maxit = max_iterations)
  
  # Add higher order polynomials of age to capture the shape of the age effects
  m2 <- update(m1, ~ . + I(age.z ^ 3) + I(age.z ^ 4))
  
  # Add higher order polynomials of year to capture the shape of the year effects
  m3 <- update(m2, ~ .  + I(year.z ^ 3))
  
  # Add higher order interactions between age and year to capture the interactions
  # the interactions between age and year are a way of describing birth cohort effects
  m4 <- update(m3, ~ .  + I(age.z ^ 2):year.z + age.z:I(year.z ^ 2))
  
  # Add basic stratification by sex and IMD quintiles (10 subgroups)
  m5 <- update(m4, ~ . + sex + imd_quintile + sex:imd_quintile)
  
  # Add the interaction between the sex and IMD quintiles stratification and the linear form of age
  m6 <- update(m5, ~ . + age.z:sex + age.z:imd_quintile + age.z:sex:imd_quintile)
  
  # Add the interaction between the sex and IMD quintiles stratification and the linear form of year
  m7 <- update(m6, ~ . + year.z:sex + year.z:imd_quintile + year.z:sex:imd_quintile)
  
  # Add interaction between the sex and IMD quintiles and 
  # the interaction between the linear forms of age and year (higher order interactions)
  m8 <- update(m7, ~ . + age.z:year.z:sex + age.z:year.z:imd_quintile + age.z:year.z:sex:imd_quintile)
  
  # Grab predicted values from the model
  newdata <- data.frame(expand.grid(
    age = min(model_data$age):max(model_data$age),
    year = min(model_data$year):max(model_data$year),
    sex = c("Male", "Female"),
    imd_quintile = unique(model_data$imd_quintile)))
  
  setDT(newdata)
  
  newdata[ , cohort := year - age]
  
  newdata[ , age.z := (age - mu_age) / (2 * sd_age)]
  newdata[ , year.z := (year - mu_year) / (2 * sd_year)]
  
  newdata1 <- newdata[ , c("age.z", "year.z", "sex", "imd_quintile")]
  
  newdata <- cbind(newdata, stats::predict(m8, newdata = newdata1, "probs"))
  
  newdata[ , age.z := NULL]
  newdata[ , year.z := NULL]
  
  return(newdata[])
}




