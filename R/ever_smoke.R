#' Summarise and project trends in ever-smoking
#'
#' Calculates the expected proportion of ever-smokers, and
#' fits a linear trend over time to the proportion of people who have ever smoked in the
#' age category 25-34 years.
#'
#' Uses the 'survey' package to fit a model to the trends in
#' ever-smoking that accounts for survey sampling
#' error and survey design.
#'
#' The linear model is fitted to a binary response variable, 1 = ever smoker,
#' 0 = never smoker and estimates
#' variation in the trend
#' by sex and quintiles of the Index of Multiple Deprivation.
#' The fitted model is then used to predict /
#' extrapolate the trend up to the year specified by 'time horizon'.
#'
#' @param data Data table of individual characteristics.
#' @param time_horizon Integer - the last year for which to generate
#' predictions for ever-smoking at the index age.
#' @param num_bins Integer - the number of bins to create from the continuous period (year) variable. 
#' Wider bins = fewer data points from which to estimate the period trend, but less random uncertainty on each data point.
#' @param model Character - c("model1", "model2", "model3", "model4"). 
#' Model 1 - two-way interactions of the year trend with both sex and IMD quintile. 
#' Model 2 - two-way interaction of the year trend with sex only.
#' Model 3 - two-way interaction of the year trend with IMD quintile only.
#' Model 4 - no two-way interactions of the year trend with either sex or IMD quintile. 
#' @param min_age Integer - the youngest age for which a prediction of the 
#' progression of ever-smoking in a cohort should be generated. Defaults to 15 years.
#' @param min_year Integer - the first year of survey data. For England 2003 
#' and for Scotland 2008.
#' @param age_cats Character vector - the names of the age categories for which the ever-smoking 
#' reference proportion is calculated.
#' 
#' @importFrom data.table := setDT setnames
#' @importFrom survey svydesign svyby svymean svyglm
#' 
#' @return Returns two data tables:
#' \itemize{
#' \item "data_points", the proportions of ever-smokers at age 25 observed in the survey data;
#' \item "predicted_values", the model predictions up to the time horizon.
#' }
#' 
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_data_trend <- ever_smoke(data = hse_data)
#'
#' }
#'
ever_smoke <- function(
    data,
    time_horizon = 2100,
    num_bins = 7,
    model = c("model1", "model2", "model3", "model4", "model5")[1],
    min_age = 15,
    min_year = c(2003, 2008)[1],
    age_cats = c("25-34")
) {
  
  cat("setting up data...\r")
  utils::flush.console()
  
  # Select required variables
  data <- data[ , list(wt_int, psu, cluster, age, year, age_cat, sex, imd_quintile, smk.state)]
  
  # Create an ever smoker variable
  data[ , ever_smoker := ifelse(smk.state == "never", 0, 1)]
  
  data[ , cohort := year - age]
  
  # Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
  options(survey.lonely.psu = "adjust")
  
  # Fit and extrapolate time trend in ever smoking
  
  # Filter data
  
  data <- data[age_cat %in% age_cats]
  
  data[ , year_bin := smktrans::bin_var(year, n_bins = num_bins)]
  
  data[ , cluster := as.factor(cluster)]
  
  cat("making survey object...\r")
  utils::flush.console()
  
  # Convert data to a survey object
  srv.int <- svydesign(
    id =  ~ psu,
    strata =  ~ cluster,
    weights = ~ wt_int,
    nest = TRUE,
    data = data)
  
  cat("estimating prop. ever-smokers...\r")
  utils::flush.console()
  
  # Estimate the proportions of ever smokers
  # by year, sex, age category, and IMD quintile
  current_prop <- svyby( ~ ever_smoker,
                         by =  ~ year_bin + sex + imd_quintile,
                         design = srv.int,
                         svymean)
  
  setDT(current_prop)
  
  setnames(current_prop, "year_bin", "year")
  
  cat("fitting trend in ever-smoking\r")
  utils::flush.console()
  
  # Model 1
  if(model == "model1"){
    m <- svyglm(ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + imd_quintile:year_bin + sex:imd_quintile, design = srv.int, family = "quasibinomial")
  }
  
  # Model 2
  if(model == "model2"){
    m <- svyglm(ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + sex:imd_quintile, design = srv.int, family = "quasibinomial")
  }
  
  # Model 3
  if(model == "model3"){
    m <- svyglm(ever_smoker ~ sex + imd_quintile + year_bin + imd_quintile:year_bin + sex:imd_quintile, design = srv.int, family = "quasibinomial")
  }
  
  # Model 4
  if(model == "model4"){
    m <- svyglm(ever_smoker ~ sex + imd_quintile + year_bin + sex:imd_quintile, design = srv.int, family = "quasibinomial")
  }
  
  # Model 5
  if(model == "model5"){
    m <- svyglm(ever_smoker ~ imd_quintile + sex + year_bin + year_bin:sex + sex:imd_quintile, design = srv.int, family = "quasibinomial")
  }
  
  # Grab the model predictions
  newdata <- data.frame(expand.grid(year_bin = (min_year - min_age):time_horizon,
                                    sex = c("Male", "Female"), imd_quintile = unique(data$imd_quintile)))
  
  newdata$fitted_trends <- as.numeric(stats::predict(m, type = "response", newdata = newdata))
  
  setDT(newdata)
  
  setnames(newdata, "year_bin", "year")
  
  return(list(
    data_points = current_prop[],
    predicted_values = newdata[]
  ))
}