
#' Estimate smoking quit probabilities
#'
#' Combines the various inputs together to estimate smoking quit
#' probabilities according to the formula given.
#'
#' @param trend_data Data table containing the statistically model trends in
#' current, former and never smoking
#' The output of \code{trend_fit()}.
#' @param survivorship_data Data table containing estimates of cohort survivorship by
#' age, sex and IMD quintile. The output of \code{prep_surv()}.
#' @param mortality_data Data table containing the estimated age-specific
#' probabilities of death by smoking status.
#' The output of \code{smoke_surv()}.
#' @param relapse_data Data table containing estimates of long-term relapse probabilities.
#' The output of \code{prep_relapse()}.
#' @param initiation_data Data table containing estimates of initiation probabilities
#' by age, cohort, sex and IMD quintile.
#' The output of \code{init_adj()}.
#' @template age-year
#'
#' @importFrom data.table setDT := setnames shift
#'
#' @return Returns a data.table containing the estimated quit probabilities.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## SMOKING INITIATION
#'
#' # Calculate the cumulative probabilities of starting to smoke for each cohort
#' init_data_raw <- smktrans::init_est(
#'  data = hse_data,
#'  strat_vars = c("sex", "imd_quintile")
#' )
#'
#' # Estimate the trends in ever-smoking
#' ever_smoke_data <- smktrans::ever_smoke(
#'   data = hse_data,
#'   time_horizon = 2200,
#'   num_bins = 7,
#'   model = "England"
#' )
#'
#' # Adjust and forecast
#' init_data_adj <- init_adj(
#'   init_data = copy(init_data_raw),
#'   ever_smoke_data = copy(ever_smoke_data$predicted_values),
#'   ref_age = 30,
#'   cohorts = 1971:2100,
#'   period_start = 2001, period_end = 2016)
#'
#' # Convert the probabilities of ever-smoking to
#' # age-specific probabilities of smoking initiation
#' smk_init_data <- p_dense(data = copy(init_data_adj),
#'   cum_func_var = "p_ever_smoker_adj",
#'   strat_vars = c("cohort", "sex", "imd_quintile"))
#'
#'
#' ## SMOKING QUIT
#'
#' # Fit multinomial model to trends in proportion of current, former and never smokers
#' trend_data <- smktrans::trend_fit(hse_data,
#'    max_iterations = 1e3,
#'    age_var = "age",
#'    year_var = "year",
#'    sex_var = "sex",
#'    smoker_state_var = "smk.state",
#'    imd_var = "imd_quintile",
#'    weight_var = "wt_int")
#'
#' # Estimate cohort survivorship stratified by age, sex and IMD quintile
#' survivorship_data <- smktrans::prep_surv(
#'   mx_data_hmd = smktrans::hmd_data,
#'   mx_data_ons = smktrans::tob_mort_data
#' )
#'
#' # Estimate age-specific probabilities of death by smoking status
#' mortality_data <- smktrans::smoke_surv(
#'   data = hse_data,
#'   diseases  = unique(tobalcepi::tobacco_relative_risks$condition),
#'   mx_data = stapmr::tob_mort_data_cause
#' )
#'
#' # Prepare probabilities of long-term relapse
#' relapse_data <- smktrans::prep_relapse(
#'   data = hse_data,
#'   hawkins_relapse = smktrans::hawkins_relapse,
#'   lowest_year = 2001,
#'   highest_year = 2016,
#'   youngest_age = 11
#' )
#'
#' # Estimate the quit probabilities
#' quit_data <- quit_est(
#'   trend_data = copy(trend_data),
#'   survivorship_data = copy(survivorship_data),
#'   mortality_data = copy(mortality_data$data_for_quit_ests),
#'   relapse_data = copy(relapse_data$relapse_by_age_imd),
#'   initiation_data = copy(init_data_final)
#' )
#'
#' }
#'
quit_est <- function(
  trend_data,
  survivorship_data,
  mortality_data,
  relapse_data,
  initiation_data,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
) {

  #########################################
  # Pull the data together

  master_data <- data.frame(expand.grid(
    sex = c("Male", "Female"),
    age = min_age:max_age,
    year = min_year:max_year,
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  setDT(master_data)

  # Record number of rows for checking
  kn <- nrow(master_data)

  ######################
  # Add survivorship
  master_data <- merge(
    master_data,
    survivorship_data,
    by = c("sex", "age", "year", "imd_quintile"),
    all.x = T, all.y = F, sort = F)

  testthat::expect_equal(kn, nrow(master_data[!is.na(lx) & lx >= 0 & lx <= 1]))

  ######################
  # Add proportion of people at each smoker state
  master_data <- merge(
    master_data,
    trend_data, #[ , cohort := NULL],
    by = c("sex", "age", "year", "imd_quintile"),
    all.x = T, all.y = F, sort = F)

  testthat::expect_equal(kn, nrow(master_data[
    current >= 0 & current <= 1 &
    former >= 0 & former <= 1 &
    never >= 0 & never <= 1]))

  ######################
  # Add probability of survival by smoking status
  master_data <- merge(
    master_data,
    mortality_data,
    by = c("sex", "age", "year", "imd_quintile"),
    all.x = T, all.y = F, sort = F)

  testthat::expect_equal(kn, nrow(master_data[
    current_px >= 0 & current_px <= 1 &
    former_px >= 0 & former_px <= 1 &
    never_px >= 0 & never_px <= 1]))

  ######################
  # Add probability of relapse for former smokers
  master_data <- merge(
    master_data,
    relapse_data,
    by = c("age", "sex", "year", "imd_quintile"),
    all.x = T, all.y = F, sort = F)

  testthat::expect_equal(kn, nrow(master_data[p_relapse >= 0 & p_relapse <= 1]))

  ######################
  # Add probability of initiating smoking

  master_data <- merge(
    master_data,
    initiation_data[ , c("age", "year", "sex", "imd_quintile", "p_start")],
    by = c("sex", "age", "year", "imd_quintile"),
    all.x = T, all.y = F, sort = F)

  # Assume no new initiation after age 30
  master_data[age > max(initiation_data$age), p_start := 0]

  testthat::expect_equal(kn, nrow(master_data[!is.na(p_start)]))


  #########################################
  # Make the calculation

  master_data[ , cohort := year - age]

  master_data[ , lx1 := shift(lx, type = "lead"), by = c("cohort", "sex", "imd_quintile")]
  master_data[ , current1 := shift(current, type = "lead"), by = c("cohort", "sex", "imd_quintile")]

  master_data[ , c1 := (lx1 * current1) / (lx * current * current_px)]
  master_data[ , c2 := (1 / (current * current_px)) * (former * former_px * p_relapse + never * never_px * p_start)]
  master_data[ , c2_no_init := (1 / (current * current_px)) * (former * former_px * p_relapse)]

  master_data[ , quit_prob := 1 - c1 + c2]
  master_data[ , quit_prob_no_init := 1 - c1 + c2_no_init]

  master_data[age == max_age, quit_prob := 0]
  master_data[age == max_age, quit_prob_no_init := 0]

  master_data[quit_prob < 0 | is.na(quit_prob), quit_prob := 0]
  master_data[quit_prob > 1 | is.na(quit_prob), quit_prob := 1]

  master_data[quit_prob_no_init < 0 | is.na(quit_prob_no_init), quit_prob_no_init := 0]
  master_data[quit_prob_no_init > 1 | is.na(quit_prob_no_init), quit_prob_no_init := 1]
  
  
  # no estimated quit probs for the last year of data
  master_data <- master_data[year < max_year]

  # Keep only required variables
  master_data <- master_data[age < max_age, c("sex", "age", "year", "imd_quintile", "quit_prob", "quit_prob_no_init")]

  setnames(master_data, c("quit_prob", "quit_prob_no_init"), c("p_quit", "p_quit_no_init"))


return(master_data[])
}



