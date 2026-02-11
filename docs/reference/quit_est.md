# Estimate smoking quit probabilities

Combines the various inputs together to estimate smoking quit
probabilities according to the formula given.

## Usage

``` r
quit_est(
  trend_data,
  survivorship_data,
  mortality_data,
  relapse_data,
  initiation_data,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
)
```

## Arguments

- trend_data:

  Data table containing the statistically model trends in current,
  former and never smoking The output of
  [`trend_fit()`](https://stapm-platform.github.io/smktrans/reference/trend_fit.md).

- survivorship_data:

  Data table containing estimates of cohort survivorship by age, sex and
  IMD quintile. The output of
  [`prep_surv()`](https://stapm-platform.github.io/smktrans/reference/prep_surv.md).

- mortality_data:

  Data table containing the estimated age-specific probabilities of
  death by smoking status. The output of
  [`smoke_surv()`](https://stapm-platform.github.io/smktrans/reference/smoke_surv.md).

- relapse_data:

  Data table containing estimates of long-term relapse probabilities.
  The output of
  [`prep_relapse()`](https://stapm-platform.github.io/smktrans/reference/prep_relapse.md).

- initiation_data:

  Data table containing estimates of initiation probabilities by age,
  cohort, sex and IMD quintile. The output of
  [`init_adj()`](https://stapm-platform.github.io/smktrans/reference/init_adj.md).

- min_age:

  Integer - the youngest age considered.

- max_age:

  Integer - the oldest age considered.

- min_year:

  Integer - the earliest year of observed data.

- max_year:

  Integer - the latest year of observed data.

## Value

Returns a data.table containing the estimated quit probabilities.

## Examples

``` r
if (FALSE) { # \dontrun{

## SMOKING INITIATION

# Calculate the cumulative probabilities of starting to smoke for each cohort
init_data_raw <- smktrans::init_est(
 data = hse_data,
 strat_vars = c("sex", "imd_quintile")
)

# Estimate the trends in ever-smoking
ever_smoke_data <- smktrans::ever_smoke(
  data = hse_data,
  time_horizon = 2200,
  num_bins = 7,
  model = "England"
)

# Adjust and forecast
init_data_adj <- init_adj(
  init_data = copy(init_data_raw),
  ever_smoke_data = copy(ever_smoke_data$predicted_values),
  ref_age = 30,
  cohorts = 1971:2100,
  period_start = 2001, period_end = 2016)

# Convert the probabilities of ever-smoking to
# age-specific probabilities of smoking initiation
smk_init_data <- p_dense(data = copy(init_data_adj),
  cum_func_var = "p_ever_smoker_adj",
  strat_vars = c("cohort", "sex", "imd_quintile"))


## SMOKING QUIT

# Fit multinomial model to trends in proportion of current, former and never smokers
trend_data <- smktrans::trend_fit(hse_data,
   max_iterations = 1e3,
   age_var = "age",
   year_var = "year",
   sex_var = "sex",
   smoker_state_var = "smk.state",
   imd_var = "imd_quintile",
   weight_var = "wt_int")

# Estimate cohort survivorship stratified by age, sex and IMD quintile
survivorship_data <- smktrans::prep_surv(
  mx_data_hmd = smktrans::hmd_data,
  mx_data_ons = smktrans::tob_mort_data
)

# Estimate age-specific probabilities of death by smoking status
mortality_data <- smktrans::smoke_surv(
  data = hse_data,
  diseases  = unique(tobalcepi::tobacco_relative_risks$condition),
  mx_data = stapmr::tob_mort_data_cause
)

# Prepare probabilities of long-term relapse
relapse_data <- smktrans::prep_relapse(
  data = hse_data,
  hawkins_relapse = smktrans::hawkins_relapse,
  lowest_year = 2001,
  highest_year = 2016,
  youngest_age = 11
)

# Estimate the quit probabilities
quit_data <- quit_est(
  trend_data = copy(trend_data),
  survivorship_data = copy(survivorship_data),
  mortality_data = copy(mortality_data$data_for_quit_ests),
  relapse_data = copy(relapse_data$relapse_by_age_imd),
  initiation_data = copy(init_data_final)
)

} # }
```
