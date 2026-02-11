# Forecast relapse probabilities

Produces future projections of the annual probabilities of relapse by
former smokers, stratified by age, sex, IMD quintile and the number of
years since quitting (up to 10 years).

## Usage

``` r
relapse_forecast(
  relapse_forecast_data,
  relapse_by_age_imd_timesincequit,
  jump_off_year = 2018
)
```

## Arguments

- relapse_forecast_data:

  Data table - the output from
  [`quit_forecast()`](https://stapm-platform.github.io/smktrans/reference/quit_forecast.md).

- relapse_by_age_imd_timesincequit:

  Data table - the output from
  [`prep_relapse()`](https://stapm-platform.github.io/smktrans/reference/prep_relapse.md)
  that has the relapse probabilities stratified by age, sex, IMD
  quintile and time since quitting.

- jump_off_year:

  integer - the last year of observed data.

## Value

Returns a data.table of past and future relapse probabilities stratified
by age, sex, IMD quintile and time since quitting.

## Details

It is difficult to reliably forecast values that are stratified by age,
sex, IMD quintile and time since quitting due to the high dimensionality
of these data. We therefore forecast the values stratified by age, sex
and IMD quintile, and from these forecasts calculate scaling values that
are then applied to the relapse probabilities estimated from the last
year of observed data to produce forecasts stratified by age, sex, IMD
quintile and time since quitting.

## Examples

``` r
if (FALSE) { # \dontrun{

# Combine published estimates of long-term relapse with 
# the Health Survey for England data to arrive at the expected values 
# for relapse probabilities within defined subgroups.
relapse_data <- smktrans::prep_relapse(
  data = hse_data,
  hawkins_relapse = smktrans::hawkins_relapse,
  lowest_year = 2001,
  highest_year = 2018,
  youngest_age = 11
)

# Forecasting relapse probabilities is tricky because 
# the probabilities that are used in the model are stratified by age, sex, IMDq and time since quitting
# The approach will be to forecast the version of the probabilities stratified by age, sex and IMDq only
# and then use the results to scale the higher dimensional version

relapse_forecast_data <- quit_forecast(
  data = copy(relapse_data$relapse_by_age_imd),
  forecast_var = "p_relapse",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = 2030, # the year at which the forecast becomes stationary
  first_year = 2001, # the earliest year of data on which the forecast is based
  jump_off_year = 2018,
  time_horizon = 2100
)

# plot to check
relapse_forecast_plot <- ggplot(relapse_forecast_data[age == 50 & year <= 2030]) +
  geom_line(aes(x = year, y = p_relapse, colour = imd_quintile)) +
  facet_wrap(~ sex) +
  theme_minimal() +
  ylab("P(relapse)") + 
  #ylim(0, 1) +
  geom_vline(xintercept = 2018, linetype = 2)

relapse_forecast_plot

test_data <- relapse_forecast(
  relapse_forecast_data = relapse_forecast_data,
  relapse_by_age_imd_timesincequit = relapse_data$relapse_by_age_imd_timesincequit,
  jump_off_year = 2018
)
} # }
```
