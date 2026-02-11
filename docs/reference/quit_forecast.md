# Forecast probabilities of smoking initiation, quitting and relapse

Forecasts the period trends in the probabilities of smoking initiation,
quitting and relapse by age, sex and IMD quintile. This function was
designed originally to forecast just the probabilities of quitting but
has since been extended to forecast initiation and relapse probabilities
too.

## Usage

``` r
quit_forecast(
  data,
  forecast_var,
  forecast_type = c("continuing", "stationary"),
  cont_limit = NULL,
  oldest_year = 2003,
  youngest_age = 11,
  oldest_age = 88,
  age_cont_limit = 88,
  first_year = 2010,
  jump_off_year = 2015,
  time_horizon = 2050,
  smooth_rate_dim = c(3, 3),
  k_smooth_age = 3
)
```

## Arguments

- data:

  Data table containing the probabilities to be forecast.

- forecast_var:

  Character - the name of the probability variable to be forecast.

- forecast_type:

  Character - whether to apply the estimated rates of proportional
  change ("continuing") or to keep the forecast variable constant at its
  last observed value ("stationary").

- cont_limit:

  Integer - the year at which a continuing forecast becomes stationary.

- oldest_year:

  Integer - the oldest year of data we have. Default is set to 2003 for
  England.

- youngest_age:

  Integer - the youngest age we have in the data. Default is set to 11
  for England.

- oldest_age:

  Integer - the oldest age we have in the data - set to 88 for quitting
  and relapse and 30 for initiation.

- age_cont_limit:

  Integer - the age after which the forecast transition probabilities
  for a year, sex and IMD quintile are assumed not to change.

- first_year:

  Integer - the earliest year of data on which the forecast is based.

- jump_off_year:

  Integer - the last year of data.

- time_horizon:

  Integer - the last year of the forecast period.

- smooth_rate_dim:

  Numeric vector length 2. The dimensions of the 2d window used to
  smooth trends in the rates by age and year. (age, year), Defaults to
  c(3, 3). Must be odd numbers.

- k_smooth_age:

  Integer - the degree of smoothing to apply to the age pattern of
  change (rotation). If zero, then no smoothing is applied.

## Value

Returns a data.table containing the observed and forecast data.

## Details

The forecast is based on applying a Singular value decomposition (SVD)
to the logit transformed matrix of quit probabilities by age and year
for each subgroup.

## Examples

``` r
if (FALSE) { # \dontrun{

forecast_data <- quit_forecast(
  data = copy(quit_data),
  forecast_var = "quit_prob",
  forecast_type = "continuing",
  first_year = 2010,
  jump_off_year = 2015,
  time_horizon = 2030
)

} # }
```
