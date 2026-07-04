# Forecast probabilities of smoking initiation, quitting and relapse

Forecasts trends in transition probabilities using a Lee-Carter style
Singular Value Decomposition (SVD) model.

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

  Data table with input probabilities.

- forecast_var:

  Character - variable to forecast.

- forecast_type:

  "continuing" (linear trend) or "stationary" (constant).

- cont_limit:

  Integer - year where forecast becomes stationary.

- oldest_year:

  Integer - start of historical data.

- youngest_age:

  Integer - min age.

- oldest_age:

  Integer - max age.

- first_year:

  Integer - start year for trend fitting.

- jump_off_year:

  Integer - end year of historical data.

- time_horizon:

  Integer - end year of forecast.

- smooth_rate_dim:

  Vector - dimensions for raster smoothing (c(3,3)).

- k_smooth_age:

  Integer - knots for smoothing age component.

## Details

The model assumes the logit of the probability can be decomposed into:
Logit(P_xt) = Alpha_x + Beta_x \* Kappa_t Where: - Alpha_x: Average age
profile - Kappa_t: Time trend index - Beta_x: Sensitivity of each age to
the time trend
