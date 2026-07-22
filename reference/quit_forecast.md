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
  k_smooth_age = 3,
  preserve_zeros = FALSE
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

- preserve_zeros:

  Logical - if TRUE, cells that are exactly zero in the input are kept
  out of the raster smoothing and put back at the floor value
  afterwards, instead of being clamped to 1e-6 and averaged in with
  their neighbours. This exists for initiation. Since the
  cumulative-curve fix in p_dense, a zero in the initiation surface is a
  real zero - nobody in that cohort starts at that age - not survey
  noise. Clamping it and letting the focal mean run over it drags mass
  down from the ages just below, which incorrectly increases the
  estimated values. Quitting and relapse keep the default FALSE: their
  zeros genuinely are sparse-cell noise and smoothing over them is the
  right treatment.

## Details

The model assumes the logit of the probability can be decomposed into:
Logit(P_xt) = Alpha_x + Beta_x \* Kappa_t Where: - Alpha_x: Average age
profile - Kappa_t: Time trend index - Beta_x: Sensitivity of each age to
the time trend

Note that the output for the historical years is the reconstruction from
this decomposition, not the input estimates: everything this function
returns, past and future, has been through the smoothing and the rank-1
fit.
