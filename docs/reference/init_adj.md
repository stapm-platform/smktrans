# Adjust probabilities of ever-smoking

Takes the probabilities of ever-smoking estimated from respondent recall
and adjusts them according to the reported proportions of ever-smokers
at a reference age.

## Usage

``` r
init_adj(
  init_data,
  ever_smoke_data,
  ref_age = 30,
  cohorts = 1973:2020,
  period_start = 2003,
  period_end = 2018
)
```

## Arguments

- init_data:

  Data table - raw estimates of the probabilities of ever-smoking by
  age, sex and IMD quintile.

- ever_smoke_data:

  Data table - reference values for the proportion of ever-smokers.

- ref_age:

  Integer - the index age for calibration

- cohorts:

  Integer vector - the cohorts to be adjusted. The min cohort might
  sensibly be period_start - ref_age, and the max cohort might be the
  baseline year of the model e.g. 2020.

- period_start:

  Integer - the first year of data (England - 2003, Scotland - 2008)

- period_end:

  Integer - the last year of data

## Value

Returns an updated version of init_data with a new variable containing
the adjusted values for the probabilities of ever smoking. The data has
also been filtered to only include cohorts for which it is possible to
make an adjustment.

## Details

This is based on the method applied by Holford et al. (2014) .

## References

Holford TR, Levy DT, McKay LA, Clarke L, Racine B, Meza R, Land S, Jeon
J, Feuer EJ (2014). “Patterns of birth cohort–specific smoking
histories, 1965–2009.” *American journal of preventive medicine*,
**46**(2), e31–e37.

## Examples

``` r
if (FALSE) { # \dontrun{

# Estimate the raw probabilities of ever-smoking
init_data_raw <- init_est(
  data = hse_data,
  strat_vars = c("sex", "imd_quintile")
)

# Estimate the reference values - observed proportions of ever-smokers
ever_smoke_data <- ever_smoke(
  data = hse_data,
  time_horizon = 2100
)

# Adjust the probabilities of ever-smoking
init_data_adj <- init_adj(
  init_data = copy(init_data_raw),
  ever_smoke_data = copy(ever_smoke_data$predicted_values),
  ref_age = 30,
  cohorts = 1971:2020, # 50 cohorts
  period_start = 2001,
  period_end = 2016
)


} # }
```
