# Estimate socioeconomic differences in cohort survivorship

Takes mortality rates from birth since 1922 and combines them with
socioeconomically stratified mortality data from the period under
investigation to produce an estimate of the socioeconomic differences in
cohort survivorship.

## Usage

``` r
prep_surv(
  mx_data_hmd = smktrans::hmd_data_eng,
  mx_data_ons,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
)
```

## Arguments

- mx_data_hmd:

  Data table containing mortality rates from 1922 from the Human
  Mortality Database.

- mx_data_ons:

  Data table containing socioeconomically stratified mortality rates -
  that we process ourselves from mortality data.

- min_age:

  Integer - the youngest age considered.

- max_age:

  Integer - the oldest age considered.

- min_year:

  Integer - the earliest year of observed data.

- max_year:

  Integer - the latest year of observed data.

## Value

Returns a data table containing the socioeconomically stratified cohort
survivorship functions. Note that these data will only be stratified by
IMD quintile for year ages and years needed for the estimation of
smoking state transition probabilities.

## Examples

``` r
if (FALSE) { # \dontrun{
test_data <- prep_surv(
  mx_data_hmd = smktrans::hmd_data_eng,
  mx_data_ons = tob_mort_data
)
} # }
```
