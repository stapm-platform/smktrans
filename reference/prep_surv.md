# Estimate socioeconomic differences in cohort survivorship

Combines long-term historical mortality rates (HMD) with recent
socioeconomically stratified mortality data (ONS) to produce cohort
survivorship curves (lx) stratified by Age, Sex, and IMD Quintile.

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

  Data table of HMD mortality rates (1922+).

- mx_data_ons:

  Data table of ONS stratified mortality rates.

- min_age, max_age, min_year, max_year:

  Integers defining the scope.
