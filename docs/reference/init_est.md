# Cohort specific smoking initiation

Reconstructs longitudinal smoking histories from cross-sectional recall
data.

## Usage

``` r
init_est(data, strat_vars = c("sex", "imd_quintile"))
```

## Arguments

- data:

  Data table of individual characteristics. Must contain 'start_age'.

- strat_vars:

  Character vector of stratification variables.

## Value

A summarized data.table of initiation probabilities by age/year/cohort.
