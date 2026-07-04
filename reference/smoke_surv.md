# Estimate age-specific probabilities of death by smoking status

Calculates survival probabilities (px) stratified by Smoking Status. It
uses the relative risks (RR) of smoking-related diseases to adjust
background mortality rates.

## Usage

``` r
smoke_surv(
  data,
  diseases = tobalcepi::tob_disease_names,
  mx_data,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
)
```

## Arguments

- data:

  Data table of individual characteristics (survey data).

- diseases:

  Character vector of disease names.

- mx_data:

  Data table of cause-specific mortality rates.

- min_age, max_age, min_year, max_year:

  Integers.
