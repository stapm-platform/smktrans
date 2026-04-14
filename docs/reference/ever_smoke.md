# Summarise and project trends in ever-smoking

Fits a weighted GLM (quasibinomial) to the trend in ever-smoking at age
25-34. This provides the "target" level for the Holford adjustment.

## Usage

``` r
ever_smoke(
  data,
  time_horizon = 2100,
  num_bins = 7,
  model = "model2",
  min_age = 15,
  min_year = 2003,
  age_cats = c("25-34")
)
```

## Arguments

- data:

  Data table of individual characteristics.

- time_horizon:

  Integer - the last year for projection.

- num_bins:

  Integer - bins for the period trend to reduce noise.

- model:

  Character - Model specification (interaction terms).

- min_age:

  Integer - youngest age for prediction.

- min_year:

  Integer - first year of survey data.

- age_cats:

  Character vector - age category for reference (e.g., "25-34").
