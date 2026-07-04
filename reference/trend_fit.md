# Statistically model trends in smoking status

Fits a multinomial logistic regression "response surface" to estimate
Current/Former/Never status. The model includes high-order polynomials
for Age and Year, and interactions with Sex and IMD.

## Usage

``` r
trend_fit(
  data,
  max_iterations = 1000,
  age_var = "age",
  year_var = "year",
  sex_var = "sex",
  smoker_state_var = "smk.state",
  imd_var = "imd_quintile",
  weight_var = "wt_int"
)
```

## Arguments

- data:

  Data table of survey data.

- max_iterations:

  Integer.

- age_var, year_var, sex_var, smoker_state_var, imd_var, weight_var:

  Column names.
