# Statistically model trends in smoking status

Fits a multinomial logistic regression "response surface" to estimate
Current/Former/Never status. The model includes high-order polynomials
for Age and Year, and interactions with Sex and IMD.

Two changes were made when the smoking trends were brought into the
bootstrap:

1\. The prediction grid is now pinned by the caller rather than read off
the data. Under resampling the observed min/max age and year, and the
set of IMD quintiles present, can all shift between iterations. Left
unpinned that silently produces replicates with different numbers of
rows, which then cannot be stacked and cannot form a covariance matrix.

2\. Predicting outside the observed range of age or year now has to be
asked for. See the note on \`allow_extrapolation\` below.

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
  weight_var = "wt_int",
  grid_ages = NULL,
  grid_years = NULL,
  grid_sex = c("Male", "Female"),
  grid_imd = NULL,
  expected_states = c("current", "former", "never"),
  allow_extrapolation = FALSE,
  boot_id = NULL,
  tol = 1e-08
)
```

## Arguments

- data:

  Data table of survey data.

- max_iterations:

  Integer, passed to nnet::multinom.

- age_var, year_var, sex_var, smoker_state_var, imd_var, weight_var:

  Column names.

- grid_ages:

  Integer vector of ages to predict for. Defaults to the range observed
  in \`data\`, which is only safe outside bootstrap mode.

- grid_years:

  Integer vector of years to predict for. Defaults as above.

- grid_sex:

  Character vector of sex levels.

- grid_imd:

  Character vector of IMD quintile levels.

- expected_states:

  The smoking states the model must return a column for.

- allow_extrapolation:

  Logical. If FALSE (the default) it is an error for \`grid_ages\` or
  \`grid_years\` to reach beyond the range present in \`data\`.

- boot_id:

  Optional scalar written to a \`boot_id\` column on the output. Also
  suppresses the progress messages.

- tol:

  Tolerance for the "probabilities sum to one" check.
