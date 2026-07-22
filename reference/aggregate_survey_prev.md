# Design-weighted survey aggregates for the prevalence targets

Collapses one survey dataset (or one bootstrap resample of it) to
weighted sums by year, age, sex and IMD quintile: the total design
weight, the design weight carried by current smokers, and the respondent
count. Summing these over any set of cells and dividing gives the pooled
design-weighted prevalence for that set exactly, which is what the
survey-sourced calibration targets are built from. Storing sums rather
than proportions is what makes that exact: a cell that is empty in a
resample contributes nothing to either sum, which is the correct pooled
estimator, whereas a missing proportion would need a decision about how
to average over it.

## Usage

``` r
aggregate_survey_prev(
  data,
  keep_ages,
  keep_years,
  state_var = "smk.state",
  age_var = "age",
  year_var = "year",
  sex_var = "sex",
  imd_var = "imd_quintile",
  weight_var = "wt_int",
  current_level = "current"
)
```

## Arguments

- data:

  One survey dataset or resample.

- keep_ages, keep_years:

  Integer vectors - the cells to keep. Years in keep_years that the
  survey does not cover are simply absent from the output; the caller
  decides whether that is expected.

- state_var, age_var, year_var, sex_var, imd_var, weight_var:

  Column names.

- current_level:

  Character - the value of the state variable that counts as a current
  smoker.

## Details

The completeness rule matches trend_fit: a missing value in any of the
variables is an error to resolve upstream, not a row to drop here.
