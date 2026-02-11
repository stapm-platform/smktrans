# Cohort specific smoking initiation

Calculate the age-specific probabilities and cumulative probabilities of
starting to smoke cigarettes regularly based on self-reported smoking
histories.

## Usage

``` r
init_est(data, strat_vars = c("sex", "imd_quintile"))
```

## Arguments

- data:

  Data table of individual characteristics.

- strat_vars:

  Character vector - the variables by which to stratify the cohort
  estimates e.g. sex and/or IMD quintile.

## Value

Returns a data table containing the age-specific probabilities and
cumulative probabilities of starting to smoke, and of being an ever or
never smoker.

## Details

Takes the self-reported age that cigarettes were first smoked regularly
and constructs a vector of 0s, 1s and NAs corresponding to each age for
each individual. 1 = initiated smoking, NA = censored.

## Examples

``` r
if (FALSE) { # \dontrun{

test_data <- init_est(data = hse_data)

} # }
```
