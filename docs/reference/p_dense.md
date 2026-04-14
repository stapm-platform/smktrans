# Convert probabilities of ever-smoking to age-specific probabilities

Converts the Cumulative Distribution Function (CDF) of ever-smoking into
the Probability Density Function (PDF), which represents the probability
of initiating smoking at a specific age.

## Usage

``` r
p_dense(
  data,
  cum_func_var,
  strat_vars = c("cohort", "sex", "imd_quintile"),
  lowest_year = 2003,
  max_year = 2100
)
```

## Arguments

- data:

  Data table with cumulative probabilities.

- cum_func_var:

  Character - name of cumulative variable.

- strat_vars:

  Character vector - stratification variables.

- lowest_year:

  integer - start year filter.

- max_year:

  integer - end year filter.
