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
  max_year = 2100,
  cdf_smooth_df = 6
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

- cdf_smooth_df:

  Numeric - degrees of freedom for the monotone smooth of the cumulative
  curve over age. NULL or 0 skips the smooth and restores the pre-2026
  behaviour.

## Details

The cumulative curve is smoothed over age within each group before it is
differenced (see smooth_cdf). Set cdf_smooth_df = NULL to skip that and
get the old behaviour back, which is useful for comparing runs, but note
that the old behaviour is chaotic: see the note on blank_zeros in
p_smooth.
