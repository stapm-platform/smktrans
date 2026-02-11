# Convert probabilities of ever-smoking to age-specific probabilities of smoking initiation

Converts the cumulative density function to the probability density
function, assuming an interval of 1.

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

  Data table containing estimates of the cumulative probabilities of
  ever-smoking by age.

- cum_func_var:

  Character - the name of the variable containing the cumulative
  probabilities.

- strat_vars:

  Character vector - the variables by which to stratify the calculation.

- lowest_year:

  integer - lowest year of data available (for England this is 2003 and
  for Scotland this is 2008). Default is set to 2003, for HSE.

- max_year:

  integer - the latest year considered in the data + forecast

## Value

Returns an updated version of data with a new variable for the
age-specific probabilities of smoking initiation.

## Examples

``` r
if (FALSE) { # \dontrun{

init_data <- p_dense(data = copy(init_data_adj), cum_func_var = "p_ever_smoker_adj",
                    strat_vars = c("cohort", "sex", "imd_quintile"))

} # }
```
