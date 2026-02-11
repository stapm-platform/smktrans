# Prepare long-term relapse probabilities

Combines published estimates of long-term relapse with the population
survey data to arrive at the expected values for relapse probabilities
within defined subgroups.

## Usage

``` r
prep_relapse(
  data,
  hawkins_relapse = smktrans::hawkins_relapse,
  lowest_year = 2003,
  highest_year = 2018,
  youngest_age = 18
)
```

## Arguments

- data:

  Data table containing individual characteristics from the population
  survey data.

- hawkins_relapse:

  Data table containing a tidied version of the estimates of long-term
  smoking relapse probability from Hawkins et al. (2010) .

- lowest_year:

  integer - lowest year of data available (for England this is 2003 and
  for Scotland this is 2008). Default is 2003.

- highest_year:

  integer - highest year of data available.

- youngest_age:

  integer - youngest age in data (for England 11, for Scotland 16).

## Value

Returns two data tables: First, with relapse probabilities stratified by
year, age, IMD quintile and time since quit (for use in the STAPM
simulation); Second, with relapse probabilities stratified by just year,
age and IMD quintile (for use in transition prob estimation).

## Details

This function takes the estimates of relapse to smoking from Hawkins et
al. (2010) and processes them into probabilities of relapse to smoking
by the significant variables from the above paper (age, time since quit,
degree or not, mental health condition or not, married or not). Note
that physical health / gp visits was also significant but not included
here partly because the surveys do not have the right variables to do
so. Once we have mapped relapse probabilities onto the survey data by
the above variables, we can then calculate the variation in expected
probability of relapse by age, sex, time since quit and IMD quintile.

## References

Hawkins J, Hollingworth W, Campbell R (2010). “Long-term smoking
relapse: a study using the british household panel survey.” *Nicotine &
tobacco research*, **12**(12), 1228–1235.

## Examples

``` r
if (FALSE) { # \dontrun{
test_data <- prep_relapse(
  data = hse_data,
  hawkins_relapse = smktrans::hawkins_relapse
)
} # }
```
