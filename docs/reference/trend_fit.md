# Statistically model trends in current, former and never smoking

Fit a multinomial model to the distribution of people among current,
former and never smoking status and estimate the variation in trends by
sex and socioeconomic conditions in terms of quintiles of the Index of
Multiple Deprivation. The parameterisation of the statistical model
within this function can have a strong effect on the estimated
probabilities of quitting smoking and on the trajectories of smoking
prevalence that are forecast using these estimates.

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

  Data table containing individual-level survey data over multiple
  years.

- max_iterations:

  Integer - the maximum number of iterations to try when looking for the
  multinomial model to converge to a stable model fit.

- age_var:

  Character - the name of the variable containing age in single years.

- year_var:

  Character - the name of the variable containing year in single years.

- sex_var:

  Character - the name of the variable containing sex (m/f).

- smoker_state_var:

  Character - the name of the variable containing smoking status
  (current, former, never smoker).

- imd_var:

  Character - the name of the variable containing Index of Multiple
  Deprivation quintiles.

- weight_var:

  Character - the name of the variable containing the survey weights.

## Value

Returns a data table containing the fitted values by age, year, sex and
IMD quintile.

## Details

The explanatory variables for age and year are specified in the form of
a cubic response surface. An additional 4th order term for age is added
to better capture the non-linear patterns over age e.g. as individuals
initiate and then quit with age. Interactions of this surface with sex
and IMD quintiles are limited to 3rd order terms. Interactions with the
quadratic term are included for age but not for year to avoid
over-fitting variation in the trend over years, which is more of a
linear change than the pattern over age. The decision to do so was made
after visually exploring the fit of simpler and more complex model
specifications to the data.

## Examples

``` r
if (FALSE) { # \dontrun{

test_data <- trend_fit(data = hse_data)

} # }
```
