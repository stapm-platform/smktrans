# Summarise and project trends in ever-smoking

Calculates the expected proportion of ever-smokers, and fits a linear
trend over time to the proportion of people who have ever smoked in the
age category 25-34 years.

## Usage

``` r
ever_smoke(
  data,
  time_horizon = 2100,
  num_bins = 7,
  model = c("model1", "model2", "model3", "model4", "model5")[1],
  min_age = 15,
  min_year = c(2003, 2008)[1]
)
```

## Arguments

- data:

  Data table of individual characteristics.

- time_horizon:

  Integer - the last year for which to generate predictions for
  ever-smoking at the index age.

- num_bins:

  Integer - the number of bins to create from the continuous period
  (year) variable. Wider bins = fewer data points from which to estimate
  the period trend, but less random uncertainty on each data point.

- model:

  Character - c("model1", "model2", "model3", "model4"). Model 1 -
  two-way interactions of the year trend with both sex and IMD quintile.
  Model 2 - two-way interaction of the year trend with sex only. Model
  3 - two-way interaction of the year trend with IMD quintile only.
  Model 4 - no two-way interactions of the year trend with either sex or
  IMD quintile.

- min_age:

  Integer - the youngest age for which a prediction of the progression
  of ever-smoking in a cohort should be generated. Defaults to 15 years.

- min_year:

  Integer - the first year of survey data. For England 2003 and for
  Scotland 2008.

## Value

Returns two data tables:

- "data_points", the proportions of ever-smokers at age 25 observed in
  the survey data;

- "predicted_values", the model predictions up to the time horizon.

## Details

Uses the 'survey' package to fit a model to the trends in ever-smoking
that accounts for survey sampling error and survey design.

The linear model is fitted to a binary response variable, 1 = ever
smoker, 0 = never smoker and estimates variation in the trend by sex and
quintiles of the Index of Multiple Deprivation. The fitted model is then
used to predict / extrapolate the trend up to the year specified by
'time horizon'.

## Examples

``` r
if (FALSE) { # \dontrun{

test_data_trend <- ever_smoke(data = hse_data)

} # }
```
