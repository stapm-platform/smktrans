# Estimate age-specific probabilities of death by smoking status

Estimates the probability that an individual will survive from the
beginning to the end of a year interval with different probabilities of
survival depending on an individuals smoking status where the
probability of survival for former smokers is adjusted according to our
estimates of how risk declines by the time since some has quit.

## Usage

``` r
smoke_surv(
  data,
  diseases = tobalcepi::tob_disease_names,
  mx_data,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
)
```

## Arguments

- data:

  Data table of individual characteristics, including age, sex, IMD
  quintile and smoking status.

- diseases:

  Character vector of smoking related conditions.

- mx_data:

  Data table containing the cause-specific rates of death from smoking
  related diseases.

- min_age:

  Integer - the youngest age considered.

- max_age:

  Integer - the oldest age considered.

- min_year:

  Integer - the earliest year of observed data.

- max_year:

  Integer - the latest year of observed data.

## Value

Returns two data tables containing the age-specific probabilities of
death in and survival through a 1 year age interval, stratified by sex,
year, imd_quintile and smoking state. One data table has the detailed
estimates and the other has been tidied into the form needed for quit
probability estimation.

## Examples

``` r
if (FALSE) { # \dontrun{

test_data <- smoke_surv(
  data = hse_data,
  diseases  = tobalcepi::tob_disease_names,
  mx_data = tob_mort_data_cause
)

} # }
```
