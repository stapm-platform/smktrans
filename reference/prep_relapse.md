# Prepare long-term relapse probabilities

Combines published estimates of long-term relapse (Hawkins 2010) with
population survey data. It maps these probabilities onto the survey
population's demographic structure (education, mental health, etc.) to
derive aggregate relapse rates by Age, Sex, IMD, and Time Since Quit.

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

  Data table of individual survey data.

- hawkins_relapse:

  Data table of Hawkins et al. odds ratios/probs.

- lowest_year:

  Integer.

- highest_year:

  Integer.

- youngest_age:

  Integer.
