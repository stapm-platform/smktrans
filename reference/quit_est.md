# Estimate smoking quit probabilities (The Flow Equation)

Calculates the probability of quitting required to balance the
population stocks (Current, Former, Never) given the flows (Initiation,
Relapse, Mortality).

## Usage

``` r
quit_est(
  trend_data,
  survivorship_data,
  mortality_data,
  relapse_data,
  initiation_data,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
)
```

## Arguments

- trend_data:

  Output of trend_fit().

- survivorship_data:

  Output of prep_surv().

- mortality_data:

  Output of smoke_surv().

- relapse_data:

  Output of prep_relapse() (aggregated version).

- initiation_data:

  Output of init_adj().

- min_age, max_age, min_year, max_year:

  Integers.

## Details

Solves the equation: N(t+1) = N(t) \* Survival \* (1 - Quit) + (Former
\* Relapse) + (Never \* Init) Rearranged to solve for Quit.
