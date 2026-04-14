# Summarise and Plot Smoking Transition Probabilities

Generates summary plots and tables for the estimated transition
probabilities. It creates: 1. Population-weighted time trends
(Initiation, Relapse, Quit). 2. Age-specific profiles (with uncertainty
ribbons). 3. Combined panels for publication. 4. Summary CSV tables for
specific subgroups.

## Usage

``` r
summarise_smoking_transitions(config, pops)
```

## Arguments

- config:

  Named list. Must contain: `country`, `path` (root dir), `first_year`,
  `last_year`.

- pops:

  Data.table. Population counts (Age/Sex/Year/IMD) for weighting.
