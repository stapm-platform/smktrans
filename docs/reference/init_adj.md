# Adjust probabilities of ever-smoking (Holford Method)

Calibrates retrospective recall data against observed period trends
using the Holford (2014) method. This ensures that cohort histories sum
up to match the "truth" seen in cross-sectional surveys at specific
ages.

## Usage

``` r
init_adj(
  init_data,
  ever_smoke_data,
  ref_age = 30,
  fix_ref_age = FALSE,
  min_ref = 25,
  cohorts = 1973:2020,
  period_start = 2003,
  period_end = 2018
)
```

## Arguments

- init_data:

  Data table - raw estimates from init_est().

- ever_smoke_data:

  Data table - trend targets from ever_smoke().

- ref_age:

  Integer - the index age for calibration (default 30).

- fix_ref_age:

  Logical. If TRUE, forces ref_age even if data is sparse.

- min_ref:

  Integer - minimum age to allow for reference.

- cohorts:

  Integer vector - cohorts to adjust.

- period_start:

  Integer - first year of data.

- period_end:

  Integer - last year of data.
