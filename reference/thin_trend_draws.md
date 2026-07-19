# Cut bootstrap trend draws down to what the targets actually need

The full England grid is 79 ages x 16 years x 2 sexes x 5 quintiles x 3
states. At 1,000 iterations that is roughly 38 million rows, and Tables
7 to 10 need about a tenth of it. Thin before stacking rather than
after.

The thinning is checked. A filter that quietly matched nothing would
produce a raw bootstrap file that looks fine on disk and is missing a
whole table.

## Usage

``` r
thin_trend_draws(dt, keep_ages, keep_years, keep_states = "current")
```

## Arguments

- dt:

  A single trend_fit output (one bootstrap iteration).

- keep_ages, keep_years:

  Integer vectors to retain.

- keep_states:

  Character vector of smoking state columns to retain.
