# Aggregate Bootstrap Uncertainty (median central estimate, CI bounds, SE)

Aggregate Bootstrap Uncertainty (median central estimate, CI bounds, SE)

## Usage

``` r
aggregate_uncertainty(
  boot_dt,
  prob_col,
  extra_keys = character(0),
  min_boot = NULL
)
```

## Arguments

- boot_dt:

  The combined data.table from all bootstrap iterations

- prob_col:

  The name of the probability column (e.g. "p_quit")

- extra_keys:

  Additional grouping columns beyond the standard demographic keys. The
  smoking trends are grouped by smk.state as well, because each
  age/year/sex/IMD cell carries three probabilities rather than one.

- min_boot:

  The number of iterations each group must have. Groups with fewer are
  an error, not something to average over quietly.
