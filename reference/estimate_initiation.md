# Estimate and Forecast Smoking Initiation

1\. Estimates historical initiation (cumulative -\> density). 2.
Forecasts future initiation using \`quit_forecast\` (continuing trend).
3. Saves raw, adjusted, and forecasted outputs.

## Usage

``` r
estimate_initiation(config, survey_data, boot_mode = FALSE)
```

## Arguments

- config:

  List. Must contain: first_year, last_year, min_age, max_age, ref_age,
  smokefree_target_year, age_trend_limit_init, smooth_rate_dim_init,
  k_smooth_age_init. If init_model_choice is "auto", it must also
  contain the selection settings: init_auto_holdout_bins,
  init_auto_tie_margin, init_auto_floor, init_auto_ceiling,
  init_auto_max_slope_mult. Making them explicit in the config, rather
  than falling back to defaults buried in ever_smoke(), means a run can
  be reproduced from its config block alone.

- survey_data:

  Data table of individual survey records.

- boot_mode:

  Logical. If TRUE, skips writing to disk and returns the estimates for
  one bootstrap iteration.
