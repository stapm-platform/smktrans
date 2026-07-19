# Estimate and Forecast Smoking Quitting

1\. Trend fitting & Mortality calculation. 2. Historical Quit Solver. 3.
Forecasts Quit rates (continuing trend). 4. Forecasts 'No Initiation'
Quit rates (counterfactual).

In boot_mode the fitted trend surface is now returned alongside the quit
forecasts. It was always being computed and then thrown away; returning
it gives us bootstrapped smoking prevalence for free.

## Usage

``` r
estimate_quitting(
  config,
  survey_data,
  tob_mort_data,
  tob_mort_data_cause,
  boot_mode = FALSE,
  smk_init_data_boot = NULL,
  relapse_data_boot = NULL,
  precalc_mortality = NULL,
  boot_id = NULL
)
```
