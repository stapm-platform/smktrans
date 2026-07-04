# Package index

## High level functions

- [`process_country()`](https://stapm-platform.github.io/smktrans/reference/process_country.md)
  : Master Process Wrapper
- [`calculate_net_initiation()`](https://stapm-platform.github.io/smktrans/reference/calculate_net_initiation.md)
  : Calculate Net Initiation Probabilities (Synthetic Cohort)
- [`write_excel_report()`](https://stapm-platform.github.io/smktrans/reference/write_excel_report.md)
  : Write Transition Probability Estimates to Excel
- [`build_web_reports()`](https://stapm-platform.github.io/smktrans/reference/build_web_reports.md)
  : Build Web Reports for Pkgdown

## Uncertainty & Bootstrapping

- [`run_bootstrap_pipeline()`](https://stapm-platform.github.io/smktrans/reference/run_bootstrap_pipeline.md)
  : Run the Master Bootstrap Pipeline
- [`aggregate_uncertainty()`](https://stapm-platform.github.io/smktrans/reference/aggregate_uncertainty.md)
  : Aggregate Bootstrap Uncertainty (Now includes Median Central
  Estimate)
- [`generate_bootstrap_sample()`](https://stapm-platform.github.io/smktrans/reference/generate_bootstrap_sample.md)
  : Generate a Single Bootstrap Sample for Complex Survey Data
- [`generate_uncertainty()`](https://stapm-platform.github.io/smktrans/reference/generate_uncertainty.md)
  : Generate Uncertainty Intervals for Transition Probabilities
  (deprecated)

## Smoking trends

- [`ever_smoke()`](https://stapm-platform.github.io/smktrans/reference/ever_smoke.md)
  : Summarise and project trends in ever-smoking
- [`trend_fit()`](https://stapm-platform.github.io/smktrans/reference/trend_fit.md)
  : Statistically model trends in smoking status

## Smoking initiation

- [`estimate_initiation()`](https://stapm-platform.github.io/smktrans/reference/estimate_initiation.md)
  : Estimate and Forecast Smoking Initiation
- [`init_est()`](https://stapm-platform.github.io/smktrans/reference/init_est.md)
  : Cohort specific smoking initiation
- [`init_adj()`](https://stapm-platform.github.io/smktrans/reference/init_adj.md)
  : Adjust probabilities of ever-smoking (Holford Method)

## Quitting smoking

- [`estimate_quitting()`](https://stapm-platform.github.io/smktrans/reference/estimate_quitting.md)
  : Estimate and Forecast Smoking Quitting
- [`quit_est()`](https://stapm-platform.github.io/smktrans/reference/quit_est.md)
  : Estimate smoking quit probabilities (The Flow Equation)
- [`quit_forecast()`](https://stapm-platform.github.io/smktrans/reference/quit_forecast.md)
  : Forecast probabilities of smoking initiation, quitting and relapse
- [`smoke_surv()`](https://stapm-platform.github.io/smktrans/reference/smoke_surv.md)
  : Estimate age-specific probabilities of death by smoking status
- [`prep_surv()`](https://stapm-platform.github.io/smktrans/reference/prep_surv.md)
  : Estimate socioeconomic differences in cohort survivorship

## Relapse to smoking

- [`estimate_relapse()`](https://stapm-platform.github.io/smktrans/reference/estimate_relapse.md)
  : Estimate and Forecast Smoking Relapse
- [`prep_relapse()`](https://stapm-platform.github.io/smktrans/reference/prep_relapse.md)
  : Prepare long-term relapse probabilities
- [`relapse_forecast()`](https://stapm-platform.github.io/smktrans/reference/relapse_forecast.md)
  : Forecast relapse probabilities (Time-Since-Quit Stratified)

## Misc

- [`bin_var()`](https://stapm-platform.github.io/smktrans/reference/bin_var.md)
  : Bin numeric variable
- [`p_dense()`](https://stapm-platform.github.io/smktrans/reference/p_dense.md)
  : Convert probabilities of ever-smoking to age-specific probabilities
- [`p_smooth()`](https://stapm-platform.github.io/smktrans/reference/p_smooth.md)
  : Smooth age and period pattern in probability values

## Data

- [`hmd_data_eng`](https://stapm-platform.github.io/smktrans/reference/hmd_data_eng.md)
  : Death rates for England & Wales
- [`hmd_data_scot`](https://stapm-platform.github.io/smktrans/reference/hmd_data_scot.md)
  : Death rates for Scotland
- [`hawkins_relapse`](https://stapm-platform.github.io/smktrans/reference/hawkins_relapse.md)
  : Long-term probabilities of smoking relapse
