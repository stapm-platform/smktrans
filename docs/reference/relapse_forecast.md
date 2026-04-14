# Forecast relapse probabilities (Time-Since-Quit Stratified)

Applies the trends forecast by \`quit_forecast\` (which are only
Age/Sex/IMD) to the detailed Time-Since-Quit data. It calculates a
scaling factor from the "jump-off" year and applies it to future years.

## Usage

``` r
relapse_forecast(
  relapse_forecast_data,
  relapse_by_age_imd_timesincequit,
  jump_off_year = 2018
)
```

## Arguments

- relapse_forecast_data:

  Data table - output from quit_forecast() (The trend).

- relapse_by_age_imd_timesincequit:

  Data table - detailed base rates from prep_relapse().

- jump_off_year:

  Integer.
