# Estimate and Forecast Smoking Relapse

1\. Prepares base relapse rates (Hawkins). 2. Forecasts the Age/Sex/IMD
specific trend using \`quit_forecast\`. 3. Scales the Time-Since-Quit
data using \`relapse_forecast\`. 4. Imputes data for ages \< 18.

## Usage

``` r
estimate_relapse(config, survey_data, boot_mode = FALSE)
```
