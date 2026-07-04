# Calculate Net Initiation Probabilities (Synthetic Cohort)

Calculates "Net Initiation" probabilities by simulating a synthetic
cohort starting at age 12. This metric represents the net flow into the
"Current Smoker" state (Initiation + Relapse - Quitting) relative to the
non-smoking population at each age.

It solves the issue where high initiation rates at young ages are offset
by high quit rates (experimentation vs. established smoking).

## Usage

``` r
calculate_net_initiation(
  init_data,
  quit_data,
  relapse_data,
  pops,
  config,
  boot_mode = FALSE
)
```

## Arguments

- init_data:

  Data.table. Initiation probabilities.

- quit_data:

  Data.table. Quit probabilities.

- relapse_data:

  Data.table. Relapse probabilities (must contain 'time_since_quit').

- pops:

  Data.table. Population weights.

- config:

  List. Must contain 'country' and uncertainty params ('kn', 'kn_samp',
  'kR').

- boot_mode:

  Logical. If TRUE, skips writing to disk and returns the data.table
  directly.

## Details

**Assumptions:**

- The cohort starts at age 12 with 100

- Relapse probabilities are selected based on age-specific assumptions
  about average time since quitting: (12-17: 1yr, 18-24: 3yrs, 25-30:
  5yrs).
