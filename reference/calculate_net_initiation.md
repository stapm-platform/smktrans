# Calculate Net Initiation Probabilities (Synthetic Cohort)

Calculates "Net Initiation" probabilities by simulating a synthetic
cohort. This metric represents the net flow into the "Current Smoker"
state (Initiation + Relapse - Quitting) relative to the non-smoking
population at each age.

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

  Data.table. Not used. The synthetic cohort is a fixed 1000 people per
  subgroup, so there is nothing to weight. Kept in the signature because
  process_country_wrapper() and run_bootstrap_pipeline() both pass it.

- config:

  List. Must contain 'country' and 'path'. The uncertainty parameters
  this used to document ('kn', 'kn_samp', 'kR') are not used either:
  uncertainty is handled by run_bootstrap_pipeline() calling this once
  per bootstrap sample.

- boot_mode:

  Logical. If TRUE, skips writing to disk and returns the data.table
  directly.

## Details

**Assumptions:**

- The cohort starts with 100 in the initiation data.

- Nobody dies. Over ages 12 to 30 that is close enough to true, and it
  is what makes the denominator below equal to 1 - prevalence.

**p_start_net can be negative.** Past the age where the cohort's smoking
prevalence peaks, quitting runs ahead of initiation and relapse and the
net flow turns negative. That is a real feature of the age profile and
it is returned as it is. It used to be clamped at zero, which flattened
the curve from about age 24 and hid the fact that the model has
prevalence peaking there at all.

**Time since quitting.**

This used to pick a relapse probability by assuming how long people at
each age had been quit: 1 year if under 18, 3 years from 18 to 24, 5
years from 25. That produced a step change in p_relapse at exactly 18
and 25 (a 63 18 on the England data), and because the relapse flow is a
large part of the net flow, it put a spurious cliff into the published
numbers. Net initiation fell 83

It is not necessary to assume any of it. The simulation already carries
a stock of former smokers, so carry it BY time since quit instead:
quitters enter at time_since_quit 0, survivors move up one year at a
time, and the top category absorbs. Every former smoker then has the
relapse probability that actually applies to them, and the assumption
disappears rather than being replaced by a better one.
