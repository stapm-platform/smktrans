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

- The cohort is synthetic within a single year: ages are iterated with
  the year held fixed, so the stocks at age a are those of a lifetime
  lived under that one year's rates. Under secular change this differs
  from a real cohort's stocks, which matters when comparing against a
  cohort-followed survey estimator - see the header of
  22_validate_net_initiation.R for the direction and size.

- Quit and relapse probabilities must cover every age present in the
  initiation data; the function stops if they do not. On the current
  pipeline they always do – the relapse table extends below 18 by
  carrying the age-18 values, the same convention as everywhere else, so
  this calculation assumes nothing about under-18 relapse that the main
  estimates do not.

**p_start_net can be negative.** Past the age where the cohort's smoking
prevalence peaks, quitting runs ahead of initiation and relapse and the
net flow turns negative. That is a real feature of the age profile and
it is returned as it is. It used to be clamped at zero, which flattened
the curve from about age 24 and hid the fact that the model has
prevalence peaking there at all.

**Time since quitting.**

Quitters enter at time_since_quit 0, ongoing quitters move up one year
at a time, and the top category absorbs. Every former smoker then has
the relapse probability that actually applies to them.
