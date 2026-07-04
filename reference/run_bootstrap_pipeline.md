# Run the Master Bootstrap Pipeline

Executes the optimized bootstrapping loop for smoking transition
probabilities. This function pre-calculates mortality risks once to save
memory, then iterates through \`B\` bootstrap samples, saving
intermediate results to a temporary directory before combining them into
final output tables.

## Usage

``` r
run_bootstrap_pipeline(
  config,
  survey_data,
  pops,
  tob_mort_data,
  tob_mort_data_cause,
  B = 100
)
```

## Arguments

- config:

  A list containing model configuration parameters (e.g., country,
  years, ages).

- survey_data:

  A data.table or data.frame containing the base survey data.

- pops:

  A data.table containing population denominators.

- tob_mort_data:

  A data.table containing general tobacco mortality data.

- tob_mort_data_cause:

  A data.table containing cause-specific tobacco mortality data.

- B:

  Integer. The number of bootstrap iterations to run. Defaults to 100.

## Value

A list containing five massive data.tables with all bootstrap iterations
combined: `init`, `quit`, `quit_no_init`, `relapse`, and `net`.
