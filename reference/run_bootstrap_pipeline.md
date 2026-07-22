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
  B = 100,
  seed = NULL
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

- seed:

  Integer. Master seed for the resampling. Taken from \`config\$seed\`
  by the caller. Supplying it makes the whole run reproducible; passing
  NULL restores the old unseeded behaviour and warns, because an
  unseeded run cannot be reproduced or audited.

## Value

A list containing seven data.tables with all bootstrap iterations
combined: `init`, `quit`, `quit_no_init`, `relapse`, `net`, `trend` and
`survey_prev`. The master seed and the per-iteration seeds are attached
as attributes.

## Details

The fitted smoking trend surface is collected too. estimate_quitting has
always fitted it on every iteration in order to solve for quitting; it
just discarded it afterwards. Each replicate is thinned to the ages,
years and smoking states the prevalence targets need before it is
written to disk, because the full grid at B = 1000 is roughly 38 million
rows.

Alongside the fitted surface, each iteration also saves the
design-weighted survey aggregates for the same ages and years
(aggregate_survey_prev), so the prevalence targets can be built either
from the model fit or from the survey data directly, from the same draws
under the same seed. Years inside the target range that the survey does
not cover are checked against the original data once and must stay
identical across iterations.

The central estimates written out by \`process_country()\` are bootstrap
medians, not point estimates, so they are a function of the random
draws. Without a seed, two runs of identical code on identical data
return different numbers, and a diff against a previous delivery cannot
separate a genuine change from resampling noise. This bit us in July
2026 when the project team queried initiation probabilities that had
moved between deliveries.

Rather than seeding once and relying on the loop running in order, we
draw \`B\` iteration seeds up front from the master seed and set the
seed at the top of each iteration. Iteration \`i\` then depends only on
the master seed and \`i\`, so results are identical whether the loop is
run start to finish, resumed part way, or parallelised later.
