# smktrans 2.3.0

Changes to initiation estimation and the calibration-target outputs, plus
reporting and validation additions. Versions 2.2.1 and 2.3.0 were developed in
one cycle; the 2.2.1 changes were never released separately and are folded in
here.

## Estimation changes

* The initiation age trend limit rises from 25 to 30 (= `ref_age`), so ages
  26--30 are estimated rather than carrying the age-25 value forward. The quit
  trend limit stays at 79; the relapse trend limit is retained but inert under
  the stationary relapse forecast, where the trend scaler is identically 1.

* New `anchor_recent_cohorts()`. For cohorts beyond the trend model's own
  data, the ever-smoking targets that set the level of each cohort's
  initiation curve are re-anchored on an external youth smoking series -- the
  SDD for England (`sdd_ever_smoked_england.csv`). The youth series is linked
  onto the target scale by a factor estimated on the cohorts both sources
  observe, applied as a cohort-level ratio so the sex and IMD gradients pass
  through, blended over a configurable taper at the handover, and held at its
  final value for cohorts born after the last youth survey. Cohorts inside the
  trend model's data are unchanged. Config keys `youth_anchor_file`,
  `youth_anchor_age_centre` and `youth_anchor_taper` are all required once a
  file is set; countries without a file are unchanged. In the bootstrap the
  youth series is fixed external data and the link factor is re-estimated each
  iteration. The `estimate_initiation()` step gains a hook that calls the
  function when a file is configured and errors if the accompanying keys are
  absent. Note that this has reduced initiation rates for England but the corresponding 
  corrections have not yet been explored for Scotland and Wales, so these are still reading slightly 
  higher on initiation. 

## Calibration-target changes

* `produce_prevalence_targets.R` gains a `target_source` option, `"survey"` or
  `"model"`. `"survey"` builds each target as the pooled design-weighted
  prevalence taken directly from each bootstrap resample of the survey;
  `"model"` reproduces the previous behaviour, collapsing the fitted trend
  surface over each target's cells with ONS population weights. Default is
  `"survey"`. The means file gains `source` and `years_used` columns.

* Survey years the data does not yet cover are declared in
  `known_missing_years`. A target keeps its label but records the years
  actually used, the only permitted gap between label and data is the declared
  one, and a declared-missing year that later appears in the data stops the
  script rather than being silently ignored.

* `run_bootstrap_pipeline()` collects design-weighted survey aggregates
  (`aggregate_survey_prev()`, new in `trend_fit.R`) for the target ages and
  years on each iteration, alongside the fitted trend surface it already
  collected, and returns both. `process_country()` saves the survey
  aggregates as `raw_boot_survey_prev_<country>.rds`. Both sources are written
  from the same run, so switching between them needs no re-run.

## Reporting changes

* `write_excel_report()` gains a 'Run Configuration' sheet: an auto-generated
  dump of every element of the run config, plus the package version, the
  resolved initiation trend model, the bootstrap seed and the run time. It
  iterates `names(config)`, so a config parameter added later cannot fall out
  of the report.

* All config access in the report uses `[[` rather than `$`, which had
  partial-matched `config$kn` to `kn_samp`. The legacy `kn` and `kR` rows are
  removed; the uncertainty block reports the bootstrap iteration count, the
  seed and the interval definition.

* The parameter table no longer overwrites the variable-definitions table
  above it. Table contents are corrected and extended: the initiation age
  trend limit is reported, the relapse trend limit is flagged inactive under
  the stationary forecast, the model-choice row reports the resolved model,
  `cont_limit` is described as the year the projected trend goes flat, and
  `max_age_init` is replaced by `ref_age`. The hard-coded package-version
  fallback is removed.

* The run manifest saved by `process_country()` now includes the full config,
  so a run's settings travel with its outputs rather than depending on a
  separately saved snapshot.

## Validation and verification

* New `24_verify_youth_anchor.R`. Hard checks are exact identities the
  anchored outputs satisfy by construction: the link factor rebuilt from
  scratch, anchored targets equalling the linked youth series outside the
  taper, the taper blend rearranged into a checkable identity, and the
  structure of the ratio path. A shape diagnostic plots the implied initiation
  against the youth series over years with complete age coverage. The config
  is read from the run manifest when present.

* The net-initiation validation is reframed. The survey-side estimators are
  unchanged; the documentation is corrected to state that the model's
  single-year synthetic cohort and the survey's followed cohort carry
  different stocks, so interval coverage is reported for ages 16--24 and the
  older ages are descriptive. The model side is averaged over the diagonal's
  transition years rather than the full survey window, and a stale comment
  about clamping negative flows is removed.

* `calculate_net_initiation()` documents the single-year synthetic cohort
  assumption and converts two silent NA-to-zero fills, on the quit and relapse
  merges, into hard checks.

# smktrans 2.2.1

Not released. Developed as the age-trend-limit change plus the reporting
upgrade, then folded into 2.3.0 during the same QA cycle; its changes are
recorded above.

# smktrans 2.2.0

This release changes published numbers. The headline movements on the England
run: initiation at ages 16--21 fall slightly (truncated cohorts now
calibrate on their own completed curves rather than a borrowed profile),
relapse rises slightly at short times since quit (the rebuilt Hawkins table),
the relapse forecast is flat rather than trending, and net initiation is
reported on both sides of zero. Quitting moves as a knock-on effect of these changes through the
balancing equation. The estimates were validated against the Smoking Toolkit
Study after these changes -- and the validation passed; see the new validation vignette.

## Estimation changes

* `hawkins_relapse` rebuilt (`data-raw/Relapse_Hawkins2010/`). The paper's
  sparse years 6--9 are pooled into a single monotone tail (17 relapses / 1,194
  at risk), and the baseline odds are calibrated so that a cohort reconstructed
  from the paper's own Table 1 reproduces its reported one-year relapse rate of
  15.1% exactly. The build script verifies this on every run.

* `init_adj()` completes truncated cohorts before calibration: a cohort
  observed only to age $r$ has its curve multiplied by $F(30)/F(r)$, estimated
  from the most recent fully observed cohorts within sex and IMD quintile,
  before the age-30 target divides it. The old behaviour quietly assumed nobody
  starts between $r$ and 30, over-scaling young-age initiation by about 5% at
  $r = 21$. `min_ref` drops from 21 to 18 so the youngest cohorts with usable
  data calibrate on their own numbers. Fully observed cohorts are identical
  to the previous version.

* `calculate_net_initiation()` rewritten. The former-smoker stock is tracked by
  time since quit -- quitters enter at zero and age one band per year -- 
  replacing a fixed assumption (1/3/5 years by age band) that stepped the
  relapse probability 63% at exactly age 18 and put a spurious cliff in the
  published series. Negative net flows are returned rather than clamped: past
  the prevalence peak they are the finding. Ages and years now come from the
  data rather than being hard-coded.

* `estimate_relapse()` forecasts relapse as stationary. Hawkins has no time
  dimension; the only year-to-year movement in the surface is demographic
  re-weighting, and projecting a trend fitted to it produced relapse
  probabilities outside the envelope of the evidence (over six times the
  jump-off level at ages 75--83 by 2040).

* `quit_forecast()` gains `preserve_zeros` (default `FALSE`; initiation passes
  `TRUE`). Genuine zeros in the input are held out of the surface smoothing and
  restored at the floor, instead of being clamped to 1e-6 and averaged with
  their neighbours -- which inflated the published initiation tail at ages
  24--25 by a factor of 3 to 4. The clamps use `which()` indexing, fixing a
  latent error under NA subscripts. The documentation now states that all
  output, historical years included, is the Lee--Carter reconstruction.

* `estimate_initiation()` jumps the initiation forecast off from the last
  estimated year rather than the year before it, so the final year of data
  informs the trend. Quitting and relapse keep their existing convention.

## New features

* `ever_smoke()` accepts `model = "auto"`: candidate structures are scored on
  held-out survey years (holdout QAIC with the dispersion estimated once), the
  simplest model within two units of the best wins, and the winner must pass
  range and slope guard rails on its projection before acceptance. The
  scoreboard is returned with the output. Selection settings live in the
  config; explicit model names behave exactly as before.

* The base run resolves the "auto" choice and writes it to the outputs
  directory; bootstrap iterations read the file rather than re-selecting, so a
  single model structure underlies each uncertainty interval. A bootstrap run
  before the base run stops and says why.

## Validation and verification

* New validation suite in `transition_probability_validation/` (project
  repository): quitting and net initiation compared against the Smoking Toolkit
  Study over the estimated years, with the survey-side estimators corrected for
  definition, age indexing, exposure weighting, and -- for net initiation -- 
  the cohort effect in cross-sectional age gradients (a cohort-followed
  estimator is the primary comparison). Relapse is verified against the
  envelope of its own Hawkins inputs, with version-skew guards. The report
  doubles as a regression test and its England results are written up in the
  new vignette *Validation of the England Transition Probabilities*.

* New holdout harness (`30_trend_holdout.R`) for adjudicating changes to the
  trend model: fit on all years but the last, predict the last, score weighted
  deviance and prevalence error against the held-out people, alongside a
  persistence baseline and a bootstrap stability number; plus a refit-churn
  measure of how much adding a year of data rewrites settled history. Several
  proposed upgrades to `trend_fit()`, `quit_forecast()` and `p_smooth()` were
  scored by these instruments during this cycle and not adopted; the current
  implementations stand on that evidence.

## Documentation

* Main workflow vignette updated for the changes above, with diagrams of the
  model-selection contract and the net-initiation synthetic cohort.
  
* Model validation vignette for England added.

# smktrans 2.1.2

* Last release before this changelog was introduced. Earlier changes are
  recorded in the git history.
