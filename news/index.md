# Changelog

## smktrans 2.2.0

This release changes published numbers. The headline movements on the
England run: initiation at ages 16–21 fall slightly (truncated cohorts
now calibrate on their own completed curves rather than a borrowed
profile), relapse rises slightly at short times since quit (the rebuilt
Hawkins table), the relapse forecast is flat rather than trending, and
net initiation is reported on both sides of zero. Quitting moves as a
knock-on effect of these changes through the balancing equation. The
estimates were validated against the Smoking Toolkit Study after these
changes – and the validation passed; see the new validation vignette.

### Estimation changes

- `hawkins_relapse` rebuilt (`data-raw/Relapse_Hawkins2010/`). The
  paper’s sparse years 6–9 are pooled into a single monotone tail (17
  relapses / 1,194 at risk), and the baseline odds are calibrated so
  that a cohort reconstructed from the paper’s own Table 1 reproduces
  its reported one-year relapse rate of 15.1% exactly. The build script
  verifies this on every run.

- [`init_adj()`](https://stapm-platform.github.io/smktrans/reference/init_adj.md)
  completes truncated cohorts before calibration: a cohort observed only
  to age $`r`$ has its curve multiplied by $`F(30)/F(r)`$, estimated
  from the most recent fully observed cohorts within sex and IMD
  quintile, before the age-30 target divides it. The old behaviour
  quietly assumed nobody starts between $`r`$ and 30, over-scaling
  young-age initiation by about 5% at $`r = 21`$. `min_ref` drops from
  21 to 18 so the youngest cohorts with usable data calibrate on their
  own numbers. Fully observed cohorts are bit-identical to the previous
  version.

- [`calculate_net_initiation()`](https://stapm-platform.github.io/smktrans/reference/calculate_net_initiation.md)
  rewritten. The former-smoker stock is tracked by time since quit –
  quitters enter at zero and age one band per year – replacing a fixed
  assumption (1/3/5 years by age band) that stepped the relapse
  probability 63% at exactly age 18 and put a spurious cliff in the
  published series. Negative net flows are returned rather than clamped:
  past the prevalence peak they are the finding. Ages and years now come
  from the data rather than being hard-coded.

- [`estimate_relapse()`](https://stapm-platform.github.io/smktrans/reference/estimate_relapse.md)
  forecasts relapse as stationary. Hawkins has no time dimension; the
  only year-to-year movement in the surface is demographic re-weighting,
  and projecting a trend fitted to it produced relapse probabilities
  outside the envelope of the evidence (over six times the jump-off
  level at ages 75–83 by 2040).

- [`quit_forecast()`](https://stapm-platform.github.io/smktrans/reference/quit_forecast.md)
  gains `preserve_zeros` (default `FALSE`; initiation passes `TRUE`).
  Genuine zeros in the input are held out of the surface smoothing and
  restored at the floor, instead of being clamped to 1e-6 and averaged
  with their neighbours – which inflated the published initiation tail
  at ages 24–25 by a factor of 3 to 4. The clamps use
  [`which()`](https://rdrr.io/r/base/which.html) indexing, fixing a
  latent error under NA subscripts. The documentation now states that
  all output, historical years included, is the Lee–Carter
  reconstruction.

- [`estimate_initiation()`](https://stapm-platform.github.io/smktrans/reference/estimate_initiation.md)
  jumps the initiation forecast off from the last estimated year rather
  than the year before it, so the final year of data informs the trend.
  Quitting and relapse keep their existing convention.

### New features

- [`ever_smoke()`](https://stapm-platform.github.io/smktrans/reference/ever_smoke.md)
  accepts `model = "auto"`: candidate structures are scored on held-out
  survey years (holdout QAIC with the dispersion estimated once), the
  simplest model within two units of the best wins, and the winner must
  pass range and slope guard rails on its projection before acceptance.
  The scoreboard is returned with the output. Selection settings live in
  the config; explicit model names behave exactly as before.

- The base run resolves the “auto” choice and writes it to the outputs
  directory; bootstrap iterations read the file rather than
  re-selecting, so a single model structure underlies each uncertainty
  interval. A bootstrap run before the base run stops and says why.

### Validation and verification

- New validation suite in `transition_probability_validation/` (project
  repository): quitting and net initiation compared against the Smoking
  Toolkit Study over the estimated years, with the survey-side
  estimators corrected for definition, age indexing, exposure weighting,
  and – for net initiation – the cohort effect in cross-sectional age
  gradients (a cohort-followed estimator is the primary comparison).
  Relapse is verified against the envelope of its own Hawkins inputs,
  with version-skew guards. The report doubles as a regression test and
  its England results are written up in the new vignette *Validation of
  the England Transition Probabilities*.

- New holdout harness (`30_trend_holdout.R`) for adjudicating changes to
  the trend model: fit on all years but the last, predict the last,
  score weighted deviance and prevalence error against the held-out
  people, alongside a persistence baseline and a bootstrap stability
  number; plus a refit-churn measure of how much adding a year of data
  rewrites settled history. Several proposed upgrades to
  [`trend_fit()`](https://stapm-platform.github.io/smktrans/reference/trend_fit.md),
  [`quit_forecast()`](https://stapm-platform.github.io/smktrans/reference/quit_forecast.md)
  and
  [`p_smooth()`](https://stapm-platform.github.io/smktrans/reference/p_smooth.md)
  were scored by these instruments during this cycle and not adopted;
  the current implementations stand on that evidence.

### Documentation

- Main workflow vignette updated for the changes above, with diagrams of
  the model-selection contract and the net-initiation synthetic cohort.

- Model validation vignette for England added.

## smktrans 2.1.2

- Last release before this changelog was introduced. Earlier changes are
  recorded in the git history.
