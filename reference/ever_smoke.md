# Summarise and project trends in ever-smoking

Fits a weighted GLM (quasibinomial) to the trend in ever-smoking at age
25-34. This provides the "target" level for the Holford adjustment.

## Usage

``` r
ever_smoke(
  data,
  time_horizon = 2100,
  num_bins = 7,
  model = "model2",
  min_age = 15,
  min_year = 2003,
  age_cats = c("25-34"),
  auto_holdout_bins = 2,
  auto_tie_margin = 2,
  auto_floor = 0.02,
  auto_ceiling = 0.98,
  auto_max_slope_mult = 2
)
```

## Arguments

- data:

  Data table of individual characteristics.

- time_horizon:

  Integer - the last year for projection.

- num_bins:

  Integer - bins for the period trend to reduce noise.

- model:

  Character - Model specification (interaction terms), or "auto" to
  select the structure as described above.

- min_age:

  Integer - youngest age for prediction.

- min_year:

  Integer - first year of survey data.

- age_cats:

  Character vector - age category for reference (e.g., "25-34").

- auto_holdout_bins:

  Integer - how many of the most recent year bins to hold out when
  scoring candidates (model = "auto" only).

- auto_tie_margin:

  Numeric - QAIC margin within which a simpler model is preferred to the
  best-scoring one (model = "auto" only). 2 is the conventional "no real
  difference" threshold.

- auto_floor, auto_ceiling:

  Numeric - the projected proportion for every stratum must stay inside
  this range over the whole projection.

- auto_max_slope_mult:

  Numeric - cap on any stratum's logit slope as a multiple of the
  main-effects model's common slope.

## Details

The candidate models differ only in their interactions. The ones
involving year_bin are the ones to be careful with: year_bin enters
linearly on the logit scale, and the predictions run from before the
data starts out to time_horizon, which is a twenty-odd year
extrapolation from about sixteen years of observation. A sex or IMD
difference in \*level\* is safe under that extrapolation; a difference
in \*slope\* compounds with every projected year. So the risk in a
richer model is not the extra parameters, it is what those parameters do
a long way outside the data.

Setting model = "auto" picks the structure from the data,
conservatively. Three stages:

1\. Score every candidate on held-out time. The last auto_holdout_bins
of the binned years are set aside, each model is fitted to the earlier
bins and scored by weighted deviance on the held-out people. That scores
the thing the projection actually does - predict forward along the
fitted trend - rather than in-sample fit, which interactions can always
improve.

2\. Prefer simplicity. Candidates are compared on a holdout QAIC: the
held-out deviance divided by the dispersion (estimated once, from the
richest model on the training years, so every candidate is scaled the
same), plus two per parameter. Models within auto_tie_margin of the best
are treated as ties and the one with the fewest coefficients wins - the
usual reading of differences under 2 on that scale. A relative tolerance
does not work here: the deviance is dominated by irreducible
person-level noise, so even a real slope difference only moves it by a
fraction of a percent, and any percentage margin hands the choice to the
plain model every time regardless of the data.

3\. Guard the projection. The winner is refitted to all the data and its
projection to time_horizon is checked: every stratum must stay inside
\[auto_floor, auto_ceiling\], and no stratum's logit slope may exceed
auto_max_slope_mult times the common slope from the main-effects model.
A model that fails falls back to the next admissible candidate, and it
says so. If nothing survives, the main-effects model is used; if even
that fails the range check, that is a data problem and the function
stops rather than projecting it.

Passing an explicit "model1" to "model8" behaves exactly as it always
has.
