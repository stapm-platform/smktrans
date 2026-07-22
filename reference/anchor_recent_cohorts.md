# Anchor extrapolated ever-smoking targets on a youth survey series

Replaces the ever_smoke() targets for cohorts beyond the trend model's
own data with values anchored on an external youth smoking series,
linked onto the target scale by a factor estimated on the cohorts both
sources observe. Cohorts within the trend model's data are returned
unchanged.

## Usage

``` r
anchor_recent_cohorts(
  ever_smoke_data,
  youth_anchor_data,
  ref_age,
  anchor_age_centre,
  taper_cohorts,
  quiet = FALSE
)
```

## Arguments

- ever_smoke_data:

  List returned by ever_smoke().

- youth_anchor_data:

  Data table with columns survey_year and p_ever_smoked (a fraction),
  and optionally sex. If sex is present its values must exactly match
  the sexes in the trend model output; if absent, a single link and
  ratio path is estimated and applied to all sexes.

- ref_age:

  Integer - the calibration age used by init_adj.

- anchor_age_centre:

  Integer - the age the youth series represents: the single age
  surveyed, or a representative age for a band (13 for an 11-15 band).

- taper_cohorts:

  Integer - number of cohorts over which the handover from trend to
  anchor is blended. 0 disables the blend.

- quiet:

  Logical - suppress messages.

## Details

A youth survey in year t reporting the proportion who have ever smoked
is read as an observation on the cohort born in t - anchor_age_centre.
The series is linearly interpolated onto integer cohorts, so gap years
in a biennial survey are covered. The link factor is the mean of target
/ youth value over the overlap cohorts (those at or before the last
cohort the trend model observed at ref_age), estimated separately by sex
when the series has a sex column, and absorbs both later initiation
between the survey age and ref_age and differential recall between the
two instruments. Anchored targets are applied as a cohort-level ratio to
the fitted trend, so gradients across strata within a cohort pass
through. The ratio is 1 up to the last trend-supported cohort, blends
linearly over taper_cohorts, follows the linked youth series, and is
held at its final value for cohorts born after the last youth survey.

Assumed: the link factor is stable across cohorts, and (for a series
covering an age band rather than a single age) that anchor_age_centre is
a fixed representative age for the band. In the bootstrap the youth
series is fixed external data; the link factor is re-estimated each
iteration because the trend model is refit.

The link factor(s) and ratio path are attached to the returned list as
\$anchor.
