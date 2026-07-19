# Monotone smooth of a cumulative curve over age

Internal helper for p_dense. Fits a smoothing spline to the cumulative
probability of ever smoking over age within one cohort/sex/IMD group,
clamps it to \[0, 1\], then forces it to be monotone non-decreasing.

## Usage

``` r
smooth_cdf(p, a, df = 6)
```

## Arguments

- p:

  Numeric vector - cumulative probabilities for one group.

- a:

  Numeric vector - ages, same length as p.

- df:

  Numeric - degrees of freedom for the spline. Higher follows the raw
  curve more closely. 6 over a ~20 year age range keeps the shape of the
  ogive without chasing the steps.

## Value

Numeric vector the same length as p.

## Details

The cumulative curve coming out of init_adj is a step function. init_est
builds it from a weighted count of the ages at which people report
starting, so an age at which nobody in that cohort happened to report
starting gets a hazard of exactly zero and the curve goes flat.
Differencing a step function gives a density that is zero roughly half
the time and spiky the rest, which is why p_smooth was then blanking and
interpolating the zeros.

Smoothing the cumulative curve first and differencing afterwards avoids
all of that. The curve is the thing we actually estimated, the density
is derived from it, so the curve is the right thing to smooth. Because
cummax makes the result monotone, the differenced density is
non-negative by construction and the clamps in p_dense become a
formality rather than load-bearing.
