# Continuous smoking abstinence rates over time

Mean continuous abstinence rates up to 52 weeks from the start of a quit
attempt in clinical trials can be modelled with simple power functions
for placebo, nicotine replacement therapy, bupropion and varenicline.
This lets us predict abstinence at any point up to 52 weeks from any
other point.

## Usage

``` r
SmkContAbst(
  treatment = c("placebo", "varenicline", "bupropion", "nrt"),
  weeks = 4
)
```

## Arguments

- treatment:

  Character - the pharmacological support used for the quit attempt. One
  of "placebo", "varenicline", "bupropion", "nrt".

- weeks:

  Numeric vector - weeks since the quit attempt started. The start of
  the attempt is week 0. Maximum 52.

## Value

A vector of probabilities that the person is still abstinent.

## Details

The equations are taken from Supplementary Figure 2 of:

Jackson SE, Kotz D, West R, Brown J (2019). Moderators of real-world
effectiveness of smoking cessation aids: a population study. Addiction
114(9):1627-1638.
[doi:10.1111/add.14549](https://doi.org/10.1111/add.14549)

This function used to live in stapmr. It has been moved here because
data-raw/Relapse_Hawkins2010/prep_Hawkins_relapse.R needs it to build
hawkins_relapse, and stapmr is a private package, so anyone outside the
group could not rebuild the package data. There is nothing in the
function that needs stapmr: it is four curves and no dependencies.

Note the curves are only fitted out to 52 weeks. Do not ask it for
anything beyond that.

## Examples

``` r

if (FALSE) { # \dontrun{

# Continuous abstinence over the first four weeks
SmkContAbst("placebo", 0:4)
# 1.0000000 0.4129423 0.3152202 0.2691612 0.2406239

# Relative effectiveness of quit aids at 4 weeks
SmkContAbst("varenicline", 4) / SmkContAbst("placebo", 4)
# 2.279061

# For 1000 four-week quits on placebo, how many twelve-week quits
1000 * (SmkContAbst("placebo", 12) / SmkContAbst("placebo", 4))
# 651.8131

} # }
```
