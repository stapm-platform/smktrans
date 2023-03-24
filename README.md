
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smktrans: An R Package for estimating smoking state transition probabilities from cross-sectional survey data <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/YGXQ9-green.svg)](https://doi.org/10.17605/OSF.IO/YGXQ9)
<!-- badges: end -->

## The Sheffield Tobacco and Alcohol Policy Modelling Platform

This R package was developed as part of the Sheffield Tobacco and
Alcohol Policy Modelling <https://stapm.gitlab.io/> by the [School of
Health and Related Research at the University of
Sheffield](https://www.sheffield.ac.uk/scharr).

The aim of the research programme is to identify and evaluate approaches
to reducing the harm from tobacco and alcohol, with the aim of improving
commissioning in a public health policy context, i.e. providing
knowledge to support benefits achieved by policymakers.

The two objectives of the research programme are:

-   To evaluate the health and economic effects of past trends, policy
    changes or interventions that have affected alcohol consumption
    and/or tobacco smoking
-   To appraise the health and economic outcomes of potential future
    trends, changes to alcohol and/or tobacco policy or new
    interventions

The STAPM modelling is not linked to the tobacco or alcohol industry and
is conducted without industry funding or influence.

## Purpose of making the code open source

The code has been made open source for the following two reasons:

-   Transparency. Open science, allowing review and feedback to the
    project team on the code and methods used.
-   Methodology sharing. For people to understand the code and methods
    used so they might use aspects of it in their own work, e.g.,
    because they are doing something partially related that isn’t
    exactly the same job and might like to ‘dip into’ elements of this
    code for inspiration.

## Stage of testing and development

The code is actively being used in project work. It is being reviewed
and developed all the time; more tests and checks are being added.

The repository is not intended to be maintained by an open source
community wider than the development team.

## Dependencies on other internal STAPM R packages

The functions in smktrans are designed for use with:

Survey data processed by the hseclean package

-   Gillespie D, Webster L, Leeming G, Morris D, Angus C, Brennan A
    (\[YEAR\]). hseclean: An R Package for Health Survey Data Wrangling.
    R package version \[x.x.x\]. University of Sheffield.
    <https://stapm.github.io/hseclean/>. doi:
    <https://doi.org/10.17605/OSF.IO/43N7P>

Mortality data processed by the mort.tools package

-   Gillespie D, Webster L, Angus C, Brennan A (\[YEAR\]). mort.tools:
    An R Package for Processing Mortality Microdata. R package version
    \[x.x.x\]. University of Sheffield.
    <https://stapm.gitlab.io/r-packages/mort.tools/>. doi:
    <https://doi.org/10.17605/OSF.IO/WN6RH>

Relative risks of diseases related to smoking as encoded with the
tobalcepi package

-   Gillespie D, Webster L, Henney M, Brennan A, Angus C (\[YEAR\]).
    tobalcepi: An R Package for Computing the Relative Risks and
    Population Attributable Fractions of Diseases Related to Tobacco and
    Alcohol. R package version \[x.x.x\]. University of Sheffield.
    <https://stapm.github.io/tobalcepi/>. doi:
    <https://doi.org/10.17605/OSF.IO/XQ8MV>

## Data access

Detailing all of the inputs used

**Health survey data**

The smktrans package is so far designed to work with cross-sectional
survey data from the Health Survey for England (HSE) and the Scottish
Health Survey (SHeS).

To be able to download data from these surveys from the UK Data Service,
you will need to register with the UK Data Service website, which will
enable you to request access to the datasets. Instructions on how to do
this can be found
[here](https://www.ukdataservice.ac.uk/get-data/how-to-access.aspx).

**Mortality data**

The smktrans package is designed to use cause-specific death rates
stratified by sex and Index of Multiple Deprivation quintiles. Data for
the smoking related causes detailed in [this list](https://osf.io/v945r)
were supplied for England by the Office for National Statistics and for
Scotland by National Records Scotland.

Smoking state specific death rates are estimated by combining these
death rates with smoking survey data and relative risks of diseases
related to smoking.

Birth cohort survivorship data was derived from a long history of
mortality rates by age and sex downloaded from the Human Mortality
Database <https://www.mortality.org/>.

**Long term relapse to smoking by former smokers**

Annual cross-sectional survey data does not normally track individual
life-histories enough to reliably estimate the probabilities of relapse
to smoking by former smokers over many since since quitting smoking. To
address this gap in information, the smktrans package is designed to use
statistical estimates of the probabilities of smoking relapse from

-   Hawkins, J., Hollingworth, W., & Campbell, R. (2010). Long-term
    smoking relapse: A study using the British Household Panel Survey.
    Nicotine & tobacco research, 12(12), 1228-1235. doi:
    <https://doi.org/10.1093/ntr/ntq175>

To estimate relapse probabilities, former smokers in the survey data are
assigned a probability of relapse to smoking based on the match between
the data fields in the survey data and the covariates considered in the
statistical analysis.

**Information governance**

In ScHARR, all data is stored and processed according to the [ScHARR
Information Governance
Policy](https://www.sheffield.ac.uk/scharr/research/igov/policy00). No
sensitive individual-level survey or mortality data is included within
this package.

## Data checks

Data checks are brief reports that show the estimates of the smoking
state transition probabilities produced by the smktrans package.

## Code repositories

The code on Github (<https://github.com/STAPM/smktrans>) is a mirror of
the code in a private Gitlab repository where the actual development
takes place (<https://gitlab.com/stapm/r-packages/smktrans>). The code
in the Github repository is linked to a repository on the Open Science
Framework, which provides the doi for the package citation
(<https://osf.io/ygxq9/>). The OSF repository is also where the latest
versions of the smoking state transition probability estimates produced
by the applying the functions in the smktrans package are made
available.

## Citation

Gillespie D, Webster L, Leeming G, Brennan A (\[YEAR\]). smktrans: An R
Package for estimating smoking state transition probabilities from
cross-sectional survey data. R package version \[x.x.x\]. University of
Sheffield. <https://stapm.github.io/smktrans/>. doi:
<https://doi.org/10.17605/OSF.IO/YGXQ9>

## Motivation for developing the R package

The motivation for `smktrans` was to develop a set of functions to
estimate the transition probabilities among current, former and never
smoking states (i.e. initiation, quitting and relapse) from repeat
cross-sectional survey data.

The code was developed originally for the Health Survey for England, and
subsequently extended to work with the Scottish Health Survey (SHeS).
Subsequent versions will be extended to Wales.

## Functionality

What the software does in general and how it relates to data is
documented in the vignettes under “Technical Documentation”.

## Usage

The **inputs** are:

1.  Several years of annual cross-sectional survey data on smoking.  
2.  Estimates of long-term smoking relapse.  
3.  Estimates of smoking related disease risk and mortality rates.

The **processes** applied by the functions in `smktrans` implement
calculations to estimate and forecast the transition probabilities to
get them into a form that can be used in a model that simulated the
population dynamics of smoking.

The **output** of these processes are the probabilities of smoking
initiation, quitting and relapse, stratified by age, period, sex and
quintiles of the Index of Multiple Deprivation. These data can be saved
so that you don’t need to run the estimation processes in `smktrans`
each time you want to run a model that uses the estimated transition
probabilities.

## Installation

`smktrans` is publicly available via Github.

By default the user should install the latest tagged version of the
package. Otherwise, if you want to reproduce project work and know the
version of the package used, install that version.

If on a University of Sheffield managed computer, install the R, RStudio
and Rtools bundle from the Software Centre. Install Rtools - using the
[installr](https://cran.r-project.org/web/packages/installr/index.html)
package can make this easier. Then install the latest or a specified
version of `smktrans` from Github with:

``` r
#install.packages("devtools")

devtools::install_git(
  "https://github.com/stapm/hseclean.git", 
  #ref = "x.x.x",
  build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/tobalcepi.git", 
  #ref = "x.x.x",
  build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/mort.tools.git", 
  #ref = "x.x.x",
  build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/smktrans.git", 
  #ref = "x.x.x",
  build_vignettes = FALSE, quiet = TRUE)

# ref = "x.x.x" is the version to install - change to the version you want e.g. "1.2.3"
```

Or clone the package repo locally and use the ‘install and restart’
button in the Build tab of RStudio. This option is more convenient when
testing development versions.

Then load the package, and some other packages that are useful. Note
that the code within `smktrans` uses the `data.table::data.table()`
syntax.

``` r
# Load the package
library(smktrans)
library(tobalcepi)
library(hseclean)
library(mort.tools)

# Other useful packages
library(data.table)
library(dplyr) # for data manipulation and summary
library(magrittr) # for pipes
library(ggplot2) # for plotting
```
