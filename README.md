
# smktrans <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="120" />

### An R Package and Workflow for estimating smoking state transition probabilities

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/YGXQ9-green.svg)](https://doi.org/10.17605/OSF.IO/YGXQ9)
—

## 📖 Overview

**smktrans** is a research compendium structured as an R package. It
serves two distinct purposes: 1. **Toolbox:** It provides a set of R
functions to estimate transition probabilities among current, former,
and never smoking states (initiation, quitting, and relapse) from repeat
cross-sectional survey data. 2. **Workflow:** It contains the specific
analytical workflow used to produce the official estimates for the
Sheffield Tobacco Policy Model (STPM).

This package is part of the **Sheffield Tobacco and Alcohol Policy
Modelling (STAPM)** platform developed by the [School of Health and
Related Research at the University of
Sheffield](https://www.sheffield.ac.uk/scharr).

------------------------------------------------------------------------

## 📦 Package Structure: A “Vertical” Approach

While `smktrans` can be installed and used like a standard R package, it
is designed to be “vertical” — meaning the package encompasses both the
tools *and* the analysis.

The estimates produced by this workflow are probabilities of **smoking
initiation**, **quitting**, and **relapse**, stratified by: \* Age \*
Period \* Sex \* Index of Multiple Deprivation (IMD) quintiles

### Where to find the analysis

The workflow used to produce these estimates, including slides and plots
of trends for England and Scotland, can be found in the repository here:
[📂
transition_probability_estimates](https://github.com/stapm-platform/smktrans/tree/master/transition_probability_estimates)

### Open Science & Estimates

We have made this code open source to ensure transparency, share
methodology, and allow others to utilize our pre-calculated estimates.
\* **England Estimates:** [Download via OSF](https://osf.io/zy9q8) \*
**Scotland Estimates:** [Download via OSF](https://osf.io/wtz7b) \*
**Wales Estimates:** *\[Placeholder: Estimates for Wales are currently
under development and will be linked here once available.\]*

------------------------------------------------------------------------

## 🛠 Installation

`smktrans` relies on several internal STAPM packages (`hseclean`,
`tobalcepi`, `mort.tools`). You can install the package directly from
GitHub.

If you are on a University of Sheffield managed computer, we recommend
installing Rtools via the
[installr](https://cran.r-project.org/web/packages/installr/index.html)
package first.

``` r
# install.packages("devtools")

# 1. Install dependencies
devtools::install_git("[https://github.com/stapm/hseclean.git](https://github.com/stapm/hseclean.git)", build_vignettes = FALSE, quiet = TRUE)
devtools::install_git("[https://github.com/stapm/tobalcepi.git](https://github.com/stapm/tobalcepi.git)", build_vignettes = FALSE, quiet = TRUE)
devtools::install_git("[https://github.com/stapm/mort.tools.git](https://github.com/stapm/mort.tools.git)", build_vignettes = FALSE, quiet = TRUE)

# 2. Install smktrans
devtools::install_git("[https://github.com/stapm-platform/smktrans.git](https://github.com/stapm-platform/smktrans.git)", build_vignettes = FALSE, quiet = TRUE)

# Note: You can add ref = "x.x.x" to any of the above to install a specific version.
```

### Usage

``` r
library(smktrans)
library(tobalcepi)
library(hseclean)
library(mort.tools)

# Other useful packages for the workflow
library(data.table)
library(dplyr)
library(ggplot2)
```

------------------------------------------------------------------------

## 📊 Data Sources & Inputs

The functions in `smktrans` are designed to process specific data
inputs.

| Input Type | Source / Description | Access |
|:---|:---|:---|
| **Health Survey Data** | Cross-sectional data from the **Health Survey for England (HSE)** and **Scottish Health Survey (SHeS)**. Work to include the **National Survey for Wales** is ongoing. | Requires registration with the [UK Data Service](https://www.ukdataservice.ac.uk/get-data/how-to-access.aspx). |
| **Mortality Data** | Cause-specific death rates stratified by sex and IMD. | Supplied by ONS (England/Wales) and NRS (Scotland). See [cause list](https://osf.io/v945r). |
| **Survivorship** | Birth cohort survivorship derived from historical mortality rates. | [Human Mortality Database](https://www.mortality.org/). |
| **Relapse Data** | Statistical estimates of long-term relapse probabilities ( |  |
