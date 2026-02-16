
# smktrans <img src="man/figures/logo.png" align="right" style="padding-left:10px;background-color:white;" width="120" />

### Smoking State Transition Probability Estimation

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/YGXQ9-green.svg)](https://doi.org/10.17605/OSF.IO/YGXQ9)

------------------------------------------------------------------------

## Overview

The **smktrans** R package provides a methodological framework and
automated workflow for estimating the probabilities of individuals
transitioning between smoking states (initiation, quitting, and relapse)
using repeat cross-sectional survey data.

It is a core component of the [Sheffield Tobacco and Alcohol Policy
Modelling (STAPM) platform](https://stapm-platform.github.io/),
specifically designed to generate transition parameters for the
Sheffield Tobacco Policy Model (STPM).

### Key Methodological Capabilities

- **Cohort Reconstruction:** Methods to derive longitudinal-style life
  histories from cross-sectional survey data.

- **The “Quit Solver”:** A mathematical approach to estimating
  age-specific quit rates by reconciling changes in smoking prevalence
  with known initiation, relapse and mortality rates.

- **Long-term Forecasting:** Tools to project transition probabilities
  over extended time horizons (e.g., to 2040) to support policy impact
  assessments.

- **Data Stratification:** Estimation by age, sex, and Index of Multiple
  Deprivation (IMD) quintiles.

------------------------------------------------------------------------

## Documentation and Vignettes

For comprehensive guides on package usage and detailed methodology,
please refer to the
**[Articles](https://stapm-platform.github.io/smktrans/articles/index.html)**
section of the package website.

Key documentation includes:

- **Package Workflow:** A step-by-step guide to the estimation process.

- **Methodological Documentation:** Technical details on the “Quit
  Solver” and cohort reconstruction.

- **Accessing Estimates:** Presentation of the pre-calculated estimates
  for England, Scotland, and Wales.

------------------------------------------------------------------------

## Installation

The package can be installed from GitHub. Please note that `smktrans`
depends on several internal STAPM packages for data processing and
mortality adjustment.

``` r
# install.packages("remotes")

# 1. Install STAPM dependencies
remotes::install_github("stapm-platform/hseclean")
remotes::install_github("stapm-platform/tobalcepi")
remotes::install_github("stapm-platform/mort.tools")

# 2. Install smktrans
remotes::install_github("stapm-platform/smktrans")
```

## Citation

If you use the **smktrans** estimates in your research, **please cite
our peer-reviewed modelling papers**. These publications validate the
use of these estimates in policy appraisal contexts:

> Chen RKL, Morris D, Angus C, Gilmore A, Hiscock R, Holmes J, Langley
> TE, Pryce R, Wilson LB, Brennan A, Gillespie D (2026). Reducing the
> exceptional affordability of hand-rolling tobacco using tax
> escalators: a health and economic impact modelling study for England.
> *Tobacco Control*. [DOI:
> 10.1136/tc-2025-059670](https://doi.org/10.1136/tc-2025-059670)

> Gillespie D, Morris D, Angus C, Wilson L, Chen RKL, Leeming G, Holmes
> J, Brennan A (2025). Model-based appraisal of the potential effects of
> minimum pricing for tobacco in Scotland. *Tobacco Control*. [DOI:
> 10.1136/tc-2024-059252](https://doi.org/10.1136/tc-2024-059252)

To cite the **smktrans** software package specifically:

> Gillespie, D., and Brennan, A. (2026). **smktrans: An R Package for
> estimating smoking state transition probabilities (v2.0.0).**
> University of Sheffield. <https://doi.org/10.17605/OSF.IO/YGXQ9>

To cite the full technical documentation for the underlying model:

> Gillespie, D. & Brennan, A. (Year). The Sheffield Tobacco Policy Model
> (STPM): full technical documentation. Documentation version number
> \[x.x.x\]. University of Sheffield. [DOI:
> 10.17605/OSF.IO/FR7WN](https://doi.org/10.17605/OSF.IO/FR7WN)

*(Note: The technical documentation is a living document. Please cite
the year and version of the report you used.)*

## Authors & Acknowledgements

### Authors

- **Duncan Gillespie** - *Author, Maintainer*  
- **Alan Brennan** - *Author*

### Contributors

- **Laura Webster** - *Original code and methodology*  
- **Grace Leeming** - *Code extension to Scotland*  
- **Robin Purshouse** - *Methodological review and feedback*  
- **Hazel Squires** - *Methodological review and feedback*  
- **Shangshang Gu** - *Methodological review and feedback*

### Acknowledgements

We would also like to thank **Magdalena Opazo-Breton** for her review of
early documentation and methodological feedback.

------------------------------------------------------------------------

## Contact

For methodological queries or to report technical issues, please use the
[GitHub Issue
Tracker](https://github.com/stapm-platform/smktrans/issues) or contact
**Dr Duncan Gillespie** (<duncan.gillespie@sheffield.ac.uk>).

------------------------------------------------------------------------

## Contact

For methodological queries or to report technical issues, please use the
[GitHub Issue
Tracker](https://github.com/stapm-platform/smktrans/issues) or contact
**Dr Duncan Gillespie** (<duncan.gillespie@sheffield.ac.uk>).
