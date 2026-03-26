# ==============================================================================
# Script: build_renv_lock.R
# Purpose: Initialize renv, install all dependencies, and create the lockfile.
# Note: only need to run this once to set up the project. 
# ==============================================================================

# 1. Install renv if you don't have it
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# 2. Initialize the project (this creates the renv folder)
#  use bare = TRUE to prevent it from guessing; we will explicitly tell it what to install.
renv::init(bare = TRUE)

# 3. Define all CRAN packages
cran_packages <- c(
  "crosstalk", "rprojroot", "DT", "matrixStats", "DiagrammeR", "data.table", 
  "ggplot2", "devtools", "getPass", "git2r", "cowplot", "readxl", "gt", 
  "gtsummary", "knitr", "stringr", "here", "magrittr", "RColorBrewer", 
  "testthat", "DirichletReg", "fitdistrplus", "fastDummies", "flextable", 
  "bookdown", "viridis", "rmarkdown", "TTR", "ids", "boot", "VGAM", "praise", 
  "parallel", "readr", "cowsay", "snowfall", "bit64", "Rdpack", "lifecycle", 
  "crayon", "writexl", "Rfast", "dvmisc", "fastmatch", "dplyr", "plyr", 
  "openxlsx", "demography", "forecast", "raster", "mice", "Hmisc", "waldo", 
  "gitcreds", "nnet", "quantmod", "Matrix", "survival", "codetools", "nlme", 
  "tibble", "ggthemes", "foreign", "shiny", "tidyverse", "extrafont", 
  "forcats", "snakecase", "paletteer", "scales", "ggtext", "car", "plotly", 
  "gganimate", "ggpubr", "seecolor", "survey", "distill"
)

# Install CRAN packages
renv::install(cran_packages)

# 4. Define STAPM GitHub packages with exact versions
# renv will automatically use the GITHUB_PAT from your .Renviron file
github_packages <- c(
  "stapm-platform/stapmr@1.11.2",
  "stapm-platform/tobalcepi@1.7.4",
  "stapm-platform/hseclean@1.14.5",
  "stapm-platform/mort.tools@1.6.0",
  "stapm-platform/smktrans@2.0.0", 
  "stapm-platform/toolkitr@0.5.0"
)

# Install GitHub packages
renv::install(github_packages)

# 5. Snapshot the environment to create the renv.lock file
renv::snapshot()



