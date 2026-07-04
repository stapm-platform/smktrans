
# The aim of this code is to install the required packages

# Update R ad R studio
#installr::updateR()

installed_devtools <- "devtools" %in% rownames(installed.packages())
if (installed_devtools == FALSE) {
  install.packages("devtools", lib = project_lib)
}

###########################
# CRAN packages installation

packages <- c("crosstalk", "rprojroot", "DT", "matrixStats", "DiagrammeR", "data.table", "ggplot2", "devtools",
              "getPass", "git2r", "cowplot", "readxl", "gt", "gtsummary",
              "knitr", "stringr", "here", "magrittr",
              "RColorBrewer", "testthat",
              "DirichletReg", "fitdistrplus",
              "fastDummies",  "flextable",  "bookdown", "viridis",
              "rmarkdown", "TTR", "ids", "boot",
              "VGAM", "praise", "parallel", "readr", "cowsay", "snowfall",
              "bit64", "Rdpack", "lifecycle", "crayon", "writexl",
              "Rfast", "fastmatch", "dplyr", "plyr",
              "openxlsx", "demography", "forecast", "raster", "mice",
              "Hmisc", "waldo", "gitcreds", "nnet", "quantmod", "Matrix",
              "survival", "codetools", "nlme", "tibble", "ggthemes",    
              "foreign", "shiny", "tidyverse", "gt", "extrafont", "forcats", 
              "snakecase", "paletteer", "scales", "ggtext", "car",
              "plotly", "gganimate", "ggpubr", "seecolor", "survey", "distill")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  renv::install(packages[!installed_packages])
}

renv::install("dvmisc@1.1.4") # version removed from CRAN - check all packages that import this

###########################
#### STAPM packages #######

# To install STAPM packages from the STAPM GitHub organisation, you will first need a 
# GitHub PAT (personal access token) and a .Renviron file setup locally on your machine. 
# See the README for guidance on how to set this up. 

# Once this is done, you can run the code below to install the STAPM R packages 
# (defaulting to the most recent version)

# Define STAPM GitHub packages with exact versions
# renv will automatically use the GITHUB_PAT from your .Renviron file
# Ensure you have set this up before running this part of the installation: 

github_packages <- c(
  "stapm-platform/stapmr@1.11.11",
  "stapm-platform/tobalcepi@1.7.4",
  "stapm-platform/hseclean@1.14.8",
  #"stapm-platform/mort.tools@1.6.0",
  "stapm-platform/smktrans@2.0.0"#,
  #"stapm-platform/toolkitr@0.5.0"
)

# Install GitHub packages
renv::install(github_packages)

renv::install("stapm-platform/stapmr@1.11.10")
renv::install("stapm-platform/tobalcepi@1.7.4")

# 5. Snapshot the environment to create the renv.lock file
renv::snapshot()
