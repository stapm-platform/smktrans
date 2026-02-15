
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
              "Rfast", "dvmisc", "fastmatch", "dplyr", "plyr",
              "openxlsx", "demography", "forecast", "raster", "mice",
              "Hmisc", "waldo", "gitcreds", "nnet", "quantmod", "Matrix",
              "survival", "codetools", "nlme", "tibble", "ggthemes",    
              "foreign", "shiny", "tidyverse", "gt", "extrafont", "forcats", 
              "snakecase", "paletteer", "scales", "ggtext", "car",
              "plotly", "gganimate", "ggpubr", "seecolor", "survey", "distill")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], lib = project_lib)
}

###########################
#### STAPM packages #######

# To install STAPM packages from the STAPM GitHub organisation, you will first need a 
# GitHub PAT (personal access token) and a .Renviron file setup locally on your machine. 
# See the README for guidance on how to set this up. 

# Once this is done, you can run the code below to install the STAPM R packages 
# (defaulting to the most recent version)

# Fetch environment variable "GITHUB_PAT" to local variable
token <- Sys.getenv("GITHUB_PAT")

versions <- c("1.11.2", # stapmr
              "1.7.4",  # tobalcepi
              "1.14.5", # hseclean 
              "1.6.0", # mort.tools
              "1.5.2", # smktrans
              "0.5.0") # toolkitr


######################################################################
####### Install package versions for the chosen version of the model #

devtools::install_github(
  "https://github.com/stapm-platform/stapmr.git",
  ref = versions[1],
  auth_token = token,
  build_vignettes = FALSE, quiet = FALSE,
  force = TRUE)

devtools::install_git(
  "https://github.com/stapm-platform/tobalcepi.git", 
  ref = versions[2],
  build_vignettes = FALSE, quiet = FALSE)

devtools::install_git(
  "https://github.com/stapm-platform/hseclean.git", 
  ref = versions[3],
  build_vignettes = FALSE, quiet = FALSE)

devtools::install_git(
  "https://github.com/stapm-platform/mort.tools.git", 
  ref = versions[4],
  build_vignettes = FALSE, quiet = FALSE)

devtools::install_git(
  "https://github.com/stapm-platform/smktrans.git", 
  #ref = versions[5],
  build_vignettes = FALSE, quiet = FALSE)

devtools::install_github(
  "https://github.com/stapm-platform/toolkitr.git",
  ref = versions[6],
  auth_token = token,
  build_vignettes = FALSE, quiet = FALSE)


