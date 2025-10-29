
# The aim of this code is to install the required packages

# new users will not need to reinstall packages if they have access to the project lib below

# CRAN packages

# project_lib <- "X:/ScHARR/PR_STAPM/Data/smoking_transition_prob_estimation/R_packages"
project_lib <- "C:/Users/cm1dog/Documents/libloc"

.libPaths(project_lib)

#update.packages(project_lib)

# Package names
packages <- c("DiagrammeR",
              "data.table",
              "ggplot2",
              "cowplot",
              "readxl",
              "knitr",
              "stringr",
              "here",
              "magrittr",
              "RColorBrewer",
              "git2r",
              "getPass",
              "devtools",
              "flextable",
              "bookdown",
              "viridis",
              "rmarkdown",
              "TTR",
              "boot",
              "VGAM",
              "readr",
              "writexl",
              "Rfast",
              "dvmisc",
              "fastmatch",
              "dplyr",
              "plyr",
              "openxlsx",
              "demography",
              "forecast",
              "raster",
              "mice",
              "Hmisc",
              "nnet",
              "ggthemes",
              "survey")
              #"svrep")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  #install.packages(packages[!installed_packages], type = "source", INSTALL_opts = "--byte-compile")
  install.packages(packages[!installed_packages], lib = project_lib)
  #install.packages(packages[!installed_packages])
}

#update.packages(lib.loc = project_lib, lib = project_lib)

# Packages loading
#invisible(lapply(packages, library, character.only = TRUE))

###########################
# STAPM packages

# The versions specified here indicate the versions that the code in this repo was last tested with

# You will need to replace "dosgillespie" with your own Gitlab user name

uname <- "dosgillespie"

#devtools::install_git(
#  url = "https://gitlab.com/stapm/r-packages/hseclean.git",
#  credentials = git2r::cred_user_pass(uname, getPass::getPass()),
#  ref = "1.8.4", build_vignettes = FALSE, lib = project_lib)

#devtools::install_git(
#  "https://github.com/stapm/hseclean.git",
#  build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  url = "https://gitlab.com/stapm/r-packages/hseclean.git",
  credentials = git2r::cred_user_pass(uname, getPass::getPass()),
  ref = "1.12.1", build_vignettes = FALSE, quiet = TRUE)

install.packages("C:/Users/cm1dog/Documents/hseclean_1.12.1.zip", repos=NULL)

devtools::install_git(
  url = "https://gitlab.com/stapm/r-packages/stapmr.git",
  credentials = git2r::cred_user_pass(uname, getPass::getPass()),
  ref = "1.9.4", build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/tobalcepi.git",
  build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/mort.tools.git",
  build_vignettes = FALSE, quiet = TRUE)

# devtools::install_git(
#   "https://github.com/stapm/smktrans.git",
#   build_vignettes = FALSE, quiet = TRUE)




