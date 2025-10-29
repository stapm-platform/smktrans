
# install.packages("data.table")
# install.packages("magrittr")

# install.packages("git2r")
# install.packages("getPass")

library(data.table)
library(magrittr)
library(ggplot2)

# Use the STAPM R packages designed to support cleaning of the Health Survey for England data
# and of the Toolkit data
#devtools::install_git("https://github.com/stapm/hseclean.git", ref = "1.11.3", build_vignettes = FALSE)

# devtools::install_git("https://gitlab.com/stapm/r-packages/toolkitr.git", 
#                       credentials = git2r::cred_user_pass("dosgillespie", getPass::getPass()),
#                       ref = "0.5.0", build_vignettes = FALSE)

library(hseclean)
library(toolkitr)

