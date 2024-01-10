
# The aim of this script is to load the packages required
# to run the simulation and process the results

#file.edit(file.path("~", ".Rprofile")) # edit .Rprofile in HOME
#file.edit(".Rprofile") # edit project specific .Rprofile

#project_lib <- "X:/ScHARR/PR_STAPM/Data/smoking_transition_prob_estimation/R_packages"
project_lib <- "C:/Users/cm1dog/Documents/libloc"


.libPaths(project_lib)

# CRAN packages
library(data.table)
library(here)
library(stringr)
library(flextable)
library(magrittr)
library(plyr)
library(readxl)
library(openxlsx)
library(testthat)
library(cowplot)
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(survey)
#library(svrep) # https://cran.r-project.org/web/packages/svrep/vignettes/bootstrap-replicates.html

# STAPM packages
library(stapmr)
library(tobalcepi)
library(hseclean)
library(smktrans)
library(mort.tools)



#library(mort.tools)


#data.table::setDTthreads(1)

options(warn = 1)


