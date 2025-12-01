
# The aim of this script is to load the packages required
# to run the simulation and process the results

#file.edit(file.path("~", ".Rprofile")) # edit .Rprofile in HOME
#file.edit(".Rprofile") # edit project specific .Rprofile 

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

# STAPM packages
library(stapmr)
library(tobalcepi)
library(hseclean)
library(smktrans)
library(mort.tools)
library(toolkitr)


#data.table::setDTthreads(1)

options(warn = 1)
#memory.limit(size = 1e8)
