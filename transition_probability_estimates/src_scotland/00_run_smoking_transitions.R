
# The aim of this code is to run the scripts that prepare inputs for the smoking transition probabilities for Scotland

# Pre-requisites
  # connection to the University of Sheffield VPN
  # Access to the X drive folders PR_Consumption_TA and PR_STAPM

# # Point to the location of the X drive
#root_dir <- "X:/"
# #root_dir <- "/Volumes/Shared/"

#path <- "src_scotland/"

source(paste0(path, "10_clean_shes.R"))
source(paste0(path, "15_prep_mortality.R"))
