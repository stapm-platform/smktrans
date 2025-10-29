
# The aim of this code is to run the scripts that prepare inputs for the estimates of smoking transition probabilities for England

# Pre-requisites
  # connection to the University of Sheffield VPN
  # Access to the X drive folders PR_Consumption_TA and PR_STAPM

# # Point to the location of the X drive
root_dir <- "X:/"
# #root_dir <- "/Volumes/Shared/"

path <- "transition_probability_estimates/src_england/"

source(paste0(path, "10_clean_hse.R"))
source(paste0(path, "15_prep_mortality.R"))
source(paste0(path, "17_smoking_prevalence_summary_table.R"))
