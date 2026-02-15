
# The aim of this code is to run the scripts that prepare inputs for the estimates of smoking transition probabilities for England

# Requires access to the location where the HSE data is stored
# defaults relate to the internal University of Sheffield setup

# # Point to the location of the X drive
root_dir <- "X:/"

path <- "transition_probability_estimates/src_england/"

source(paste0(path, "10_clean_hse.R"))
source(paste0(path, "15_prep_mortality.R"))
source(paste0(path, "17_smoking_prevalence_summary_table.R"))
