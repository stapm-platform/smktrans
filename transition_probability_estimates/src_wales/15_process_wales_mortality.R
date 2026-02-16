# ==============================================================================
# SCRIPT: Process Mortality Data for Smoking Model (Wales)
# PURPOSE: Prepare mortality rates (mx) for:
#          1. Transition probability estimation (All-cause mortality)
#          2. STAPM Simulation Model (Cause-specific mortality)
#
# INPUT:   Aggregated mortality CSV (Age/Sex/IMD/Year/Cause)
# OUTPUT:  Two RDS files in 'intermediate_data/'
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------

library(data.table)
library(magrittr)

# Robust Project Root
tryCatch({
  root_dir <- rprojroot::find_root(rprojroot::is_rstudio_project)
}, error = function(e) {
  root_dir <- getwd()
})

# Define Paths
input_file  <- "05_input/death_rates_wales_tob_trans_2009_2022.csv"
output_path <- file.path(root_dir, "transition_probability_estimates/src_wales/intermediate_data")

if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Define Scope
# Wales data covers 2009-2022
analysis_years <- 2009:2022
# Wales analysis typically uses 16-89 (vs England 11-89)
age_range      <- 16:89


# 2. LOAD AND CLEAN DATA -------------------------------------------------------

tob_mort_data <- fread(input_file)

if("N" %in% names(tob_mort_data)) setnames(tob_mort_data, "N", "pops")
if("cause" %in% names(tob_mort_data)) setnames(tob_mort_data, "cause", "condition")

# Filter dimensions
# We filter for valid years, ages, and ensure condition is not missing
tob_mort_data <- tob_mort_data[
  year %in% analysis_years & 
    age %in% age_range & 
    !is.na(condition), 
  
  # Select and reorder columns
  .(age, sex, imd_quintile, year, condition, n_deaths, pops)
]


# 3. PREPARE DATA FOR TRANSITION ESTIMATION (ALL-CAUSE) ------------------------
# The transition model needs the TOTAL risk of death (all causes combined)

# Collapse data: Sum deaths across all causes, keep population constant
tob_mort_data_trans <- tob_mort_data[, .(
  n_deaths = sum(n_deaths, na.rm = TRUE),
  pops     = first(pops) # Population is constant across conditions for the same strata
), by = .(age, sex, imd_quintile, year)]

# Calculate Central Death Rate (mx)
tob_mort_data_trans[, mx := n_deaths / pops]

# Drop raw counts (only 'mx' is needed for transition prob)
tob_mort_data_trans[, `:=`(n_deaths = NULL, pops = NULL)]

# Sort
setorderv(tob_mort_data_trans, c("age", "year", "sex", "imd_quintile"))

# Save Output 1
outfile_trans <- file.path(output_path, "tob_mort_data_trans.rds")
saveRDS(tob_mort_data_trans, outfile_trans)

# Cleanup
rm(tob_mort_data_trans); gc()


# 4. PREPARE DATA FOR SIMULATION MODEL (CAUSE-SPECIFIC) ------------------------
# The simulation model needs specific disease risks

# Calculate cause-specific death rates
# Note: code uses variable name 'mix' for cause-specific mortality index
tob_mort_data[, mix := n_deaths / pops]

# Sort
setorderv(tob_mort_data, c("age", "year", "sex", "imd_quintile", "condition"))

# Save Output 2
outfile_cause <- file.path(output_path, "tob_mort_data_cause.rds")
saveRDS(tob_mort_data, outfile_cause)

