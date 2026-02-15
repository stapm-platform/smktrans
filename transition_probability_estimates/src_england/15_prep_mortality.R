# ==============================================================================
# SCRIPT: Process Mortality Data for Smoking Model
# PURPOSE: Prepare mortality rates (mx) for:
#          1. Transition probability estimation (All-cause mortality)
#          2. STAPM Simulation Model (Cause-specific mortality)
#
# INPUT:   Aggregated mortality CSV (Age/Sex/IMD/Year/Cause)
# OUTPUT:  Two RDS files in 'intermediate_data/'
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------
source("03_load_packages.R") 
# library(data.table)

# Define file paths
# input_file: The raw mortality counts and population sizes
input_file <- "05_input/tob_death_rates_england_national_2020-09-29_mort.tools_1.1.0.csv"
root_dir <- "X:/"
path <- "transition_probability_estimates/src_england/"

# Define Scope
analysis_years <- 2003:2018
age_range      <- 11:89

# 2. LOAD AND CLEAN DATA -------------------------------------------------------

tob_mort_data <- fread(input_file)

# Filter dimensions and select columns
# We filter for valid years, ages, and ensure 'cause' is not missing
tob_mort_data <- tob_mort_data[
  year %in% analysis_years & 
    age %in% age_range & 
    !is.na(cause), 
  
  # Select columns
  .(age, sex, imd_quintile, year, condition = cause, n_deaths, pops)
]

# 3. PREPARE DATA FOR TRANSITION ESTIMATION (ALL-CAUSE) ------------------------
# The transition model needs the TOTAL risk of death (all causes combined)
# to calculate the probability of surviving to the next year.

# Collapse data: Sum deaths across all causes, keep population constant
# Note: 'pops' is the population count for that age/sex/imd/year stratum.
# It is repeated for each cause, so we take the first value (unique would fail if duplicates exist).
tob_mort_data_trans <- tob_mort_data[, .(
  n_deaths = sum(n_deaths, na.rm = TRUE),
  pops     = first(pops) 
), by = .(age, sex, imd_quintile, year)]

# Calculate Central Death Rate (mx)
tob_mort_data_trans[, mx := n_deaths / pops]

# Drop raw counts as we only need the rate 'mx'
tob_mort_data_trans[, `:=`(n_deaths = NULL, pops = NULL)]

# Sort for consistent time-series processing
setorderv(tob_mort_data_trans, c("age", "year", "sex", "imd_quintile"))

# Save Output 1
output_trans <- paste0(path, "intermediate_data/tob_mort_data_trans.rds")
saveRDS(tob_mort_data_trans, output_trans)

# Clean up memory
rm(tob_mort_data_trans); gc()

# 4. PREPARE DATA FOR SIMULATION MODEL (CAUSE-SPECIFIC) ------------------------
# The simulation model needs specific disease risks (e.g., Lung Cancer rates)
# so we keep the 'condition' column.

# Calculate cause-specific death rates
tob_mort_data[, mix := n_deaths / pops]

# Save Output 2
output_cause <- paste0(path, "intermediate_data/tob_mort_data_cause.rds")
saveRDS(tob_mort_data, output_cause)

rm(tob_mort_data); gc()



