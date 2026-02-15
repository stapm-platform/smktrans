# ==============================================================================
# SCRIPT: Generate ABM Inputs & Calibration Targets
# PURPOSE: Process probabilistic uncertainty data for the ABM.
#          1. Outputs: Full probability tables for Init, Quit, Relapse.
#          2. Targets: Aggregated targets for calibrating the ABM.
# ==============================================================================

# 1. CONFIGURATION -------------------------------------------------------------
source("03_load_packages.R") # Ensure data.table is loaded

# Define Paths
# Using a base path ensures portability between users
base_path   <- "transition_probability_estimates/src_england/outputs/"
output_date <- format(Sys.Date(), "%Y%m%d") # Automatically uses today's date (e.g., 20251222)
version     <- "v1"

# Define Output Filenames
file_quit_probs    <- paste0(base_path, "quit_probabilities_", output_date, "_", version, ".csv")
file_init_probs    <- paste0(base_path, "init_probabilities_", output_date, "_", version, ".csv")
file_relapse_probs <- paste0(base_path, "relapse_probabilities_", output_date, "_", version, ".csv")
file_calib_targets <- paste0(base_path, "quit_probability_calibration_targets_", output_date, "_", version, ".csv")

# ABM Standard Column Names
# Defining these here ensures consistency across all files
abm_cols <- c("arrivalYear", "pAge", "pGender", "pIMDquintile")

# 2. HELPER FUNCTIONS ----------------------------------------------------------

# Process and Export Probability Tables
# filters data, renames columns to ABM standard, and saves CSV.
export_abm_table <- function(data, 
                             age_range, 
                             year_range, 
                             cols_to_keep, 
                             col_mapping, 
                             output_path) {
  
  # Filter
  dt <- data[age >= age_range[1] & age <= age_range[2] & 
               year >= year_range[1] & year <= year_range[2], 
             ..cols_to_keep] # .. syntax looks for variables in parent scope
  
  # Sort (Year -> Age -> Sex -> IMD)
  # Assuming first 4 columns are the sorting keys
  setorderv(dt, cols_to_keep[1:length(col_mapping)], rep(1, length(col_mapping)))
  
  # Rename
  setnames(dt, names(col_mapping), as.character(col_mapping))
  
  # Write
  write.csv(dt, output_path, row.names = FALSE)
  
}

# 3. LOAD DATA -----------------------------------------------------------------
# We load these once and keep them in memory to avoid redundant disk reads

raw_quit    <- readRDS(paste0(base_path, "quit_data_england_uncertainty.rds"))
raw_init    <- readRDS(paste0(base_path, "init_data_england_uncertainty.rds"))
raw_relapse <- readRDS(paste0(base_path, "relapse_data_england_uncertainty.rds"))


# 4. PART A: GENERATE ABM INPUT FILES (PROBABILITIES) --------------------------

# --- A1. Quitting ---
# Map R names (left) to ABM names (right)
quit_map <- c("year"="arrivalYear", "age"="pAge", "sex"="pGender", "imd_quintile"="pIMDquintile")

export_abm_table(
  data = raw_quit,
  age_range = c(16, 89),
  year_range = c(2011, 2040),
  cols_to_keep = c("year", "age", "sex", "imd_quintile", "p_quit"),
  col_mapping = quit_map,
  output_path = file_quit_probs
)

# --- A2. Initiation ---
export_abm_table(
  data = raw_init,
  age_range = c(16, 30),
  year_range = c(2011, 2040),
  cols_to_keep = c("year", "age", "sex", "imd_quintile", "p_start"),
  col_mapping = quit_map, # Uses same structure
  output_path = file_init_probs
)

# --- A3. Relapse ---
# Relapse has an extra column: time_since_quit
relapse_map <- c(quit_map, "time_since_quit" = "bYearsSinceQuit")

export_abm_table(
  data = raw_relapse,
  age_range = c(16, 89),
  year_range = c(2011, 2040),
  cols_to_keep = c("year", "age", "sex", "imd_quintile", "time_since_quit", "p_relapse"),
  col_mapping = relapse_map,
  output_path = file_relapse_probs
)


# 5. PART B: GENERATE CALIBRATION TARGETS (AGGREGATED) -------------------------

# Work with a copy of the loaded quit data to avoid affecting the original
calib_dt <- copy(raw_quit)

# 5.1 Filters & Categorization
calib_dt <- calib_dt[age >= 25 & age <= 74]
calib_dt <- calib_dt[year >= 2011 & year <= 2019]

# Create Age Categories
# Cuts: [-1..45) -> "25-44", [45..65) -> "45-64", [65..100) -> "65-74"
calib_dt[, age_cat := c("25-44", "45-64", "65-74")[findInterval(age, c(-1, 45, 65, 100))]]

# Create Year Categories
# Cuts: [-1..2014) -> "2011-2013", [2014..2017) -> "2014-2016", [2017..) -> "2017-2019"
calib_dt[, year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(-1, 2014, 2017, 10000))]]

# 5.2 Aggregation Tables

# --- Period 1: 2011 - 2016 ---

# Table 3: By Age/Sex (Collapsed across years 2011-2016)
t3 <- calib_dt[year <= 2016, 
               .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), 
               by = .(age_cat, sex)]
t3[, year_cat := "2011-2016"] # Explicit label for the whole period

# Table 4: By YearCat/IMD (2011-2013 and 2014-2016)
t4 <- calib_dt[year <= 2016, 
               .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), 
               by = .(year_cat, imd_quintile)]


# --- Period 2: 2017 - 2019 ---

# Table 5: By Age/Sex (Collapsed across years 2017-2019)
t5 <- calib_dt[year >= 2017, 
               .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), 
               by = .(age_cat, sex)]
# FIX: Corrected typo (was ":")
t5[, year_cat := "2017-2019"] 

# Table 6: By YearCat/IMD (2017-2019)
t6 <- calib_dt[year >= 2017, 
               .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), 
               by = .(year_cat, imd_quintile)]


# 5.3 Combine & Clean
# Bind all tables; 'fill=TRUE' handles the mismatched columns (e.g. t3 lacks IMD)
data_t <- rbindlist(list(t3, t4, t5, t6), use.names = TRUE, fill = TRUE)

# Fill Missing Values with "All" 
# This makes it explicit that these rows apply to All Ages or All IMDs
data_t[is.na(age_cat), age_cat := "All"]
data_t[is.na(sex), sex := "All"]
data_t[is.na(imd_quintile), imd_quintile := "All"]
# year_cat is never NA based on the logic above, but good to check if debugging.

# Rename to ABM format
setnames(data_t, 
         old = c("year_cat", "age_cat", "sex", "imd_quintile"), 
         new = c("arrivalYearCategorical", "pAgeCategorical", "pGender", "pIMDquintile"))

# Reorder columns
final_cols <- c("arrivalYearCategorical", "pIMDquintile", "pGender", "pAgeCategorical", "p_quit", "p_quit_var")
setcolorder(data_t, final_cols)

# 5.4 Save
write.csv(data_t, file_calib_targets, row.names = FALSE)


