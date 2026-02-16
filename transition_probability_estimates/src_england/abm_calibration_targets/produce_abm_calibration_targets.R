# ==============================================================================
# SCRIPT: Generate ABM Inputs & Calibration Targets
# PURPOSE: Process probabilistic uncertainty data for the ABM.
#          1. Outputs: Full probability tables for Init, Quit, Relapse.
#          2. Targets: Aggregated targets for calibrating the ABM.
# ==============================================================================

# 1. CONFIGURATION -------------------------------------------------------------
source("03_load_packages.R")

# Define Paths
base_path   <- "transition_probability_estimates/src_england/outputs/"
output_date <- format(Sys.Date(), "%Y%m%d")
version     <- "v2"

# Define Output Filenames
file_quit_probs    <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/quit_probabilities_", output_date, "_", version, ".csv")
file_init_probs    <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/init_probabilities_", output_date, "_", version, ".csv")
file_relapse_probs <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/relapse_probabilities_", output_date, "_", version, ".csv")
file_calib_targets <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/quit_probability_calibration_targets_", output_date, "_", version, ".csv")

# ABM Standard Column Names
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
             ..cols_to_keep]
  
  # Sort (Year -> Age -> Sex -> IMD)
  # Assuming first 4 columns are the sorting keys
  setorderv(dt, cols_to_keep[1:length(col_mapping)], rep(1, length(col_mapping)))
  
  # Rename
  setnames(dt, names(col_mapping), as.character(col_mapping))
  
  # Write
  write.csv(dt, output_path, row.names = FALSE)
  
}

# 3. LOAD DATA -----------------------------------------------------------------

raw_quit    <- readRDS(paste0(base_path, "quit_data_england_uncertainty.rds"))$data
raw_init    <- readRDS(paste0(base_path, "init_data_england_uncertainty.rds"))$data
raw_relapse <- readRDS(paste0(base_path, "relapse_data_england_uncertainty.rds"))$data


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

# Work with a copy of the loaded quit data
calib_dt <- copy(raw_quit)

# 5.1 Filters & Categorization

# Filter Years globally (Standard for all targets)
calib_dt <- calib_dt[year >= 2011 & year <= 2019]

# Create Detailed Age Categories (For Age/Sex Verification)
# Cuts: 16-24, 25-44, 45-64, 65-74, 75-89
age_breaks_detailed <- c(-1, 25, 45, 65, 75, 1000)
age_labels_detailed <- c("16-24", "25-44", "45-64", "65-74", "75-89")
calib_dt[, age_cat_detailed := age_labels_detailed[findInterval(age, age_breaks_detailed)]]

# Create Broad Age Categories (For IMD Calibration vs Verification)
# Cuts: 16-24, 25-74, 75-89
age_breaks_broad <- c(-1, 25, 75, 1000)
age_labels_broad <- c("16-24", "25-74", "75-89")
calib_dt[, age_cat_broad := age_labels_broad[findInterval(age, age_breaks_broad)]]

# Create Year Categories
# Cuts: [-1..2014) -> "2011-2013", [2014..2017) -> "2014-2016", [2017..) -> "2017-2019"
calib_dt[, year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(-1, 2014, 2017, 10000))]]

# --- WEIGHTING PREPARATION ---
# Load Survey Data
survey_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds")

# Note that the input data currently only runs to 2018, but up to 2019 is needed
# until the HSE2019 is integrated into this code, as a fix duplicate 2018 and name it 2019
sd19 <- copy(survey_data[year == 2018])[ , year := 2019]
survey_data <- rbindlist(list(survey_data, sd19), use.names = TRUE)


# Calculate weights by SINGLE YEAR and SINGLE AGE
# This ensures that:
# 1. A 25-year-old counts more than a 75-year-old (Age structure)
# 2. The year 2011 counts more than 2016 (because there were more smokers back then)
data_w <- survey_data[smk.state == "current", 
                      .(wgt = sum(wt_int, na.rm = TRUE)), 
                      by = .(year, age, sex, imd_quintile)]

# Merge weights into the calibration table
# Match strictly on Year + Age + Sex + IMD
calib_dt <- merge(calib_dt, data_w, 
                  all.x = TRUE, 
                  by = c("year", "age", "sex", "imd_quintile"))

# Fill NA weights with 0 (for strata with no survey representation)
calib_dt[is.na(wgt), wgt := 0]

# 5.2 Aggregation Tables

# --- Period 1: 2011 - 2016 ---

# Table 3: By Age/Sex (Collapsed across years 2011-2016)
# Uses Detailed Age Categories (16-24, 25-44, 45-64, 65-74, 75-89)
t3 <- calib_dt[year <= 2016, 
               .(p_quit = weighted.mean(p_quit, wgt), p_quit_var = weighted.mean(p_quit_var, wgt)), 
               by = .(age_cat = age_cat_detailed, sex)]
t3[, year_cat := "2011-2016"] 
t3[, imd_quintile := "All"] # Explicitly mark IMD as All

# Table 4: By YearCat/IMD/Broad Age (2011-2013 and 2014-2016)
# Uses Broad Age Categories (16-24, 25-74, 75-89) so we can distinguish the calibration target (25-74)
t4 <- calib_dt[year <= 2016, 
               .(p_quit = weighted.mean(p_quit, wgt), p_quit_var = weighted.mean(p_quit_var, wgt)), 
               by = .(year_cat, imd_quintile, age_cat = age_cat_broad)]
t4[, sex := "All"] # Explicitly mark Sex as All


# --- Period 2: 2017 - 2019 ---

# Table 5: By Age/Sex (Collapsed across years 2017-2019)
t5 <- calib_dt[year >= 2017, 
               .(p_quit = weighted.mean(p_quit, wgt), p_quit_var = weighted.mean(p_quit_var, wgt)), 
               by = .(age_cat = age_cat_detailed, sex)]
t5[, year_cat := "2017-2019"] 
t5[, imd_quintile := "All"]

# Table 6: By YearCat/IMD/Broad Age (2017-2019)
t6 <- calib_dt[year >= 2017, 
               .(p_quit = weighted.mean(p_quit, wgt), p_quit_var = weighted.mean(p_quit_var, wgt)), 
               by = .(year_cat, imd_quintile, age_cat = age_cat_broad)]
t6[, sex := "All"]


# 5.3 Combine & Clean
# Bind all tables
data_t <- rbindlist(list(t3, t4, t5, t6), use.names = TRUE, fill = TRUE)

# Rename to ABM format
setnames(data_t, 
         old = c("year_cat", "age_cat", "sex", "imd_quintile"), 
         new = c("arrivalYearCategorical", "pAgeCategorical", "pGender", "pIMDquintile"))

# Reorder columns
final_cols <- c("arrivalYearCategorical", "pIMDquintile", "pGender", "pAgeCategorical", "p_quit", "p_quit_var")
setcolorder(data_t, final_cols)

# Sort for readability (Year -> IMD -> Sex -> Age)
setorder(data_t, arrivalYearCategorical, pIMDquintile, pGender, pAgeCategorical)

# 5.4 Save
write.csv(data_t, file_calib_targets, row.names = FALSE)



