# ==============================================================================
# SCRIPT: Process Health Survey for England (HSE) Data
# PURPOSE: Clean raw survey data and prepare it for smoking transition estimation.
# ==============================================================================

source("03_load_packages.R")
# library(data.table) # For fast data manipulation
# library(magrittr)   # For pipes (%>%, %<>%)
# library(hseclean)   # Custom package for cleaning HSE data
# library(mice)       # For imputation

# Define paths
root_dir <- "X:/"
path <- "transition_probability_estimates/src_england/"

# Define Scope
analysis_years <- 2003:2018
age_range      <- 11:89

# DEFINE VARIABLES

# Variables to retain for analysis 
# (these are names of the processed variables produced by the hseclean functions)
keep_vars <- c(
  # Survey design
  "wt_int", "psu", "cluster", "year",
  # Demographics
  "age", "age_cat", "sex", "imd_quintile", "degree", 
  "relationship_status", "kids", "employ2cat", "income5cat",
  # Health
  "hse_mental",
  # Smoking
  "cig_smoker_status", "years_since_quit", "years_reg_smoker", "cig_ever",
  "smk_start_age", "smk_stop_age", "censor_age", "cigs_per_day", 
  "smoker_cat", "hand_rolled_per_day", "machine_rolled_per_day", 
  "prop_handrolled", "cig_type"
)

# The variables that must have complete cases
complete_vars <- c("age", "sex", "imd_quintile", "year", "psu", "cluster", 
                   "cig_smoker_status", "censor_age")


# DATA CLEANING FUNCTIONS

# Wrapper function to clean a single year of data
# Uses magrittr pipe (%>%) to chain cleaning steps
process_year_data <- function(data) {
  data %>%
    clean_age() %>%
    clean_demographic() %>%
    clean_education() %>%
    clean_economic_status() %>%
    clean_family() %>%
    clean_income() %>%
    clean_health_and_bio() %>%
    smk_status() %>%
    smk_former() %>%
    smk_quit() %>%
    smk_life_history() %>%
    select_data(
      ages = age_range,
      years = analysis_years,
      keep_vars = keep_vars,
      complete_vars = complete_vars
    )
}

# LOAD AND COMBINE DATA

data_list <- lapply(analysis_years, function(yr) {
  read_fn <- get(paste0("read_", yr))
  raw_dt  <- read_fn(root = root_dir)
  process_year_data(raw_dt)
})

data <- combine_years(data_list)
rm(data_list) 

# WEIGHTING & RECODING

# Load population data for re-weighting
pop_file <- "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv"
eng_pops <- fread(pop_file)
setnames(eng_pops, "pops", "N")

# Adjust survey weights
data <- clean_surveyweights(data, pop_data = eng_pops)

# Recode Age Categories
age_breaks <- c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000)
age_labels <- c("11-15", "16-17", "18-24", "25-34", "35-44", 
                "45-54", "55-64", "65-74", "75-89")

data[, age_cat := age_labels[findInterval(age, age_breaks)]]

# Rename columns for internal consistency
setnames(data,
         old = c("smk_start_age", "cig_smoker_status", "years_since_quit"),
         new = c("start_age", "smk.state", "time_since_quit"))

# Remove invalid start/stop ages
data[start_age < 11, start_age := NA]
data[smk_stop_age < 11, smk_stop_age := NA]

# Save clean (but unimputed) data
if(!dir.exists(paste0(path, "intermediate_data"))) dir.create(paste0(path, "intermediate_data"))
saveRDS(data, paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco.rds"))

# IMPUTATION ----------------------------------------------------------------

# Check missingness
missing_counts <- colSums(is.na(data))
missing_vars   <- missing_counts[missing_counts > 0]

if(length(missing_vars) > 0) {
  cat("Variables with missing data:\n")
  print(missing_vars)
}

# Assumption: Missing mental health data for <16s implies 'no_mental' issues
data[is.na(hse_mental) & age < 16, hse_mental := "no_mental"]

# Prepare factors for imputation
# Defining levels ensures ordinal regression (polr) works correctly
data[, kids := factor(kids, levels = c("0", "1", "2", "3+"))]
data[, income5cat := factor(income5cat, levels = c("1_lowest_income", "2", "3", "4", "5_highest_income"))]

# Run Imputation
# 'impute_data_mice' is a wrapper function from hseclean
imp <- impute_data_mice(
  data = data,
  var_names = c("sex", "age_cat", "kids", "relationship_status", 
                "imd_quintile", "degree", "employ2cat", "income5cat", 
                "hse_mental", "smk.state"),
  var_methods = c("", "", "polr", "polyreg", "", "logreg", "", "polr", "logreg", ""),
  n_imputations = 5 
  # Note: n_imputations = 2 is low. Used for testing speed. 
  # For final production estimates, consider increasing to 5 or 10.
)

data_imp <- copy(imp$data)

# SAVE FINAL OUTPUT
saveRDS(data_imp, paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds"))



