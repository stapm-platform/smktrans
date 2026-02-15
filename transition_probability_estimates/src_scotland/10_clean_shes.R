# ==============================================================================
# SCRIPT: Process Scottish Health Survey (SHeS) Data
# PURPOSE: Clean raw survey data and prepare it for smoking transition estimation.
# SCOPE:   Ages 16+ (SHeS only asks smoking questions to ages 16+)
#          Years 2008 - 2019
# ==============================================================================

source("03_load_packages.R")
# library(data.table)
# library(magrittr)
# library(hseclean)
# library(mice)

# 1. SETUP & PATHS -------------------------------------------------------------

# Define paths
root_dir <- "X:/HAR_PR/PR/Consumption_TA/HSE/Scottish Health Survey (SHeS)/"
path     <- "transition_probability_estimates/src_scotland/"

# Define Scope
analysis_years <- 2008:2019
age_range      <- 16:89

# 2. DEFINE VARIABLES ----------------------------------------------------------

# Variables to retain for analysis
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
  "smk_start_age", "smk_stop_age", "censor_age"
)

# Variables that must have complete cases
complete_vars <- c("age", "sex", "imd_quintile", "year", "psu", "cluster", 
                   "cig_smoker_status", "censor_age")

# 3. DATA CLEANING FUNCTIONS ---------------------------------------------------

# Wrapper function to clean a single year of data
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

# 4. LOAD AND COMBINE DATA -----------------------------------------------------

# Efficiently load and clean each year using lapply
data_list <- lapply(analysis_years, function(yr) {
  # Dynamically construct function name (e.g., read_SHeS_2008)
  # The hseclean package usually follows this naming convention for SHeS
  read_fn_name <- paste0("read_SHeS_", yr)
  
  if (exists(read_fn_name)) {
    read_fn <- get(read_fn_name)
    raw_dt  <- read_fn(root = root_dir)
    process_year_data(raw_dt)
  } else {
    warning(paste("Function", read_fn_name, "not found. Skipping year", yr))
    return(NULL)
  }
})

# Combine into one data.table
data <- combine_years(data_list)
rm(data_list)

# 5. WEIGHTING & RECODING ------------------------------------------------------

# Load population data for Scotland
pop_file <-"05_input/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv"
scot_pops <- fread(pop_file)
setnames(scot_pops, "pops", "N")

# Adjust survey weights
data <- clean_surveyweights(data, pop_data = scot_pops)

# Recode Age Categories
# SHeS analysis starts at 16, so "11-15" category will be empty but kept for alignment
age_breaks <- c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000)
age_labels <- c("11-15", "16-17", "18-24", "25-34", "35-44", 
                "45-54", "55-64", "65-74", "75-89")

data[, age_cat := age_labels[findInterval(age, age_breaks)]]

# Rename columns for internal consistency
setnames(data,
         old = c("smk_start_age", "cig_smoker_status", "years_since_quit"),
         new = c("start_age", "smk.state", "time_since_quit"))

# Sanity Check: Remove invalid start/stop ages
# Note: SHeS data is 16+, so the <11 check is just a safeguard
data[start_age < 11, start_age := NA]
data[smk_stop_age < 11, smk_stop_age := NA]

# Checkpoint: Save clean (but unimputed) data
out_dir_inter <- file.path(path, "intermediate_data")
if(!dir.exists(out_dir_inter)) dir.create(out_dir_inter, recursive = TRUE)

saveRDS(data, file.path(out_dir_inter, "SHeS_2008_to_2019_tobacco.rds"))

# 6. IMPUTATION ----------------------------------------------------------------

# Check missingness
missing_counts <- colSums(is.na(data))
missing_vars   <- missing_counts[missing_counts > 0]

if(length(missing_vars) > 0) {
  cat("Variables with missing data:\n")
  print(missing_vars)
}

# Note: The <16 mental health fix is removed because SHeS data is strictly 16+

# Prepare factors for imputation
# Defining levels ensures ordinal regression (polr) works correctly
data[, kids := factor(kids, levels = c("0", "1", "2", "3+"))]
data[, income5cat := factor(income5cat, levels = c("1_lowest_income", "2", "3", "4", "5_highest_income"))]

# Run Imputation
imp <- impute_data_mice(
  data = data,
  var_names = c("sex", "age_cat", "kids", "relationship_status", 
                "imd_quintile", "degree", "employ2cat", "income5cat", 
                "hse_mental", "smk.state"),
  var_methods = c("", "", "polr", "polyreg", "", "logreg", "", "polr", "logreg", ""),
  n_imputations = 5
)

data_imp <- copy(imp$data)

# 7. SAVE FINAL OUTPUT ---------------------------------------------------------

saveRDS(data_imp, file.path(out_dir_inter, "SHeS_2008_to_2019_tobacco_imputed.rds"))

