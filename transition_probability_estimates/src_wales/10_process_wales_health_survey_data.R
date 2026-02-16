# ==============================================================================
# SCRIPT: Process Wales Data (WHS + NSW)
# PURPOSE: Combine Welsh Health Survey (2009-15) and National Survey for Wales (2016-22).
# ==============================================================================

source("03_load_packages.R")
# library(data.table)
# library(magrittr)
# library(hseclean)
# library(mice)
# library(plyr)

# 1. SETUP & PATHS -------------------------------------------------------------

# Robust Project Root
tryCatch({
  root_dir <- rprojroot::find_root(rprojroot::is_rstudio_project)
}, error = function(e) {
  root_dir <- getwd()
})

# Define Output Path (Aligned with England structure)
output_path <- file.path(root_dir, "transition_probability_estimates", "src_wales")
if(!dir.exists(file.path(output_path, "intermediate_data"))) {
  dir.create(file.path(output_path, "intermediate_data"), recursive = TRUE)
}

# 2. DEFINE SCOPE & VARIABLES --------------------------------------------------

# Wales Data Coverage
# WHS: 2009-2015 | NSW: 2016-2022
analysis_years <- 2009:2022 
age_range      <- 16:89

# Variables to retain (Matching England where possible)
keep_vars <- c(
  # Survey design
  "wt_int", "year", "cluster", "psu", # Note: psu/cluster often imputed for Wales
  "region", "tenure", "tenure3",      # Specific to Wales cleaning
  
  # Demographics
  "age", "age_cat", "sex", "imd_quintile", "degree", 
  "relationship_status", "kids", "kids_bin", "employ2cat", "income5cat",
  "nssec3", "activity_lstweek", "social_grade", # Specific Wales vars
  
  # Health
  "hse_mental", 
  
  # Smoking
  "cig_smoker_status", "cig_ever", "years_since_quit", "smage"
)

# Variables requiring complete cases before imputation
complete_vars <- c("age", "sex", "year", "cig_smoker_status")


# 3. PROCESSING FUNCTIONS (WHS vs NSW) -----------------------------------------

# Helper: Clean WHS (2009-2015)
process_whs <- function(data) {
  data %>%
    clean_age %>%
    clean_demographic %>%
    clean_education %>%
    clean_economic_status %>%
    clean_family %>%
    clean_health_and_bio %>%
    smk_status %>%
    # Select only what is available in WHS
    select_data(
      ages = age_range,
      years = 2009:2015,
      keep_vars = keep_vars[keep_vars %in% names(.)], 
      complete_vars = complete_vars
    ) %>%
    as.data.table()
}

# Helper: Clean NSW (2016-2022)
process_nsw <- function(data) {
  data %>%
    clean_age %>%
    clean_demographic %>%
    clean_education %>%
    clean_economic_status %>%
    clean_family %>%
    clean_income %>%
    clean_health_and_bio %>%
    smk_status %>%
    # Select only what is available in NSW
    select_data(
      ages = age_range,
      years = 2016:2022,
      keep_vars = keep_vars[keep_vars %in% names(.)],
      complete_vars = complete_vars
    ) %>%
    as.data.table()
}

# 4. LOAD AND COMBINE DATA -----------------------------------------------------

data_whs <- combine_years(list(
  process_whs(read_WHS_2009()),
  process_whs(read_WHS_2010()),
  process_whs(read_WHS_2011()),
  process_whs(read_WHS_2012()),
  process_whs(read_WHS_2013()),
  process_whs(read_WHS_2014()),
  process_whs(read_WHS_2015())
))

# WHS Standardization
data_whs[, survey := "WHS"]
data_whs[, cluster := paste0(year, "_1")] # Create dummy cluster
# Map Region
data_whs[region == 3, region3 := "north_wales"]
data_whs[region == 2, region3 := "mid_and_west_wales"]
data_whs[region == 1, region3 := "south_wales"]
# Map Tenure
data_whs[tenure3 == 2, social_housing_tenure := "in_social_housing"]
data_whs[tenure3 %in% c(1, 3), social_housing_tenure := "not_in_social_housing"]
# Mental Health (WHS is missing this specific variable)
data_whs[, hse_mental := NA_character_]

# National survey for Wales
data_nsw <- combine_years(list(
  process_nsw(read_NSW_2016_17()),
  process_nsw(read_NSW_2017_18()),
  process_nsw(read_NSW_2018_19()),
  process_nsw(read_NSW_2019_20()),
  process_nsw(read_NSW_2020_21()),
  process_nsw(read_NSW_2021_22()),
  process_nsw(read_NSW_2022_23())
))

# NSW Standardization
data_nsw[, survey := "NSW"]
# Map Region
data_nsw[region == 1, region3 := "north_wales"]
data_nsw[region == 2, region3 := "mid_and_west_wales"]
data_nsw[region %in% 3:4, region3 := "south_wales"]
# Map Tenure
data_nsw[tenure %in% 2:3, social_housing_tenure := "in_social_housing"]
data_nsw[tenure %in% c(1, 4, 5), social_housing_tenure := "not_in_social_housing"]

# Combine
data_w <- rbindlist(list(data_whs, data_nsw), use.names = TRUE, fill = TRUE)

# Cleanup temp columns
data_w[, c("region", "tenure", "tenure3", "nssec3", "social_grade") := NULL]


# 5. RECODING ----------------------------------

age_breaks <- c(16, 18, 25, 35, 45, 55, 65, 75, 1000)
age_labels <- c("16-17", "18-24", "25-34", "35-44", 
                "45-54", "55-64", "65-74", "75-89")

data_w[, age_cat := age_labels[findInterval(age, age_breaks)]]

# Factors for MICE
data_w[, income5cat := factor(income5cat, levels = c("1_lowest_income", "2", "3", "4", "5_highest_income"))]
data_w[, imd_quintile := factor(imd_quintile, levels = c("1_least_deprived", "2", "3", "4", "5_most_deprived"))]

# 6. MICE IMPUTATION (DEMOGRAPHICS) --------------------------------------------

# Check missingness
missing_counts <- colSums(is.na(data_w))
if(sum(missing_counts) > 0) print(missing_counts[missing_counts > 0])

# Impute
imp <- impute_data_mice(
  data = data_w,
  var_names = c(
    "sex", "age_cat", "imd_quintile", "degree", "employ2cat", 
    "income5cat", "cig_smoker_status", "activity_lstweek", 
    "region3", "social_housing_tenure", "relationship_status", "hse_mental"
  ),
  var_methods = c(
    "", # sex - complete
    "", # age_cat - complete
    "polr", # imd_quintile 
    "logreg", # degree
    "logreg", # employ2cat
    "polr", # income5cat
    "", # cigsmokerstatus
    "polyreg", # activity last week
    "polyreg", # region
    "logreg", # social housing
    "polyreg", # relationship
    "logreg" # mental health
  ),
  n_imputations = 5 
)

data_imp <- copy(imp$data)

# 7. POPULATION RE-WEIGHTING ---------------------------------------------------
# Using external Welsh population file to standardize weights

# Load Population Data
pop_path <- "05_input/WalPopQuintiles.csv"

if(file.exists(pop_path)) {
  data_pop <- fread(pop_path)
  setnames(data_pop, c("Age", "IMDq", "Sex", "Year", "Pop"), c("age", "imd_quintile", "sex", "year", "N"))
  
  # Align IMD labels in pop file to data
  data_pop[, imd_quintile := plyr::revalue(imd_quintile, 
                                           c("Q1 (least deprived)" = "1_least_deprived",
                                             "Q2" = "2", "Q3" = "3", "Q4" = "4",
                                             "Q5 (most deprived)" = "5_most_deprived"))]
  
  write.csv(data_pop, "05_input/pop_sizes_wales_national.csv", row.names = FALSE)
  
  # Standardize current weights to mean 1 per year
  data_imp[, wt_int := wt_int / mean(wt_int, na.rm = TRUE), by = "year"]
  
  # Merge Pop Data
  data_imp <- merge(data_imp, data_pop, by = c("age", "imd_quintile", "sex", "year"), all.x = TRUE)
  
  # Calculate Reweighting Factors
  # Sum of weights in sample by strata
  data_imp[, wt_int_ag := sum(wt_int, na.rm = TRUE), by = c("age", "imd_quintile", "sex", "year")]
  
  # Normalize Population and Sample totals
  data_imp[, N_std := N / sum(N, na.rm = TRUE)] 
  data_imp[, wt_int_ag_std := wt_int_ag / sum(wt_int_ag, na.rm = TRUE)]
  
  # Apply Adjustment
  data_imp[, wt_int_adj := wt_int * N_std / wt_int_ag_std]
  
  # Cleanup
  data_imp[, c("N", "N_std", "wt_int_ag", "wt_int_ag_std") := NULL]
  
  # Replace original weight with adjusted weight for analysis
  data_imp[, wt_int := wt_int_adj]
  data_imp[, wt_int_adj := NULL]
  
} else {
  warning("Population file not found. Skipping re-weighting step.")
}

# 8. ENGLAND DATA IMPUTATION (TIME SINCE QUIT) ---------------------------------
# Wales lacks detailed quit times. We impute using England distributions.

# Helper to load HSE quickly
process_hse_lookup <- function(yr) {
  read_fn <- get(paste0("read_", yr))
  read_fn(root = "X:/") %>% # Adjust root if needed
    clean_age %>% clean_demographic %>% smk_status %>% smk_former %>% smk_quit %>%
    select_data(ages = 16:89, years = yr, 
                keep_vars = c("age", "year", "age_cat", "sex", "imd_quintile", "cig_smoker_status", "years_since_quit", "wt_int"),
                complete_vars = c("cig_smoker_status")) %>%
    as.data.table()
}

# Load HSE Reference Data (2009-2019)
hse_list <- lapply(2009:2019, process_hse_lookup)
data_hse <- rbindlist(hse_list, fill = TRUE)

# Align HSE Age Cats to Wales/England Standard
data_hse[, age_cat := age_labels[findInterval(age, age_breaks)]]

# Build Probability Lookup (Stratified by Sex, IMD, Age)
quittime_lkup <- data_hse[cig_smoker_status == "former" & !is.na(years_since_quit), 
                          .(wt = sum(wt_int, na.rm = TRUE)), 
                          by = c("years_since_quit", "sex", "imd_quintile", "age_cat")]

quittime_lkup[, p := wt / sum(wt), by = c("sex", "imd_quintile", "age_cat")]

# Apply Imputation to Wales Data
data_imp[, years_since_quit := NA_integer_]

# Identify former smokers in Wales who need a time
target_rows <- data_imp[cig_smoker_status == "former"]

# Get unique strata in the target data to loop over
# (This is much faster than looping over every individual)
strata <- unique(target_rows[, .(sex, imd_quintile, age_cat)])

for(i in 1:nrow(strata)) {
  
  # Current Stratum
  s_sex <- strata$sex[i]
  s_imd <- strata$imd_quintile[i]
  s_age <- strata$age_cat[i]
  
  # Get Probability Distribution from England Lookup
  probs <- quittime_lkup[sex == s_sex & imd_quintile == s_imd & age_cat == s_age]
  
  # Fallback 1: Relax IMD if no match
  if(nrow(probs) == 0) probs <- quittime_lkup[sex == s_sex & age_cat == s_age]
  
  # Fallback 2: Relax Sex if still no match
  if(nrow(probs) == 0) probs <- quittime_lkup[age_cat == s_age]
  
  # Apply Imputation
  if(nrow(probs) > 0) {
    
    # Identify which rows in the main dataset match this stratum
    idx <- data_imp[cig_smoker_status == "former" & 
                      sex == s_sex & 
                      imd_quintile == s_imd & 
                      age_cat == s_age, which = TRUE]
    
    if(length(idx) > 0) {
      # Handle case where only 1 option exists
      if(nrow(probs) == 1) {
        # If only one option, just assign it (sampling is unnecessary)
        data_imp[idx, years_since_quit := probs$years_since_quit]
      } else {
        # Normal sampling
        data_imp[idx, years_since_quit := sample(probs$years_since_quit, 
                                                 size = length(idx), 
                                                 replace = TRUE, 
                                                 prob = probs$p)]
      }
    }
  }
}

# 9. FINAL ALIGNMENT & SAVE ----------------------------------------------------

# Rename columns to match England Script Output
setnames(data_imp, 
         old = c("cig_smoker_status", "years_since_quit"), 
         new = c("smk.state", "time_since_quit"), 
         skip_absent = TRUE)

# Handle Start Age (smage in NSW, missing in WHS)
if("smage" %in% names(data_imp)) {
  setnames(data_imp, "smage", "start_age", skip_absent = TRUE)
  data_imp[, start_age := as.numeric(start_age)]
  data_imp[start_age < 0 | start_age > age, start_age := NA]
} else {
  data_imp[, start_age := NA_real_]
}

# Censor Age
data_imp[, censor_age := age + 1]

data_imp[ , psu := 1]

# Sort and Save
data_imp <- data_imp[order(year, age, sex)]

outfile <- file.path(output_path, "intermediate_data", "Wales_2009_to_2022_tobacco_imputed.rds")

saveRDS(data_imp, outfile)

