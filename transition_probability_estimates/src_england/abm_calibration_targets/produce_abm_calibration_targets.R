# ==============================================================================
# SCRIPT: Generate ABM Inputs & Calibration Targets
# PURPOSE: Process probabilistic uncertainty data for the ABM.
#          1. Outputs: Full probability tables for Init, Quit, Relapse.
#          2. Targets: Aggregated targets (Means & Covariance) for calibrating ABM.
# ==============================================================================

# 1. CONFIGURATION -------------------------------------------------------------
source("03_load_packages.R")

# Define Paths
base_path   <- "transition_probability_estimates/src_england/outputs/"
output_date <- format(Sys.Date(), "%Y%m%d")
version     <- "v1"

# Define Output Filenames
file_quit_probs    <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/quit_probabilities_", output_date, "_", version, ".csv")
file_init_probs    <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/init_probabilities_", output_date, "_", version, ".csv")
file_relapse_probs <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/relapse_probabilities_", output_date, "_", version, ".csv")

# NEW: We output two files for the calibration targets
file_calib_means   <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/quit_calibration_targets_means_", output_date, "_", version, ".csv")
file_calib_covar   <- paste0("transition_probability_estimates/src_england/abm_calibration_targets/quit_calibration_targets_covariance_", output_date, "_", version, ".csv")

# 2. HELPER FUNCTIONS ----------------------------------------------------------
export_abm_table <- function(data, age_range, year_range, cols_to_keep, col_mapping, output_path) {
  dt <- data[age >= age_range[1] & age <= age_range[2] & 
               year >= year_range[1] & year <= year_range[2], ..cols_to_keep]
  setorderv(dt, cols_to_keep[1:length(col_mapping)], rep(1, length(col_mapping)))
  setnames(dt, names(col_mapping), as.character(col_mapping))
  write.csv(dt, output_path, row.names = FALSE)
}

# 3. PART A: GENERATE ABM INPUT FILES (PROBABILITIES) --------------------------
# Load the aggregated uncertainty data (Used only for direct ABM lookups)
raw_quit    <- readRDS(paste0(base_path, "quit_data_england_uncertainty.rds"))$data
raw_init    <- readRDS(paste0(base_path, "init_data_england_uncertainty.rds"))$data
raw_relapse <- readRDS(paste0(base_path, "relapse_data_england_uncertainty.rds"))$data

quit_map <- c("year"="arrivalYear", "age"="pAge", "sex"="pGender", "imd_quintile"="pIMDquintile")
export_abm_table(raw_quit, c(16, 89), c(2011, 2040), c("year", "age", "sex", "imd_quintile", "p_quit"), quit_map, file_quit_probs)
export_abm_table(raw_init, c(16, 30), c(2011, 2040), c("year", "age", "sex", "imd_quintile", "p_start"), quit_map, file_init_probs)

relapse_map <- c(quit_map, "time_since_quit" = "bYearsSinceQuit")
export_abm_table(raw_relapse, c(16, 89), c(2011, 2040), c("year", "age", "sex", "imd_quintile", "time_since_quit", "p_relapse"), relapse_map, file_relapse_probs)

# Free up memory
rm(raw_quit, raw_init, raw_relapse); gc()


# 4. PART B: GENERATE CALIBRATION TARGETS (EMPIRICAL COVARIANCE) ---------------
message("Processing empirical bootstrap targets...")

# CRITICAL: We must load the RAW bootstrap data (all 1000 iterations) 
# to calculate accurate empirical variance and covariance.
boot_dt <- readRDS(paste0(base_path, "raw_boot_quit_data_England.rds"))
boot_dt <- boot_dt[!is.na(p_quit)]
boot_dt <- boot_dt[year >= 2011 & year <= 2019]

# --- 4.1 Categorization ---
age_breaks_detailed <- c(-1, 25, 45, 65, 75, 1000)
boot_dt[, age_cat_detailed := c("16-24", "25-44", "45-64", "65-74", "75-89")[findInterval(age, age_breaks_detailed)]]

age_breaks_broad <- c(-1, 25, 75, 1000)
boot_dt[, age_cat_broad := c("16-24", "25-74", "75-89")[findInterval(age, age_breaks_broad)]]

boot_dt[, year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(-1, 2014, 2017, 10000))]]

# --- 4.2 Apply Static Base Weights ---
survey_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds")
sd19 <- copy(survey_data[year == 2018])[ , year := 2019]
survey_data <- rbindlist(list(survey_data, sd19), use.names = TRUE)

data_w <- survey_data[smk.state == "current", 
                      .(wgt = sum(wt_int, na.rm = TRUE)), 
                      by = .(year, age, sex, imd_quintile)]

# Merge weights into the 1,000 iterations
boot_dt <- merge(boot_dt, data_w, all.x = TRUE, by = c("year", "age", "sex", "imd_quintile"))
boot_dt[is.na(wgt), wgt := 0]

# --- 4.2b PRE-AGGREGATION DIAGNOSTIC: Check Raw Data Sparsity ---
message("\n>> Checking raw data for missingness (values that will be skipped by na.rm)...")

raw_na_count <- sum(is.na(boot_dt$p_quit))
raw_total <- nrow(boot_dt)
raw_na_pct <- (raw_na_count / raw_total) * 100

if (raw_na_count > 0) {
  message(sprintf("⚠️ WARNING: %d missing 'p_quit' values detected in the raw data (%.2f%%).", raw_na_count, raw_na_pct))
  
  # Calculate how much data is missing for each specific demographic chunk
  # Checking the Detailed Age categories as an indicator:
  sparse_groups <- boot_dt[, .(
    total_underlying_rows = .N,
    missing_rows = sum(is.na(p_quit)),
    pct_missing = (sum(is.na(p_quit)) / .N) * 100
  ), by = .(year_cat, age_cat_detailed, sex, imd_quintile)]
  
  # Filter to groups that have NAs and sort by the worst offenders
  bad_sparse <- sparse_groups[pct_missing > 0][order(-pct_missing)]
  
  message("\n  - Most sparse demographic groups (highest % of underlying NA values):")
  print(head(bad_sparse, 10))
  
  if (max(bad_sparse$pct_missing) > 50) {
    warning("CRITICAL: Some demographic groups are calculating their weighted mean based on less than half of their expected data points!")
  }
} else {
  message("✅ Raw data is completely solid! `na.rm = TRUE` will not have to skip a single NA.")
}
message("--------------------------------------------------\n")

# --- 4.3 Target Aggregation (Computed PER BOOTSTRAP ITERATION) ---

# Table 3 (2011-2016): Detailed Age / Sex
t3 <- boot_dt[year <= 2016, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, age_cat = age_cat_detailed, sex)]
t3[, `:=`(year_cat = "2011-2016", imd_quintile = "All")]

# Table 4 (2011-2016): YearCat / IMD / Broad Age
t4 <- boot_dt[year <= 2016, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
t4[, sex := "All"]

# Table 5 (2017-2019): Detailed Age / Sex
t5 <- boot_dt[year >= 2017, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, age_cat = age_cat_detailed, sex)]
t5[, `:=`(year_cat = "2017-2019", imd_quintile = "All")]

# Table 6 (2017-2019): YearCat / IMD / Broad Age
t6 <- boot_dt[year >= 2017, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
t6[, sex := "All"]

# Combine all target estimations
all_targets <- rbindlist(list(t3, t4, t5, t6), use.names = TRUE, fill = TRUE)

# Assign a unique Target ID to each demographic group
setorder(all_targets, year_cat, imd_quintile, sex, age_cat, boot_id)
all_targets[, target_id := sprintf("T_%03d", .GRP), by = .(year_cat, age_cat, sex, imd_quintile)]

# ... [End of Part 4.3] ...
all_targets[, target_id := sprintf("T_%03d", .GRP), by = .(year_cat, age_cat, sex, imd_quintile)]


# --- 4.4 Calculate Final Variance & Covariance Matrices ---

# FILE 1: Target Definitions, Means, and Variances
target_means <- all_targets[, .(
  mean_p_quit = mean(p_quit, na.rm = TRUE),
  var_p_quit  = var(p_quit, na.rm = TRUE)
), by = .(target_id, year_cat, imd_quintile, sex, age_cat)]

setnames(target_means, 
         old = c("year_cat", "age_cat", "sex", "imd_quintile"), 
         new = c("arrivalYearCategorical", "pAgeCategorical", "pGender", "pIMDquintile"))

# FILE 2: The Global Covariance Matrix
# Pivot wider: rows = iterations (1 to 1000), columns = target_ids
boot_wide <- dcast(all_targets, boot_id ~ target_id, value.var = "p_quit")
boot_wide[, boot_id := NULL] # Remove ID so we only have data columns

# Calculate covariance matrix ignoring NAs
cov_matrix <- cov(boot_wide, use = "pairwise.complete.obs")

# Convert to data.table with a named column for export
cov_dt <- as.data.table(cov_matrix, keep.rownames = "target_id")

# --- 4.5 Save Outputs ---
write.csv(target_means, file_calib_means, row.names = FALSE)
write.csv(cov_dt, file_calib_covar, row.names = FALSE)


