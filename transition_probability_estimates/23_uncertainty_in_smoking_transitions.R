# -------------------------------------------------------------------------
# Script: 23_uncertainty_in_smoking_transitions.R
# Purpose: Estimate uncertainty intervals using correlated Beta sampling.
#          Run automatically by the Master Runner.
# -------------------------------------------------------------------------

message("\n--- Starting Uncertainty Estimation ---")

# Ensure required parameters exist (defaults provided if missing)
if (!exists("kn")) kn <- 100       # Effective sample size assumption
if (!exists("kn_samp")) kn_samp <- 100 # Number of PSA samples
if (!exists("kR")) kR <- 0.95      # Correlation

# Define Output Paths (Relative to the 'path' defined in master runner)
# Note: 'path' comes from the config object in the master runner
path_out_rds <- file.path(config$path, "outputs")
dir.create(path_out_rds, showWarnings = FALSE, recursive = TRUE)

# Helper to write to Excel safely
write_sheet <- function(wb, sheet_name, title, dt) {
  if (!(sheet_name %in% names(wb))) openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, title, startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet_name, dt, startCol = 1, startRow = 2)
}

# -------------------------------------------------------------------------
# 1. Initiation Uncertainty
# -------------------------------------------------------------------------
message("  > Calculating Initiation Uncertainty...")

# 'init_data' comes from the Global Environment (passed by process_country)
# We use copy() to ensure we don't modify the original in place unexpectedly
init_unc <- generate_uncertainty(
  data = init_data, 
  prob_col = "p_start", 
  n_eff = kn, 
  n_samp = kn_samp, 
  correlation = kR
)

# Extract results
init_final <- init_unc$data
init_samples <- init_unc$samples

# Save
saveRDS(init_final, file.path(path_out_rds, paste0("init_data_", config$country, "_uncertainty.rds")))
# Optionally save the full sample matrix if needed for PSA
# saveRDS(init_samples, file.path(path_out_rds, paste0("init_samples_", config$country, ".rds")))

write_sheet(wb, "Initiation", 
            "Probabilities of smoking initiation (never to current smoker)", 
            init_final)

# Clean up memory
rm(init_unc, init_samples)
gc()

# -------------------------------------------------------------------------
# 2. Relapse Uncertainty
# -------------------------------------------------------------------------
message("  > Calculating Relapse Uncertainty...")

# We use 'relapse_tsq' (Time-Since-Quit) as the primary dataset for reporting
# Ensure p_relapse is not exactly 0 to allow Beta calc
relapse_input <- copy(relapse_tsq) 
setnames(relapse_input, "p_relapse_adj", "p_relapse", skip_absent = TRUE) # Ensure col name matches

relapse_unc <- generate_uncertainty(
  data = relapse_input, 
  prob_col = "p_relapse", 
  n_eff = kn, 
  n_samp = kn_samp, 
  correlation = kR
)

relapse_final <- relapse_unc$data
relapse_samples <- relapse_unc$samples

saveRDS(relapse_final, file.path(path_out_rds, paste0("relapse_data_", config$country, "_uncertainty.rds")))

write_sheet(wb, "Relapse", 
            "Probabilities of relapse (former to current). Stratified by years since quitting.", 
            relapse_final)

rm(relapse_unc, relapse_samples)
gc()

# -------------------------------------------------------------------------
# 3. Quit Uncertainty
# -------------------------------------------------------------------------
message("  > Calculating Quit Uncertainty...")

quit_unc <- generate_uncertainty(
  data = quit_data, 
  prob_col = "p_quit", 
  n_eff = kn, 
  n_samp = kn_samp, 
  correlation = kR
)

quit_final <- quit_unc$data
quit_samples <- quit_unc$samples

# The legacy script saved the quit MATRIX specifically. 
# We maintain this behaviour as STAPM likely uses it for PSA.
saveRDS(quit_samples, file.path(path_out_rds, paste0("quit_mat_", config$country, "_uncertainty.rds")))
saveRDS(quit_final, file.path(path_out_rds, paste0("quit_data_", config$country, "_uncertainty.rds")))

write_sheet(wb, "Quit", 
            "Probabilities of quitting smoking (current to former smoker).", 
            quit_final)

rm(quit_unc, quit_samples)
gc()

message("--- Uncertainty Estimation Complete ---")