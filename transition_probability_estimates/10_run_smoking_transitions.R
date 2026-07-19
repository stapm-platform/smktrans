# -------------------------------------------------------------------------
# Master Script: Smoking Transition Probability Estimation
# Orchestrates England, Scotland, and Wales
# -------------------------------------------------------------------------

# 1. Global Setup
# -------------------------------------------------------------------------
root_dir <- "X:/"

# Load standard packages
source("03_load_packages.R")

# Prepare data (needs the source files in 05_input)
# run once
#source("transition_probability_estimates/src_england/00_prepare_data_wrapper_england.R")
#source("transition_probability_estimates/src_scotland/00_prepare_data_wrapper_scotland.R")
#source("transition_probability_estimates/src_wales/00_prepare_data_wrapper_wales.R")

# Set number of bootstrap resamples (applied to all countries)
kn_samp_global <- 1000

# Master seed for the bootstrap (applied to all countries; each country gets its
# own derived seed below).
seed_global <- 20260716

# --- LOAD FUNCTIONS ---

devtools::load_all()

#func_path <- "R/"

#source(paste0(func_path, "aggregate_uncertainty.R"))
#source(paste0(func_path, "bin_var.R"))
#source(paste0(func_path, "build_reports.R"))
#source(paste0(func_path, "calculate_net_initiation.R"))
#source(paste0(func_path, "estimate_initiation.R"))
#source(paste0(func_path, "estimate_quitting.R"))
#source(paste0(func_path, "estimate_relapse.R"))
#source(paste0(func_path, "ever_smoke.R"))
#source(paste0(func_path, "generate_bootstrap_sample.R"))
#source(paste0(func_path, "init_adj.R"))
#source(paste0(func_path, "init_est.R"))
#source(paste0(func_path, "p_dense.R"))
#source(paste0(func_path, "p_smooth.R"))
#source(paste0(func_path, "prep_relapse.R"))
#source(paste0(func_path, "prep_surv.R"))
#source(paste0(func_path, "process_country_wrapper.R"))
#source(paste0(func_path, "quit_est.R"))
#source(paste0(func_path, "quit_forecast.R"))
#source(paste0(func_path, "relapse_forecast.R"))
#source(paste0(func_path, "run_bootstrap_pipeline.R"))
#source(paste0(func_path, "smoke_surv.R"))
#source(paste0(func_path, "trend_fit.R"))
#source(paste0(func_path, "write_excel_report.R"))

# -------------------------------------------------------------------------
# 2. Execution Configuration
# -------------------------------------------------------------------------

# --- England Config ---
config_eng <- list(
  country = "England",
  survey_name = "Health Survey for England",
  path = "transition_probability_estimates/src_england",
  survey_file = "intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds",
  pop_file = "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv",
  
  first_year = 2003, 
  last_year = 2018,
  trend_last_year = 2019,
  trend_allow_extrapolation = TRUE,
  
  min_age = 11, 
  max_age = 89, 
  ref_age = 30,
  
  time_horizon = 2040,
  
  # What to keep from each bootstrap replicate of the trend surface.
  # The full grid is 79 ages x 16 years x 2 sexes x 5 quintiles x 3 states,
  # which at 1000 iterations is about 38 million rows. Tables 7-10 need a tenth
  # of that. Widen these if a future table needs more.
  trend_keep_ages   = 25:74,
  trend_keep_years  = 2011:2019,
  trend_keep_states = "current",
  
  # Initiation Params
  max_age_init = 30, age_trend_limit_init = 25,
  init_model_choice = "auto", # or "model8" to fix it on a safe model
  init_auto_holdout_bins   = 2,     # year bins held out when scoring candidates
  init_auto_tie_margin     = 2,     # QAIC units within which the simpler model wins
  init_auto_floor          = 0.02,  # projections must stay inside...
  init_auto_ceiling        = 0.98,  # ...this range out to the horizon
  init_auto_max_slope_mult = 2,      # cap on any stratum slope vs the common one
  
  smooth_rate_dim_init = c(3, 7), 
  # The dimensions of the 2d window used to 
  # smooth trends in the rates by age and year. (age, year), 
  # Defaults to c(3, 3). Must be odd numbers
  
  k_smooth_age_init = 0,
  # the degree of smoothing to apply to the age pattern of change (rotation). 
  # If zero, then no smoothing is applied.
  
  # Quit/Relapse Params
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 79,
  smooth_rate_dim_relapse = c(5, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 79,
  
  # Uncertainty Params (Note: kn_samp determines bootstrap iterations)
  #kn = 100, kR = 0.9, # use only with the old generate_uncertainty function
  kn_samp = kn_samp_global,
  seed = seed_global + 1L,
  cont_limit = 2026
)

# --- Scotland Config ---

config_scot <- list(
  country = "Scotland",
  survey_name = "Scottish Health Survey",
  path = "transition_probability_estimates/src_scotland",
  survey_file = "intermediate_data/SHeS_2008_to_2019_tobacco_imputed.rds",
  pop_file = "05_input/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv",
  
  first_year = 2008, 
  last_year = 2019,
  trend_last_year = 2019,
  trend_allow_extrapolation = FALSE,
  trend_keep_ages   = 25:74,
  trend_keep_years  = 2011:2019,
  trend_keep_states = "current",
  
  min_age = 16, 
  max_age = 89, 
  ref_age = 30,
  time_horizon = 2040,
  
  # Initiation Params
  max_age_init = 30, age_trend_limit_init = 25,
  init_model_choice = "auto", # or "model8" to fix it on a safe model
  init_auto_holdout_bins   = 2,     # year bins held out when scoring candidates
  init_auto_tie_margin     = 2,     # QAIC units within which the simpler model wins
  init_auto_floor          = 0.02,  # projections must stay inside...
  init_auto_ceiling        = 0.98,  # ...this range out to the horizon
  init_auto_max_slope_mult = 2,      # cap on any stratum slope vs the common one
  
  smooth_rate_dim_init = c(3, 7), 
  # The dimensions of the 2d window used to 
  # smooth trends in the rates by age and year. (age, year), 
  # Defaults to c(3, 3). Must be odd numbers
  
  k_smooth_age_init = 0,
  # the degree of smoothing to apply to the age pattern of change (rotation). 
  # If zero, then no smoothing is applied.
  
  # Quit/Relapse Params
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 79,
  smooth_rate_dim_relapse = c(5, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 79,
  
  # Uncertainty Params
  #kn = 100, kR = 0.9, # use only with the old generate_uncertainty function
  kn_samp = kn_samp_global,
  seed = seed_global + 2L,
  cont_limit = 2026
)

# --- Wales Config ---

config_wales <- list(
  country = "Wales",
  survey_name = "National Survey for Wales",
  path = "transition_probability_estimates/src_wales",
  survey_file = "intermediate_data/Wales_2009_to_2022_tobacco_imputed.rds",
  pop_file = "05_input/pop_sizes_wales_national.csv",
  
  first_year = 2009, 
  last_year = 2022,
  trend_last_year = 2022, 
  trend_allow_extrapolation = FALSE,
  trend_keep_ages   = 25:74,
  trend_keep_years  = 2011:2019,
  trend_keep_states = "current",
  
  min_age = 16, 
  max_age = 89, 
  ref_age = 30,
  
  time_horizon = 2040,
  
  # Initiation Params
  max_age_init = 30, age_trend_limit_init = 25,
  init_model_choice = "auto", # or "model8" to fix it on a safe model
  init_auto_holdout_bins   = 2,     # year bins held out when scoring candidates
  init_auto_tie_margin     = 2,     # QAIC units within which the simpler model wins
  init_auto_floor          = 0.02,  # projections must stay inside...
  init_auto_ceiling        = 0.98,  # ...this range out to the horizon
  init_auto_max_slope_mult = 2,      # cap on any stratum slope vs the common one
  
  smooth_rate_dim_init = c(3, 7), 
  # The dimensions of the 2d window used to 
  # smooth trends in the rates by age and year. (age, year), 
  # Defaults to c(3, 3). Must be odd numbers
  
  k_smooth_age_init = 0,
  # the degree of smoothing to apply to the age pattern of change (rotation). 
  # If zero, then no smoothing is applied.
  
  # Quit/Relapse Params
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 79,
  smooth_rate_dim_relapse = c(5, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 79,
  
  # Uncertainty Params
  #kn = 100, kR = 0.9, # use only with the old generate_uncertainty function
  kn_samp = kn_samp_global,
  seed = seed_global + 3L,
  cont_limit = 2026
)

# -------------------------------------------------------------------------
# 3. Run Everything
# -------------------------------------------------------------------------

# Run England
process_country(config_eng)

# Run Scotland
process_country(config_scot)

# Run Wales
process_country(config_wales)

# -------------------------------------------------------------------------
# 4. Generate Web Reports
# -------------------------------------------------------------------------

# Create the folder if it doesn't exist
if(!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

# Name the list items
all_configs <- list(
  "England"  = config_eng, 
  "Scotland" = config_scot, 
  "Wales"    = config_wales
)

# Save to disk
saveRDS(all_configs, "inst/extdata/report_configs.rds")

# Generate the Rmd wrappers
build_web_reports()

# Build the site
pkgdown::build_site()
# This only builds the local docs/ preview — it does not publish anything. 
# The live site serves from the gh-pages branch, 
# which only updates with pkgdown::deploy_to_branch()

