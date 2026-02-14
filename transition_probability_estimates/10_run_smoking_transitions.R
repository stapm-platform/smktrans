# -------------------------------------------------------------------------
# Master Script: Smoking Transition Probability Estimation
# Orchestrates England, Scotland, and Wales
# -------------------------------------------------------------------------

# 1. Global Setup
# -------------------------------------------------------------------------
root_dir <- "X:/"

# Load standard packages
source("03_load_packages.R")

# --- LOAD FUNCTIONS ---
func_path <- "R/"

source(paste0(func_path, "build_reports.R"))
source(paste0(func_path, "bin_var.R"))
source(paste0(func_path, "calculate_net_initiation.R"))
source(paste0(func_path, "estimate_initiation.R"))
source(paste0(func_path, "estimate_quitting.R"))
source(paste0(func_path, "estimate_relapse.R"))
source(paste0(func_path, "ever_smoke.R"))
source(paste0(func_path, "generate_uncertainty.R"))
source(paste0(func_path, "init_adj.R"))
source(paste0(func_path, "init_est.R"))
source(paste0(func_path, "p_dense.R"))
source(paste0(func_path, "p_smooth.R"))
source(paste0(func_path, "prep_relapse.R"))
source(paste0(func_path, "prep_surv.R"))
source(paste0(func_path, "process_country_wrapper.R"))
source(paste0(func_path, "quit_est.R"))
source(paste0(func_path, "quit_forecast.R"))
source(paste0(func_path, "relapse_forecast.R"))
source(paste0(func_path, "smoke_surv.R"))
source(paste0(func_path, "summarise_smoking_transitions.R"))
source(paste0(func_path, "trend_fit.R"))
source(paste0(func_path, "write_excel_report.R"))

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
  
  first_year = 2003, last_year = 2018,
  min_age = 11, max_age = 89, ref_age = 30,
  time_horizon = 2040,
  
  # Initiation Params
  max_age_init = 30, age_trend_limit_init = 30,
  init_model_choice = "model8",
  
  smooth_rate_dim_init = c(3, 15), 
  # The dimensions of the 2d window used to 
  # smooth trends in the rates by age and year. (age, year), 
  # Defaults to c(3, 3). Must be odd numbers
  
  k_smooth_age_init = 0,
  # the degree of smoothing to apply to the age pattern of change (rotation). 
  # If zero, then no smoothing is applied.
  
  # Quit/Relapse Params
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 79,
  smooth_rate_dim_relapse = c(15, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 79,
  
  # Uncertainty Params
  kn = 50, kR = 0.9, kn_samp = 3,
  cont_limit = 2019
)

# --- Scotland Config ---
# config_scot <- list(
#   country = "Scotland",
#   survey_name = "Scottish Health Survey",
#   path = "transition_probability_estimates/src_scotland/",
#   survey_file = "SHeS_2008_to_2019_tobacco_imputed.rds",
#   pop_file = "transition_probability_estimates/src_scotland/inputs/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv",
#   
#   first_year = 2008, last_year = 2019,
#   min_age = 16, max_age = 89, ref_age = 30,
#   time_horizon = 2040,
#   
#   max_age_init = 30, age_trend_limit_init = 22,
#   smooth_rate_dim_init = c(3, 5), k_smooth_age_init = 0,
#   
#   smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 75,
#   smooth_rate_dim_relapse = c(5, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 75,
#   
#   kn = 50, kR = 0.9, kn_samp = 1000,
#   smokefree_target_year = 2034 # Scotland's target is usually 2034
# )

# --- Wales Config ---
# config_wales <- list(
#   country = "Wales",
#   survey_name = "National Survey for Wales",
#   path = "transition_probability_estimates/src_wales/",
#   survey_file = "HSE_2003_to_2018_tobacco_imputed.rds", 
#   pop_file = "transition_probability_estimates/src_wales/inputs/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv",
#   
#   first_year = 2016, last_year = 2023,
#   min_age = 18, max_age = 89, ref_age = 30,
#   time_horizon = 2040,
#   
#   max_age_init = 30, age_trend_limit_init = 21,
#   smooth_rate_dim_init = c(3, 7), k_smooth_age_init = 0,
#   
#   smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 75,
#   smooth_rate_dim_relapse = c(15, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 75,
#   
#   kn = 50, kR = 0.95, kn_samp = 1000,
#   smokefree_target_year = 2030
# )


# -------------------------------------------------------------------------
# 3. Run Everything
# -------------------------------------------------------------------------

# Run England
process_country(config_eng)

# Run Scotland
#process_country(config_scot)

# Run Wales
#process_country(config_wales)

# -------------------------------------------------------------------------
# 4. Generate Web Reports
# -------------------------------------------------------------------------

# Create the folder if it doesn't exist
if(!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

# Name the list items
all_configs <- list(
  "England"  = config_eng#, 
  #"Scotland" = config_scot, 
  #"Wales"    = config_wales
)

# Save to disk
saveRDS(all_configs, "inst/extdata/report_configs.rds")

# Generate the Rmd wrappers
build_web_reports()

# Build the site
pkgdown::build_site()
