# -------------------------------------------------------------------------
# Master Script: Smoking Transition Probability Estimation
# Orchestrates England, Scotland, and Wales
# -------------------------------------------------------------------------

# 1. Global Setup
# -------------------------------------------------------------------------
root_dir <- "X:/"

# Load standard packages
source("03_load_packages.R")

# --- LOAD NEW MODULAR FUNCTIONS ---
# Assuming you saved the functions I provided in a 'functions' subfolder.
# If they are in the root of 'transition_probability_estimates', remove 'functions/'.

func_path <- "transition_probability_estimates/functions/"

source(paste0(func_path, "estimate_initiation.R"))
source(paste0(func_path, "estimate_quitting.R"))
source(paste0(func_path, "estimate_relapse.R"))
source(paste0(func_path, "calculate_net_initiation.R"))      # The new Net Init function
source(paste0(func_path, "summarise_smoking_transitions.R")) # The new Summary function
source(paste0(func_path, "process_country_wrapper.R"))       # The master wrapper (see below)

# -------------------------------------------------------------------------
# 2. Execution Configuration
# -------------------------------------------------------------------------

# --- England Config ---
config_eng <- list(
  country = "England",
  survey_name = "Health Survey for England",
  path = "transition_probability_estimates/src_england/",
  survey_file = "HSE_2003_to_2018_tobacco_imputed.rds",
  pop_file = "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv",
  
  first_year = 2003, last_year = 2018,
  min_age = 12, max_age = 89, ref_age = 30,
  
  # Initiation Params
  max_age_init = 30, age_trend_limit_init = 21,
  smooth_rate_dim_init = c(3, 7), k_smooth_age_init = 0,
  
  # Quit/Relapse Params
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 79,
  smooth_rate_dim_relapse = c(15, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 79,
  
  # Uncertainty Params
  kn = 20, kR = 0.9, kn_samp = 100,
  smokefree_target_year = 2030
)

# --- Scotland Config ---
config_scot <- list(
  country = "Scotland",
  survey_name = "Scottish Health Survey",
  path = "transition_probability_estimates/src_scotland/",
  survey_file = "SHeS_2008_to_2019_tobacco_imputed.rds",
  pop_file = "transition_probability_estimates/src_scotland/inputs/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv",
  
  first_year = 2008, last_year = 2019,
  min_age = 16, max_age = 89, ref_age = 30,
  
  max_age_init = 30, age_trend_limit_init = 22,
  smooth_rate_dim_init = c(3, 5), k_smooth_age_init = 0,
  
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 75,
  smooth_rate_dim_relapse = c(5, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 75,
  
  kn = 20, kR = 0.9, kn_samp = 100,
  smokefree_target_year = 2034 # Scotland's target is usually 2034
)

# --- Wales Config ---
config_wales <- list(
  country = "Wales",
  survey_name = "National Survey for Wales",
  path = "transition_probability_estimates/src_wales/",
  # NOTE: Your original script pointed to HSE (England) data here. 
  # Please verify if this should be "NSW_...rds" or if you are using England as a proxy.
  survey_file = "HSE_2003_to_2018_tobacco_imputed.rds", 
  pop_file = "transition_probability_estimates/src_wales/inputs/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv",
  
  first_year = 2016, last_year = 2023,
  min_age = 18, max_age = 89, ref_age = 30,
  
  max_age_init = 30, age_trend_limit_init = 21,
  smooth_rate_dim_init = c(3, 7), k_smooth_age_init = 0,
  
  smooth_rate_dim_quit = c(5, 7), k_smooth_age_quit = 6, age_trend_limit_quit = 75,
  smooth_rate_dim_relapse = c(15, 7), k_smooth_age_relapse = 6, age_trend_limit_relapse = 75,
  
  kn = 100, kR = 0.95, kn_samp = 100,
  smokefree_target_year = 2030
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

message("\n=======================================================")
message(" ALL COUNTRIES PROCESSED SUCCESSFULLY.")
message("=======================================================")