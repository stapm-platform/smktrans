#' Estimate and Forecast Smoking Quitting
#'
#' @description
#' 1. Trend fitting & Mortality calculation.
#' 2. Historical Quit Solver.
#' 3. Forecasts Quit rates (continuing trend).
#' 4. Forecasts 'No Initiation' Quit rates (counterfactual).
#'
#' @export
estimate_quitting <- function(config, survey_data, tob_mort_data, tob_mort_data_cause) {
  
  message(">> [Step 3] Estimating & Forecasting Quitting...")
  
  # Dependencies
  init_file <- file.path(config$path, "outputs", paste0("smk_init_data_", config$country, ".rds"))
  rel_file  <- file.path(config$path, "outputs", paste0("relapse_data_", config$country, ".rds"))
  
  if(!file.exists(init_file) || !file.exists(rel_file)) stop("Missing init/relapse intermediate files.")
  
  smk_init_data <- readRDS(init_file)
  relapse_data  <- readRDS(rel_file)
  
  # A. Trend Fit & Mortality
  # -------------------------------------------------------------------------
  trend_data <- trend_fit(
    data = survey_data,
    max_iterations = 1e3,
    age_var = "age", year_var = "year", sex_var = "sex",
    smoker_state_var = "smk.state", imd_var = "imd_quintile", weight_var = "wt_int"
  )
  saveRDS(trend_data, file.path(config$path, "outputs", paste0("smoking_trends_", config$country, ".rds")))
  
  ###############################################################
  # Load HMD Data (Conditional on Package vs. Local File)
  # 1. Determine which dataset is needed based on country
  if (config$country %in% c("England", "Wales")) {
    dataset_name <- "hmd_data_eng"
    file_path    <- "data/hmd_data_eng.rda"
  } else if (config$country == "Scotland") {
    dataset_name <- "hmd_data_scot"
    file_path    <- "data/hmd_data_scot.rda"
  } else {
    stop("Error: 'country' variable must be 'England', 'Wales', or 'Scotland'.")
  }
  
  # 2. Attempt to load
  if (requireNamespace("smktrans", quietly = TRUE)) {
    
    message(paste0("   > Loading ", dataset_name, " from 'smktrans' package..."))
    
    #Load data directly from the package namespace using the string name
    hmd_data <- get(dataset_name, envir = asNamespace("smktrans"))
    
  } else {
    
    message(paste0("   > 'smktrans' package not detected. Loading local file '", file_path, "'..."))
    
    # Check if the local file actually exists
    if (!file.exists(file_path)) {
      stop(paste0("Error: 'smktrans' package not loaded and local file '", file_path, "' not found."))
    }
    
    # Load into a temporary environment to keep the global namespace clean
    temp_env <- new.env()
    load(file_path, envir = temp_env)
    
    # Flexible loading: check if the expected object name exists in the file
    if (exists(dataset_name, envir = temp_env)) {
      hmd_data <- temp_env[[dataset_name]]
    } else {
      # Fallback: if the .rda contains an object with a different name, grab the first one
      obj_list <- ls(temp_env)
      if (length(obj_list) > 0) {
        obj_name <- obj_list[1]
        message(paste0("     Note: Object '", dataset_name, "' not found in .rda. Using '", obj_name, "' instead."))
        hmd_data <- temp_env[[obj_name]]
      } else {
        stop(paste0("Error: The file '", file_path, "' appears to be empty."))
      }
    }
    rm(temp_env)
  }
  ###############################################################
  
  survivorship_data <- prep_surv(
    mx_data_hmd = hmd_data,
    mx_data_ons = tob_mort_data,
    min_age = config$min_age, max_age = config$max_age,
    min_year = config$first_year, max_year = config$last_year
  )
  saveRDS(survivorship_data, file.path(config$path, "outputs", paste0("survivorship_data_", config$country, ".rds")))
  
  mortality_data <- smoke_surv(
    data = survey_data,
    diseases = tobalcepi::tob_disease_names,
    mx_data = tob_mort_data_cause,
    min_age = config$min_age, max_age = config$max_age,
    min_year = config$first_year, max_year = config$last_year
  )
  saveRDS(mortality_data, file.path(config$path, "outputs", paste0("mortality_data_", config$country, ".rds")))
  
  # B. Historical Quit Solver
  # -------------------------------------------------------------------------
  quit_data <- quit_est(
    trend_data = trend_data,
    survivorship_data = survivorship_data,
    mortality_data = mortality_data$data_for_quit_ests,
    relapse_data = relapse_data$relapse_by_age_imd,
    initiation_data = smk_init_data,
    min_age = config$min_age, max_age = config$max_age,
    min_year = config$first_year, max_year = config$last_year
  )
  saveRDS(quit_data, file.path(config$path, "outputs", paste0("quit_data_", config$country, ".rds")))
  
  # C. Forecast Quitting (Standard)
  # -------------------------------------------------------------------------
  message("   > Forecasting Quit Rates...")
  
  forecast_data <- quit_forecast(
    data = copy(quit_data),
    forecast_var = "p_quit",
    forecast_type = "continuing",
    cont_limit = config$smokefree_target_year + 10,
    first_year = config$first_year,
    jump_off_year = config$last_year - 1,
    time_horizon = 2100,
    youngest_age = config$min_age,
    oldest_age = config$max_age - 1,
    oldest_year = config$first_year,
    age_cont_limit = config$age_trend_limit_quit,
    smooth_rate_dim = config$smooth_rate_dim_quit,
    k_smooth_age = config$k_smooth_age_quit
  )
  
  forecast_data <- forecast_data[age >= config$min_age & age <= config$max_age]
  
  saveRDS(forecast_data, file.path(config$path, "outputs", paste0("quit_forecast_data_", config$country, ".rds")))
  write.csv(forecast_data, file.path(config$path, "outputs", paste0("quit_forecast_data_", config$country, ".csv")), row.names = FALSE)
  
  # D. Forecast Quitting (No Initiation Adjustment)
  # -------------------------------------------------------------------------
  message("   > Forecasting Quit Rates (No Initiation)...")
  
  forecast_data_no_init <- quit_forecast(
    data = copy(quit_data),
    forecast_var = "p_quit_no_init",
    forecast_type = "continuing",
    cont_limit = config$smokefree_target_year + 10,
    first_year = config$first_year,
    jump_off_year = config$last_year - 1,
    time_horizon = 2100,
    youngest_age = config$min_age,
    oldest_age = config$max_age - 1,
    oldest_year = config$first_year,
    age_cont_limit = config$age_trend_limit_quit,
    smooth_rate_dim = config$smooth_rate_dim_quit,
    k_smooth_age = config$k_smooth_age_quit
  )
  
  forecast_data_no_init <- forecast_data_no_init[age >= config$min_age & age <= config$max_age]
  
  saveRDS(forecast_data_no_init, file.path(config$path, "outputs", paste0("quit_forecast_data_no_init_", config$country, ".rds")))
  write.csv(forecast_data_no_init, file.path(config$path, "outputs", paste0("quit_forecast_data_no_init_", config$country, ".csv")), row.names = FALSE)
  
  return(invisible(forecast_data))
}