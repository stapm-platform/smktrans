#' Resolve which ever-smoking trend model an initiation run should use
#'
#' Internal helper for estimate_initiation(). Model selection is a base-run job:
#' the base run selects and writes its choice, and every bootstrap iteration
#' reads it back rather than re-selecting, so a single model underlies the whole
#' interval. Not exported and not part of the public reference.
#'
#' @param config List - the country config passed to estimate_initiation().
#' @param boot_mode Logical - TRUE inside a bootstrap iteration.
#' @return A list with the resolved model name and, on the base run, the path to
#'   save the choice to.
#' @keywords internal
#' @noRd
init_resolve_model <- function(config, boot_mode) {

  choice_file <- file.path(config$path, "outputs",
                           paste0("init_model_choice_", config$country, ".rds"))

  if (!identical(config$init_model_choice, "auto")) {
    return(list(model = config$init_model_choice, choice_file = NULL))
  }

  needed <- c("init_auto_holdout_bins", "init_auto_tie_margin", "init_auto_floor",
              "init_auto_ceiling", "init_auto_max_slope_mult")
  missing_knobs <- needed[!needed %in% names(config)]
  if (length(missing_knobs) > 0) {
    stop("estimate_initiation: init_model_choice is 'auto' but the config does not ",
         "set ", paste(missing_knobs, collapse = ", "), ". The selection settings ",
         "live in the config so the run is reproducible from it.")
  }

  if (boot_mode) {
    if (!file.exists(choice_file)) {
      stop("estimate_initiation: init_model_choice is 'auto' and this is a bootstrap ",
           "iteration, but there is no resolved choice at ", choice_file, ". Run the ",
           "base estimation first - the bootstrap fixes the model the base run chose ",
           "rather than re-selecting on every resample.")
    }
    return(list(model = readRDS(choice_file), choice_file = NULL))
  }

  list(model = "auto", choice_file = choice_file)
}

#' Estimate and Forecast Smoking Initiation
#'
#' @description
#' 1. Estimates historical initiation (cumulative -> density).
#' 2. Forecasts future initiation using `quit_forecast` (continuing trend).
#' 3. Saves raw, adjusted, and forecasted outputs.
#'
#' @param config List. Must contain: first_year, last_year, min_age, max_age, ref_age,
#' smokefree_target_year, age_trend_limit_init, smooth_rate_dim_init, k_smooth_age_init.
#' If init_model_choice is "auto", it must also contain the selection settings:
#' init_auto_holdout_bins, init_auto_tie_margin, init_auto_floor,
#' init_auto_ceiling, init_auto_max_slope_mult. Making them explicit in the
#' config, rather than falling back to defaults buried in ever_smoke(), means a
#' run can be reproduced from its config block alone.
#' @param survey_data Data table of individual survey records.
#' @param boot_mode Logical. If TRUE, skips writing to disk and returns the
#'   estimates for one bootstrap iteration.
#'
#' @export
estimate_initiation <- function(config, survey_data, boot_mode = FALSE) {
  
  if (!boot_mode) message(">> [Step 1] Estimating & Forecasting Initiation...")
  
  # A. Estimate Raw Initiation (Cohort Cumulative)
  # -------------------------------------------------------------------------
  init_data_raw <- init_est(
    data = survey_data,
    strat_vars = c("sex", "imd_quintile")
  )
  
  # B. Estimate 'Ever Smoker' Trends (for adjustment)
  # -------------------------------------------------------------------------
  
  resolved <- init_resolve_model(config, boot_mode)

  ever_smoke_data <- ever_smoke(
    data = survey_data,
    time_horizon = config$time_horizon,
    num_bins = 7,
    model = resolved$model, 
    min_age = config$min_age,
    min_year = config$first_year,
    age_cats = c("25-34"),
    # Only read when model is "auto"; validated by init_resolve_model above.
    auto_holdout_bins   = config$init_auto_holdout_bins,
    auto_tie_margin     = config$init_auto_tie_margin,
    auto_floor          = config$init_auto_floor,
    auto_ceiling        = config$init_auto_ceiling,
    auto_max_slope_mult = config$init_auto_max_slope_mult
  )

  # The base run under "auto" writes its choice for the bootstrap to pick up.
  if (!is.null(resolved$choice_file)) {
    saveRDS(ever_smoke_data$model_choice, resolved$choice_file)
    message("   > Ever-smoking trend model resolved to ", ever_smoke_data$model_choice,
            " and saved for the bootstrap (", basename(resolved$choice_file), ")")
  }
  
  # C. Adjust for Recall Bias
  # -------------------------------------------------------------------------
  init_data_adj <- init_adj(
    init_data = copy(init_data_raw),
    ever_smoke_data = copy(ever_smoke_data$predicted_values),
    ref_age = config$ref_age,
    fix_ref_age = FALSE,
    # 18 rather than 21. init_adj now completes a truncated cohort's curve up to
    # its ref_age equivalent before calibrating, so the old reason for refusing
    # cohorts seen only to their early twenties - the truncation bias - is
    # corrected rather than avoided, and the youngest cohorts with usable data
    # get to calibrate on their own numbers.
    min_ref = 18,
    cohorts = (config$first_year - config$ref_age):config$time_horizon,
    period_start = config$first_year, 
    period_end = config$last_year
  )
  
  # D. Convert to Density (Annual Probability)
  # -------------------------------------------------------------------------
  smk_init_data <- p_dense(
    data = copy(init_data_adj),
    cum_func_var = "p_ever_smoker_adj",
    strat_vars = c("cohort", "sex", "imd_quintile"),
    lowest_year = config$first_year, 
    max_year = config$last_year
  )
  
  # E. Forecast
  # -------------------------------------------------------------------------
  if (!boot_mode) message("   > Forecasting Initiation Trends...")
  
  init_forecast_data <- quit_forecast(
    data = copy(smk_init_data),
    forecast_var = "p_start",
    forecast_type = "continuing", 
    cont_limit = config$cont_limit,
    oldest_year = config$first_year,
    youngest_age = config$min_age,
    oldest_age = config$ref_age,
    age_cont_limit = config$age_trend_limit_init,
    first_year = config$first_year,    
    # Jump off from the last estimated year rather than the year before it.
    # Quit and relapse keep the last_year - 1 convention, but initiation is only
    # estimated to last_year in the first place, and with the validation running
    # on estimated years those years are the scarce resource - no reason to
    # throw the final one away from the trend fit.
    jump_off_year = config$last_year, 
    time_horizon = config$time_horizon,
    smooth_rate_dim = config$smooth_rate_dim_init,
    k_smooth_age = config$k_smooth_age_init,
    # Since the cumulative-curve fix in p_dense, a zero in the initiation
    # surface is a real zero - nobody in that cohort starts at that age - not
    # survey noise. Without this flag those zeros get clamped to 1e-6 and the
    # raster smoothing averages them with the ages just below, which inflated
    # the published tail at 24-25 by a factor of 3 to 4, and the age 26+ fill
    # then carried the inflated value up to 30. Quit and relapse leave this
    # off: their zeros are sparse-cell noise and smoothing over them is right.
    preserve_zeros = TRUE
  )
  
  # Filter Age Range
  init_forecast_data <- init_forecast_data[age >= config$min_age & age <= config$max_age]
  
  # F. Save Final Outputs
  # -------------------------------------------------------------------------
  if (!boot_mode) {
    saveRDS(init_data_raw, file.path(config$path, "outputs", paste0("init_data_raw_", config$country, ".rds")))
    saveRDS(ever_smoke_data, file.path(config$path, "outputs", paste0("ever_smoke_data_", config$country, ".rds")))
    saveRDS(init_data_adj, file.path(config$path, "outputs", paste0("init_data_adj_", config$country, ".rds")))
    saveRDS(smk_init_data, file.path(config$path, "outputs", paste0("smk_init_data_", config$country, ".rds")))
    saveRDS(init_forecast_data, file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".rds")))
    write.csv(init_forecast_data, file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".csv")), row.names = FALSE)
  }
  
  if (boot_mode) {
    return(list(final = init_forecast_data, smk_init_data = smk_init_data))
  } else {
    return(invisible(init_forecast_data))
  }
}
