#' Statistically model trends in smoking status
#'
#' @description
#' Fits a multinomial logistic regression "response surface" to estimate
#' Current/Former/Never status. The model includes high-order polynomials
#' for Age and Year, and interactions with Sex and IMD.
#'
#' Two changes were made when the smoking trends were brought into the
#' bootstrap:
#'
#' 1. The prediction grid is now pinned by the caller rather than read off the
#'    data. Under resampling the observed min/max age and year, and the set of
#'    IMD quintiles present, can all shift between iterations. Left unpinned
#'    that silently produces replicates with different numbers of rows, which
#'    then cannot be stacked and cannot form a covariance matrix.
#'
#' 2. Predicting outside the observed range of age or year now has to be asked
#'    for. See the note on `allow_extrapolation` below.
#'
#' @param data Data table of survey data.
#' @param max_iterations Integer, passed to nnet::multinom.
#' @param age_var,year_var,sex_var,smoker_state_var,imd_var,weight_var Column names.
#' @param grid_ages Integer vector of ages to predict for. Defaults to the range
#'   observed in `data`, which is only safe outside bootstrap mode.
#' @param grid_years Integer vector of years to predict for. Defaults as above.
#' @param grid_sex Character vector of sex levels.
#' @param grid_imd Character vector of IMD quintile levels.
#' @param expected_states The smoking states the model must return a column for.
#' @param allow_extrapolation Logical. If FALSE (the default) it is an error for
#'   `grid_ages` or `grid_years` to reach beyond the range present in `data`.
#' @param boot_id Optional scalar written to a `boot_id` column on the output.
#'   Also suppresses the progress messages.
#' @param tol Tolerance for the "probabilities sum to one" check.
#' @importFrom data.table setDT := copy setattr uniqueN
#' @importFrom nnet multinom
#' @importFrom stats predict complete.cases
#' @export
trend_fit <- function(
    data,
    max_iterations = 1e3,
    age_var = "age",
    year_var = "year",
    sex_var = "sex",
    smoker_state_var = "smk.state",
    imd_var = "imd_quintile",
    weight_var = "wt_int",
    grid_ages = NULL,
    grid_years = NULL,
    grid_sex = c("Male", "Female"),
    grid_imd = NULL,
    expected_states = c("current", "former", "never"),
    allow_extrapolation = FALSE,
    boot_id = NULL,
    tol = 1e-8
) {

  quiet <- !is.null(boot_id)

  # 1. Prepare data ------------------------------------------------------------
  required <- c(smoker_state_var, age_var, year_var, sex_var, imd_var, weight_var)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("trend_fit: columns missing from `data`: ", paste(missing_cols, collapse = ", "))
  }

  model_data <- data.table(
    smk.state    = data[[smoker_state_var]],
    age          = data[[age_var]],
    year         = data[[year_var]],
    sex          = data[[sex_var]],
    imd_quintile = data[[imd_var]],
    wt_int       = data[[weight_var]]
  )

  # multinom drops incomplete rows without saying so. Count them here instead.
  n_incomplete <- sum(!stats::complete.cases(model_data))
  if (n_incomplete > 0) {
    stop("trend_fit: ", n_incomplete, " of ", nrow(model_data),
         " rows have a missing value in one of the model variables. ",
         "Drop or impute them upstream so the loss is explicit.")
  }
  if (any(model_data$wt_int < 0)) stop("trend_fit: negative survey weights.")
  if (sum(model_data$wt_int) <= 0) stop("trend_fit: survey weights sum to zero.")

  # Pin the factor levels so the design matrix is identical across resamples.
  # A resample containing no observations of some level would otherwise quietly
  # change the model and the shape of the output.
  if (is.null(grid_imd)) grid_imd <- sort(unique(as.character(model_data$imd_quintile)))

  observed_states <- sort(unique(as.character(model_data$smk.state)))
  if (!setequal(observed_states, expected_states)) {
    stop("trend_fit: smoking states in the data (", paste(observed_states, collapse = ", "),
         ") do not match expected_states (", paste(expected_states, collapse = ", "), ").")
  }
  observed_sex <- sort(unique(as.character(model_data$sex)))
  if (!setequal(observed_sex, grid_sex)) {
    stop("trend_fit: sex levels in the data (", paste(observed_sex, collapse = ", "),
         ") do not match grid_sex (", paste(grid_sex, collapse = ", "), ").")
  }
  observed_imd <- sort(unique(as.character(model_data$imd_quintile)))
  if (!setequal(observed_imd, grid_imd)) {
    stop("trend_fit: IMD quintiles in the data (", paste(observed_imd, collapse = ", "),
         ") do not match grid_imd (", paste(grid_imd, collapse = ", "), "). ",
         "A bootstrap resample has lost a quintile.")
  }

  model_data[, smk.state    := factor(as.character(smk.state), levels = expected_states)]
  model_data[, sex          := factor(as.character(sex), levels = grid_sex)]
  model_data[, imd_quintile := factor(as.character(imd_quintile), levels = grid_imd)]

  # Z-score standardisation for numerical stability in GLMs.
  # These come from the data in hand, so a resample shifts them very slightly.
  # That is fine: the same mu/sd are applied to the prediction grid below.
  mu_age  <- mean(model_data$age, na.rm = TRUE)
  sd_age  <- sqrt(var(model_data$age, na.rm = TRUE))
  mu_year <- mean(model_data$year, na.rm = TRUE)
  sd_year <- sqrt(var(model_data$year, na.rm = TRUE))
  if (sd_age == 0 || sd_year == 0) stop("trend_fit: no variation in age or year.")

  model_data[, age.z  := (age - mu_age) / (2 * sd_age)]
  model_data[, year.z := (year - mu_year) / (2 * sd_year)]

  # Compress the dataset by summing weights over identical profiles. Because
  # multinom weights the likelihood, the fit is unchanged and the design matrix
  # shrinks by roughly 95%.
  agg_data <- model_data[, .(wt_int = sum(wt_int, na.rm = TRUE)),
                         by = .(smk.state, age.z, year.z, sex, imd_quintile)]

  # 2. Fit model ---------------------------------------------------------------
  if (!quiet) message("  - Fitting multinomial response surface...")

  f_legacy <- smk.state ~
    age.z + year.z + I(age.z^2) + I(year.z^2) + age.z:year.z +
    I(age.z^3) + I(age.z^4) +
    I(year.z^3) +
    I(age.z^2):year.z + age.z:I(year.z^2) +
    sex + imd_quintile + sex:imd_quintile +
    age.z:sex + age.z:imd_quintile + age.z:sex:imd_quintile +
    year.z:sex + year.z:imd_quintile + year.z:sex:imd_quintile +
    age.z:year.z:sex + age.z:year.z:imd_quintile + age.z:year.z:sex:imd_quintile

  m_final <- nnet::multinom(f_legacy, data = agg_data, weights = wt_int,
                            maxit = max_iterations, trace = FALSE)

  # nnet sets convergence to 1 when it hits maxit, and says nothing about it.
  # Accepting that silently means the odd bootstrap replicate is a half-fitted
  # model quietly contributing to the variance.
  if (!is.null(m_final$convergence) && m_final$convergence != 0) {
    stop("trend_fit: multinom did not converge within ", max_iterations,
         " iterations", if (quiet) paste0(" (boot_id ", boot_id, ")") else "", ".")
  }

  # 3. Predict on grid ---------------------------------------------------------
  if (!quiet) message("  - Generating predictions...")

  if (is.null(grid_ages))  grid_ages  <- min(model_data$age):max(model_data$age)
  if (is.null(grid_years)) grid_years <- min(model_data$year):max(model_data$year)

  # ==========================================================================
  # EXTRAPOLATION - TEMPORARY, REMOVE WHEN HSE 2019 IS ADDED
  # --------------------------------------------------------------------------
  # For England the survey (HSE) currently ends in 2018, but the validation
  # targets in Tables 9 and 10 are defined over 2017-2019. We therefore predict
  # one year beyond the data, which means evaluating the cubic in year.z outside
  # the range it was fitted on. A cubic can turn sharply just past the edge of
  # its support, so the 2019 numbers carry more uncertainty than the bootstrap
  # spread will show. The bootstrap resamples the data. It does not resample the
  # decision to extrapolate, so that source of error is nowhere in the variance.
  # Treat 2019 prevalence, and any validation result that leans on it, as the
  # weakest number in the set.
  #
  # >>> WHEN HSE 2019 IS ADDED TO THIS WORKFLOW, in 10_run_smoking_transitions.R:
  #       config_eng$last_year                <- 2019
  #       config_eng$trend_last_year          <- 2019   (now equal to last_year)
  #       config_eng$trend_allow_extrapolation <- FALSE
  #     Nothing else needs deleting; the check below simply stops firing.
  #     The 2019 estimates WILL move when this happens, so any calibration or
  #     validation run against the extrapolated numbers must be repeated.
  # ==========================================================================
  extrap_years <- setdiff(grid_years, min(model_data$year):max(model_data$year))
  extrap_ages  <- setdiff(grid_ages,  min(model_data$age):max(model_data$age))

  if (length(extrap_years) > 0 || length(extrap_ages) > 0) {
    if (!allow_extrapolation) {
      stop("trend_fit: the grid reaches outside the data",
           if (length(extrap_years)) paste0(" (years: ", paste(extrap_years, collapse = ", "), ")") else "",
           if (length(extrap_ages))  paste0(" (ages: ",  paste(extrap_ages,  collapse = ", "), ")") else "",
           ". Set allow_extrapolation = TRUE if that is intended.")
    }
    if (!quiet) {
      message("  - Extrapolating beyond the survey data",
              if (length(extrap_years)) paste0(", years: ", paste(extrap_years, collapse = ", ")) else "",
              if (length(extrap_ages))  paste0(", ages: ",  paste(extrap_ages,  collapse = ", ")) else "")
    }
  }

  newdata <- data.table(expand.grid(
    age          = grid_ages,
    year         = grid_years,
    sex          = factor(grid_sex, levels = grid_sex),
    imd_quintile = factor(grid_imd, levels = grid_imd),
    stringsAsFactors = FALSE
  ))

  # Same z-score transformation as the fit
  newdata[, age.z  := (age - mu_age) / (2 * sd_age)]
  newdata[, year.z := (year - mu_year) / (2 * sd_year)]
  newdata[, cohort := year - age]

  probs <- stats::predict(m_final, newdata = newdata, type = "probs")

  # With only two outcome levels predict() returns a vector, not a matrix.
  # We have already checked for three, but check the shape rather than trust it.
  if (is.null(dim(probs)) || ncol(probs) != length(expected_states)) {
    stop("trend_fit: predict() returned ",
         if (is.null(dim(probs))) "a vector" else paste(ncol(probs), "columns"),
         " rather than one column per smoking state.")
  }
  if (!setequal(colnames(probs), expected_states)) {
    stop("trend_fit: predicted columns (", paste(colnames(probs), collapse = ", "),
         ") do not match expected_states.")
  }
  probs <- probs[, expected_states, drop = FALSE]   # fix column order

  newdata <- cbind(newdata, probs)
  newdata[, `:=`(age.z = NULL, year.z = NULL)]

  # 4. Validate the output shape and content -----------------------------------
  expected_rows <- length(grid_ages) * length(grid_years) * length(grid_sex) * length(grid_imd)
  if (nrow(newdata) != expected_rows) {
    stop("trend_fit: grid has ", nrow(newdata), " rows, expected ", expected_rows, ".")
  }
  if (anyNA(newdata)) stop("trend_fit: NA values in the prediction grid.")

  row_sums <- rowSums(as.matrix(newdata[, ..expected_states]))
  if (any(abs(row_sums - 1) > tol)) {
    stop("trend_fit: predicted probabilities do not sum to one (worst deviation ",
         signif(max(abs(row_sums - 1)), 3), ").")
  }
  if (newdata[, any(.SD < 0 | .SD > 1), .SDcols = expected_states]) {
    stop("trend_fit: predicted probabilities outside [0, 1].")
  }

  # Back to plain character so downstream merges on sex/imd behave predictably
  newdata[, sex := as.character(sex)]
  newdata[, imd_quintile := as.character(imd_quintile)]

  if (!is.null(boot_id)) newdata[, boot_id := boot_id]

  setcolorder(newdata, c(intersect(c("boot_id", "age", "year", "sex", "imd_quintile", "cohort"),
                                   names(newdata)), expected_states))

  # Record what was extrapolated so it travels with the object
  setattr(newdata, "extrapolated_years", extrap_years)
  setattr(newdata, "extrapolated_ages",  extrap_ages)

  return(newdata[])
}


#' Cut bootstrap trend draws down to what the targets actually need
#'
#' @description
#' The full England grid is 79 ages x 16 years x 2 sexes x 5 quintiles x 3
#' states. At 1,000 iterations that is roughly 38 million rows, and Tables 7 to
#' 10 need about a tenth of it. Thin before stacking rather than after.
#'
#' The thinning is checked. A filter that quietly matched nothing would produce
#' a raw bootstrap file that looks fine on disk and is missing a whole table.
#'
#' @param dt A single trend_fit output (one bootstrap iteration).
#' @param keep_ages,keep_years Integer vectors to retain.
#' @param keep_states Character vector of smoking state columns to retain.
#' @export
thin_trend_draws <- function(dt, keep_ages, keep_years, keep_states = "current") {

  missing_ages   <- setdiff(keep_ages,   unique(dt$age))
  missing_years  <- setdiff(keep_years,  unique(dt$year))
  missing_states <- setdiff(keep_states, names(dt))

  if (length(missing_ages) > 0) {
    stop("thin_trend_draws: ages not in the trend grid: ", paste(missing_ages, collapse = ", "))
  }
  if (length(missing_years) > 0) {
    stop("thin_trend_draws: years not in the trend grid: ", paste(missing_years, collapse = ", "))
  }
  if (length(missing_states) > 0) {
    stop("thin_trend_draws: smoking state columns not in the trend grid: ",
         paste(missing_states, collapse = ", "))
  }

  keep_cols <- c(intersect(c("boot_id", "age", "year", "sex", "imd_quintile"), names(dt)), keep_states)
  out <- dt[age %in% keep_ages & year %in% keep_years, ..keep_cols]

  expected_rows <- length(keep_ages) * length(keep_years) *
    uniqueN(dt$sex) * uniqueN(dt$imd_quintile)
  if (nrow(out) != expected_rows) {
    stop("thin_trend_draws: kept ", nrow(out), " rows, expected ", expected_rows, ".")
  }

  return(out[])
}
