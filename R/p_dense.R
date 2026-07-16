#' Monotone smooth of a cumulative curve over age
#'
#' @description
#' Internal helper for p_dense. Fits a smoothing spline to the cumulative
#' probability of ever smoking over age within one cohort/sex/IMD group, clamps
#' it to [0, 1], then forces it to be monotone non-decreasing.
#'
#' @details
#' The cumulative curve coming out of init_adj is a step function. init_est
#' builds it from a weighted count of the ages at which people report starting,
#' so an age at which nobody in that cohort happened to report starting gets a
#' hazard of exactly zero and the curve goes flat. Differencing a step function
#' gives a density that is zero roughly half the time and spiky the rest, which
#' is why p_smooth was then blanking and interpolating the zeros.
#'
#' Smoothing the cumulative curve first and differencing afterwards avoids all
#' of that. The curve is the thing we actually estimated, the density is derived
#' from it, so the curve is the right thing to smooth. Because cummax makes the
#' result monotone, the differenced density is non-negative by construction and
#' the clamps in p_dense become a formality rather than load-bearing.
#'
#' @param p Numeric vector - cumulative probabilities for one group.
#' @param a Numeric vector - ages, same length as p.
#' @param df Numeric - degrees of freedom for the spline. Higher follows the raw
#' curve more closely. 6 over a ~20 year age range keeps the shape of the ogive
#' without chasing the steps.
#' @importFrom stats smooth.spline predict
#' @return Numeric vector the same length as p.
#' @keywords internal
smooth_cdf <- function(p, a, df = 6) {

  ok <- is.finite(p) & is.finite(a)

  # Nothing to fit: too few points, or the curve is flat (e.g. cohorts that are
  # all zero because they are too young to have started). Hand back untouched.
  if(sum(ok) < 5 || diff(range(p[ok])) < 1e-8) return(p)

  # df cannot exceed the number of distinct ages we have
  df_use <- min(df, max(4, sum(ok) - 2))

  fit <- try(stats::smooth.spline(a[ok], p[ok], df = df_use), silent = TRUE)

  if(inherits(fit, "try-error")) {
    warning("smooth_cdf: smooth.spline failed for a group; returning the raw curve. ",
            "The density for this group will keep the old step-function behaviour.")
    return(p)
  }

  out <- rep(NA_real_, length(p))
  out[ok] <- stats::predict(fit, a[ok])$y

  # A spline through a monotone curve can still dip slightly. Clamp to [0,1]
  # first, then cummax, so that the difference downstream is never negative.
  out <- pmin(pmax(out, 0), 1)
  out[ok] <- cummax(out[ok])

  out
}


#' Convert probabilities of ever-smoking to age-specific probabilities
#'
#' @description
#' Converts the Cumulative Distribution Function (CDF) of ever-smoking 
#' into the Probability Density Function (PDF), which represents the 
#' probability of initiating smoking at a specific age.
#'
#' @details
#' The cumulative curve is smoothed over age within each group before it is
#' differenced (see smooth_cdf). Set cdf_smooth_df = NULL to skip that and get
#' the old behaviour back, which is useful for comparing runs, but note that the
#' old behaviour is chaotic: see the note on blank_zeros in p_smooth.
#'
#' @param data Data table with cumulative probabilities.
#' @param cum_func_var Character - name of cumulative variable.
#' @param strat_vars Character vector - stratification variables.
#' @param lowest_year integer - start year filter.
#' @param max_year integer - end year filter.
#' @param cdf_smooth_df Numeric - degrees of freedom for the monotone smooth of
#' the cumulative curve over age. NULL or 0 skips the smooth and restores the
#' pre-2026 behaviour.
#' @importFrom data.table shift := copy setnames rbindlist setorderv
#' @export
p_dense <- function(
    data,
    cum_func_var,
    strat_vars = c("cohort", "sex", "imd_quintile"),
    lowest_year = 2003,
    max_year = 2100,
    cdf_smooth_df = 6
) {
  
  dt <- copy(data)

  if(!all(c("age", "year") %in% names(dt))) {
    stop("p_dense: data must contain 'age' and 'year'.")
  }
  if(!cum_func_var %in% names(dt)) {
    stop("p_dense: '", cum_func_var, "' is not a column in data.")
  }

  # shift() below takes the next row as the next age, so the sort is not
  # cosmetic. It was relying on init_adj happening to return sorted rows.
  setorderv(dt, c(strat_vars, "age"))

  # 0. Smooth the cumulative curve over age, within group, before differencing
  #    it. See smooth_cdf for why this is the right place to do the smoothing.

  if(!is.null(cdf_smooth_df) && cdf_smooth_df > 0) {

    dt[, (cum_func_var) := smooth_cdf(get(cum_func_var), age, df = cdf_smooth_df),
       by = strat_vars]

    if(anyNA(dt[[cum_func_var]])) {
      warning("p_dense: the smoothed cumulative curve contains NAs. ",
              "These will become zeros in the density.")
    }
  }
  
  # 1. Calculate PDF from CDF
  # p_init = 1 - (Survival_t+1 / Survival_t)
  # Where Survival = 1 - Cumulative_Ever_Smoker
  
  # Create Lead variable
  # data.table:: is spelled out because raster exports an S4 shift() and masks
  # this one if it is attached second. The importFrom covers us inside the
  # package, but not in a bare script that does library(raster).
  dt[, (paste0(cum_func_var, "_lead1")) := data.table::shift(get(cum_func_var), type = "lead"), by = strat_vars]
  
  # Calculate conditional probability
  dt[, initiation_pdf := (1 - ((1 - get(paste0(cum_func_var, "_lead1"))) / 
                                 (1 - get(cum_func_var))))]
  
  # 2. Safety Clamping
  # Numerical noise can cause <0 or >1. With the smooth on, cummax means we
  # should not be seeing negatives at all, so count them and complain if we do.

  n_neg <- sum(dt$initiation_pdf < 0, na.rm = TRUE)

  if(!is.null(cdf_smooth_df) && cdf_smooth_df > 0 && n_neg > 0) {
    warning("p_dense: ", n_neg, " negative densities after differencing a curve ",
            "that cummax should have made monotone. Check smooth_cdf.")
  }

  dt[is.na(initiation_pdf), initiation_pdf := 0]
  dt[initiation_pdf < 0, initiation_pdf := 0]
  dt[initiation_pdf > 1, initiation_pdf := 1]
  
  # Cleanup
  dt[, (paste0(cum_func_var, "_lead1")) := NULL]
  
  # Filter relevant ages/years
  smk_init_data <- dt[age >= 10 & age <= 30 & year >= lowest_year & year <= max_year, 
                      .(sex, imd_quintile, age, year, initiation_pdf)]
  
  setnames(smk_init_data, "initiation_pdf", "p_start")
  
  # 3. Smoothing
  # We apply smoothing within subgroups to reduce jaggedness
  
  # Define subgroups
  subgroups <- expand.grid(
    sex = c("Male", "Female"),
    imd = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
    stringsAsFactors = FALSE
  )
  
  smoothed_list <- list()
  
  for(i in 1:nrow(subgroups)) {
    sx <- subgroups$sex[i]
    md <- subgroups$imd[i]
    
    subset_data <- smk_init_data[sex == sx & imd_quintile == md]
    
    # DEBUG: Check if data actually exists here
    if(nrow(subset_data) == 0) {
      message(paste("Skipping: No data found for", sx, md))
      next
    }
    
    # Check if all values are NA
    if(all(is.na(subset_data$p_start))) {
      message(paste("Skipping: All values are NA for", sx, md))
      next
    }
    
    # Apply smoothing function
    # blank_zeros = FALSE because the cumulative curve has already been smoothed
    # above, so a zero here means initiation really has stopped at that age
    # rather than nothing having been observed. Blanking them would interpolate
    # initiation back into ages where there is none, and would make the whole
    # step chaotic (0.1% in, 51% out on the England data).
    smoothed <- p_smooth(
      data = subset_data, 
      value_var = "p_start", 
      window_size = 5,
      blank_zeros = is.null(cdf_smooth_df) || cdf_smooth_df <= 0
    )
    smoothed[, `:=`(sex = sx, imd_quintile = md)]
    smoothed_list[[i]] <- smoothed
    
  }
  
  final_data <- rbindlist(smoothed_list, use.names = TRUE)
  
  return(final_data[])
}