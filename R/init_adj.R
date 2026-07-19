#' Adjust initiation curves for recall bias
#'
#' @description
#' Scales the cohort initiation curves from init_est() so that the cumulative
#' probability of ever smoking at the reference age matches the trend in ever
#' smoking estimated by ever_smoke(). init_est() gives us the shape of the curve
#' (the distribution of starting ages among people who ever start); this
#' function pins down the level.
#'
#' @details
#' The trend target is the level of ever smoking at age ref_age: ever_smoke()
#' estimates it on 25-34 year olds and we map each year's value to the cohort
#' that is ref_age in that year. For cohorts old enough to have been surveyed at
#' ref_age, the raw curve and the target refer to the same age and the scalar is
#' simply target / raw.
#'
#' Younger cohorts have not been seen that far. Their raw curve stops at
#' whatever age the surveys last observed them, and its value there is missing
#' the initiation that happens between that age and ref_age. Dividing the age-30
#' target by, say, an age-21 curve quietly assumes nobody starts between 21 and
#' 30 - and the cohorts we did see all the way say that is wrong by about 5% at
#' 21 and 2% at 24. So for truncated cohorts we first complete the raw value,
#' multiplying it by F(ref_age) / F(r) averaged over the most recent fully
#' observed cohorts within sex and IMD quintile. That trades the "initiation
#' stops dead at the truncation age" assumption for "the timing of initiation,
#' conditional on ever starting, is stable across adjacent cohorts", which is a
#' much weaker thing to assume and one we can check in the data.
#'
#' Initiation is still assumed to stop after ref_age: the curve is carried
#' forward flat from there.
#'
#' @param init_data Data table - raw estimates from init_est().
#' @param ever_smoke_data Data table - trend targets from ever_smoke().
#' @param ref_age Integer - the index age for calibration (default 30).
#' @param fix_ref_age Logical. If TRUE, forces ref_age even if data is sparse.
#' @param min_ref Integer - youngest reference age a truncated cohort may
#' calibrate at. With the completion step this can sit lower than it used to:
#' the old guard against truncation bias was to refuse cohorts seen only to
#' their early twenties, whereas now the bias is corrected rather than avoided.
#' It should not go so low that F(r) is a small and noisy fraction of F(ref_age).
#' @param cohorts Integer vector - cohorts to adjust.
#' @param period_start Integer - first year of data.
#' @param period_end Integer - last year of data.
#' @param n_completion_cohorts Integer - how many of the most recent fully
#' observed cohorts to average the completion ratios over.
#' @importFrom data.table := setDT setnames copy CJ
#' @export
init_adj <- function(
    init_data,
    ever_smoke_data,
    ref_age = 30,
    fix_ref_age = FALSE,
    min_ref = 18,
    cohorts = 1973:2020,
    period_start = 2003,
    period_end = 2018,
    n_completion_cohorts = 10
) {

  dt <- copy(init_data)

  # 1. Determine Reference Ages
  # Dynamic reference age allows us to use older cohorts where we only have older data
  if(fix_ref_age == FALSE) {
    dt[cohort <= (period_end - ref_age), ref_ages := ref_age]
    dt[cohort > (period_end - ref_age), ref_ages := period_end - cohort]
    dt[cohort < (period_start - ref_age), ref_ages := period_start - cohort]

    # Filter out cohorts where the ref age is too young to be reliable
    dt <- dt[ref_ages >= min_ref]
  } else {
    dt[, ref_ages := ref_age]
  }

  # 2. Completion factors for cohorts not observed to ref_age
  #
  # Built from the most recent cohorts that were seen all the way to ref_age:
  # for each truncation age r, the average of F(ref_age) / F(r) within sex and
  # IMD quintile. A truncated cohort's raw value gets multiplied by this before
  # the scalar is computed, so the target and the raw value refer to the same
  # age. Complete cohorts get a factor of exactly 1 and come out of this
  # function unchanged from before.

  complete_cohorts <- sort(unique(dt[ref_ages == ref_age, cohort]))

  if(length(complete_cohorts) < n_completion_cohorts) {
    stop("init_adj: only ", length(complete_cohorts), " cohorts are observed all the ",
         "way to age ", ref_age, ", but n_completion_cohorts = ", n_completion_cohorts,
         ". Either the data window is too short or ref_age is set too high.")
  }

  comp_basis <- utils::tail(complete_cohorts, n_completion_cohorts)

  f_top <- dt[cohort %in% comp_basis & age == ref_age,
              .(cohort, sex, imd_quintile, f_ref = p_ever_smoker)]

  comp <- merge(
    dt[cohort %in% comp_basis & age >= min_ref & age < ref_age,
       .(cohort, sex, imd_quintile, age, f_r = p_ever_smoker)],
    f_top, by = c("cohort", "sex", "imd_quintile"))

  # Zero at F(r) in a basis cohort would put an infinity into the mean. The
  # basis cohorts are essentially complete curves at these ages, so a zero
  # means something has gone wrong upstream rather than a sparse cell.
  n_zero <- comp[f_r <= 0, .N]
  if(n_zero > 0) {
    stop("init_adj: ", n_zero, " completion-basis cells have a raw cumulative ",
         "probability of zero at ages ", min_ref, "-", ref_age - 1, " (cohorts ",
         min(comp_basis), "-", max(comp_basis), "). These cohorts are fully ",
         "observed, so their curves should be well above zero there. Check what ",
         "init_est received.")
  }

  comp <- comp[, .(completion = mean(f_ref / f_r)), by = .(sex, imd_quintile, age)]

  # The raw curves are cumulative, so F(ref_age) >= F(r) within a cohort and
  # every completion factor must be at least 1. If one is not, the input is not
  # a set of cumulative curves.
  if(comp[completion < 1 - 1e-9, .N] > 0) {
    stop("init_adj: completion factors below 1 - the raw curves are not ",
         "monotone in age. init_est output should be cumulative.")
  }

  # 3. Calculate Adjustment Factors
  # We look at the cumulative prob at the reference age in the raw data
  # vs the modeled trend data.

  # Get raw values at reference age
  ref_data <- dt[age == ref_ages]

  # Complete the truncated ones up to their ref_age equivalent
  ref_data <- merge(ref_data, comp, by = c("sex", "imd_quintile", "age"), all.x = TRUE)
  ref_data[ref_ages == ref_age, completion := 1]

  if(ref_data[is.na(completion), .N] > 0) {
    stop("init_adj: no completion factor for reference ages ",
         paste(sort(unique(ref_data[is.na(completion), age])), collapse = ", "),
         ". They should exist for every age from min_ref up to ref_age.")
  }

  n_trunc <- ref_data[completion > 1, uniqueN(cohort)]
  if(n_trunc > 0) {
    message("   init_adj: completing ", n_trunc, " truncated cohorts before ",
            "calibration (factors ",
            round(ref_data[completion > 1, min(completion)], 3), " to ",
            round(ref_data[completion > 1, max(completion)], 3),
            ", basis cohorts ", min(comp_basis), "-", max(comp_basis), ")")
  }

  # Prepare trend data
  evr_smk_ref <- copy(ever_smoke_data)
  evr_smk_ref[, cohort := year - ref_age]
  evr_smk_ref[, year := NULL]

  # Merge
  ref_data <- merge(ref_data, evr_smk_ref, by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)

  # Calculate scalar: Target / (Raw, completed to ref_age)
  ref_data[, adjustment_factor := fitted_trends / (p_ever_smoker * completion)]

  # A raw value of zero at the reference age makes the scalar infinite, and this
  # used to pass through as a silent NA or Inf. For a fully observed cohort it
  # means broken input, so stop. For a truncated cohort it is a sparse stratum
  # that cannot calibrate on its own numbers, so send that cohort - and any
  # younger ones, to keep the handover to the extension in one piece - down the
  # extension pathway instead, and say so.
  bad_complete <- ref_data[ref_ages == ref_age & !is.finite(adjustment_factor), unique(cohort)]
  if(length(bad_complete) > 0) {
    stop("init_adj: non-finite adjustment factors for fully observed cohorts ",
         paste(bad_complete, collapse = ", "),
         ". Their raw curves or trend targets are zero or missing at age ",
         ref_age, ".")
  }

  bad_trunc <- ref_data[ref_ages < ref_age & !is.finite(adjustment_factor), unique(cohort)]
  if(length(bad_trunc) > 0) {
    drop_from <- min(bad_trunc)
    message("   init_adj: cohorts ", drop_from, " onwards cannot calibrate on their ",
            "own data (zero or missing raw values at their reference age). They ",
            "fall back to the extrapolated average profile.")
    dt <- dt[cohort < drop_from]
    ref_data <- ref_data[cohort < drop_from]
  }

  # Merge scalar back to main data
  dt <- merge(dt, ref_data[, .(cohort, sex, imd_quintile, adjustment_factor)],
              by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)

  # Apply adjustment
  dt[, p_ever_smoker_adj := p_ever_smoker * adjustment_factor]

  # 4. Standardization and Extrapolation
  # We need a full grid of values.
  domain <- data.table(expand.grid(
    cohort = unique(dt$cohort),
    sex = unique(dt$sex),
    imd_quintile = unique(dt$imd_quintile),
    age = unique(dt$age)
  ))

  domain <- merge(domain, dt[, .(cohort, sex, imd_quintile, age, ref_ages, p_ever_smoker_adj)],
                  by = c("cohort", "sex", "imd_quintile", "age"), all.x = TRUE)

  # Fill missing ref_ages within groups
  domain[, ref_ages := unique(ref_ages[!is.na(ref_ages)]), by = .(cohort, sex, imd_quintile)]

  # LOCF (Last Observation Carried Forward) for ages > ref_age
  # Initiation is assumed to stop/flatten after the reference age (usually 30)
  domain[, last_val := p_ever_smoker_adj[age == ref_ages], by = .(cohort, sex, imd_quintile)]
  domain[age > ref_ages, p_ever_smoker_adj := last_val]

  domain[, `:=`(last_val = NULL, ref_ages = NULL)]

  # 5. Handle Future Cohorts (Extrapolation)
  maxc <- max(domain$cohort)

  if(max(cohorts) > maxc) {

    # Create extension grid
    cohorts_ext <- min(domain$cohort):max(cohorts)

    domain_ex <- data.table(expand.grid(
      cohort = cohorts_ext,
      sex = unique(dt$sex),
      imd_quintile = unique(dt$imd_quintile),
      age = unique(dt$age)
    ))

    domain_ex <- merge(domain_ex, domain, by = c("cohort", "sex", "imd_quintile", "age"), all.x = TRUE)

    # Calculate average profile of the last 5 observed cohorts
    data_av <- domain[cohort %in% (maxc - 5):maxc, .(av10 = mean(p_ever_smoker_adj, na.rm = TRUE)),
                      by = .(age, sex, imd_quintile)]

    domain_ex <- merge(domain_ex, data_av, by = c("age", "sex", "imd_quintile"), all.x = TRUE)

    # Fill future cohorts with average profile
    domain_ex[cohort > maxc, p_ever_smoker_adj := av10]
    domain_ex[, av10 := NULL]

    # Re-apply trend adjustment to these extrapolated cohorts
    # (Same logic as above, but for future cohorts using projected trends)
    domain_ex[cohort > maxc, ref_ages := ref_age]

    ref_data_ex <- domain_ex[cohort > maxc & age == ref_ages]

    evr_smk_ref <- copy(ever_smoke_data)
    evr_smk_ref[, cohort := year - ref_age]

    ref_data_ex <- merge(ref_data_ex, evr_smk_ref[, .(cohort, sex, imd_quintile, fitted_trends)],
                         by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)

    ref_data_ex[, adjustment_factor := fitted_trends / p_ever_smoker_adj]

    domain_ex <- merge(domain_ex, ref_data_ex[, .(cohort, sex, imd_quintile, adjustment_factor)],
                       by = c("cohort", "sex", "imd_quintile"), all.x = TRUE)

    domain_ex[cohort > maxc, p_ever_smoker_adj := p_ever_smoker_adj * adjustment_factor]

    # LOCF for future
    domain_ex[cohort > maxc, last_val := p_ever_smoker_adj[age == ref_ages], by = .(cohort, sex, imd_quintile)]
    domain_ex[cohort > maxc & age > ref_ages, p_ever_smoker_adj := last_val]

    domain_ex[, `:=`(ref_ages = NULL, adjustment_factor = NULL, last_val = NULL)]

    domain <- domain_ex
  }

  domain[, year := cohort + age]

  return(domain[])
}
