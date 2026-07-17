# Shared functions for validating the smktrans transition probabilities against
# the Smoking Toolkit Study (STS).
#
# The general approach is to derive, from the STS, a quantity that is defined
# the same way as one of our estimated probabilities, and then plot the two
# against each other with uncertainty on both.
#
# Everything that could quietly go wrong is checked and stops. The point of this
# folder is to tell us whether the estimates are right, so it is no use to us if
# it fails in a way that looks like a result.

library(data.table)
library(ggplot2)


# ---------------------------------------------------------------------------
# Paths. All input data lives in 05_input, all estimates come from the outputs
# folder of the England estimation run.
# ---------------------------------------------------------------------------

val_paths <- list(
  input        = "05_input",
  toolkit_file = "omni205_39.1_65.2cot_31.3a_25.4s_recodes_91.5sa",
  pop_file     = "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv",
  estimates    = "transition_probability_estimates/src_england/outputs",
  intermediate = "transition_probability_validation/intermediate_data"
)


#' Convert an STS wave number to a calendar year
#'
#' STS wave 1 was fielded in November 2006 and one wave has been fielded each
#' month since, so the wave number maps onto a month and year arithmetically.
#'
#' This mapping is the single biggest hidden assumption in the old code, which
#' hard-coded "xwave >= 148" with a comment saying 2019 and left the reader to
#' take that on trust. Note that the old commented-out filter for "2013-2018"
#' was xwave 75:147, and wave 147 is January 2019, so that filter was picking up
#' an extra month. Worth knowing if we ever compare against the older results.
#'
#' CHECK THIS against the toolkit documentation before relying on it. If wave 1
#' is not November 2006 then every year label in this folder is wrong.
sts_wave_to_year <- function(wave) {
  stopifnot(is.numeric(wave), all(wave >= 1, na.rm = TRUE))
  # wave 1 = month 11 of 2006, so shift by 10 months and divide
  2006L + (wave - 1L + 10L) %/% 12L
}

sts_wave_to_month <- function(wave) {
  ((wave - 1L + 10L) %% 12L) + 1L
}

#' Which waves cover a set of calendar years, completely
#'
#' Returns only whole years. A year that is only part-covered by the available
#' waves is dropped, with a message, rather than silently contributing a
#' seasonally biased average.
sts_waves_for_years <- function(years, max_wave) {
  w <- 1:max_wave
  dt <- data.table(wave = w, year = sts_wave_to_year(w), month = sts_wave_to_month(w))
  have <- dt[year %in% years, .(n_months = uniqueN(month)), by = year]
  full <- have[n_months == 12]$year
  part <- setdiff(years, full)
  if (length(part) > 0) {
    message("sts_waves_for_years: dropping ", paste(part, collapse = ", "),
            " - not covered by 12 whole months of waves.")
  }
  if (length(full) == 0) stop("sts_waves_for_years: no complete years in the requested range.")
  dt[year %in% full]$wave
}


# ---------------------------------------------------------------------------
# Reading and cleaning the STS
# ---------------------------------------------------------------------------

#' Read and clean the STS data for England
#'
#' @param years Integer vector - the calendar years to keep.
#' @param ages Integer vector - the ages to keep.
sts_read_england <- function(years, ages = 16:89) {

  if (!requireNamespace("toolkitr", quietly = TRUE)) {
    stop("sts_read_england: the toolkitr package is not installed.")
  }

  raw <- toolkitr::ReadToolkit(path_in = val_paths$input,
                               data_in = val_paths$toolkit_file,
                               save = FALSE)
  setDT(raw)

  required <- c("gore", "actage", "xwave", "Aweight0", "q632b8", "q632b9")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("sts_read_england: the toolkit file is missing: ", paste(missing, collapse = ", "))
  }

  n_raw <- nrow(raw)

  # England only. The STS covers Great Britain, and gore is the government
  # office region, so England is the nine English regions.
  eng_regions <- c("East of England", "South West", "South East", "North East",
                   "East Midlands", "West Midlands", "North West",
                   "Yorkshire and The Humber", "London")
  unknown_regions <- setdiff(unique(na.omit(raw$gore)),
                             c(eng_regions, "Scotland", "Wales"))
  if (length(unknown_regions) > 0) {
    stop("sts_read_england: unrecognised values of gore, so the England filter ",
         "cannot be trusted: ", paste(unknown_regions, collapse = ", "))
  }
  raw <- raw[gore %in% eng_regions]

  # Waves -> years. Keep only whole years.
  keep_waves <- sts_waves_for_years(years, max_wave = max(raw$xwave, na.rm = TRUE))
  raw <- raw[xwave %in% keep_waves]
  raw[, year := sts_wave_to_year(xwave)]

  raw <- raw[!is.na(actage) & actage >= min(ages) & actage <= max(ages)]

  raw[, id := .I]

  clean_demo  <- toolkitr::ToolkitCleanDemographic(
    data = raw,
    age_categories   = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"),
    age_cat_start_age = c(16, 18, 25, 35, 50, 65, 75))
  clean_smoke <- toolkitr::ToolkitCleanSmkStatus(data = raw)

  keep <- raw[, .(id, xwave, year, weight_england = Aweight0, q632b8, q632b9)]
  dt <- merge(keep, clean_demo,  by = "id", sort = FALSE, all.x = TRUE, all.y = FALSE)
  dt <- merge(dt,   clean_smoke, by = "id", sort = FALSE, all.x = TRUE, all.y = FALSE)

  # The weight is the whole basis of every estimate here, so a missing or
  # non-positive weight is not something to quietly drop.
  n_bad_wt <- dt[is.na(weight_england) | weight_england <= 0, .N]
  if (n_bad_wt > 0) {
    stop("sts_read_england: ", n_bad_wt, " rows have a missing or non-positive ",
         "weight_england. Decide what these are before going further.")
  }

  if (!"smoker_status" %in% names(dt)) {
    stop("sts_read_england: toolkitr did not return smoker_status.")
  }

  message(sprintf("sts_read_england: %d rows read, %d kept (England, %s, ages %d-%d).",
                  n_raw, nrow(dt), paste(range(years), collapse = "-"),
                  min(ages), max(ages)))
  dt[]
}


# ---------------------------------------------------------------------------
# The STS estimators
# ---------------------------------------------------------------------------

#' Probability of quitting in the last year, by age
#'
#' Two variables are used. The first is current smoking status. The second is a
#' counter-factual status that turns people who are currently ex-smokers back
#' into smokers if their most recent quit attempt began less than a year ago,
#' i.e. what their status would have been a year ago.
#'
#' p_quit is then 1 - P(smoker now) / P(smoker a year ago), which is the share
#' of a year ago's smokers who have since stopped.
#'
#' Note what this does with ex-smokers whose q632b8 is missing. They stay
#' ex-smokers, so they are treated as long-term quitters and drop out of the
#' denominator. If missingness on q632b8 is related to how recently someone
#' quit, this biases p_quit down. sts_check_q632b8() reports how big that group
#' is so we can decide whether it matters.
sts_quit_by_age <- function(dt, domain_ages) {

  recent <- c("In the last week",
              "More than a week and up to a month",
              "More than 1 month and up to 2 months",
              "More than 2 months and up to 3 months",
              "More than 3 months and up to 6 months",
              "More than 6 months and up to a year")

  d <- copy(dt)
  d[, smk_now := as.integer(smoker_status == "current_smoker")]
  d[, smk_year_ago := smk_now]
  d[smoker_status == "ex_smoker" & q632b8 %in% recent, smk_year_ago := 1L]

  s <- d[, .(smkt  = sum(smk_now      * weight_england) / sum(weight_england),
             smkt1 = sum(smk_year_ago * weight_england) / sum(weight_england),
             n     = .N),
         by = .(age = actage)]

  s <- merge(data.table(age = domain_ages), s, by = "age", all.x = TRUE, sort = TRUE)

  # An age with no smokers a year ago has no quit probability. That is missing,
  # not zero. The old code set it to zero, which pulled the bootstrap mean down
  # at exactly the ages where the data is thinnest.
  s[, p_quit := ifelse(!is.na(smkt1) & smkt1 > 0, 1 - smkt / smkt1, NA_real_)]
  s[]
}

#' Current-smoker prevalence by age
sts_prev_by_age <- function(dt, domain_ages) {
  d <- copy(dt)
  d[, smk_now := as.integer(smoker_status == "current_smoker")]
  s <- d[, .(prev = sum(smk_now * weight_england) / sum(weight_england), n = .N),
         by = .(age = actage)]
  merge(data.table(age = domain_ages), s, by = "age", all.x = TRUE, sort = TRUE)[]
}


#' Bootstrap an STS estimator
#'
#' The toolkit has no clustering or stratification, just a calibration weight,
#' so individuals are resampled with replacement uniformly and the weighted
#' estimator is applied to each resample.
#'
#' The old code sampled with probability proportional to weight AND then applied
#' the weights again inside the summary, which counts them twice. Pick one. The
#' weights belong in the estimator, so the resample is uniform.
#'
#' @param dt Data table of individual STS records.
#' @param fn Function(dt, domain_ages) returning a data table with columns
#'   age and `value_var`, one row per age in domain_ages.
#' @param value_var Character - the column of fn's output to bootstrap.
#' @param domain_ages Integer vector.
#' @param B Integer - number of bootstrap iterations.
#' @param seed Integer - REQUIRED. An unseeded bootstrap cannot be reproduced,
#'   and we have been bitten by that already elsewhere in this package.
sts_boot <- function(dt, fn, value_var, domain_ages, B = 1000, seed = NULL) {

  if (is.null(seed)) stop("sts_boot: a seed is required. Without one the plot cannot be reproduced.")
  if (B < 100) warning("sts_boot: B = ", B, " is low for 2.5th/97.5th percentiles.")

  set.seed(seed)
  n <- nrow(dt)
  iter_seeds <- sample.int(.Machine$integer.max, B)

  out <- matrix(NA_real_, nrow = length(domain_ages), ncol = B)

  for (i in seq_len(B)) {
    set.seed(iter_seeds[i])
    idx <- sample.int(n, n, replace = TRUE)
    res <- fn(dt[idx], domain_ages)
    out[, i] <- res[[value_var]]
  }

  # NAs here mean the estimator was undefined in that draw, not zero. Carry the
  # count through so the caller can see how much of each age is being thrown away.
  n_na <- rowSums(is.na(out))

  data.table(
    age      = domain_ages,
    est      = apply(out, 1, mean,     na.rm = TRUE),
    lower    = apply(out, 1, quantile, 0.025, na.rm = TRUE),
    upper    = apply(out, 1, quantile, 0.975, na.rm = TRUE),
    n_draws_undefined = n_na,
    frac_undefined    = n_na / B
  )
}

#' How many ex-smokers have no answer on q632b8?
#'
#' These fall out of the quit denominator. Run this before trusting p_quit.
sts_check_q632b8 <- function(dt) {
  ex <- dt[smoker_status == "ex_smoker"]
  n_na <- ex[is.na(q632b8) | q632b8 %in% c("Don't know", "Refused"), .N]
  message(sprintf(paste0("sts_check_q632b8: %d of %d ex-smokers (%.1f%%) have no usable ",
                         "answer.\n  These are treated as long-term quitters and drop out of ",
                         "the p_quit denominator.\n  If that share is large, p_quit is biased down."),
                  n_na, nrow(ex), 100 * n_na / nrow(ex)))
  invisible(n_na / nrow(ex))
}



#' Net initiation from STS prevalence, by age
#'
#' The STS is a repeat cross-section so it cannot see anyone start smoking. What
#' it can see is the number of current smokers rising with age, and that rise is
#' initiation net of quitting. That is the quantity calculate_net_initiation()
#' produces:
#'
#'   p_start_net = (n_current(a+1) - n_current(a)) / (n_never(a) + n_former(a))
#'
#' and since never, current and former sum to a constant the denominator is the
#' people who are not currently smoking, so it reduces to
#'
#'   p_start_net = (prev(a+1) - prev(a)) / (1 - prev(a))
#'
#' Prevalence is smoothed over age and then differenced, in that order.
#' Differencing a noisy curve gives a very noisy derivative and repairing that
#' afterwards is worse than not making the mess. This is the same lesson as
#' p_dense: smooth the thing you estimated, then take the difference of it.
#'
#' The old 22_prepare_toolkit_data_init.R did cumsum(prevalence), fitted a gam to
#' that, and differenced the fit, which is a roundabout way of doing the same
#' thing because the cumsum and the diff cancel. Done directly here.
#'
#' @param dt Individual STS records.
#' @param domain_ages Integer vector.
#' @param smooth_df Numeric - degrees of freedom for the prevalence smooth.
sts_net_init_by_age <- function(dt, domain_ages, smooth_df = 6) {

  s <- sts_prev_by_age(dt, domain_ages)

  ok <- !is.na(s$prev)
  if (sum(ok) < 5) return(data.table(age = domain_ages, p_start_net = NA_real_))

  fit <- try(stats::smooth.spline(s$age[ok], s$prev[ok],
                                  df = min(smooth_df, max(4, sum(ok) - 2))),
             silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(data.table(age = domain_ages, p_start_net = NA_real_))
  }

  s[, prev_sm := NA_real_]
  s[ok, prev_sm := stats::predict(fit, age)$y]
  s[, prev_sm := pmin(pmax(prev_sm, 0), 1)]

  s[, prev_next := data.table::shift(prev_sm, type = "lead")]
  s[, p_start_net := (prev_next - prev_sm) / (1 - prev_sm)]

  s[, .(age, p_start_net)]
}


# ---------------------------------------------------------------------------
# The smktrans estimates
# ---------------------------------------------------------------------------

#' Load one of the England estimation outputs
stapm_load <- function(name) {
  f <- file.path(val_paths$estimates, name)
  if (!file.exists(f)) {
    stop("stapm_load: ", f, " not found. Has 10_run_smoking_transitions.R been run for England?")
  }
  x <- readRDS(f)
  if (is.list(x) && !is.data.frame(x) && "data" %in% names(x)) x <- x$data
  setDT(x)
  x[]
}

#' Collapse a model estimate over year, sex and IMD using ONS populations
#'
#' The STS figure is a population-weighted average over whoever is actually in
#' England. A plain mean over the sex x IMD x year cells weights all of them
#' equally, which is not the same thing and is not what we are comparing
#' against. The old code did a plain mean and carried a comment saying it
#' shouldn't.
#'
#' The population file only runs to 2019. For later years we hold the 2019
#' population structure. That is an assumption about the age/sex/IMD
#' composition, not about smoking, and it is applied loudly rather than
#' silently.
pop_weight_by_age <- function(model_dt, value_var, years, ages) {

  pops <- fread(val_paths$pop_file)
  if (!"N" %in% names(pops) && "pops" %in% names(pops)) setnames(pops, "pops", "N")

  need <- c("year", "age", "sex", "imd_quintile", "N")
  miss <- setdiff(need, names(pops))
  if (length(miss) > 0) {
    stop("pop_weight_by_age: population file is missing ", paste(miss, collapse = ", "),
         ". Present: ", paste(names(pops), collapse = ", "))
  }
  pops <- pops[, ..need]
  pops[, `:=`(sex = as.character(sex), imd_quintile = as.character(imd_quintile))]

  max_pop_year <- max(pops$year)
  future <- years[years > max_pop_year]
  if (length(future) > 0) {
    message("pop_weight_by_age: the population file stops at ", max_pop_year,
            ". Holding the ", max_pop_year, " age/sex/IMD structure for ",
            paste(range(future), collapse = "-"), ".")
    base <- pops[year == max_pop_year]
    pops <- rbindlist(c(list(pops), lapply(future, function(y) copy(base)[, year := y])))
  }

  pops <- pops[year %in% years & age %in% ages]
  if (nrow(pops) == 0) stop("pop_weight_by_age: no population rows for the requested years/ages.")
  if (pops[, any(N <= 0)]) stop("pop_weight_by_age: non-positive population counts.")

  m <- copy(model_dt)[year %in% years & age %in% ages]
  m[, `:=`(sex = as.character(sex), imd_quintile = as.character(imd_quintile))]

  j <- merge(m, pops, by = c("year", "age", "sex", "imd_quintile"), all.x = TRUE)

  n_unmatched <- j[is.na(N), .N]
  if (n_unmatched > 0) {
    stop("pop_weight_by_age: ", n_unmatched, " model cells have no matching population. ",
         "A weighted average over an incomplete population is not a population average. ",
         "Check the sex and imd_quintile labels match between the two files.")
  }

  j[, .(value = sum(get(value_var) * N) / sum(N)), by = age][order(age)]
}
