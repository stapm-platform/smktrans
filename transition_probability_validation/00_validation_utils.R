# Shared functions for validating the smktrans transition probabilities against
# the Smoking Toolkit Study (STS).
#
# The general approach is to derive, from the STS, a quantity that is defined the
# same way as one of our estimated probabilities, and then plot the two against
# each other with uncertainty on both.
#
# Everything that could quietly go wrong is checked and stops. The point of this
# folder is to tell us whether the estimates are right, so it is no use to us if
# it fails in a way that looks like a result.
#
# Note there is no dependency on toolkitr here. We read the SPSS file directly.
# See sts_read_england for why.

library(data.table)
library(ggplot2)


# ---------------------------------------------------------------------------
# Paths. All input data lives in 05_input, all estimates come from the outputs
# folder of the England estimation run.
# ---------------------------------------------------------------------------

val_paths <- list(
  input        = "05_input",
  toolkit_file = "omni225_39.1_65.2cot_31.3a_25.4s_recodes_111.5sa",
  pop_file     = "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv",
  estimates    = "transition_probability_estimates/src_england/outputs",
  intermediate = "transition_probability_validation/intermediate_data"
)


#' Convert an STS wave number to a calendar year and month
#'
#' Anchored on wave 150 being March 2019, which is a fact from the toolkit team
#' rather than an inference. One wave a month with no gaps then puts wave 1 in
#' October 2006, since 150 - 1 = 149 months back from March 2019 is October 2006.
#'
#' This is the single assumption everything in this folder is labelled by, so it
#' is worth saying how it checks out. The old script filtered xwave >= 148 and
#' commented it "2019-". Under this mapping wave 148 is January 2019, so that
#' filter is exactly 2019 onwards, which is what the comment says. An earlier
#' guess of November 2006 for wave 1 would have made wave 148 February, and the
#' filter would have been quietly missing a month.
#'
#' The commented-out 75:147 in the old script was labelled 2013-2018. Wave 75 is
#' December 2012 and wave 147 is December 2018, so that one did pick up an extra
#' month at the front. 76:147 is January 2013 to December 2018 exactly.
sts_wave_to_year <- function(wave) {
  stopifnot(is.numeric(wave), all(wave >= 1, na.rm = TRUE))
  2006L + (wave + 8L) %/% 12L
}

sts_wave_to_month <- function(wave) {
  stopifnot(is.numeric(wave), all(wave >= 1, na.rm = TRUE))
  ((wave + 8L) %% 12L) + 1L
}

# Anchor check. If this ever fails the mapping has been edited and every year
# label in this folder is wrong.
stopifnot(sts_wave_to_year(150) == 2019, sts_wave_to_month(150) == 3,
          sts_wave_to_year(1) == 2006,   sts_wave_to_month(1) == 10,
          sts_wave_to_year(148) == 2019, sts_wave_to_month(148) == 1)


#' Which waves cover a set of calendar years, completely
#'
#' Only whole years are returned. A year that is only part covered by the
#' available waves is dropped, with a message, rather than quietly contributing a
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
# Reading the STS
# ---------------------------------------------------------------------------

#' Read the Smoking Toolkit Study data for England
#'
#' Reads the raw SPSS file rather than going through toolkitr. Three reasons.
#'
#' We only want a handful of variables, and toolkitr's cleaning functions build a
#' great deal we then throw away, including a merge onto its LA_codes lookup
#' table. Depending on a package to get at actage and smokstat was more trouble
#' than it was worth, particularly one that is not being kept up.
#'
#' ReadToolkit builds its file path with paste0(path_in, data_in, ".sav"), so
#' path_in has to end in a slash, and if it does not you get "unable to open
#' file: No such file or directory" with no clue as to why. file.path() below
#' does not care.
#'
#' The real reason is ToolkitCleanSmkStatus. smokstat arrives with four levels,
#' "Smoker", "Stopped>1y ago", "Stopped in past year" and "Never smoked", and it
#' collapses the two stopped categories into a single ex_smoker. That throws away
#' exactly the distinction the quit estimate needs. We were then reconstructing
#' it from q632b8, the question about how long ago the most recent quit attempt
#' started. That is a different question. q632b8 is put to anyone who has made a
#' serious attempt, including people who are smoking again now, so a current
#' smoker who tried three months ago and relapsed has a perfectly good answer to
#' it. It was never a measure of who had stopped.
#'
#' What using it cost us was in the other direction: anyone smokstat puts in
#' "Stopped in past year" who did not give a usable q632b8 was being treated as a
#' long-term quitter, which took them out of the p_quit denominator and biased it
#' down. On the England data that is about 18% of recent quitters.
#' sts_compare_quit_definitions() reports it.
#'
#' @param years Integer vector - the calendar years to keep.
#' @param ages Integer vector - the ages to keep.
#' @param path,file Where the SPSS file is. Defaults come from val_paths.
#' @importFrom data.table := setDT setnames set data.table
#' @return A data table of individual records.
sts_read_england <- function(years,
                             ages = 16:89,
                             path = val_paths$input,
                             file = val_paths$toolkit_file) {

  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop("sts_read_england: the foreign package is needed to read the SPSS file.")
  }

  f <- file.path(path, paste0(file, ".sav"))
  if (!file.exists(f)) {
    stop("sts_read_england: cannot find\n  ", normalizePath(f, mustWork = FALSE),
         "\nThe working directory is ", getwd(), ".\n",
         "Check val_paths$input and val_paths$toolkit_file at the top of ",
         "00_validation_utils.R.")
  }

  message("sts_read_england: reading ", f, ", which takes a minute.")
  raw <- foreign::read.spss(f, to.data.frame = TRUE)
  setDT(raw)
  n_raw <- nrow(raw)

  # ReadToolkit did str_replace(colnames, "X.", "A"). The dot there is a regex
  # wildcard rather than a dot, so it matches an X followed by anything and eats
  # the character after it: X39.1cot would come back as A9.1cot. Anchored, with
  # the dot escaped.
  setnames(raw, sub("^X\\.", "A", names(raw)))

  required <- c("actage", "sexz", "gore", "Aweight0", "xwave", "smokstat")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("sts_read_england: the toolkit file has no ", paste(missing, collapse = ", "),
         ". Either it is not the file we think it is, or the variable names have ",
         "changed. Names present that look similar: ",
         paste(grep(paste(substr(missing, 1, 3), collapse = "|"), names(raw),
                    value = TRUE, ignore.case = TRUE), collapse = ", "))
  }

  # read.spss pads its value labels out to a common width, so the levels come
  # back with trailing spaces and a plain == comparison quietly matches nothing.
  for (v in intersect(c("sexz", "gore", "smokstat", "q632b8", "q632b9"), names(raw))) {
    set(raw, j = v, value = trimws(as.character(raw[[v]])))
  }

  # Smoking status. Keep all four levels. Stop rather than let an unrecognised
  # value fall through as NA, because that would silently drop people out of the
  # denominator and look like a lower quit rate.
  smokstat_levels <- c("Smoker", "Stopped>1y ago", "Stopped in past year", "Never smoked")
  unexpected <- setdiff(unique(stats::na.omit(raw$smokstat)), smokstat_levels)
  if (length(unexpected) > 0) {
    stop("sts_read_england: smokstat has values we do not recognise: ",
         paste(unexpected, collapse = ", "),
         ". Expected only: ", paste(smokstat_levels, collapse = ", "), ".")
  }
  n_smokstat_na <- raw[is.na(smokstat), .N]
  if (n_smokstat_na > 0) {
    message("sts_read_england: ", n_smokstat_na, " rows have no smoking status and are dropped.")
    raw <- raw[!is.na(smokstat)]
  }
  raw[, smokstat := factor(smokstat, levels = smokstat_levels)]

  # England only. gore is the government office region, and the STS covers Great
  # Britain, so England is the nine English regions.
  eng_regions <- c("East of England", "South West", "South East", "North East",
                   "East Midlands", "West Midlands", "North West",
                   "Yorkshire and The Humber", "London")
  unknown <- setdiff(unique(stats::na.omit(raw$gore)), c(eng_regions, "Scotland", "Wales"))
  if (length(unknown) > 0) {
    stop("sts_read_england: gore has values we do not recognise, so the England filter ",
         "cannot be trusted: ", paste(unknown, collapse = ", "))
  }
  raw <- raw[gore %in% eng_regions]

  keep_waves <- sts_waves_for_years(years, max_wave = max(raw$xwave, na.rm = TRUE))
  raw <- raw[xwave %in% keep_waves]
  raw[, year := sts_wave_to_year(xwave)]

  # actage is refused by some people. The STS also carries agez, a banded age,
  # and other work in the group recovers an age from the band midpoint when
  # actage is missing. We do not do that here, because a midpoint is a guess and
  # this is a validation, but say how many go rather than dropping them quietly.
  n_no_age <- raw[is.na(actage), .N]
  if (n_no_age > 0) {
    message("sts_read_england: ", n_no_age, " rows (",
            sprintf("%.2f%%", 100 * n_no_age / nrow(raw)),
            ") did not give an exact age and are dropped. agez holds a banded age ",
            "for some of them if that ever matters.")
  }
  raw <- raw[!is.na(actage) & actage >= min(ages) & actage <= max(ages)]

  setnames(raw, "Aweight0", "weight_england")

  # The weight is the basis of every estimate here, so a missing or non-positive
  # one is not something to quietly drop.
  n_bad_wt <- raw[is.na(weight_england) | weight_england <= 0, .N]
  if (n_bad_wt > 0) {
    stop("sts_read_england: ", n_bad_wt, " rows have a missing or non-positive weight. ",
         "Decide what these are before going any further.")
  }

  # sexz. The labels are "Men" and "Women", and the more recent waves have added
  # "In another way". smktrans is estimated by sex with two categories, so there
  # is nothing to compare those respondents against and they have to go. Say how
  # many rather than letting a case_when quietly turn them into NA, which is what
  # happens in the STS synthesis code elsewhere in the group.
  sex_levels <- c("Men", "Women", "In another way")
  unexpected_sex <- setdiff(unique(stats::na.omit(raw$sexz)), sex_levels)
  if (length(unexpected_sex) > 0) {
    stop("sts_read_england: sexz has values we do not recognise: ",
         paste(unexpected_sex, collapse = ", "), ". Expected only: ",
         paste(sex_levels, collapse = ", "),
         ". If the STS has added another response, decide what to do with it here.")
  }

  raw[, sex := fcase(sexz == "Men", "Male", sexz == "Women", "Female",
                     default = NA_character_)]

  n_no_sex <- raw[is.na(sex), .N]
  if (n_no_sex > 0) {
    message("sts_read_england: ", n_no_sex, " rows (",
            sprintf("%.2f%%", 100 * n_no_sex / nrow(raw)),
            ") answered sexz as \"In another way\" or did not answer. smktrans has ",
            "no category for them so they are dropped. If that percentage is ",
            "anything but tiny, say so alongside the results.")
    raw <- raw[!is.na(sex)]
  }

  keep <- c("xwave", "year", "age", "sex", "gore", "weight_england", "smokstat",
            intersect(c("q632b8", "q632b9"), names(raw)))
  setnames(raw, "actage", "age")
  dt <- raw[, ..keep]

  message(sprintf("sts_read_england: %d rows read, %d kept (England, %s, ages %d-%d).",
                  n_raw, nrow(dt), paste(range(years), collapse = "-"),
                  min(ages), max(ages)))
  dt[]
}


#' How the STS quit window maps onto the model's calendar years
#'
#' Getting this right matters because the levels are the point of the comparison
#' and p_quit is trending, so mislabelling the years shifts the whole thing.
#'
#' An interview in month m of year Y asks whether the person stopped in the
#' previous 12 months, so the quit happened somewhere in the window running from
#' month m of Y-1 to month m of Y. That window straddles two calendar years:
#' 12 - m of its months fall in Y-1 and m of them fall in Y. Taking quitting as
#' uniform within the window, that interview contributes (12-m)/12 of its
#' exposure to model year Y-1 and m/12 to model year Y.
#'
#' So a year of STS waves is not evidence about that calendar year. Pooling all
#' twelve waves of 2019 puts 5.5 years of exposure into 2018 and 6.5 into 2019.
#' Pooling five years of waves spreads exposure across six model years, and
#' comparing against a plain mean of the model over the wave years drops the
#' earliest of them entirely and
#' over-weights 2023.
#'
#' The effect is small on the England data, about 0.9% on p_quit, because the
#' model's quit rate moves slowly. It is here because it is cheap and because
#' guessing at it is how the age indexing went wrong.
#'
#' @param waves Integer vector of the wave numbers actually used.
#' @return A data table of model_year and weight, summing to 1.
sts_model_year_weights <- function(waves) {
  d <- data.table(wave = waves, Y = sts_wave_to_year(waves), m = sts_wave_to_month(waves))
  ex <- rbind(d[, .(year = Y - 1L, e = (12 - m) / 12)],
              d[, .(year = Y,      e = m / 12)])
  ex <- ex[e > 0, .(exposure = sum(e)), by = year][order(year)]
  ex[, weight := exposure / sum(exposure)]
  ex[]
}


# ---------------------------------------------------------------------------
# The STS estimators
#
# All of them take a `domain` data table saying which rows to return, built by
# sts_domain(). That is what lets us stratify by sex without writing everything
# twice, and it means sts_boot() knows exactly what shape to expect back.
# ---------------------------------------------------------------------------

#' The rows an estimator should return
#'
#' @param ages Integer vector.
#' @param by_sex Logical - if TRUE the domain is age by sex rather than age.
sts_domain <- function(ages, by_sex = FALSE) {
  d <- if (by_sex) CJ(sex = c("Female", "Male"), age = ages) else data.table(age = ages)
  setorderv(d, names(d))
  d[]
}

#' Probability of quitting over the last year
#'
#' Builds what our p_quit is, from the STS.
#'
#' smokstat says directly whether someone stopped in the past year, so the people
#' who were smoking a year ago are the ones smoking now plus the ones who stopped
#' in the past year, and
#'
#'   p_quit = P(stopped in past year) / P(smoking now OR stopped in past year)
#'
#' i.e. of the counterfactual population who could have quit, the share who did.
#'
#' On the age. smktrans indexes p_quit on age at the START of the year. Someone
#' observed at 40 who stopped in the past year was 39 when the clock started, and
#' so was a smoker observed at 40. So both the numerator and the denominator are
#' assigned to age - 1. Leave age_at_start_of_year FALSE and the whole STS curve
#' shifts a year to the right against the model, which on a curve that climbs
#' through the twenties and again after sixty is plainly visible and is not noise.
#'
#' A caveat these data cannot fix, and it is worth being exact about it because
#' it is not uniform over age. "Smoking now" is not the same as "was smoking a
#' year ago". It also picks up people who were not smoking at the start of the
#' year and are smoking at the end of it, of whom there are two kinds: relapsers
#' who were ex-smokers, and initiators who had never smoked. Writing the states
#' at the start of the year as S, F and V, the denominator we can build is
#'
#'   |S| + |F->S| + |V->S|
#'
#' when what p_quit wants is |S| alone. Turning the recent quitters back on adds
#' S->F back in, which is right, but nothing removes F->S or V->S, and the STS
#' has no variable for when a current smoking spell started so we cannot find
#' them. The denominator is therefore too big and the STS p_quit comes out too
#' low.
#'
#' Running a synthetic cohort through our own 2019 probabilities puts numbers on
#' it. Over 26 and up it is 1 to 5%, which is well inside the bootstrap interval
#' and not worth worrying about, and almost all of it is relapsers. Under 25 it
#' is another matter: the denominator is 23% too big, about a third of that is
#' initiators rather than relapsers, and at 18 the bias reaches 28%. That is why
#' the plots start at 25.
#'
#' @param dt Individual STS records.
#' @param domain From sts_domain(). Stratifies by whatever columns it holds.
#' @param age_at_start_of_year Logical - index on age - 1. See above.
sts_quit_by_age <- function(dt, domain, age_at_start_of_year = TRUE) {

  strat <- intersect(names(domain), c("age", "sex"))

  d <- copy(dt)
  d[, smk_now      := as.integer(smokstat == "Smoker")]
  d[, smk_year_ago := as.integer(smokstat %in% c("Smoker", "Stopped in past year"))]
  if (age_at_start_of_year) d[, age := age - 1L]

  s <- d[, .(smkt  = sum(smk_now      * weight_england) / sum(weight_england),
             smkt1 = sum(smk_year_ago * weight_england) / sum(weight_england),
             n     = .N),
         by = strat]

  s <- merge(domain, s, by = strat, all.x = TRUE, sort = FALSE)
  setorderv(s, strat)

  # An age with nobody smoking a year ago has no quit probability. That is
  # missing, not zero. The old code set it to zero, which dragged the bootstrap
  # mean down at exactly the ages where the data is thinnest.
  s[, p_quit := ifelse(!is.na(smkt1) & smkt1 > 0, 1 - smkt / smkt1, NA_real_)]
  s[]
}


#' What the old q632b8 definition of "stopped in the past year" was losing
#'
#' We used to identify recent quitters by asking whether q632b8, how long ago the
#' most recent serious quit attempt started, fell inside a year. smokstat answers
#' the question directly, so this reports what the switch did.
#'
#' The two variables are not asking the same thing and it is worth being clear
#' about which. smokstat asks about current status. q632b8 asks when the most
#' recent serious quit ATTEMPT started, and it is put to anyone who has made one,
#' including people who are smoking again now. So a lot of current smokers have a
#' recent q632b8: they tried, and it did not hold. That is not a fault in either
#' variable, it is the questions being different, and it is why q632b8 was the
#' wrong one to build p_quit on.
#'
#' What the old definition lost is the other direction: people smokstat puts in
#' "Stopped in past year" who did not give a usable q632b8. They were being
#' treated as long-term quitters, which took them out of the p_quit denominator
#' and biased it down.
#'
#' Run once and record the numbers. Not needed on every run.
sts_compare_quit_definitions <- function(dt) {

  if (!"q632b8" %in% names(dt)) {
    message("sts_compare_quit_definitions: q632b8 is not in the data, nothing to compare.")
    return(invisible(NULL))
  }

  # foreign::read.spss gives the value labels. The group's synthesis code reads
  # with haven and sees the numeric codes 1 to 6, with -1 for don't know and 8
  # for not stated. Match on either, so this does not quietly find nothing if the
  # reader ever changes.
  recent_labels <- c("In the last week",
                     "More than a week and up to a month",
                     "More than 1 month and up to 2 months",
                     "More than 2 months and up to 3 months",
                     "More than 3 months and up to 6 months",
                     "More than 6 months and up to a year")
  q <- as.character(dt$q632b8)
  recent <- !is.na(q) & (q %in% recent_labels | q %in% as.character(1:6))

  stopped_past_year <- dt$smokstat == "Stopped in past year"

  n_smokstat <- sum(stopped_past_year, na.rm = TRUE)
  n_both     <- sum(stopped_past_year & recent, na.rm = TRUE)
  n_lost     <- sum(stopped_past_year & !recent, na.rm = TRUE)

  message(sprintf(paste0(
    "Identifying people who stopped in the past year:\n\n",
    "  smokstat says so for                    %d\n",
    "  of whom q632b8 also puts inside a year  %d\n",
    "  of whom q632b8 does not                 %d  (%.1f%%)\n",
    "    ^ the old code counted these as long-term quitters, so they dropped out\n",
    "      of the p_quit denominator and biased it down"),
    n_smokstat, n_both, n_lost, 100 * n_lost / max(n_smokstat, 1)))

  # Where everyone with a recent q632b8 actually sits. Most of them will be
  # current smokers, because q632b8 is asked of anyone who made an attempt and a
  # failed attempt still counts.
  message("\nSmoking status of everyone with a quit attempt starting inside the last year:")
  print(dt[recent, .N, by = smokstat][order(-N)])

  # The only genuinely odd cell. A never smoker has not made a quit attempt.
  n_never <- sum(recent & dt$smokstat == "Never smoked", na.rm = TRUE)
  if (n_never > 0) {
    warning(n_never, " people have a quit attempt starting in the last year but smokstat ",
            "says they never smoked. One of the two variables is not what we think it is.")
  }

  invisible(data.table(n_smokstat = n_smokstat, n_both = n_both, n_lost = n_lost,
                       n_recent_attempt = sum(recent, na.rm = TRUE)))
}


#' Current-smoker prevalence
sts_prev_by_age <- function(dt, domain) {
  strat <- intersect(names(domain), c("age", "sex"))
  d <- copy(dt)
  d[, smk_now := as.integer(smokstat == "Smoker")]
  s <- d[, .(prev = sum(smk_now * weight_england) / sum(weight_england), n = .N), by = strat]
  s <- merge(domain, s, by = strat, all.x = TRUE, sort = FALSE)
  setorderv(s, strat)
  s[]
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
#' and since never, current and former sum to a constant, the denominator is the
#' people who are not currently smoking, so it reduces to
#'
#'   p_start_net = (prev(a+1) - prev(a)) / (1 - prev(a))
#'
#' Prevalence is smoothed over age and then differenced, in that order.
#' Differencing a noisy curve gives a very noisy derivative and repairing that
#' afterwards is worse than not making the mess in the first place. This is the
#' same lesson as p_dense: smooth the thing you estimated, then difference it.
#'
#' The old 22_prepare_toolkit_data_init.R did cumsum(prevalence), fitted a gam to
#' that, and differenced the fit, which is a roundabout way of doing the same
#' thing because the cumsum and the diff cancel.
#'
#' @param dt Individual STS records.
#' @param domain From sts_domain().
#' @param smooth_df Numeric - degrees of freedom for the prevalence smooth.
sts_net_init_by_age <- function(dt, domain, smooth_df = 6) {

  strat <- intersect(names(domain), c("age", "sex"))
  s <- sts_prev_by_age(dt, domain)

  # Smooth and difference WITHIN each stratum. Differencing across a sex boundary
  # would be meaningless.
  by_cols <- setdiff(strat, "age")

  smooth_one <- function(prev, age) {
    ok <- !is.na(prev)
    if (sum(ok) < 5) return(rep(NA_real_, length(prev)))
    fit <- try(stats::smooth.spline(age[ok], prev[ok],
                                    df = min(smooth_df, max(4, sum(ok) - 2))),
               silent = TRUE)
    if (inherits(fit, "try-error")) return(rep(NA_real_, length(prev)))
    out <- rep(NA_real_, length(prev))
    out[ok] <- stats::predict(fit, age[ok])$y
    pmin(pmax(out, 0), 1)
  }

  if (length(by_cols) > 0) {
    s[, prev_sm := smooth_one(prev, age), by = by_cols]
    s[, prev_next := data.table::shift(prev_sm, type = "lead"), by = by_cols]
  } else {
    s[, prev_sm := smooth_one(prev, age)]
    s[, prev_next := data.table::shift(prev_sm, type = "lead")]
  }

  s[, p_start_net := (prev_next - prev_sm) / (1 - prev_sm)]
  s[, c(strat, "p_start_net"), with = FALSE]
}


#' Net initiation following pseudo-cohorts across survey years
#'
#' The cross-sectional estimator above has a bias that matters here. In one
#' cross-section, prev(a+1) - prev(a) compares DIFFERENT birth cohorts: the
#' people aged a+1 started smoking a year earlier in calendar time, when
#' initiation was higher, so the age gradient carries a cohort effect on top of
#' the true within-cohort flow. With initiation falling, the cross-section reads
#' net initiation as staying positive to older ages than it really does. That is
#' exactly the region where the model and the STS disagree, so we should not be
#' using a biased ruler there.
#'
#' We have five years of STS, so we do not have to. Follow the same birth cohort
#' from one survey year to the next:
#'
#'   p_start_net(a, t) = (prev(a+1, t+1) - prev(a, t)) / (1 - prev(a, t))
#'
#' The people aged a+1 in year t+1 ARE the people aged a in year t (repeat
#' cross-sections of the same population, no differential mortality at these
#' ages), so the cohort effect cancels by construction. Prevalence is smoothed
#' over age WITHIN each survey year before the diagonal difference is taken, same
#' lesson as always: smooth the estimate, then difference it.
#'
#' The model side needs no matching change: within a five-year window the model's own
#' probabilities move slowly, so its period synthetic cohort and its true cohort
#' are close. The big cohort effect in the STS comes from real history - the
#' fall in youth initiation over 2005-2019 - which the model's period profile
#' never contained in the first place.
#'
#' @param dt Individual STS records spanning at least two survey years.
#' @param domain From sts_domain(). Age only; sex stratification works but
#'   halves the cohort sizes, so check the intervals before trusting it.
#' @param smooth_df Numeric - degrees of freedom for the within-year smooth.
sts_net_init_cohort <- function(dt, domain, smooth_df = 6) {

  strat <- intersect(names(domain), c("age", "sex"))
  by_year <- c(setdiff(strat, "age"), "year")

  yrs <- sort(unique(dt$year))
  if (length(yrs) < 2) {
    stop("sts_net_init_cohort: needs at least two survey years to follow a cohort, got ",
         paste(yrs, collapse = ", "), ".")
  }
  if (!all(diff(yrs) == 1)) {
    stop("sts_net_init_cohort: survey years must be consecutive to follow cohorts ",
         "one year at a time, got ", paste(yrs, collapse = ", "), ".")
  }

  d <- copy(dt)
  d[, smk_now := as.integer(smokstat == "Smoker")]

  # weighted prevalence by age within each survey year (and sex if stratified)
  s <- d[, .(prev = sum(smk_now * weight_england) / sum(weight_england), n = .N),
         by = c(by_year, "age")]

  full <- CJ(year = yrs, age = min(domain$age):(max(domain$age) + 1L))
  if ("sex" %in% strat) full <- full[, CJ(year = year, age = age, sex = c("Female", "Male")), by = NULL]
  s <- merge(full, s, by = intersect(names(full), names(s)), all.x = TRUE)

  smooth_one <- function(prev, age) {
    ok <- !is.na(prev)
    if (sum(ok) < 5) return(rep(NA_real_, length(prev)))
    fit <- try(stats::smooth.spline(age[ok], prev[ok],
                                    df = min(smooth_df, max(4, sum(ok) - 2))), silent = TRUE)
    if (inherits(fit, "try-error")) return(rep(NA_real_, length(prev)))
    out <- rep(NA_real_, length(prev))
    out[ok] <- stats::predict(fit, age[ok])$y
    pmin(pmax(out, 0), 1)
  }
  s[, prev_sm := smooth_one(prev, age), by = by_year]

  # the cohort diagonal: prev at (a+1, t+1) against prev at (a, t)
  nxt <- s[, .(year = year - 1L, age = age - 1L, prev_next = prev_sm,
               grp = if ("sex" %in% strat) sex else "all")]
  cur <- s[, .(year, age, prev_sm, grp = if ("sex" %in% strat) sex else "all")]
  j <- merge(cur, nxt, by = c("year", "age", "grp"))
  j[, p_start_net := (prev_next - prev_sm) / (1 - prev_sm)]

  # pool the year-pairs at each age, weighting equally: each pair is one
  # cohort-year of evidence
  out <- j[, .(p_start_net = mean(p_start_net, na.rm = TRUE)), by = c(if ("sex" %in% strat) "grp", "age")]
  if ("sex" %in% strat) setnames(out, "grp", "sex")
  out[is.nan(p_start_net), p_start_net := NA_real_]

  out <- merge(domain, out, by = strat, all.x = TRUE, sort = FALSE)
  setorderv(out, strat)
  out[, c(strat, "p_start_net"), with = FALSE]
}


#' Bootstrap an STS estimator
#'
#' The toolkit has no clustering or stratification, just a calibration weight, so
#' individuals are resampled with replacement uniformly and the weighted
#' estimator is applied to each resample.
#'
#' The old code sampled with probability proportional to weight AND then applied
#' the weights again inside the summary, which counts them twice. Pick one. The
#' weights belong in the estimator, so the resample is uniform.
#'
#' @param dt Individual STS records.
#' @param fn Function(dt, domain) returning a data table holding the domain's
#'   columns plus `value_var`, one row per domain row and in the same order.
#' @param value_var Character - the column of fn's output to bootstrap.
#' @param domain From sts_domain(). fn must return one row per row of it.
#' @param B Integer - number of bootstrap iterations.
#' @param seed Integer - REQUIRED. An unseeded bootstrap cannot be reproduced,
#'   and we have been bitten by that already elsewhere in this package.
sts_boot <- function(dt, fn, value_var, domain, B = 1000, seed = NULL) {

  if (is.null(seed)) stop("sts_boot: a seed is required. Without one the plot cannot be reproduced.")
  if (B < 100) warning("sts_boot: B = ", B, " is low for 2.5th and 97.5th percentiles.")

  set.seed(seed)
  n <- nrow(dt)
  iter_seeds <- sample.int(.Machine$integer.max, B)

  domain <- copy(domain)
  setorderv(domain, names(domain))

  out <- matrix(NA_real_, nrow = nrow(domain), ncol = B)

  for (i in seq_len(B)) {
    set.seed(iter_seeds[i])
    idx <- sample.int(n, n, replace = TRUE)
    res <- fn(dt[idx], domain)

    # Check the alignment once. Everything below assumes fn hands rows back in
    # the domain's order, and if it ever stops doing that the results would be
    # scrambled rather than wrong in any way you could see.
    if (i == 1) {
      if (nrow(res) != nrow(domain)) {
        stop("sts_boot: the estimator returned ", nrow(res), " rows for a domain of ",
             nrow(domain), ". It must return exactly one row per domain row.")
      }
      for (k in names(domain)) {
        if (!identical(as.character(res[[k]]), as.character(domain[[k]]))) {
          stop("sts_boot: the estimator returned rows out of order on '", k,
               "'. Results would be silently scrambled.")
        }
      }
    }
    out[, i] <- res[[value_var]]
  }

  # NAs here mean the estimator was undefined in that draw, not zero. Carry the
  # count through so the caller can see how much of each age is being thrown away.
  n_na <- rowSums(is.na(out))

  cbind(domain, data.table(
    est               = apply(out, 1, mean, na.rm = TRUE),
    lower             = apply(out, 1, quantile, 0.025, na.rm = TRUE),
    upper             = apply(out, 1, quantile, 0.975, na.rm = TRUE),
    n_draws_undefined = n_na,
    frac_undefined    = n_na / B
  ))[]
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
#' The STS figure is a population weighted average over whoever is actually in
#' England. A plain mean over the sex by IMD by year cells weights all of them
#' equally, which is not the same thing and is not what we are comparing against.
#' The old code did a plain mean and carried a comment saying it shouldn't.
#'
#' The population file only runs to 2019. For later years we hold the 2019
#' structure. That is an assumption about the age, sex and IMD composition of
#' England, not about smoking, and it is applied loudly rather than silently.
#' @param by Character vector - what to keep. c("age") collapses over sex,
#'   c("age", "sex") keeps sex, which is what the quit plot needs.
#' @param year_weights Optional data table of year and weight, from
#'   sts_model_year_weights(). Without it every year in `years` counts according
#'   to its population alone, which is not what the STS window implies. See
#'   sts_model_year_weights.
pop_weight_by_age <- function(model_dt, value_var, years, ages, by = "age",
                              year_weights = NULL) {

  pops <- fread(val_paths$pop_file)
  if (!"N" %in% names(pops) && "pops" %in% names(pops)) setnames(pops, "pops", "N")

  need <- c("year", "age", "sex", "imd_quintile", "N")
  miss <- setdiff(need, names(pops))
  if (length(miss) > 0) {
    stop("pop_weight_by_age: the population file is missing ", paste(miss, collapse = ", "),
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
  if (nrow(pops) == 0) stop("pop_weight_by_age: no population rows for the requested years and ages.")
  if (pops[, any(N <= 0)]) stop("pop_weight_by_age: non-positive population counts.")

  m <- copy(model_dt)[year %in% years & age %in% ages]
  m[, `:=`(sex = as.character(sex), imd_quintile = as.character(imd_quintile))]

  j <- merge(m, pops, by = c("year", "age", "sex", "imd_quintile"), all.x = TRUE)

  # Weight each year by how much of the STS lookback window actually falls in it,
  # on top of the population weighting.
  if (!is.null(year_weights)) {
    yw <- copy(year_weights)
    if (!all(c("year", "weight") %in% names(yw))) {
      stop("pop_weight_by_age: year_weights needs columns 'year' and 'weight'.")
    }
    missing_years <- setdiff(unique(j$year), yw$year)
    if (length(missing_years) > 0) {
      stop("pop_weight_by_age: no exposure weight for model year(s) ",
           paste(sort(missing_years), collapse = ", "),
           ". Pass years = year_weights$year so the two agree.")
    }
    j <- merge(j, yw[, .(year, .exp_weight = weight)], by = "year", all.x = TRUE)
    j[, N := N * .exp_weight]
    j[, .exp_weight := NULL]
  }

  n_unmatched <- j[is.na(N), .N]
  if (n_unmatched > 0) {
    stop("pop_weight_by_age: ", n_unmatched, " model cells have no matching population. ",
         "A weighted average over an incomplete population is not a population average. ",
         "Check that the sex and imd_quintile labels match between the two files.")
  }

  out <- j[, .(value = sum(get(value_var) * N) / sum(N)), by = by]
  setorderv(out, by)
  out[]
}
