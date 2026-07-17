# Check the estimated relapse probabilities against the Hawkins inputs.
#
# Be clear about what this is. Hawkins is our INPUT, not independent data, so
# this is not validation in the sense that 21_validate_quit.R is validation. It
# cannot tell us whether our relapse probabilities are right. What it can tell
# us is whether the pipeline is doing to Hawkins what we think it is doing, and
# where the forecast has left the evidence behind.
#
# We do not have anything better for relapse. The STS is a repeat cross-section,
# so it never sees the same person twice and cannot observe a relapse. See the
# note at the bottom for the one partial thing it could offer.
#
# The check that does the work here is an invariant rather than a comparison.
# prep_relapse computes
#
#   p_relapse = sum(p_relapse * p) / sum(p)
#
# over the demographic profile, which is a weighted average of Hawkins values.
# A weighted average cannot fall outside the range of the things it averages. So
# for every (time_since_quit, age, sex, imd_quintile) our output MUST sit inside
# the min and max of Hawkins across the 160 covariate combinations. If it does
# not, something between Hawkins and the output has moved it somewhere it has no
# right to be.
#
# Historically that is a hard invariant and a failure is a bug. For forecast
# years it is not: relapse_forecast deliberately scales by the fitted trend, so
# it can legitimately go beyond anything Hawkins contains. We count those rather
# than fail on them, because a forecast that asserts a relapse probability no
# covariate combination supports is a claim we should be making knowingly.

source("transition_probability_validation/00_validation_utils.R")

jump_off_year <- 2017     # config$last_year - 1 for England
hist_years    <- 2003:jump_off_year


# ---- Inputs ---------------------------------------------------------------

if (!requireNamespace("smktrans", quietly = TRUE)) {
  stop("23_verify_relapse: smktrans is not installed, so hawkins_relapse cannot be loaded.")
}
hawkins <- as.data.table(smktrans::hawkins_relapse)

final <- stapm_load("relapse_by_age_imd_timesincequit_England.rds")

prep <- readRDS(file.path(val_paths$estimates, "relapse_data_England.rds"))
prep_tsq <- as.data.table(prep$relapse_by_age_imd_timesincequit)


# ---- The Hawkins envelope -------------------------------------------------

env <- hawkins[, .(h_min = min(p_relapse), h_max = max(p_relapse),
                   h_mean = mean(p_relapse), n_combos = .N),
               by = .(time_since_quit, age, sex, imd_quintile)]

if (uniqueN(env$n_combos) != 1) {
  warning("The Hawkins envelope is built on an uneven number of covariate combinations ",
          "per cell (", paste(range(env$n_combos), collapse = "-"),
          "). The min/max are still valid bounds but the mean is not comparable across cells.")
}

message("Hawkins envelope: ", nrow(env), " cells, ", env$n_combos[1],
        " covariate combinations each.")


#' Check an output table against the envelope
#'
#' @param dt Output table with year, age, sex, imd_quintile, time_since_quit, p_relapse.
#' @param years Years to check.
#' @param hard Logical - if TRUE a breach is an error, if FALSE it is counted.
check_envelope <- function(dt, years, hard) {

  # tsq >= 10 is set to a hard zero by prep_relapse and is not a weighted
  # average of anything, so the invariant does not apply to it.
  d <- merge(dt[year %in% years & time_since_quit < 10],
             env, by = c("time_since_quit", "age", "sex", "imd_quintile"), all.x = TRUE)

  n_unmatched <- d[is.na(h_min), .N]
  if (n_unmatched > 0) {
    stop("check_envelope: ", n_unmatched, " output cells have no matching Hawkins cell. ",
         "Either the age/sex/IMD labels have drifted apart, or the output covers ",
         "cells Hawkins does not. Cannot check an invariant against a gap.")
  }

  tol <- 1e-9
  d[, breach := p_relapse < h_min - tol | p_relapse > h_max + tol]
  n_b <- sum(d$breach)

  msg <- sprintf("%d of %d cells (%.2f%%) outside the Hawkins envelope, years %s.",
                 n_b, nrow(d), 100 * n_b / nrow(d), paste(range(years), collapse = "-"))

  if (n_b > 0) {
    w <- d[breach][order(-pmax(h_min - p_relapse, p_relapse - h_max))][1]
    msg <- paste0(msg, sprintf("\n  Worst: p_relapse = %.5f against [%.5f, %.5f] ",
                               w$p_relapse, w$h_min, w$h_max),
                  sprintf("(age %d, %s, IMD %s, tsq %d, year %d).",
                          w$age, w$sex, w$imd_quintile, w$time_since_quit, w$year))
  }

  if (hard && n_b > 0) {
    stop("check_envelope: the output is a weighted average of Hawkins, so it cannot sit ",
         "outside the Hawkins range. It does.\n  ", msg,
         "\n  Before hunting for a bug: the most likely cause by far is that the England ",
         "outputs predate the current hawkins_relapse. If data-raw has been re-run since ",
         "the last estimation run, re-run 10_run_smoking_transitions.R for England first. ",
         "Only if the outputs are current is this a real failure.")
  }
  message(msg)
  invisible(d)
}


# ---- 1. Hard invariant: historical years ----------------------------------
# prep_relapse's output before any forecasting. This must pass.

message("\n-- prep_relapse output, historical --")
check_envelope(prep_tsq, hist_years, hard = TRUE)

message("\n-- final output, historical (scaling is 1 at and before the jump-off year) --")
check_envelope(final, hist_years, hard = TRUE)


# ---- 2. Soft diagnostic: forecast years -----------------------------------
# relapse_forecast scales these by the fitted trend, so they may legitimately
# leave the Hawkins range. Count them; do not fail.

message("\n-- final output, forecast --")
fc <- check_envelope(final, (jump_off_year + 1):max(final$year), hard = FALSE)

fc_breach <- fc[breach == TRUE]
if (nrow(fc_breach) > 0) {
  message("\nThese cells assert a relapse probability that no combination of Hawkins ",
          "covariates produces.\nThat is the forecast trend extrapolating past the ",
          "evidence, not a bug, but it is a claim we are making.")
  cat("\n  Forecast breaches by year:\n")
  print(fc_breach[, .(n = .N, worst_excess = round(max(p_relapse - h_max), 4)), by = year][order(year)])
  cat("\n  Forecast breaches by age band:\n")
  print(fc_breach[, .(n = .N), by = .(age_band = cut(age, c(17, 30, 50, 70, 89)))][order(age_band)])
}


# ---- 3. What the demographic re-weighting is doing -------------------------
# prep_relapse maps Hawkins onto England's demographic profile. The gap between
# our output and the unweighted Hawkins mean IS that re-weighting. Worth
# reporting so we know how much work it is doing, and in which direction.

cmp <- merge(prep_tsq[year == jump_off_year & time_since_quit < 10],
             env, by = c("time_since_quit", "age", "sex", "imd_quintile"))
cmp[, ratio := p_relapse / h_mean]

cat("\n--- Effect of re-weighting Hawkins onto England's demographic profile ---\n")
cat("    (our output relative to the unweighted Hawkins mean, jump-off year)\n\n")
print(cmp[, .(ratio_median = round(median(ratio), 3),
              ratio_q05 = round(quantile(ratio, 0.05), 3),
              ratio_q95 = round(quantile(ratio, 0.95), 3)), by = time_since_quit][order(time_since_quit)])
cat("\n  A ratio below 1 means England's profile is lower-relapse than the average\n")
cat("  Hawkins respondent. A ratio that varies a lot across tsq means the covariate\n")
cat("  effects interact with time since quit.\n")


# ---- 4. Does our output preserve the Hawkins shape over tsq? --------------
# p_smooth is applied by = .(sex, imd_quintile, time_since_quit), i.e. each tsq
# is smoothed over age and year separately and nothing is smoothed ACROSS tsq.
# So whatever shape Hawkins has over tsq passes straight through.

h_prof <- hawkins[, .(hawkins = mean(p_relapse)), by = time_since_quit][order(time_since_quit)]
o_prof <- final[year == jump_off_year & age %in% 30:60,
                .(output = mean(p_relapse)), by = time_since_quit][order(time_since_quit)]
prof <- merge(h_prof, o_prof, by = "time_since_quit")

cat("\n--- Relapse over time since quit ---\n\n")
print(prof)

h_rises <- which(diff(prof$hawkins) > 1e-12)
o_rises <- which(diff(prof$output) > 1e-12)

if (length(h_rises) > 0) {
  warning("The Hawkins source data is not monotone over time since quit. It rises at ",
          "tsq ", paste(prof$time_since_quit[h_rises], collapse = ", "),
          ". Our output inherits this because p_smooth smooths within each tsq and ",
          "never across them. A relapse hazard that goes up with years abstinent is ",
          "not credible and it is going into the ABM. This is a question for whoever ",
          "curated hawkins_relapse, not a bug in this package.")
}
if (!identical(h_rises, o_rises)) {
  message("Note: the output rises at different tsq values from Hawkins, so something ",
          "in the pipeline is changing the shape over tsq. Worth understanding.")
}

# The cliff at tsq 10
cat(sprintf("\n  tsq 9 -> 10: %.4f -> %.4f. Relapse is set to exactly zero at 10 years\n",
            prof[time_since_quit == 9]$output, prof[time_since_quit == 10]$output))
cat("  by construction in prep_relapse, with zero uncertainty attached. That is a\n")
cat("  modelling convention, not a finding, and the ABM cannot tell the difference.\n")


# ---- 5. Is the package data the data we think it is? ----------------------
#
# The checks that hawkins_relapse matches the paper now live in
# tests/testthat/test-hawkins-relapse.R, which is where they belong: they are
# about the data, not about the pipeline, and they should fail a build rather
# than a validation run. What is left here is a short report of the profile so
# that it appears in the record next to the pipeline checks, plus one guard that
# the data-raw script has actually been run since the last change to it.
#
# Hawkins, Hollingworth & Campbell (2010), Nicotine & Tobacco Research
# 12(12):1228-1235, doi:10.1093/ntr/ntq175.

paper_tab2 <- data.table(
  time_since_quit = 1:10,
  n_abstinent     = c(1578, 1128, 832, 645, 535, 410, 317, 254, 213, 180),
  n_relapsing     = c(227, 95, 48, 25, 16, 6, 6, 1, 4, 0),
  p_paper         = c(15.1, 7.9, 4.9, 3.0, 2.3, 1.1, 1.4, 0.3, 1.3, 0.0) / 100
)

pkg_prof <- hawkins[, .(p_pkg = mean(p_relapse)), by = time_since_quit][order(time_since_quit)]
chk_prof <- merge(pkg_prof, paper_tab2, by = "time_since_quit", all.x = TRUE)

cat("\n--- hawkins_relapse next to the paper's Table 2 ---\n")
cat("    (the level is not expected to match: the package holds modelled\n")
cat("     probabilities per covariate combination, Table 2 is a marginal, and\n")
cat("     the baseline is calibrated so the Hawkins cohort reproduces Table 2)\n\n")
print(chk_prof[, .(time_since_quit,
                   paper_pct = round(100 * p_paper, 2),
                   package_pct = round(100 * p_pkg, 2),
                   paper_n_relapse = n_relapsing,
                   paper_n_at_risk = n_abstinent)])

# Years 6 to 9 are pooled in data-raw, following the paper's own model, because
# the yearly counts are 6, 6, 1 and 4. If they are not pooled here then the
# package data predates that change and everything below is about the old table.
n_distinct_6plus <- uniqueN(round(pkg_prof[time_since_quit %in% 6:9]$p_pkg, 8))
if (n_distinct_6plus > 1) {
  warning("hawkins_relapse holds ", n_distinct_6plus, " distinct rates for time_since_quit ",
          "6 to 9, so it predates the pooling change in data-raw. Re-run ",
          "data-raw/Relapse_Hawkins2010/prep_Hawkins_relapse.R before reading anything ",
          "into this report: the old table rises at tsq 7 and tsq 9 on the back of ",
          "1 and 4 relapses.")
}

# The profile must fall. This is the thing the pooling was for.
if (any(diff(pkg_prof[time_since_quit < 10]$p_pkg) > 1e-12)) {
  warning("hawkins_relapse still rises with time since quit at: ",
          paste(pkg_prof$time_since_quit[which(diff(pkg_prof$p_pkg) > 1e-12) + 1],
                collapse = ", "),
          ". A relapse hazard that goes up with years abstinent is not credible ",
          "and it is going into the ABM.")
}

# Years 1 to 5 come straight from Table 2 with no pooling, so the SHAPE over
# those years has to track the paper. The level does not, because of the
# calibration, so compare the year-on-year ratios rather than the values. Years
# 6 to 9 are excluded because their ratios are 1 by design once pooled.
shape <- chk_prof[time_since_quit %in% 1:5]
shape[, `:=`(r_paper = p_paper / shift(p_paper), r_pkg = p_pkg / shift(p_pkg))]
max_shape_diff <- max(abs(shape$r_pkg - shape$r_paper), na.rm = TRUE)
cat(sprintf("\n  Largest difference in year-on-year ratio, years 1-5: %.3f\n", max_shape_diff))
if (max_shape_diff > 0.15) {
  warning("The tsq profile in hawkins_relapse no longer tracks the paper's Table 2 ",
          "over years 1 to 5. Either data-raw has changed or the wrong table was ",
          "transcribed. Everything downstream of this is then meaningless.")
}

# The tsq = 0 value is not in the paper: its analysis cohort is people already
# abstinent for a year and Table 2 starts at a length of quit of 1. It is
# derived in data-raw by scaling the year 1 rate along the placebo continuous
# abstinence curve from Jackson et al., to reflect that someone who quit at some
# point in the last year and is still abstinent at the next annual tick has been
# abstinent for about 21 weeks rather than 52. Check it is still doing that
# rather than having been set by hand.
if (0 %in% pkg_prof$time_since_quit) {
  wk <- 1:52
  pa <- smktrans::SmkContAbst("placebo", wk)
  dur <- sum(wk * pa) / sum(pa)
  adj0 <- smktrans::SmkContAbst("placebo", dur) / smktrans::SmkContAbst("placebo", 52)
  ro <- function(p) p / (1 - p)
  # Take this off a single cell, not off the mean over the grid. The covariate
  # odds ratios and the calibration factor multiply the odds, so they cancel in a
  # ratio taken within one cell. They do not cancel in a ratio of means, because
  # the odds of an average is not the average of the odds, and comparing those
  # gives 1.435 against the 1.534 we are looking for.
  ref <- function(tsq) {
    hawkins[time_since_quit == tsq & age == 45 & sex == "Female" &
              degree == "no_degree" & relationship_status == "single" &
              employ2cat == "employed" & hse_mental == "no_mental" &
              income5cat == "1_lowest_income" &
              imd_quintile == "1_least_deprived"]$p_relapse
  }
  got <- ro(ref(0)) / ro(ref(1))
  want <- ro(paper_tab2[time_since_quit == 1]$p_paper * adj0) /
          ro(paper_tab2[time_since_quit == 1]$p_paper)
  cat(sprintf("  tsq 0 vs tsq 1 odds ratio: %.4f (Jackson implies %.4f, mean abstinence %.1f weeks)\n",
              got, want, dur))
  if (abs(got - want) > 1e-4) {
    warning("The tsq = 0 value in hawkins_relapse is not the year 1 rate scaled by the ",
            "Jackson placebo curve. It should be ", round(want, 3), " times the year 1 ",
            "odds and it is ", round(got, 3), ". Check data-raw.")
  }
}


# ---- Note on what the STS could add ---------------------------------------
#
# The STS cannot validate relapse. It is a repeat cross-section, so it never
# observes someone abstinent at t and smoking at t+1, which is what a relapse
# probability is.
#
# The one partial thing it could give us: q632b8 records how long ago the most
# recent serious quit attempt started and q632b9 records how long it lasted. For
# people whose attempt started more than a year ago we can see whether they are
# still not smoking, which gives a one-year survival of quit attempts and is
# comparable to our p_relapse at tsq = 0.
#
# That is one point against a model that runs to ten years, it only covers the
# most recent attempt, and it relies on recall of exactly the thing people are
# worst at recalling. It is worth having if q632b8 has a "more than a year ago"
# category. Check the levels before writing it.
