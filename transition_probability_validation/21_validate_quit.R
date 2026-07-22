# Validate the estimated probabilities of quitting smoking against the Smoking
# Toolkit Study.
#
# The STS quantity is 1 - P(smoker now) / P(smoker a year ago), where "smoker a
# year ago" is reconstructed by turning current ex-smokers back into smokers if
# their most recent quit attempt began within the last year. That is the same
# thing our p_quit is: the probability that someone who was a smoker at the
# start of the year is not one at the end.
#
# See 00_validation_utils.R for the estimators and for what they assume.

source("transition_probability_validation/00_validation_utils.R")


# ---- Parameters -----------------------------------------------------------
# The years are set once and used for both sides of the comparison, so the STS
# waves and the modelled years cannot drift apart. The old code filtered waves
# with a hard-coded number and then separately hard-coded the model years, with
# nothing tying them together.

# Validation years. These are chosen to sit entirely inside the model's
# ESTIMATION window, for two reasons. First, initiation is only estimated to
# 2017: the forecast jumps off at last_year - 1, so 2018 onwards in the
# published outputs is trend continuation, and comparing a forecast against a
# survey tests the forecast, not the estimation. Waves in 2013-2017 are
# face-to-face with full sampling, and their 12-month lookback reaches into 2012
# at the earliest, which is still inside the estimation window. A comparison
# against 2019+ waves would be a forecast check which is different to what we are doing here.
val_years <- 2013:2017
val_ages   <- 16:89
# Plot from 25. The denominator we can build from the STS holds everyone smoking
# now plus everyone who stopped in the past year, and "smoking now" includes
# people who were not smoking a year ago: relapsers, and at young ages
# initiators. They inflate the denominator and drag the STS p_quit down. Above 80 the STS gets thin.
plot_ages  <- 25:80
boot_B     <- 1000
boot_seed  <- 20260716


# ---- STS side -------------------------------------------------------------

data_tk <- sts_read_england(years = val_years, ages = val_ages)

# Which model years the STS window actually covers. The STS asks whether someone
# stopped in the last 12 months, not whether they stopped in a given calendar
# year, so a wave fielded in March 2019 is reporting on quits back to March 2018.
# Pooling the wave years therefore reaches one year further back, and the model has
# to be averaged over the years the window actually covers rather than the years
# the waves are stamped with. See sts_model_year_weights.
#
year_wts <- sts_model_year_weights(unique(data_tk$xwave))
print(year_wts)

# smokstat tells us directly who stopped in the past year, so there is no
# reconstruction from q632b8 any more and no missingness to worry about. This
# reports what the change did, for the record. Comment it out once noted.
sts_compare_quit_definitions(data_tk)

# Age and sex. sts_quit_by_age indexes on age at the START of the year, i.e.
# age - 1, because that is how smktrans indexes p_quit: someone observed at 40
# who stopped in the past year was 39 when the clock started.
quit_domain <- sts_domain(val_ages, by_sex = TRUE)

quit_sts <- sts_boot(
  dt        = data_tk,
  fn        = sts_quit_by_age,
  value_var = "p_quit",
  domain    = quit_domain,
  B         = boot_B,
  seed      = boot_seed
)

# Ages where the estimator was undefined in a large share of draws are not
# reliable. Flag them rather than plotting them as if they were.
thin <- quit_sts[frac_undefined > 0.05]
if (nrow(thin) > 0) {
  message("Ages where p_quit was undefined in >5% of bootstrap draws (too few ",
          "smokers): ", paste(thin$age, collapse = ", "))
}

# Sanity: a probability of quitting outside [0, 1] means the counter-factual
# has gone wrong, e.g. more smokers now than a year ago.
if (quit_sts[!is.na(est) & (est < 0 | est > 1), .N] > 0) {
  warning("STS p_quit falls outside [0,1] at ages: ",
          paste(quit_sts[est < 0 | est > 1]$age, collapse = ", "),
          ". This usually means sampling noise has made smkt > smkt1.")
}


# ---- smktrans side --------------------------------------------------------

quit_model <- stapm_load("quit_data_England_uncertainty.rds")

stopifnot(all(c("year", "age", "sex", "imd_quintile", "p_quit") %in% names(quit_model)))

quit_ref <- pop_weight_by_age(quit_model, "p_quit", years = year_wts$year, ages = val_ages,
                              by = c("sex", "age"), year_weights = year_wts)
setnames(quit_ref, "value", "p_quit")

# The model's own uncertainty, collapsed the same way. p_quit_lower/upper are
# bootstrap percentiles per cell, so population-weighting them gives the band
# around the population-weighted central estimate only if the draws move
# together across cells. They largely do (the same resampled survey drives every
# cell), but this is an approximation and the band should be read as indicative.
if (all(c("p_quit_lower", "p_quit_upper") %in% names(quit_model))) {
  lo <- pop_weight_by_age(quit_model, "p_quit_lower", year_wts$year, val_ages,
                          by = c("sex", "age"), year_weights = year_wts)
  hi <- pop_weight_by_age(quit_model, "p_quit_upper", year_wts$year, val_ages,
                          by = c("sex", "age"), year_weights = year_wts)
  quit_ref[, `:=`(lower = lo$value, upper = hi$value)]
}


# ---- Plot -----------------------------------------------------------------

sts_plot   <- quit_sts[age %in% plot_ages & !is.na(est) & frac_undefined <= 0.05]
model_plot <- quit_ref[age %in% plot_ages]

p <- ggplot() +
  geom_ribbon(data = model_plot, aes(x = age, ymin = lower, ymax = upper,
                                     fill = "smktrans estimate"), alpha = 0.25) +
  geom_line(data = model_plot, aes(x = age, y = p_quit, colour = "smktrans estimate"),
            linewidth = 1) +
  geom_errorbar(data = sts_plot, aes(x = age, ymin = lower, ymax = upper,
                                     colour = "Smoking Toolkit Study"),
                width = 0, alpha = 0.5) +
  geom_point(data = sts_plot, aes(x = age, y = est, colour = "Smoking Toolkit Study"),
             size = 1.5) +
  facet_wrap(~ sex) +
  scale_colour_manual(values = c("smktrans estimate" = "#1f78b4",
                                 "Smoking Toolkit Study" = "#33a02c")) +
  scale_fill_manual(values = c("smktrans estimate" = "#1f78b4")) +
  theme_minimal() +
  labs(x = "Age at the start of the year", y = "P(quit)", colour = NULL, fill = NULL,
       title = "Probability of quitting smoking, England",
       subtitle = sprintf(paste0("smktrans vs Smoking Toolkit Study, waves %s. ",
                                 "Bands and bars are 95%%.\n",
                                 "From age 25: below that the STS denominator picks up ",
                                 "new smokers and reads low."),
                          paste(range(val_years), collapse = "-"))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom")

print(p)

ggsave("transition_probability_validation/outputs/validation_quit_england.png",
       p, width = 10, height = 5.5, dpi = 150)


# ---- Numerical summary ----------------------------------------------------


cmp <- merge(sts_plot[, .(age, sex, sts = est, sts_lo = lower, sts_hi = upper)],
             model_plot[, .(age, sex, model = p_quit)], by = c("age", "sex"))
cmp[, diff := model - sts]
cmp[, model_inside_sts_ci := model >= sts_lo & model <= sts_hi]

cat("\n--- smktrans vs STS, ages", min(cmp$age), "-", max(cmp$age), "---\n")
print(cmp[, .(ages_compared      = .N,
              model_in_sts_ci    = sprintf("%.0f%%", 100 * mean(model_inside_sts_ci)),
              median_difference  = sprintf("%+.4f", median(diff)),
              mean_abs_difference = sprintf("%.4f", mean(abs(diff))),
              correlation        = sprintf("%.3f", cor(model, sts))),
          by = sex])
