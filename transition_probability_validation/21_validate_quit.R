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

val_years  <- 2019:2023
val_ages   <- 16:89
plot_ages  <- 16:80        # the STS gets thin above 80
boot_B     <- 1000
boot_seed  <- 20260716


# ---- STS side -------------------------------------------------------------

data_tk <- sts_read_england(years = val_years, ages = val_ages)

# How many ex-smokers drop out of the denominator because q632b8 is missing?
# This is reported rather than hidden, because it biases p_quit downwards.
sts_check_q632b8(data_tk)

quit_sts <- sts_boot(
  dt          = data_tk,
  fn          = sts_quit_by_age,
  value_var   = "p_quit",
  domain_ages = val_ages,
  B           = boot_B,
  seed        = boot_seed
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

quit_ref <- pop_weight_by_age(quit_model, "p_quit", years = val_years, ages = val_ages)
setnames(quit_ref, "value", "p_quit")

# The model's own uncertainty, collapsed the same way. p_quit_lower/upper are
# bootstrap percentiles per cell, so population-weighting them gives the band
# around the population-weighted central estimate only if the draws move
# together across cells. They largely do (the same resampled survey drives every
# cell), but this is an approximation and the band should be read as indicative.
if (all(c("p_quit_lower", "p_quit_upper") %in% names(quit_model))) {
  lo <- pop_weight_by_age(quit_model, "p_quit_lower", val_years, val_ages)
  hi <- pop_weight_by_age(quit_model, "p_quit_upper", val_years, val_ages)
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
  scale_colour_manual(values = c("smktrans estimate" = "#1f78b4",
                                 "Smoking Toolkit Study" = "#33a02c")) +
  scale_fill_manual(values = c("smktrans estimate" = "#1f78b4")) +
  theme_minimal() +
  labs(x = "Age", y = "P(quit)", colour = NULL, fill = NULL,
       title = "Probability of quitting smoking, England",
       subtitle = sprintf("smktrans vs Smoking Toolkit Study, %s. Bands and bars are 95%%.",
                          paste(range(val_years), collapse = "-"))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom")

print(p)

ggsave("transition_probability_validation/outputs/validation_quit_england.png",
       p, width = 9, height = 5.5, dpi = 150)


# ---- Numerical summary ----------------------------------------------------
# The plot is for looking at. This is the bit to quote.

cmp <- merge(sts_plot[, .(age, sts = est, sts_lo = lower, sts_hi = upper)],
             model_plot[, .(age, model = p_quit)], by = "age")
cmp[, diff := model - sts]
cmp[, model_inside_sts_ci := model >= sts_lo & model <= sts_hi]

cat("\n--- smktrans vs STS, ages", min(cmp$age), "-", max(cmp$age), "---\n")
cat(sprintf("  ages compared              : %d\n", nrow(cmp)))
cat(sprintf("  model inside the STS 95%% CI: %.0f%%\n", 100 * mean(cmp$model_inside_sts_ci)))
cat(sprintf("  median difference          : %+.4f\n", median(cmp$diff)))
cat(sprintf("  mean absolute difference   : %.4f\n", mean(abs(cmp$diff))))
cat(sprintf("  correlation over age       : %.3f\n", cor(cmp$model, cmp$sts)))
