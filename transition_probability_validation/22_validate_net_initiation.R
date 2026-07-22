# Validate the estimated NET initiation probabilities against the Smoking
# Toolkit Study.
#
# Why net initiation and not p_start.
#
# The STS is a repeat cross-section. It never observes the same person twice, so
# it cannot see anyone start smoking. What it can see is the number of current
# smokers rising with age, and that rise is initiation net of quitting. So the
# only initiation quantity the STS can identify is the net one.
#
# That is close to what calculate_net_initiation() produces:
#
#   p_start_net = (n_current(a+1) - n_current(a)) / (n_never(a) + n_former(a))
#
# and since never + current + former is a constant 1000, the denominator is
# 1000 - n_current(a). Dividing through:
#
#   p_start_net = (prev(a+1) - prev(a)) / (1 - prev(a))
#
# which is what we can compute from STS prevalence. But the two sides are NOT
# fully like for like, and the difference matters past the mid-20s. The model
# runs a separate synthetic cohort for each year, so its stocks at age a are
# the ones a cohort would have if it had lived its whole life under that single
# year's rates. The STS pseudo-cohort estimator follows a real cohort, whose
# stocks reflect the actual history of higher initiation ten years earlier.
# Under secular decline the synthetic stocks are smaller than the real ones at
# every age past about 20, so the model's quit outflow - p_quit times the
# current-smoker stock - is smaller, and the model's net flow sits above
# (less negative than) the STS diagonal at 25+ even when every probability in
# it is right. Roughly: gap ~ p_quit * (real prev - synthetic prev) / (1 -
# prev), which at 25-30 is about 0.11 * 0.08 / 0.85 ~ 0.010, the size of the
# gap seen in practice. The STS diagonal additionally absorbs composition
# change (migration and response drift between waves), which the closed
# synthetic cohort excludes, and its 16-17 points are boundary derivatives of
# a smoothed curve, which is where a smoother is least reliable. So treat the
# 25+ comparison as descriptive, not as a pass/fail test of the model.
#
# Where it stops working. Net initiation is only informative while the number
# of smokers is still growing. Past the peak the quantity means net quitting,
# and the stock-vintage mismatch above dominates it. Negative model values are
# real and are NOT clamped (calculate_net_initiation stopped clamping; see its
# roxygen), so the curve past the peak is meaningful in its own terms - it is
# the ruler, not the model, that measures a different thing there.

source("transition_probability_validation/00_validation_utils.R")


# ---- Parameters -----------------------------------------------------------

# Validation years. These are chosen to sit entirely inside the model's
# ESTIMATION window, for two reasons. First, initiation is only estimated to
# 2017: the forecast jumps off at last_year - 1, so 2018 onwards in the
# published outputs is trend continuation, and comparing a forecast against a
# survey tests the forecast, not the estimation. Second, the STS switched to
# telephone interviewing in April 2020 and barely sampled 16-17 year olds for
# the next two years (cells of 6-29 people), so any window touching 2020-21 is
# soft at exactly the ages that matter for initiation. Waves in 2013-2017 are
# face-to-face with full sampling, and their 12-month lookback reaches into 2012
# at the earliest, which is still inside the estimation window.
val_years <- 2013:2017
sts_ages   <- 16:35        # read wider than we plot, so the difference at the

# Defined here, not inherited. An earlier version of this script used plot_ages
# without defining it, and because these scripts get sourced into one session it
# quietly picked up the QUIT script's 25:80 - which cut the initiation plot off
# at exactly the ages where initiation happens. The quit restriction is about
# the quit denominator and has no business here. 16 is the STS sampling floor;
# 34 is the last age the cohort diagonal reaches when reading to 35. The model
# line will stop at 30 because the initiation pipeline runs to age 30, and the
# STS carrying on without it past 30 is informative, not a mismatch.
plot_ages  <- 16:34

                           # top of the plotted range uses a real neighbour
boot_B     <- 1000
boot_seed  <- 20260716
smooth_df  <- 6            # for the prevalence curve, see 00_validation_utils.R


# ---- STS side -------------------------------------------------------------

data_tk <- sts_read_england(years = val_years, ages = sts_ages)

# sts_net_init_by_age lives in 00_validation_utils.R so that the report can use
# it too. See there for why the prevalence is smoothed before it is differenced.

net_domain <- sts_domain(sts_ages)

# Two STS estimators, deliberately shown together.
#
# The cross-sectional one differences prevalence over age within the pooled
# sample. Its age gradient compares different birth cohorts, and with initiation
# having fallen over 2005-2019 the older cohorts carry more smoking, so it reads
# net initiation as staying positive to older ages than it really does. That is
# exactly where we are trying to judge the model, so it is the wrong ruler there.
#
# The pseudo-cohort one follows the same birth cohort from one survey year to
# the next - prev(a+1, t+1) against prev(a, t) - which cancels the cohort effect
# by construction. It is the primary comparison. The cross-sectional one stays on
# the plot so the size of the cohort effect is visible rather than asserted: the
# gap between the two green series IS the cohort effect, measured from the data.

net_sts_cohort <- sts_boot(
  dt        = data_tk,
  fn        = function(d, dom) sts_net_init_cohort(d, dom, smooth_df = smooth_df),
  value_var = "p_start_net",
  domain    = net_domain,
  B         = boot_B,
  seed      = boot_seed
)

net_sts_cross <- sts_boot(
  dt        = data_tk,
  fn        = function(d, dom) sts_net_init_by_age(d, dom, smooth_df = smooth_df),
  value_var = "p_start_net",
  domain    = net_domain,
  B         = boot_B,
  seed      = boot_seed
)

# ---- smktrans side --------------------------------------------------------

net_model <- stapm_load("net_init_data_England_uncertainty.rds")

if (!"p_start_net" %in% names(net_model)) {
  stop("22_validate_net_initiation: net_init_data has no p_start_net. Columns: ",
       paste(names(net_model), collapse = ", "))
}

# Two alignment rules, both stated rather than implicit. First, the STS cohort
# diagonal indexes each transition by its STARTING year t, and the last survey
# year only ever appears as a destination, so the diagonals span
# val_years[-length(val_years)]. Averaging the model over all of val_years
# would include one year of rates the STS side never uses. Second,
# calculate_net_initiation may not cover the whole window; use the overlap and
# say so, rather than quietly comparing different years on the two sides.
transition_years <- val_years[-length(val_years)]
model_years <- intersect(transition_years, unique(net_model$year))
if (length(model_years) == 0) {
  stop("22_validate_net_initiation: net_init_data covers ",
       paste(range(net_model$year), collapse = "-"),
       " but the STS window is ", paste(range(val_years), collapse = "-"),
       ". No overlap, so there is nothing to compare. Either widen the STS ",
       "window or extend the years in calculate_net_initiation().")
}
if (!identical(sort(model_years), sort(transition_years))) {
  message("Note: STS transitions cover ", paste(range(transition_years), collapse = "-"),
          " but net initiation is only modelled for ",
          paste(range(model_years), collapse = "-"),
          ". Comparing on the overlap: ", paste(model_years, collapse = ", "), ".")
}

net_ref <- pop_weight_by_age(net_model, "p_start_net",
                             years = model_years, ages = plot_ages)
setnames(net_ref, "value", "p_start_net")


# ---- Plot -----------------------------------------------------------------

sts_coh   <- net_sts_cohort[age %in% plot_ages & !is.na(est)]
sts_cro   <- net_sts_cross[age %in% plot_ages & !is.na(est)]
model_plot <- net_ref[age %in% plot_ages]

p <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(data = sts_coh, aes(x = age, ymin = lower, ymax = upper,
                                  fill = "STS, cohorts followed"), alpha = 0.2) +
  geom_point(data = sts_coh, aes(x = age, y = est, colour = "STS, cohorts followed"),
             size = 1.6) +
  geom_point(data = sts_cro, aes(x = age, y = est, colour = "STS, single cross-section"),
             size = 1.6, shape = 1) +
  geom_line(data = model_plot, aes(x = age, y = p_start_net, colour = "smktrans estimate"),
            linewidth = 1) +
  scale_colour_manual(values = c("smktrans estimate" = "#1f78b4",
                                 "STS, cohorts followed" = "#33a02c",
                                 "STS, single cross-section" = "#b2df8a")) +
  scale_fill_manual(values = c("STS, cohorts followed" = "#33a02c")) +
  theme_minimal() + theme(legend.position = "bottom") +
  labs(x = "Age", y = "P(net initiation)", colour = NULL, fill = NULL,
       title = "Net smoking initiation, England",
       subtitle = paste0("Following cohorts across survey years removes the cohort effect\n",
                         "that inflates the single cross-section at older ages. Band is 95%."))

print(p)

ggsave("transition_probability_validation/outputs/validation_net_initiation_england.png",
       p, width = 9, height = 5.5, dpi = 150)


# ---- Numerical summary ----------------------------------------------------

cmp <- merge(sts_coh[, .(age, sts = est, sts_lo = lower, sts_hi = upper)],
             model_plot[, .(age, model = p_start_net)], by = "age")
cmp[, model_inside_sts_ci := model >= sts_lo & model <= sts_hi]

# The interval-coverage statistic is only a test where the two sides measure
# the same thing. Past the mid-20s they do not (the stock-vintage mismatch in
# the header), so coverage is reported for 16-24 and the full range is
# summarised by shape (correlation) and the median gap, which the mismatch
# affects far less than it affects coverage.
strict <- cmp[age %in% 16:24]

cat("\n--- net initiation: smktrans vs STS ---\n")
cat(sprintf("  ages compared                        : %d (%d-%d)\n",
            nrow(cmp), min(cmp$age), max(cmp$age)))
cat(sprintf("  model inside the STS 95%% CI, 16-24   : %.0f%% (%d of %d ages)\n",
            100 * mean(strict$model_inside_sts_ci), sum(strict$model_inside_sts_ci), nrow(strict)))
cat(sprintf("  median difference, full range        : %+.5f\n", median(cmp$model - cmp$sts)))
cat(sprintf("  correlation over age, full range     : %.3f\n", cor(cmp$model, cmp$sts)))
cat("  25+ is descriptive, not pass/fail: see the estimand note in the header.\n")
