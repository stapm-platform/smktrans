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
# That happens to be exactly what calculate_net_initiation() produces:
#
#   p_start_net = (n_current(a+1) - n_current(a)) / (n_never(a) + n_former(a))
#
# and since never + current + former is a constant 1000, the denominator is
# 1000 - n_current(a). Dividing through:
#
#   p_start_net = (prev(a+1) - prev(a)) / (1 - prev(a))
#
# which is what we can compute from STS prevalence. The comparison is like for
# like. It is also a period comparison on both sides: calculate_net_initiation
# runs a separate synthetic cohort for each year through that year's
# probabilities, so it is an age profile for a year, not a real birth cohort.
#
# Where it stops working. Net initiation is only informative while the number of
# smokers is still growing. Once quitting overtakes initiation the numerator
# goes negative and the quantity stops meaning "initiation". The model clamps
# negatives to zero (see calculate_net_initiation), so past that age it is
# comparing a clamped zero against noise. The age range below is chosen from the
# data rather than assumed.

source("transition_probability_validation/00_validation_utils.R")


# ---- Parameters -----------------------------------------------------------

val_years  <- 2019:2023
sts_ages   <- 16:35        # read wider than we plot, so the difference at the
                           # top of the plotted range uses a real neighbour
boot_B     <- 1000
boot_seed  <- 20260716
smooth_df  <- 6            # for the prevalence curve, see 00_validation_utils.R


# ---- STS side -------------------------------------------------------------

data_tk <- sts_read_england(years = val_years, ages = sts_ages)

# sts_net_init_by_age lives in 00_validation_utils.R so that the report can use
# it too. See there for why the prevalence is smoothed before it is differenced.

net_sts <- sts_boot(
  dt          = data_tk,
  fn          = function(d, a) sts_net_init_by_age(d, a, smooth_df = smooth_df),
  value_var   = "p_start_net",
  domain_ages = sts_ages,
  B           = boot_B,
  seed        = boot_seed
)


# ---- Choose the age range from the data, do not assume it ------------------
# Net initiation only means "initiation" while prevalence is still rising. Find
# the last age at which the central estimate is still positive, and stop there.

prev_point <- sts_net_init_by_age(data_tk, sts_ages, smooth_df = smooth_df)
last_rising <- suppressWarnings(max(prev_point[!is.na(p_start_net) & p_start_net > 0]$age))

if (!is.finite(last_rising)) {
  stop("22_validate_net_initiation: prevalence is never rising over ",
       paste(range(sts_ages), collapse = "-"),
       ", so net initiation cannot be validated from these data.")
}

plot_ages <- min(sts_ages):last_rising
message("Prevalence stops rising after age ", last_rising,
        ". Validating over ages ", min(plot_ages), "-", max(plot_ages), ".")
if (last_rising < 20) {
  warning("Prevalence stops rising at age ", last_rising, ", which leaves a very ",
          "short window. Check the STS prevalence curve before reading anything ",
          "into this plot.")
}


# ---- smktrans side --------------------------------------------------------

net_model <- stapm_load("net_init_data_England_uncertainty.rds")

if (!"p_start_net" %in% names(net_model)) {
  stop("22_validate_net_initiation: net_init_data has no p_start_net. Columns: ",
       paste(names(net_model), collapse = ", "))
}

# calculate_net_initiation only runs for 2011:2019, so it will not cover the
# whole STS window. Use the overlap and say so, rather than quietly comparing
# different years on the two sides.
model_years <- intersect(val_years, unique(net_model$year))
if (length(model_years) == 0) {
  stop("22_validate_net_initiation: net_init_data covers ",
       paste(range(net_model$year), collapse = "-"),
       " but the STS window is ", paste(range(val_years), collapse = "-"),
       ". No overlap, so there is nothing to compare. Either widen the STS ",
       "window or extend the years in calculate_net_initiation().")
}
if (!identical(sort(model_years), sort(val_years))) {
  message("Note: STS window is ", paste(range(val_years), collapse = "-"),
          " but net initiation is only modelled for ",
          paste(range(model_years), collapse = "-"),
          ". Comparing on the overlap: ", paste(model_years, collapse = ", "), ".")
}

net_ref <- pop_weight_by_age(net_model, "p_start_net",
                             years = model_years, ages = plot_ages)
setnames(net_ref, "value", "p_start_net")


# ---- Plot -----------------------------------------------------------------

sts_plot   <- net_sts[age %in% plot_ages & !is.na(est)]
model_plot <- net_ref[age %in% plot_ages]

p <- ggplot() +
  geom_line(data = model_plot, aes(x = age, y = p_start_net, colour = "smktrans estimate"),
            linewidth = 1) +
  geom_ribbon(data = sts_plot, aes(x = age, ymin = lower, ymax = upper,
                                   fill = "Smoking Toolkit Study"), alpha = 0.2) +
  geom_point(data = sts_plot, aes(x = age, y = est, colour = "Smoking Toolkit Study"),
             size = 1.5) +
  scale_colour_manual(values = c("smktrans estimate" = "#1f78b4",
                                 "Smoking Toolkit Study" = "#33a02c")) +
  scale_fill_manual(values = c("Smoking Toolkit Study" = "#33a02c")) +
  theme_minimal() +
  labs(x = "Age", y = "P(net initiation)", colour = NULL, fill = NULL,
       title = "Net smoking initiation, England",
       subtitle = sprintf(paste0("smktrans (%s) vs Smoking Toolkit Study (%s). ",
                                 "Validated only while prevalence is rising."),
                          paste(range(model_years), collapse = "-"),
                          paste(range(val_years), collapse = "-"))) +
  theme(legend.position = "bottom")

print(p)

ggsave("transition_probability_validation/outputs/validation_net_initiation_england.png",
       p, width = 9, height = 5.5, dpi = 150)


# ---- Numerical summary ----------------------------------------------------

cmp <- merge(sts_plot[, .(age, sts = est, sts_lo = lower, sts_hi = upper)],
             model_plot[, .(age, model = p_start_net)], by = "age")
cmp[, model_inside_sts_ci := model >= sts_lo & model <= sts_hi]

cat("\n--- net initiation: smktrans vs STS, ages", min(cmp$age), "-", max(cmp$age), "---\n")
cat(sprintf("  ages compared              : %d\n", nrow(cmp)))
cat(sprintf("  model inside the STS 95%% CI: %.0f%%\n", 100 * mean(cmp$model_inside_sts_ci)))
cat(sprintf("  median difference          : %+.5f\n", median(cmp$model - cmp$sts)))
cat(sprintf("  correlation over age       : %.3f\n", cor(cmp$model, cmp$sts)))
