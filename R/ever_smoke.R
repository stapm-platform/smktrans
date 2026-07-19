#' Summarise and project trends in ever-smoking
#'
#' @description
#' Fits a weighted GLM (quasibinomial) to the trend in ever-smoking 
#' at age 25-34. This provides the "target" level for the Holford adjustment.
#'
#' @details
#' The candidate models differ only in their interactions. The ones involving
#' year_bin are the ones to be careful with: year_bin enters linearly on the
#' logit scale, and the predictions run from before the data starts out to
#' time_horizon, which is a twenty-odd year extrapolation from about sixteen
#' years of observation. A sex or IMD difference in *level* is safe under that
#' extrapolation; a difference in *slope* compounds with every projected year.
#' So the risk in a richer model is not the extra parameters, it is what those
#' parameters do a long way outside the data.
#'
#' Setting model = "auto" picks the structure from the data, conservatively.
#' Three stages:
#'
#' 1. Score every candidate on held-out time. The last auto_holdout_bins of the
#'    binned years are set aside, each model is fitted to the earlier bins and
#'    scored by weighted deviance on the held-out people. That scores the thing
#'    the projection actually does - predict forward along the fitted trend -
#'    rather than in-sample fit, which interactions can always improve.
#'
#' 2. Prefer simplicity. Candidates are compared on a holdout QAIC: the
#'    held-out deviance divided by the dispersion (estimated once, from the
#'    richest model on the training years, so every candidate is scaled the
#'    same), plus two per parameter. Models within auto_tie_margin of the best
#'    are treated as ties and the one with the fewest coefficients wins - the
#'    usual reading of differences under 2 on that scale. A relative tolerance
#'    does not work here: the deviance is dominated by irreducible person-level
#'    noise, so even a real slope difference only moves it by a fraction of a
#'    percent, and any percentage margin hands the choice to the plain model
#'    every time regardless of the data.
#'
#' 3. Guard the projection. The winner is refitted to all the data and its
#'    projection to time_horizon is checked: every stratum must stay inside
#'    [auto_floor, auto_ceiling], and no stratum's logit slope may exceed
#'    auto_max_slope_mult times the common slope from the main-effects model.
#'    A model that fails falls back to the next admissible candidate, and it
#'    says so. If nothing survives, the main-effects model is used; if even
#'    that fails the range check, that is a data problem and the function
#'    stops rather than projecting it.
#'
#' Passing an explicit "model1" to "model8" behaves exactly as it always has.
#'
#' @param data Data table of individual characteristics.
#' @param time_horizon Integer - the last year for projection.
#' @param num_bins Integer - bins for the period trend to reduce noise.
#' @param model Character - Model specification (interaction terms), or "auto"
#' to select the structure as described above.
#' @param min_age Integer - youngest age for prediction.
#' @param min_year Integer - first year of survey data.
#' @param age_cats Character vector - age category for reference (e.g., "25-34").
#' @param auto_holdout_bins Integer - how many of the most recent year bins to
#' hold out when scoring candidates (model = "auto" only).
#' @param auto_tie_margin Numeric - QAIC margin within which a simpler model is
#' preferred to the best-scoring one (model = "auto" only). 2 is the
#' conventional "no real difference" threshold.
#' @param auto_floor,auto_ceiling Numeric - the projected proportion for every
#' stratum must stay inside this range over the whole projection.
#' @param auto_max_slope_mult Numeric - cap on any stratum's logit slope as a
#' multiple of the main-effects model's common slope.
#' @importFrom data.table := setDT setnames copy
#' @importFrom stats glm predict quasibinomial weighted.mean
#' @export
ever_smoke <- function(
    data,
    time_horizon = 2100,
    num_bins = 7,
    model = "model2", # Default to Model 2 (Sex interaction) as per England standard
    min_age = 15,
    min_year = 2003,
    age_cats = c("25-34"),
    auto_holdout_bins = 2,
    auto_tie_margin = 2,
    auto_floor = 0.02,
    auto_ceiling = 0.98,
    auto_max_slope_mult = 2
) {

  # Copy to avoid modifying original by reference
  dt <- copy(data)

  # Select required variables
  cols <- c("wt_int", "age", "year", "age_cat", "sex", "imd_quintile", "smk.state")
  dt <- dt[, ..cols]

  # Create binary ever smoker variable
  dt[, ever_smoker := ifelse(smk.state == "never", 0, 1)]
  dt[, cohort := year - age]

  # Filter data to reference age category
  dt <- dt[age_cat %in% age_cats]

  # Bin the year variable to smooth out annual survey noise
  dt[, year_bin := bin_var(year, n_bins = num_bins)]

  message("  - Estimating observed proportions...")

  # FAST BYPASS: Use data.table for weighted means instead of survey::svyby
  current_prop <- dt[, .(
    ever_smoker = stats::weighted.mean(ever_smoker, w = wt_int, na.rm = TRUE)
  ), by = .(year_bin, sex, imd_quintile)]

  setnames(current_prop, "year_bin", "year")

  # The candidate structures. Ordered so that a walk down the list only ever
  # adds terms - useful to keep in mind when reading the auto scoreboard.
  formulas <- list(
    "model1" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + imd_quintile:year_bin + sex:imd_quintile,
    "model2" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + imd_quintile:year_bin,
    "model3" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + sex:imd_quintile,
    "model4" = ever_smoker ~ sex + imd_quintile + year_bin + imd_quintile:year_bin + sex:imd_quintile,
    "model5" = ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin,
    "model6" = ever_smoker ~ sex + imd_quintile + year_bin + sex:imd_quintile,
    "model7" = ever_smoker ~ sex + imd_quintile + year_bin + imd_quintile:year_bin,
    "model8" = ever_smoker ~ sex + imd_quintile + year_bin
  )

  fit_one <- function(f, d) {
    stats::glm(f, data = d, family = stats::quasibinomial(link = "logit"), weights = wt_int)
  }

  # Weighted binomial deviance on held-out individuals. The quasibinomial
  # dispersion scales this equally for every candidate, so it drops out of the
  # comparison.
  holdout_deviance <- function(m, d) {
    mu <- as.numeric(stats::predict(m, newdata = d, type = "response"))
    mu <- pmin(pmax(mu, 1e-12), 1 - 1e-12)
    -2 * sum(d$wt_int * (d$ever_smoker * log(mu) + (1 - d$ever_smoker) * log(1 - mu)))
  }

  # The projection grid, needed below both for the guard checks and the output
  newdata <- data.frame(expand.grid(
    year_bin = (min_year - min_age):time_horizon,
    sex = c("Male", "Female"),
    imd_quintile = unique(dt$imd_quintile)
  ))

  if(model == "auto") {

    # Callers that plumb these from a config may pass NULL for an explicit
    # model; under "auto" they must be real numbers.
    for (nm_arg in c("auto_holdout_bins", "auto_tie_margin", "auto_floor",
                     "auto_ceiling", "auto_max_slope_mult")) {
      if (is.null(get(nm_arg))) {
        stop("ever_smoke: model = 'auto' but ", nm_arg, " is NULL.")
      }
    }

    bins <- sort(unique(dt$year_bin))
    if(length(bins) < auto_holdout_bins + 3) {
      stop("ever_smoke: model = 'auto' needs at least ", auto_holdout_bins + 3,
           " year bins to hold ", auto_holdout_bins, " out and still fit a trend, ",
           "but the data has ", length(bins), ". Reduce auto_holdout_bins or num_bins, ",
           "or pick a model explicitly.")
    }

    test_bins <- utils::tail(bins, auto_holdout_bins)
    d_train <- dt[!year_bin %in% test_bins]
    d_test  <- dt[year_bin %in% test_bins]

    # 1. Score every candidate on the held-out years. The dispersion comes
    # from the richest model on the training data and is applied to all of
    # them, so it rescales the comparison without favouring anyone.
    fits <- lapply(formulas, fit_one, d = d_train)
    phi <- summary(fits[["model1"]])$dispersion

    score <- rbindlist(lapply(names(formulas), function(nm) {
      data.table(model = nm,
                 n_par = length(stats::coef(fits[[nm]])),
                 holdout_dev = holdout_deviance(fits[[nm]], d_test))
    }))
    score[, qaic := holdout_dev / phi + 2 * n_par]

    # 2. Simplest model among the ties
    best <- min(score$qaic)
    score[, admissible := qaic <= best + auto_tie_margin]
    setorder(score, -admissible, n_par, -model)
    message("  - Auto model selection, holding out year bins ",
            paste(round(test_bins), collapse = ", "),
            sprintf(" (dispersion %.3f):", phi))
    for(i in seq_len(nrow(score))) {
      message(sprintf("      %s: %2d parameters, holdout QAIC %.1f (dev %.1f)%s",
                      score$model[i], score$n_par[i], score$qaic[i], score$holdout_dev[i],
                      fifelse(score$admissible[i], "", "  (outside margin)")))
    }

    candidates <- score[admissible == TRUE]$model

    # 3. Guard the projection. The reference slope is the main-effects model's
    # common year coefficient: the average decline everyone shares. Richer
    # models can bend stratum slopes around it, but not without limit.
    m_ref <- fit_one(formulas[["model8"]], dt)
    ref_slope <- abs(stats::coef(m_ref)[["year_bin"]])

    strata <- unique(newdata[, c("sex", "imd_quintile")])
    chosen <- NULL

    for(nm in candidates) {
      m_try <- fit_one(formulas[[nm]], dt)

      # Range check over the whole projection
      pr <- as.numeric(stats::predict(m_try, newdata = newdata, type = "response"))
      out_of_range <- sum(pr < auto_floor | pr > auto_ceiling)

      # Slope check: linear on the logit, so one year's difference on the link
      # scale is the slope, per stratum
      s0 <- strata; s0$year_bin <- 2000
      s1 <- strata; s1$year_bin <- 2001
      slopes <- as.numeric(stats::predict(m_try, newdata = s1, type = "link")) -
                as.numeric(stats::predict(m_try, newdata = s0, type = "link"))
      worst <- max(abs(slopes))

      if(out_of_range > 0) {
        message("      ", nm, " rejected: projection leaves [", auto_floor, ", ",
                auto_ceiling, "] in ", out_of_range, " cells before ", time_horizon, ".")
        next
      }
      if(worst > auto_max_slope_mult * ref_slope) {
        message("      ", nm, " rejected: a stratum's logit slope (",
                round(worst, 4), "/year) is more than ", auto_max_slope_mult,
                "x the common slope (", round(ref_slope, 4), "/year). That ",
                "difference compounds over the projection.")
        next
      }
      chosen <- nm
      break
    }

    if(is.null(chosen)) {
      # Nothing admissible survived the guards; the main-effects model is the
      # fallback of last resort, subject to the range check only (it has no
      # stratum slopes to diverge).
      pr <- as.numeric(stats::predict(m_ref, newdata = newdata, type = "response"))
      if(any(pr < auto_floor | pr > auto_ceiling)) {
        stop("ever_smoke: no candidate model, including the main-effects one, keeps ",
             "the projection inside [", auto_floor, ", ", auto_ceiling, "] out to ",
             time_horizon, ". That is not a model selection problem, it is the trend ",
             "itself heading somewhere implausible - look at the data before projecting it.")
      }
      chosen <- "model8"
      message("      no richer candidate survived the guards; using model8.")
    }

    message("  - Selected: ", chosen)
    model <- chosen
  }

  # Model Selection
  f <- formulas[[model]]

  if(is.null(f)) stop("Invalid model selection")

  message(paste("  - Fitting trend model:", model))

  # FAST BYPASS: Use standard glm() instead of survey::svyglm()
  m <- fit_one(f, dt)

  # Generate predictions
  newdata$fitted_trends <- as.numeric(stats::predict(m, type = "response", newdata = newdata))

  setDT(newdata)
  setnames(newdata, "year_bin", "year")

  out <- list(
    data_points = current_prop[],
    predicted_values = newdata[],
    model_choice = model
  )
  # Under "auto" the scoreboard travels with the output, so the choice is
  # auditable from the saved rds and not just the run log.
  if (exists("score", inherits = FALSE)) out$selection <- score[]
  return(out)
}
