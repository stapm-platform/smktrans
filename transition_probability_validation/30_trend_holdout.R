# Hold out the last year and ask the trend model to predict it.
#
# This is the acceptance test for any change to trend_fit. Nothing about the
# model - formula, capacity, spline knots, anything - gets decided by in-sample
# fit or by argument. A candidate fits on every year except the last one in the
# data, predicts that year, and is scored against the people actually surveyed
# in it. That is the same task the pipeline performs for real whenever the grid
# runs a year past the data, and it is the task that decides whether a surface
# is trustworthy when the next year of data arrives.
#
# Three numbers per candidate, and what each one means:
#
#   deviance    - weighted multinomial deviance per unit of survey weight on the
#                 held-out people. The likelihood the model assigns to what was
#                 actually observed. Lower is better; this is the headline.
#   prev_error  - mean absolute error in weighted smoking prevalence by
#                 sex x IMD x age band, averaged over states. The deviance can
#                 hide an aggregate bias; this catches it.
#   boot_sd     - the spread of the held-out year's predicted current-smoking
#                 prevalence across bootstrap refits. Two candidates with the
#                 same error are not the same if one swings twice as far on a
#                 resample; this is the stability half of "accurate and robust".
#                 Read it last, not first: stability can be bought with bias -
#                 a constant returns zero spread - so it only ranks candidates
#                 whose deviance is already competitive.
#
# Every scoreboard also carries a "persistence" row: last training year's
# observed prevalence, carried forward, no model at all. A surface that cannot
# beat carrying last year forward has no business extrapolating anything.
#
# The harness takes the fitting function as an argument, so the current
# trend_fit, a spline variant, or any rung of a capacity ladder can be scored
# by the same ruler without touching this file.

library(data.table)


#' Score one trend-fitting candidate on a held-out year
#'
#' @param data Individual survey records: one row per person with the smoking
#'   state, age, year, sex, IMD quintile and survey weight columns that
#'   trend_fit expects.
#' @param fit_fun The fitting function. Must accept the data plus grid_ages,
#'   grid_years, grid_sex, grid_imd, allow_extrapolation, and return a table
#'   with age, year, sex, imd_quintile and one probability column per state.
#' @param fit_args Named list of further arguments passed to fit_fun - this is
#'   where a formula_version or a ladder rung goes.
#' @param holdout_year The year to hold out. Defaults to the last year present.
#' @param age_bands Cut points for the prevalence-error cells.
#' @param boot_B Bootstrap refits for the stability number. 0 skips it.
#' @param boot_seed Seed for the bootstrap resampling, used locally so the
#'   caller's random number stream is left exactly as it was found.
#' @param states The smoking states expected in the data and the predictions.
#' @param min_holdout_n Stop if the held-out year has fewer people than this:
#'   a score against a handful of respondents is noise wearing a number.
trend_holdout <- function(data,
                          fit_fun,
                          fit_args = list(),
                          holdout_year = NULL,
                          age_bands = c(16, 25, 35, 50, 65, 80, 90),
                          boot_B = 30,
                          boot_seed = 20260718,
                          states = c("current", "former", "never"),
                          min_holdout_n = 500) {

  d <- as.data.table(copy(data))
  needed <- c("smk.state", "age", "year", "sex", "imd_quintile", "wt_int")
  miss <- setdiff(needed, names(d))
  if (length(miss) > 0) {
    stop("trend_holdout: data has no ", paste(miss, collapse = ", "),
         ". Rename with fit_args on the fitter, but the harness needs the ",
         "standard names to score with.")
  }

  yrs <- sort(unique(d$year))
  if (length(yrs) < 4) stop("trend_holdout: ", length(yrs), " years of data is not ",
                            "enough to hold one out and still fit a trend.")
  if (is.null(holdout_year)) holdout_year <- max(yrs)
  if (!holdout_year %in% yrs) stop("trend_holdout: ", holdout_year, " is not in the data.")
  if (holdout_year != max(yrs)) {
    message("trend_holdout: holding out ", holdout_year, ", which is not the last year. ",
            "That makes this an interpolation test, which is easier than the ",
            "extrapolation the pipeline actually performs. Fine for diagnosis, ",
            "not for acceptance.")
  }

  train <- d[year < holdout_year | year > holdout_year]
  train <- train[year != holdout_year]
  test  <- d[year == holdout_year]
  if (nrow(test) < min_holdout_n) {
    stop("trend_holdout: only ", nrow(test), " people in ", holdout_year,
         ", below min_holdout_n = ", min_holdout_n, ".")
  }

  # The grid is pinned from the FULL data, holdout year included, so the fit is
  # asked to predict it - and asked explicitly, which is what the
  # allow_extrapolation flag is for.
  grid_ages <- min(d$age):max(d$age)
  grid_sex  <- sort(unique(as.character(d$sex)))
  grid_imd  <- sort(unique(as.character(d$imd_quintile)))

  run_fit <- function(train_data) {
    do.call(fit_fun, c(list(data = train_data,
                            grid_ages  = grid_ages,
                            grid_years = c(sort(unique(train_data$year)), holdout_year),
                            grid_sex   = grid_sex,
                            grid_imd   = grid_imd,
                            allow_extrapolation = TRUE),
                       fit_args))
  }

  # ---- Scoring --------------------------------------------------------------

  score_predictions <- function(pred) {
    pmiss <- setdiff(states, names(pred))
    if (length(pmiss) > 0) {
      stop("trend_holdout: the fitter's output has no ", paste(pmiss, collapse = ", "), ".")
    }
    p <- merge(test, pred[year == holdout_year],
               by = c("age", "year", "sex", "imd_quintile"), all.x = TRUE)
    n_unmatched <- p[is.na(get(states[1])), .N]
    if (n_unmatched > 0) {
      stop("trend_holdout: ", n_unmatched, " held-out people have no matching ",
           "prediction cell. The grid does not cover the data it is being scored on.")
    }

    # Deviance: the probability the model gave to each person's observed state.
    # Clamped at 1e-12 so a confidently wrong model is punished, not infinite,
    # and the clamping is counted rather than silent.
    p[, p_obs := fifelse(smk.state == states[1], get(states[1]),
                  fifelse(smk.state == states[2], get(states[2]), get(states[3])))]
    n_clamped <- p[p_obs < 1e-12, .N]
    if (n_clamped > 0) {
      message("trend_holdout: ", n_clamped, " people were given probability < 1e-12 ",
              "for their observed state. Clamped for the log; a model doing this is ",
              "already disqualifying itself.")
      p[p_obs < 1e-12, p_obs := 1e-12]
    }
    deviance <- p[, -2 * sum(wt_int * log(p_obs)) / sum(wt_int)]

    # Prevalence error by cell, weighted both sides by the held-out people's
    # survey weights so the comparison is like for like.
    p[, age_band := cut(age, age_bands, right = FALSE)]
    obs <- p[, c(.N, lapply(states, function(s) sum(wt_int * (smk.state == s)) / sum(wt_int))),
             by = .(sex, imd_quintile, age_band)]
    setnames(obs, c("sex", "imd_quintile", "age_band", "n", paste0("obs_", states)))
    prd <- p[, lapply(states, function(s) sum(wt_int * get(s)) / sum(wt_int)),
             by = .(sex, imd_quintile, age_band)]
    setnames(prd, c("sex", "imd_quintile", "age_band", paste0("pred_", states)))
    cells <- merge(obs, prd, by = c("sex", "imd_quintile", "age_band"))
    err_cols <- paste0("err_", states)
    for (s in states) cells[, (paste0("err_", s)) := get(paste0("pred_", s)) - get(paste0("obs_", s))]
    prev_error <- cells[, mean(unlist(lapply(err_cols, function(cc) mean(abs(get(cc))))))]

    # The worst cell is reported from cells with enough people to mean something.
    # An 80+ cell holding five respondents will top the list every run and say
    # nothing except that five is a small number.
    w <- cells[, .(sex, imd_quintile, age_band, n,
                   worst_err = do.call(pmax, lapply(err_cols, function(cc) abs(get(cc)))))]
    worst <- if (w[n >= 30, .N] > 0) w[n >= 30][order(-worst_err)][1] else w[order(-worst_err)][1]

    list(deviance = deviance, prev_error = prev_error, cells = cells[], worst = worst)
  }

  # ---- The candidate --------------------------------------------------------

  message("trend_holdout: fitting on ", min(train$year), "-", max(train$year),
          ", predicting ", holdout_year, " (", nrow(test), " people held out).")
  pred_main <- run_fit(train)
  sc <- score_predictions(pred_main)

  # ---- Persistence baseline -------------------------------------------------
  # Last training year's observed weighted prevalence per cell, carried forward.
  # No model. Anything that cannot beat this has no business extrapolating.

  last_yr <- max(train$year)
  base <- train[year == last_yr]
  base[, age_band := cut(age, age_bands, right = FALSE)]
  base_prev <- base[, lapply(states, function(s) sum(wt_int * (smk.state == s)) / sum(wt_int)),
                    by = .(sex, imd_quintile, age_band)]
  setnames(base_prev, c("sex", "imd_quintile", "age_band", states))
  tst <- copy(test)[, age_band := cut(age, age_bands, right = FALSE)]
  tb <- merge(tst, base_prev, by = c("sex", "imd_quintile", "age_band"), all.x = TRUE)
  n_nocell <- tb[is.na(get(states[1])), .N]
  if (n_nocell > 0) {
    message("trend_holdout: ", n_nocell, " held-out people sit in a cell empty in ",
            last_yr, "; they are dropped from the persistence baseline only.")
    tb <- tb[!is.na(get(states[1]))]
  }
  tb[, p_obs := fifelse(smk.state == states[1], get(states[1]),
               fifelse(smk.state == states[2], get(states[2]), get(states[3])))]
  tb[p_obs < 1e-12, p_obs := 1e-12]
  base_dev <- tb[, -2 * sum(wt_int * log(p_obs)) / sum(wt_int)]
  base_cells <- merge(
    tb[, c(lapply(states, function(s) sum(wt_int * (smk.state == s)) / sum(wt_int))),
       by = .(sex, imd_quintile, age_band)],
    base_prev, by = c("sex", "imd_quintile", "age_band"))
  setnames(base_cells, c("sex", "imd_quintile", "age_band", paste0("obs_", states), paste0("pred_", states)))
  base_prev_error <- base_cells[, mean(unlist(lapply(states, function(s)
    mean(abs(get(paste0("pred_", s)) - get(paste0("obs_", s)))))))]

  # ---- Bootstrap stability --------------------------------------------------
  # Uniform resample of the training people, weights doing their work inside
  # the fit, house rule. The spread of the held-out year's predicted current
  # prevalence is the stability number.

  boot_sd <- NA_real_
  if (boot_B > 0) {
    message("trend_holdout: ", boot_B, " bootstrap refits for the stability number...")
    old_seed <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
    set.seed(boot_seed)
    boot_prev <- vector("list", boot_B)
    for (b in seq_len(boot_B)) {
      idx <- sample.int(nrow(train), nrow(train), replace = TRUE)
      pb <- tryCatch(run_fit(train[idx]), error = function(e) NULL)
      if (is.null(pb)) {
        message("  refit ", b, " failed and is excluded; a candidate that fails ",
                "under resampling is telling you something.")
        next
      }
      boot_prev[[b]] <- pb[year == holdout_year,
                           .(b = b, p_cur = mean(get(states[1]))),
                           by = .(sex, imd_quintile)]
    }
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, .GlobalEnv) else
      rm(".Random.seed", envir = .GlobalEnv)
    bp <- rbindlist(boot_prev)
    if (nrow(bp) > 0) {
      boot_sd <- bp[, .(sd = sd(p_cur)), by = .(sex, imd_quintile)][, median(sd)]
    }
  }

  # ---- Scoreboard -----------------------------------------------------------

  board <- data.table(
    candidate  = c("fitted model", "persistence (no model)"),
    deviance   = round(c(sc$deviance, base_dev), 5),
    prev_error = round(c(sc$prev_error, base_prev_error), 5),
    boot_sd    = c(round(boot_sd, 5), NA)
  )
  cat("\n--- trend_holdout:", min(train$year), "-", max(train$year),
      "predicting", holdout_year, "---\n\n")
  print(board)
  cat(sprintf("\n  worst prevalence cell: %s, IMD %s, age %s (n = %d), |error| = %.4f\n",
              sc$worst$sex, sc$worst$imd_quintile, as.character(sc$worst$age_band),
              sc$worst$n, sc$worst$worst_err))

  invisible(list(scoreboard = board, cells = sc$cells, holdout_year = holdout_year,
                 train_years = range(train$year)))
}


#' Score several candidates on the same held-out year
#'
#' @param data As trend_holdout.
#' @param candidates A named list; each element is a list with fit_fun and,
#'   optionally, fit_args. The names are the rows of the scoreboard.
#' @param ... Passed to trend_holdout.
trend_holdout_compare <- function(data, candidates, ...) {
  if (is.null(names(candidates)) || any(names(candidates) == "")) {
    stop("trend_holdout_compare: every candidate needs a name; the name is the ",
         "row of the scoreboard and the thing that goes in the run log.")
  }
  out <- rbindlist(lapply(names(candidates), function(nm) {
    message("\n== candidate: ", nm, " ==")
    cc <- candidates[[nm]]
    r <- trend_holdout(data, fit_fun = cc$fit_fun,
                       fit_args = if (is.null(cc$fit_args)) list() else cc$fit_args, ...)
    cbind(candidate_set = nm, r$scoreboard[candidate == "fitted model"])
  }))
  cat("\n=== scoreboard, all candidates ===\n\n")
  print(out[order(deviance)])
  invisible(out)
}
