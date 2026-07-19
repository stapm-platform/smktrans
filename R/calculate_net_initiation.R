#' Calculate Net Initiation Probabilities (Synthetic Cohort)
#'
#' @description
#' Calculates "Net Initiation" probabilities by simulating a synthetic cohort.
#' This metric represents the net flow into the "Current Smoker" state
#' (Initiation + Relapse - Quitting) relative to the non-smoking population at
#' each age.
#'
#' It solves the issue where high initiation rates at young ages are offset by
#' high quit rates (experimentation vs. established smoking).
#'
#' @param init_data Data.table. Initiation probabilities.
#' @param quit_data Data.table. Quit probabilities.
#' @param relapse_data Data.table. Relapse probabilities (must contain 'time_since_quit').
#' @param pops Data.table. Not used. The synthetic cohort is a fixed 1000 people
#'   per subgroup, so there is nothing to weight. Kept in the signature because
#'   process_country_wrapper() and run_bootstrap_pipeline() both pass it.
#' @param config List. Must contain 'country' and 'path'. The uncertainty
#'   parameters this used to document ('kn', 'kn_samp', 'kR') are not used
#'   either: uncertainty is handled by run_bootstrap_pipeline() calling this
#'   once per bootstrap sample.
#' @param boot_mode Logical. If TRUE, skips writing to disk and returns the data.table directly.
#'
#' @details
#' \strong{Assumptions:}
#' \itemize{
#'   \item The cohort starts with 100% Never Smokers at the youngest age present
#'         in the initiation data.
#'   \item Nobody dies. Over ages 12 to 30 that is close enough to true, and it
#'         is what makes the denominator below equal to 1 - prevalence.
#' }
#'
#' \strong{p_start_net can be negative.} Past the age where the cohort's smoking
#' prevalence peaks, quitting runs ahead of initiation and relapse and the net
#' flow turns negative. That is a real feature of the age profile and it is
#' returned as it is. It used to be clamped at zero, which flattened the curve
#' from about age 24 and hid the fact that the model has prevalence peaking
#' there at all.
#'
#' \strong{Time since quitting.}
#'
#' This used to pick a relapse probability by assuming how long people at each
#' age had been quit: 1 year if under 18, 3 years from 18 to 24, 5 years from 25.
#' That produced a step change in p_relapse at exactly 18 and 25 (a 63% drop at
#' 18 on the England data), and because the relapse flow is a large part of the
#' net flow, it put a spurious cliff into the published numbers. Net initiation
#' fell 83% between 17 and 18 for no reason other than the age band changing.
#'
#' It is not necessary to assume any of it. The simulation already carries a
#' stock of former smokers, so carry it BY time since quit instead: quitters
#' enter at time_since_quit 0, survivors move up one year at a time, and the top
#' category absorbs. Every former smoker then has the relapse probability that
#' actually applies to them, and the assumption disappears rather than being
#' replaced by a better one.
#'
#'
#' @import data.table
#' @export
calculate_net_initiation <- function(init_data, quit_data, relapse_data, pops, config, boot_mode = FALSE) {

  if (!boot_mode) message(">> Calculating Net Initiation (Synthetic Cohort)...")

  # 1. Prepare Data
  # -------------------------------------------------------------------------

  cols <- c("year", "age", "sex", "imd_quintile")

  for (nm in c("init_data", "quit_data", "relapse_data")) {
    d <- get(nm)
    miss <- setdiff(cols, names(d))
    if (length(miss) > 0) stop("calculate_net_initiation: ", nm, " has no ", paste(miss, collapse = ", "), ".")
  }
  if (!"time_since_quit" %in% names(relapse_data)) {
    stop("calculate_net_initiation: relapse_data has no time_since_quit.")
  }

  # Take the range from the data rather than hard-coding it. The old version
  # fixed ages 12:29 and years 2011:2019, which silently threw away whatever the
  # estimation had actually produced. The initiation data currently runs 2003 to
  # 2040 and ages 11 to 30, so the old years alone dropped 30 of the 38 years,
  # and anything comparing this against a survey outside 2011-2019 was left with
  # almost no overlap.
  ages  <- sort(unique(init_data$age))
  years <- sort(intersect(unique(init_data$year), unique(quit_data$year)))
  if (length(years) == 0) stop("calculate_net_initiation: init_data and quit_data share no years.")

  dt <- init_data[age %in% ages & year %in% years, c(cols, "p_start"), with = FALSE]
  dt <- merge(dt, quit_data[, c(cols, "p_quit"), with = FALSE], by = cols, all.x = TRUE)
  dt[is.na(p_quit), p_quit := 0]

  # Relapse, one column per time since quit, so every former smoker can be given
  # the probability that applies to them.
  tsq_vals <- sort(unique(relapse_data$time_since_quit))
  n_tsq <- length(tsq_vals)
  r_wide <- dcast(relapse_data[age %in% ages & year %in% years],
                  year + age + sex + imd_quintile ~ time_since_quit,
                  value.var = "p_relapse")
  tsq_cols <- paste0("tsq_", tsq_vals)
  setnames(r_wide, as.character(tsq_vals), tsq_cols)

  dt <- merge(dt, r_wide, by = cols, all.x = TRUE)
  for (cc in tsq_cols) dt[is.na(get(cc)), (cc) := 0]

  # 2. Run Synthetic Cohort Simulation
  # -------------------------------------------------------------------------
  # Vectorised across subgroups, looping only over age. The former-smoker stock
  # is a matrix: one row per subgroup, one column per time since quit.

  grp <- unique(dt[, .(year, sex, imd_quintile)])
  setkeyv(grp, c("year", "sex", "imd_quintile"))
  grp[, .grp_id := .I]
  dt <- merge(dt, grp, by = c("year", "sex", "imd_quintile"))

  n_grp    <- nrow(grp)
  n_never  <- rep(1000, n_grp)
  n_curr   <- rep(0, n_grp)
  n_former <- matrix(0, nrow = n_grp, ncol = n_tsq)   # columns are tsq_vals

  # Iterate only over ages actually present. Some countries' survey data begins
  # at 16 rather than 12.
  loop_ages <- sort(unique(dt$age))

  results_list <- list()
  n_negative <- 0L

  for (a in loop_ages) {

    probs <- dt[age == a]
    if (nrow(probs) == 0) next

    # Every subgroup must be present at every age, or the matrix rows stop
    # lining up with the cohort state and the result is scrambled rather than
    # visibly wrong. The old version merged and carried on.
    if (nrow(probs) != n_grp) {
      stop("calculate_net_initiation: age ", a, " is present for ", nrow(probs),
           " of ", n_grp, " subgroups. The cohort state is indexed by subgroup, ",
           "so every age has to be complete or the rows stop lining up.")
    }
    setorder(probs, .grp_id)
    if (!identical(probs$.grp_id, grp$.grp_id)) {
      stop("calculate_net_initiation: subgroup ordering is not stable at age ", a, ".")
    }

    r_mat <- as.matrix(probs[, tsq_cols, with = FALSE])

    # Flows
    flow_init    <- n_never * probs$p_start
    flow_quit    <- n_curr  * probs$p_quit
    relapse_mat  <- n_former * r_mat            # relapses out of each tsq band
    flow_relapse <- rowSums(relapse_mat)

    # Stocks for the next step
    n_never_next <- n_never - flow_init
    n_curr_next  <- n_curr + flow_init - flow_quit + flow_relapse

    # Net initiation for THIS step. never + current + former is a constant 1000,
    # so dividing through gives (prev(a+1) - prev(a)) / (1 - prev(a)), which is
    # what a survey can be asked for.
    n_non_current <- n_never + rowSums(n_former)
    delta_current <- n_curr_next - n_curr
    p_start_net <- ifelse(n_non_current > 0, delta_current / n_non_current, 0)

    # Not clamped at zero. A negative net flow is a real thing and a useful one:
    # it says quitting is running ahead of initiation plus relapse, so the cohort's
    # smoking prevalence has peaked and is coming down. The old version clamped
    # it, which flattened the curve to a floor of zero from about age 24 and made
    # the biggest cliff in the whole series an artefact of the clamp. It also made
    # the comparison against a survey one-sided, because a survey estimate of the
    # same quantity is free to go negative and ours was not.
    n_negative <- n_negative + sum(p_start_net < 0)

    results_list[[as.character(a)]] <- data.table(
      year = probs$year, sex = probs$sex, imd_quintile = probs$imd_quintile,
      age = a, p_start_net = p_start_net
    )

    # Age the former-smoker stock. Survivors move up one time-since-quit band,
    # this year's quitters enter at the bottom, and the top band absorbs.
    surv <- n_former - relapse_mat
    n_former_next <- matrix(0, nrow = n_grp, ncol = n_tsq)
    n_former_next[, 1] <- flow_quit
    if (n_tsq > 1) {
      n_former_next[, 2:n_tsq] <- surv[, 1:(n_tsq - 1), drop = FALSE]
      n_former_next[, n_tsq] <- n_former_next[, n_tsq] + surv[, n_tsq]
    }

    n_never  <- n_never_next
    n_curr   <- n_curr_next
    n_former <- n_former_next

    # The cohort is closed, so the three stocks have to keep adding to 1000.
    tot <- n_never + n_curr + rowSums(n_former)
    if (any(abs(tot - 1000) > 1e-6)) {
      stop("calculate_net_initiation: the cohort no longer adds to 1000 after age ", a,
           " (worst: ", round(max(abs(tot - 1000)), 6), "). People are being created or lost.")
    }
  }

  net_data <- rbindlist(results_list)
  setkeyv(net_data, c("year", "sex", "imd_quintile", "age"))

  if (!boot_mode && n_negative > 0) {
    message("   ", n_negative, " of ", nrow(net_data), " cells have a negative net flow, ",
            "i.e. quitting running ahead of initiation and relapse combined. These are ",
            "kept rather than clamped: past the age where smoking prevalence peaks, net ",
            "initiation is genuinely negative and saying so is the point.")
  }

  # 3. Save Outputs (Skipped in Boot Mode)
  # -------------------------------------------------------------------------
  if (!boot_mode) {
    out_path <- file.path(config$path, "outputs")
    saveRDS(net_data, file.path(out_path, paste0("net_init_data_", config$country, ".rds")))
  }

  if (boot_mode) {
    return(net_data)
  } else {
    return(invisible(net_data))
  }
}
