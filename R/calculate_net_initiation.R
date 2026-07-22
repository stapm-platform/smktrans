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
#'   \item The cohort is synthetic within a single year: ages are iterated with
#'         the year held fixed, so the stocks at age a are those of a lifetime
#'         lived under that one year's rates. Under secular change this differs
#'         from a real cohort's stocks, which matters when comparing against a
#'         cohort-followed survey estimator - see the header of
#'         22_validate_net_initiation.R for the direction and size.
#'   \item Quit and relapse probabilities must cover every age present in the
#'         initiation data; the function stops if they do not. On the current
#'         pipeline they always do -- the relapse table extends below 18 by
#'         carrying the age-18 values, the same convention as everywhere else,
#'         so this calculation assumes nothing about under-18 relapse that the
#'         main estimates do not.
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
#' Quitters enter at time_since_quit 0, ongoing quitters move up one year at a time, and the top
#' category absorbs. Every former smoker then has the relapse probability that
#' actually applies to them.
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

  # Take the range from the data 
  ages  <- sort(unique(init_data$age))
  years <- sort(intersect(unique(init_data$year), unique(quit_data$year)))
  if (length(years) == 0) stop("calculate_net_initiation: init_data and quit_data share no years.")

  dt <- init_data[age %in% ages & year %in% years, c(cols, "p_start"), with = FALSE]
  dt <- merge(dt, quit_data[, c(cols, "p_quit"), with = FALSE], by = cols, all.x = TRUE)
  if (dt[is.na(p_quit), .N] > 0) {
    stop("calculate_net_initiation: no quit probability for ages ",
         paste(sort(unique(dt[is.na(p_quit), age])), collapse = ", "),
         " that the initiation data covers. Zero-filling here would quietly ",
         "assume nobody quits at those ages.")
  }

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
  na_relapse <- dt[rowSums(is.na(dt[, tsq_cols, with = FALSE])) > 0]
  if (nrow(na_relapse) > 0) {
    stop("calculate_net_initiation: no relapse probability for ages ",
         paste(sort(unique(na_relapse$age)), collapse = ", "),
         " that the initiation data covers. The relapse table is expected to ",
         "extend to the youngest initiation age (currently by carrying the ",
         "age-18 values down, as elsewhere in the pipeline). Zero-filling here ",
         "would quietly assume under-age quitters never relapse.")
  }

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
    # smoking prevalence has peaked and is coming down.
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
