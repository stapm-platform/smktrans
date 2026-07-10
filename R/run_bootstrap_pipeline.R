#' Run the Master Bootstrap Pipeline
#'
#' Executes the optimized bootstrapping loop for smoking transition probabilities.
#' This function pre-calculates mortality risks once to save memory, then iterates
#' through `B` bootstrap samples, saving intermediate results to a temporary directory
#' before combining them into final output tables.
#'
#' The fitted smoking trend surface is now collected too. estimate_quitting has
#' always fitted it on every iteration in order to solve for quitting; it just
#' discarded it afterwards. Each replicate is thinned to the ages, years and
#' smoking states the prevalence targets need before it is written to disk,
#' because the full grid at B = 1000 is roughly 38 million rows.
#'
#' @param config A list containing model configuration parameters (e.g., country, years, ages).
#' @param survey_data A data.table or data.frame containing the base survey data.
#' @param pops A data.table containing population denominators.
#' @param tob_mort_data A data.table containing general tobacco mortality data.
#' @param tob_mort_data_cause A data.table containing cause-specific tobacco mortality data.
#' @param B Integer. The number of bootstrap iterations to run. Defaults to 100.
#'
#' @return A list containing six data.tables with all bootstrap iterations combined:
#'   \code{init}, \code{quit}, \code{quit_no_init}, \code{relapse}, \code{net} and \code{trend}.
#' @export
run_bootstrap_pipeline <- function(config, survey_data, pops, tob_mort_data, tob_mort_data_cause, B = 100) {

  temp_dir <- file.path(tempdir(), paste0("smktrans_boot_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  message(sprintf(">> Storing temporary bootstrap files in: %s", temp_dir))

  # What we keep from each trend replicate. Defaults cover Tables 7-10.
  keep_ages   <- if (!is.null(config$trend_keep_ages))   config$trend_keep_ages   else 25:74
  keep_years  <- if (!is.null(config$trend_keep_years))  config$trend_keep_years  else 2011:2019
  keep_states <- if (!is.null(config$trend_keep_states)) config$trend_keep_states else "current"

  # =====================================================================
  # PRE-CALCULATE MORTALITY (Runs exactly once!)
  # =====================================================================
  message("\n>> Pre-calculating biological mortality risks...")

  # We run this on the ORIGINAL, un-resampled survey_data using the exact
  # arguments that estimate_quitting previously used internally.
  master_mortality <- smoke_surv(
    data = survey_data,
    diseases = tobalcepi::tob_disease_names,
    mx_data = tob_mort_data_cause,
    min_age = config$min_age, max_age = config$max_age,
    min_year = config$first_year, max_year = config$last_year
  )
  # =====================================================================

  pb <- txtProgressBar(min = 0, max = B, style = 3)

  message("\n>> Starting Bootstrap Iterations...")

  for (i in seq_len(B)) {
    # 2. Resample Data
    bs_data <- generate_bootstrap_sample(survey_data)

    # 3. Run Estimations (passing boot_mode = TRUE)
    init_res <- estimate_initiation(config, bs_data, boot_mode = TRUE)
    relapse_res <- estimate_relapse(config, bs_data, boot_mode = TRUE)

    # Pass intermediates AND the master_mortality directly to estimate_quitting
    quit_res <- estimate_quitting(
      config = config,
      survey_data = bs_data,
      tob_mort_data = tob_mort_data,
      tob_mort_data_cause = tob_mort_data_cause,
      boot_mode = TRUE,
      smk_init_data_boot = init_res$smk_init_data,
      relapse_data_boot = relapse_res$relapse_data,
      precalc_mortality = master_mortality, # <--- SURGICAL BYPASS HANDOFF
      boot_id = i
    )

    # Extract the final tables BEFORE calculating net initiation
    init_dt <- init_res$final
    quit_dt <- quit_res$final
    relapse_dt <- relapse_res$final
    quit_no_init_dt <- quit_res$final_no_init

    # The trend surface. trend_fit has already stamped boot_id onto it.
    if (is.null(quit_res$trend)) {
      stop("run_bootstrap_pipeline: estimate_quitting returned no trend on iteration ", i,
           ". Check that the patched estimate_quitting.R is the one being sourced.")
    }
    trend_dt <- thin_trend_draws(quit_res$trend, keep_ages, keep_years, keep_states)

    # Calculate net initiation using the final objects
    net_dt <- calculate_net_initiation(init_dt, quit_dt, relapse_dt, pops, config, boot_mode = TRUE)

    # Attach the boot_id
    init_dt[, boot_id := i]
    quit_dt[, boot_id := i]
    quit_no_init_dt[, boot_id := i]
    relapse_dt[, boot_id := i]
    net_dt[, boot_id := i]

    if (!identical(unique(trend_dt$boot_id), i)) {
      stop("run_bootstrap_pipeline: trend replicate ", i, " carries boot_id ",
           paste(unique(trend_dt$boot_id), collapse = "/"), ".")
    }

    # 5. Save the DATATABLES directly to disk
    saveRDS(init_dt, file.path(temp_dir, sprintf("boot_init_%04d.rds", i)))
    saveRDS(quit_dt, file.path(temp_dir, sprintf("boot_quit_%04d.rds", i)))
    saveRDS(quit_no_init_dt, file.path(temp_dir, sprintf("boot_quit_no_init_%04d.rds", i)))
    saveRDS(relapse_dt, file.path(temp_dir, sprintf("boot_relapse_%04d.rds", i)))
    saveRDS(net_dt, file.path(temp_dir, sprintf("boot_net_%04d.rds", i)))
    saveRDS(trend_dt, file.path(temp_dir, sprintf("boot_trend_%04d.rds", i)))

    # Clear memory each loop to prevent RAM bloat on large B runs
    gc(verbose = FALSE)

    setTxtProgressBar(pb, i)
  }

  close(pb)

  # 6. Read all temporary files back and combine them into massive tables
  message("\n>> Reading and aggregating temporary files...")

  read_and_combine <- function(exact_pattern, expected_n = B) {
    # list.files uses regex by default, so exact_pattern will be strictly enforced
    files <- list.files(temp_dir, pattern = exact_pattern, full.names = TRUE)
    if(length(files) == 0) stop(paste("No temporary files found for pattern:", exact_pattern))

    # A short read is a lost iteration. Do not average over it quietly.
    if (length(files) != expected_n) {
      stop(sprintf("Found %d files for pattern '%s' but ran %d iterations.",
                   length(files), exact_pattern, expected_n))
    }

    rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
  }

  out <- list(
    # Using strict Regex anchors (^ for start, $ for end)
    init = read_and_combine("^boot_init_[0-9]+\\.rds$"),
    quit = read_and_combine("^boot_quit_[0-9]+\\.rds$"),
    quit_no_init = read_and_combine("^boot_quit_no_init_[0-9]+\\.rds$"),
    relapse = read_and_combine("^boot_relapse_[0-9]+\\.rds$"),
    net = read_and_combine("^boot_net_[0-9]+\\.rds$"),
    trend = read_and_combine("^boot_trend_[0-9]+\\.rds$")
  )

  # Every trend cell must appear once per iteration, or the covariance matrix
  # downstream is built from ragged columns.
  cell_counts <- out$trend[, .N, by = .(age, year, sex, imd_quintile)]
  if (any(cell_counts$N != B)) {
    stop("run_bootstrap_pipeline: trend cells have between ", min(cell_counts$N),
         " and ", max(cell_counts$N), " draws, expected ", B, " each.")
  }
  if (uniqueN(out$trend$boot_id) != B) {
    stop("run_bootstrap_pipeline: trend draws span ", uniqueN(out$trend$boot_id),
         " boot_ids, expected ", B, ".")
  }

  return(out)
}
