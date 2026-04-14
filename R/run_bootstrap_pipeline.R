#' Run the Master Bootstrap Pipeline
#'
#' Executes the optimized bootstrapping loop for smoking transition probabilities.
#' This function pre-calculates mortality risks once to save memory, then iterates
#' through `B` bootstrap samples, saving intermediate results to a temporary directory
#' before combining them into final output tables.
#'
#' @param config A list containing model configuration parameters (e.g., country, years, ages).
#' @param survey_data A data.table or data.frame containing the base survey data.
#' @param pops A data.table containing population denominators.
#' @param tob_mort_data A data.table containing general tobacco mortality data.
#' @param tob_mort_data_cause A data.table containing cause-specific tobacco mortality data.
#' @param B Integer. The number of bootstrap iterations to run. Defaults to 100.
#'
#' @return A list containing five massive data.tables with all bootstrap iterations combined:
#'   \code{init}, \code{quit}, \code{quit_no_init}, \code{relapse}, and \code{net}.
#' @export
run_bootstrap_pipeline <- function(config, survey_data, pops, tob_mort_data, tob_mort_data_cause, B = 100) {
  
  temp_dir <- file.path(tempdir(), paste0("smktrans_boot_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE)) 
  
  message(sprintf(">> Storing temporary bootstrap files in: %s", temp_dir))
  
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
      precalc_mortality = master_mortality # <--- SURGICAL BYPASS HANDOFF
    )
    
    # Extract the final tables BEFORE calculating net initiation
    init_dt <- init_res$final
    quit_dt <- quit_res$final
    relapse_dt <- relapse_res$final
    quit_no_init_dt <- quit_res$final_no_init 
    
    # Calculate net initiation using the final objects
    net_dt <- calculate_net_initiation(init_dt, quit_dt, relapse_dt, pops, config, boot_mode = TRUE)
    
    # Attach the boot_id
    init_dt[, boot_id := i]
    quit_dt[, boot_id := i]
    quit_no_init_dt[, boot_id := i]
    relapse_dt[, boot_id := i]
    net_dt[, boot_id := i]
    
    # 5. Save the DATATABLES directly to disk
    saveRDS(init_dt, file.path(temp_dir, sprintf("boot_init_%04d.rds", i)))
    saveRDS(quit_dt, file.path(temp_dir, sprintf("boot_quit_%04d.rds", i)))
    saveRDS(quit_no_init_dt, file.path(temp_dir, sprintf("boot_quit_no_init_%04d.rds", i)))
    saveRDS(relapse_dt, file.path(temp_dir, sprintf("boot_relapse_%04d.rds", i)))
    saveRDS(net_dt, file.path(temp_dir, sprintf("boot_net_%04d.rds", i)))
    
    # Clear memory each loop to prevent RAM bloat on large B runs
    gc(verbose = FALSE) 
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # 6. Read all temporary files back and combine them into massive tables
  message("\n>> Reading and aggregating temporary files...")
  
  read_and_combine <- function(exact_pattern) {
    # list.files uses regex by default, so exact_pattern will be strictly enforced
    files <- list.files(temp_dir, pattern = exact_pattern, full.names = TRUE)
    if(length(files) == 0) stop(paste("No temporary files found for pattern:", exact_pattern))
    
    rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
  }
  
  return(list(
    # Using strict Regex anchors (^ for start, $ for end)
    init = read_and_combine("^boot_init_[0-9]+\\.rds$"),
    quit = read_and_combine("^boot_quit_[0-9]+\\.rds$"),
    quit_no_init = read_and_combine("^boot_quit_no_init_[0-9]+\\.rds$"),
    relapse = read_and_combine("^boot_relapse_[0-9]+\\.rds$"),
    net = read_and_combine("^boot_net_[0-9]+\\.rds$")
  ))
}