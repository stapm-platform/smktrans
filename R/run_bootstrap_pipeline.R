#' Run Bootstrap Estimation Pipeline (Disk-Backed)
#' 
#' @param config Model configuration list
#' @param survey_data Original survey data
#' @param pops Population data
#' @param tob_mort_data Mortality transitions
#' @param tob_mort_data_cause Cause-specific mortality
#' @param B Number of bootstrap iterations (e.g., 100)
#' @import data.table
run_bootstrap_pipeline <- function(config, survey_data, pops, tob_mort_data, tob_mort_data_cause, B = 100) {
  
  # 1. Create a secure temporary directory for this run
  # Using tempdir() ensures the OS cleans it up eventually, but we will explicitly clean it too.
  temp_dir <- file.path(tempdir(), paste0("smktrans_boot_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Ensure the temp directory is deleted when the function exits (even if it crashes)
  on.exit(unlink(temp_dir, recursive = TRUE)) 
  
  message(sprintf(">> Storing temporary bootstrap files in: %s", temp_dir))
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  
  for (i in seq_len(B)) {
    # 2. Resample Data
    bs_data <- generate_bootstrap_sample(survey_data)
    
    # 3. Run Estimations (passing boot_mode = TRUE)
    init_res <- estimate_initiation(config, bs_data, boot_mode = TRUE)
    relapse_res <- estimate_relapse(config, bs_data, boot_mode = TRUE)
    
    # Pass the intermediates directly to estimate_quitting
    quit_res <- estimate_quitting(
      config = config, 
      survey_data = bs_data, 
      tob_mort_data = tob_mort_data, 
      tob_mort_data_cause = tob_mort_data_cause,
      boot_mode = TRUE,
      smk_init_data_boot = init_res$smk_init_data,
      relapse_data_boot = relapse_res$relapse_data
    )
    
    # Extract the final tables BEFORE calculating net initiation
    init_dt <- init_res$final
    quit_dt <- quit_res$final
    relapse_dt <- relapse_res$final
    quit_no_init_dt <- quit_res$final_no_init 
    
    # Calculate net initiation using the final objects (ADDED boot_mode = TRUE!)
    net_dt <- calculate_net_initiation(init_dt, quit_dt, relapse_dt, pops, config, boot_mode = TRUE)
    
    # Attach the boot_id
    init_dt[, boot_id := i]
    quit_dt[, boot_id := i]
    quit_no_init_dt[, boot_id := i]
    relapse_dt[, boot_id := i]
    net_dt[, boot_id := i]
    
    # 5. Save the DATATABLES directly to disk to free up RAM (NOT the result lists)
    saveRDS(init_dt, file.path(temp_dir, sprintf("boot_init_%04d.rds", i)))
    saveRDS(quit_dt, file.path(temp_dir, sprintf("boot_quit_%04d.rds", i)))
    saveRDS(quit_no_init_dt, file.path(temp_dir, sprintf("boot_quit_no_init_%04d.rds", i)))
    saveRDS(relapse_dt, file.path(temp_dir, sprintf("boot_relapse_%04d.rds", i)))
    saveRDS(net_dt, file.path(temp_dir, sprintf("boot_net_%04d.rds", i)))
    
    # Force R's garbage collector to free up memory from the iteration
    gc(verbose = FALSE)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # 6. Read all temporary files back and combine them into massive tables
  message("\n>> Reading and aggregating temporary files...")
  
  # Helper function to read and rbind all files matching a pattern
  read_and_combine <- function(pattern) {
    files <- list.files(temp_dir, pattern = pattern, full.names = TRUE)
    if(length(files) == 0) stop(paste("No temporary files found for pattern:", pattern))
    
    rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
  }
  
  # Return the combined data (ADDED quit_no_init!)
  return(list(
    init = read_and_combine("boot_init_"),
    quit = read_and_combine("boot_quit_"),
    quit_no_init = read_and_combine("boot_quit_no_init_"),
    relapse = read_and_combine("boot_relapse_"),
    net = read_and_combine("boot_net_")
  ))
}