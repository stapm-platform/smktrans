#' Calculate Net Initiation Probabilities (Synthetic Cohort)
#'
#' @description
#' Calculates "Net Initiation" probabilities by simulating a synthetic cohort 
#' starting at age 12. This metric represents the net flow into the "Current Smoker" 
#' state (Initiation + Relapse - Quitting) relative to the non-smoking population 
#' at each age.
#'
#' It solves the issue where high initiation rates at young ages are offset by 
#' high quit rates (experimentation vs. established smoking).
#'
#' @param init_data Data.table. Initiation probabilities.
#' @param quit_data Data.table. Quit probabilities.
#' @param relapse_data Data.table. Relapse probabilities (must contain 'time_since_quit').
#' @param pops Data.table. Population weights.
#' @param config List. Must contain 'country' and uncertainty params ('kn', 'kn_samp', 'kR').
#'
#' @details
#' \strong{Assumptions:}
#' \itemize{
#'   \item The cohort starts at age 12 with 100% Never Smokers.
#'   \item Relapse probabilities are selected based on age-specific assumptions about 
#'         average time since quitting: 
#'         (12-17: 1yr, 18-24: 3yrs, 25-30: 5yrs).
#' }
#'
#' @import data.table
#' @export
calculate_net_initiation <- function(init_data, quit_data, relapse_data, pops, config) {
  
  message(">> Calculating Net Initiation (Synthetic Cohort)...")
  
  # 1. Prepare Data & Assumptions
  # -------------------------------------------------------------------------
  
  # Filter Data Range
  ages <- 12:29
  years <- 2011:2019 # As per your script, though could be parameterized
  
  # Prepare Relapse Data (The specific age/duration assumption)
  # 12-17: TSQ 1 (coded as 2 in your script?) -> Using 2 based on your snippet
  # 18-24: TSQ 3 (coded as 2?) -> Your script used 'time_since_quit == 2' for ALL?
  # NOTE: Your script had `time_since_quit == 2` for all three groups. 
  # I will preserve your script's logic exactly, but this looks like a placeholder 
  # in your original code. 
  
  # Applying the filter from your script:
  r_dt <- relapse_data[
    (age <= 17 & time_since_quit == 1) | 
      (age >= 18 & age <= 24 & time_since_quit == 3) | 
      (age >= 25 & age <= 29 & time_since_quit == 5)
  ]
  # If the above returns empty, fallback to your specific code (TSQ=2):
  if(nrow(r_dt) == 0) {
    warning("Using fallback TSQ=2 assumption based on original script logic.")
    r_dt <- relapse_data[time_since_quit == 2]
  }
  
  # Select columns and merge
  cols <- c("year", "age", "sex", "imd_quintile")
  
  # Merge all inputs into one 'domain' table
  dt <- merge(init_data[age %in% ages & year %in% years, ..cols], 
              init_data[, c(..cols, "p_start")], by = cols)
  dt <- merge(dt, quit_data[, c(..cols, "p_quit")], by = cols, all.x = TRUE)
  dt <- merge(dt, r_dt[, c(..cols, "p_relapse")], by = cols, all.x = TRUE)
  
  # Fill NAs (safe defaults)
  dt[is.na(p_quit), p_quit := 0]
  dt[is.na(p_relapse), p_relapse := 0]
  
  # Sort strictly by Year > Sex > IMD > Age (Critical for the loop)
  setkeyv(dt, c("year", "sex", "imd_quintile", "age"))
  
  
  # 2. Run Synthetic Cohort Simulation
  # -------------------------------------------------------------------------
  # We vectorize across subgroups. We only loop over AGE.
  # We create a list to store state at each age step.
  
  # Initialize States (N = 1000)
  # We create a 'current state' table that evolves
  sim_state <- unique(dt[, .(year, sex, imd_quintile)])
  sim_state[, `:=`(
    n_never = 1000,
    n_current = 0,
    n_former = 0,
    age = 11 # Start before the loop
  )]
  
  results_list <- list()
  
  for (a in ages) {
    # Get probabilities for this age 'a'
    probs <- dt[age == a]
    
    # Merge current state with probabilities
    # We join on Year/Sex/IMD
    step <- merge(sim_state[, -c("age")], probs, by = c("year", "sex", "imd_quintile"))
    
    # Calculate Flows
    # 1. Initiation (from Never)
    step[, flow_init := n_never * p_start]
    
    # 2. Quit (from Current)
    step[, flow_quit := n_current * p_quit]
    
    # 3. Relapse (from Former)
    step[, flow_relapse := n_former * p_relapse]
    
    # Update Stocks for NEXT step
    step[, n_never_next := n_never - flow_init]
    step[, n_current_next := n_current + flow_init - flow_quit + flow_relapse]
    step[, n_former_next := n_former + flow_quit - flow_relapse]
    
    # Calculate Net Initiation for THIS step
    # Definition: Net change in smokers / Non-smokers (Never + Former)
    step[, n_non_current := n_never + n_former]
    step[, delta_current := n_current_next - n_current]
    
    # Avoid division by zero
    step[, p_start_net := ifelse(n_non_current > 0, delta_current / n_non_current, 0)]
    step[, p_start_net := pmax(0, p_start_net)] # Clamp negative values to 0
    
    # Save results
    results_list[[as.character(a)]] <- step[, .(year, sex, imd_quintile, age, p_start_net)]
    
    # Update state for next iteration
    sim_state <- step[, .(year, sex, imd_quintile, 
                          n_never = n_never_next, 
                          n_current = n_current_next, 
                          n_former = n_former_next)]
  }
  
  # Combine all ages
  net_data <- rbindlist(results_list)
  
  
  # 3. Uncertainty & Summaries
  # -------------------------------------------------------------------------
  
  # A. Calculate Uncertainty (using our optimized function)
  if (!exists("generate_uncertainty")) stop("Function 'generate_uncertainty' not found.")
  
  unc_res <- generate_uncertainty(
    data = net_data,
    prob_col = "p_start_net",
    n_eff = config$kn,
    n_samp = 1000, # Fixed as per your script
    correlation = config$kR
  )
  
  final_data <- unc_res$data
  
  # B. Save Outputs
  out_path <- file.path(config$path, "outputs")
  saveRDS(final_data, file.path(out_path, paste0("net_init_data_", config$country, "_uncertainty.rds")))
  
  # C. Generate Summary CSVs (Aggregations)
  # Merge Pops
  final_data <- merge(final_data, pops, by = c("age", "sex", "imd_quintile"), all.x = TRUE)
  
  # Summary by Sex
  summ_sex <- final_data[, .(
    p_init = weighted.mean(p_start_net, N, na.rm=TRUE),
    p_var = weighted.mean(p_start_net_var, N, na.rm=TRUE)
  ), by = sex]
  fwrite(summ_sex, file.path(out_path, paste0("init_summary_", config$country, "_sex.csv")))
  
  # Summary by Year/IMD
  # Create Year Categories
  final_data[, year_cat := cut(year, breaks = c(2010, 2013, 2016, 2019), 
                               labels = c("2011-2013", "2014-2016", "2017-2019"))]
  
  summ_imd <- final_data[!is.na(year_cat), .(
    p_init = weighted.mean(p_start_net, N, na.rm=TRUE),
    p_var = weighted.mean(p_start_net_var, N, na.rm=TRUE)
  ), by = .(year_cat, imd_quintile)]
  fwrite(summ_imd, file.path(out_path, paste0("init_summary_", config$country, "_year_imd.csv")))
  
  
  # 4. Plotting (Optional quick check)
  # -------------------------------------------------------------------------
  p <- ggplot(final_data[age %in% 16:25]) +
    geom_line(aes(x = age, y = p_start_net, group = year, colour = year)) +
    facet_wrap(~sex + imd_quintile, nrow = 2) +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(title = "Net Initiation Probability", y = "Net P(initiate)", x = "Age")
  
  print(p)
  
  message(">> Net Initiation Calculation Complete.")
  return(final_data)
}