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
  years <- 2011:2019
  
  # Applying the filter:
  r_dt <- relapse_data[
    (age <= 17 & time_since_quit == 1) | 
      (age >= 18 & age <= 24 & time_since_quit == 3) | 
      (age >= 25 & age <= 29 & time_since_quit == 5)
  ]
  
  if(nrow(r_dt) == 0) {
    warning("Using fallback TSQ=2 assumption")
    r_dt <- relapse_data[time_since_quit == 2]
  }
  
  # Select columns and merge
  cols <- c("year", "age", "sex", "imd_quintile")
  
  # Merge all inputs into one 'domain' table
  dt <- merge(init_data[age %in% ages & year %in% years, ..cols], 
              init_data[, c(..cols, "p_start")], by = cols)
  dt <- merge(dt, quit_data[, c(..cols, "p_quit")], by = cols, all.x = TRUE)
  dt <- merge(dt, r_dt[, c(..cols, "p_relapse")], by = cols, all.x = TRUE)
  
  # Fill NAs
  dt[is.na(p_quit), p_quit := 0]
  dt[is.na(p_relapse), p_relapse := 0]
  
  # Sort strictly by Year > Sex > IMD > Age
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
    n_former = 0
  )]
  
  results_list <- list()
  
  for (a in ages) {
    # Get probabilities for this age 'a'
    probs <- dt[age == a]
    
    # Merge current state with probabilities
    # We join on Year/Sex/IMD
    step <- merge(sim_state, probs, by = c("year", "sex", "imd_quintile"))
    
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
  
  # Save Outputs
  out_path <- file.path(config$path, "outputs")
  saveRDS(net_data, file.path(out_path, paste0("net_init_data_", config$country, ".rds")))
  
  return(invisible(net_data))
}