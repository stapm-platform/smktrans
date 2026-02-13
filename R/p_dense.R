#' Convert probabilities of ever-smoking to age-specific probabilities
#'
#' @description
#' Converts the Cumulative Distribution Function (CDF) of ever-smoking 
#' into the Probability Density Function (PDF), which represents the 
#' probability of initiating smoking at a specific age.
#'
#' @param data Data table with cumulative probabilities.
#' @param cum_func_var Character - name of cumulative variable.
#' @param strat_vars Character vector - stratification variables.
#' @param lowest_year integer - start year filter.
#' @param max_year integer - end year filter.
#' @importFrom data.table shift := copy setnames rbindlist
#' @export
p_dense <- function(
    data,
    cum_func_var,
    strat_vars = c("cohort", "sex", "imd_quintile"),
    lowest_year = 2003,
    max_year = 2100
) {
  
  dt <- copy(data)
  
  # 1. Calculate PDF from CDF
  # p_init = 1 - (Survival_t+1 / Survival_t)
  # Where Survival = 1 - Cumulative_Ever_Smoker
  
  # Create Lead variable
  dt[, (paste0(cum_func_var, "_lead1")) := shift(get(cum_func_var), type = "lead"), by = strat_vars]
  
  # Calculate conditional probability
  dt[, initiation_pdf := (1 - ((1 - get(paste0(cum_func_var, "_lead1"))) / 
                                 (1 - get(cum_func_var))))]
  
  # 2. Safety Clamping
  # Numerical noise can cause <0 or >1
  dt[is.na(initiation_pdf), initiation_pdf := 0]
  dt[initiation_pdf < 0, initiation_pdf := 0]
  dt[initiation_pdf > 1, initiation_pdf := 1]
  
  # Cleanup
  dt[, (paste0(cum_func_var, "_lead1")) := NULL]
  
  # Filter relevant ages/years
  smk_init_data <- dt[age >= 10 & age <= 30 & year >= lowest_year & year <= max_year, 
                      .(sex, imd_quintile, age, year, initiation_pdf)]
  
  setnames(smk_init_data, "initiation_pdf", "p_start")
  
  # 3. Smoothing
  # We apply smoothing within subgroups to reduce jaggedness
  
  # Define subgroups
  subgroups <- expand.grid(
    sex = c("Male", "Female"),
    imd = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
    stringsAsFactors = FALSE
  )
  
  smoothed_list <- list()
  
  for(i in 1:nrow(subgroups)) {
    sx <- subgroups$sex[i]
    md <- subgroups$imd[i]
    
    subset_data <- smk_init_data[sex == sx & imd_quintile == md]
    
    # DEBUG: Check if data actually exists here
    if(nrow(subset_data) == 0) {
      message(paste("Skipping: No data found for", sx, md))
      next
    }
    
    # Check if all values are NA
    if(all(is.na(subset_data$p_start))) {
      message(paste("Skipping: All values are NA for", sx, md))
      next
    }
    
    # Apply smoothing function
    smoothed <- p_smooth(
      data = subset_data, 
      value_var = "p_start", 
      window_size = 5
    )
    smoothed[, `:=`(sex = sx, imd_quintile = md)]
    smoothed_list[[i]] <- smoothed
    
  }
  
  final_data <- rbindlist(smoothed_list, use.names = TRUE)
  
  return(final_data[])
}