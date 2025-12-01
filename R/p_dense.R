
#' Convert probabilities of ever-smoking to age-specific probabilities of smoking initiation
#'
#' Converts the cumulative density function to the probability density function,
#' assuming an interval of 1.
#'
#' @param data Data table containing estimates of the cumulative 
#' probabilities of ever-smoking by age.
#' @param cum_func_var Character - the name of the variable 
#' containing the cumulative probabilities.
#' @param strat_vars Character vector - the variables by which to stratify the calculation.
#' @param lowest_year integer - lowest year of data available 
#' (for England this is 2003 and for Scotland this is 2008). 
#' Default is set to 2003, for HSE.
#' @param max_year integer - the latest year considered in the data + forecast
#' 
#' @importFrom data.table shift := copy setnames rbindlist
#' 
#' @return Returns an updated version of data with a new variable 
#' for the age-specific probabilities of
#' smoking initiation.
#' 
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' init_data <- p_dense(data = copy(init_data_adj), cum_func_var = "p_ever_smoker_adj",
#'                     strat_vars = c("cohort", "sex", "imd_quintile"))
#'
#' }
#'
p_dense <- function(
    data,
    cum_func_var,
    strat_vars = c("cohort", "sex", "imd_quintile"),
    lowest_year = 2003,
    max_year = 2100
) {
  
  # Lead the cdf
  data[ , (paste0(cum_func_var, "_lead1")) := shift(get(cum_func_var), type = "lead"), 
        by = strat_vars]
  
  # Calculate the pdf
  data[ , initiation_pdf := (1 - ((1 - get(paste0(cum_func_var, "_lead1"))) / 
                                    (1 - get(cum_func_var))))]
  
  data[is.na(initiation_pdf), initiation_pdf := 0]
  
  data[initiation_pdf < 0, initiation_pdf := 0]
  data[initiation_pdf > 1, initiation_pdf := 1]
  
  # Tidy
  data[ , (paste0(cum_func_var, "_lead1")) := NULL]
  
  smk_init_data <- copy(data[age >= 10 & age <= 30 & year >= lowest_year & 
                               year <= max_year, 
                             c("sex", "imd_quintile", "age", "year", "initiation_pdf")])
  
  setnames(smk_init_data, "initiation_pdf", "p_start")
  
  # Smooth values
  counter <- 0
  
  for(sx in c("Male", "Female")) {
    for(md in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      #sx <- "Male"
      #md <- "5_most_deprived"
      
      data_temp <- smktrans::p_smooth(
        data = smk_init_data[sex == sx & imd_quintile == md], value_var = "p_start", window_size = 5)
      
      data_temp[ , `:=`(sex = sx, imd_quintile = md)]
      
      
      if(counter == 0) {
        data_sm <- copy(data_temp)
      } else {
        data_sm <- rbindlist(list(data_sm, copy(data_temp)), use.names = T)
      }
      
      counter <- counter + 1
      
    }
  }
  
  smk_init_data <- copy(data_sm)
  
  
  return(smk_init_data[])
}