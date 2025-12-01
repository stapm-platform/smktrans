#' Prepare long-term relapse probabilities
#'
#' Combines published estimates of long-term relapse with the population survey data
#' to arrive at the expected values for relapse probabilities within defined subgroups.
#'
#' This function takes the estimates of relapse to smoking from 
#' \insertCite{hawkins2010long;textual}{smktrans}
#' and processes them into probabilities of relapse to smoking by the significant
#' variables from the above paper (age, time since quit, degree or not, mental health condition or not,
#' married or not). Note that physical health / gp visits was also significant but not included here
#' partly because the surveys do not have the right variables to do so.
#' Once we have mapped relapse probabilities onto the survey data by the above variables,
#'  we can then calculate the variation in expected probability of relapse by age, sex, time since quit and IMD quintile.
#'
#' @param data Data table containing individual characteristics from the population survey data.
#' @param hawkins_relapse Data table containing a tidied version of the estimates of
#' long-term smoking relapse probability
#'  from \insertCite{hawkins2010long;textual}{smktrans}.
#' @param lowest_year integer - lowest year of data available (for England this is 2003
#' and for Scotland this is 2008). Default is 2003.
#' @param highest_year integer - highest year of data available.
#' @param youngest_age integer - youngest age in data (for England 11, for
#' Scotland 16).
#' 
#' @importFrom data.table copy := rbindlist setDT
#' @importFrom Rdpack reprompt
#' 
#' @return Returns two data tables: First, with relapse probabilities stratified by year,
#' age, IMD quintile and time since quit
#' (for use in the STAPM simulation);
#'  Second, with relapse probabilities stratified by just year, age and IMD
#'  quintile (for use in transition prob estimation).
#'  
#' @references
#' \insertRef{hawkins2010long}{smktrans}
#'  
#' @export
#' @examples
#' \dontrun{
#' test_data <- prep_relapse(
#'   data = hse_data,
#'   hawkins_relapse = smktrans::hawkins_relapse
#' )
#' }
prep_relapse <- function(
    data,
    hawkins_relapse = smktrans::hawkins_relapse,
    lowest_year = 2003,
    highest_year = 2018,
    youngest_age = 18
) {
  
  data <- copy(data)
  hawkins_relapse <- copy(hawkins_relapse)
  
  # Merge data with adjusted odds of relapse
  
  # Filter data and select required variables
  data_f <- data[!is.na(age) &
                   age >= youngest_age &
                   smk.state == "former" &
                   !is.na(time_since_quit) & time_since_quit > 0 &
                   !is.na(degree) &
                   !is.na(relationship_status) &
                   !is.na(hse_mental) &
                   !is.na(income5cat) &
                   !is.na(employ2cat) &
                   !is.na(imd_quintile),
                 c("wt_int",
                   "year",
                   "age",
                   "sex",
                   "income5cat",
                   "employ2cat",
                   "imd_quintile",
                   "time_since_quit",
                   "degree",
                   "relationship_status",
                   "hse_mental")]
  
  # Cap time since quit at 10 years, to fit with relapse estimates
  #data_f[time_since_quit > 10, time_since_quit := 10]
  
  # Make weights sum to 1 within a year
  data_f[ , wt_int := wt_int / sum(wt_int), by = "year"]
  
  ########################################
  # Relapse probability by age, imd_quintile and time since quit - for model input
  
  # Map imd onto the other variables
  
  # Use weights to estimate the distribution of the other variables
  # within age and IMD quintile subgroups
  imd_map <- data_f[, list(mu = sum(wt_int)), by = c(
    "year",
    "age",
    "sex",
    "income5cat",
    "employ2cat",
    "imd_quintile",
    #"time_since_quit",
    "degree",
    "relationship_status",
    "hse_mental"
  )]
  
  imd_map[ , p := mu / sum(mu), by = c("year", "age", "sex", "imd_quintile")]
  imd_map[ , mu := NULL]
  
  temp <- copy(imd_map)
  for(i in 0:10) {
    if(i == 0) {
      imd_map <- copy(temp[ , time_since_quit := 0])
    } else {
      imd_map <- rbindlist(list(imd_map, copy(temp[ , time_since_quit := i])), use.names = T)
    }
  }
  
  # Merge the Hawkins relapse prob estimates with the estimates
  # of the distributions of traits by age and IMD quintile
  temp <- merge(imd_map, hawkins_relapse, by = c(
    "age",
    "sex",
    "income5cat",
    "employ2cat",
    "imd_quintile",
    "time_since_quit",
    "degree",
    "relationship_status",
    "hse_mental"
  ), all.x = T, all.y = F)
  
  # ggplot() +
  #   geom_point(data = temp[year == 2017], aes(x = age, y = p_relapse, group = time_since_quit)) +
  #   theme_minimal() +
  #   ylab("P(relapse)") +
  #   facet_wrap(~ sex + imd_quintile, nrow = 2)
  
  temp[is.na(p), p := 0]
  
  temp[ , test := sum(p), by = c("year", "age", "sex", "imd_quintile", "time_since_quit")]
  #temp[test == 0]
  temp[test == 0, p := 1/ 160]
  temp[ , test := NULL]
  
  # Summarise relapse probabilities by year, age, IMD quintiles and time since quit
  relapse_by_age_imd_timesincequit <- temp[ , list(p_relapse = sum(p_relapse * p) / sum(p)),
                                            by = c("year", "age", "sex", "time_since_quit", "imd_quintile")]
  
  # ggplot() +
  #   geom_line(data = relapse_by_age_imd_timesincequit[year == 2017], aes(x = age, y = p_relapse, group = time_since_quit)) +
  #   theme_minimal() +
  #   ylab("P(relapse)") +
  #   facet_wrap(~ sex + imd_quintile, nrow = 2)
  
  
  # Enforce the boundaries on relapse prob between 0 and 1
  relapse_by_age_imd_timesincequit[p_relapse < 0, p_relapse := 0]
  relapse_by_age_imd_timesincequit[p_relapse > 1, p_relapse := 1]
  
  # Create a standardised data table will combinations of all variables
  domain <- data.frame(expand.grid(
    year = lowest_year:highest_year,
    age = youngest_age:89,
    time_since_quit = 0:10,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  setDT(domain)
  
  relapse_by_age_imd_timesincequit <- merge(domain, relapse_by_age_imd_timesincequit,
                                            all = T, sort = F,
                                            by = c("year", "age", "time_since_quit", "sex", "imd_quintile"))
  
  #relapse_by_age_imd_timesincequit[time_since_quit == 1 & sex == "Male" & imd_quintile == "5_most_deprived" & year == 2001]
  
  # ggplot() +
  #   geom_line(data = relapse_by_age_imd_timesincequit[year == 2017], aes(x = age, y = p_relapse, group = time_since_quit)) +
  #   theme_minimal() +
  #   ylab("P(relapse)") +
  #   facet_wrap(~ sex + imd_quintile, nrow = 2)
  
  
  # Smooth values
  counter <- 0
  
  for(tq in 0:10) {
    for(sx in c("Male", "Female")) {
      for(md in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
        
        #tq <- 1
        #sx <- "Male"
        #md <- "3"
        if(tq < 10) {
          
          data_temp <- smktrans::p_smooth(
            relapse_by_age_imd_timesincequit[time_since_quit == tq & sex == sx & imd_quintile == md],
            "p_relapse", 5)
          
          data_temp[ , `:=`(time_since_quit = tq, sex = sx, imd_quintile = md)]
        }
        
        if(tq == 10) {
          data_temp <- relapse_by_age_imd_timesincequit[time_since_quit == tq & sex == sx & imd_quintile == md]
          data_temp[ , p_relapse := 0]
        }
        
        if(counter == 0) {
          data_sm <- copy(data_temp)
        } else {
          data_sm <- rbindlist(list(data_sm, copy(data_temp)), use.names = T)
        }
        
        counter <- counter + 1
        
      }
    }
  }
  
  relapse_by_age_imd_timesincequit <- copy(data_sm)
  
  
  ###########
  
  # Add future values
  # rather than forecast, keep simple and assume that all future years
  # have the value from the last year
  temp <- relapse_by_age_imd_timesincequit[year == highest_year]
  #temp <- temp[ , list(p_relapse = mean(p_relapse, na.rm = T)), by = c("age", "time_since_quit", "sex", "imd_quintile")]
  
  next_year <- highest_year + 1
  
  for(i in next_year:2100) {
    
    relapse_by_age_imd_timesincequit <- rbindlist(list(
      relapse_by_age_imd_timesincequit,
      copy(temp)[ , year := i]), use.names = T)
    
  }
  
  ###########
  
  
  ########################################
  # Relapse probability by age and imd_quintile - for trans prob estimation
  
  # Merge the data with the Hawkins relapse prob estimates
  data_f <- merge(data_f, hawkins_relapse, by = c(
    "age",
    "sex",
    "income5cat",
    "employ2cat",
    "imd_quintile",
    "time_since_quit",
    "degree",
    "relationship_status",
    "hse_mental"
  ), all.x = T, all.y = F)
  
  # Calculate the average relapse probability within age and IMD quintile subgroups
  relapse_by_age_imd <- data_f[, list(
    
    p_relapse = sum(p_relapse * wt_int, na.rm = T) / sum(wt_int)
    
  ), by = c("year", "age", "sex", "imd_quintile")]
  
  # Enforce the boundaries on relapse prob between 0 and 1
  relapse_by_age_imd[p_relapse < 0, p_relapse := 0]
  relapse_by_age_imd[p_relapse > 1, p_relapse := 1]
  
  # Fill-in any missing age, sex, IMD quintile combinations with the average age and sex value
  domain <- data.frame(expand.grid(
    year = lowest_year:highest_year,
    age = youngest_age:89,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  setDT(domain)
  
  relapse_by_age_imd <- merge(domain, relapse_by_age_imd,
                              all = T, sort = F,
                              by = c("year", "age", "sex", "imd_quintile"))
  
  # Smooth values
  counter <- 0
  
  for(sx in c("Male", "Female")) {
    for(md in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      #sx <- "Male"
      #md <- "5_most_deprived"
      
      data_temp <- smktrans::p_smooth(relapse_by_age_imd[sex == sx & imd_quintile == md], "p_relapse", 5)
      data_temp[ , `:=`(sex = sx, imd_quintile = md)]
      
      
      if(counter == 0) {
        data_sm <- copy(data_temp)
      } else {
        data_sm <- rbindlist(list(data_sm, copy(data_temp)), use.names = T)
      }
      
      counter <- counter + 1
      
    }
  }
  
  relapse_by_age_imd <- copy(data_sm)
  
  
  return(list(
    relapse_by_age_imd_timesincequit = relapse_by_age_imd_timesincequit[],
    relapse_by_age_imd = relapse_by_age_imd[]
  ))
}