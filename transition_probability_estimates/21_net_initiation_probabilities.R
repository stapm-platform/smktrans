
# The aim of the code is to calculate the net initiation probabilities

# load data
init_data <- readRDS(paste0(path, "outputs/init_forecast_data_", country, ".rds"))
relapse_data <- readRDS(paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))
quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

# Average 
survey_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds")
survey_data <- survey_data[year >= 2011 & year <= 2019 & smk.state == "former"]
survey_data[time_since_quit > 10, time_since_quit := 10]
survey_data[ , .(qt = sum(time_since_quit * wt_int)/sum(wt_int)), by = c("age")]

# average times since quitting

# age
# 12-17 - 1 year
# 18-24 - 3 years
# 25-30 - 5 years

init_data <- init_data[year >= 2011 & year <= 2019 & age <= 29]
quit_data <- quit_data[year >= 2011 & year <= 2019 & age <= 29]

relapse_data <- relapse_data[year >= 2011 & year <= 2019 & ((age <= 17 & time_since_quit == 2) | 
                                                              (age >= 18 & age <= 24 & time_since_quit == 2) | 
                                                              (age >= 25 & age <= 29 & time_since_quit == 2))]

relapse_data[ , time_since_quit := NULL]

domain <- merge(init_data, quit_data, by = c("year", "age", "sex", "imd_quintile"), all.x = T, all.y = F)
domain <- merge(domain, relapse_data, by = c("year", "age", "sex", "imd_quintile"), all.x = T, all.y = F)

sn <- c("Male", "Female")
imdn <- c("1_least_deprived", "2", "3", "4", "5_most_deprived")
ages <- 12:29
years <- 2011:2019

for(sn_i in sn) {
  
  for(imd_i in imdn) {
    
    for(y_i in years) {
      
      #sn_i <- "Male"
      #imd_i <- "2"
      #y_i <- 2011
      
      tab_i <- domain[sex == sn_i & imd_quintile == imd_i & year == y_i]
      
      radix <- 1000
      
      # p_start	
      p_start_vec <- as.vector(tab_i[age %in% 12:29, p_start])
      
      # p_quit
      p_quit_vec <- as.vector(tab_i[age %in% 12:29, p_quit])
      
      # p_relapse
      p_relapse_vec <- as.vector(tab_i[age %in% 12:29, p_relapse])
      
      # Number of never smokers
      n_never_vec <- c(rep(0, length(12:29)))
      
      # Number of people who initiate
      n_init_vec <- c(rep(0, length(12:29)))
      
      # Number of ever smokers
      n_ever_vec <- c(rep(0, length(12:29)))
      
      # Number of people who quit
      n_quit_vec <- c(rep(0, length(12:29)))
      
      # Number of people who relapse
      n_relapse_vec <- c(rep(0, length(12:29)))
      
      # Number of former smokers
      n_former_vec <- c(rep(0, length(12:29)))
      
      # Number of current smokers
      n_current_vec <- c(rep(0, length(12:29)))
      
      # Net change to the number of current smokers
      delta_current_vec <- c(rep(0, length(12:29)))
      
      # Number of non-current smokers	
      n_non_current_vec <- c(rep(0, length(12:29)))
      
      # p_start_net
      p_start_net_vec <- c(rep(0, length(12:29)))
      
      ## Calculations
      
      for(a_i in ages) {
        
        # Number of people who initiate
        if(a_i == 12) {
          n_init_vec[1] <- 1000 * p_start_vec[1]
        } else {
          n_init_vec[a_i - 12 + 1] <- n_never_vec[a_i - 12] * p_start_vec[a_i - 12 + 1]
        }
        
        # Number of never smokers
        if(a_i == 12) {
          n_never_vec[1] <- 1000 - n_init_vec[1]
        } else {
          n_never_vec[a_i - 12 + 1] <- n_never_vec[a_i - 12] - n_init_vec[a_i - 12 + 1]
        }
        
        # Number of people who quit
        if(a_i == 12) {
          n_quit_vec[1] <- 0
        } else {
          n_quit_vec[a_i - 12 + 1] <- n_current_vec[a_i - 12] * p_quit_vec[a_i - 12 + 1]
        }
        
        # Number of people who relapse
        if(a_i == 12) {
          n_relapse_vec[1] <- 0
        } else {
          n_relapse_vec[a_i - 12 + 1] <- n_former_vec[a_i - 12] * p_relapse_vec[a_i - 12 + 1]
        }
        
        # Number of former smokers
        if(a_i == 12) {
          n_former_vec[1] <- 0 + n_quit_vec[1] - n_relapse_vec[1]
        } else {
          n_former_vec[a_i - 12 + 1] <- n_former_vec[a_i - 12] + n_quit_vec[a_i - 12 + 1] - n_relapse_vec[a_i - 12 + 1]
        }
        
        # Number of current smokers
        if(a_i == 12) {
          n_current_vec[1] <- n_init_vec[1]
        } else {
          n_current_vec[a_i - 12 + 1] <- n_current_vec[a_i - 12] + n_init_vec[a_i - 12 + 1] - n_quit_vec[a_i - 12 + 1] + n_relapse_vec[a_i - 12 + 1]
        }
        
        # Number of ever smokers
        n_ever_vec[a_i - 12 + 1] <- 1000 - n_never_vec[a_i - 12 + 1]
        
        # Net change to the number of current smokers
        if(a_i == 12) {
          delta_current_vec[1] <- n_current_vec[1] - 0
        } else {
          delta_current_vec[a_i - 12 + 1] <- n_current_vec[a_i - 12 + 1] - n_current_vec[a_i - 12]
        }
        
        # Number of non-current smokers	
        n_non_current_vec[a_i - 12 + 1] <- 1000 - n_current_vec[a_i - 12 + 1]
        
        # p_start_net
        if(a_i == 12) {
          p_start_net_vec[1] <- NA
        } else {
          p_start_net_vec[a_i - 12 + 1] <- delta_current_vec[a_i - 12 + 1] / n_non_current_vec[a_i - 12]
        }
        
      }
      
      tab_i[ , p_start_net := p_start_net_vec]
      
      if(sn_i == sn[1] & imd_i == imdn[1] & y_i == years[1]) {
        tab_out <- copy(tab_i)
      } else {
        tab_out <- rbindlist(list(tab_out, copy(tab_i)), use.names = T)
      }
      
    }
  }
}

tab_out[p_start_net < 0, p_start_net := 0]


ggplot(tab_out[age >= 16 & age <= 25]) +
  geom_line(aes(x = age, y = p_start_net, group = year, colour = year)) +
  facet_wrap(~sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("Net initiation probability") + xlab("Age")


# uncertainty

set.seed(2023)

# Load data
net_init_data <- tab_out[age >= 13]

net_init_data[p_start_net == 0, p_start_net := 1/1000]

# Generate samples
init_mat <- genUncertainty(data = net_init_data, var_name = "p_start_net", n = kn, n_samp = 1000, R = kR)

saveRDS(init_mat, paste0(path, "outputs/net_init_mat_", country, "_uncertainty.rds"))


# Add the key sample quantiles to the data
net_init_data[ , p_init_low := apply(init_mat, 1, quantile, 0.025)]
net_init_data[ , p_init_median := apply(init_mat, 1, quantile, 0.5)]
net_init_data[ , p_init_high := apply(init_mat, 1, quantile, 0.975)]
net_init_data[ , p_init_var := apply(init_mat, 1, var_func)]

#stapmr::WriteToExcel(wb, sheet = "Quit",
#                     title = "Probabilities of quitting smoking (current to former smoker).",
#                     quit_data, startCol = 1, startRow = 1)

saveRDS(net_init_data, paste0(path, "outputs/net_init_data_", country, "_uncertainty.rds"))

#rm(quit_mat, quit_data)#
#gc()

#quit_data <- quit_data[sex == "Male" & age >= 50 & age < 90 & year == 2024]
net_init_data_c <- net_init_data[age >= 16 & age <= 24 & year >= 2011 & year <= 2019]

net_init_data_c[ , year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(2011, 2014, 2017))]]

net_init_data_c <- merge(net_init_data_c, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

init_summary <- net_init_data_c[ , .(p_init = sum(p_start_net * N) / sum(N), 
                               p_var = sum(var * N) / sum(N)
), by = c("sex")]

write.csv(init_summary, paste0(path, "outputs/init_summary_", country, "_sex.csv"))


init_summary <- net_init_data_c[ , .(p_init = sum(p_start_net * N) / sum(N), 
                                     p_var = sum(var * N) / sum(N)
), by = c("year_cat", "imd_quintile")]

write.csv(init_summary, paste0(path, "outputs/init_summary_", country, "_year_imd.csv"))


# quit

quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

quit_data <- quit_data[year >= 2011 & year <= 2019 & age >= 25 & age <= 74]


ggplot(quit_data) +
  geom_line(aes(x = age, y = p_quit, group = year, colour = year)) +
  facet_wrap(~sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("quit probability") + xlab("Age")


# uncertainty

set.seed(2023)

# Load data
quit_data[p_quit == 0, p_quit := 1/1000]

# Generate samples
quit_mat <- genUncertainty(data = quit_data, var_name = "p_quit", n = kn, n_samp = 1000, R = kR)

saveRDS(quit_mat, paste0(path, "outputs/quit_mat_", country, "_uncertainty.rds"))


# Add the key sample quantiles to the data
quit_data[ , p_quit_low := apply(quit_mat, 1, quantile, 0.025)]
quit_data[ , p_quit_median := apply(quit_mat, 1, quantile, 0.5)]
quit_data[ , p_quit_high := apply(quit_mat, 1, quantile, 0.975)]
quit_data[ , p_quit_var := apply(quit_mat, 1, var_func)]

#stapmr::WriteToExcel(wb, sheet = "Quit",
#                     title = "Probabilities of quitting smoking (current to former smoker).",
#                     quit_data, startCol = 1, startRow = 1)

saveRDS(quit_data, paste0(path, "outputs/quit_data_", country, "_uncertainty.rds"))

#rm(quit_mat, quit_data)#
#gc()

quit_data[ , age_cat := c("25-49", "50-74")[findInterval(age, c(0, 50, 100))]]
quit_data[ , year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(2011, 2014, 2017))]]

quit_data <- merge(quit_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

quit_summary <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N), 
                                     p_var = sum(var * N) / sum(N)
), by = c("sex", "age_cat")]

write.csv(quit_summary, paste0(path, "outputs/quit_summary_", country, "_sex_agecat.csv"))


quit_summary <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N), 
                               p_var = sum(var * N) / sum(N)
), by = c("year_cat", "imd_quintile")]

write.csv(quit_summary, paste0(path, "outputs/quit_summary_", country, "_year_imd.csv"))
