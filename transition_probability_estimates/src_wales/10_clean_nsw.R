
# The aim of this code is to process the National Survey for Wales data
# into the form required to estimate smoking transition probabilities

# Use the age range 16 - 89
# and years 2016 - 2022

# Load the required packages
library(hseclean)
library(magrittr)
library(data.table)
library(ggplot2)
library(survey)
library(RColorBrewer)
library(viridis)

# Apply functions to create the variables for analysis and to retain only the required variables

# Note that NSW does not collect single years since quitting smoking

# SmQuitTm	Smoking - How long ago did you stop smoking	-99	Refused
# -98	Interview terminated early
# -88	Not selected in sub-sample
# -9	Don't Know (SPONTANEOUS ONLY)
# 		-8	Question not asked due to routing
# 		1	Less than 1 month ago
# 		2	1 month to 1 year ago
# 		3	More than 1 year ago





nsw2016 <- read_NSW_2016_17(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2017/UKDA-8301-tab/tab/national_survey_for_wales_2016-17_respondent_data_anonymised_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2016[ , year := 2016]

# take time since quit from HSE
hse2016 <- read_2016() %>%
  clean_age %>%
  clean_demographic %>%
  clean_education %>%
  clean_economic_status %>%
  clean_family %>%
  clean_income %>%
  clean_health_and_bio %>%
  smk_status %>%
  smk_former

#table(hse2016$years_since_quit)

quittime_lkup <- hse2016[!is.na(years_since_quit), .(n = sum(wt_int, na.rm = T)), by = c("years_since_quit", "sex", "imd_quintile", "age_cat")]
quittime_lkup[ , p := n / sum(n), by = c("sex", "imd_quintile", "age_cat")]
quittime_lkup[ , n := NULL]
quittime_lkup[ , years_since_quit := as.integer(years_since_quit)]

nsw2016 <- nsw2016[!is.na(sex)]
nsw2016 <- nsw2016[!is.na(age)]
nsw2016 <- nsw2016[!is.na(imd_quintile)]
nsw2016 <- nsw2016[!is.na(cig_smoker_status)]

nsw2016[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2016)) {
#cat(i, "\n")
  
  if(nsw2016[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2016[i, sex] & 
                                 imd_quintile == nsw2016[i, imd_quintile] & 
                                 age_cat == nsw2016[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {

      data_temp <- quittime_lkup[imd_quintile == nsw2016[i, imd_quintile] & 
                                   age_cat == nsw2016[i, age_cat]]
            
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2016[i, age_cat]]
      
    }
    
    nsw2016[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}

#nsw2016[cig_smoker_status == "former", years_since_quit]


nsw2017 <- read_NSW_2017_18(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2018/UKDA-8390-tab/tab/nsw_2017-18_respondent_file_final_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2017[ , year := 2017]

# take time since quit from HSE
hse2017 <- read_2017() %>%
  clean_age %>%
  clean_demographic %>%
  clean_education %>%
  clean_economic_status %>%
  clean_family %>%
  clean_income %>%
  clean_health_and_bio %>%
  smk_status %>%
  smk_former

#table(hse2017$years_since_quit)

quittime_lkup <- hse2017[!is.na(years_since_quit), .(n = sum(wt_int, na.rm = T)), by = c("years_since_quit", "sex", "imd_quintile", "age_cat")]
quittime_lkup[ , p := n / sum(n), by = c("sex", "imd_quintile", "age_cat")]
quittime_lkup[ , n := NULL]
quittime_lkup[ , years_since_quit := as.integer(years_since_quit)]

nsw2017 <- nsw2017[!is.na(sex)]
nsw2017 <- nsw2017[!is.na(age)]
nsw2017 <- nsw2017[!is.na(imd_quintile)]
nsw2017 <- nsw2017[!is.na(cig_smoker_status)]

nsw2017[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2017)) {
  #cat(i, "\n")
  
  if(nsw2017[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2017[i, sex] & 
                                 imd_quintile == nsw2017[i, imd_quintile] & 
                                 age_cat == nsw2017[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[imd_quintile == nsw2017[i, imd_quintile] & 
                                   age_cat == nsw2017[i, age_cat]]
      
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2017[i, age_cat]]
      
    }
    
    nsw2017[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}


nsw2018 <- read_NSW_2018_19(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2019/UKDA-8591-tab/tab/national_survey_for_wales_2018-19_respondent_file_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2018[ , year := 2018]

# take time since quit from HSE
hse2018 <- read_2018() %>%
  clean_age %>%
  clean_demographic %>%
  clean_education %>%
  clean_economic_status %>%
  clean_family %>%
  clean_income %>%
  clean_health_and_bio %>%
  smk_status %>%
  smk_former

#table(hse2018$years_since_quit)

quittime_lkup <- hse2018[!is.na(years_since_quit), .(n = sum(wt_int, na.rm = T)), by = c("years_since_quit", "sex", "imd_quintile", "age_cat")]
quittime_lkup[ , p := n / sum(n), by = c("sex", "imd_quintile", "age_cat")]
quittime_lkup[ , n := NULL]
quittime_lkup[ , years_since_quit := as.integer(years_since_quit)]

nsw2018 <- nsw2018[!is.na(sex)]
nsw2018 <- nsw2018[!is.na(age)]
nsw2018 <- nsw2018[!is.na(imd_quintile)]
nsw2018 <- nsw2018[!is.na(cig_smoker_status)]

nsw2018[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2018)) {
  #cat(i, "\n")

  if(nsw2018[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2018[i, sex] & 
                                 imd_quintile == nsw2018[i, imd_quintile] & 
                                 age_cat == nsw2018[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[imd_quintile == nsw2018[i, imd_quintile] & 
                                   age_cat == nsw2018[i, age_cat]]
      
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2018[i, age_cat]]
      
    }
    
    nsw2018[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}

nsw2019 <- read_NSW_2019_20(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2020/UKDA-8718-tab/tab/national_survey_for_wales_respondent_file_2019-20_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2019[ , year := 2019]


# take time since quit from HSE
hse2019 <- read_2019() %>%
  clean_age %>%
  clean_demographic %>%
  clean_education %>%
  clean_economic_status %>%
  clean_family %>%
  clean_income %>%
  clean_health_and_bio %>%
  smk_status %>%
  smk_former

#table(hse2019$years_since_quit)

quittime_lkup <- hse2019[!is.na(years_since_quit), .(n = sum(wt_int, na.rm = T)), by = c("years_since_quit", "sex", "imd_quintile", "age_cat")]
quittime_lkup[ , p := n / sum(n), by = c("sex", "imd_quintile", "age_cat")]
quittime_lkup[ , n := NULL]
quittime_lkup[ , years_since_quit := as.integer(years_since_quit)]

nsw2019 <- nsw2019[!is.na(sex)]
nsw2019 <- nsw2019[!is.na(age)]
nsw2019 <- nsw2019[!is.na(imd_quintile)]
nsw2019 <- nsw2019[!is.na(cig_smoker_status)]

nsw2019[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2019)) {
  #cat(i, "\n")
  
  if(nsw2019[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2019[i, sex] & 
                                 imd_quintile == nsw2019[i, imd_quintile] & 
                                 age_cat == nsw2019[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[imd_quintile == nsw2019[i, imd_quintile] & 
                                   age_cat == nsw2019[i, age_cat]]
      
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2019[i, age_cat]]
      
    }
    
    nsw2019[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}

nsw2020 <- read_NSW_2020_21(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2021/UKDA-8870-tab/tab/national_survey_for_wales_2020-21_respondent_file_to_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2020[ , year := 2020]

nsw2020 <- nsw2020[!is.na(sex)]
nsw2020 <- nsw2020[!is.na(age)]
nsw2020 <- nsw2020[!is.na(imd_quintile)]
nsw2020 <- nsw2020[!is.na(cig_smoker_status)]

nsw2020[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2020)) {
  #cat(i, "\n")
  
  if(nsw2020[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2020[i, sex] & 
                                 imd_quintile == nsw2020[i, imd_quintile] & 
                                 age_cat == nsw2020[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[imd_quintile == nsw2020[i, imd_quintile] & 
                                   age_cat == nsw2020[i, age_cat]]
      
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2020[i, age_cat]]
      
    }
    
    nsw2020[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}




nsw2021 <- read_NSW_2021_22(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2022/UKDA-9011-tab/tab/nsw_2021-22_respondent_file_for_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2021[ , year := 2021]

nsw2021 <- nsw2021[!is.na(sex)]
nsw2021 <- nsw2021[!is.na(age)]
nsw2021 <- nsw2021[!is.na(imd_quintile)]
nsw2021 <- nsw2021[!is.na(cig_smoker_status)]

nsw2021[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2021)) {
  #cat(i, "\n")
  
  if(nsw2021[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2021[i, sex] & 
                                 imd_quintile == nsw2021[i, imd_quintile] & 
                                 age_cat == nsw2021[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[imd_quintile == nsw2021[i, imd_quintile] & 
                                   age_cat == nsw2021[i, age_cat]]
      
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2021[i, age_cat]]
      
    }
    
    nsw2021[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}





nsw2022 <- read_NSW_2022_23(root = "C:/Users/cm1dog/Documents/National Survey for Wales (NSW)/", 
                            file = "NSW 2023/UKDA-9144-tab/tab/nsw_2022-23_respondent_file_for_ukds.tab") %>%
  clean_age %>%
  clean_demographic %>%
  smk_status %>%
  clean_education %>%
  clean_family %>%
  clean_economic_status %>%
  clean_income %>%
  clean_health_and_bio

nsw2022[ , year := 2022]

nsw2022 <- nsw2022[!is.na(sex)]
nsw2022 <- nsw2022[!is.na(age)]
nsw2022 <- nsw2022[!is.na(imd_quintile)]
nsw2022 <- nsw2022[!is.na(cig_smoker_status)]

nsw2022[ , years_since_quit := NA_integer_]

for(i in 1:nrow(nsw2022)) {
  #cat(i, "\n")
  
  if(nsw2022[i, cig_smoker_status] == "former") {
    
    data_temp <- quittime_lkup[sex == nsw2022[i, sex] & 
                                 imd_quintile == nsw2022[i, imd_quintile] & 
                                 age_cat == nsw2022[i, age_cat]]
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[imd_quintile == nsw2022[i, imd_quintile] & 
                                   age_cat == nsw2022[i, age_cat]]
      
    }
    
    if(nrow(data_temp) <= 1) {
      
      data_temp <- quittime_lkup[age_cat == nsw2022[i, age_cat]]
      
    }
    
    nsw2022[i, years_since_quit := sample(x = data_temp$years_since_quit, size = 1, prob = data_temp$p)]
  }
}


data_nsw <- rbindlist(list(nsw2016, nsw2017, nsw2018, nsw2019, nsw2020, nsw2021, nsw2022), use.names = T, fill = T)

data_nsw <- data_nsw[!is.na(sex) & !is.na(age) & !is.na(imd_quintile)]

setnames(data_nsw, c("smage", "cig_smoker_status", "sampleadultweight", "years_since_quit"), c("start_age", "smk.state", "wt_int", "time_since_quit"))
data_nsw[ , start_age := as.double(start_age)]
data_nsw[start_age < 0 | start_age >= 97, start_age := NA_real_]

data_nsw[ , censor_age := as.double(age + 1)]

#table(data_nsw$age)

# remake age categories
data_nsw[, age_cat := c("16-24",
                        "25-34",
                        "35-44",
                        "45-54",
                        "55-64",
                        "65-74",
                        "75-89")[findInterval(age, c(16, 23, 35, 45, 55, 65, 75, 1000))]]


######################################
############# Initiation #############
######################################

# Calculate the cumulative probabilities of starting to smoke for each cohort
init_data_raw <- smktrans::init_est(
  data = data_nsw,
  strat_vars = c("sex", "imd_quintile"))

# select only ages from 11 to 30
# and calculate the average across birth cohorts

init_data_raw_av <- init_data_raw[age %in% 10:30, .(p_ever_smoker = mean(p_ever_smoker, na.rm = T)), by = c("age", "sex", "imd_quintile")]

ggplot(init_data_raw_av) +
  geom_line(aes(x = age, y = p_ever_smoker, colour = imd_quintile)) +
  facet_wrap(~sex) +
  xlim(10, 30)

#data_nsw[age_cat %in% "25-34"]

# table(data_nsw[is.na(wt_int), year])
# 2017 2018 2019 2021 2022 
# 5615 5863 6211 6363 6024 
# weight variable needs fixing
data_nsw[is.na(wt_int), wt_int := .1]

# Estimate the trend in the proportion of people who have ever smoked
# in the age range 25-34
ever_smoke_data <- smktrans::ever_smoke(
  data = data_nsw,
  time_horizon = 2022 + 100,
  num_bins = 9,
  model = "model5", # model 2 or 5
  min_age = 10,
  min_year = 2016,
  age_cats = c("25-34"))

ggplot() +
  geom_line(data = ever_smoke_data$predicted_values, aes(x = year, y = fitted_trends, colour = imd_quintile)) +
  facet_wrap(~sex) +
  xlim(2016, 2040) +
  geom_point(data = ever_smoke_data$data_points, aes(x = year, y = ever_smoker, colour = imd_quintile))

# duplicate average data for all the cohorts
for(ci in 1970:2100) {
  temp <- copy(init_data_raw_av)[ , cohort := ci]
  if(ci == 1970) {
    init_data_raw <- temp
  } else {
    init_data_raw <- rbindlist(list(init_data_raw, temp))
  }
}

# Adjust and forecast data for later cohorts

init_data_adj <- smktrans::init_adj(
  init_data = copy(init_data_raw),
  ever_smoke_data = copy(ever_smoke_data$predicted_values),
  ref_age = 28,
  fix_ref_age = T,
  min_ref = 23,
  cohorts = 1970:2100,
  period_start = 2016, period_end = 2040)

# predictions available for birth cohorts 1973 - 1994
ggplot(init_data_adj[cohort == 1982]) +
  geom_line(aes(x = age, y = p_ever_smoker_adj, colour = imd_quintile)) +
  facet_wrap(~sex) +
  xlim(10, 30)

# Convert from cumulative probs to the prob of initiation
smk_init_data <- smktrans::p_dense(
  data = copy(init_data_adj),
  cum_func_var = "p_ever_smoker_adj",
  strat_vars = c("cohort", "sex", "imd_quintile"),
  lowest_year = 1998, max_year = 2040)

ggplot(smk_init_data[year == 2040]) +
  geom_line(aes(x = age, y = p_start, colour = imd_quintile)) +
  facet_wrap(~sex) +
  xlim(10, 30)


# Initiation --------

init_data <- copy(smk_init_data)

#init_data <- merge(init_data, stapmr::pop_counts[year == 2019][ ,year := NULL], all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
init_data <- init_data[age >= 10 & age < 30 & year >= 2016 & year <= 2040]

#init_data_plot <- init_data[ , .(p_start = sum(p_start * N, na.rm = T) / sum(N, na.rm = T)), by = c("year", "sex", "imd_quintile")]
init_data_plot <- init_data[ , .(p_start = mean(p_start, na.rm = T)), by = c("year", "sex", "imd_quintile")]



pi <- ggplot() +
  geom_line(data = init_data_plot, aes(x = year, y = p_start, colour = imd_quintile), linetype = 1) +
  #geom_line(data = init_data_plot[year >= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(initiate)") +
  theme(axis.text.x = element_text(angle = 45)) +
  #ylim(0, 0.04) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

pi


# age specific plot

init_data <- init_data <- copy(smk_init_data)

init_data <- init_data[age >= 10 & age < 30 & year <= 2040 & year >= 2016]

pi1 <- ggplot() +
  geom_line(data = init_data, aes(x = age, y = p_start, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(initiate)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")
pi1

######################################
############## Relapse ###############
######################################

# data <- data_nsw
# hawkins_relapse = smktrans::hawkins_relapse
# lowest_year = 2016
# highest_year = 2022
# youngest_age = 16
# 
# data <- copy(data)
# hawkins_relapse <- copy(hawkins_relapse)

relapse_data <- smktrans::prep_relapse(
  data = data_nsw,
  hawkins_relapse = smktrans::hawkins_relapse,
  lowest_year = 2016,
  highest_year = 2022,
  youngest_age = 16)


#saveRDS(relapse_data, paste0(path, "outputs/relapse_data_", country, ".rds"))

relapse_data_temp <- relapse_data$relapse_by_age_imd_timesincequit[year == 2017]
 
ggplot() +
 geom_line(data = relapse_data_temp, aes(x = age, y = p_relapse, group = time_since_quit)) +
 theme_minimal() +
 ylab("P(relapse)") +
 facet_wrap(~ sex + imd_quintile, nrow = 2)


ggplot() +
 geom_line(data = relapse_data$relapse_by_age_imd, aes(x = age, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
 facet_wrap(~ sex + imd_quintile, nrow = 2) +
 theme_minimal() +
 ylab("P(relapse)") +
 theme(axis.text.x = element_text(angle = 45)) +
 scale_colour_viridis(option = "plasma")


# the probabilities that are used in the model are stratified by age, sex, IMDq and time since quitting
# The approach will be to forecast the version of the probabilities stratified by age, sex and IMDq only
# and then use the results to scale the higher dimensional version

relapse_forecast_data <- smktrans::quit_forecast(
  data = copy(relapse_data$relapse_by_age_imd),
  forecast_var = "p_relapse",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = 2022, # the year at which the forecast becomes stationary
  first_year = 2016, # the earliest year of data on which the forecast is based
  jump_off_year = 2021,
  time_horizon = 2100,
  youngest_age = 16,
  oldest_age = 89,
  age_cont_limit = 75,
  oldest_year = 2016,
  smooth_rate_dim = c(5, 5),
  k_smooth_age = 0)

#saveRDS(relapse_forecast_data, paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

# Forecast the values by age, sex, IMD quintile and time since quitting
relapse_by_age_imd_timesincequit <- smktrans::relapse_forecast(
  relapse_forecast_data = relapse_forecast_data,
  relapse_by_age_imd_timesincequit = relapse_data$relapse_by_age_imd_timesincequit,
  jump_off_year = 2021)

relapse_by_age_imd_timesincequit <- relapse_by_age_imd_timesincequit[age >= 16 & age <= 89]


# # Add ages younger than 18
# # assume younger than 18 have the relapse profile of 18 year olds
# # have the value from the last year
# temp <- relapse_by_age_imd_timesincequit[age == 18]
# #temp <- temp[ , list(p_relapse = mean(p_relapse, na.rm = T)), by = c("age", "time_since_quit", "sex", "imd_quintile")]
# 
# next_age <- min_age
# 
# for(i in min_age:17) {
#   
#   relapse_by_age_imd_timesincequit <- rbindlist(list(
#     relapse_by_age_imd_timesincequit,
#     copy(temp)[ , age := i]), use.names = T)
#   
# }
# 
# temp <- relapse_data$relapse_by_age_imd[age == 18]
# #temp <- temp[ , list(p_relapse = mean(p_relapse, na.rm = T)), by = c("age", "time_since_quit", "sex", "imd_quintile")]
# 
# next_age <- min_age
# 
# for(i in min_age:17) {
#   
#   relapse_data$relapse_by_age_imd <- rbindlist(list(
#     relapse_data$relapse_by_age_imd,
#     copy(temp)[ , age := i]), use.names = T)
#   
# }

# check outputs

#relapse_data_plot <- merge(relapse_by_age_imd_timesincequit, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
relapse_data_plot <- relapse_by_age_imd_timesincequit[age >= 16 & age <= 89 & year <= 2030]

#relapse_data_plot <- relapse_data_plot[ , .(p_relapse = sum(p_relapse * N) / sum(N)), by = c("year", "sex", "imd_quintile")]
relapse_data_plot <- relapse_data_plot[ , .(p_relapse = mean(p_relapse, na.rm = T)), by = c("year", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = relapse_data_plot[year <= 2022], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 1) +
  geom_line(data = relapse_data_plot[year >= 2022], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() + ylim(0, .04) +
  ylab("P(relapse)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))



# age specific plot

relapse_data_plot <- relapse_by_age_imd_timesincequit[age >= 16 & age <= 89 & year <= 2030]

relapse_data_plot <- relapse_data_plot[ , .(p_relapse = mean(p_relapse)), by = c("year", "age", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = relapse_data_plot, aes(x = age, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() + #ylim(0, .04) +
  ylab("P(relapse)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


# time since quit plot

relapse_data_plot <- relapse_by_age_imd_timesincequit[age >= 16 & age <= 89 & year <= 2030]

relapse_data_plot <- relapse_data_plot[ , .(p_relapse = mean(p_relapse)), by = c("year", "time_since_quit", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = relapse_data_plot, aes(x = time_since_quit, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(relapse)") +
  xlab("years since quitting") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


#saveRDS(relapse_by_age_imd_timesincequit, paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))
#write.csv(relapse_by_age_imd_timesincequit, paste0(path, "outputs/relapse_forecast_data_", country, ".csv"), row.names = FALSE)

#stapmr::WriteToExcel(wb, sheet = "Relapse",
#                     title = "Probabilities of relapse to smoking (former to current smoker). Added stratification by years since quitting.",
#                     relapse_by_age_imd_timesincequit, startCol = 1, startRow = 1)


######################################
############# Quitting ###############
######################################

# model trends in current, former and never smoking
trend_data <- smktrans::trend_fit(data = data_nsw,
                                  max_iterations = 1e3,
                                  age_var = "age",
                                  year_var = "year",
                                  sex_var = "sex",
                                  smoker_state_var = "smk.state",
                                  imd_var = "imd_quintile",
                                  weight_var = "wt_int")

trend_data[ , imd_quintile := factor(imd_quintile, levels = c("1_least_deprived", "2", "3", "4", "5_most_deprived"))]

ggplot(trend_data[year == 2019]) +
  geom_line(aes(x = age, y = current, colour = imd_quintile)) +
  facet_wrap(~ sex)  + theme_minimal() + #ylim(0, .04) +
  ylab("P(current smoker)") + ylim(0, .5) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

ggplot(trend_data[age == 50]) +
  geom_line(aes(x = year, y = current, colour = imd_quintile)) +
  facet_wrap(~ sex)  + theme_minimal() + #ylim(0, .04) +
  ylab("P(current smoker)") + ylim(0, .5) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))


#saveRDS(trend_data, paste0(path, "outputs/smoking_trends_", country, ".rds"))

# Cause-specific mortality data
tob_mort_data_cause <- readRDS("transition_probability_estimates/src_england/intermediate_data/tob_mort_data_cause.rds")

tob_mort_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/tob_mort_data_trans.rds")

pop_counts_c <- fread("transition_probability_estimates/src_england/inputs/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv")
setnames(pop_counts_c, "pops", "N")

pops <- pop_counts_c[year == max(pop_counts_c$year)]
pops[ , year := NULL]

hmd_data <- copy(smktrans::hmd_data_eng)

# Estimate the shape of the cohort survivorship functions
survivorship_data <- smktrans::prep_surv(
  mx_data_hmd = hmd_data,
  mx_data_ons = tob_mort_data,
  min_age = 16,
  max_age = 89,
  min_year = 2016,
  max_year = 2022)

#https://www.mortality.org/Home/Index

ggplot(survivorship_data[year == 2019]) +
  geom_line(aes(x = age, y = lx, colour = imd_quintile)) +
  facet_wrap(~ sex)  + theme_minimal() + #ylim(0, .04) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

#saveRDS(survivorship_data, paste0(path, "outputs/survivorship_data_", country, ".rds"))

data_nsw <- copy(data_nsw[ , 2:122])

# Estimate age-specific probabilities of death by smoking status
mortality_data <- smktrans::smoke_surv(
  data = data_nsw,
  diseases  = tobalcepi::tob_disease_names,
  mx_data = tob_mort_data_cause,
  min_age = 16,
  max_age = 89,
  min_year = 2016,
  max_year = 2022)

temp <- melt(mortality_data$data_for_quit_ests[year == 2017], id.vars = c("year", "age", "sex", "imd_quintile"))

ggplot(temp) +
  geom_line(aes(x = age, y = value, colour = variable)) +
  facet_wrap(~ sex + imd_quintile, nrow = 2)  + theme_minimal()

# up to here

#saveRDS(mortality_data, paste0(path, "outputs/mortality_data_", country, ".rds"))

# Calculate quit probabilities
quit_data <- quit_est(
  trend_data = trend_data,
  survivorship_data = survivorship_data,
  mortality_data = mortality_data$data_for_quit_ests,
  relapse_data = relapse_data$relapse_by_age_imd,
  initiation_data = smk_init_data,
  min_age = 16,
  max_age = 89,
  min_year = 2016,
  max_year = 2022)

#saveRDS(quit_data, paste0(path, "outputs/quit_data_", country, ".rds"))

# Forecast a continuing trend in quit probabilities

forecast_data <- quit_forecast(
  data = copy(quit_data),
  forecast_var = "p_quit",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = 2030 + 10, # the year at which the forecast becomes stationary
  first_year = 2016, # the earliest year of data on which the forecast is based
  jump_off_year = 2022 - 1,
  time_horizon = 2040,
  youngest_age = 16,
  oldest_age = 89 - 1,
  oldest_year = 2016,
  age_cont_limit = 75,
  smooth_rate_dim = c(3, 3),
  k_smooth_age = 0)


forecast_data <- forecast_data[age >= 16 & age <= 89]

#saveRDS(forecast_data, paste0(path, "outputs/quit_forecast_data_", country, ".rds"))
#write.csv(forecast_data, paste0(path, "outputs/quit_forecast_data_", country, ".csv"), row.names = FALSE)

# 
# forecast_data_no_init <- quit_forecast(
#   data = copy(quit_data),
#   forecast_var = "p_quit_no_init",
#   forecast_type = "continuing", # continuing or stationary
#   cont_limit = smokefree_target_year + 10, # the year at which the forecast becomes stationary
#   first_year = first_year_of_data_forecast, # the earliest year of data on which the forecast is based
#   jump_off_year = last_year_of_data - 1,
#   time_horizon = max_year,
#   youngest_age = min_age,
#   oldest_age = max_age - 1,
#   oldest_year = first_year_of_data,
#   age_cont_limit = age_trend_limit_quit,
#   smooth_rate_dim = smooth_rate_dim_quit,
#   k_smooth_age = k_smooth_age_quit)
# 
# 
# forecast_data_no_init <- forecast_data_no_init[age >= min_age & age <= max_age]
# 
# saveRDS(forecast_data_no_init, paste0(path, "outputs/quit_forecast_data_no_init_", country, ".rds"))
# write.csv(forecast_data_no_init, paste0(path, "outputs/quit_forecast_data_no_init", country, ".csv"), row.names = FALSE)
# 
# 
# #stapmr::WriteToExcel(wb, sheet = "Quit",
# #                     title = "Probabilities of quitting smoking (current to former smoker).",
# #                     forecast_data, startCol = 1, startRow = 1)
# 
# 
# 



# Quitting --------

quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

quit_data <- merge(quit_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
quit_data <- quit_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

quit_data_plot <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pq <- ggplot() +
  geom_line(data = quit_data_plot[year <= last_year_of_data], aes(x = year, y = p_quit, colour = imd_quintile), linetype = 1) +
  geom_line(data = quit_data_plot[year >= last_year_of_data], aes(x = year, y = p_quit, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(quit)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

png(paste0(path, "outputs/quitting_probabilities_av.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pq + labs(title = "Average quitting probability of current-smokers",
                caption = "Plot shows population weighted average probabilities of quiting over age."))
dev.off()



# age specific plot

#quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

#quit_data <- quit_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

quit_data_plot <- quit_data[ , .(p_quit = mean(p_quit)), by = c("year", "age", "sex", "imd_quintile")]

pq1 <- ggplot() +
  geom_line(data = quit_data_plot, aes(x = age, y = p_quit, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(quit)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")

