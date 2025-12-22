
# The aim of this code is to produce the calibration targets for calibrating the ABM probabilities of quitting smoking

# Quitting
quit_data <- readRDS("transition_probability_estimates/src_england/outputs/quit_data_england_uncertainty.rds")
quit_data <- quit_data[age >= 16 & age <= 89]
quit_data <- quit_data[year >= 2011 & year <= 2040, .(year, age, sex, imd_quintile, p_quit)]
setorderv(quit_data, c("year", "age", "sex", "imd_quintile"), c(1, 1, 1, 1))
setnames(quit_data, c("year", "age", "sex", "imd_quintile"), c("arrivalYear", "pAge", "pGender", "pIMDquintile"))
write.csv(quit_data, "transition_probability_estimates/src_england/outputs/quit_probabilities_20251222_v1.csv", row.names = F)

# Initiation
init_data <- readRDS("transition_probability_estimates/src_england/outputs/init_data_england_uncertainty.rds")
init_data <- init_data[age >= 16 & age <= 30]
init_data <- init_data[year >= 2011 & year <= 2040, .(year, age, sex, imd_quintile, p_start)]
setorderv(init_data, c("year", "age", "sex", "imd_quintile"), c(1, 1, 1, 1))
setnames(init_data, c("year", "age", "sex", "imd_quintile"), c("arrivalYear", "pAge", "pGender", "pIMDquintile"))
write.csv(init_data, "transition_probability_estimates/src_england/outputs/init_probabilities_20251222_v1.csv", row.names = F)

# Relapse
relapse_data <- readRDS("transition_probability_estimates/src_england/outputs/relapse_data_england_uncertainty.rds")
relapse_data <- relapse_data[age >= 16 & age <= 89]
relapse_data <- relapse_data[year >= 2011 & year <= 2040, .(year, age, sex, imd_quintile, time_since_quit, p_relapse)]
setorderv(relapse_data, c("year", "age", "sex", "imd_quintile", "time_since_quit"), c(1, 1, 1, 1, 1))
setnames(relapse_data, c("year", "age", "sex", "imd_quintile", "time_since_quit"), c("arrivalYear", "pAge", "pGender", "pIMDquintile", "bYearsSinceQuit"))
write.csv(relapse_data, "transition_probability_estimates/src_england/outputs/relapse_probabilities_20251222_v1.csv", row.names = F)

##############################
# Quit calibration targets

# Read the estimated quit probabilities with uncertainty
quit_data <- readRDS("transition_probability_estimates/src_england/outputs/quit_data_england_uncertainty.rds")

# filter ages
quit_data <- quit_data[age >= 25 & age <= 74]

# filter years
quit_data <- quit_data[year >= 2011 & year <= 2019]

# Make age categories
quit_data[ , age_cat := c("25-44", "45-64", "65-74")[findInterval(age, c(-1, 45, 65, 100))]]

# Make year categories
quit_data[ , year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(-1, 2014, 2017, 10000))]]

# Table 3: cross tabulated averages of the annual probabilities of quitting by sex and age category (25-44, 45-64, 65-74) 
# - corresponding to the average across calendar years 2011 - 2016.
t3 <- quit_data[year %in% 2011:2016, .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), by = c("age_cat", "sex")]

# Table 4: cross tabulated averages of the annual probabilities of quitting by calendar year categories 
# (2011 - 2013, 2014 - 2016, 2017 - 2019) and IMD quintile
t4 <- quit_data[year %in% 2011:2016, .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), by = c("year_cat", "imd_quintile")]

# Table 5: cross tabulated averages of the annual probabilities of quitting by sex and age category (25-44, 45-64, 65-74) 
# - corresponding to the average across calendar years 2017 - 2019.
t5 <- quit_data[year %in% 2017:2019, .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), by = c("age_cat", "sex")]

# Table 6: annual probabilities of quitting by IMD quintile for calendar years 2017 - 2019.
t6 <- quit_data[year %in% 2017:2019, .(p_quit = mean(p_quit), p_quit_var = mean(p_quit_var)), by = c("year_cat", "imd_quintile")]

# combine
data_t <- rbindlist(list(t3, t4, t5, t6), use.names = T, fill = T)

data_t[is.na(year_cat), year_cat := "2011-2016"]

data_t[is.na(age_cat), age_cat := "All"]
data_t[is.na(sex), sex := "All"]
data_t[is.na(imd_quintile), imd_quintile := "All"]

setnames(data_t, c("year_cat", "age_cat", "sex", "imd_quintile"), c("arrivalYearCategorical", "pAgeCategorical", "pGender", "pIMDquintile"))

data_t <- data_t[ , .(arrivalYearCategorical, pIMDquintile, pGender, pAgeCategorical, p_quit, p_quit_var)]

write.csv(data_t, "transition_probability_estimates/src_england/outputs/quit_probability_calibration_targets_20251222_v1.csv", row.names = F)

