
# The aim of this code is to further process the STS data prepared in "20_prepare_toolkit_data.R"
# to produce metrics for validation of the probabilities of quitting smoking

# The approach to processing the STS data for this purpose involves two variables:

# First,
# taking each individual's current smoking status, as recorded in each wave of the STS
# this is in the "smoker_status" variable in the toolkitr processed version of the data

# Second,
# constructing a counter-factual smoking status variable by turning back people who are currently 
# recorded as ex-smokers into current smokers if their latest quit attempt began less than a year ago
# this is done using information from the "q632b8" variable in the toolkit data

# How long ago most recent serious quit attempt started	q632b8
#unique(data_tk$q632b8)
#unique(data_tk$smoker_status)

# Read-in the previously processed STS data
data_tk <- readRDS("transition_probability_validation/intermediate_data/toolkit_england.rds")

# Construct the counter-factual smoking status variable
data_tk[, smoker_status1 := smoker_status]
data_tk[smoker_status1 == "ex_smoker" & q632b8 %in% c("More than 6 months and up to a year",
                                                      "More than 3 months and up to 6 months",
                                                      "More than 2 months and up to 3 months",
                                                      "More than 1 month and up to 2 months",
                                                      "More than a week and up to a month",
                                                      "In the last week"), smoker_status1 := "current_smoker"]

# Convert the actual and counter-factual smoking status variables to binary

# Actual
data_tk[ , smk_status_bin := 0]
data_tk[smoker_status == "current_smoker" , smk_status_bin := 1]

# Counter-factual
data_tk[ , smk1_status_bin := 0]
data_tk[smoker_status1 == "current_smoker" , smk1_status_bin := 1]

# Compute the survey-weighted averages of these binary variables for each age
test <- data_tk[ , .(smkt = sum(smk_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
                     smkt1 = sum(smk1_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
                     pop = .N), by = "age"]

# Compute the probability of quitting as 1 minus the probability that someone remains a smoker 
# between the start and end of the year interval
test[ , p_quit := 1 - smkt / smkt1]

# Calculate the expected binomial standard error around this probability of quitting
test[ , se := sqrt(p_quit * (1 - p_quit) / (pop * smkt))]
test[ , upper95 := p_quit + 1.96 * se]
test[ , lower95 := p_quit - 1.96 * se]

# Plot the estimated probabilities of quitting by age
p <- ggplot(test[p_quit > 0 & age >= 16]) + geom_point(aes(x = age, y = p_quit)) + 
  #geom_errorbar(aes(x = age, ymin = lower95, ymax = upper95)) +
  theme_minimal() + ylab("P(quit)") +
  ylim(0, 1)
p  

# Load the STAPM estimated quitting probabilities 
quit_data <- readRDS("transition_probability_validation/intermediate_data/quit_forecast_data_2011_2100.rds")

# select the years in question - to match the STS data
#quit_data <- quit_data[year %in% 2013:2018]
quit_data <- quit_data[year %in% 2019:2023]

# calculate the average probability of quitting by age (**note, this should be a population weighted average**)
quit_data_a <- quit_data[ , .(p_quit_ref = mean(p_quit, na.rm = T)), by = "age"]

p <- p + geom_line(data = quit_data_a, aes(x = age, y = p_quit_ref))

p  



