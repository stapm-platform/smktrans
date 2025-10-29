
# The aim of this code is to further process the STS data prepared in "20_prepare_toolkit_data.R"
# to produce metrics for validation of the net smoking initiation probabilities

# The approach to processing the STS data for this purpose involves:

# Calculating the net initiation probability using 
# the same approach as for the estimated STAPM transition probabilities

# The net initiation probability is defined at the population-level
# it is used to simulate the change over age in the number of current smokers (initiation net of quitting)
# it is best defined for ages when the number of current smokers is growing,
# indicating that initiation is the dominant process, 
# rather than at later ages, when the number of current smokers is falling
# the age cut-off (for when numbers of current smokers begins to fall)
# could be at any age after 19 (based on similar analysis for the US)
# and could vary by sex and IMD quintile
# so the age range for validation needs to be carefully chosen based on the empirical data

# Read-in the previously processed STS data
data_tk <- readRDS("transition_probability_validation/intermediate_data/toolkit_england.rds")

# Construct a binary variable for who is a current smoker
data_tk[ , smk_status_bin := 0]
data_tk[smoker_status == "current_smoker" , smk_status_bin := 1]

# Compute the population weighted average probability of being a current smoker over age
test <- data_tk[ , .(smkt = sum(smk_status_bin * weight_england, na.rm = T) / sum(weight_england, na.rm = T),
  pop = .N), by = "age"]

# Filter to ages less than 30
test <- test[age < 30]

# order increasingly by age
setorderv(test, "age", 1)

# Fit a curve 
library(mgcv)

test[ , cumsum_p := cumsum(smkt)]

m1 <- gam(cumsum_p ~ s(age, k = 10), data = test)
test$preds <- predict(m1, newdata = test)

test[ , preds_diff := diff(c(0, test$preds))]

ggplot(test) +
  geom_point(aes(x = age, y = smkt)) +
  geom_line(aes(x = age, y = preds_diff))


# Compute the difference in the 
test[ , smkt_diff := c(0, diff(preds_diff))]

test <- test[smkt_diff > 0]


# up to here



test <- rbindlist(list(data.table(age = 15, smkt = NA, pop = NA, smkt_adj = NA, smkt_adj_diff = NA), test))

test[1 , nc_smk := 1000]

for(i in 1:10) {
  test[i+1, nc_smk := 1000- test[i+1, smkt_adj]]
}

for(i in 1:10) {
  test[i+1, p_start_net := test[i+1, smkt_adj_diff] / test[i, nc_smk]]
}







