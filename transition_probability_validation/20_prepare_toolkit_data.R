
# The aim of this code is to prepare the Smoking Toolkit Study data
# for use in validating the estimated smoking state probabilities of
# net initiation & quitting, which are the only estimated quantities that are estimable from the STS

# This code file does the general data preparation
# and subsequent code files summarise the data to produce the values for comparison for net initiation and quitting

# Access to the STS data is by permission from the study owners
# The data is not included in this R package of code

# A local STAPM R package "toolkitr" has been written that contains basic reusable processing functions
# that can be applied to process the STS data
# The code below applies these functions

# Set file path to the STS data omnibus data file
path <- "transition_probability_validation/input/"
file <- "omni205_39.1_65.2cot_31.3a_25.4s_recodes_91.5sa"

# Age limits and categorisation of age
ages <- 16:89
age_categories = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")
age_cat_start_age = c(16, 18, 25, 35, 50, 65, 75)

# Read the STS data
toolkit_data <- toolkitr::ReadToolkit(
  path_in = path,
  data_in = file,
  save = F)

# Convert to data.table format
setDT(toolkit_data)

# Filter to keep only England
toolkit_data <- toolkit_data[gore %in% c("East of England",
                                         "South West",
                                         "South East",
                                         "North East",
                                         "East Midlands",
                                         "West Midlands",
                                         "North West",
                                         "Yorkshire and The Humber",
                                         "London")]

age_daily <- toolkit_data[sexz %in% c("Men", "Women"), .(mu = sum(daily * Aweight0, na.rm = T) / sum(Aweight0, na.rm = T)), by = c("actage", "sexz")]
age_daily <- toolkit_data[sexz %in% c("Men", "Women"), .(mu = sum(smoker * Aweight0, na.rm = T) / sum(Aweight0, na.rm = T)), by = c("actage", "sexz")]

ggplot(age_daily) +
  geom_point(aes(x = actage, y = mu, colour = sexz)) +
  geom_vline(xintercept = 25) + 
  geom_vline(xintercept = 16)

age_daily <- age_daily[actage <= 25]

setorderv(age_daily, c("sexz", "actage"), c(1, 1))

#library(zoo)
#age_daily[ , mu_av := rollmean(mu, k = 2, fill = NA, align = "left"), by = "sexz"]

#ggplot(age_daily) +
#  geom_point(aes(x = actage, y = mu, colour = sexz)) +
#  geom_line(aes(x = actage, y = mu_av, colour = sexz))

age_daily[ , mu_diff := -1 * c(diff(1 - mu), NA), by = "sexz"]
age_daily[mu_diff < 0, `:=`(mu = NA, mu_diff = NA)]
age_daily[ , mu_approx := approx(x = actage, y = mu, xout = actage)$y, by = "sexz"]

age_daily[ , mu_diff := -1 * c(diff(1 - mu_approx), NA), by = "sexz"]
age_daily[mu_diff < 0, `:=`(mu_approx = NA, mu_diff = NA)]
age_daily[ , mu_approx := approx(x = actage, y = mu_approx, xout = actage)$y, by = "sexz"]

ggplot(age_daily) +
  geom_line(aes(x = actage, y = mu_approx, colour = sexz))

age_daily[ , mu_diff := -1 * c(diff(1 - mu_approx), NA), by = "sexz"]

age_daily[ , q_init := mu_diff / (1 - mu_approx)]

ggplot(age_daily) +
  geom_line(aes(x = actage, y = q_init, colour = sexz)) +
  geom_vline(xintercept = 18)



#nrow(toolkit_data)

# Filter to select ages
# Upper age limit is set to correspond to the upper age limit of 89 in the STAPM model
toolkit_data <- toolkit_data[actage >= 16 & actage <= 89]

# Filter to select STS waves
# The STS waves have been selected to align with the time period for which the transition probabilities
# have been estimated from the Health Survey for England data
#toolkit_data <- toolkit_data[xwave >= 75 & xwave <= 147] # 2013-2018

# Option to select waves from 2019 onwards
toolkit_data <- toolkit_data[xwave >= 148] # 2019-

#nrow(toolkit_data)

# Apply the functions in the toolkitr package to clean the data

# Assign each row a unique number
toolkit_data[, `:=`(id, .I)]

# Apply functions to prepare the different categories of data
clean_demo <- toolkitr::ToolkitCleanDemographic(data = toolkit_data, 
                                                age_categories = age_categories, age_cat_start_age = age_cat_start_age)
clean_smoke <- toolkitr::ToolkitCleanSmkStatus(data = toolkit_data)
clean_smoke_con <- toolkitr::ToolkitCleanSmkCons(data = toolkit_data)
clean_smoke_pur <- toolkitr::ToolkitCleanSmkSpend(data = toolkit_data)

# Select key variables
toolkit_data <- toolkit_data[, c("id", "xwave", "weight_gb", "Aweight0", "q632b7_1", "q632b8", "q632b9")]
setnames(toolkit_data, "Aweight0", "weight_england")

# Merge key variables with the various cleaned covariates
data_tk <- merge(toolkit_data, clean_demo, by = "id", sort = F, all.x = T, all.y = F)
data_tk <- merge(data_tk, clean_smoke, by = "id", sort = F, all.x = T, all.y = F)
data_tk <- merge(data_tk, clean_smoke_con, by = "id", sort = F, all.x = T, all.y = F)
data_tk <- merge(data_tk, clean_smoke_pur, by = "id", sort = F, all.x = T, all.y = F)

rm(clean_demo, clean_smoke, clean_smoke_con, clean_smoke_pur)
rm(toolkit_data)
gc()

#nrow(data_tk)

# Save the cleaned dataset to be used in further processing
saveRDS(data_tk, "transition_probability_validation/intermediate_data/toolkit_england.rds")




