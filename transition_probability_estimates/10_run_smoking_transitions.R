
# The aim of this code is to estimate the smoking transition probabilities
# needed for use in the model

root_dir <- "X:/"

source("transition_probability_estimates/03_load_packages.R")

# template for the summary sheet
template <- "smoking_state_transition_probabilities_template"

#######################################
#######################################
# England

survey_data_name <- "Health Survey for England"

# Load the spreadsheet template
wb <- openxlsx::loadWorkbook(paste0("transition_probability_estimates/", template, ".xlsx"))

# the first year of available survey data
first_year_of_data <- 2003

# the last year of available survey data
last_year_of_data <- 2018

# the earliest year of data on which the forecast is based
first_year_of_data_forecast <- 2013

current_year <- 2023
smokefree_target_year <- 2030
max_year <- 2100
min_age <- 12
max_age <- 89
country <- "England"
path <- "transition_probability_estimates/src_england/"

ref_age <- 30
max_age_init <- 30
age_trend_limit_init <- 21

# Initiation forecast
smooth_rate_dim_init <- c(3, 7) # The dimensions of the 2d window used to smooth trends in the rates by age and year. (age, year), Defaults to c(3, 3). Must be odd numbers
k_smooth_age_init <- 0 # the degree of smoothing to apply to the age pattern of change (rotation). If zero, then no smoothing is applied.

# Quit forecast
smooth_rate_dim_quit <- c(5, 7)
k_smooth_age_quit <- 6
age_trend_limit_quit <- 75

# Relapse forecast
#smooth_rate_dim_relapse <- c(5, 3)
#k_smooth_age_relapse <- 0

smooth_rate_dim_relapse <- c(15, 7)
k_smooth_age_relapse <- 6
age_trend_limit_relapse <- 75

#################################################
#### Key assumptions for estimating uncertainty

# Number of trials
# this is an approximation of the sample size expected in each cell (smoking state, year, age, sex, IMD quintile)
# vary it in orders of magnitude of 10 since this is a rough approximation
kn <- 100

# Correlation of uncertainly
# e.g. the expected correlation of uncertainty between the initiation probability at age 20 and age 21
# We might expect that if the estimate at age 20 is high, then the estimate at age 21 will also be high
# so set the level of correlation high but not perfect
kR <- 0.95

# Number of uncertainty samples
# ideally this would be set at 10,000 but that would take ages to run in the current setup
# test robustness of estimates at 100 or 1000
kn_samp <- 100

#################################################

# Spreadsheet cover sheet
openxlsx::writeData(wb, sheet = "Cover sheet", country, startCol = 2, startRow = 2)
openxlsx::writeData(wb, sheet = "Cover sheet", Sys.Date(), startCol = 2, startRow = 3)

openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("smktrans", fields = "Version"), startCol = 2, startRow = 10)
openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("tobalcepi", fields = "Version"), startCol = 2, startRow = 11)
openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("hseclean", fields = "Version"), startCol = 2, startRow = 12)
openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("mort.tools", fields = "Version"), startCol = 2, startRow = 13)


openxlsx::writeData(wb, sheet = "Cover sheet", survey_data_name, startCol = 2, startRow = 26)
openxlsx::writeData(wb, sheet = "Cover sheet", first_year_of_data, startCol = 2, startRow = 27)
openxlsx::writeData(wb, sheet = "Cover sheet", last_year_of_data, startCol = 2, startRow = 28)
openxlsx::writeData(wb, sheet = "Cover sheet", min_age, startCol = 2, startRow = 29)
openxlsx::writeData(wb, sheet = "Cover sheet", ref_age, startCol = 2, startRow = 30)
openxlsx::writeData(wb, sheet = "Cover sheet", max_age, startCol = 2, startRow = 31)
openxlsx::writeData(wb, sheet = "Cover sheet", paste0(first_year_of_data_forecast, " to ", last_year_of_data, " (", last_year_of_data - first_year_of_data_forecast + 1, " years)"), startCol = 2, startRow = 32)
openxlsx::writeData(wb, sheet = "Cover sheet", smokefree_target_year + 10, startCol = 2, startRow = 33)
openxlsx::writeData(wb, sheet = "Cover sheet", max_year, startCol = 2, startRow = 34)

# run once to build inputs
#source("transition_probability_estimates/src_england/00_run_smoking_transitions.R")

# Survey data
survey_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds")

# Cause-specific mortality data
tob_mort_data_cause <- readRDS("transition_probability_estimates/src_england/intermediate_data/tob_mort_data_cause.rds")

tob_mort_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/tob_mort_data_trans.rds")

pop_counts_c <- fread("transition_probability_estimates/src_england/inputs/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv")
setnames(pop_counts_c, "pops", "N")

pops <- pop_counts_c[year == max(pop_counts_c$year)]
pops[ , year := NULL]

hmd_data <- copy(smktrans::hmd_data_eng)

# Functions to estimate the probabilities
source("transition_probability_estimates/20_estimate_smoking_transition_probabilities.R")

# Add uncertainty estimates
source("transition_probability_estimates/23_uncertainty_in_smoking_transitions.R")

saveWorkbook(wb, paste0("transition_probability_estimates/outputs/smoking_state_transition_probabilities_", country, ".xlsx"), overwrite = T)

# Summarise transition probabilities
source("transition_probability_estimates/22_summarise_smoking_transitions.R")


#######################################
#######################################
# Scotland

survey_data_name <- "Scottish Health Survey"

# Load the spreadsheet template
wb <- openxlsx::loadWorkbook(paste0("transition_probability_estimates/", template, ".xlsx"))

# the first year of available survey data
first_year_of_data <- 2008

# the last year of available survey data
last_year_of_data <- 2019

# the earliest year of data on which the forecast is based
first_year_of_data_forecast <- 2015

current_year <- 2023
smokefree_target_year <- 2034
max_year <- 2100
min_age <- 16
max_age <- 89
country <- "Scotland"
path <- "transition_probability_estimates/src_scotland/"

ref_age <- 30
max_age_init <- 30
age_trend_limit_init <- 22

# Initiation forecast
smooth_rate_dim_init <- c(3, 5)
k_smooth_age_init <- 0

# Quit forecast
smooth_rate_dim_quit <- c(5, 7)
k_smooth_age_quit <- 6
age_trend_limit_quit <- 75

# Relapse forecast
smooth_rate_dim_relapse <- c(5, 7)
k_smooth_age_relapse <- 6
age_trend_limit_relapse <- 75

# Spreadsheet cover sheet
openxlsx::writeData(wb, sheet = "Cover sheet", country, startCol = 2, startRow = 2)
openxlsx::writeData(wb, sheet = "Cover sheet", Sys.Date(), startCol = 2, startRow = 3)

openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("smktrans", fields = "Version"), startCol = 2, startRow = 10)
openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("tobalcepi", fields = "Version"), startCol = 2, startRow = 11)
openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("hseclean", fields = "Version"), startCol = 2, startRow = 12)
openxlsx::writeData(wb, sheet = "Cover sheet", packageDescription("mort.tools", fields = "Version"), startCol = 2, startRow = 13)

openxlsx::writeData(wb, sheet = "Cover sheet", survey_data_name, startCol = 2, startRow = 26)
openxlsx::writeData(wb, sheet = "Cover sheet", first_year_of_data, startCol = 2, startRow = 27)
openxlsx::writeData(wb, sheet = "Cover sheet", last_year_of_data, startCol = 2, startRow = 28)
openxlsx::writeData(wb, sheet = "Cover sheet", min_age, startCol = 2, startRow = 29)
openxlsx::writeData(wb, sheet = "Cover sheet", ref_age, startCol = 2, startRow = 30)
openxlsx::writeData(wb, sheet = "Cover sheet", max_age, startCol = 2, startRow = 31)
openxlsx::writeData(wb, sheet = "Cover sheet", paste0(first_year_of_data_forecast, " to ", last_year_of_data, " (", last_year_of_data - first_year_of_data_forecast + 1, " years)"), startCol = 2, startRow = 32)
openxlsx::writeData(wb, sheet = "Cover sheet", smokefree_target_year + 10, startCol = 2, startRow = 33)
openxlsx::writeData(wb, sheet = "Cover sheet", max_year, startCol = 2, startRow = 34)

# run once to build inputs
#source("transition_probability_estimates/src_scotland/00_run_smoking_transitions.R")

# Survey data
survey_data <- readRDS("transition_probability_estimates/src_scotland/intermediate_data/SHeS_2008_to_2019_tobacco_imputed.rds")

# Cause-specific mortality data
tob_mort_data_cause <- readRDS("transition_probability_estimates/src_scotland/intermediate_data/tob_mort_data_cause.rds")

tob_mort_data <- readRDS("transition_probability_estimates/src_scotland/intermediate_data/tob_mort_data_trans.rds")

pop_counts_c <- fread("transition_probability_estimates/src_scotland/inputs/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv")
setnames(pop_counts_c, "pops", "N")

pops <- pop_counts_c[year == max(pop_counts_c$year)]
pops[ , year := NULL]

hmd_data <- copy(smktrans::hmd_data_scot)

# Estimate the probabilities
source("transition_probability_estimates/20_estimate_smoking_transition_probabilities.R")

saveWorkbook(wb, paste0("transition_probability_estimates/outputs/smoking_state_transition_probabilities_", country, ".xlsx"), overwrite = T)

# Summarise transition probabilities
source("transition_probability_estimates/22_summarise_smoking_transitions.R")





