
# The aim of this code is to process the Scottish Health Survey data
# into the form required to estimate smoking transition probabilities

# SHeS only asks smoking questions to ages 16+
# ages 16 & 17 do a self completion questionnaire

# years 2008 - 2019

# Load the required packages
library(hseclean)
library(magrittr)
library(data.table)


# https://stapm.gitlab.io/r-packages/hseclean/articles/scottish_data.html

# Location of Scottish data
root_dir <- "X:/HAR_PR/PR/Consumption_TA/HSE/Scottish Health Survey (SHeS)/"

# Apply functions to create the variables for analysis and to retain only the required variables

# The variables to retain
keep_vars = c(
  # Survey design variables
  "wt_int",
  "psu",
  "cluster",
  "year",

  # Social / economic / demographic variables
  "age",
  "age_cat",
  "sex",
  "imd_quintile",
  "degree",
  "relationship_status",
  "kids",
  "employ2cat",
  "income5cat",

  # Long term health conditions
  "hse_mental",

  # Smoking
  "cig_smoker_status",
  "years_since_quit", "years_reg_smoker", "cig_ever",
  "smk_start_age", "smk_stop_age", "censor_age"
  #"cigs_per_day", "smoker_cat", "hand_rolled_per_day", "machine_rolled_per_day", "prop_handrolled", "cig_type"

)

# The variables that must have complete cases
complete_vars <- c("age", "sex", "imd_quintile", "year", "psu", "cluster", "cig_smoker_status", "censor_age")


#-----------------------------------------------------
# Read and clean the data

cleandata <- function(data) {

  data %<>%
    clean_age %>%
    clean_demographic %>%
    clean_education %>%
    clean_economic_status %>%
    clean_family %>%
    clean_income %>%
    clean_health_and_bio %>%
    smk_status %>%
    smk_former %>%
    smk_quit %>%
    smk_life_history %>%
    #smk_amount %>%

    select_data(
      ages = 16:89,
      years = 2008:2019,

      # variables to retain
      keep_vars = keep_vars,

      # The variables that must have complete cases
      complete_vars = complete_vars
    )

  return(data)
}

# Read and clean each year of data and bind them together in one big dataset
data <- combine_years(list(
  cleandata(read_SHeS_2008(root = root_dir)),
  cleandata(read_SHeS_2009(root = root_dir)),
  cleandata(read_SHeS_2010(root = root_dir)),
  cleandata(read_SHeS_2011(root = root_dir)),
  cleandata(read_SHeS_2012(root = root_dir)),
  cleandata(read_SHeS_2013(root = root_dir)),
  cleandata(read_SHeS_2014(root = root_dir)),
  cleandata(read_SHeS_2015(root = root_dir)),
  cleandata(read_SHeS_2016(root = root_dir)),
  cleandata(read_SHeS_2017(root = root_dir)),
  cleandata(read_SHeS_2018(root = root_dir)),
  cleandata(read_SHeS_2019(root = root_dir))
))


# Load population data for Scotland
# from here - X:\ScHARR\PR_Mortality_data_TA\data\Processed pop sizes and death rates from VM
# copied to the inputs folder in this repo
scot_pops <- fread(paste0(path, "inputs/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv"))
setnames(scot_pops, c("pops"), c("N"))

# adjust the survey weights according to the ratio of the real population to the sampled population
data <- clean_surveyweights(data, pop_data = scot_pops)

# remake age categories
data[, age_cat := c("11-15",
                    "16-17",
                    "18-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-64",
                    "65-74",
                    "75-89")[findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

setnames(data,
         c("smk_start_age", "cig_smoker_status", "years_since_quit"),
         c("start_age", "smk.state", "time_since_quit"))

# remove invalid smoking start and stop ages
data[start_age < 11, start_age := NA]
data[smk_stop_age < 11, smk_stop_age := NA]

# Save data
saveRDS(data, paste0(path, "intermediate_data/SHeS_2008_to_2019_tobacco.rds"))


################
# Impute missing values

# Load the data
data <- readRDS(paste0(path, "intermediate_data/SHeS_2008_to_2019_tobacco.rds"))

# view variables with missingness
misscheck <- function(var) {
  x <- table(var, useNA = "ifany")
  na <- x[which(is.na(names(x)))]
  if(length(na) == 0) na <- 0
  perc <- round(100 * na / sum(x), 2)
  #return(c(paste0(na, " missing obs, ", perc, "%")))
  return(na)
}

n_missing <- sapply(data, misscheck)
missing_vars <- n_missing[which(n_missing > 0)]
missing_vars

# quick fixes to missing data
# assume that if missing data on mental health issues below age 16,
# then no mental health issues
#data[is.na(hse_mental) & age < 16, hse_mental := "no_mental"]

# household equivalised income has the most missingness
# - this is a key variable to impute
# as we will use it to understand policy inequalities

# Set order of factors where needed for imputing as ordered.
data[ , kids := factor(kids, levels = c("0", "1", "2", "3+"))]
data[ , income5cat := factor(income5cat, levels = c("1_lowest_income", "2", "3", "4", "5_highest_income"))]

# Impute missing values

# Run the imputation
imp <- impute_data_mice(
  data = data,
  var_names = c(
    "sex",
    "age_cat",
    "kids",
    "relationship_status",
    "imd_quintile",
    "degree",
    "employ2cat",
    "income5cat",
    "hse_mental",
    "smk.state"
  ),
  var_methods = c(
    "",
    "",
    "polr",
    "polyreg",
    "",
    "logreg",
    "",
    "polr",
    "logreg",
    ""
  ),
  n_imputations = 5
  # for testing just do 1 imputation
  # but test with more later
  # for point estimates, apparently 2-10 imputations are enough
)

data_imp <- copy(imp$data)

# Save data
saveRDS(data_imp, paste0(path, "intermediate_data/SHeS_2008_to_2019_tobacco_imputed.rds"))


