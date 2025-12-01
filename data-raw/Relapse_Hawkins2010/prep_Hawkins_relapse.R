
# The aim of this code is to prep the estimates of relapse to smoking from
# Hawkins J, Hollingworth W, Campbell R. Long-term smoking relapse: 
# a study using the British Household Panel Survey.
# Nicotine & Tobacco Research. 2010 Oct 29;12(12):1228-35.
# https://doi.org/10.1093/ntr/ntq175

# With an adjusted relapse probability added for people who have quit for less than a year
# based on the placebo curve from Jackson et al.
# https://doi.org/10.1111/add.14549

library(data.table)
library(stapmr)

# Read in csv file from Hawkins 2010 paper.
relapse <- fread("data-raw/Relapse_Hawkins2010/Smoking_Relapse_Hawkins_percentage.csv")

###################################
# Insert additional relapse probability for people who have quit for less than a year
relapse0 <- data.table(Quit = as.integer(0))
relapse <- rbindlist(list(relapse0, relapse), use.names = T, fill = T)

# Calculate the expected duration of time since quitting for people who have quit
# for less than a year, assuming use of NRT
wk <- 1:52
pa <- stapmr::SmkContAbst("placebo", wk)
dur <- sum(wk * pa) / sum(pa)
# 21.2 weeks

# Calculate adjustment factor
adj0 <- stapmr::SmkContAbst("placebo", dur) / stapmr::SmkContAbst("placebo", 52)

# Calculate the percentage who relapse
relapse[Quit == 0, Percentage := relapse[Quit == 1, Percentage] * adj0]

###################################

# convert the percentage of people in continuous abstinence from percentage to probability and then to odds
relapse[ , Probability := Percentage / 100]

relapse[ , odds := Probability / (1 - Probability)]
relapse <- relapse[ , c("Quit", "odds")]
setnames(relapse, "Quit", "time_since_quit")

# expand by covariates
domain <- data.frame(expand.grid(
  time_since_quit = 0:10,
  age = 18:89,
  sex = c("Male", "Female"),
  degree = c("degree", "no_degree"),
  relationship_status = c("single", "married", "sep_div_wid", "cohabit"),
  employ2cat = c("employed", "unemployed"),
  hse_mental = c("mental", "no_mental"),
  income5cat = c("1_lowest_income", "2", "3", "4", "5_highest_income"),
  imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
))
setDT(domain)

# merge with time since quit variation
domain <- merge(domain, relapse, by = "time_since_quit")

# add effect of age
# average age of individuals in study
av_age <- round((1147 * 44.8 + 431 * 38.2) / (1147 + 431), 0)
domain[ , age_or := .96 ^ (age - 45)]

# add effect of sex
domain[ , sex_or := 1]
domain[sex == "Male", sex_or := 1.15]

# add effect of unemployment
domain[ , employ2cat_or := 1]
domain[employ2cat == "unemployed", employ2cat_or := .58]

# add effect of degree
domain[ , degree_or := 1]
domain[degree == "degree", degree_or := .6]

# add effect of relationship status
domain[ , relationship_status_or := 1]
domain[relationship_status == "married", relationship_status_or := .6]
domain[relationship_status == "cohabit", relationship_status_or := .91]

# add effects of health
domain[ , mental_health_or := 1]
domain[hse_mental == "mental", mental_health_or := 2.49]

# effect of income
domain[ , income_or := 1]
domain[income5cat == "2", income_or := 1]
domain[income5cat == "3", income_or := .91]
domain[income5cat == "4", income_or := .99]
domain[income5cat == "5_highest_income", income_or := .87]

# adjust odds for the above effects

domain[ , odds_adj := odds * age_or * sex_or * degree_or * employ2cat_or * relationship_status_or * mental_health_or * income_or]

domain[ , `:=`(age_or = NULL, sex_or = NULL, degree_or = NULL, employ2cat_or = NULL, relationship_status_or = NULL, mental_health_or = NULL, income_or = NULL)]

# covert odds back to probability
domain[ , p_relapse := odds_adj / (odds_adj + 1)]

domain[ , odds := NULL]
domain[ , odds_adj := NULL]

# Assume < 18 year olds have same relapse characteristics as 18 year olds
temp <- copy(domain[age == 18])

for(i in 1:17) {
  domain <- rbindlist(list(
    domain,
    copy(temp[ , age := i])
  ))
}
rm(temp)

# Save the result to the package data folder
hawkins_relapse <- copy(domain)
usethis::use_data(hawkins_relapse, overwrite = TRUE)

