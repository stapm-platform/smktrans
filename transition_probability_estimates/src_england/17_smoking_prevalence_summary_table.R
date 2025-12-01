
# Produce a summary table of the smoking prevalence data

# Load the data
data <- readRDS(paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco.rds"))

data[ , year_cat := c("2003-2012", "2013-2018")[findInterval(year, c(-1, 2013,100000))]]

year_data <- data[ , .N, by = c("year_cat", "smk.state")]
year_data[ , perc := round(100 * N / sum(N), 1), by = c("year_cat")]

# Remake age categories to align with the HSE definitions
data[ , age_cat := c("11-17", "18-24", "25-49", "50+")[findInterval(age, c(-1, 18, 25, 50, 1000))]]

age_data <- data[ , .N, by = c("age_cat", "smk.state")]
age_data[ , perc := round(100 * N / sum(N), 1), by = c("age_cat")]

sex_data <- data[ , .N, by = c("sex", "smk.state")]
sex_data[ , perc := round(100 * N / sum(N), 1), by = c("sex")]

imd_data <- data[ , .N, by = c("imd_quintile", "smk.state")]
imd_data[ , perc := round(100 * N / sum(N), 1), by = c("imd_quintile")]





