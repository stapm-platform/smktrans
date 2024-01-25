
# Produce a summary table of the smoking prevalence data

library(data.table)

# Load the data
#data <- readRDS(paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds"))
data <- readRDS(paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco.rds"))
#age
#sex
#imd_quintile
#smk.state
#time_since_quit

data[ , year_cat := c("2003-2012", "2013-2018")[findInterval(year, c(-1, 2013,100000))]]

year_data <- data[ , .N, by = c("year_cat", "smk.state")]
year_data[ , perc := round(100 * N / sum(N), 1), by = c("year_cat")]
#year_data <- melt(year_data, id.vars = c("year_cat", "smk.state"))
#setnames(year_data, "year_cat", "var")
#year_data[ , name := "year"]

# Remake age categories to align with the HSE definitions
data[ , age_cat := c("11-17", "18-24", "25-49", "50+")[findInterval(age, c(-1, 18, 25, 50, 1000))]]

age_data <- data[ , .N, by = c("age_cat", "smk.state")]
age_data[ , perc := round(100 * N / sum(N), 1), by = c("age_cat")]
#age_data <- melt(age_data, id.vars = c("age_cat", "smk.state"))
#setnames(age_data, "age_cat", "var")
#age_data[ , name := "age"]

sex_data <- data[ , .N, by = c("sex", "smk.state")]
sex_data[ , perc := round(100 * N / sum(N), 1), by = c("sex")]
#sex_data <- melt(sex_data, id.vars = c("sex", "smk.state"))
#setnames(sex_data, "sex", "var")
#sex_data[ , name := "sex"]

imd_data <- data[ , .N, by = c("imd_quintile", "smk.state")]
imd_data[ , perc := round(100 * N / sum(N), 1), by = c("imd_quintile")]
#imd_data <- melt(imd_data, id.vars = c("imd_quintile", "smk.state"))
#setnames(imd_data, "imd_quintile", "var")
#imd_data[ , name := "IMD quintile"]

# tot_data <- data[ , .N, by = c("smk.state")]
# tot_data[ , perc := round(100 * N / sum(N), 1)]
# tot_data <- melt(tot_data, id.vars = c("smk.state"))
# tot_data[ , name := "Total"]
# tot_data[ , var := ""]
# 
# table_data <- rbindlist(list(tot_data, age_data, sex_data, imd_data), fill = F, use.names = T)
# 
# table_data <- dcast(table_data, name + var ~ variable + smk.state, value.var = "value")
# 
# table_data <- rbindlist(list(table_data[name != 'IMD quintile'], table_data[name == 'IMD quintile']), use.names = T)
# 
# write.table(table_data, paste0(path, "outputs/smoking_prev_summary_data_england.csv"), row.names = F, sep = ",")







