
# Produce a summary table of the smoking prevalence data

library(data.table)

# Load the data
data <- readRDS(paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds"))

#age
#sex
#imd_quintile
#smk.state
#time_since_quit

# Remake age categories to align with the HSE definitions
data[ , age_cat := c("11-12", "13-15", "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89")[findInterval(age, c(-1, 13,16,18,seq(20, 90, 5)))]]

age_data <- data[ , .N, by = c("age_cat", "smk.state")]
age_data[ , perc := round(100 * N / sum(N), 1), by = c("age_cat")]
age_data <- melt(age_data, id.vars = c("age_cat", "smk.state"))
setnames(age_data, "age_cat", "var")
age_data[ , name := "age"]

sex_data <- data[ , .N, by = c("sex", "smk.state")]
sex_data[ , perc := round(100 * N / sum(N), 1), by = c("sex")]
sex_data <- melt(sex_data, id.vars = c("sex", "smk.state"))
setnames(sex_data, "sex", "var")
sex_data[ , name := "sex"]

imd_data <- data[ , .N, by = c("imd_quintile", "smk.state")]
imd_data[ , perc := round(100 * N / sum(N), 1), by = c("imd_quintile")]
imd_data <- melt(imd_data, id.vars = c("imd_quintile", "smk.state"))
setnames(imd_data, "imd_quintile", "var")
imd_data[ , name := "IMD quintile"]

tot_data <- data[ , .N, by = c("smk.state")]
tot_data[ , perc := round(100 * N / sum(N), 1)]
tot_data <- melt(tot_data, id.vars = c("smk.state"))
tot_data[ , name := "Total"]
tot_data[ , var := ""]

table_data <- rbindlist(list(tot_data, age_data, sex_data, imd_data), fill = F, use.names = T)

table_data <- dcast(table_data, name + var ~ variable + smk.state, value.var = "value")

table_data <- rbindlist(list(table_data[name != 'IMD quintile'], table_data[name == 'IMD quintile']), use.names = T)

write.table(table_data, paste0(path, "outputs/smoking_prev_summary_data_england.csv"), row.names = F, sep = ",")







