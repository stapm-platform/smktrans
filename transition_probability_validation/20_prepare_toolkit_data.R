

# Set parameters
#path <- "X:/HAR_PR/PR/Toolkit_TA/Data/August 2023/Latest omnibus SPSS data file/"
path <- "transition_probability_estimates/input/"
file <- "omni205_39.1_65.2cot_31.3a_25.4s_recodes_91.5sa"
#waves <- NULL
ages <- 16:89
#country <- "England"
age_categories = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")
age_cat_start_age = c(16, 18, 25, 35, 50, 65, 75)

# Read data
toolkit_data <- toolkitr::ReadToolkit(
  path_in = path,
  data_in = file,
  save = F)

setDT(toolkit_data)

# filter to keep only England
toolkit_data <- toolkit_data[gore %in% c("East of England",
                                         "South West",
                                         "South East",
                                         "North East",
                                         "East Midlands",
                                         "West Midlands",
                                         "North West",
                                         "Yorkshire and The Humber",
                                         "London")]
#nrow(toolkit_data)

# filter to select ages
toolkit_data <- toolkit_data[actage >= 16 & actage <= 89]
# > range(toolkit_data$actage, na.rm = T)
# [1]  16 113

#toolkit_data <- toolkit_data[xwave >= 75 & xwave <= 147] # 2013-2018
toolkit_data <- toolkit_data[xwave >= 148] # 2019-

#nrow(toolkit_data)

# clean data

toolkit_data[, `:=`(id, .I)]

clean_demo <- toolkitr::ToolkitCleanDemographic(data = toolkit_data, age_categories = age_categories, age_cat_start_age = age_cat_start_age)
clean_smoke <- toolkitr::ToolkitCleanSmkStatus(data = toolkit_data)
clean_smoke_con <- toolkitr::ToolkitCleanSmkCons(data = toolkit_data)
clean_smoke_pur <- toolkitr::ToolkitCleanSmkSpend(data = toolkit_data)

toolkit_data <- toolkit_data[, c("id", "xwave", "weight_gb", "Aweight0", "q632b7_1", "q632b8", "q632b9")]
setnames(toolkit_data, "Aweight0", "weight_england")

data_tk <- merge(toolkit_data, clean_demo, by = "id", sort = F, all.x = T, all.y = F)
data_tk <- merge(data_tk, clean_smoke, by = "id", sort = F, all.x = T, all.y = F)
data_tk <- merge(data_tk, clean_smoke_con, by = "id", sort = F, all.x = T, all.y = F)
data_tk <- merge(data_tk, clean_smoke_pur, by = "id", sort = F, all.x = T, all.y = F)

rm(clean_demo, clean_smoke, clean_smoke_con, clean_smoke_pur)
rm(toolkit_data)
gc()

#nrow(data_tk)


# save data
#saveRDS(data_tk, "intermediate_data/toolkit_england.rds")


# How many serious quit attempts to stop smoking made in the last 12 months - raw data	q632b7_1
#unique(data_tk$q632b7_1)
#[1]  NA   1   2   0   7   5  24   6   4   3  10   8  11  40  99  30  12  50  31  20   9  15  80  18  26  52 100  21  13 150  45 120  25  60  17 111
#[37]  33  56  55

# How long most recent serious quit attempt lasted	q632b9
#unique(data_tk$q632b9)
#[1] <NA>                                  Less than a day                       Still not smoking                    
#[4] Less than a week                      More than 1 month and up to 2 months  More than 1 week and up to a month   
#[7] More than 2 months and up to 3 months More than 6 months and up to a year   More than 3 months and up to 6 months
#[10] Don't know 

#unique(data_tk$q632b8)

#unique(data_tk$smoker_status)

data_tk[, smoker_status1 := smoker_status]
data_tk[smoker_status1 == "ex_smoker" & q632b8 %in% c("More than 6 months and up to a year",
                                                      "More than 3 months and up to 6 months",
                                                      "More than 2 months and up to 3 months",
                                                      "More than 1 month and up to 2 months",
                                                      "More than a week and up to a month",
                                                      "In the last week"), smoker_status1 := "current_smoker"]



data_tk[ , smk1_status_bin := 0]
data_tk[smoker_status1 == "current_smoker" , smk1_status_bin := 1]

data_tk[ , smk_status_bin := 0]
data_tk[smoker_status == "current_smoker" , smk_status_bin := 1]

ids <- data_tk$id
probs <- data_tk$weight_england
n <- length(ids)

domain <- data.table(age = 16:89)

k <- 10000

out_mat <- matrix(nrow = nrow(domain), ncol = k)

for(i in 1:k) {
  ids_samp <- sample(ids, n, replace = TRUE, prob = probs)
  data_tk_samp <- data_tk[ids_samp]
  
  summary_samp <- data_tk_samp[ , .(
    smkt = sum(smk_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
    smkt1 = sum(smk1_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T)
  ), by = "age"]
  
  summary_samp[ , p_quit := 1-smkt/smkt1]
  
  summary_samp[ , `:=`(smkt = NULL, smkt1 = NULL)]
  
  summary_samp <- merge(domain, summary_samp, all.x = T, all.y = F, sort = T, by = "age")
  
  summary_samp[is.na(p_quit), p_quit := 0]

  out_mat[,i] <- summary_samp$p_quit
    
}

p_quit_av <- apply(out_mat, 1, mean)
p_quit_upper <- apply(out_mat, 1, quantile, 0.975)
p_quit_lower <- apply(out_mat, 1, quantile, 0.025)

domain[ , `:=`(p_quit_av = p_quit_av, p_quit_upper = p_quit_upper, p_quit_lower = p_quit_lower)]


# test <- data_tk[ , .(
#   smkt = sum(smk_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
#   smkt1 = sum(smk1_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
#   pop = .N
# 
#   ), by = "age"]

# test <- data_tk[ , .(
#   smkt = mean(smk_status_bin, na.rm = T),
#   smkt1 = mean(smk1_status_bin, na.rm = T),
#   pop = .N
#   
# ), by = "age"]

# test[ , p_quit := 1-smkt/smkt1]
# 
# # Calculate the binomial standard error
# test[ , se := sqrt(p_quit * (1 - p_quit) / (pop * smkt))]
# test[ , upper95 := p_quit + 1.96 * se]
# test[ , lower95 := p_quit - 1.96 * se]
# 
# 
# p <- ggplot(test[p_quit>0 & age >= 16]) + geom_point(aes(x = age, y = p_quit)) #+ 
# #geom_errorbar(aes(x = age, ymin = lower95, ymax = upper95))
# 
# # p <- ggplot(test[p_quit>0]) + geom_point(aes(x = xwave, y = p_quit)) + 
# #   geom_errorbar(aes(x = xwave, ymin = lower95, ymax = upper95))
# 
# # data_tk[smoked_lastyr == "smoked", q_attempt_bin := 0]
# # #data_tk[q632b7_1 >= 1, q_attempt_bin := 1]
# # data_tk[smoked_lastyr == "smoked" & q632b8 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year"), q_attempt_bin := 1]
# # 
# # data_qa <- data_tk[ , .(qa = mean(q_attempt_bin, na.rm = T)), by = "age"]
# # #data_qa <- data_tk[smoker == "current_smoker", .(qa = mean(q_attempt_bin, na.rm = T)), by = "age"]
# # 
# # ggplot(data_qa) + geom_point(aes(x = age, y = qa)) + ylim(0, 0.5)
# # 
# # 
# # ######
# # data_tk[q632b8 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year"), q_success_bin_12m := 0]
# # 
# # #data_tk[q632b9 %in% c("Still not smoking"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("Less than a week"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("More than 1 month and up to 2 months"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("More than 2 months and up to 3 months"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("More than 3 months and up to 6 months"), q_success_bin_12m := 1]
# # data_tk[q632b8 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year") & q632b9 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year"), q_success_bin_12m := 1]
# # 
# # #[1] <NA>                                  Less than a day                       Still not smoking                    
# # #[4] Less than a week                      More than 1 month and up to 2 months  More than 1 week and up to a month   
# # #[7] More than 2 months and up to 3 months More than 6 months and up to a year   More than 3 months and up to 6 months
# # #[10] Don't know 
# # 
# # 
# # #data_qs <- data_tk[smoker == "current_smoker", .(qs = mean(q_success_bin_12m, na.rm = T)), by = "age"]
# # data_qs <- data_tk[, .(qs = mean(q_success_bin_12m, na.rm = T)), by = "age"]
# # 
# # ggplot(data_qs) + geom_point(aes(x = age, y = qs)) + ylim(0, 0.5)
# # 
# # #######
# # 
# # data_q <- merge(data_qa, data_qs, by = "age")
# # data_q[ , p_quit := qa* qs]
# # 
# # ggplot(data_q) + geom_point(aes(x = age, y = p_quit))
# 
# ###
# 
# quit_data <- readRDS("intermediate_data/quit_forecast_data_2011_2100.rds")
# 
# quit_data <- quit_data[year %in% 2013:2018]
# #quit_data <- quit_data[year %in% 2019:2023]
# 
# 
# quit_data_a <- quit_data[ , .(p_quit_ref = mean(p_quit, na.rm = T)), by = "age"]
# 
# #data_q <- merge(data_q, quit_data_a, all = T, by = "age")
# 
# #data_q <- data_q[age >= 25 & age <= 75]
# 
# 
# 
# # ggplot(data_q) + geom_point(aes(x = age, y = p_quit)) +
# #   geom_line(aes(x = age, y = p_quit_ref), col = 2)
# 
# p <- p + geom_line(data = quit_data_a, aes(x = age, y = p_quit_ref))
# p <- p + theme_minimal() + ylab("P(quit)") 
# p <- p +labs(title = "2013-2018") 
# p <- p + ylim(0, 1)
# 
# p  
# 
# 
# 
# 
# 
# # Set parameters
# #path <- "X:/HAR_PR/PR/Toolkit_TA/Data/August 2023/Latest omnibus SPSS data file/"
# path <- "input/"
# file <- "omni205_39.1_65.2cot_31.3a_25.4s_recodes_91.5sa"
# #waves <- NULL
# ages <- 16:89
# #country <- "England"
# age_categories = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")
# age_cat_start_age = c(16, 18, 25, 35, 50, 65, 75)
# 
# # Read data
# toolkit_data <- toolkitr::ReadToolkit(
#   path_in = path,
#   data_in = file,
#   save = F)
# 
# setDT(toolkit_data)
# 
# # filter to keep only England
# toolkit_data <- toolkit_data[gore %in% c("East of England",
#                                          "South West",
#                                          "South East",
#                                          "North East",
#                                          "East Midlands",
#                                          "West Midlands",
#                                          "North West",
#                                          "Yorkshire and The Humber",
#                                          "London")]
# nrow(toolkit_data)
# 
# # filter to select ages
# toolkit_data <- toolkit_data[actage >= 16 & actage <= 89]
# # > range(toolkit_data$actage, na.rm = T)
# # [1]  16 113
# 
# #toolkit_data <- toolkit_data[xwave >= 75 & xwave <= 147] # 2013-2018
# toolkit_data <- toolkit_data[xwave >= 148] # 2019-
# 
# nrow(toolkit_data)
# 
# # clean data
# 
# toolkit_data[, `:=`(id, .I)]
# 
# clean_demo <- toolkitr::ToolkitCleanDemographic(data = toolkit_data, age_categories = age_categories, age_cat_start_age = age_cat_start_age)
# clean_smoke <- toolkitr::ToolkitCleanSmkStatus(data = toolkit_data)
# clean_smoke_con <- toolkitr::ToolkitCleanSmkCons(data = toolkit_data)
# clean_smoke_pur <- toolkitr::ToolkitCleanSmkSpend(data = toolkit_data)
# 
# toolkit_data <- toolkit_data[, c("id", "xwave", "weight_gb", "Aweight0", "q632b7_1", "q632b8", "q632b9")]
# setnames(toolkit_data, "Aweight0", "weight_england")
# 
# data_tk <- merge(toolkit_data, clean_demo, by = "id", sort = F, all.x = T, all.y = F)
# data_tk <- merge(data_tk, clean_smoke, by = "id", sort = F, all.x = T, all.y = F)
# data_tk <- merge(data_tk, clean_smoke_con, by = "id", sort = F, all.x = T, all.y = F)
# data_tk <- merge(data_tk, clean_smoke_pur, by = "id", sort = F, all.x = T, all.y = F)
# 
# rm(clean_demo, clean_smoke, clean_smoke_con, clean_smoke_pur)
# rm(toolkit_data)
# gc()
# 
# nrow(data_tk)
# 
# 
# # save data
# saveRDS(data_tk, "intermediate_data/toolkit_england.rds")
# 
# 
# # How many serious quit attempts to stop smoking made in the last 12 months - raw data	q632b7_1
# unique(data_tk$q632b7_1)
# #[1]  NA   1   2   0   7   5  24   6   4   3  10   8  11  40  99  30  12  50  31  20   9  15  80  18  26  52 100  21  13 150  45 120  25  60  17 111
# #[37]  33  56  55
# 
# # How long most recent serious quit attempt lasted	q632b9
# unique(data_tk$q632b9)
# #[1] <NA>                                  Less than a day                       Still not smoking                    
# #[4] Less than a week                      More than 1 month and up to 2 months  More than 1 week and up to a month   
# #[7] More than 2 months and up to 3 months More than 6 months and up to a year   More than 3 months and up to 6 months
# #[10] Don't know 
# 
# unique(data_tk$q632b8)
# 
# unique(data_tk$smoker_status)
# 
# data_tk[, smoker_status1 := smoker_status]
# data_tk[smoker_status1 == "ex_smoker" & q632b8 %in% c("More than 6 months and up to a year",
#                                                       "More than 3 months and up to 6 months",
#                                                       "More than 2 months and up to 3 months",
#                                                       "More than 1 month and up to 2 months",
#                                                       "More than a week and up to a month",
#                                                       "In the last week"), smoker_status1 := "current_smoker"]
# 
# 
# 
# data_tk[ , smk1_status_bin := 0]
# data_tk[smoker_status1 == "current_smoker" , smk1_status_bin := 1]
# 
# data_tk[ , smk_status_bin := 0]
# data_tk[smoker_status == "current_smoker" , smk_status_bin := 1]
# 
# test <- data_tk[ , .(
#   smkt = sum(smk_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
#   smkt1 = sum(smk1_status_bin * weight_england, na.rm = T)/sum(weight_england, na.rm = T),
#   pop = .N
#   
# ), by = "age"]
# 
# # test <- data_tk[ , .(
# #   smkt = mean(smk_status_bin, na.rm = T),
# #   smkt1 = mean(smk1_status_bin, na.rm = T),
# #   pop = .N
# #   
# # ), by = "age"]
# 
# test[ , p_quit := 1-smkt/smkt1]
# 
# # Calculate the binomial standard error
# test[ , se := sqrt(p_quit * (1 - p_quit) / (pop * smkt))]
# test[ , upper95 := p_quit + 1.96 * se]
# test[ , lower95 := p_quit - 1.96 * se]
# 
# 
# p1 <- ggplot(test[p_quit>0 & age >= 16]) + geom_point(aes(x = age, y = p_quit)) #+ 
# #geom_errorbar(aes(x = age, ymin = lower95, ymax = upper95))
# 
# # p <- ggplot(test[p_quit>0]) + geom_point(aes(x = xwave, y = p_quit)) + 
# #   geom_errorbar(aes(x = xwave, ymin = lower95, ymax = upper95))
# 
# # data_tk[smoked_lastyr == "smoked", q_attempt_bin := 0]
# # #data_tk[q632b7_1 >= 1, q_attempt_bin := 1]
# # data_tk[smoked_lastyr == "smoked" & q632b8 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year"), q_attempt_bin := 1]
# # 
# # data_qa <- data_tk[ , .(qa = mean(q_attempt_bin, na.rm = T)), by = "age"]
# # #data_qa <- data_tk[smoker == "current_smoker", .(qa = mean(q_attempt_bin, na.rm = T)), by = "age"]
# # 
# # ggplot(data_qa) + geom_point(aes(x = age, y = qa)) + ylim(0, 0.5)
# # 
# # 
# # ######
# # data_tk[q632b8 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year"), q_success_bin_12m := 0]
# # 
# # #data_tk[q632b9 %in% c("Still not smoking"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("Less than a week"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("More than 1 month and up to 2 months"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("More than 2 months and up to 3 months"), q_success_bin_12m := 1]
# # #data_tk[q632b9 %in% c("More than 3 months and up to 6 months"), q_success_bin_12m := 1]
# # data_tk[q632b8 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year") & q632b9 %in% c("More than 1 month and up to 2 months", "More than 2 months and up to 3 months", "More than 3 months and up to 6 months", "More than 6 months and up to a year"), q_success_bin_12m := 1]
# # 
# # #[1] <NA>                                  Less than a day                       Still not smoking                    
# # #[4] Less than a week                      More than 1 month and up to 2 months  More than 1 week and up to a month   
# # #[7] More than 2 months and up to 3 months More than 6 months and up to a year   More than 3 months and up to 6 months
# # #[10] Don't know 
# # 
# # 
# # #data_qs <- data_tk[smoker == "current_smoker", .(qs = mean(q_success_bin_12m, na.rm = T)), by = "age"]
# # data_qs <- data_tk[, .(qs = mean(q_success_bin_12m, na.rm = T)), by = "age"]
# # 
# # ggplot(data_qs) + geom_point(aes(x = age, y = qs)) + ylim(0, 0.5)
# # 
# # #######
# # 
# # data_q <- merge(data_qa, data_qs, by = "age")
# # data_q[ , p_quit := qa* qs]
# # 
# # ggplot(data_q) + geom_point(aes(x = age, y = p_quit))
# 
# ###
# 
# quit_data <- readRDS("intermediate_data/quit_forecast_data_2011_2100.rds")
# 
# #quit_data <- quit_data[year %in% 2013:2018]
# quit_data <- quit_data[year %in% 2019:2023]
# 
# 
# quit_data_a <- quit_data[ , .(p_quit_ref = mean(p_quit, na.rm = T)), by = "age"]
# 
# #data_q <- merge(data_q, quit_data_a, all = T, by = "age")
# 
# #data_q <- data_q[age >= 25 & age <= 75]
# 
# 
# 
# # ggplot(data_q) + geom_point(aes(x = age, y = p_quit)) +
# #   geom_line(aes(x = age, y = p_quit_ref), col = 2)
# 
# p1 <- p1 + geom_line(data = quit_data_a, aes(x = age, y = p_quit_ref))
# p1 <- p1 + theme_minimal() + ylab("P(quit)") 
# p1 <- p1 +labs(title = "2019-2023") 
# p1 <- p1 + ylim(0, 1)
# 
# p1  
# 
# 
# library(cowplot)
# 
# plot_grid(p, p1, labels = c('A', 'B'))
# 


