
# This code is designed to work for any country

#######################################
#######################################

# plot the time trends in smoking initiation, quitting and relapse

# This will be a weighted average across ages,

# Separate trends plotted for each sex and IMD quintile


# Initiation --------

init_data <- readRDS(paste0(path, "outputs/init_forecast_data_", country, ".rds"))

init_data <- merge(init_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
init_data <- init_data[age >= min_age & age < 30 & year <= smokefree_target_year + 10]

init_data_plot <- init_data[ , .(p_start = sum(p_start * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pi <- ggplot() +
  geom_line(data = init_data_plot[year <= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 1) +
  geom_line(data = init_data_plot[year >= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(initiate)") +
  theme(axis.text.x = element_text(angle = 45)) +
  #ylim(0, 0.04) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))


png(paste0(path, "outputs/initiation_probabilities_av.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pi + labs(title = "Average initiation probability of never-smokers",
                caption = "Plot shows population weighted average probabilities of initiation over ages up to 30 years."))
dev.off()


# age specific plot

init_data <- readRDS(paste0(path, "outputs/init_forecast_data_", country, ".rds"))

init_data <- init_data[age >= min_age & age < 30 & year <= smokefree_target_year + 10]

pi1 <- ggplot() +
  geom_line(data = init_data, aes(x = age, y = p_start, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(initiate)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


png(paste0(path, "outputs/initiation_probabilities_age.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pi1 + labs(title = "Age-specific initiation probability of never-smokers",
                 caption = "Plot shows age-specific probabilities of initiation by period."))
dev.off()




# Relapse --------

relapse_data <- readRDS(paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

relapse_data <- merge(relapse_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
relapse_data <- relapse_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

relapse_data_plot <- relapse_data[ , .(p_relapse = sum(p_relapse * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pr <- ggplot() +
  geom_line(data = relapse_data_plot[year <= last_year_of_data], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 1) +
  geom_line(data = relapse_data_plot[year >= last_year_of_data], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(relapse)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

png(paste0(path, "outputs/relapse_probabilities_av.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pr + labs(title = "Average relapse probability of former-smokers",
                caption = "Plot shows population weighted average probabilities of relapse over age."))
dev.off()


# age specific plot

relapse_data <- readRDS(paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

relapse_data <- relapse_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

relapse_data_plot <- relapse_data[ , .(p_relapse = mean(p_relapse)), by = c("year", "age", "sex", "imd_quintile")]

pr1 <- ggplot() +
  geom_line(data = relapse_data_plot, aes(x = age, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(relapse)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


png(paste0(path, "outputs/relapse_probabilities_age.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pr1 + labs(title = "Age-specific relapse probability of former-smokers",
                 caption = "Plot shows age-specific probabilities of relapse by period."))
dev.off()


# time since quit plot

relapse_data <- readRDS(paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

relapse_data <- relapse_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

relapse_data_plot <- relapse_data[ , .(p_relapse = mean(p_relapse)), by = c("year", "time_since_quit", "sex", "imd_quintile")]

pr2 <- ggplot() +
  geom_line(data = relapse_data_plot, aes(x = time_since_quit, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(relapse)") +
  xlab("years since quitting") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


png(paste0(path, "outputs/relapse_probabilities_timesincequit.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pr2 + labs(title = "Relapse probability by time since quitting of former-smokers",
                 caption = "Plot shows probabilities of relapse by years since quitting by period."))
dev.off()






# Quitting --------

quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

quit_data <- merge(quit_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
quit_data <- quit_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

quit_data_plot <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pq <- ggplot() +
  geom_line(data = quit_data_plot[year <= last_year_of_data], aes(x = year, y = p_quit, colour = imd_quintile), linetype = 1) +
  geom_line(data = quit_data_plot[year >= last_year_of_data], aes(x = year, y = p_quit, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(quit)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

png(paste0(path, "outputs/quitting_probabilities_av.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pq + labs(title = "Average quitting probability of current-smokers",
                caption = "Plot shows population weighted average probabilities of quiting over age."))
dev.off()



# age specific plot

quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

quit_data <- quit_data[age >= min_age & age <= max_age & year <= smokefree_target_year + 10]

quit_data_plot <- quit_data[ , .(p_quit = mean(p_quit)), by = c("year", "age", "sex", "imd_quintile")]

pq1 <- ggplot() +
  geom_line(data = quit_data_plot, aes(x = age, y = p_quit, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(quit)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


png(paste0(path, "outputs/quit_probabilities_age.png"), units="in", width=14/1.5, height=6.5/1.5, res=600)
print(pq1 + labs(title = "Age-specific quitting probability of current-smokers",
                 caption = "Plot shows age-specific probabilities of quitting by period."))
dev.off()

#######################################

# age plots for paper

init_data <- readRDS(paste0(path, "outputs/init_data_", country, "_uncertainty.rds"))

init_data <- init_data[age >= min_age & age < 30 & year == 2024]

init_data <- init_data[ , .(p_start = mean(p_start), p_start_low = mean(p_start_low), p_start_high = mean(p_start_high)), by = c("age")]

pi_paper <- ggplot() +
  geom_line(data = init_data, aes(x = age, y = p_start), linewidth = .4) +
  geom_ribbon(data = init_data, aes(x = age, ymin = p_start_low, ymax = p_start_high), alpha = .15) +
  theme_minimal() +
  ylab("P(initiate)")


# net initiation probabilities
#init_data <- merge(init_data, quit_data, by = "age")
#init_data <- merge(init_data, relapse_data, by = "age")
#write.csv(init_data, "transition_probability_estimates/outputs/check3.csv", row.names = F)

# values from excel formula
check_data <- fread("transition_probability_estimates/outputs/check3_netvals.csv")

#check_data <- check_data[ , .(age, p_start, p_start_net)]

#check_data <- melt(check_data, id.vars = "age", variable.name = "type", value.name = "p_start")
pi_paper <- pi_paper + geom_line(data = check_data, aes(x = age, y = p_start_net), linetype = 2, linewidth = .6)# +
# annotate("text", x = 13, y = 0.02, 
#          label = c("Net initiation\nprobability"),
#          size=3 , angle=0) +
# geom_segment(aes(x = 14.75, y = 0.02, xend = 16, yend = 0.0075),
#              arrow = arrow(length = unit(0.2, "cm")))
pi_paper

# relapse_data <- readRDS(paste0(path, "outputs/relapse_data_", country, "_uncertainty.rds"))
# 
# relapse_data <- relapse_data[age >= min_age & age <= max_age & year == 2017]
# 
# pr_paper <- ggplot() +
#   geom_line(data = relapse_data, aes(x = age, y = p_relapse, group = time_since_quit)) +
#   theme_minimal() +
#   ylab("P(relapse)") +
#   facet_wrap(~ sex + imd_quintile, nrow = 2)


relapse_data <- readRDS(paste0(path, "outputs/relapse_data_", country, "_uncertainty.rds"))

relapse_data <- relapse_data[age >= min_age & age <= max_age & year == 2024]

relapse_data <- relapse_data[ , .(p_relapse = mean(p_relapse), p_relapse_low = mean(p_relapse_low), p_relapse_high = mean(p_relapse_high)), by = c("age")]

pr_paper <- ggplot() +
  geom_line(data = relapse_data, aes(x = age, y = p_relapse), linewidth = .4) +
  geom_ribbon(data = relapse_data, aes(x = age, ymin = p_relapse_low, ymax = p_relapse_high), alpha = .15) +
  theme_minimal() +
  ylab("P(relapse)")


relapse_data <- readRDS(paste0(path, "outputs/relapse_data_", country, "_uncertainty.rds"))

relapse_data <- relapse_data[age >= min_age & age <= max_age & year == 2024]

relapse_data <- relapse_data[ , .(p_relapse = mean(p_relapse), p_relapse_low = mean(p_relapse_low), p_relapse_high = mean(p_relapse_high)), by = c("time_since_quit")]

pr_paper2 <- ggplot() +
  geom_line(data = relapse_data, aes(x = time_since_quit, y = p_relapse), linewidth = .4) +
  geom_ribbon(data = relapse_data, aes(x = time_since_quit, ymin = p_relapse_low, ymax = p_relapse_high), alpha = .15) +
  theme_minimal() +
  ylab("P(relapse)") + xlab("Years since quitting")



# run "20_prepare_toolkit_data.R"

quit_data <- readRDS(paste0(path, "outputs/quit_data_", country, "_uncertainty.rds"))

quit_data <- quit_data[age >= min_age & age <= max_age & year == 2024]

quit_data <- quit_data[ , .(p_quit = mean(p_quit), p_quit_low = mean(p_quit_low), p_quit_high = mean(p_quit_high)), by = c("age")]

pq_paper <- ggplot() +
  geom_line(data = quit_data, aes(x = age, y = p_quit), linewidth = .4) +
  geom_ribbon(data = quit_data, aes(x = age, ymin = p_quit_low, ymax = p_quit_high), alpha = .125) +
  theme_minimal() +
  ylab("P(quit)")# +
  #geom_point(data = domain[p_quit_av>0], aes(x = age, y = p_quit_av), alpha = 0.2, size = .7) + 
  #geom_errorbar(data = domain[p_quit_av>0], aes(x = age, ymin = p_quit_lower, ymax = p_quit_upper), alpha = 0.25, width = 0) 


# no init
quit_data_no_init <- readRDS(paste0(path, "outputs/quit_forecast_data_no_init_", country, ".rds"))

quit_data_no_init <- quit_data_no_init[age >= min_age & age < 30 & year == 2024]

quit_data_no_init <- quit_data_no_init[ , .(p_quit = mean(p_quit_no_init)), by = c("age")]

pq_paper <- pq_paper + geom_line(data = quit_data_no_init, aes(x = age, y = p_quit), linewidth = .6, linetype = 2)

pq_paper

# 
# pi_paper_net <- ggplot(check_data) +
#   geom_line(aes(x = age, y = p_start, linetype = type), linewidth = .4) +
#   theme_minimal() +
#   ylab("P(initiate)")

# knit to powerpoint

pp <- plot_grid(pi_paper + xlim(11, 89), 
                pq_paper + xlim(11, 89),
                pr_paper + xlim(11, 89),
                pr_paper2
                
                , labels = "AUTO", ncol = 2, nrow = 2)

pp

png(paste0(path, "outputs/combined_probabilities_age.png"), units="in", width=10/1.2, height=6/1.2, res=800)
print(pp)
dev.off()


######################################

# time trend plots for paper

init_data <- readRDS(paste0(path, "outputs/init_forecast_data_", country, ".rds"))

init_data <- merge(init_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
init_data <- init_data[age >= 12 & age < 30 & year <= smokefree_target_year + 10]

init_data_plot <- init_data[ , .(p_start = sum(p_start * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pi <- ggplot() +
  geom_line(data = init_data_plot[year <= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 1) +
  geom_line(data = init_data_plot[year >= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(initiate)") + ylim(0, 0.07) +
  theme(axis.text.x = element_text(angle = 45)) +
  #ylim(0, 0.04) +
  scale_colour_manual("IMD quintile", labels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"), values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))


quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

quit_data <- merge(quit_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
quit_data <- quit_data[age >= 12 & age <= 89 & year <= smokefree_target_year + 10]

quit_data_plot <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pq <- ggplot() +
  geom_line(data = quit_data_plot[year <= last_year_of_data], aes(x = year, y = p_quit, colour = imd_quintile), linetype = 1) +
  geom_line(data = quit_data_plot[year >= last_year_of_data], aes(x = year, y = p_quit, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(quit)") + ylim(0, 0.3) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))


relapse_data <- readRDS(paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

relapse_data <- merge(relapse_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
relapse_data <- relapse_data[age >= 12 & age <= 89 & year <= smokefree_target_year + 10]

relapse_data_plot <- relapse_data[ , .(p_relapse = sum(p_relapse * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

pr <- ggplot() +
  geom_line(data = relapse_data_plot[year <= last_year_of_data], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 1) +
  geom_line(data = relapse_data_plot[year >= last_year_of_data], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(relapse)")  + ylim(0, 0.07) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))


pp <- plot_grid(
  pi + theme(legend.position="none", axis.text.x = element_blank(), axis.title.x = element_blank()),
  pq + theme(legend.position="none", axis.text.x = element_blank(), axis.title.x = element_blank(), strip.text.x = element_blank()),
  pr + theme(legend.position="none", strip.text.x = element_blank()),
  align = "v",
  labels = c("A", "B", "C"), nrow = 3, ncol = 1, hjust = -1, rel_heights = c(1, 1, 1.2))

legend <- get_legend(
  pi + theme(legend.box.margin = margin(0, 0, 0, 11))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
pp <- plot_grid(pp, legend, rel_widths = c(3, 1))


png(paste0(path, "outputs/combined_probabilities_year.png"), units="in", width=10/1.5, height=9/1.5, res=800)
print(pp)
dev.off()

#####################################

# Table 2 for paper

init_data <- readRDS(paste0(path, "outputs/init_data_", country, "_uncertainty.rds"))

#init_data <- init_data[sex == "Male" & age >= 12 & age < 18 & year == 2024]
init_data <- init_data[age >= 12 & age < 25 & year == 2024 & imd_quintile == "5_most_deprived"]

init_data <- merge(init_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

init_data[ , .(p_start = sum(p_start * N) / sum(N), 
               p_start_low = sum(p_start_low * N) / sum(N), 
               p_start_high = sum(p_start_high * N) / sum(N))]


# probability of quitting
quit_data <- readRDS(paste0(path, "outputs/quit_data_", country, "_uncertainty.rds"))

#quit_data <- quit_data[sex == "Male" & age >= 50 & age < 90 & year == 2024]
quit_data <- quit_data[age >= 25 & age < 74 & year >= 2011 & year <= 2019]

quit_data[ , age_cat := c("25-50", "50-74")[findInterval(age, c(0, 50, 75))]]
quit_data[ , year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(2011, 2014, 2017))]]

quit_data <- merge(quit_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

quit_summary <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N), 
               p_quit_low = sum(p_quit_low * N) / sum(N), 
               p_quit_high = sum(p_quit_high * N) / sum(N),
               p_quit_var = sum(p_quit_var * N) / sum(N)
               ), by = c("sex", "age_cat")]

write.csv(quit_summary, paste0(path, "outputs/quit_summary_", country, "_age_sex.csv"))


quit_summary <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N), 
                               p_quit_low = sum(p_quit_low * N) / sum(N), 
                               p_quit_high = sum(p_quit_high * N) / sum(N),
                               p_quit_var = sum(p_quit_var * N) / sum(N)
), by = c("year_cat", "imd_quintile")]

write.csv(quit_summary, paste0(path, "outputs/quit_summary_", country, "_year_imd.csv"))




# need to add variance to the uncertainty sampling output



relapse_data <- readRDS(paste0(path, "outputs/relapse_data_", country, "_uncertainty.rds"))

#relapse_data <- relapse_data[sex == "Male" & age >= 50 & age < 90 & year == 2024]
relapse_data <- relapse_data[age >= 12 & age < 90 & year == 2024 & imd_quintile == "5_most_deprived"]

relapse_data <- merge(relapse_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

relapse_data[ , .(p_relapse = sum(p_relapse * N) / sum(N), 
                  p_relapse_low = sum(p_relapse_low * N) / sum(N), 
                  p_relapse_high = sum(p_relapse_high * N) / sum(N))]

###########################################
# net probability of initiation - figure 3

init_data <- readRDS(paste0(path, "outputs/init_data_", country, "_uncertainty.rds"))

init_data <- init_data[age >= 12 & age < 30 & year %in% 2019:2023 & sex == "Male" & imd_quintile == "5_most_deprived"]

init_data <- merge(init_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

init1 <- init_data[ , .(p_start = sum(p_start * N) / sum(N), 
               p_start_low = sum(p_start_low * N) / sum(N), 
               p_start_high = sum(p_start_high * N) / sum(N)), by = "age"]

write.csv(init1[age %in% 11:30], "transition_probability_estimates/outputs/init_temp.csv")

quit_data <- readRDS(paste0(path, "outputs/quit_data_", country, "_uncertainty.rds"))

quit_data <- quit_data[age >= 12 & age <= 89 & year %in% 2019:2023  & sex == "Male" & imd_quintile == "5_most_deprived"]

quit_data <- merge(quit_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

quit1 <- quit_data[ , .(p_quit = sum(p_quit * N) / sum(N), 
               p_quit_low = sum(p_quit_low * N) / sum(N), 
               p_quit_high = sum(p_quit_high * N) / sum(N)), by = "age"]

write.csv(quit1[age %in% 11:29], "transition_probability_estimates/outputs/quit_temp.csv")


relapse_data <- readRDS(paste0(path, "outputs/relapse_data_", country, "_uncertainty.rds"))

relapse_data <- relapse_data[age >= 12 & age <= 89 & year %in% 2019:2023  & sex == "Male" & imd_quintile == "5_most_deprived"]

relapse_data <- merge(relapse_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))

relapse1 <- relapse_data[ , .(p_relapse = sum(p_relapse * N) / sum(N), 
                  p_relapse_low = sum(p_relapse_low * N) / sum(N), 
                  p_relapse_high = sum(p_relapse_high * N) / sum(N)), by = "age"]

write.csv(relapse1[age %in% 11:29], "transition_probability_estimates/outputs/relapse_temp.csv")



pops_data <- pops[age >= 12 & age <= 29]

pops1 <- pops_data[ , .(sum(N)), by = "age"]

write.csv(pops1, "transition_probability_estimates/outputs/pops_temp.csv")


data_n <- fread("transition_probability_estimates/outputs/net_probs_for_plot.csv")


pi <- ggplot(data_n) +
  #geom_point(aes(x = age, y = value, colour = imd_quintile, shape = sex)) +
  geom_line(aes(x = age, y = value, colour = imd_quintile, linetype = sex), linewidth = 1) +
  theme_minimal() +
  ylab("Net initiation probability") +
  geom_vline(xintercept = 18) + 
  scale_colour_manual("IMD quintile", labels = c("1 (least deprived)", "5 (most deprived)"), values = c("#fcc5c0", "#7a0177"))

png(paste0(path, "outputs/net_initiation.png"), units="in", width=6, height=3, res=800)
print(pi)
dev.off()



#####################################

rmarkdown::render(
  input = "transition_probability_estimates/25_smk_trans_probs_QAcheck.Rmd",
  #output_dir = "70_docs",
  output_file = paste0("smoking_transitions_", country),
  quiet = TRUE,
  params = list(path = path))



