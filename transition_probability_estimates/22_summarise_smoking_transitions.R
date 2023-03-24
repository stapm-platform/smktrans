
# This code is designed to work for any country

#######################################
#######################################

# plot the time trends in smoking initiation, quitting and relapse

# This will be a weighted average across ages,

# Separate trends plotted for each sex and IMD quintile


# Initiation --------

init_data <- readRDS(paste0(path, "outputs/init_forecast_data_", country, ".rds"))

init_data <- merge(init_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
init_data <- init_data[age >= min_age & age < 30 & year <= smokefree_target_year]

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

init_data <- init_data[age >= min_age & age < 30 & year <= smokefree_target_year]

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
relapse_data <- relapse_data[age >= min_age & age <= max_age & year <= smokefree_target_year]

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

relapse_data <- relapse_data[age >= min_age & age <= max_age & year <= smokefree_target_year]

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

relapse_data <- relapse_data[age >= min_age & age <= max_age & year <= smokefree_target_year]

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
quit_data <- quit_data[age >= min_age & age <= max_age & year <= smokefree_target_year]

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

quit_data <- quit_data[age >= min_age & age <= max_age & year <= smokefree_target_year]

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


# knit to powerpoint

rmarkdown::render(
  input = "transition_probability_estimates/25_smk_trans_probs_QAcheck.Rmd",
  #output_dir = "70_docs",
  output_file = paste0("smoking_transitions_", country),
  quiet = TRUE,
  params = list(path = path))



