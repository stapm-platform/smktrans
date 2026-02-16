
# Inspect the estimated probabilities
# for QA

source("03_load_packages.R")

#config <- config_eng
#config <- config_scot
config <- config_wales

###############
# Initiation
###############

ever_smoke_data <- readRDS(file.path(config$path, "outputs", paste0("ever_smoke_data_", config$country, ".rds")))

ggplot() +
  geom_point(data = ever_smoke_data$data_points, aes(x = year, y = ever_smoker, color = imd_quintile)) +
  geom_line(data = ever_smoke_data$predicted_values, aes(x = year, y = fitted_trends, color = imd_quintile), linewidth = 1.2) +
  facet_wrap(~ sex) +
  labs(x = "Year", y = "Probability of ever smoking") +
  theme_minimal() +
  scale_color_viridis_d("IMD quintile", option = "viridis", begin = 0.2, end = 0.9, labels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)")) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, 0.05))

###

init_data <- readRDS(file.path(config$path, "outputs", paste0("init_forecast_data_", config$country, ".rds")))

ggplot(init_data[year == 2014 & age <= 30]) +
   geom_line(aes(x = age, y = p_start, color = imd_quintile), linewidth = 1.2) +
   facet_wrap(~ sex) +
   labs(x = "Age", y = "Probability of Initiation") +
   theme_minimal() +
   scale_color_viridis_d("IMD quintile", option = "viridis", begin = 0.2, end = 0.9, labels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)")) +
   geom_vline(xintercept = 18, linetype = "dashed", color = "grey40", linewidth = 0.8)  +
   scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, 0.05))

###############
# Relapse
###############

relapse_data <- readRDS(file.path(config$path, "outputs", paste0("relapse_forecast_data_", config$country, ".rds")))

ggplot(relapse_data[year == 2014 & time_since_quit == 0]) +
  geom_line(aes(x = age, y = p_relapse, color = imd_quintile), linewidth = 1.2) +
  facet_wrap(~ sex) +
  labs(x = "Age", y = "Probability of Relapse") +
  theme_minimal() +
  scale_color_viridis_d("IMD quintile", option = "viridis", begin = 0.2, end = 0.9, labels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)")) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "grey40", linewidth = 0.8)  +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, 0.05))

###############
# Quitting
###############

quit_data <- readRDS(file.path(config$path, "outputs", paste0("quit_forecast_data_", config$country, ".rds")))

ggplot(quit_data[year == 2011]) +
  geom_line(aes(x = age, y = p_quit, color = imd_quintile), linewidth = 1.2) +
  facet_wrap(~ sex) +
  labs(x = "Age", y = "Probability of Quitting") +
  theme_minimal() +
  scale_color_viridis_d("IMD quintile", option = "viridis", begin = 0.2, end = 0.9, labels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)")) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "grey40", linewidth = 0.8)  +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, 0.05))

















