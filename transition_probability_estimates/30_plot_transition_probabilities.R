# ==============================================================================
# Script: 30_plot_transition_probabilities.R
# Purpose: Read Excel output and plot estimates with 95% uncertainty intervals
# ==============================================================================

# 1. Load Required Libraries
library(readxl)
library(data.table)
library(ggplot2)
library(viridis)

# 2. Define File Path
# Update this to match the actual location of your Excel file
#file_path <- "transition_probability_estimates/outputs/SmokeStateTransProbs_England_2026-03-27.xlsx"
#file_path <- "transition_probability_estimates/outputs/SmokeStateTransProbs_Scotland_2026-03-27.xlsx"
file_path <- "transition_probability_estimates/outputs/SmokeStateTransProbs_Wales_2026-03-27.xlsx"

# 3. Helper Function for Plotting
# This function automatically maps the correct columns based on the metric name
plot_metric_with_ci <- function(data, metric_name, title, y_label) {
  
  # Define column names dynamically
  y_col <- paste0("p_", metric_name)
  ymin_col <- paste0("p_", metric_name, "_lower")
  ymax_col <- paste0("p_", metric_name, "_upper")
  
  # Ensure IMD is a factor for categorical coloring
  data$imd_quintile <- factor(data$imd_quintile, 
                              levels = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
                              labels = c("1 (Least Deprived)", "2", "3", "4", "5 (Most Deprived)"))
  
  # Generate Plot
  p <- ggplot(data, aes(x = age, color = imd_quintile, fill = imd_quintile)) +
    # Uncertainty Ribbon (alpha makes it transparent)
    geom_ribbon(aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]), 
                alpha = 0.2, color = NA) +
    # Central Estimate Line
    geom_line(aes(y = .data[[y_col]]), linewidth = 1) +
    # Facet by biological sex
    facet_wrap(~ sex) +
    # Formatting & Theme
    scale_color_viridis_d(option = "plasma", name = "IMD Quintile") +
    scale_fill_viridis_d(option = "plasma", name = "IMD Quintile") +
    labs(
      title = title,
      x = "Age",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#f0f0f0", color = NA),
      strip.text = element_text(face = "bold")
    )
  
  return(p)
}

# ==============================================================================
# 4. Read Data & Generate Plots
# ==============================================================================
# Choose a specific year to visualize (otherwise the plot will overlay all years)
target_year <- 2026

# --- A. INITIATION ---
cat("Plotting Initiation...\n")
dt_init <- setDT(read_excel(file_path, sheet = "Initiation"))
dt_init_sub <- dt_init[year == target_year]

p_init <- plot_metric_with_ci(
  data = dt_init_sub, 
  metric_name = "start", 
  title = paste("Probability of Smoking Initiation by Age (Year:", target_year, ")"),
  y_label = "Probability of Starting"
)
print(p_init)


# --- B. QUITTING ---
cat("Plotting Quitting...\n")
dt_quit <- setDT(read_excel(file_path, sheet = "Quitting"))
dt_quit_sub <- dt_quit[year == target_year]

p_quit <- plot_metric_with_ci(
  data = dt_quit_sub, 
  metric_name = "quit", 
  title = paste("Probability of Quitting Smoking by Age (Year:", target_year, ")"),
  y_label = "Probability of Quitting"
)
print(p_quit)


# --- C. RELAPSE ---
cat("Plotting Relapse...\n")
dt_relapse <- setDT(read_excel(file_path, sheet = "Relapse"))

# Relapse has an extra dimension: time_since_quit. 
# We need to filter for a specific year AND a specific time_since_quit (e.g., 1 year)
target_tsq <- 1 
dt_relapse_sub <- dt_relapse[year == target_year & time_since_quit == target_tsq]

p_relapse <- plot_metric_with_ci(
  data = dt_relapse_sub, 
  metric_name = "relapse", 
  title = paste("Probability of Relapse by Age (Year:", target_year, "| Years Quit:", target_tsq, ")"),
  y_label = "Probability of Relapsing"
)
print(p_relapse)

# ==============================================================================
# 5. Save Plots (Optional)
# ==============================================================================
# ggsave("Initiation_Plot.png", plot = p_init, width = 10, height = 6, dpi = 300)
# ggsave("Quitting_Plot.png", plot = p_quit, width = 10, height = 6, dpi = 300)
# ggsave("Relapse_Plot.png", plot = p_relapse, width = 10, height = 6, dpi = 300)


