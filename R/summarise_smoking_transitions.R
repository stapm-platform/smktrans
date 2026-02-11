#' Summarise and Plot Smoking Transition Probabilities
#'
#' @description
#' Generates summary plots and tables for the estimated transition probabilities.
#' It creates:
#' 1. Population-weighted time trends (Initiation, Relapse, Quit).
#' 2. Age-specific profiles (with uncertainty ribbons).
#' 3. Combined panels for publication.
#' 4. Summary CSV tables for specific subgroups.
#'
#' @param config Named list. Must contain:
#'   \code{country}, \code{path} (root dir), \code{first_year}, \code{last_year}.
#' @param pops Data.table. Population counts (Age/Sex/Year/IMD) for weighting.
#'
#' @import data.table
#' @import ggplot2
#' @import cowplot
#' @import viridis
#' @export
summarise_smoking_transitions <- function(config, pops) {
  
  # -- Setup Paths & Constants --
  out_dir <- file.path(config$path, "outputs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Colors for IMD Quintiles
  imd_cols <- c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")
  names(imd_cols) <- c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  
  # Helper: Load and Weight Data
  # Loads .rds, merges population, filters age/year
  get_data <- function(type, is_uncertainty = FALSE) {
    
    file_suffix <- if(is_uncertainty) "_uncertainty.rds" else paste0("_forecast_data_", config$country, ".rds")
    file_name <- paste0(type, if(type == "init") "" else "_data", file_suffix)
    
    dt <- readRDS(file.path(out_dir, file_name))
    
    # Merge Population for weighting
    # Note: Using inner join logic for years where we have pop data
    dt <- merge(dt, pops, by = c("age", "sex", "imd_quintile"), all.x = TRUE)
    
    # Standard Filter
    dt <- dt[year <= config$last_year + 10]
    return(dt)
  }
  
  # Helper: Calculate Weighted Trends
  calc_weighted_trend <- function(dt, var_col) {
    # Weighted average by Year, Sex, IMD
    dt[, .(val = sum(get(var_col) * N, na.rm=TRUE) / sum(N, na.rm=TRUE)), 
       by = .(year, sex, imd_quintile)]
  }
  
  # Helper: Standard Trend Plot
  plot_trend <- function(dt, title, y_lab, y_limit = NULL) {
    p <- ggplot(dt) +
      geom_line(aes(x = year, y = val, colour = imd_quintile, linetype = year > config$last_year)) +
      facet_wrap(~ sex) +
      scale_linetype_manual(values = c(1, 2), guide = "none") +
      scale_colour_manual("IMD Quintile", values = imd_cols) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = title, y = y_lab, x = "Year")
    
    if (!is.null(y_limit)) p <- p + coord_cartesian(ylim = y_limit)
    return(p)
  }
  
  # Helper: Standard Age Plot (with Uncertainty Ribbon)
  plot_age_uncertainty <- function(dt, var_col, title, y_lab, x_lab = "Age") {
    # Aggregate over sex/imd for the paper plot (or as specified)
    # Using 'mean' of bounds for the aggregate ribbon is an approximation but matches original script intent
    agg <- dt[, .(
      mean = mean(get(var_col), na.rm=TRUE),
      low = mean(get(paste0(var_col, "_low")), na.rm=TRUE),
      high = mean(get(paste0(var_col, "_high")), na.rm=TRUE)
    ), by = .(x_var = get(if(x_lab == "Age") "age" else "time_since_quit"))]
    
    ggplot(agg, aes(x = x_var)) +
      geom_line(aes(y = mean), linewidth = 0.6) +
      geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) +
      theme_minimal() +
      labs(y = y_lab, x = x_lab, title = title)
  }
  
  message(">> Generating Summaries for: ", config$country)
  
  # =========================================================================
  # 1. INITIATION
  # =========================================================================
  message("   Processing Initiation...")
  
  # A. Trends
  init_raw <- get_data("init")[age >= config$min_age & age < 30] # Filter for young adults
  init_trend <- calc_weighted_trend(init_raw, "p_start")
  
  p_init_trend <- plot_trend(init_trend, "Avg Initiation Probability (Age < 30)", "P(initiate)")
  ggsave(file.path(out_dir, "initiation_probabilities_av.png"), p_init_trend, width=10, height=6, bg="white")
  
  # B. Paper Plot (Uncertainty @ 2024)
  init_unc <- get_data("init", is_uncertainty = TRUE)[year == 2024 & age >= config$min_age & age < 30]
  p_init_paper <- plot_age_uncertainty(init_unc, "p_start", "", "P(initiate)")
  
  
  # =========================================================================
  # 2. RELAPSE
  # =========================================================================
  message("   Processing Relapse...")
  
  # A. Trends
  rel_raw <- get_data("relapse")[age >= config$min_age & age <= config$max_age]
  rel_trend <- calc_weighted_trend(rel_raw, "p_relapse")
  
  p_rel_trend <- plot_trend(rel_trend, "Avg Relapse Probability", "P(relapse)")
  ggsave(file.path(out_dir, "relapse_probabilities_av.png"), p_rel_trend, width=10, height=6, bg="white")
  
  # B. Time Since Quit (Unique to Relapse)
  # Aggregating over year/sex/imd to show the TSQ curve
  rel_tsq <- rel_raw[, .(p_relapse = mean(p_relapse)), by = .(time_since_quit, year)]
  p_rel_tsq <- ggplot(rel_tsq, aes(x = time_since_quit, y = p_relapse, group = year, color = year)) +
    geom_line(alpha = 0.5) + scale_color_viridis_c() + theme_minimal() +
    labs(title = "Relapse by Time Since Quit", x = "Years since quitting", y = "P(relapse)")
  ggsave(file.path(out_dir, "relapse_probabilities_timesincequit.png"), p_rel_tsq, width=10, height=6, bg="white")
  
  # C. Paper Plots (Uncertainty @ 2024)
  rel_unc <- get_data("relapse", is_uncertainty = TRUE)[year == 2024]
  p_rel_paper_age <- plot_age_uncertainty(rel_unc, "p_relapse", "", "P(relapse)")
  p_rel_paper_tsq <- plot_age_uncertainty(rel_unc, "p_relapse", "", "P(relapse)", x_lab = "Time Since Quit")
  
  
  # =========================================================================
  # 3. QUITTING
  # =========================================================================
  message("   Processing Quitting...")
  
  # A. Trends
  quit_raw <- get_data("quit")[age >= config$min_age & age <= config$max_age]
  quit_trend <- calc_weighted_trend(quit_raw, "p_quit")
  
  p_quit_trend <- plot_trend(quit_trend, "Avg Quit Probability", "P(quit)")
  ggsave(file.path(out_dir, "quitting_probabilities_av.png"), p_quit_trend, width=10, height=6, bg="white")
  
  # B. Paper Plot (Uncertainty @ 2024)
  quit_unc <- get_data("quit", is_uncertainty = TRUE)[year == 2024]
  p_quit_paper <- plot_age_uncertainty(quit_unc, "p_quit", "", "P(quit)")
  
  # Add "No Init" comparison line if file exists (Legacy support)
  no_init_path <- file.path(out_dir, paste0("quit_forecast_data_no_init_", config$country, ".rds"))
  if (file.exists(no_init_path)) {
    q_no_init <- readRDS(no_init_path)[year == 2024, .(p = mean(p_quit_no_init)), by = age]
    p_quit_paper <- p_quit_paper + 
      geom_line(data = q_no_init, aes(x = age, y = p), linetype = "dashed", linewidth = 0.6)
  }
  
  
  # =========================================================================
  # 4. COMPOSITE PLOTS & OUTPUTS
  # =========================================================================
  message("   Generating Composite Figures...")
  
  # Figure: Age Profiles (Paper)
  # Grid: Initiation | Quit | Relapse(Age) | Relapse(TSQ)
  p_grid_age <- plot_grid(
    p_init_paper + xlim(11, 89), 
    p_quit_paper + xlim(11, 89),
    p_rel_paper_age + xlim(11, 89),
    p_rel_paper_tsq,
    labels = "AUTO", ncol = 2
  )
  ggsave(file.path(out_dir, "combined_probabilities_age.png"), p_grid_age, width=10, height=6, dpi=600, bg="white")
  
  # Figure: Time Trends (Paper)
  # Vertical stack: Init / Quit / Relapse
  # Remove X axis from top plots to save space
  p_trend_stack <- plot_grid(
    p_init_trend + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()),
    p_quit_trend + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()),
    p_rel_trend + theme(legend.position="none"),
    ncol = 1, align = "v", rel_heights = c(1, 1, 1.3), labels = "AUTO"
  )
  # Add shared legend
  legend <- get_legend(p_init_trend + theme(legend.box.margin = margin(0,0,0,12)))
  p_final_trend <- plot_grid(p_trend_stack, legend, rel_widths = c(3, .5))
  
  ggsave(file.path(out_dir, "combined_probabilities_year.png"), p_final_trend, width=10, height=9, dpi=600, bg="white")
  
  
  # =========================================================================
  # 5. SUMMARY TABLES
  # =========================================================================
  message("   Writing Summary Tables...")
  
  # Table: Quit Rates by Age/Sex
  quit_summ_age <- quit_unc[age %in% 25:74, .(
    p_quit = weighted.mean(p_quit, N),
    low    = weighted.mean(p_quit_low, N),
    high   = weighted.mean(p_quit_high, N)
  ), by = .(sex, age_cat = cut(age, c(25, 50, 75), labels = c("25-50", "50-74")))]
  
  fwrite(quit_summ_age, file.path(out_dir, paste0("quit_summary_", config$country, "_age_sex.csv")))
  
  # Table: Quit Rates by IMD/Year Block
  quit_summ_imd <- quit_unc[, .(
    p_quit = weighted.mean(p_quit, N)
  ), by = .(imd_quintile, year_cat = cut(year, breaks=c(2010, 2013, 2016, 2019)))]
  
  fwrite(quit_summ_imd, file.path(out_dir, paste0("quit_summary_", config$country, "_year_imd.csv")))
  
  
  # =========================================================================
  # 6. REPORT GENERATION
  # =========================================================================
  # Optional: Render QA RMarkdown if it exists
  qa_rmd <- file.path(config$path, "25_smk_trans_probs_QAcheck.Rmd")
  if (file.exists(qa_rmd)) {
    message("   Rendering QA Report...")
    rmarkdown::render(
      input = qa_rmd,
      output_file = paste0("smoking_transitions_", config$country),
      quiet = TRUE,
      params = list(path = config$path)
    )
  }
  
  message(">> Summary Complete.")
}