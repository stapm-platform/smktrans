#' Write Transition Probability Estimates to Excel
#' 
#' @description Generates a multi-sheet Excel workbook with a professional cover sheet 
#' including detailed methodological metadata for reproducibility.
#' @param config Configuration list containing metadata and model parameters.
#' @param init_res Initiation results list/dataframe.
#' @param quit_res Quitting results list/dataframe.
#' @param relapse_res Relapse results list/dataframe.
#' @param net_init_dt Optional net initiation table (can be NULL).
#' @param quit_no_init Optional quitting without initiation table (can be NULL).
#' 
#' @import openxlsx
#' @importFrom utils packageVersion
#' @export
write_excel_report <- function(config, init_res, quit_res, relapse_res, 
                               net_init_dt = NULL, quit_no_init = NULL) {
  
  # 1. Initialize Workbook
  wb <- createWorkbook()
  
  # =========================================================================
  # STYLES DEFINITION
  # =========================================================================
  brand_blue <- "#005A8F" 
  
  style_title <- createStyle(fontName = "Arial", fontSize = 16, textDecoration = "bold", 
                             fontColour = brand_blue)
  
  style_affil <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#555555", 
                             wrapText = TRUE)
  
  style_contact <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#000000", 
                               textDecoration = "bold", wrapText = TRUE)
  
  # Professional citation box style
  style_cite_box <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#000000",
                                fgFill = "#F2F2F2", border = "LeftRightTopBottom", 
                                borderColour = "#CCCCCC", wrapText = TRUE, valign = "top")
  
  style_cite_header <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#000000",
                                   textDecoration = "bold", fgFill = "#F2F2F2", wrapText = TRUE)
  
  style_header <- createStyle(fontName = "Arial", textDecoration = "bold", fontColour = "white", 
                              fgFill = brand_blue, halign = "left", valign = "center",
                              border = "TopBottom", borderColour = brand_blue)
  
  style_col_header <- createStyle(fontName = "Arial", textDecoration = "bold", fontColour = "black", 
                                  fgFill = "#D9E1F2", halign = "center", border = "Bottom")
  
  style_text <- createStyle(fontName = "Arial", fontSize = 10, wrapText = TRUE, valign = "top")
  
  style_num <- createStyle(numFmt = "0.0000")
  
  # =========================================================================
  # 2. COVER SHEET CONSTRUCTION
  # =========================================================================
  sheet_name <- "Cover Sheet"
  addWorksheet(wb, sheet_name)
  showGridLines(wb, sheet_name, showGridLines = FALSE)
  
  # --- A. Title, Date & Package Version ---
  # Dynamic Package Version
  pkg_ver <- tryCatch(as.character(utils::packageVersion("smktrans")), error = function(e) "2.0.0")
  run_date <- format(Sys.Date(), "%d %B %Y")
  current_year <- format(Sys.Date(), "%Y")
  
  title_text <- paste0("Smoking Transition Probability Estimates (v", pkg_ver, ")")
  writeData(wb, sheet_name, title_text, startRow = 1, startCol = 1)
  addStyle(wb, sheet_name, style_title, rows = 1, cols = 1)
  
  date_text <- paste0("Report Generated: ", run_date)
  writeData(wb, sheet_name, date_text, startRow = 2, startCol = 1)
  addStyle(wb, sheet_name, style_affil, rows = 2, cols = 1)
  
  # --- B. Affiliation & Contact ---
  affiliation_text <- paste(
    "Sheffield Addictions Research Group (SARG)",
    "Sheffield Centre for Health and Related Research (SCHARR)",
    "School of Medicine and Population Health, The University of Sheffield",
    "Western Bank, Sheffield, S10 2TN, United Kingdom",
    sep = "\n"
  )
  writeData(wb, sheet_name, affiliation_text, startRow = 4, startCol = 1)
  addStyle(wb, sheet_name, style_affil, rows = 4, cols = 1)
  setRowHeights(wb, sheet_name, rows = 4, heights = 60)
  
  contact_text <- "Contact: Dr Duncan Gillespie (duncan.gillespie@sheffield.ac.uk)"
  writeData(wb, sheet_name, contact_text, startRow = 5, startCol = 1)
  addStyle(wb, sheet_name, style_contact, rows = 5, cols = 1)
  
  # --- C. HOW TO CITE (Expanded) ---
  row_idx <- 7
  writeData(wb, sheet_name, "1. How to Cite", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  row_idx <- row_idx + 1
  
  # Helper to write citation rows cleanly
  write_cite_row <- function(wb, sheet, row, text, height = 30, style = style_cite_box) {
    writeData(wb, sheet, text, startRow = row, startCol = 1)
    mergeCells(wb, sheet, cols = 1:4, rows = row)
    addStyle(wb, sheet, style, rows = row, cols = 1)
    setRowHeights(wb, sheet, rows = row, heights = height)
  }
  
  # Intro Text
  intro_text <- "If you use the smktrans estimates in your research, please cite our peer-reviewed modelling papers. These publications validate the use of these estimates in policy appraisal contexts:"
  write_cite_row(wb, sheet_name, row_idx, intro_text, height = 30, style = style_cite_header)
  
  # Paper 1
  row_idx <- row_idx + 1
  paper1 <- "Chen RKL, Morris D, Angus C, Gilmore A, Hiscock R, Holmes J, Langley TE, Pryce R, Wilson LB, Brennan A, Gillespie D (2026). Reducing the exceptional affordability of hand-rolling tobacco using tax escalators: a health and economic impact modelling study for England. Tobacco Control. DOI: 10.1136/tc-2025-059670"
  write_cite_row(wb, sheet_name, row_idx, paper1, height = 45)
  
  # Paper 2
  row_idx <- row_idx + 1
  paper2 <- "Gillespie D, Morris D, Angus C, Wilson L, Chen RKL, Leeming G, Holmes J, Brennan A (2025). Model-based appraisal of the potential effects of minimum pricing for tobacco in Scotland. Tobacco Control. DOI: 10.1136/tc-2024-059252"
  write_cite_row(wb, sheet_name, row_idx, paper2, height = 30)
  
  # Software Citation (Dynamic)
  row_idx <- row_idx + 1
  soft_intro <- "To cite the smktrans software package specifically:"
  write_cite_row(wb, sheet_name, row_idx, soft_intro, height = 15, style = style_cite_header)
  
  row_idx <- row_idx + 1
  soft_cite <- paste0("Gillespie, D., and Brennan, A. (", current_year, "). smktrans: An R Package for estimating smoking state transition probabilities (v", pkg_ver, "). University of Sheffield. https://doi.org/10.17605/OSF.IO/YGXQ9")
  write_cite_row(wb, sheet_name, row_idx, soft_cite, height = 30)
  
  # Tech Doc Citation
  row_idx <- row_idx + 1
  tech_intro <- "To cite the full technical documentation for the underlying model:"
  write_cite_row(wb, sheet_name, row_idx, tech_intro, height = 15, style = style_cite_header)
  
  row_idx <- row_idx + 1
  tech_cite <- "Gillespie, D. & Brennan, A. (Year). The Sheffield Tobacco Policy Model (STPM): full technical documentation. Documentation version number [x.x.x]. University of Sheffield. DOI: 10.17605/OSF.IO/FR7WN"
  write_cite_row(wb, sheet_name, row_idx, tech_cite, height = 30)
  
  # Note
  row_idx <- row_idx + 1
  note_cite <- "(Note: The technical documentation is a living document. Please cite the year and version of the report you used.)"
  write_cite_row(wb, sheet_name, row_idx, note_cite, height = 15)
  
  
  # --- D. About This Dataset ---
  row_idx <- row_idx + 2
  writeData(wb, sheet_name, "2. About this Dataset", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  desc_text <- paste(
    "This workbook contains estimated annual probabilities for smoking state transitions used in the STAPM microsimulation model.",
    "Estimates are derived using methods in the R package 'smktrans' fitted to repeat annual cross-sectional survey data.",
    "These probabilities represent the likelihood of an individual changing their smoking status within a one-year period.",
    sep = " "
  )
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, desc_text, startRow = row_idx, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:4, rows = row_idx) 
  addStyle(wb, sheet_name, style_text, rows = row_idx, cols = 1)
  setRowHeights(wb, sheet_name, rows = row_idx, heights = 60)
  
  # --- E. Sheet Guide ---
  row_idx <- row_idx + 2
  writeData(wb, sheet_name, "3. Worksheet Guide", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  # Build Guide DF dynamically
  guide_df <- data.frame(
    Worksheet = c("Initiation", "Quitting", "Relapse"),
    Description = c(
      "Probability of a 'Never Smoker' becoming a 'Current Smoker' (Start).",
      "Probability of a 'Current Smoker' becoming a 'Former Smoker' (Quit).",
      "Probability of a 'Former Smoker' becoming a 'Current Smoker' (Relapse), dependent on time since quitting."
    )
  )
  
  if (!is.null(net_init_dt)) {
    guide_df <- rbind(guide_df, data.frame(Worksheet = "Net Initiation", Description = "Net initiation probabilities (Initiation - Quitting)."))
  }
  if (!is.null(quit_no_init)) {
    guide_df <- rbind(guide_df, data.frame(Worksheet = "Quit (No Init)", Description = "Quitting probabilities calculated without initiation adjustment."))
  }
  
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, guide_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  addStyle(wb, sheet_name, style_text, rows = (row_idx+1):(row_idx+nrow(guide_df)), cols = 2)
  
  # --- F. Variable Definitions ---
  row_idx <- row_idx + nrow(guide_df) + 2
  writeData(wb, sheet_name, "4. Variable Definitions", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  var_df <- data.frame(
    Variable = c("age", "sex", "imd_quintile", "time_since_quit", "p_start / p_quit / p_relapse"),
    Definition = c(
      "Age of the individual in years (single year of age).",
      "Biological sex (Men / Women).",
      "Index of Multiple Deprivation Quintile. 1 = Least Deprived, 5 = Most Deprived.",
      "Number of years since the individual last quit smoking (Relapse only). 0 = quit for <1 year.",
      "The estimated annual probability (0.0 to 1.0)."
    )
  )
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, var_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  
  # --- G. Methodological Parameters ---
  row_idx <- row_idx + nrow(var_df) + 2
  writeData(wb, sheet_name, "5. Methodological Parameters & Configuration", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  # Helper to format vector inputs like c(3,15) -> "3, 15"
  fmt <- function(x) {
    if (is.null(x)) return("NA")
    if (length(x) > 1) return(paste(x, collapse = ", "))
    return(as.character(x))
  }
  
  meta_df <- data.frame(
    Category = c(
      "Software", "Data Source", "Data Source", "Data Source",
      "Projection", "Projection",
      "Initiation Model", "Initiation Model", "Initiation Model", "Initiation Model",
      "Quitting Model", "Quitting Model", "Quitting Model",
      "Relapse Model", "Relapse Model", "Relapse Model",
      "Uncertainty", "Uncertainty", "Uncertainty"
    ),
    Parameter = c(
      "Package Version", "Region", "Survey Source", "Survey Years",
      "Time Horizon", "Continuity Limit",
      "Model Choice", "Smooth Dims (Age, Year)", "Age Smoothing (k)", "Max Age Init",
      "Smooth Dims (Age, Year)", "Age Smoothing (k)", "Age Trend Limit",
      "Smooth Dims (Age, Year)", "Age Smoothing (k)", "Age Trend Limit",
      "Eff. Sample Size (kn)", "Uncertainty Samples (kn_samp)", "Correlation (kR)"
    ),
    Value = c(
      pkg_ver, config$country, config$survey_name, paste(config$first_year, "-", config$last_year),
      config$time_horizon, config$cont_limit,
      config$init_model_choice, fmt(config$smooth_rate_dim_init), config$k_smooth_age_init, config$max_age_init,
      fmt(config$smooth_rate_dim_quit), config$k_smooth_age_quit, config$age_trend_limit_quit,
      fmt(config$smooth_rate_dim_relapse), config$k_smooth_age_relapse, config$age_trend_limit_relapse,
      config$kn, config$kn_samp, config$kR
    ),
    Description = c(
      "smktrans version.", "Target population.", "Primary dataset.", "Range of data used.",
      "Final projected year.", "Last year of continuous data.",
      "Specific model ID.", "Smoothing window dimensions.", "Degree of age smoothing (rotation).", "Maximum age for initiation.",
      "Smoothing window dimensions.", "Degree of age smoothing.", "Upper age limit for trends.",
      "Smoothing window dimensions.", "Degree of age smoothing.", "Upper age limit for trends.",
      "Effective sample size.", "Number of Monte Carlo samples.", "Correlation param."
    )
  )
  
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, meta_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  
  # --- H. Column Width Adjustment (UPDATED) ---
  # Significantly increased the width of Column 1 to prevent text bunching
  setColWidths(wb, sheet_name, cols = 1, widths = 85) 
  setColWidths(wb, sheet_name, cols = 2, widths = 50)
  setColWidths(wb, sheet_name, cols = 3, widths = 35)
  setColWidths(wb, sheet_name, cols = 4, widths = 100)
  
  # =========================================================================
  # 3. DATA SHEETS
  # =========================================================================
  add_data_sheet <- function(wb, s_name, res_obj) {
    addWorksheet(wb, s_name)
    # Handle list vs dataframe input
    df <- if (inherits(res_obj, "data.frame")) res_obj else if ("data" %in% names(res_obj)) res_obj$data else data.frame(Warning = "Data not found")
    
    writeData(wb, s_name, df, headerStyle = style_col_header)
    freezePane(wb, s_name, firstRow = TRUE)
    
    # Format probability columns
    p_cols <- grep("^p_", names(df))
    if (length(p_cols) > 0) addStyle(wb, s_name, style_num, rows = 2:(nrow(df) + 1), cols = p_cols, gridExpand = TRUE)
    
    setColWidths(wb, s_name, cols = 1:ncol(df), widths = 25)
  }
  
  # Add Core Sheets
  add_data_sheet(wb, "Initiation", init_res)
  add_data_sheet(wb, "Quitting", quit_res)
  add_data_sheet(wb, "Relapse", relapse_res)
  
  # Add Optional Sheets
  if (!is.null(net_init_dt)) {
    add_data_sheet(wb, "Net Initiation", net_init_dt)
  }
  if (!is.null(quit_no_init)) {
    add_data_sheet(wb, "Quit (No Init)", quit_no_init)
  }
  
  # =========================================================================
  # 4. SAVE
  # =========================================================================
  out_dir <- file.path("transition_probability_estimates", "outputs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, paste0("SmokeStateTransProbs_", config$country, "_", Sys.Date(), ".xlsx"))
  saveWorkbook(wb, out_file, overwrite = TRUE)
}