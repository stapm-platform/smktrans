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
#' @export
write_excel_report <- function(config, init_res, quit_res, relapse_res, 
                               net_init_dt = NULL, quit_no_init = NULL) {
  
  # 1. Initialize Workbook
  wb <- createWorkbook()
  
  # =========================================================================
  # STYLES DEFINITION
  # =========================================================================
  brand_blue <- "#005A8F" 
  
  # Title: Big, Bold, Blue
  style_title <- createStyle(fontName = "Arial", fontSize = 16, textDecoration = "bold", 
                             fontColour = brand_blue)
  
  # Affiliation: Smaller, Dark Grey
  style_affil <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#555555", 
                             wrapText = TRUE)
  
  # Contact Info: Bold, Dark Grey
  style_contact <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#000000", 
                               textDecoration = "bold", wrapText = TRUE)
  
  # Section Headers: Bold, White Text, Blue Background
  style_header <- createStyle(fontName = "Arial", textDecoration = "bold", fontColour = "white", 
                              fgFill = brand_blue, halign = "left", valign = "center",
                              border = "TopBottom", borderColour = brand_blue)
  
  # Table Column Headers
  style_col_header <- createStyle(fontName = "Arial", textDecoration = "bold", fontColour = "black", 
                                  fgFill = "#D9E1F2", halign = "center", border = "Bottom")
  
  # General Text
  style_text <- createStyle(fontName = "Arial", fontSize = 10, wrapText = TRUE, valign = "top")
  
  # Numbers
  style_num <- createStyle(numFmt = "0.0000")
  
  # =========================================================================
  # 2. COVER SHEET CONSTRUCTION
  # =========================================================================
  sheet_name <- "Cover Sheet"
  addWorksheet(wb, sheet_name)
  showGridLines(wb, sheet_name, showGridLines = FALSE)
  
  # --- A. Title, Date & Package Version ---
  # Get Package Version Safely
  pkg_ver <- tryCatch(as.character(packageVersion("smktrans")), error = function(e) "Unknown")
  run_date <- format(Sys.Date(), "%d %B %Y")
  
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
  setRowHeights(wb, sheet_name, rows = 4, heights = 60) # Space for 4 lines
  
  contact_text <- "Contact: Dr Duncan Gillespie (duncan.gillespie@sheffield.ac.uk)"
  writeData(wb, sheet_name, contact_text, startRow = 5, startCol = 1)
  addStyle(wb, sheet_name, style_contact, rows = 5, cols = 1)
  
  # --- C. About This Dataset ---
  row_idx <- 8
  writeData(wb, sheet_name, "1. About this Dataset", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  desc_text <- paste(
    "This workbook contains estimated annual probabilities for smoking state transitions used in the STAPM microsimulation model.",
    "Estimates are derived using methods in the R package 'smktrans' (https://stapm-platform.github.io/smktrans/) fitted to repeat annual cross-sectional survey data.",
    "These probabilities represent the likelihood of an individual changing their smoking status within a one-year period,",
    "stratified by Age, Sex, and Index of Multiple Deprivation (IMD) quintiles.",
    sep = " "
  )
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, desc_text, startRow = row_idx, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:4, rows = row_idx) 
  addStyle(wb, sheet_name, style_text, rows = row_idx, cols = 1)
  setRowHeights(wb, sheet_name, rows = row_idx, heights = 60) # Increased height for safety
  
  # --- D. Sheet Guide ---
  row_idx <- row_idx + 2
  writeData(wb, sheet_name, "2. Worksheet Guide", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  guide_df <- data.frame(
    Worksheet = c("Initiation", "Quitting", "Relapse"),
    Description = c(
      "Probability of a 'Never Smoker' becoming a 'Current Smoker' (Start).",
      "Probability of a 'Current Smoker' becoming a 'Former Smoker' (Quit).",
      "Probability of a 'Former Smoker' becoming a 'Current Smoker' (Relapse), dependent on time since quitting."
    )
  )
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, guide_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  addStyle(wb, sheet_name, style_text, rows = (row_idx+1):(row_idx+3), cols = 2)
  
  # --- E. Variable Definitions ---
  row_idx <- row_idx + 5
  writeData(wb, sheet_name, "3. Variable Definitions", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  var_df <- data.frame(
    Variable = c("age", "sex", "imd_quintile", "time_since_quit", "p_start / p_quit / p_relapse"),
    Definition = c(
      "Age of the individual in years (single year of age).",
      "Biological sex (Men / Women).",
      "Index of Multiple Deprivation Quintile. 1 = Least Deprived, 5 = Most Deprived.",
      "Number of years since the individual last quit smoking (Only applicable to Relapse). 0 = quit for less than 1 year.",
      "The estimated annual probability of the event occurring (0.0 to 1.0)."
    )
  )
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, var_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  addStyle(wb, sheet_name, style_text, rows = (row_idx+1):(row_idx+nrow(var_df)), cols = 2)
  
  # --- F. Methodological Parameters (Detailed) ---
  row_idx <- row_idx + nrow(var_df) + 2
  writeData(wb, sheet_name, "4. Methodological Parameters & Configuration", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  # Helper to safely format vector outputs like c(3, 15)
  fmt_vec <- function(v) { if(is.null(v)) "NA" else paste(v, collapse = ", ") }
  
  # Construct the detailed parameters table
  meta_df <- data.frame(
    Category = c(
      "Software", "Data Source", "Data Source", "Data Source", "Projection",
      "Initiation Model", "Initiation Model", "Initiation Model", "Initiation Model",
      "Quitting Model", "Quitting Model", "Quitting Model",
      "Relapse Model", "Relapse Model", "Relapse Model",
      "Uncertainty", "Uncertainty", "Uncertainty"
    ),
    Parameter = c(
      "Package Version", "Region", "Survey Source", "Survey Years", "Time Horizon",
      
      # Init
      "Model Choice", "Smoothing Window (Age, Year)", "SVD Smoothing (Age)", "Age Trend Limit",
      
      # Quit
      "Smoothing Window (Age, Year)", "SVD Smoothing (Age)", "Age Trend Limit",
      
      # Relapse
      "Smoothing Window (Age, Year)", "SVD Smoothing (Age)", "Age Trend Limit",
      
      # Uncertainty
      "Eff. Sample Size (kn)", "Uncertainty Samples (kn_samp)", "Correlation (kR)"
    ),
    Value = c(
      pkg_ver,
      config$country,
      config$survey_name,
      paste(config$first_year, "-", config$last_year),
      config$time_horizon,
      
      # Init Params
      config$init_model_choice,
      fmt_vec(config$smooth_rate_dim_init),
      config$k_smooth_age_init,
      paste(config$age_trend_limit_init, "years"),
      
      # Quit Params
      fmt_vec(config$smooth_rate_dim_quit),
      config$k_smooth_age_quit,
      paste(config$age_trend_limit_quit, "years"),
      
      # Relapse Params
      fmt_vec(config$smooth_rate_dim_relapse),
      config$k_smooth_age_relapse,
      paste(config$age_trend_limit_relapse, "years"),
      
      # Uncertainty
      config$kn,
      config$kn_samp,
      config$kR
    ),
    Description = c(
      "Version of the smktrans R package used to generate estimates.",
      "Target population for the estimates.",
      "Primary dataset used for model fitting.",
      "Range of data years included in the pooled analysis.",
      "The final year for which probabilities are projected (to project beyond this year, hold the probabilities for the final year constant for future years).",
      
      # Init Description
      "Specific Generalised Linear Model (GLM) specification used to describe trends in smoking status.",
      "Dimensions of the 2D window used to smooth rate trends. Format: c(Age Window, Year Window).",
      "Degree of smoothing applied to the age pattern of change (rotation) in the Lee-Carter SVD decomposition. If zero, no smoothing is applied.",
      "The age up to which time-variant trends are estimated. Beyond this age, rates are held constant (initiation is only estimated up to age 30, after which no initiation is assumed to occur).",
      
      # Quit Description
      "Dimensions of the 2D window used to smooth quitting rate trends.",
      "Degree of smoothing applied to the age pattern of change (rotation) in the Lee-Carter SVD decomposition.",
      "The age up to which time-variant quitting trends are estimated (the max age in the model is 89 years).",
      
      # Relapse Description
      "Dimensions of the 2D window used to smooth relapse rate trends.",
      "Degree of smoothing applied to the age pattern of change (rotation) in the Lee-Carter SVD decomposition.",
      "The age up to which time-variant relapse trends are estimated.",
      
      # Uncertainty Description
      "The effective sample size parameter of the Beta distribution used to model uncertainty around the central estimate.",
      "The number of stochastic realizations (samples) generated to represent parameter uncertainty.",
      "Correlation coefficient for age-specific uncertainty samples (0 = Independent, 1 = Fully Correlated)."
    )
  )
  
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, meta_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  
  # Apply text wrapping to the Description column
  addStyle(wb, sheet_name, style_text, rows = (row_idx+1):(row_idx+nrow(meta_df)), cols = 4) # Description col
  
  # Formatting Cover Columns - UPDATED WIDTHS
  setColWidths(wb, sheet_name, cols = 1, widths = 30) # Category (Increased from 20)
  setColWidths(wb, sheet_name, cols = 2, widths = 50) # Parameter Name (Increased from 35)
  setColWidths(wb, sheet_name, cols = 3, widths = 35) # Value (Increased from 25)
  setColWidths(wb, sheet_name, cols = 4, widths = 100) # Description (Increased from 60)
  
  # =========================================================================
  # 3. DATA SHEETS
  # =========================================================================
  
  add_data_sheet <- function(wb, s_name, res_obj) {
    addWorksheet(wb, s_name)
    
    if (inherits(res_obj, "data.frame")) {
      df <- res_obj
    } else if (is.list(res_obj) && "data" %in% names(res_obj)) {
      df <- res_obj$data
    } else {
      df <- data.frame(Warning = "Data not found")
    }
    
    writeData(wb, s_name, df, headerStyle = style_col_header)
    freezePane(wb, s_name, firstRow = TRUE)
    
    if (ncol(df) > 0) {
      p_cols <- grep("^p_", names(df)) 
      if (length(p_cols) > 0) {
        addStyle(wb, s_name, style_num, rows = 2:(nrow(df) + 1), cols = p_cols, gridExpand = TRUE)
      }
    }
    setColWidths(wb, s_name, cols = 1:ncol(df), widths = "auto")
  }
  
  add_data_sheet(wb, "Initiation", init_res)
  add_data_sheet(wb, "Quitting", quit_res)
  add_data_sheet(wb, "Relapse", relapse_res)
  
  if (!is.null(net_init_dt)) add_data_sheet(wb, "Net Initiation", net_init_dt)
  if (!is.null(quit_no_init)) add_data_sheet(wb, "Quit (No Init)", quit_no_init)
  
  # =========================================================================
  # 4. SAVE
  # =========================================================================
  out_dir <- file.path("transition_probability_estimates", "outputs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  file_name <- paste0("SmokeStateTransProbs_", config$country, "_", Sys.Date(), ".xlsx")
  out_file <- file.path(out_dir, file_name)
  
  saveWorkbook(wb, out_file, overwrite = TRUE)
  
}
