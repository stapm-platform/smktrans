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
  pkg_ver <- tryCatch(as.character(packageVersion("smktrans")), error = function(e) "2.0.0")
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
  setRowHeights(wb, sheet_name, rows = 4, heights = 60)
  
  contact_text <- "Contact: Dr Duncan Gillespie (duncan.gillespie@sheffield.ac.uk)"
  writeData(wb, sheet_name, contact_text, startRow = 5, startCol = 1)
  addStyle(wb, sheet_name, style_contact, rows = 5, cols = 1)
  
  # --- C. HOW TO CITE (New Section with Persistent DOI) ---
  doi_link <- "https://doi.org/10.17605/OSF.IO/YGXQ9"
  citation_text <- paste0(
    "How to cite these estimates:\n",
    "Gillespie, D., and Brennan, A. (", format(Sys.Date(), "%Y"), "). ",
    "smktrans: An R Package for estimating smoking state transition probabilities (v", pkg_ver, "). ",
    "University of Sheffield. DOI: ", doi_link
  )
  
  row_idx <- 7
  writeData(wb, sheet_name, citation_text, startRow = row_idx, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:4, rows = row_idx)
  addStyle(wb, sheet_name, style_cite_box, rows = row_idx, cols = 1)
  setRowHeights(wb, sheet_name, rows = row_idx, heights = 50) # Extra height for the citation
  
  # --- D. About This Dataset ---
  row_idx <- 9
  writeData(wb, sheet_name, "1. About this Dataset", startRow = row_idx, startCol = 1)
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
  
  # --- F. Variable Definitions ---
  row_idx <- row_idx + 5
  writeData(wb, sheet_name, "3. Variable Definitions", startRow = row_idx, startCol = 1)
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
  writeData(wb, sheet_name, "4. Methodological Parameters & Configuration", startRow = row_idx, startCol = 1)
  addStyle(wb, sheet_name, style_header, rows = row_idx, cols = 1:4)
  
  meta_df <- data.frame(
    Category = c("Software", "Data Source", "Projection", "Uncertainty"),
    Parameter = c("Package Version", "Region", "Time Horizon", "Eff. Sample Size (kn)"),
    Value = c(pkg_ver, config$country, config$time_horizon, config$kn),
    Description = c("smktrans version.", "Target population.", "Final projected year.", "Effective sample size for uncertainty.")
  )
  
  row_idx <- row_idx + 1
  writeData(wb, sheet_name, meta_df, startRow = row_idx, startCol = 1, headerStyle = style_col_header)
  
  # Ensure columns are wide enough for the content
  setColWidths(wb, sheet_name, cols = 1, widths = 30)
  setColWidths(wb, sheet_name, cols = 2, widths = 50)
  setColWidths(wb, sheet_name, cols = 3, widths = 35)
  setColWidths(wb, sheet_name, cols = 4, widths = 100)
  
  # =========================================================================
  # 3. DATA SHEETS
  # =========================================================================
  add_data_sheet <- function(wb, s_name, res_obj) {
    addWorksheet(wb, s_name)
    df <- if (inherits(res_obj, "data.frame")) res_obj else if ("data" %in% names(res_obj)) res_obj$data else data.frame(Warning = "Data not found")
    writeData(wb, s_name, df, headerStyle = style_col_header)
    freezePane(wb, s_name, firstRow = TRUE)
    p_cols <- grep("^p_", names(df))
    if (length(p_cols) > 0) addStyle(wb, s_name, style_num, rows = 2:(nrow(df) + 1), cols = p_cols, gridExpand = TRUE)
    setColWidths(wb, s_name, cols = 1:ncol(df), widths = "auto")
  }
  
  add_data_sheet(wb, "Initiation", init_res)
  add_data_sheet(wb, "Quitting", quit_res)
  add_data_sheet(wb, "Relapse", relapse_res)
  
  # =========================================================================
  # 4. SAVE
  # =========================================================================
  out_dir <- file.path("transition_probability_estimates", "outputs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, paste0("SmokeStateTransProbs_", config$country, "_", Sys.Date(), ".xlsx"))
  saveWorkbook(wb, out_file, overwrite = TRUE)
}
