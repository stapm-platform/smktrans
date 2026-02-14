#' Write Transition Probability Estimates to Excel
#' 
#' @description Generates a multi-sheet Excel workbook with formatted data and a cover sheet.
#' @param config Configuration list.
#' @param init_res Initiation results list.
#' @param quit_res Quitting results list.
#' @param relapse_res Relapse results list.
#' @param net_init_dt Net initiation data table.
#' 
#' @import openxlsx
#' @export
write_excel_report <- function(config, init_res, quit_res, relapse_res, net_init_dt, quit_no_init) {
  
  wb <- createWorkbook()
  
  # Define Styles
  header_st <- createStyle(fontName="Arial", textDecoration="bold", fontColour="white", 
                           fgFill="#4F81BD", halign="center")
  num_st <- createStyle(numFmt="0.0000")
  
  # 1. Cover Sheet
  addWorksheet(wb, "Cover Sheet")
  writeData(wb, "Cover Sheet", "Smoking Transition Probability Report", startRow=1, startCol=1)
  
  # Summary Metadata Table
  meta <- data.frame(
    Parameter = c("Country", "Run Date", "Survey", "Samples", "Correlation"),
    Value = c(config$country, as.character(Sys.Date()), config$survey_name, config$kn_samp, config$kR))
  
  writeData(wb, "Cover Sheet", meta, startRow=3, startCol=1, headerStyle=header_st)
  
  # Detailed Config Dump
  config_df <- data.frame(Key = names(config), Value = sapply(config, paste, collapse=", "))
  writeData(wb, "Cover Sheet", "Detailed Configuration Parameters", startRow=10, startCol=1)
  writeData(wb, "Cover Sheet", config_df, startRow=11, startCol=1, headerStyle=header_st)
  
  # 2. Data Sheets Internal Helper
  add_formatted_sheet <- function(wb, s_name, res_obj) {
    addWorksheet(wb, s_name)
    if (inherits(res_obj, "data.frame")) {
      df <- res_obj
    }
    else if (is.list(res_obj) && "data" %in% names(res_obj)) {
      df <- res_obj$data
    }
    else {
      df <- data.frame(Warning = "Data not found")
    }
    writeData(wb, s_name, df, headerStyle=header_st)
    freezePane(wb, s_name, firstRow=TRUE)
    if (ncol(df) > 0) p_cols <- grep("^p_", names(df))
    if(length(p_cols) > 0) addStyle(wb, s_name, num_st, rows=2:(nrow(df)+1), cols=p_cols, gridExpand=TRUE)
    setColWidths(wb, s_name, cols=1:ncol(df), widths="auto")
  }
  
  # 3. Add Sheets
  add_formatted_sheet(wb, "Initiation", init_res)
  add_formatted_sheet(wb, "Quitting", quit_res)
  add_formatted_sheet(wb, "Relapse", relapse_res)
  add_formatted_sheet(wb, "Net Initiation", net_init_dt)
  add_formatted_sheet(wb, "Quitting - no init adj", quit_no_init)
  
  # 4. Save
  current_date <- Sys.Date()
  file_name <- paste0("SmokeStateTransProbs_", config$country, "_", current_date, ".xlsx")
  out_file <- file.path("transition_probability_estimates/outputs", file_name)
  saveWorkbook(wb, out_file, overwrite = TRUE)
  
  message(paste("Excel saved to:", out_file))
  
}


