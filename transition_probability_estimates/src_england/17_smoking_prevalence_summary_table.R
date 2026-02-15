# ==============================================================================
# Create a summary table of smoking prevalence by subgroup
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------
source("03_load_packages.R")
# library(data.table)
# library(gt)
# library(gtsummary)

# Define paths
root_dir <- "X:/"
path <- "transition_probability_estimates/src_england/"

# Load the CLEANED data
input_file <- paste0(path, "intermediate_data/HSE_2003_to_2018_tobacco.rds")
data <- readRDS(input_file)

# 2. PREPARE VARIABLES ---------------------------------------------------------

# Create custom groupings for the report
# Years: Split into two eras
data[, year_cat := c("2003-2012", "2013-2018")[findInterval(year, c(-1, 2013, 10000))]]

# Make broad age categories
data[, age_cat_rept := c("11-17", "18-24", "25-49", "50+")[findInterval(age, c(-1, 18, 25, 50, 1000))]]

# Ensure factors are ordered correctly for the table
data[, imd_quintile := factor(imd_quintile, levels = 1:5, labels = c("1 (Least Deprived)", "2", "3", "4", "5 (Most Deprived)"))]
data[, smk.state := factor(smk.state, levels = c("current", "former", "never"), labels = c("Current Smoker", "Former Smoker", "Never Smoker"))]

# 3. HELPER FUNCTION -----------------------------------------------------------
# Calculates N and % for a specific grouping variable
get_summary <- function(dt, group_var, group_label) {
  
  # Calculate counts by subgroup and smoking state
  res <- dt[, .N, by = c(group_var, "smk.state")]
  
  # Calculate percentages within the subgroup
  res[, total := sum(N), by = group_var]
  res[, perc := round(100 * N / total, 1)]
  
  # Format as "N (%)"
  # format(N, big.mark=",") adds commas to thousands (e.g., 1,200)
  res[, cell_val := paste0(format(N, big.mark = ","), " (", sprintf("%.1f", perc), "%)")]
  
  # Cast (Pivot) to wide format: One row per subgroup, columns for states
  # Formula: subgroup ~ smoking_state
  wide <- dcast(res, as.formula(paste(group_var, "~ smk.state")), value.var = "cell_val", fill = "0 (0.0%)")
  
  # Add the 'Total N' column and the Label column
  # We get the total N from the first occurrence in the original calculation
  totals <- unique(res[, c(group_var, "total"), with = FALSE])
  wide <- merge(wide, totals, by = group_var)
  
  # Rename the grouping column to "Subgroup" so we can stack them later
  setnames(wide, group_var, "Level")
  wide[, Variable := group_label]
  
  # Reorder columns: Variable, Level, Total, States...
  setcolorder(wide, c("Variable", "Level", "total"))
  
  return(wide)
}

# 4. GENERATE SECTIONS ---------------------------------------------------------

# We run the function for each variable we want in the table
tbl_year <- get_summary(data, "year_cat", "Survey Year")
tbl_sex  <- get_summary(data, "sex", "Sex")
tbl_age  <- get_summary(data, "age_cat_rept", "Age Group")
tbl_imd  <- get_summary(data, "imd_quintile", "IMD Quintile")

# Combine into one master table
final_dt <- rbindlist(list(tbl_year, tbl_sex, tbl_age, tbl_imd))

# 5. RENDER BEAUTIFUL TABLE (using gt) -----------------------------------------

final_dt %>%
  gt(groupname_col = "Variable", rowname_col = "Level") %>%
  
  # Add Title and Subtitle
  tab_header(
    title = md("**Study Population Characteristics**"),
    subtitle = "Smoking prevalence by demographic characteristics (Health Survey for England 2003-2018)"
  ) %>%
  
  # Rename Columns
  cols_label(
    total = "Total N",
    `Current Smoker` = "Current Smoker n (%)",
    `Former Smoker` = "Former Smoker n (%)",
    `Never Smoker` = "Never Smoker n (%)"
  ) %>%
  
  # Formatting
  fmt_number(columns = "total", decimals = 0, use_seps = TRUE) %>%
  
  # Add footnotes
  tab_footnote(
    footnote = "Percentages are row percentages calculated within each subgroup.",
    locations = cells_column_labels(columns = everything())
  ) %>%
  
  # Apply a clean theme
  opt_row_striping() %>% 
  tab_options(
    table.font.names = "Arial",
    heading.align = "left",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) -> my_table

# 6. SAVE ----------------------------------------------------------------------

# Print to Viewer
print(my_table)

# Save as HTML (easy to open in Word/Excel)
gtsave(my_table, paste0(path, "outputs/Table1_Demographics.html"))

# Save as RTF (for direct Word copy-paste)
gtsave(my_table, paste0(path, "outputs/Table1_Demographics.rtf"))

