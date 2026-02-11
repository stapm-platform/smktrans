#' Estimate socioeconomic differences in cohort survivorship
#'
#' @description
#' Combines long-term historical mortality rates (HMD) with recent socioeconomically 
#' stratified mortality data (ONS) to produce cohort survivorship curves (lx) 
#' stratified by Age, Sex, and IMD Quintile.
#'
#' @param mx_data_hmd Data table of HMD mortality rates (1922+).
#' @param mx_data_ons Data table of ONS stratified mortality rates.
#' @param min_age,max_age,min_year,max_year Integers defining the scope.
#' @importFrom data.table copy := setDT shift merge CJ setkeyv
#' @export
prep_surv <- function(
    mx_data_hmd = smktrans::hmd_data_eng,
    mx_data_ons,
    min_age = 11,
    max_age = 89,
    min_year = 2003,
    max_year = 2018
) {
  
  # 1. HMD Data Processing (Baseline Trend)
  hmd <- copy(mx_data_hmd)
  
  # Calculate survival probability (px) from mortality rate (mx)
  # Using actuarial approximation: qx = mx / (1 + 0.5*mx)
  hmd[, qx := mx / (1 + 0.5 * mx)]
  hmd[, px_hmd := 1 - qx]
  
  # Filter valid data
  hmd <- hmd[px_hmd >= 0 & px_hmd <= 1]
  
  # Cohort definition: Year of Birth
  hmd[, cohort := year - age]
  
  # Keep only relevant columns
  hmd <- hmd[, .(age, sex, cohort, px_hmd)]
  
  # 2. ONS Data Processing (Socioeconomic Stratification)
  ons <- copy(mx_data_ons)
  
  ons[, qx := mx / (1 + 0.5 * mx)]
  ons[, px_ons := 1 - qx]
  ons <- ons[px_ons >= 0 & px_ons <= 1]
  
  ons[, cohort := year - age]
  ons <- ons[, .(age, sex, cohort, imd_quintile, px_ons)]
  
  # 3. Create Master Domain
  # We need a grid of all Cohorts x Ages x Sex x IMD
  # Range of cohorts needed covers the min_year/max_year window
  
  # Define the required cohort range based on the input years and ages
  # e.g., to model age 89 in 2003, we need cohort 1914.
  c_min <- min_year - max_age
  c_max <- max_year - min_age
  
  domain <- CJ(
    age = min_age:max_age,
    cohort = c_min:c_max,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  )
  
  # 4. Merge HMD (Baseline)
  domain <- merge(domain, hmd, by = c("age", "sex", "cohort"), all.x = TRUE)
  
  # 5. Merge ONS (Stratification)
  domain <- merge(domain, ons, by = c("age", "sex", "cohort", "imd_quintile"), all.x = TRUE)
  
  # 6. Calculate IMD-Specific Scaling Factors
  # Logic: How much worse/better is a specific IMD quintile compared to the average (Quintile 3)?
  # We calculate this ratio using the ONS data, then apply it to the long-term HMD data.
  
  # Calculate the reference (Quintile 3) px for every Age/Sex/Cohort
  # We do a self-join or aggregation to get the Q3 value
  ons_q3 <- ons[imd_quintile == "3", .(age, sex, cohort, px_ref = px_ons)]
  
  domain <- merge(domain, ons_q3, by = c("age", "sex", "cohort"), all.x = TRUE)
  
  # Calculate Multiplier (Default to 1 if missing)
  domain[, multiplier := 1]
  domain[!is.na(px_ons) & !is.na(px_ref) & px_ref > 0, multiplier := px_ons / px_ref]
  
  # Apply Multiplier to HMD baseline
  domain[, px_adj := px_hmd * multiplier]
  
  # Clamp probabilities
  domain[px_adj > 1, px_adj := 1]
  domain[px_adj < 0, px_adj := 0]
  
  # Fill missing with HMD baseline (implies multiplier=1)
  domain[is.na(px_adj), px_adj := px_hmd]
  # If still missing (e.g. future cohorts not in HMD), assume 1 (no death) or carry forward logic?
  domain[is.na(px_adj), px_adj := 1]
  
  # 7. Calculate Cohort Survivorship (lx)
  # lx is the cumulative product of px along the cohort line
  
  # Sort strictly by Cohort then Age
  setkeyv(domain, c("cohort", "sex", "imd_quintile", "age"))
  
  # Calculate lx using cumulative product
  # Note: Standard lx starts at 1. We assume lx at entry is 1 (or carry over).
  # The original code used a lag. We can use cumprod directly on the sorted vector.
  
  # We want the cumulative probability of surviving FROM the minimum age up to the current age.
  domain[, lx := cumprod(px_adj), by = .(cohort, sex, imd_quintile)]
  
  # 8. Filter Output to requested Year/Age range
  domain[, year := cohort + age]
  domain <- domain[year >= min_year & year <= max_year & age >= min_age & age <= max_age]
  
  cols_keep <- c("age", "year", "cohort", "sex", "imd_quintile", "lx")
  return(domain[, ..cols_keep])
}