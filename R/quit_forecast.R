#' Forecast probabilities of smoking initiation, quitting and relapse
#'
#' @description
#' Forecasts trends in transition probabilities using a Lee-Carter style 
#' Singular Value Decomposition (SVD) model.
#'
#' @details
#' The model assumes the logit of the probability can be decomposed into:
#' Logit(P_xt) = Alpha_x + Beta_x * Kappa_t
#' Where:
#' - Alpha_x: Average age profile
#' - Kappa_t: Time trend index
#' - Beta_x: Sensitivity of each age to the time trend
#'
#' @param data Data table with input probabilities.
#' @param forecast_var Character - variable to forecast.
#' @param forecast_type "continuing" (linear trend) or "stationary" (constant).
#' @param cont_limit Integer - year where forecast becomes stationary.
#' @param oldest_year Integer - start of historical data.
#' @param youngest_age Integer - min age.
#' @param oldest_age Integer - max age.
#' @param first_year Integer - start year for trend fitting.
#' @param jump_off_year Integer - end year of historical data.
#' @param time_horizon Integer - end year of forecast.
#' @param smooth_rate_dim Vector - dimensions for raster smoothing (c(3,3)).
#' @param k_smooth_age Integer - knots for smoothing age component.
#' @importFrom data.table copy := setDT dcast melt rbindlist
#' @importFrom raster raster as.matrix focal
#' @importFrom VGAM logitlink
#' @importFrom mgcv gam
#' @export
quit_forecast <- function(
    data,
    forecast_var,
    forecast_type = c("continuing", "stationary"),
    cont_limit = NULL,
    oldest_year = 2003,
    youngest_age = 11,
    oldest_age = 88,
    age_cont_limit = 88,
    first_year = 2010,
    jump_off_year = 2015,
    time_horizon = 2050,
    smooth_rate_dim = c(3, 3),
    k_smooth_age = 3
) {
  
  forecast_type <- match.arg(forecast_type)
  
  # Define Ranges
  ages_model <- youngest_age:age_cont_limit
  years_hist <- oldest_year:jump_off_year
  years_proj <- (jump_off_year + 1):time_horizon
  
  # Filter Data
  dt <- copy(data[age %in% ages_model & year <= jump_off_year])
  
  # Create full grid to ensure no missing cells
  domain <- data.table(expand.grid(
    year = years_hist,
    age = ages_model,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  
  dt <- merge(domain, dt, by = c("year", "age", "sex", "imd_quintile"), all.x = TRUE)
  
  # Storage for results
  results_list <- list()
  
  # Loop through subgroups
  # (Loop is acceptable here as models are independent and N=10)
  for(sex_i in c("Male", "Female")) {
    for(imd_i in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      subdata <- dt[sex == sex_i & imd_quintile == imd_i]
      
      # 1. Matrix Prep
      # Cast to Wide: Rows=Age, Cols=Year
      mat_wide <- dcast(subdata, age ~ year, value.var = forecast_var)
      mat_vals <- as.matrix(mat_wide[, -1]) # Drop age col
      
      # 2. Clamping and Smoothing
      # Clamp 0/1 to avoid Inf in logit
      mat_vals[mat_vals <= 0] <- 1e-6
      mat_vals[mat_vals >= 1] <- 1 - 1e-6
      mat_vals[is.na(mat_vals)] <- 1e-6
      
      # Raster Smoothing (Reduces survey noise)
      r <- raster::raster(mat_vals)
      mat_smooth <- raster::as.matrix(raster::focal(r, matrix(1, smooth_rate_dim[1], smooth_rate_dim[2]), mean, pad = TRUE, padValue = NA, na.rm = TRUE))
      
      # Handle edges/NAs from smoothing
      mat_smooth[is.na(mat_smooth)] <- 1e-6
      
      # 3. SVD Decomposition (Lee-Carter)
      # Transpose so Rows=Year, Cols=Age for standard SVD processing
      # (Note: Original code transposed here, sticking to that logic)
      mat_t <- t(mat_smooth) 
      
      # Logit Transform
      logit_mat <- VGAM::logitlink(mat_t)
      
      # Center (Alpha_x)
      alpha_x <- colMeans(logit_mat, na.rm = TRUE)
      centered_mat <- sweep(logit_mat, 2, alpha_x, "-")
      
      # SVD
      svd_res <- svd(centered_mat)
      
      # Components
      # k_t (Time Index)
      # b_x (Age Sensitivity)
      sum_v <- sum(svd_res$v[, 1])
      k_t <- svd_res$d[1] * svd_res$u[, 1] * sum_v
      b_x <- svd_res$v[, 1] / sum_v
      
      # Smooth Age Component (b_x)
      if(k_smooth_age > 0) {
        b_x <- predict(mgcv::gam(b_x ~ s(I(1:length(b_x)), k = k_smooth_age)))
        b_x <- b_x / sum(b_x) # Re-normalize
      }
      
      # 4. Forecast Time Component (k_t)
      # Match years to k_t
      kt_df <- data.frame(kt = k_t, year = years_hist)
      
      if(forecast_type == "continuing") {
        # Linear trend on k_t
        model_data <- kt_df[kt_df$year >= first_year, ]
        m_trend <- lm(kt ~ year, data = model_data)
        
        preds <- predict(m_trend, newdata = data.frame(year = years_proj))
        
        # Handle Stationary Limit
        if(!is.null(cont_limit)) {
          limit_val <- preds[years_proj == cont_limit]
          # If limit year not in projection, take last
          if(length(limit_val) == 0) limit_val <- tail(preds, 1)
          
          preds[years_proj > cont_limit] <- limit_val
        }
        kt_proj <- preds
        
      } else {
        # Stationary: Hold last value
        kt_proj <- rep(tail(k_t, 1), length(years_proj))
      }
      
      # Combined Time Vector
      kt_full <- c(k_t, kt_proj)
      years_full <- c(years_hist, years_proj)
      
      # 5. Reconstruct Rates
      # Logit = Alpha + b_x * k_t
      recon_logit <- outer(kt_full, b_x)
      recon_logit <- sweep(recon_logit, 2, alpha_x, "+")
      
      # Inverse Logit
      recon_prob <- boot::inv.logit(recon_logit)
      
      # 6. Format Output
      colnames(recon_prob) <- ages_model
      out_df <- as.data.frame(recon_prob)
      out_df$year <- years_full
      
      setDT(out_df)
      out_long <- melt(out_df, id.vars = "year", variable.name = "age", value.name = forecast_var)
      
      out_long[, age := as.numeric(as.character(age))]
      out_long[, sex := sex_i]
      out_long[, imd_quintile := imd_i]
      
      results_list[[paste(sex_i, imd_i)]] <- out_long
    }
  }
  
  final_dt <- rbindlist(results_list)
  
  # Final Safety Clamp
  final_dt[get(forecast_var) < 0, (forecast_var) := 0]
  final_dt[get(forecast_var) > 1, (forecast_var) := 1]
  
  # Fill older ages (constant assumption beyond age_cont_limit)
  # Create full domain including oldest_age
  full_domain <- data.table(expand.grid(
    year = unique(final_dt$year),
    age = youngest_age:oldest_age,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  
  full_domain <- merge(full_domain, final_dt, by = c("year", "age", "sex", "imd_quintile"), all.x = TRUE)
  
  # LOCF for older ages
  full_domain[, temp_lim := get(forecast_var)[age == age_cont_limit], by = .(year, sex, imd_quintile)]
  full_domain[age > age_cont_limit, (forecast_var) := temp_lim]
  full_domain[, temp_lim := NULL]
  
  return(full_domain[])
}

#' @export
fill.zero <- function(x, method = "constant") {
  # Helper for imputation if needed
  tt <- 1:length(x)
  zeros <- abs(x) < 1e-9
  xx <- x[!zeros]
  tt <- tt[!zeros]
  if(length(tt) > 0) {
    x_approx <- stats::approx(tt, xx, 1:length(x), method = method, f = 0.5, rule = 2)
    return(x_approx$y)
  } else {
    return(x + 1e-5)
  }
}