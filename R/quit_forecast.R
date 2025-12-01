#' Forecast probabilities of smoking initiation, quitting and relapse
#'
#' Forecasts the period trends in the probabilities of smoking initiation, quitting and relapse
#' by age, sex and IMD quintile. This function was designed originally to forecast just 
#' the probabilities of quitting but has since been extended to forecast initiation and relapse 
#' probabilities too.
#'
#' The forecast is based on applying a Singular value decomposition (SVD)
#' to the logit transformed matrix of quit probabilities by
#' age and year for each subgroup.
#'
#' @param data Data table containing the probabilities to be forecast.
#' @param forecast_var Character - the name of the probability variable to be forecast.
#' @param forecast_type Character - whether to apply the estimated 
#' rates of proportional change ("continuing")
#' or to keep the forecast variable constant at its last observed value ("stationary").
#' @param cont_limit Integer - the year at which a continuing forecast becomes stationary.
#' @param oldest_year Integer - the oldest year of data we have. Default is set to 2003 for England. 
#' @param youngest_age Integer - the youngest age we have in the data. 
#' Default is set to 11 for England.
#' @param oldest_age Integer - the oldest age we have in the data - set to 88 for quitting and relapse 
#' and 30 for initiation.
#' @param age_cont_limit Integer - the age after which the forecast transition probabilities 
#' for a year, sex and IMD quintile are assumed not to change.
#' @param first_year Integer - the earliest year of data on which the forecast is based.
#' @param jump_off_year Integer - the last year of data.
#' @param time_horizon Integer - the last year of the forecast period.
#' @param smooth_rate_dim Numeric vector length 2. The dimensions of the 2d window used to smooth trends 
#' in the rates by age and year. (age, year), Defaults to c(3, 3). Must be odd numbers.  
#' @param k_smooth_age Integer - the degree of smoothing to apply to the age pattern of change (rotation). 
#' If zero, then no smoothing is applied.
#' 
#' @importFrom data.table copy := setDT melt rbindlist dcast
#' 
#' @return Returns a data.table containing the observed and forecast data.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' forecast_data <- quit_forecast(
#'   data = copy(quit_data),
#'   forecast_var = "quit_prob",
#'   forecast_type = "continuing",
#'   first_year = 2010,
#'   jump_off_year = 2015,
#'   time_horizon = 2030
#' )
#'
#' }
#'
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
  
  # The ages and years
  
  ages1 <- youngest_age:oldest_age
  ages2 <- youngest_age:age_cont_limit
  
  years <- oldest_year:jump_off_year
  proj_years <- (jump_off_year + 1):time_horizon
  
  n <- length(ages2)
  m <- length(years)
  
  data <- copy(data[age %in% ages2 & year <= jump_off_year])
  
  
  domain <- data.frame(expand.grid(
    year = years,
    age = ages2,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  
  setDT(domain)
  
  data <- merge(domain, data, by = c("year", "age", "sex", "imd_quintile"), all.x = T, all.y = F)
  
  
  # Loop through subgroups
  
  counter <- 1
  
  for(sex_i in c("Male", "Female")) {
    
    #sex_i <- "Male"
    cat(sex_i, "\n")
    
    for(imd_quintile_i in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      #imd_quintile_i <- "2"
      cat("\t", imd_quintile_i, "\n")
      
      # Select the data for one subgroup
      
      subdata <- copy(data[sex == sex_i & imd_quintile == imd_quintile_i])
      
      subdata[ , `:=`(sex = NULL, imd_quintile = NULL)]
      
      # Reshape to wide form with years as columns
      # then make into a matrix
      
      qdat <- dcast(subdata, age ~ year, value.var = forecast_var)
      
      qdat[ , age := NULL]
      
      qdat <- as.matrix(qdat)
      
      # Transpose matrix data and get deaths and logrates
      # replace extreme quit probabilities with NA - then fill with approx trend
      # then logit transform
      
      qdat[qdat == 0] <- NA
      #qdat[qdat > .4] <- .4
      qdat[qdat > 1] <- 1
      
      # smooth values in sliding a 3x3 window
      r <- raster::raster(qdat) # convert to rasterLayer
      qdat <- raster::as.matrix(raster::focal(r, matrix(1, smooth_rate_dim[1], smooth_rate_dim[2]), mean, pad = T, padValue = NA, na.rm = T))
      
      # Fill any remaining missing values
      
      # and remove 0s and 1s from the data because they don't play well with the logit link
      
      qdat[is.na(qdat)] <- 0
      qdat[qdat == 1] <- 0
      
      for(i in 1:n) {
        qdat[i,] <- fill.zero(qdat[i,])
      }
      
      # Transpose and transform
      
      qdat <- t(qdat)
      
      qtrans <- VGAM::logitlink(qdat)
      
      # Do SVD
      age_means <- apply(qtrans, 2, mean, na.rm = TRUE) # age_means is mean of qtrans by column
      cqtrans <- sweep(qtrans, 2 , age_means) # logit quit probs (with age_means subtracted) (dimensions m*n)
      svd_qdat <- svd(cqtrans)
      
      # Extract first principal component
      sumv <- sum(svd_qdat$v[ , 1])
      kt <- svd_qdat$d[1] * svd_qdat$u[ , 1] * sumv
      
      bx <- svd_qdat$v[ , 1] / sumv
      
      if(k_smooth_age == 0) {
        
        bx_fit <- bx
        
      } else {
        
        bx_fit <- stats::predict(mgcv::gam(bx ~ s(I(1:length(bx)), k = k_smooth_age)))
        
      }
      
      bx_fit <- bx_fit / sum(bx_fit)
      
      
      if(forecast_type == "continuing") {
        
        # Fit a linear model through the trend
        kt_data <- data.frame(kt, years)
        
        m1 <- stats::lm(kt ~ years, data = kt_data[kt_data$years >= first_year, ])
        newdata <- data.frame(years = proj_years)
        newdata$preds <- stats::predict(m1, newdata = newdata)
        
        #x <- kt_data[kt_data$years >= first_year, ]$kt
        #m1 <- forecast::auto.arima(x)
        #f1 <- forecast::forecast(m1, h = length(proj_years))
        #newdata$preds <- as.vector(f1$mean)
        
        kval <- newdata$preds[newdata$years == cont_limit]
        newdata <- newdata[newdata$years <= cont_limit, ]
        kt_proj <- c(kt, newdata$preds, rep(kval, time_horizon - cont_limit))
        #kt_proj <- c(kt, newdata$preds)
        
      }
      
      if(forecast_type == "stationary") {
        
        # Hold last value constant
        kt_proj <- c(kt, rep(kt[length(kt)], length(proj_years)))
        
      }
      
      # Estimate probabilities from forecast fitted values
      
      cqtransfit <- outer(kt_proj, bx_fit)
      qtransfit <- sweep(cqtransfit, 2, age_means, "+")
      fit <- boot::inv.logit(qtransfit)
      
      colnames(fit) <- ages2
      rownames(fit) <- c(years, proj_years)
      
      fit <- as.data.frame(fit)
      fit$year <- c(years, proj_years)
      
      setDT(fit)
      
      fit <- melt(fit, id.vars = "year", variable.name = "age", value.name = forecast_var)
      
      fit[ , age := as.numeric(as.vector(age))]
      fit[ , sex := as.character(sex_i)]
      fit[ , imd_quintile := as.character(imd_quintile_i)]
      
      if(counter == 1) {
        data_proj <- copy(fit)
      } else {
        data_proj <- rbindlist(list(data_proj, copy(fit)), use.names = T)
      }
      
      counter <- counter + 1
    }
  }
  
  data_proj[get(forecast_var) < 0, (forecast_var) := 0]
  data_proj[get(forecast_var) > 1, (forecast_var) := 1]
  
  # duplicate estimates for age_cont_limit up to oldest_age
  
  domain <- data.frame(expand.grid(
    year = union(years, proj_years),
    age = ages1,
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  
  setDT(domain)
  
  domain <- merge(domain, data_proj, by = c("year", "age", "sex", "imd_quintile"), all.x = T, all.y = F)
  
  domain[ , temp := get(forecast_var)[age == age_cont_limit], by = c("year", "sex", "imd_quintile")]
  
  domain[age > age_cont_limit, (forecast_var) := temp]
  
  domain[ , temp := NULL]
  
  
  return(domain[])
}


#' @export
fill.zero <- function(x, method = "constant") {
  #x <- qdat[1,]
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