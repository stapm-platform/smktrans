
#' Smooth age and period pattern in probability values
#'
#' Applies a 2D moving average smooth to the trends over age and period, 
#' and fills any remaining missing values by 
#' approximating the age pattern within a particular year.
#'
#' @param data Data table containing "age", "year" and a variable bounded by 0 and 1.
#' @param value_var Character - the name of the variable to be smoothed.
#' @param window_size - Integer - must be an odd number - 
#' the number of years covered by the moving average window.
#' @importFrom data.table := setDT setorderv dcast melt
#' @return Returns a data table the same as data but with smoothed probability values.
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' test_data <- data.frame(expand.grid(age = c(12:40, 42:89), year = 2001:2016))
#' setDT(test_data)
#' test_data[ , prob := runif(nrow(test_data))]
#' test_data[sample(1:nrow(test_data), 100, replace = F), prob := NA]
#' 
#' data_sm <- p_smooth(test_data, "prob", 3)
#' }
p_smooth <- function(
  data,
  value_var,
  window_size = 3
) {
  
  setDT(data)
  
  setorderv(data, c("age", "year"), c(1, 1))

  ages <- min(data$age):max(data$age)
  years <- min(data$year):max(data$year)
  
  if(nrow(data) != length(ages) * length(years)) {
    
    domain <- data.frame(expand.grid(age = ages, year = years))
    setDT(domain)
    data <- merge(domain, data, all.x = T, by = c("age", "year"))
    
  }
  
  # Reshape to wide form with years as columns
  # then make into a matrix
  
  data_wide <- dcast(data, age ~ year, value.var = value_var)
  
  data_wide[ , age := NULL]
  
  data_mat <- as.matrix(data_wide)
  
  # replace 0s and 1s with NA - then fill with approx trend

  data_mat[data_mat <= 0 | data_mat >= 1] <- NA
  
  # smooth values in sliding a n x n window
  r <- raster::raster(data_mat) # convert to rasterLayer
  data_sm <- raster::as.matrix(raster::focal(r, matrix(1, window_size, window_size), mean, pad = T, padValue = NA, na.rm = T))
  
  # Fill any remaining missing values
  # by approximating the age pattern for a particular year
  
  # rows are ages
  
  for(i in 1:length(years)) {
    #cat(i)
    #i <- 1
    x <- data_sm[ ,i]
    tt <- 1:length(x)
    missing <- is.na(x)
    xx <- x[!missing]
    tt <- tt[!missing]
    x <- stats::approx(tt, xx, 1:length(x), method = "constant", f = 0.5, rule = 2)
    data_sm[ , i] <- x$y
    
  }
  
  rownames(data_sm) <- ages
  colnames(data_sm) <- years
  
  data_sm <- as.data.frame(data_sm)
  data_sm$age <- ages
  
  setDT(data_sm)
  
  data_long <- melt(data_sm, id.vars = "age", variable.name = "year", value.name = value_var)
  
  data_long[ , year := as.numeric(as.vector(year))]
  
  
return(data_long[])
}




