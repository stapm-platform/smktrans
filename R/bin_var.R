#' Bin numeric variable
#' 
#' Converts a numeric variable to bins of specified number, 
#' where the binned variable is also numeric and has the mid-points of the bins.
#' 
#'
#' @param x Numeric variable to be binned
#' @param n_bins Integer - the number of bins to create
#' @importFrom data.table data.table :=
#' @return Returns a numeric binned variable
#' @export
#'
#' @examples
#' bin_var(1:10, 3)
#' 
#' 
bin_var <- function(x, n_bins) {
  
  start <- min(x, na.rm = T)
  stop <- max(x, na.rm = T)
  
  x1 <- findInterval(x, seq(start, stop, length = n_bins), all.inside = T)
  
  xd <- data.table(x = x, x1 = x1)
  
  xd[ , med := as.double(1)]
  xd[ , med := as.double(median(x)), by = "x1"]
  
return(xd[ , med])
}