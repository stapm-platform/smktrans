#' Generate Uncertainty Intervals for Transition Probabilities
#'
#' @description
#' Estimates uncertainty around transition probabilities using a Beta distribution 
#' assumption derived from the binomial variance of the sample size. It generates 
#' correlated Monte Carlo samples (using a Gaussian Copula) to preserve the 
#' relationship between age/cohort groups across iterations.
#'
#' The process follows these steps:
#' 1. \strong{Standard Error}: Calculates SE based on the effective sample size (\eqn{SE = \sqrt{p(1-p)/n}}).
#' 2. \strong{Beta Parameters}: Converts the Mean (p) and Variance (SE^2) into Beta shape parameters (\eqn{\alpha, \beta}).
#' 3. \strong{Correlated Sampling}: Generates a matrix of correlated uniform probabilities using a Gaussian Copula.
#'    \itemize{
#'      \item Common factor \eqn{Z_{shared}} accounts for systematic uncertainty.
#'      \item Idiosyncratic factor \eqn{Z_{indiv}} accounts for random noise.
#'    }
#' 4. \strong{Inverse CDF}: Maps these probabilities through the \code{qbeta} function to get the final samples.
#'
#' @param data Data table. Must contain the probability column.
#' @param prob_col Character. Name of the column containing the point estimate probabilities (e.g., "p_start").
#' @param n_eff Numeric. The effective sample size used to calculate variance. 
#'        Higher \code{n} implies narrower confidence intervals.
#' @param n_samp Integer. Number of Monte Carlo samples to generate (e.g., 100 or 1000).
#' @param correlation Numeric (0-1). The correlation coefficient (\eqn{R}) between rows in a single sample iteration. 
#'        High correlation implies that if one age group is overestimated, others likely are too.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: The original data table with added summary columns 
#'         (\code{_low}, \code{_median}, \code{_high}, \code{_var}).
#'   \item \code{samples}: A matrix of dimensions \code{[nrow(data) x n_samp]} containing the simulated values.
#' }
#' 
#' @import data.table
#' @importFrom stats qnorm pnorm qbeta quantile
#' @export
generate_uncertainty <- function(data, prob_col, n_eff, n_samp, correlation) {
  
  # 1. Validation and Setup
  dt <- copy(data)
  p <- dt[[prob_col]]
  
  # Clamp probabilities to avoid variance = 0 (which breaks Beta calculation)
  # effectively assuming at least 1/1000 risk even if estimate is 0
  epsilon <- 1e-4
  p_clamped <- p
  p_clamped[p_clamped < epsilon] <- epsilon
  p_clamped[p_clamped > (1 - epsilon)] <- 1 - epsilon
  
  # 2. Calculate Beta Parameters (Vectorized)
  # Var = p(1-p) / n
  var_p <- (p_clamped * (1 - p_clamped)) / n_eff
  
  # Method of moments for Beta distribution
  # alpha = ((1 - mu) / var - 1 / mu) * mu^2
  # beta = alpha * (1 / mu - 1)
  term <- (p_clamped * (1 - p_clamped) / var_p) - 1
  alpha <- p_clamped * term
  beta_param <- (1 - p_clamped) * term
  
  # 3. Generate Correlated Random Matrix (Gaussian Copula)
  # We want a matrix [Rows x Samples]
  # U_correlated = pnorm( sqrt(R)*Z_shared + sqrt(1-R)*Z_indiv )
  
  n_rows <- nrow(dt)
  
  # Z_shared: One random value per SAMPLE column (broadcast across all rows)
  # This represents the systematic shift for that specific simulation run
  Z_shared <- matrix(rnorm(n_samp), nrow = n_rows, ncol = n_samp, byrow = TRUE)
  
  # Z_indiv: Unique random value per cell
  Z_indiv <- matrix(rnorm(n_rows * n_samp), nrow = n_rows, ncol = n_samp)
  
  # Combine
  Z_total <- (sqrt(correlation) * Z_shared) + (sqrt(1 - correlation) * Z_indiv)
  
  # Transform to Uniform[0,1]
  U_correlated <- pnorm(Z_total)
  
  # 4. Map to Beta Distribution (Inverse CDF)
  # qbeta is vectorized, so we can pass the matrix U and vectors alpha/beta directly
  # (R recycles alpha/beta columns to match the matrix)
  samples_mat <- qbeta(U_correlated, shape1 = alpha, shape2 = beta_param)
  
  # Handle potential numerical NaNs if p was exactly 0 or 1 originally
  samples_mat[is.na(samples_mat)] <- 0
  
  # 5. Calculate Summaries
  # Row-wise quantiles (much faster than apply)
  # Note: If matrixStats is not available, we use a simple loop or apply 
  # (apply is acceptable here as calculation is lightweight compared to the loops in the old script)
  
  dt[, paste0(prob_col, "_low") := apply(samples_mat, 1, quantile, probs = 0.025, na.rm = TRUE)]
  dt[, paste0(prob_col, "_median") := apply(samples_mat, 1, quantile, probs = 0.50, na.rm = TRUE)]
  dt[, paste0(prob_col, "_high") := apply(samples_mat, 1, quantile, probs = 0.975, na.rm = TRUE)]
  
  # Variance of the samples (Mean of squares - Square of mean)
  row_means <- rowMeans(samples_mat)
  row_means_sq <- rowMeans(samples_mat^2)
  dt[, paste0(prob_col, "_var") := row_means_sq - (row_means^2)]
  
  return(list(data = dt, samples = samples_mat))
}