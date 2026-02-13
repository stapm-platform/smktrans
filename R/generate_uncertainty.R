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
  
  # 1. Setup
  dt <- copy(data)
  setDT(dt)
  
  p_vec <- dt[[prob_col]]
  n_rows <- length(p_vec)
  
  # 2. Vectorized Clamping
  # Prevent variance from being zero
  epsilon <- 1e-4
  p_clamped <- pmax(epsilon, pmin(p_vec, 1 - epsilon))
  
  # 3. Calculate Beta Parameters (Vectorized)
  # Var = p(1-p) / n
  term_num <- p_clamped * (1 - p_clamped)
  var_p <- term_num / n_eff
  
  # Method of moments
  # alpha = ((1 - mu) / var - 1 / mu) * mu^2
  # Simplifies to: alpha = mu * ( (mu(1-mu)/var) - 1 )
  common_factor <- (term_num / var_p) - 1
  alpha <- p_clamped * common_factor
  beta_param <- (1 - p_clamped) * common_factor
  
  # 4. Generate Correlated Random Matrix
  # We use vector recycling to build the matrix implicitly, which is faster.
  # Z_shared: Represents the systematic shift for a specific simulation run.
  # We want the same random value for the entire COLUMN (Sample), repeated N_ROW times.
  # generating n_samp values and repeating each one n_rows times ensures
  # that when R fills the matrix (Column Major), the columns are uniform.
  z_shared_vec <- rep(rnorm(n_samp), each = n_rows)
  
  # Z_indiv: Unique random value per cell
  z_indiv_vec <- rnorm(n_rows * n_samp)
  
  # Combine using weights
  # sqrt(R) * Shared + sqrt(1-R) * Indiv
  z_total <- (sqrt(correlation) * z_shared_vec) +
    (sqrt(1 - correlation) * z_indiv_vec)
  
  # Transform to Uniform[0,1]
  u_correlated <- pnorm(z_total)
  
  # 5. Map to Beta Distribution (Inverse CDF)
  # qbeta recycles the alpha/beta vectors to match the length of u_correlated.
  # Since u_correlated is effectively a long vector of columns, and alpha/beta
  # correspond to rows, we just need to ensure alignment.
  # However, qbeta recycles alpha/beta linearly.
  # u_correlated structure: [Row1-Samp1, Row2-Samp1 ... Row1-Samp2, Row2-Samp2]
  # alpha structure:        [Row1, Row2 ... RowN]
  # This alignment is perfect. We do not need to expand alpha/beta manually.
  samples_vec <- qbeta(u_correlated, shape1 = alpha, shape2 = beta_param)
  
  # Reshape into matrix [Rows x Samples] for summary calculation
  samples_mat <- matrix(samples_vec, nrow = n_rows, ncol = n_samp)
  
  # Handle numerical issues (NaNs turn to 0)
  samples_mat[is.na(samples_mat)] <- 0
  
  # 6. Calculate Summaries (High Performance)
  # using matrixStats to avoid the slow apply() loop
  if (requireNamespace("matrixStats", quietly = TRUE)) {
    
    quants <- matrixStats::rowQuantiles(samples_mat, probs = c(0.025, 0.5, 0.975))
    
    vars <- matrixStats::rowVars(samples_mat)
    
    dt[, (paste0(prob_col, "_low")) := quants[, 1]]
    dt[, (paste0(prob_col, "_median")) := quants[, 2]]
    dt[, (paste0(prob_col, "_high")) := quants[, 3]]
    dt[, (paste0(prob_col, "_var")) := vars]
    
  } else {
    warning("Package 'matrixStats' not found. Falling back to slow base R 'apply'.")
    
    dt[, (paste0(prob_col, "_low")) := apply(samples_mat, 1, quantile, probs = 0.025, na.rm = TRUE)]
    dt[, (paste0(prob_col, "_median")) := apply(samples_mat, 1, quantile, probs = 0.50, na.rm = TRUE)]
    dt[, (paste0(prob_col, "_high")) := apply(samples_mat, 1, quantile, probs = 0.975, na.rm = TRUE)]
    dt[, (paste0(prob_col, "_var")) := apply(samples_mat, 1, var, na.rm = TRUE)]
    
  }
  
  return(list(data = dt[], samples = samples_mat))
}