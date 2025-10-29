
# The aim of this code is to produce an estimate of the uncertainty in the estimated transition probabilities

# A full characterisation of the empirical uncertainty in all components of the estimated method has not been designed or implemented

# Instead the method below is designed to provide an approximate estimate of the uncertainty that might be expected 
# given the sample size of survey data typically used, and the division of this sample size between single years of age, calendar years, sex and IMD subgroup

# The key assumptions are represented by parameters that can be changed to explore the sensitivity of the uncertainty estimates to these assumptions

# For each type of transition probability, varying assumptions about the correlation of uncertainty among the individual transition probabilities is possible

# When combining uncertainty across initiation, quitting and relapse
# choose common percentiles of the sampled distributions to represent high, low and medium scenarios

# Set country
#path <- "transition_probability_estimates/src_england/"
#country <- "England"

# Load functions

var_func <- function(x) {
  mean(x ^ 2) - (mean(x) ^ 2)
}

# Function to estimate beta distribution parameters from mean and variance
estimate_beta_params <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu^2
  beta <- alpha * (1 / mu - 1)
  return(list(alpha = alpha, beta = beta))
}

# Function to control the correlation between samples of different parameters
SampleFnc <- function(R, K) {
  X <- sqrt(R) * rnorm(1)
  Y <- X + sqrt(1 - R) * rnorm(K)
  return(pnorm(Y))
}

#R <- 0
#K.vec <- SampleFnc(R, 2)

BetaSamp <- function(alpha, beta, U) {
  x1 <- seq(0.001, 1 - 0.001, length.out = 1000)
  x2 <- dbeta(x1, alpha, beta)
  x3 <- cumsum(x2 / sum(x2))
  x4 <- x1[findInterval(U, x3, all.inside = T)]
  return(x4)
}

# Generate samples from the estimated beta distribution
#samples <- rbeta(10000, beta_params$alpha, beta_params$beta)

#BetaSamp(5, 90, SampleFnc(0.5, 10))

# Function to conduct the sampling
genUncertainty <- function(data, var_name, n, n_samp, R) {
  
  # Calculate the binomial standard error
  data[ , se := sqrt(get(var_name) * (1 - get(var_name)) / n)]
  
  # Approximate the variance of the beta distribution by squaring the standard error
  data[ , var := se ^ 2]
  
  # Calculate the beta parameters
  data[ , alpha := estimate_beta_params(get(var_name), var)$alpha]
  data[ , beta := estimate_beta_params(get(var_name), var)$beta]
  
  # Generate samples from the beta distribution
  
  # Matrix to store the outputs
  sample_mat <- matrix(0, nrow = nrow(data), ncol = n_samp)
  
  # Sampling loop
  # Look at ways to make this run faster
  for(cn in 1:n_samp) {
    
    # Generate a vector of correlated percentiles for use in sampling from the distributions
    data[ , percentile := SampleFnc(R, nrow(data))]
    
    for(rn in 1:nrow(data)) {
      
      # rn <- 1
      
      sample_mat[rn, cn] <- BetaSamp(data[rn, alpha], data[rn, beta], data[rn, percentile])
    }
    cat(cn, "\n")
  }
  
  return(sample_mat)
}

#################################################
# Do the sampling

# kn <- 100
# kn_samp <- 100
# kR <- 0.95

##
# Initiation

set.seed(2023)

# Load data
init_data <- readRDS(paste0(path, "outputs/init_forecast_data_", country, ".rds"))

init_data[p_start == 0, p_start := 1/1000]

# Generate samples
init_mat <- genUncertainty(init_data, "p_start", n = kn, n_samp = kn_samp, R = kR)

# Add the key sample quantiles to the data
init_data[ , p_start_low := apply(init_mat, 1, quantile, 0.025)]
init_data[ , p_start_median := apply(init_mat, 1, quantile, 0.5)]
init_data[ , p_start_high := apply(init_mat, 1, quantile, 0.975)]

stapmr::WriteToExcel(wb, sheet = "Initiation",
                     title = "Probabilities of smoking initiation (never to current smoker)",
                     init_data, startCol = 1, startRow = 1)

saveRDS(init_data, paste0(path, "outputs/init_data_", country, "_uncertainty.rds"))

rm(init_mat, init_data)#
gc()

##
# Relapse

set.seed(2023)

# Load data
relapse_data <- readRDS(paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

relapse_data[p_relapse == 0, p_relapse := 1/1000]

# Generate samples
relapse_mat <- genUncertainty(relapse_data, "p_relapse", n = kn, n_samp = kn_samp, R = kR)

# Add the key sample quantiles to the data
relapse_data[ , p_relapse_low := apply(relapse_mat, 1, quantile, 0.025)]
relapse_data[ , p_relapse_median := apply(relapse_mat, 1, quantile, 0.5)]
relapse_data[ , p_relapse_high := apply(relapse_mat, 1, quantile, 0.975)]

stapmr::WriteToExcel(wb, sheet = "Relapse",
                     title = "Probabilities of relapse to smoking (former to current smoker). Added stratification by years since quitting.",
                     relapse_data, startCol = 1, startRow = 1)

saveRDS(relapse_data, paste0(path, "outputs/relapse_data_", country, "_uncertainty.rds"))

rm(relapse_mat, relapse_data)#
gc()

##
# Quit

set.seed(2023)

# Load data
quit_data <- readRDS(paste0(path, "outputs/quit_forecast_data_", country, ".rds"))

quit_data[p_quit == 0, p_quit := 1/1000]

# Generate samples
quit_mat <- genUncertainty(data = quit_data, var_name = "p_quit", n = kn, n_samp = kn_samp, R = kR)

saveRDS(quit_mat, paste0(path, "outputs/quit_mat_", country, "_uncertainty.rds"))



# Add the key sample quantiles to the data
quit_data[ , p_quit_low := apply(quit_mat, 1, quantile, 0.025)]
quit_data[ , p_quit_median := apply(quit_mat, 1, quantile, 0.5)]
quit_data[ , p_quit_high := apply(quit_mat, 1, quantile, 0.975)]
quit_data[ , p_quit_var := apply(quit_mat, 1, var_func)]

#stapmr::WriteToExcel(wb, sheet = "Quit",
#                     title = "Probabilities of quitting smoking (current to former smoker).",
#                     quit_data, startCol = 1, startRow = 1)

saveRDS(quit_data, paste0(path, "outputs/quit_data_", country, "_uncertainty.rds"))

rm(quit_mat, quit_data)#
gc()


