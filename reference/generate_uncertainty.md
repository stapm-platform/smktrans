# Generate Uncertainty Intervals for Transition Probabilities (deprecated)

Estimates uncertainty around transition probabilities using a Beta
distribution assumption derived from the binomial variance of the sample
size. It generates correlated Monte Carlo samples (using a Gaussian
Copula) to preserve the relationship between age/cohort groups across
iterations.

The process follows these steps: 1. **Standard Error**: Calculates SE
based on the effective sample size (\\SE = \sqrt{p(1-p)/n}\\). 2. **Beta
Parameters**: Converts the Mean (p) and Variance (SE^2) into Beta shape
parameters (\\\alpha, \beta\\). 3. **Correlated Sampling**: Generates a
matrix of correlated uniform probabilities using a Gaussian Copula.

- Common factor \\Z\_{shared}\\ accounts for systematic uncertainty.

- Idiosyncratic factor \\Z\_{indiv}\\ accounts for random noise.

4\. **Inverse CDF**: Maps these probabilities through the `qbeta`
function to get the final samples.

## Usage

``` r
generate_uncertainty(data, prob_col, n_eff, n_samp, correlation)
```

## Arguments

- data:

  Data table. Must contain the probability column.

- prob_col:

  Character. Name of the column containing the point estimate
  probabilities (e.g., "p_start").

- n_eff:

  Numeric. The effective sample size used to calculate variance. Higher
  `n` implies narrower confidence intervals.

- n_samp:

  Integer. Number of Monte Carlo samples to generate (e.g., 100 or
  1000).

- correlation:

  Numeric (0-1). The correlation coefficient (\\R\\) between rows in a
  single sample iteration. High correlation implies that if one age
  group is overestimated, others likely are too.

## Value

A list containing:

- `data`: The original data table with added summary columns (`_low`,
  `_median`, `_high`, `_var`).

- `samples`: A matrix of dimensions `[nrow(data) x n_samp]` containing
  the simulated values.
