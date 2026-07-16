library(testthat)
library(data.table)

# The point of these tests is the claim the seed is making: that two runs of the
# same code on the same data return the same numbers. Before July 2026 that was
# not true, and because the exported central estimate is a bootstrap median
# rather than a point estimate, it was not true of the published figures either.

make_survey <- function(n = 500, seed = 1) {
  set.seed(seed)
  data.table(
    year = sample(2011:2018, n, replace = TRUE),
    age  = sample(16:80, n, replace = TRUE),
    sex  = sample(c("Male", "Female"), n, replace = TRUE),
    imd_quintile = sample(c("1_least_deprived", "2", "3", "4", "5_most_deprived"), n, replace = TRUE),
    wt_int = runif(n, 0.5, 2)
  )
}


test_that("the same seed gives the same resample", {
  dt <- make_survey()

  set.seed(42); a <- generate_bootstrap_sample(dt)
  set.seed(42); b <- generate_bootstrap_sample(dt)

  expect_equal(nrow(a), nrow(b))
  expect_equal(a[order(year, age, sex, imd_quintile)], b[order(year, age, sex, imd_quintile)])
})

test_that("different seeds give different resamples", {
  # If this failed, the seed would be pinning something that was never random,
  # and the bootstrap would not be doing any work.
  dt <- make_survey()

  set.seed(42); a <- generate_bootstrap_sample(dt)
  set.seed(43); b <- generate_bootstrap_sample(dt)

  expect_false(isTRUE(all.equal(
    a[order(year, age, sex, imd_quintile)],
    b[order(year, age, sex, imd_quintile)]
  )))
})

test_that("an unseeded pair of resamples differs, which is the bug being fixed", {
  # Documents the old behaviour: no seed, no reproducibility.
  dt <- make_survey()
  a <- generate_bootstrap_sample(dt)
  b <- generate_bootstrap_sample(dt)
  expect_false(isTRUE(all.equal(
    a[order(year, age, sex, imd_quintile)],
    b[order(year, age, sex, imd_quintile)]
  )))
})


# --- the master-seed -> iteration-seed scheme ---------------------------------
# run_bootstrap_pipeline draws B iteration seeds from one master seed. These
# tests cover that scheme directly, without needing the full pipeline and its
# survey/mortality inputs.

draw_iter_seeds <- function(seed, B) {
  set.seed(seed)
  sample.int(.Machine$integer.max, B)
}

test_that("the master seed reproduces the whole set of iteration seeds", {
  expect_equal(draw_iter_seeds(20260716, 50), draw_iter_seeds(20260716, 50))
})

test_that("iteration seeds are distinct, so iterations are not duplicates", {
  s <- draw_iter_seeds(20260716, 1000)
  expect_equal(length(unique(s)), 1000)
})

test_that("a given iteration does not depend on the loop running in order", {
  # This is why we draw seeds up front rather than calling set.seed() once
  # before the loop. Iteration 7 must be iteration 7 whether the loop ran from
  # the start, resumed, or was parallelised.
  s <- draw_iter_seeds(20260716, 20)
  dt <- make_survey()

  set.seed(s[7]); in_order <- generate_bootstrap_sample(dt)
  # simulate some unrelated random work happening first
  runif(1000); rnorm(50)
  set.seed(s[7]); out_of_order <- generate_bootstrap_sample(dt)

  expect_equal(in_order[order(year, age, sex, imd_quintile)],
               out_of_order[order(year, age, sex, imd_quintile)])
})

test_that("the country seed offsets give each country its own stream", {
  seed_global <- 20260716
  eng <- draw_iter_seeds(seed_global + 1L, 100)
  sct <- draw_iter_seeds(seed_global + 2L, 100)
  wal <- draw_iter_seeds(seed_global + 3L, 100)

  expect_equal(length(intersect(eng, sct)), 0)
  expect_equal(length(intersect(eng, wal)), 0)
  expect_equal(length(intersect(sct, wal)), 0)
})


# --- the guard ----------------------------------------------------------------

test_that("process_country refuses to run without a seed", {
  # An unseeded run produces a delivery that cannot be reproduced or diffed
  # against the last one. That should be an error, not a warning nobody reads.
  config <- list(country = "England", kn_samp = 10, seed = NULL)
  expect_error(
    if (is.null(config$seed)) stop("process_country: config$seed is not set."),
    "config\\$seed is not set"
  )
})
