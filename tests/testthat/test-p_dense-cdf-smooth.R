library(testthat)
library(data.table)

# These tests cover the change made in July 2026 to smooth the cumulative curve
# over age before differencing it, rather than differencing a step function and
# then trying to repair the resulting zeros in p_smooth.
#
# The thing being protected against is not a wrong answer, it is a chaotic one.
# The old route amplified 0.1% of noise on the input into a 51% change in the
# published initiation probabilities, which is how the April and July 2026 runs
# came out looking so different with no change to the estimation code.

# A step-function cumulative curve of the kind init_adj actually produces:
# rises over the teens, then goes flat because nobody reports starting at 26.
make_cdf <- function(cohort = 1985, sex = "Male", imd = "1_least_deprived") {
  age <- 10:30
  p <- c(0, 0, 0.035, 0.086, 0.140, 0.213, 0.293, 0.327, 0.349, 0.368, 0.375,
         0.393, 0.393, 0.393, 0.393, 0.393, 0.399, 0.399, 0.399, 0.399, 0.399)
  data.table(cohort = cohort, sex = sex, imd_quintile = imd,
             age = age, year = cohort + age, p_ever_smoker_adj = p)
}


test_that("smooth_cdf returns a monotone non-decreasing curve", {
  d <- make_cdf()
  out <- smooth_cdf(d$p_ever_smoker_adj, d$age, df = 6)
  expect_false(is.unsorted(out))
  expect_true(all(out >= 0 & out <= 1))
  expect_equal(length(out), nrow(d))
})

test_that("smooth_cdf removes the flat runs that produce zero densities", {
  d <- make_cdf()
  raw <- d$p_ever_smoker_adj
  sm  <- smooth_cdf(raw, d$age, df = 6)

  # the raw curve is flat across ages 22-25 and 27-30
  expect_true(sum(diff(raw) == 0) > 5)
  # the smoothed one should be flat in fewer places
  expect_lt(sum(diff(sm) == 0), sum(diff(raw) == 0))
})

test_that("smooth_cdf keeps the overall level, it is a smooth not a rescale", {
  d <- make_cdf()
  sm <- smooth_cdf(d$p_ever_smoker_adj, d$age, df = 6)
  # the value at the reference age is what init_adj calibrated against, so it
  # should not move much
  expect_equal(sm[d$age == 30], d$p_ever_smoker_adj[d$age == 30], tolerance = 0.05)
})

test_that("smooth_cdf hands back short or flat inputs untouched", {
  expect_equal(smooth_cdf(c(0, 0, 0), 10:12), c(0, 0, 0))          # too few points
  expect_equal(smooth_cdf(rep(0.4, 21), 10:30), rep(0.4, 21))      # flat curve
})

test_that("the differenced density is non-negative by construction", {
  d <- make_cdf()
  sm <- smooth_cdf(d$p_ever_smoker_adj, d$age, df = 6)
  pdf <- 1 - ((1 - data.table::shift(sm, type = "lead")) / (1 - sm))
  expect_true(all(pdf[!is.na(pdf)] >= 0))
})


test_that("p_dense with cdf_smooth_df = NULL reproduces the old behaviour", {
  # This is the escape hatch for comparing against previously published runs.
  # If this ever fails, the old path has been changed by accident.
  d <- rbindlist(lapply(1973:2008, make_cdf))
  old <- p_dense(copy(d), "p_ever_smoker_adj", lowest_year = 2003, cdf_smooth_df = NULL)
  expect_s3_class(old, "data.table")
  expect_true("p_start" %in% names(old))
})

test_that("p_dense errors rather than guessing when columns are missing", {
  d <- make_cdf()
  expect_error(p_dense(copy(d)[, age := NULL], "p_ever_smoker_adj"), "must contain")
  expect_error(p_dense(copy(d), "not_a_column"), "not a column")
})

test_that("p_dense does not depend on the incoming row order", {
  # shift() takes the next row as the next age. Before this change p_dense was
  # relying on init_adj returning sorted rows, which it happens to do.
  d <- rbindlist(lapply(1973:2008, make_cdf))
  a <- p_dense(copy(d), "p_ever_smoker_adj", lowest_year = 2003)
  b <- p_dense(copy(d)[sample(.N)], "p_ever_smoker_adj", lowest_year = 2003)
  setorderv(a, c("sex", "imd_quintile", "age", "year"))
  setorderv(b, c("sex", "imd_quintile", "age", "year"))
  expect_equal(a$p_start, b$p_start, tolerance = 1e-10)
})


test_that("smoothing the curve first makes p_dense stable to small input noise", {
  # The regression test for the whole point of the change. Perturb the input by
  # 0.1% and the output should move by about the same, not by 50%.
  set.seed(1)
  d <- rbindlist(lapply(1973:2008, make_cdf))
  d <- rbindlist(list(d, copy(d)[, sex := "Female"]))

  pert <- copy(d)
  pert[, p_ever_smoker_adj := pmin(pmax(p_ever_smoker_adj * (1 + rnorm(.N, 0, 0.001)), 0), 1)]

  base <- p_dense(copy(d),    "p_ever_smoker_adj", lowest_year = 2003)
  alt  <- p_dense(copy(pert), "p_ever_smoker_adj", lowest_year = 2003)

  m <- merge(base, alt, by = c("age", "year", "sex", "imd_quintile"),
             suffixes = c("_b", "_a"))
  m <- m[p_start_b > 1e-4]
  amp <- median(abs((m$p_start_a - m$p_start_b) / m$p_start_b)) / 0.001

  # was ~500x on the England data with the old route
  expect_lt(amp, 20)
})


test_that("p_smooth blank_zeros = FALSE leaves no NAs and no exact zeros", {
  dt <- data.table(expand.grid(age = 10:30, year = 2003:2018))
  set.seed(2)
  dt[, prob := runif(.N, 0, 0.05)]
  dt[age > 22, prob := 0]   # the flat tail that causes the trouble

  keep <- p_smooth(copy(dt), "prob", 5, blank_zeros = FALSE)
  expect_false(anyNA(keep$prob))
  expect_true(all(keep$prob > 0))
})

test_that("p_smooth still defaults to the old behaviour so relapse is unaffected", {
  dt <- data.table(expand.grid(age = 10:30, year = 2003:2018))
  set.seed(3)
  dt[, prob := runif(.N, 0, 0.05)]
  dt[age > 22, prob := 0]

  a <- p_smooth(copy(dt), "prob", 5)
  b <- p_smooth(copy(dt), "prob", 5, blank_zeros = TRUE)
  expect_equal(a$prob, b$prob)
})
