library(testthat)
library(data.table)

# Synthetic survey with a known structure. Smoking status depends on age, year
# and sex so the model has something real to find, but the point of these tests
# is the contract trend_fit offers, not the quality of the fit.
make_survey <- function(n = 8000, seed = 1, years = 2003:2018, ages = 11:89) {
  set.seed(seed)
  dt <- data.table(
    age  = sample(ages, n, replace = TRUE),
    year = sample(years, n, replace = TRUE),
    sex  = sample(c("Male", "Female"), n, replace = TRUE),
    imd_quintile = sample(c("1_least_deprived", "2", "3", "4", "5_most_deprived"), n, replace = TRUE)
  )
  p_current <- plogis(-1 - 0.02 * dt$age + 0.05 * (dt$year - 2010) + 0.3 * (dt$sex == "Male"))
  p_former  <- plogis(-2 + 0.03 * dt$age)
  dt[, smk.state := mapply(function(pc, pf) {
    sample(c("current", "former", "never"), 1, prob = c(pc, pf, max(1 - pc - pf, 0.01)))
  }, p_current, p_former)]
  dt[, wt_int := runif(n, 0.5, 2)]
  # make sure every level appears, so the pinning checks are not tripped by chance
  stopifnot(uniqueN(dt$smk.state) == 3, uniqueN(dt$imd_quintile) == 5)
  dt[]
}

ages  <- 11:89
years <- 2003:2018
imds  <- c("1_least_deprived", "2", "3", "4", "5_most_deprived")

fit <- function(dt, ...) trend_fit(dt, grid_ages = ages, grid_years = years, grid_imd = imds, ...)


# --- Shape and content of the grid --------------------------------------------

test_that("the grid has exactly one row per age/year/sex/IMD combination", {
  out <- fit(make_survey())
  expect_equal(nrow(out), length(ages) * length(years) * 2 * length(imds))
  expect_equal(uniqueN(out, by = c("age", "year", "sex", "imd_quintile")), nrow(out))
  expect_true(all(c("current", "former", "never", "cohort") %in% names(out)))
})

test_that("probabilities are valid and sum to one", {
  p <- as.matrix(fit(make_survey())[, .(current, former, never)])
  expect_true(all(p >= 0 & p <= 1))
  expect_true(all(abs(rowSums(p) - 1) < 1e-8))
})

test_that("cohort is year minus age", {
  out <- fit(make_survey())
  expect_equal(out$cohort, out$year - out$age)
})

test_that("the state columns come back in the order we asked for", {
  # predict() orders columns by factor level. If that ever changed, 'current'
  # would silently become 'former' everywhere downstream.
  out <- fit(make_survey())
  expect_equal(names(out)[(ncol(out) - 2):ncol(out)], c("current", "former", "never"))
})


# --- The bit that matters for bootstrapping ------------------------------------

test_that("the grid does not depend on the range of the data supplied", {
  # This is the failure being guarded against. Resample away the youngest ages
  # and the earliest year and the old code returned a smaller grid without
  # complaining, so replicates could not be stacked.
  full    <- fit(make_survey(seed = 1))
  narrow  <- make_survey(seed = 2)[age >= 25 & year >= 2006]
  clipped <- fit(narrow, allow_extrapolation = TRUE)

  expect_equal(nrow(clipped), nrow(full))
  expect_equal(clipped$age,  full$age)
  expect_equal(clipped$year, full$year)
})

test_that("two replicates stack into a rectangle", {
  b1 <- fit(make_survey(seed = 11), boot_id = 1L)
  b2 <- fit(make_survey(seed = 12), boot_id = 2L)
  stacked <- rbindlist(list(b1, b2))

  expect_equal(nrow(stacked), 2 * nrow(b1))
  expect_equal(sort(unique(stacked$boot_id)), c(1L, 2L))
  expect_true(all(stacked[, .N, by = .(age, year, sex, imd_quintile)]$N == 2L))
})

test_that("different resamples give different numbers", {
  # If replicates came back identical the bootstrap would be doing nothing.
  b1 <- fit(make_survey(seed = 21))
  b2 <- fit(make_survey(seed = 22))
  expect_false(isTRUE(all.equal(b1$current, b2$current)))
})


# --- Failures are loud ---------------------------------------------------------

test_that("a missing IMD quintile is an error, not a smaller grid", {
  expect_error(fit(make_survey()[imd_quintile != "3"]), "lost a quintile")
})

test_that("a missing smoking state is an error", {
  expect_error(fit(make_survey()[smk.state != "former"]), "do not match expected_states")
})

test_that("missing values are reported rather than dropped", {
  dt <- make_survey()
  dt[1:5, age := NA_integer_]
  expect_error(fit(dt), "5 of .* rows have a missing value")
})

test_that("non-convergence is an error", {
  expect_error(fit(make_survey(), max_iterations = 1), "did not converge")
})

test_that("a missing input column is named in the error", {
  dt <- make_survey()
  dt[, wt_int := NULL]
  expect_error(trend_fit(dt), "wt_int")
})


# --- Extrapolation has to be asked for -----------------------------------------

test_that("reaching past the last survey year is refused by default", {
  expect_error(
    trend_fit(make_survey(), grid_ages = ages, grid_years = 2003:2019, grid_imd = imds),
    "reaches outside the data.*2019"
  )
})

test_that("reaching past the last survey year works when allowed, and names the year", {
  # This is what England does for 2019 until HSE 2019 lands.
  out <- trend_fit(make_survey(), grid_ages = ages, grid_years = 2003:2019,
                   grid_imd = imds, allow_extrapolation = TRUE)
  expect_equal(sort(unique(out$year)), 2003:2019)
  expect_equal(attr(out, "extrapolated_years"), 2019)
  expect_equal(attr(out, "extrapolated_ages"), integer(0))
  # the extrapolated year is still a valid probability distribution
  p <- as.matrix(out[year == 2019, .(current, former, never)])
  expect_true(all(p >= 0 & p <= 1))
  expect_true(all(abs(rowSums(p) - 1) < 1e-8))
})

test_that("a grid inside the data is not treated as extrapolation", {
  out <- trend_fit(make_survey(), grid_ages = 25:74, grid_years = 2011:2018, grid_imd = imds)
  expect_equal(attr(out, "extrapolated_years"), integer(0))
})


# --- thin_trend_draws ----------------------------------------------------------

test_that("thinning keeps exactly the requested cells and columns", {
  full <- fit(make_survey(seed = 5), boot_id = 3L)
  thin <- thin_trend_draws(full, keep_ages = 25:74, keep_years = 2011:2018, keep_states = "current")

  expect_equal(nrow(thin), 50 * 8 * 2 * 5)
  expect_equal(names(thin), c("boot_id", "age", "year", "sex", "imd_quintile", "current"))
  expect_equal(sort(unique(thin$age)), 25:74)
  expect_equal(sort(unique(thin$year)), 2011:2018)
  # values are unchanged, just fewer of them
  chk <- merge(thin, full[, .(age, year, sex, imd_quintile, full_current = current)],
               by = c("age", "year", "sex", "imd_quintile"))
  expect_equal(chk$current, chk$full_current)
})

test_that("asking for a year that is not in the grid is an error, not an empty table", {
  full <- fit(make_survey(seed = 6))
  expect_error(thin_trend_draws(full, 25:74, 2011:2019), "years not in the trend grid: 2019")
})

test_that("asking for a state column that does not exist is an error", {
  full <- fit(make_survey(seed = 7))
  expect_error(thin_trend_draws(full, 25:74, 2011:2018, keep_states = "vaping"),
               "state columns not in the trend grid")
})


# --- aggregate_uncertainty -----------------------------------------------------

make_boot_trends <- function(B = 50, seed = 3) {
  set.seed(seed)
  grid <- CJ(age = 30:32, year = 2011:2012, sex = c("Male", "Female"),
             imd_quintile = c("1_least_deprived", "5_most_deprived"),
             smk.state = c("current", "former", "never"),
             boot_id = seq_len(B))
  grid[, probability := runif(.N, 0.05, 0.5)]
  grid[]
}

test_that("aggregation groups by smk.state as well as the demographics", {
  boot <- make_boot_trends()
  agg <- aggregate_uncertainty(boot, "probability", extra_keys = "smk.state")

  expect_equal(nrow(agg), uniqueN(boot, by = c("age", "year", "sex", "imd_quintile", "smk.state")))
  expect_true(all(c("probability", "probability_lower", "probability_upper", "probability_se") %in% names(agg)))
})

test_that("without extra_keys the three states would be collapsed together", {
  # Documenting why extra_keys exists: one third the rows, silently averaging
  # current, former and never into a single number.
  boot <- make_boot_trends()
  wrong <- aggregate_uncertainty(boot, "probability")
  right <- aggregate_uncertainty(boot, "probability", extra_keys = "smk.state")
  expect_equal(nrow(right), 3 * nrow(wrong))
})

test_that("the central estimate sits inside its own interval", {
  agg <- aggregate_uncertainty(make_boot_trends(), "probability", extra_keys = "smk.state")
  expect_true(all(agg$probability_lower <= agg$probability))
  expect_true(all(agg$probability <= agg$probability_upper))
  expect_true(all(agg$probability_se > 0))
})

test_that("a cell that is short of draws is an error", {
  boot <- make_boot_trends()
  boot <- boot[!(age == 30 & boot_id %in% 1:10)]
  expect_error(aggregate_uncertainty(boot, "probability", extra_keys = "smk.state"), "fewer than")
})

test_that("NA draws are counted as missing draws", {
  boot <- make_boot_trends()
  boot[age == 31 & boot_id == 1, probability := NA_real_]
  expect_error(aggregate_uncertainty(boot, "probability", extra_keys = "smk.state"), "fewer than")
})


# --- The target arithmetic itself ----------------------------------------------
# These do not call the production script, they check the maths it relies on.

test_that("population weighting is not the same as an unweighted mean", {
  # If it were, the choice of weights would not matter and I would not have
  # bothered switching from survey weights to ONS populations.
  dt <- data.table(
    age = c(30, 60), N = c(1e6, 1e5), current = c(0.30, 0.10)
  )
  expect_equal(weighted.mean(dt$current, dt$N), (1e6 * 0.30 + 1e5 * 0.10) / 1.1e6)
  expect_false(isTRUE(all.equal(weighted.mean(dt$current, dt$N), mean(dt$current))))
})

test_that("the covariance diagonal equals the variances", {
  set.seed(9)
  B <- 200
  wide <- data.table(P_001 = rnorm(B), P_002 = rnorm(B))
  wide[, P_003 := 0.7 * P_001 + rnorm(B, sd = 0.3)]  # correlated with the first
  cm <- cov(wide)

  expect_equal(diag(cm), sapply(wide, var), ignore_attr = TRUE)
  expect_true(cm["P_001", "P_003"] > 0)              # correlation survives
  expect_equal(cm["P_001", "P_003"], cm["P_003", "P_001"])
  expect_true(min(eigen(cm, symmetric = TRUE, only.values = TRUE)$values) > 0)
})

test_that("a duplicated target makes the covariance matrix singular", {
  # This is the failure the positive-definiteness check in the targets script
  # is there to catch: two targets that are the same column twice.
  set.seed(10)
  wide <- data.table(P_001 = rnorm(200))
  wide[, P_002 := P_001]
  cm <- cov(wide)
  expect_true(min(eigen(cm, symmetric = TRUE, only.values = TRUE)$values) < 1e-12)
})
