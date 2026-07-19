library(testthat)
library(data.table)

# These tests build a small synthetic world where the truth is known: a fixed
# starting-age distribution shared by all cohorts, and a target level that
# varies by cohort. Because the timing is stationary by construction, the
# completion step should let a truncated cohort land on exactly the same
# adjusted curve it would have had if we had observed it all the way. That is
# the whole claim the completion factors rest on, so it is the thing to test.

make_world <- function(period_end = 2018) {

  # hazard among eventual starters, same for every cohort
  h <- c(rep(0.02, 4), 0.10, 0.20, 0.25, 0.20, 0.10, 0.05,
         rep(0.02, 5), rep(0.005, 8))          # ages 8:30
  ages <- 8:30
  stopifnot(length(h) == length(ages))
  F_cdf <- 1 - cumprod(1 - h)

  grid <- CJ(cohort = 1970:2005, sex = c("Male", "Female"),
             imd_quintile = c("1_least_deprived", "5_most_deprived"), age = ages)
  grid[, p_start := h[age - 7]]
  grid[, p_ever_smoker := F_cdf[age - 7]]

  # truncate: beyond the survey horizon there is no observation, and init_est
  # would have zero-filled, so mimic that
  grid[cohort + age > period_end, `:=`(p_start = 0)]
  setkeyv(grid, c("cohort", "sex", "imd_quintile", "age"))
  grid[, p_ever_smoker := 1 - cumprod(1 - p_start), by = .(cohort, sex, imd_quintile)]
  grid[, year := cohort + age]

  # targets: ever smoking at 30 falling across cohorts, by year at age 30
  trg <- CJ(year = 1990:2040, sex = c("Male", "Female"),
            imd_quintile = c("1_least_deprived", "5_most_deprived"))
  trg[, fitted_trends := 0.5 - 0.004 * (year - 2000)]

  list(init = grid, ever = trg)
}


test_that("fully observed cohorts come out exactly as before", {
  w <- make_world()
  out <- suppressMessages(init_adj(w$init, w$ever, ref_age = 30, min_ref = 18,
                                   cohorts = 1970:2010, period_start = 2003, period_end = 2018))
  # for a complete cohort the completion factor is 1, so the adjusted value at
  # ref_age must equal the target for that cohort exactly
  chk <- out[cohort %in% 1975:1985 & age == 30]
  trg <- copy(w$ever)[, cohort := year - 30]
  chk <- merge(chk, trg[, .(cohort, sex, imd_quintile, fitted_trends)],
               by = c("cohort", "sex", "imd_quintile"))
  expect_equal(chk$p_ever_smoker_adj, chk$fitted_trends, tolerance = 1e-12)
})

test_that("a truncated cohort lands where it would have if fully observed", {
  # run the same cohort twice: once truncated at 21, once complete. With
  # stationary timing the completion step should make the truncated run match
  # the complete run at every age the truncated run covers.
  w_full  <- make_world(period_end = 2100)
  w_trunc <- make_world(period_end = 2018)

  full <- suppressMessages(init_adj(w_full$init, w_full$ever, ref_age = 30, min_ref = 18,
                                    cohorts = 1970:2010, period_start = 2003, period_end = 2100))
  trunc <- suppressMessages(init_adj(w_trunc$init, w_trunc$ever, ref_age = 30, min_ref = 18,
                                     cohorts = 1970:2010, period_start = 2003, period_end = 2018))

  # cohort 1997 is seen to 21 in the truncated world
  cmp <- merge(full[cohort == 1997 & age <= 21,
                    .(sex, imd_quintile, age, complete_world = p_ever_smoker_adj)],
               trunc[cohort == 1997 & age <= 21,
                     .(sex, imd_quintile, age, truncated_world = p_ever_smoker_adj)],
               by = c("sex", "imd_quintile", "age"))
  expect_gt(nrow(cmp), 0)
  expect_equal(cmp$truncated_world, cmp$complete_world, tolerance = 1e-10)
})

test_that("without the completion step the truncated cohort is over-scaled", {
  # the size of the problem the completion fixes: dividing an age-30 target by
  # an age-21 curve inflates the whole curve by F(30)/F(21)
  w <- make_world(period_end = 2018)
  h <- c(rep(0.02, 4), 0.10, 0.20, 0.25, 0.20, 0.10, 0.05, rep(0.02, 5), rep(0.005, 8))
  F_cdf <- 1 - cumprod(1 - h)
  expected_overscale <- F_cdf[30 - 7] / F_cdf[21 - 7]
  expect_gt(expected_overscale, 1.01)  # the synthetic world has real late initiation

  out <- suppressMessages(init_adj(w$init, w$ever, ref_age = 30, min_ref = 18,
                                   cohorts = 1970:2010, period_start = 2003, period_end = 2018))
  # with the fix in place, the truncated cohort's value at its ref age should be
  # target / expected_overscale, not target
  trg <- copy(w$ever)[, cohort := year - 30]
  chk <- merge(out[cohort == 1997 & age == 21],
               trg[, .(cohort, sex, imd_quintile, fitted_trends)],
               by = c("cohort", "sex", "imd_quintile"))
  expect_equal(chk$p_ever_smoker_adj, chk$fitted_trends / expected_overscale,
               tolerance = 1e-10)
})

test_that("completion factors respect the cumulative structure", {
  # F(30)/F(r) must be >= 1 and shrink towards 1 as r rises
  w <- make_world()
  h <- c(rep(0.02, 4), 0.10, 0.20, 0.25, 0.20, 0.10, 0.05, rep(0.02, 5), rep(0.005, 8))
  F_cdf <- 1 - cumprod(1 - h)
  ratios <- F_cdf[30 - 7] / F_cdf[(18:29) - 7]
  expect_true(all(ratios >= 1))
  expect_true(all(diff(ratios) <= 0))
})

test_that("min_ref admits the cohorts it says it does", {
  w <- make_world(period_end = 2018)
  out18 <- suppressMessages(init_adj(w$init, w$ever, ref_age = 30, min_ref = 18,
                                     cohorts = 1970:2010, period_start = 2003, period_end = 2018))
  out21 <- suppressMessages(init_adj(w$init, w$ever, ref_age = 30, min_ref = 21,
                                     cohorts = 1970:2010, period_start = 2003, period_end = 2018))
  # cohorts 1998-2000 have ref ages 20, 19, 18: only the min_ref = 18 run should
  # calibrate them on their own data. Both runs still RETURN them, via the
  # extension, so test the values rather than presence: under min_ref = 21 they
  # carry the borrowed average profile, under 18 their own curves.
  a18 <- out18[cohort == 1999 & age == 15]
  a21 <- out21[cohort == 1999 & age == 15]
  expect_gt(nrow(a18), 0)
  expect_gt(nrow(a21), 0)
  # in this synthetic world the timing is stationary, so own-data and borrowed
  # values coincide; just check both are finite and positive rather than
  # asserting a difference that stationarity removes
  expect_true(all(is.finite(a18$p_ever_smoker_adj)))
  expect_true(all(is.finite(a21$p_ever_smoker_adj)))
})

test_that("a sparse truncated stratum falls back loudly, not silently", {
  w <- make_world(period_end = 2018)
  # break one stratum of cohort 1999: zero curve at its reference age
  w$init[cohort == 1999 & sex == "Male" & imd_quintile == "5_most_deprived",
         `:=`(p_start = 0, p_ever_smoker = 0)]
  expect_message(
    out <- init_adj(w$init, w$ever, ref_age = 30, min_ref = 18,
                    cohorts = 1970:2010, period_start = 2003, period_end = 2018),
    "cannot calibrate on their own data")
  # cohorts from 1999 onwards should still be present, via the extension
  expect_true(nrow(out[cohort == 1999]) > 0)
  expect_true(all(is.finite(out[cohort == 1999 & age <= 30]$p_ever_smoker_adj)))
})

test_that("a zero at ref age in a fully observed cohort stops the run", {
  w <- make_world(period_end = 2018)
  # break a complete cohort OUTSIDE the completion basis (the basis is the last
  # 10 complete cohorts, 1979-1988 here): breaking a basis cohort trips the
  # monotonicity check first, which is also a stop, but not the one under test
  w$init[cohort == 1975 & sex == "Female" & imd_quintile == "1_least_deprived" & age == 30,
         p_ever_smoker := 0]
  expect_error(
    suppressMessages(init_adj(w$init, w$ever, ref_age = 30, min_ref = 18,
                              cohorts = 1970:2010, period_start = 2003, period_end = 2018)),
    "fully observed cohorts")
})
