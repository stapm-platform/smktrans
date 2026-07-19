library(testthat)
library(data.table)

# Synthetic survey microdata with a known trend structure. The auto selection
# should find interactions when the world really has them, refuse them when it
# does not, and refuse them even when they fit if their slopes would run wild
# over the projection.

make_micro <- function(n_per_year = 4000,
                       years = 2003:2018,
                       slope_common = -0.04,
                       slope_male_extra = 0,
                       slope_imd5_extra = 0,
                       seed = 1) {
  set.seed(seed)
  d <- CJ(year = years, i = 1:n_per_year)
  d[, `:=`(
    age = sample(25:34, .N, replace = TRUE),
    sex = sample(c("Male", "Female"), .N, replace = TRUE),
    imd_quintile = sample(c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
                          .N, replace = TRUE),
    wt_int = runif(.N, 0.5, 2)
  )]
  d[, lp := 0.2 + slope_common * (year - 2010) +
        fifelse(sex == "Male", 0.2 + slope_male_extra * (year - 2010), 0) +
        fifelse(imd_quintile == "5_most_deprived", 0.5 + slope_imd5_extra * (year - 2010), 0)]
  d[, ever := rbinom(.N, 1, plogis(lp))]
  d[, smk.state := fifelse(ever == 1, "former", "never")]
  d[, age_cat := "25-34"]
  d[, c("i", "lp", "ever") := NULL]
  d[]
}


test_that("an explicit model choice behaves exactly as the current function", {
  d <- make_micro()
  new_out <- suppressMessages(ever_smoke(d, time_horizon = 2040, model = "model8",
                                         min_age = 11, min_year = 2003))
  old_out <- suppressMessages(ever_smoke_old(d, time_horizon = 2040, model = "model8",
                                             min_age = 11, min_year = 2003))
  expect_equal(new_out$predicted_values$fitted_trends,
               old_out$predicted_values$fitted_trends, tolerance = 1e-12)
  expect_equal(new_out$data_points, old_out$data_points)
})

test_that("auto picks the plain model when the world has no interactions", {
  d <- make_micro(slope_male_extra = 0, slope_imd5_extra = 0)
  out <- suppressMessages(ever_smoke(d, time_horizon = 2040, model = "auto",
                                     min_age = 11, min_year = 2003))
  expect_identical(out$model_choice, "model8")
})

test_that("auto finds a real sex slope difference when it is large", {
  # a genuinely different male trend, big enough to show up on held-out years
  d <- make_micro(slope_male_extra = -0.05, n_per_year = 8000)
  out <- suppressMessages(ever_smoke(d, time_horizon = 2040, model = "auto",
                                     min_age = 11, min_year = 2003))
  # the models containing a sex:year_bin term
  expect_true(out$model_choice %in% c("model1", "model2", "model3", "model5"))
})

test_that("the slope guard rejects a model whose stratum trend runs wild", {
  # the most deprived stratum falls three times faster than everyone else:
  # the interaction model fits best, but its slope breaks the 2x cap
  d <- make_micro(slope_imd5_extra = -0.10, n_per_year = 8000)
  msgs <- capture_messages(
    out <- ever_smoke(d, time_horizon = 2040, model = "auto",
                      min_age = 11, min_year = 2003, auto_max_slope_mult = 2)
  )
  expect_true(any(grepl("rejected: a stratum's logit slope", msgs)))
  expect_true(out$model_choice %in% paste0("model", 1:8))
})

test_that("auto stops loudly when there are too few bins to hold any out", {
  d <- make_micro(years = 2015:2018)
  expect_error(
    suppressMessages(ever_smoke(d, time_horizon = 2040, model = "auto",
                                min_age = 11, min_year = 2015, num_bins = 3)),
    "year bins")
})

test_that("an invalid model name still errors", {
  d <- make_micro(years = 2010:2018, n_per_year = 500)
  expect_error(suppressMessages(ever_smoke(d, model = "model99")),
               "Invalid model")
})


# ---- the base-selects, bootstrap-follows contract ---------------------------

test_that("the bootstrap reads the base run's choice and never re-selects", {
  cfg <- list(country = "Testland", path = tempdir(),
              init_model_choice = "auto",
              init_auto_holdout_bins = 2, init_auto_tie_margin = 2,
              init_auto_floor = 0.02, init_auto_ceiling = 0.98,
              init_auto_max_slope_mult = 2)
  dir.create(file.path(cfg$path, "outputs"), showWarnings = FALSE)
  choice_file <- file.path(cfg$path, "outputs", "init_model_choice_Testland.rds")
  if (file.exists(choice_file)) file.remove(choice_file)

  # bootstrap before base: nothing to read, and it says why
  expect_error(init_resolve_model(cfg, boot_mode = TRUE), "Run the base estimation first")

  # base run: told to select, and told where to save
  base <- init_resolve_model(cfg, boot_mode = FALSE)
  expect_identical(base$model, "auto")
  expect_identical(base$choice_file, choice_file)

  # base saves (as estimate_initiation does), bootstrap then follows it
  saveRDS("model5", choice_file)
  boot <- init_resolve_model(cfg, boot_mode = TRUE)
  expect_identical(boot$model, "model5")
  expect_null(boot$choice_file)
})

test_that("auto without its config settings stops before fitting anything", {
  cfg <- list(country = "Testland", path = tempdir(), init_model_choice = "auto",
              init_auto_holdout_bins = 2)
  expect_error(init_resolve_model(cfg, boot_mode = FALSE), "init_auto_tie_margin")
})

test_that("an explicit model choice bypasses the resolver machinery", {
  cfg <- list(country = "Testland", path = tempdir(), init_model_choice = "model8")
  r <- init_resolve_model(cfg, boot_mode = TRUE)
  expect_identical(r$model, "model8")
  expect_null(r$choice_file)
})
