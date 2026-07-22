# Generate smoking prevalence calibration and validation targets for the ABM.
#
# Produces the numbers behind Tables 7-10:
#   Table 7  calibration, prevalence by sex and age category, 2011-2016
#   Table 8  calibration, prevalence by year category and IMD quintile, ages 25-74
#   Table 9  validation, prevalence by sex and age category, 2017-2019
#   Table 10 validation, prevalence by year category and IMD quintile, ages 25-74
#
# Means, variances and standard errors go in one file. The full covariance
# matrix across every target goes in a second, so cross-target correlations are
# available to whoever is writing the calibration likelihood.
#
# The targets can be built from either of two sources, chosen below. Both come
# from the same bootstrap run under the same seed, so either flavour is
# coherent with the quit targets built from the same draws.
#
#   "survey"  Pooled design-weighted prevalence computed directly from each
#             bootstrap resample of the survey: sum of weight carried by
#             current smokers over sum of weight, across every respondent in
#             the target's ages and years. The intervals carry the survey's
#             sampling variation at the width the data supports. This is the
#             default.
#
#   "model"   The trend_fit surface fitted to each resample, collapsed over
#             the target's cells with ONS population weights. The surface
#             pools strength across all ages and years, so these intervals
#             are much narrower than the survey's; they measure uncertainty
#             in the fitted surface, conditional on the model being right.
#
# Input files, both written by process_country():
#   raw_boot_smoking_trends_<country>.rds   fitted surface per draw ("model")
#   raw_boot_survey_prev_<country>.rds      survey weighted sums per draw ("survey")
#
# NOTE ON 2019: HSE currently ends in 2018. Under "model" the 2019 surface is
# extrapolated one year past the data by the year polynomial in trend_fit, so
# Tables 9 and 10 are softer than their standard errors imply. Under "survey"
# there is no 2019 to use: Tables 9 and 10 keep their 2017-2019 labels but are
# built from 2017-2018 only, which is declared in known_missing_years below and
# recorded per target in the years_used column of the output. When HSE 2019
# comes into the workflow: delete the known_missing_years line, re-run the
# estimation so the bootstrap aggregates pick up the new year, and re-run this
# script. If the declaration is left in place after the data arrives, the
# script stops and says so.

# 1. Configuration -------------------------------------------------------------
source("03_load_packages.R")

country     <- "England"
base_path   <- "transition_probability_estimates/src_england/outputs/"
out_path    <- "transition_probability_estimates/src_england/abm_calibration_targets/"
pop_file    <- "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv"
output_date <- format(Sys.Date(), "%Y%m%d")
version     <- "v1"

# "survey" or "model". See the header for what each means.
target_source <- "model"

# Years inside the target range that the survey is known not to cover yet.
# Delete this line (set to integer(0)) when HSE 2019 is added to the workflow.
known_missing_years <- 2019L

target_ages  <- 25:74
target_years <- 2011:2019

if (!target_source %in% c("survey", "model")) {
  stop("target_source must be 'survey' or 'model', got '", target_source, "'.")
}

file_prev_means <- paste0(out_path, "prevalence_targets_means_", output_date, "_", version, ".csv")
file_prev_covar <- paste0(out_path, "prevalence_targets_covariance_", output_date, "_", version, ".csv")

if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

# Helpers shared by both sources -----------------------------------------------

# "2011-2013" -> 2011:2013. The year_cat labels are the interface the ABM sees,
# so they are parsed rather than duplicated as a second set of constants that
# could drift from the labels.
label_to_years <- function(label) {
  parts <- as.integer(strsplit(label, "-", fixed = TRUE)[[1]])
  if (length(parts) != 2 || anyNA(parts) || parts[1] > parts[2]) {
    stop("Cannot read '", label, "' as a year range.")
  }
  parts[1]:parts[2]
}

years_to_label <- function(years) {
  years <- sort(unique(years))
  if (!identical(years, min(years):max(years))) {
    stop("Years used for a target are not contiguous: ", paste(years, collapse = ", "))
  }
  if (length(years) == 1) as.character(years) else paste0(min(years), "-", max(years))
}

band_ages <- function(dt) {
  dt[, age_cat_detailed := fcase(
    age >= 25 & age <= 44, "25-44",
    age >= 45 & age <= 64, "45-64",
    age >= 65 & age <= 74, "65-74"
  )]
  if (anyNA(dt$age_cat_detailed)) stop("An age fell outside 25-74 after banding.")
  dt[, age_cat_broad := "25-74"]
  dt[, year_cat := fcase(
    year >= 2011 & year <= 2013, "2011-2013",
    year >= 2014 & year <= 2016, "2014-2016",
    year >= 2017 & year <= 2019, "2017-2019"
  )]
  if (anyNA(dt$year_cat)) stop("A year fell outside 2011-2019 after banding.")
  invisible(dt)
}

# 2. Load and check the chosen source -------------------------------------------

if (target_source == "model") {

  boot_file <- paste0(base_path, "raw_boot_smoking_trends_", country, ".rds")
  if (!file.exists(boot_file)) {
    stop("No raw bootstrap trends at ", boot_file,
         ". Run process_country() with the patched pipeline first.")
  }

  prev_dt <- readRDS(boot_file)
  setDT(prev_dt)

  required_cols <- c("boot_id", "age", "year", "sex", "imd_quintile", "current")
  missing_cols <- setdiff(required_cols, names(prev_dt))
  if (length(missing_cols) > 0) {
    stop("Bootstrap trends are missing: ", paste(missing_cols, collapse = ", "))
  }

  B <- uniqueN(prev_dt$boot_id)
  message(sprintf("Loaded %d bootstrap iterations over %s rows (source: model fit).",
                  B, format(nrow(prev_dt), big.mark = ",")))

  # The pipeline should already have thinned to exactly this. Confirm rather
  # than re-filter, so that a change upstream shows up here instead of passing
  # quietly.
  if (!setequal(unique(prev_dt$age), target_ages)) {
    stop("Bootstrap trends span ages ", min(prev_dt$age), "-", max(prev_dt$age),
         ", expected ", min(target_ages), "-", max(target_ages),
         ". Check config$trend_keep_ages.")
  }
  if (!setequal(unique(prev_dt$year), target_years)) {
    stop("Bootstrap trends span years ", min(prev_dt$year), "-", max(prev_dt$year),
         ", expected ", min(target_years), "-", max(target_years),
         ". Check config$trend_keep_years.")
  }
  if (anyNA(prev_dt$current)) {
    stop(sum(is.na(prev_dt$current)), " missing prevalence values in the bootstrap draws. ",
         "trend_fit should have made this impossible, so something has gone wrong upstream.")
  }
  if (prev_dt[, any(current < 0 | current > 1)]) stop("Prevalence values outside [0, 1].")

  cell_counts <- prev_dt[, .N, by = .(age, year, sex, imd_quintile)]
  if (any(cell_counts$N != B)) {
    stop("Cells have between ", min(cell_counts$N), " and ", max(cell_counts$N),
         " draws, expected ", B, " each.")
  }

} else {

  boot_file <- paste0(base_path, "raw_boot_survey_prev_", country, ".rds")
  if (!file.exists(boot_file)) {
    stop("No raw bootstrap survey aggregates at ", boot_file,
         ". Run process_country() with the patched pipeline first.")
  }

  sp_dt <- readRDS(boot_file)
  setDT(sp_dt)

  required_cols <- c("boot_id", "age", "year", "sex", "imd_quintile",
                     "sum_wt", "sum_wt_current", "n_obs")
  missing_cols <- setdiff(required_cols, names(sp_dt))
  if (length(missing_cols) > 0) {
    stop("Bootstrap survey aggregates are missing: ", paste(missing_cols, collapse = ", "))
  }

  B <- uniqueN(sp_dt$boot_id)
  message(sprintf("Loaded %d bootstrap iterations over %s rows (source: survey data).",
                  B, format(nrow(sp_dt), big.mark = ",")))

  # If a year declared missing is in fact present, the declaration is stale:
  # the data has moved on and the line at the top needs deleting so the
  # validation tables use it.
  stale <- intersect(known_missing_years, unique(sp_dt$year))
  if (length(stale) > 0) {
    stop("Year(s) ", paste(stale, collapse = ", "), " are declared in ",
         "known_missing_years but are present in the survey aggregates. ",
         "The survey has been extended: delete the known_missing_years ",
         "declaration and re-run, so the validation tables use the new data.")
  }

  expected_years <- setdiff(target_years, known_missing_years)
  if (!setequal(unique(sp_dt$year), expected_years)) {
    stop("Survey aggregates span years ", paste(sort(unique(sp_dt$year)), collapse = ", "),
         ", expected ", paste(expected_years, collapse = ", "),
         " (target years minus known_missing_years).")
  }
  if (!setequal(unique(sp_dt$age), target_ages)) {
    stop("Survey aggregates span ages ", min(sp_dt$age), "-", max(sp_dt$age),
         ", expected ", min(target_ages), "-", max(target_ages), ".")
  }
  if (anyNA(sp_dt)) stop("Missing values in the survey aggregates.")
  if (sp_dt[, any(sum_wt < 0 | sum_wt_current < 0 | sum_wt_current > sum_wt)]) {
    stop("Survey aggregate weights out of order: need 0 <= sum_wt_current <= sum_wt.")
  }
  # No per-cell draw-count check here, deliberately: a resample can leave a
  # single age/year/sex/IMD cell empty, and an empty cell is absent rather
  # than zero-filled. The pooled sums below remain exact either way; what has
  # to hold is that every target's total weight is positive in every draw,
  # which is checked after aggregation.
}

# 3. Population weights (model source only) -------------------------------------
# The trend surface is a modelled prediction over a complete grid, not a set of
# survey observations. Collapsing it over age, sex or IMD is therefore weighted
# by how many people are actually in each cell, i.e. ONS mid-year populations.
# The survey source needs none of this: its design weights already carry the
# population structure, and the pooled sums are the estimator.

if (target_source == "model") {

  pops <- fread(pop_file)
  if (!"N" %in% names(pops) && "pops" %in% names(pops)) setnames(pops, "pops", "N")

  pop_required <- c("year", "age", "sex", "imd_quintile", "N")
  missing_pop <- setdiff(pop_required, names(pops))
  if (length(missing_pop) > 0) {
    stop("Population file is missing: ", paste(missing_pop, collapse = ", "),
         ". Columns present: ", paste(names(pops), collapse = ", "))
  }

  pops <- pops[year %in% target_years & age %in% target_ages, .(year, age, sex, imd_quintile, N)]
  pops[, sex := as.character(sex)]
  pops[, imd_quintile := as.character(imd_quintile)]

  if (anyNA(pops$N)) stop("Missing population counts in the target years.")
  if (pops[, any(N <= 0)]) stop("Non-positive population counts in the target years.")

  pop_cells   <- unique(pops[, .(year, age, sex, imd_quintile)])
  trend_cells <- unique(prev_dt[, .(year, age, sex, imd_quintile)])
  if (nrow(pop_cells) != nrow(pops)) stop("Population file has duplicate year/age/sex/IMD rows.")

  unmatched <- fsetdiff(trend_cells, pop_cells)
  if (nrow(unmatched) > 0) {
    stop(nrow(unmatched), " trend cells have no population count. First few:\n",
         paste(capture.output(print(head(unmatched, 5))), collapse = "\n"))
  }

  prev_dt <- merge(prev_dt, pops, by = c("year", "age", "sex", "imd_quintile"), all.x = TRUE)
  if (anyNA(prev_dt$N)) stop("Population merge introduced NAs despite the cell check above.")
}

# 4. Aggregate each target, once per bootstrap iteration ------------------------
# Doing this inside boot_id is the whole point: it gives us B draws of every
# target, from which the variance and the between-target covariance both follow.
#
# The two sources aggregate differently but land in the same shape. The model
# source takes a population-weighted mean of fitted cell probabilities. The
# survey source sums weighted smokers and weighted respondents over the
# target's cells and divides once - the pooled design-weighted estimator, so a
# target's value is exactly what would be computed from the pooled respondent
# records of that draw.

if (target_source == "model") {

  band_ages(prev_dt)

  t7 <- prev_dt[year <= 2016,
                .(prevalence = weighted.mean(current, N)),
                by = .(boot_id, age_cat = age_cat_detailed, sex)]
  t7[, `:=`(year_cat = "2011-2016", imd_quintile = "All", table = "Table 7", use = "calibration")]

  t8 <- prev_dt[year <= 2016,
                .(prevalence = weighted.mean(current, N)),
                by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
  t8[, `:=`(sex = "All", table = "Table 8", use = "calibration")]

  t9 <- prev_dt[year >= 2017,
                .(prevalence = weighted.mean(current, N)),
                by = .(boot_id, age_cat = age_cat_detailed, sex)]
  t9[, `:=`(year_cat = "2017-2019", imd_quintile = "All", table = "Table 9", use = "validation")]

  t10 <- prev_dt[year >= 2017,
                 .(prevalence = weighted.mean(current, N)),
                 by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
  t10[, `:=`(sex = "All", table = "Table 10", use = "validation")]

  source_years <- sort(unique(prev_dt$year))
  source_label <- "trend model fit"

} else {

  band_ages(sp_dt)

  # Pool weighted smokers over weighted respondents, over whichever cells the
  # target spans. group_cols is passed in explicitly rather than read from the
  # enclosing scope, and the age band to use is named at each call, so each of
  # the four aggregations stands on its own. total_wt rides along so the
  # positive-weight check below has something to test.
  pooled <- function(d, age_band, group_cols) {
    d <- d[, .(boot_id, year_cat, sex, imd_quintile,
               age_cat = get(age_band), sum_wt, sum_wt_current)]
    d[, .(prevalence = sum(sum_wt_current) / sum(sum_wt),
          total_wt   = sum(sum_wt)), by = group_cols]
  }

  t7 <- pooled(sp_dt[year <= 2016], "age_cat_detailed", c("boot_id", "age_cat", "sex"))
  t7[, `:=`(year_cat = "2011-2016", imd_quintile = "All", table = "Table 7", use = "calibration")]

  t8 <- pooled(sp_dt[year <= 2016], "age_cat_broad", c("boot_id", "year_cat", "age_cat", "imd_quintile"))
  t8[, `:=`(sex = "All", table = "Table 8", use = "calibration")]

  t9 <- pooled(sp_dt[year >= 2017], "age_cat_detailed", c("boot_id", "age_cat", "sex"))
  t9[, `:=`(year_cat = "2017-2019", imd_quintile = "All", table = "Table 9", use = "validation")]

  t10 <- pooled(sp_dt[year >= 2017], "age_cat_broad", c("boot_id", "year_cat", "age_cat", "imd_quintile"))
  t10[, `:=`(sex = "All", table = "Table 10", use = "validation")]

  source_years <- sort(unique(sp_dt$year))
  source_label <- "survey data (pooled, design-weighted)"
}

all_targets <- rbindlist(list(t7, t8, t9, t10), use.names = TRUE, fill = TRUE)

if (target_source == "survey") {
  # Every target needs positive weight in every draw, or its value that draw is
  # not an estimate of anything.
  bad <- all_targets[total_wt <= 0]
  if (nrow(bad) > 0) {
    stop(nrow(bad), " target-by-iteration cells have no survey weight. First few:\n",
         paste(capture.output(print(head(bad[, .(table, year_cat, age_cat, sex, imd_quintile, boot_id)], 5))),
               collapse = "\n"))
  }
  all_targets[, total_wt := NULL]
}

if (anyNA(all_targets$prevalence)) stop("A target came out as NA after aggregation.")
if (all_targets[, any(prevalence < 0 | prevalence > 1)]) stop("A target fell outside [0, 1].")

# Expected: 6 + 10 + 6 + 5 = 27 targets (Tables 7, 8, 9, 10)
n_targets <- uniqueN(all_targets, by = c("table", "year_cat", "age_cat", "sex", "imd_quintile"))
if (n_targets != 27) {
  stop("Built ", n_targets, " targets, expected 27. Check the table definitions.")
}
if (nrow(all_targets) != n_targets * B) {
  stop("Expected ", n_targets * B, " target-by-iteration rows, got ", nrow(all_targets), ".")
}

# 5. Years used, against years declared ------------------------------------------
# Each target records the data years behind it. The only permitted difference
# between a target's label and its data is the declared known_missing_years:
# anything else stops. Under the model source the surface covers every labelled
# year, so years_used equals the label throughout (with 2019 extrapolated; see
# the header).
year_check <- unique(all_targets[, .(table, year_cat, age_cat, sex, imd_quintile)])
year_check[, years_used := {
  lab <- label_to_years(year_cat)
  years_to_label(intersect(lab, source_years))
}, by = seq_len(nrow(year_check))]

for (k in seq_len(nrow(year_check))) {
  lab_years  <- label_to_years(year_check$year_cat[k])
  used_years <- label_to_years(year_check$years_used[k])
  gap <- setdiff(lab_years, used_years)
  allowed <- intersect(lab_years, known_missing_years)
  if (target_source == "model") allowed <- integer(0)
  if (!setequal(gap, allowed)) {
    stop("Target '", year_check$year_cat[k], "' uses years ",
         year_check$years_used[k], " but the only permitted gap is {",
         paste(allowed, collapse = ", "), "}. The data and the ",
         "known_missing_years declaration disagree.")
  }
}

partial <- year_check[years_used != year_cat]
if (nrow(partial) > 0) {
  message("Note: ", nrow(partial), " targets keep their labels but currently ",
          "rest on ", paste(unique(partial$years_used), collapse = ", "),
          " (see known_missing_years and the years_used column).")
}

all_targets <- merge(all_targets, year_check,
                     by = c("table", "year_cat", "age_cat", "sex", "imd_quintile"))
if (nrow(all_targets) != n_targets * B) stop("The years_used merge changed the row count.")

# One ID per target, stable because we sort first
setorder(all_targets, table, year_cat, imd_quintile, sex, age_cat, boot_id)
all_targets[, target_id := sprintf("P_%03d", .GRP), by = .(table, year_cat, age_cat, sex, imd_quintile)]

# 6. Means, variances, and the covariance matrix --------------------------------

# File 1: target definitions with mean, variance, SE and percentile interval
target_means <- all_targets[, .(
  n_boot    = .N,
  mean_prev = mean(prevalence),
  var_prev  = var(prevalence),
  se_prev   = sd(prevalence),
  lower_95  = quantile(prevalence, 0.025),
  upper_95  = quantile(prevalence, 0.975)
), by = .(target_id, table, use, year_cat, imd_quintile, sex, age_cat, years_used)]

target_means[, source := source_label]

if (any(target_means$n_boot != B)) stop("A target has fewer than ", B, " draws.")
if (any(target_means$var_prev <= 0)) stop("A target has zero variance across the bootstrap.")

setnames(target_means,
         old = c("year_cat", "age_cat", "sex", "imd_quintile"),
         new = c("arrivalYearCategorical", "pAgeCategorical", "pGender", "pIMDquintile"))
setorder(target_means, target_id)

# File 2: the full covariance matrix across every target
# Rows are bootstrap iterations, columns are target ids
boot_wide <- dcast(all_targets, boot_id ~ target_id, value.var = "prevalence")
if (nrow(boot_wide) != B) stop("Wide table has ", nrow(boot_wide), " rows, expected ", B, ".")
boot_wide[, boot_id := NULL]
if (anyNA(boot_wide)) stop("NAs after pivoting to wide; some target/iteration pairs are missing.")

cov_matrix <- cov(boot_wide)   # no `use=` argument: there are no NAs, and we have checked

# The diagonal is the variance. If these disagree the two files disagree.
diag_check <- merge(
  data.table(target_id = colnames(cov_matrix), diag_var = diag(cov_matrix)),
  target_means[, .(target_id, var_prev)], by = "target_id"
)
if (any(abs(diag_check$diag_var - diag_check$var_prev) > 1e-12)) {
  stop("The covariance diagonal does not match the variances.")
}

# If this is going to be inverted as a precision matrix, it needs to be
# positive definite. With B = 1000 draws and 27 targets it should be, but a
# pair of perfectly collinear targets would break it.
eig <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
if (min(eig) <= -1e-15) {
  stop("Covariance matrix is not positive definite (smallest eigenvalue ",
       signif(min(eig), 3), "). Two targets may be collinear.")
}
message(sprintf("Covariance matrix is %d x %d, condition number %.1f.",
                ncol(cov_matrix), ncol(cov_matrix), max(eig) / min(eig)))

cov_dt <- as.data.table(cov_matrix, keep.rownames = "target_id")

# 7. Save ----------------------------------------------------------------------
write.csv(target_means, file_prev_means, row.names = FALSE)
write.csv(cov_dt, file_prev_covar, row.names = FALSE)

message("Written ", file_prev_means, " (source: ", source_label, ")")
message("Written ", file_prev_covar)
