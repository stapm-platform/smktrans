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
# Input is raw_boot_smoking_trends_England.rds: one row per bootstrap iteration
# per age/year/sex/IMD cell, thinned in run_bootstrap_pipeline to ages 25-74 and
# years 2011-2019 with only the 'current' probability retained.
#
# NOTE ON 2019: HSE currently ends in 2018, so the 2019 prevalence surface is
# extrapolated one year past the data by the year polynomial in trend_fit. The
# bootstrap spread does not include that extrapolation error. Tables 9 and 10
# are therefore softer than their standard errors imply. See the comment block
# in trend_fit.R for what to change once HSE 2019 is in the workflow.

# 1. Configuration -------------------------------------------------------------
source("03_load_packages.R")

country     <- "England"
base_path   <- "transition_probability_estimates/src_england/outputs/"
out_path    <- "transition_probability_estimates/src_england/abm_calibration_targets/"
pop_file    <- "05_input/pop_sizes_england_national_2001-2019_v1_2022-03-30_mort.tools_1.4.0.csv"
output_date <- format(Sys.Date(), "%Y%m%d")
version     <- "v1"

target_ages  <- 25:74
target_years <- 2011:2019

file_prev_means <- paste0(out_path, "prevalence_targets_means_", output_date, "_", version, ".csv")
file_prev_covar <- paste0(out_path, "prevalence_targets_covariance_", output_date, "_", version, ".csv")

if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

# 2. Load the bootstrapped prevalence surface ----------------------------------
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
message(sprintf("Loaded %d bootstrap iterations over %s rows.", B, format(nrow(prev_dt), big.mark = ",")))

# The pipeline should already have thinned to exactly this. Confirm rather than
# re-filter, so that a change upstream shows up here instead of passing quietly.
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

# Every cell in every iteration
cell_counts <- prev_dt[, .N, by = .(age, year, sex, imd_quintile)]
if (any(cell_counts$N != B)) {
  stop("Cells have between ", min(cell_counts$N), " and ", max(cell_counts$N),
       " draws, expected ", B, " each.")
}

# 3. Categorisation ------------------------------------------------------------
# Tables 7 and 9 use three detailed age bands; Tables 8 and 10 collapse to 25-74.
prev_dt[, age_cat_detailed := fcase(
  age >= 25 & age <= 44, "25-44",
  age >= 45 & age <= 64, "45-64",
  age >= 65 & age <= 74, "65-74"
)]
if (anyNA(prev_dt$age_cat_detailed)) stop("An age fell outside 25-74 after banding.")

prev_dt[, age_cat_broad := "25-74"]

prev_dt[, year_cat := fcase(
  year >= 2011 & year <= 2013, "2011-2013",
  year >= 2014 & year <= 2016, "2014-2016",
  year >= 2017 & year <= 2019, "2017-2019"
)]
if (anyNA(prev_dt$year_cat)) stop("A year fell outside 2011-2019 after banding.")

# 4. Population weights --------------------------------------------------------
# The trend surface is a modelled prediction over a complete grid, not a set of
# survey observations. Collapsing it over age, sex or IMD should therefore be
# weighted by how many people are actually in each cell, i.e. ONS mid-year
# populations, not by the survey design weights. Design weights would reweight a
# smooth surface by the accidents of who happened to be sampled.
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

# The join must be one-to-one. An unmatched trend cell would silently get a zero
# weight and drop out of its target's weighted mean.
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

# 5. Aggregate each target, once per bootstrap iteration ------------------------
# Doing this inside boot_id is the whole point: it gives us B draws of every
# target, from which the variance and the between-target covariance both follow.

# Table 7 (calibration): sex by detailed age, 2011-2016
t7 <- prev_dt[year <= 2016,
              .(prevalence = weighted.mean(current, N)),
              by = .(boot_id, age_cat = age_cat_detailed, sex)]
t7[, `:=`(year_cat = "2011-2016", imd_quintile = "All", table = "Table 7", use = "calibration")]

# Table 8 (calibration): year category by IMD quintile, ages 25-74
t8 <- prev_dt[year <= 2016,
              .(prevalence = weighted.mean(current, N)),
              by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
t8[, `:=`(sex = "All", table = "Table 8", use = "calibration")]

# Table 9 (validation): sex by detailed age, 2017-2019
t9 <- prev_dt[year >= 2017,
              .(prevalence = weighted.mean(current, N)),
              by = .(boot_id, age_cat = age_cat_detailed, sex)]
t9[, `:=`(year_cat = "2017-2019", imd_quintile = "All", table = "Table 9", use = "validation")]

# Table 10 (validation): year category by IMD quintile, ages 25-74
t10 <- prev_dt[year >= 2017,
               .(prevalence = weighted.mean(current, N)),
               by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
t10[, `:=`(sex = "All", table = "Table 10", use = "validation")]

all_targets <- rbindlist(list(t7, t8, t9, t10), use.names = TRUE, fill = TRUE)

if (anyNA(all_targets$prevalence)) stop("A target came out as NA after aggregation.")

# Expected: 6 + 10 + 6 + 5 = 27 targets (Tables 7, 8, 9, 10)
n_targets <- uniqueN(all_targets, by = c("table", "year_cat", "age_cat", "sex", "imd_quintile"))
if (n_targets != 27) {
  stop("Built ", n_targets, " targets, expected 27. Check the table definitions.")
}
if (nrow(all_targets) != n_targets * B) {
  stop("Expected ", n_targets * B, " target-by-iteration rows, got ", nrow(all_targets), ".")
}

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
), by = .(target_id, table, use, year_cat, imd_quintile, sex, age_cat)]

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
if (min(eig) <= 0) {
  stop("Covariance matrix is not positive definite (smallest eigenvalue ",
       signif(min(eig), 3), "). Two targets may be collinear.")
}
message(sprintf("Covariance matrix is %d x %d, condition number %.1f.",
                ncol(cov_matrix), ncol(cov_matrix), max(eig) / min(eig)))

cov_dt <- as.data.table(cov_matrix, keep.rownames = "target_id")

# 7. Save ----------------------------------------------------------------------
write.csv(target_means, file_prev_means, row.names = FALSE)
write.csv(cov_dt, file_prev_covar, row.names = FALSE)

message("Written ", file_prev_means)
message("Written ", file_prev_covar)
