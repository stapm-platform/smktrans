# Generate ABM inputs and calibration targets.
#
#   Part A: full probability tables for initiation, quitting and relapse, in the
#           column names the ABM expects.
#   Part B: aggregated quit targets for calibration - means, variances, and the
#           full covariance matrix across targets, all from the raw bootstrap.
#
# The covariance matrix is only useful if it inverts, so this script now checks
# that its diagonal agrees with the variances it also reports, and that it is
# positive definite, before writing anything out.

# 1. Configuration -------------------------------------------------------------
source("03_load_packages.R")

base_path   <- "transition_probability_estimates/src_england/outputs/"
out_path    <- "transition_probability_estimates/src_england/abm_calibration_targets/"
output_date <- format(Sys.Date(), "%Y%m%d")
version     <- "v1"

if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

# Output filenames
file_quit_probs    <- paste0(out_path, "quit_probabilities_", output_date, "_", version, ".csv")
file_init_probs    <- paste0(out_path, "init_probabilities_", output_date, "_", version, ".csv")
file_relapse_probs <- paste0(out_path, "relapse_probabilities_", output_date, "_", version, ".csv")

# Two files for the calibration targets: the target definitions with their
# means and variances, and the covariance matrix across those targets.
file_calib_means   <- paste0(out_path, "quit_calibration_targets_means_", output_date, "_", version, ".csv")
file_calib_covar   <- paste0(out_path, "quit_calibration_targets_covariance_", output_date, "_", version, ".csv")

# How to handle bootstrap iterations that produced an NA for at least one target.
#   "complete"  drop those iterations entirely. Every entry of the covariance
#               matrix is then computed from the same set of draws, so the
#               matrix is positive semi-definite and the diagonal matches the
#               variances. Costs whole iterations.
#   "pairwise"  keep everything, compute each entry from whatever draws are
#               available for that pair. Different entries then rest on
#               different sample sizes, the diagonal stops matching the
#               variances, and the matrix can fail to invert.
# "complete" is the default because a matrix that silently will not invert is
# worse than one built from fewer draws.
na_handling <- "complete"

# The smallest number of usable iterations we are prepared to build a 27-ish
# dimensional covariance matrix from. Below this, stop rather than report a
# noisy matrix as if it were solid.
min_usable_iterations <- 200

# 2. Helper functions ----------------------------------------------------------
export_abm_table <- function(data, age_range, year_range, cols_to_keep, col_mapping, output_path) {
  dt <- data[age >= age_range[1] & age <= age_range[2] &
               year >= year_range[1] & year <= year_range[2], ..cols_to_keep]

  if (nrow(dt) == 0) stop("export_abm_table: the age/year filter matched no rows for ", output_path)
  if (anyNA(dt)) stop("export_abm_table: NAs in the table destined for ", output_path)

  setorderv(dt, cols_to_keep[1:length(col_mapping)], rep(1, length(col_mapping)))
  setnames(dt, names(col_mapping), as.character(col_mapping))
  write.csv(dt, output_path, row.names = FALSE)
}

# 3. Part A: generate ABM input files (probabilities) --------------------------
# The aggregated uncertainty data, used only for direct ABM lookups
raw_quit    <- readRDS(paste0(base_path, "quit_data_england_uncertainty.rds"))$data
raw_init    <- readRDS(paste0(base_path, "init_data_england_uncertainty.rds"))$data
raw_relapse <- readRDS(paste0(base_path, "relapse_data_england_uncertainty.rds"))$data

quit_map <- c("year"="arrivalYear", "age"="pAge", "sex"="pGender", "imd_quintile"="pIMDquintile")
export_abm_table(raw_quit, c(16, 89), c(2011, 2040), c("year", "age", "sex", "imd_quintile", "p_quit"), quit_map, file_quit_probs)
export_abm_table(raw_init, c(16, 30), c(2011, 2040), c("year", "age", "sex", "imd_quintile", "p_start"), quit_map, file_init_probs)

relapse_map <- c(quit_map, "time_since_quit" = "bYearsSinceQuit")
export_abm_table(raw_relapse, c(16, 89), c(2011, 2040), c("year", "age", "sex", "imd_quintile", "time_since_quit", "p_relapse"), relapse_map, file_relapse_probs)

# Free up memory
rm(raw_quit, raw_init, raw_relapse); gc()


# 4. Part B: generate calibration targets (empirical covariance) ---------------
message("Processing empirical bootstrap targets...")

# The raw bootstrap data, all iterations. The aggregated file cannot give us
# variance or covariance, only the bounds that were already computed from it.
boot_dt <- readRDS(paste0(base_path, "raw_boot_quit_data_England.rds"))
setDT(boot_dt)

if (!"boot_id" %in% names(boot_dt)) stop("raw_boot_quit_data_England.rds has no boot_id column.")

B_total <- uniqueN(boot_dt$boot_id)
message(sprintf("Loaded %d bootstrap iterations.", B_total))

boot_dt <- boot_dt[year >= 2011 & year <= 2019]
if (nrow(boot_dt) == 0) stop("No rows left after filtering to 2011-2019.")

# Note: p_quit rows that are NA are kept for now. Dropping them here would hide
# how much of each demographic group is actually missing, which is what the
# diagnostic in 4.2b is for.

# --- 4.1 Categorisation ---
age_breaks_detailed <- c(-1, 25, 45, 65, 75, 1000)
boot_dt[, age_cat_detailed := c("16-24", "25-44", "45-64", "65-74", "75-89")[findInterval(age, age_breaks_detailed)]]

age_breaks_broad <- c(-1, 25, 75, 1000)
boot_dt[, age_cat_broad := c("16-24", "25-74", "75-89")[findInterval(age, age_breaks_broad)]]

boot_dt[, year_cat := c("2011-2013", "2014-2016", "2017-2019")[findInterval(year, c(-1, 2014, 2017, 10000))]]

if (anyNA(boot_dt$age_cat_detailed)) stop("An age fell outside every detailed age band.")
if (anyNA(boot_dt$age_cat_broad))    stop("An age fell outside every broad age band.")
if (anyNA(boot_dt$year_cat))         stop("A year fell outside every year band.")

# --- 4.2 Apply static base weights ---
# Quit probabilities apply to current smokers, so the denominator is current
# smokers, and the weights are the survey weights of current smokers only.
survey_data <- readRDS("transition_probability_estimates/src_england/intermediate_data/HSE_2003_to_2018_tobacco_imputed.rds")
sd19 <- copy(survey_data[year == 2018])[ , year := 2019]
survey_data <- rbindlist(list(survey_data, sd19), use.names = TRUE)

data_w <- survey_data[smk.state == "current",
                      .(wgt = sum(wt_int, na.rm = TRUE)),
                      by = .(year, age, sex, imd_quintile)]

# Merge weights into the iterations
boot_dt <- merge(boot_dt, data_w, all.x = TRUE, by = c("year", "age", "sex", "imd_quintile"))
boot_dt[is.na(wgt), wgt := 0]

# --- 4.2b Pre-aggregation diagnostic: check raw data sparsity ---
message("\nChecking raw data for missingness (values that will be skipped by na.rm)...")

raw_na_count <- sum(is.na(boot_dt$p_quit))
raw_total    <- nrow(boot_dt)
raw_na_pct   <- (raw_na_count / raw_total) * 100

if (raw_na_count > 0) {
  message(sprintf("WARNING: %d missing 'p_quit' values detected in the raw data (%.2f%%).", raw_na_count, raw_na_pct))

  # How much data is missing for each demographic chunk, using the detailed age
  # categories as the indicator
  sparse_groups <- boot_dt[, .(
    total_underlying_rows = .N,
    missing_rows = sum(is.na(p_quit)),
    pct_missing  = (sum(is.na(p_quit)) / .N) * 100
  ), by = .(year_cat, age_cat_detailed, sex, imd_quintile)]

  bad_sparse <- sparse_groups[pct_missing > 0][order(-pct_missing)]

  message("\n  - Most sparse demographic groups (highest % of underlying NA values):")
  print(head(bad_sparse, 10))

  if (max(bad_sparse$pct_missing) > 50) {
    warning("Some demographic groups are calculating their weighted mean from less than half of their expected data points.")
  }
} else {
  message("Raw data is complete. na.rm will not have to skip anything.")
}

# A group with no weight at all produces NaN from weighted.mean, not NA, and NaN
# slips past is.na checks in some places but not others. Find those now.
zero_wgt <- boot_dt[, .(total_wgt = sum(wgt, na.rm = TRUE)), by = .(year_cat, age_cat_detailed, sex, imd_quintile)][total_wgt == 0]
if (nrow(zero_wgt) > 0) {
  message(sprintf("\n%d demographic groups have zero total survey weight; their targets will be NaN:", nrow(zero_wgt)))
  print(head(zero_wgt, 10))
}
message("--------------------------------------------------\n")

# --- 4.3 Target aggregation, computed per bootstrap iteration ---
# Aggregating inside boot_id is the whole point: it gives B draws of each
# target, from which both the variance and the between-target covariance follow.

# Table 3 (2011-2016): detailed age by sex
t3 <- boot_dt[year <= 2016, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, age_cat = age_cat_detailed, sex)]
t3[, `:=`(year_cat = "2011-2016", imd_quintile = "All")]

# Table 4 (2011-2016): year category by IMD by broad age
t4 <- boot_dt[year <= 2016, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
t4[, sex := "All"]

# Table 5 (2017-2019): detailed age by sex
t5 <- boot_dt[year >= 2017, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, age_cat = age_cat_detailed, sex)]
t5[, `:=`(year_cat = "2017-2019", imd_quintile = "All")]

# Table 6 (2017-2019): year category by IMD by broad age
t6 <- boot_dt[year >= 2017, .(p_quit = weighted.mean(p_quit, wgt, na.rm = TRUE)), by = .(boot_id, year_cat, age_cat = age_cat_broad, imd_quintile)]
t6[, sex := "All"]

all_targets <- rbindlist(list(t3, t4, t5, t6), use.names = TRUE, fill = TRUE)

# weighted.mean over an empty or zero-weight group returns NaN. Treat it as NA
# so that everything downstream sees one kind of missing, not two.
all_targets[is.nan(p_quit), p_quit := NA_real_]

# Assign a unique target ID to each demographic group. Sorting first keeps the
# IDs stable between runs.
setorder(all_targets, year_cat, imd_quintile, sex, age_cat, boot_id)
all_targets[, target_id := sprintf("T_%03d", .GRP), by = .(year_cat, age_cat, sex, imd_quintile)]

n_targets <- uniqueN(all_targets$target_id)
message(sprintf("Built %d targets across %d iterations.", n_targets, B_total))

# Every target should appear in every iteration, even if its value is NA.
if (nrow(all_targets) != n_targets * B_total) {
  stop(sprintf("Expected %d target-by-iteration rows, got %d. Some target/iteration pairs are missing entirely.",
               n_targets * B_total, nrow(all_targets)))
}

# --- 4.4 Handle missing draws before computing anything ---
# Pivot wider: rows are iterations, columns are target ids
boot_wide <- dcast(all_targets, boot_id ~ target_id, value.var = "p_quit")
setorder(boot_wide, boot_id)
boot_ids <- boot_wide$boot_id
boot_wide[, boot_id := NULL]

# A target that is NA in every iteration cannot contribute anything and will
# make the matrix singular. Say which one.
all_na_targets <- names(boot_wide)[colSums(!is.na(boot_wide)) == 0]
if (length(all_na_targets) > 0) {
  stop("These targets are NA in every iteration: ", paste(all_na_targets, collapse = ", "),
       ". Check the zero-weight groups listed above.")
}

complete_rows <- complete.cases(boot_wide)
n_dropped <- sum(!complete_rows)

if (na_handling == "complete") {

  if (n_dropped > 0) {
    message(sprintf("\nDropping %d of %d iterations (%.1f%%) that have an NA for at least one target.",
                    n_dropped, B_total, 100 * n_dropped / B_total))

    # Which targets are responsible, so the loss is attributable rather than mysterious
    culprit <- data.table(
      target_id = names(boot_wide),
      n_missing = colSums(is.na(boot_wide))
    )[n_missing > 0][order(-n_missing)]
    message("Targets responsible for the dropped iterations:")
    print(culprit)

    boot_wide <- boot_wide[complete_rows]
    boot_ids  <- boot_ids[complete_rows]
  }

  B_used <- nrow(boot_wide)

  if (B_used < min_usable_iterations) {
    stop(sprintf("Only %d complete iterations remain, below the minimum of %d. ",
                 B_used, min_usable_iterations),
         "Either widen the demographic groups so they are less sparse, or set ",
         "na_handling to 'pairwise' and accept a matrix that may not invert.")
  }

  cov_matrix <- cov(boot_wide)

  # Restrict the means and variances to the same iterations, so the two output
  # files describe the same draws. Reporting a mean over 1000 draws next to a
  # covariance over 850 would be quietly inconsistent.
  all_targets <- all_targets[boot_id %in% boot_ids]

} else if (na_handling == "pairwise") {

  message(sprintf("\nUsing pairwise complete observations. %d of %d iterations have at least one NA.",
                  n_dropped, B_total))
  message("Entries of the covariance matrix will rest on differing numbers of draws.")

  B_used <- B_total
  cov_matrix <- cov(boot_wide, use = "pairwise.complete.obs")

} else {
  stop("na_handling must be 'complete' or 'pairwise', not '", na_handling, "'.")
}

message(sprintf("Covariance computed from %d iterations.", B_used))

# --- 4.5 Means and variances ---
target_means <- all_targets[, .(
  n_boot      = sum(!is.na(p_quit)),
  mean_p_quit = mean(p_quit, na.rm = TRUE),
  var_p_quit  = var(p_quit, na.rm = TRUE),
  se_p_quit   = sd(p_quit, na.rm = TRUE),
  lower_95    = quantile(p_quit, 0.025, na.rm = TRUE),
  upper_95    = quantile(p_quit, 0.975, na.rm = TRUE)
), by = .(target_id, year_cat, imd_quintile, sex, age_cat)]

if (any(target_means$var_p_quit <= 0 | is.na(target_means$var_p_quit))) {
  stop("A target has zero or undefined variance across the bootstrap.")
}

# --- 4.6 Check the covariance matrix before trusting it ---

if (!isTRUE(all.equal(cov_matrix, t(cov_matrix)))) stop("Covariance matrix is not symmetric.")
if (anyNA(cov_matrix)) stop("Covariance matrix contains NAs. Some target pair shares no complete iterations.")

# The diagonal is the variance of each target. If it does not match the
# variances in the means file then the two outputs describe different draws.
# Under pairwise this is expected to fail, which is exactly the point.
diag_check <- merge(
  data.table(target_id = colnames(cov_matrix), diag_var = diag(cov_matrix)),
  target_means[, .(target_id, var_p_quit)],
  by = "target_id"
)
diag_gap <- max(abs(diag_check$diag_var - diag_check$var_p_quit))

if (diag_gap > 1e-12) {
  msg <- sprintf("Covariance diagonal disagrees with the reported variances (worst gap %.3g).", diag_gap)
  if (na_handling == "pairwise") {
    warning(msg, " This is what pairwise.complete.obs does: each entry uses a different subset of iterations.")
  } else {
    stop(msg)
  }
}

eig <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values

# Tolerance scaled to the size of the matrix. Eigenvalues within +/- this of
# zero are structural zeros (redundant target combinations, see below);
# anything below its negative is a genuine error.
eig_tol <- 1e-9 * max(eig)

n_structural_zero <- sum(abs(eig) < eig_tol)
n_negative        <- sum(eig < -eig_tol)

if (n_negative > 0) {
  stop(sprintf("Covariance matrix has %d genuinely negative eigenvalue(s) (smallest %.3g). ",
               n_negative, min(eig)),
       "That is not rounding - check for corrupted bootstrap draws",
       if (na_handling == "pairwise") ", or try na_handling = 'complete'." else ".")
}

# ==========================================================================
# RANK DEFICIENCY - DELIBERATE, DO NOT 'FIX' BY DROPPING TARGETS
# --------------------------------------------------------------------------
# The four target tables (3-6) are marginal summaries of the same underlying
# bootstrap array, collapsed along different axes (sex x age, versus year x
# IMD). Their shared sub-totals are equal in every bootstrap draw, so each
# shared total is a zero-variance direction and appears as a near-zero
# eigenvalue. The matrix is therefore rank-deficient BY CONSTRUCTION, not
# because of a data problem, and not because any two targets are collinear (a
# pairwise correlation check comes back empty).
#
# We keep the full matrix on purpose - the team has chosen to receive all
# targets in one file and handle the redundancy their end. Anyone INVERTING it
# (e.g. as a precision matrix in a Gaussian calibration likelihood) must use a
# pseudoinverse (MASS::ginv) or a rank-aware solver; a plain solve() will fail
# or return noise along the zero directions. See the note handed to the team.
# ==========================================================================
if (n_structural_zero > 0) {
  message(sprintf("NOTE: covariance matrix is rank %d of %d (%d structural zero eigenvalue(s)).",
                  ncol(cov_matrix) - n_structural_zero, ncol(cov_matrix), n_structural_zero))
  message("      Expected: the tables share marginal totals. Matrix kept whole on request.")
  message("      Downstream inversion MUST use a pseudoinverse. See accompanying note.")
} else {
  cond_num <- max(eig) / min(eig)
  message(sprintf("Covariance matrix is %d x %d, full rank, condition number %.1f.",
                  ncol(cov_matrix), ncol(cov_matrix), cond_num))
  if (cond_num > 1e6) {
    warning("Condition number above 1e6. Inverting this matrix will amplify noise substantially.")
  }
}

# Rule of thumb: a d x d covariance wants at least a few times d draws before
# its off-diagonal entries settle down.
if (B_used < 10 * ncol(cov_matrix)) {
  warning(sprintf("Estimating a %d x %d covariance from %d draws. The off-diagonal entries will be noisy.",
                  ncol(cov_matrix), ncol(cov_matrix), B_used))
}

# --- 4.7 Prepare for export ---
setnames(target_means,
         old = c("year_cat", "age_cat", "sex", "imd_quintile"),
         new = c("arrivalYearCategorical", "pAgeCategorical", "pGender", "pIMDquintile"))
setorder(target_means, target_id)

cov_dt <- as.data.table(cov_matrix, keep.rownames = "target_id")

# The two files must describe the same targets in the same order.
if (!identical(cov_dt$target_id, target_means$target_id)) {
  stop("Target IDs differ between the means file and the covariance file.")
}

# --- 4.8 Save outputs ---
write.csv(target_means, file_calib_means, row.names = FALSE)
write.csv(cov_dt, file_calib_covar, row.names = FALSE)

message("\nWritten ", file_calib_means)
message("Written ", file_calib_covar)
message(sprintf("%d targets, %d iterations used, na_handling = '%s'.", n_targets, B_used, na_handling))

##################################################################################

# Block-level diagnostic for the quit calibration covariance matrix.
#
# Purpose: confirm that the four target tables can each be inverted on their own,
# so that the calibration can compute a multivariate implausibility per table and
# take the maximum (Andrianakis et al. 2015, eq 10 + eq 9), without any
# manipulation of the supplied covariance file.
#
# Run this after produce_abm_calibration_targets.R, with cov_matrix and
# target_means still in the environment.

# --- 0. Handle the column naming ------------------------------------------
# target_means gets renamed at step 4.7. This works either side of that.
if ("arrivalYearCategorical" %in% names(target_means)) {
  yr_col <- "arrivalYearCategorical"; sex_col <- "pGender"; imd_col <- "pIMDquintile"
} else {
  yr_col <- "year_cat"; sex_col <- "sex"; imd_col <- "imd_quintile"
}

# --- 1. Define the four blocks --------------------------------------------
# From the target construction at 4.3:
#   Table 3  year <= 2016, detailed age x sex   -> year_cat "2011-2016", imd "All"
#   Table 4  year <= 2016, year_cat x imd x broad age -> sex "All"
#   Table 5  year >= 2017, detailed age x sex   -> year_cat "2017-2019", imd "All"
#   Table 6  year >= 2017, year_cat x imd x broad age -> sex "All"
#
# So the sex-by-age tables are exactly those with imd == "All", and the
# IMD tables are exactly those with sex == "All". Year category splits each pair.
tab_ids <- list(
  `Table 3 (cal, sex x age, 2011-2016)` =
    target_means[get(imd_col) == "All" & get(yr_col) == "2011-2016", target_id],
  `Table 4 (cal, imd x age, 2011-2016)` =
    target_means[get(sex_col) == "All" & get(yr_col) %in% c("2011-2013", "2014-2016"), target_id],
  `Table 5 (val, sex x age, 2017-2019)` =
    target_means[get(imd_col) == "All" & get(yr_col) == "2017-2019", target_id],
  `Table 6 (val, imd x age, 2017-2019)` =
    target_means[get(sex_col) == "All" & get(yr_col) == "2017-2019", target_id]
)

# --- 2. The blocks must partition the targets ------------------------------
# If they do not, the maximum-implausibility approach either double-counts a
# target or silently drops one. Check before trusting anything below.
assigned <- unlist(tab_ids, use.names = FALSE)
if (anyDuplicated(assigned)) {
  stop("A target is assigned to more than one block: ",
       paste(assigned[duplicated(assigned)], collapse = ", "))
}
unassigned <- setdiff(colnames(cov_matrix), assigned)
if (length(unassigned) > 0) {
  stop(length(unassigned), " target(s) fall in no block: ",
       paste(unassigned, collapse = ", "))
}
cat(sprintf("Blocks partition all %d targets cleanly (%s).\n\n",
            length(assigned), paste(lengths(tab_ids), collapse = " + ")))

# --- 3. Is each block invertible on its own? -------------------------------
# This is the question that matters. The full matrix has 6 zero eigenvalues.
# If each block is full rank, all 6 live in the cross-block entries, which the
# max-implausibility approach never touches - so no manipulation is needed.
cat("Per-block eigen-diagnostics:\n")
for (nm in names(tab_ids)) {
  ids <- tab_ids[[nm]]
  blk <- cov_matrix[ids, ids, drop = FALSE]
  ev  <- eigen(blk, symmetric = TRUE, only.values = TRUE)$values
  rel <- min(ev) / max(ev)          # scale-free: is the smallest a real zero?
  cat(sprintf("  %-38s n=%2d  min eig %10.3e  max/min %9.1f  min/max %.2e  %s\n",
              nm, length(ids), min(ev), max(ev) / min(ev), rel,
              if (rel > 1e-8) "OK" else "*** NEAR-SINGULAR ***"))
}

# --- 4. Within-block correlations ------------------------------------------
# Any pair inside a block correlating at ~1 would mean two targets in the same
# table share respondents (a total and its parts). That would be a real problem,
# because it sits inside the matrix being inverted and taking the maximum across
# blocks would not remove it.
cat("\nWithin-block correlations above 0.9:\n")
found_any <- FALSE
for (nm in names(tab_ids)) {
  ids <- tab_ids[[nm]]
  cm   <- cov2cor(cov_matrix[ids, ids, drop = FALSE])
  high <- which(abs(cm) > 0.9 & upper.tri(cm), arr.ind = TRUE)
  if (nrow(high) > 0) {
    found_any <- TRUE
    cat("  ", nm, ":\n", sep = "")
    print(data.table(a = rownames(cm)[high[, 1]],
                     b = colnames(cm)[high[, 2]],
                     r = round(cm[high], 4))[order(-abs(r))])
  }
}
if (!found_any) cat("  None. No two targets within a table are near-collinear.\n")

# --- 5. Where the zero eigenvalues actually live ---------------------------
# Confirm the deficiency is purely cross-block: the block-diagonal matrix
# (cross-block entries zeroed) should be full rank even though the full one is not.
blkdiag <- matrix(0, nrow(cov_matrix), ncol(cov_matrix),
                  dimnames = dimnames(cov_matrix))
for (ids in tab_ids) blkdiag[ids, ids] <- cov_matrix[ids, ids]

ev_full <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
ev_blk  <- eigen(blkdiag,    symmetric = TRUE, only.values = TRUE)$values

cat(sprintf("\nFull matrix    : rank %d of %d (%d eigenvalues within rounding of zero)\n",
            sum(abs(ev_full) > 1e-9 * max(ev_full)), ncol(cov_matrix),
            sum(abs(ev_full) < 1e-9 * max(ev_full))))
cat(sprintf("Block-diagonal : rank %d of %d (%d eigenvalues within rounding of zero)\n",
            sum(abs(ev_blk) > 1e-9 * max(ev_blk)), ncol(blkdiag),
            sum(abs(ev_blk) < 1e-9 * max(ev_blk))))
cat("\nIf the block-diagonal is full rank, the deficiency is entirely in the\n",
    "cross-block entries and per-table inversion is safe as supplied.\n", sep = "")


















