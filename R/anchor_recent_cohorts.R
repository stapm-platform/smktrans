#' Anchor extrapolated ever-smoking targets on a youth survey series
#'
#' @description
#' Replaces the ever_smoke() targets for cohorts beyond the trend model's own
#' data with values anchored on an external youth smoking series, linked onto
#' the target scale by a factor estimated on the cohorts both sources observe.
#' Cohorts within the trend model's data are returned unchanged.
#'
#' @details
#' A youth survey in year t reporting the proportion who have ever smoked is
#' read as an observation on the cohort born in t - anchor_age_centre. The
#' series is linearly interpolated onto integer cohorts, so gap years in a
#' biennial survey are covered. The link factor is the mean of
#' target / youth value over the overlap cohorts (those at or before the last
#' cohort the trend model observed at ref_age), estimated separately by sex
#' when the series has a sex column, and absorbs both later initiation between
#' the survey age and ref_age and differential recall between the two
#' instruments. Anchored targets are applied as a cohort-level ratio to the
#' fitted trend, so gradients across strata within a cohort pass through. The
#' ratio is 1 up to the last trend-supported cohort, blends linearly over
#' taper_cohorts, follows the linked youth series, and is held at its final
#' value for cohorts born after the last youth survey.
#'
#' Assumed: the link factor is stable across cohorts, and (for a series
#' covering an age band rather than a single age) that anchor_age_centre is a
#' fixed representative age for the band. In the bootstrap the youth series is
#' fixed external data; the link factor is re-estimated each iteration because
#' the trend model is refit.
#'
#' The link factor(s) and ratio path are attached to the returned list as
#' $anchor.
#'
#' @param ever_smoke_data List returned by ever_smoke().
#' @param youth_anchor_data Data table with columns survey_year and
#'   p_ever_smoked (a fraction), and optionally sex. If sex is present its
#'   values must exactly match the sexes in the trend model output; if absent,
#'   a single link and ratio path is estimated and applied to all sexes.
#' @param ref_age Integer - the calibration age used by init_adj.
#' @param anchor_age_centre Integer - the age the youth series represents: the
#'   single age surveyed, or a representative age for a band (13 for an 11-15
#'   band).
#' @param taper_cohorts Integer - number of cohorts over which the handover
#'   from trend to anchor is blended. 0 disables the blend.
#' @param quiet Logical - suppress messages.
#' @importFrom data.table := setDT copy data.table uniqueN
#' @export
anchor_recent_cohorts <- function(
    ever_smoke_data,
    youth_anchor_data,
    ref_age,
    anchor_age_centre,
    taper_cohorts,
    quiet = FALSE
) {

  youth <- copy(setDT(youth_anchor_data))
  trend <- copy(ever_smoke_data$predicted_values)
  data_points <- ever_smoke_data$data_points

  # --- input checks -------------------------------------------------------

  if(!all(c("survey_year", "p_ever_smoked") %in% names(youth))) {
    stop("anchor_recent_cohorts: youth_anchor_data needs columns survey_year ",
         "and p_ever_smoked.")
  }
  if(youth[!is.finite(p_ever_smoked) | p_ever_smoked <= 0 | p_ever_smoked >= 1, .N] > 0) {
    stop("anchor_recent_cohorts: p_ever_smoked must be a fraction strictly ",
         "between 0 and 1. Check for percentages entered as whole numbers.")
  }
  if(youth[survey_year != round(survey_year), .N] > 0) {
    stop("anchor_recent_cohorts: survey_year must be integer years.")
  }

  sex_mode <- "sex" %in% names(youth)

  if(sex_mode) {
    trend_sexes <- sort(unique(as.character(trend$sex)))
    youth_sexes <- sort(unique(as.character(youth$sex)))
    if(!identical(trend_sexes, youth_sexes)) {
      stop("anchor_recent_cohorts: sex values in youth_anchor_data (",
           paste(youth_sexes, collapse = ", "), ") do not match the trend ",
           "model output (", paste(trend_sexes, collapse = ", "), ").")
    }
    youth[, anchor_grp := as.character(sex)]
    trend[, anchor_grp := as.character(sex)]
  } else {
    youth[, anchor_grp := "all"]
    trend[, anchor_grp := "all"]
  }

  if(youth[, .N, by = .(anchor_grp, survey_year)][N > 1, .N] > 0) {
    stop("anchor_recent_cohorts: duplicate survey_year rows",
         if(sex_mode) " within sex" else "", " in youth_anchor_data.")
  }

  # --- build the anchored ratio path per group ----------------------------

  youth[, cohort := survey_year - anchor_age_centre]
  trend[, cohort := year - ref_age]

  path_list <- list()
  link_list <- list()

  for(g in unique(youth$anchor_grp)) {

    yg <- youth[anchor_grp == g][order(cohort)]
    tg <- trend[anchor_grp == g]

    # Last cohort the trend model observed at ref_age, for this group.
    if(sex_mode) {
      last_supported <- max(data_points[sex == g, year]) - ref_age
    } else {
      last_supported <- max(data_points$year) - ref_age
    }

    if(max(yg$cohort) <= last_supported) {
      stop("anchor_recent_cohorts (", g, "): the youth series ends at cohort ",
           max(yg$cohort), ", which the trend model already covers (last ",
           "supported cohort ", last_supported, "). Nothing to anchor.")
    }
    if(max(yg$cohort) < last_supported + taper_cohorts) {
      stop("anchor_recent_cohorts (", g, "): taper_cohorts = ", taper_cohorts,
           " needs the youth series to reach cohort ",
           last_supported + taper_cohorts, " but it ends at ", max(yg$cohort),
           ". Reduce taper_cohorts or extend the series.")
    }

    cohort_grid <- min(yg$cohort):max(yg$cohort)
    yi <- data.table(
      anchor_grp = g,
      cohort = cohort_grid,
      p_youth = stats::approx(yg$cohort, yg$p_ever_smoked, xout = cohort_grid)$y
    )

    tm <- tg[, .(fitted_mean = mean(fitted_trends)), by = cohort]

    overlap <- yi[cohort <= last_supported, cohort]
    if(length(overlap) < 3) {
      stop("anchor_recent_cohorts (", g, "): only ", length(overlap),
           " cohorts appear in both the youth series and the trend model's ",
           "data (cohorts up to ", last_supported, "). At least 3 are needed ",
           "to estimate the link factor.")
    }

    link <- merge(yi[cohort %in% overlap], tm, by = "cohort")
    if(link[, .N] != length(overlap)) {
      stop("anchor_recent_cohorts (", g, "): trend targets missing for ",
           "overlap cohorts.")
    }
    L <- link[, mean(fitted_mean / p_youth)]

    # Ratio path: 1 through supported cohorts, linked youth series relative to
    # the extrapolated trend beyond them, linear blend across the taper, final
    # value held for cohorts after the last youth survey.
    pg <- merge(
      data.table(anchor_grp = g, cohort = min(tm$cohort):max(tm$cohort)),
      yi, by = c("anchor_grp", "cohort"), all.x = TRUE)
    pg <- merge(pg, tm, by = "cohort", all.x = TRUE)
    pg[, ratio := (p_youth * L) / fitted_mean]
    pg[cohort <= last_supported, ratio := 1]

    if(taper_cohorts > 0) {
      taper <- data.table(
        cohort = (last_supported + 1):(last_supported + taper_cohorts),
        w = seq_len(taper_cohorts) / (taper_cohorts + 1))
      pg <- merge(pg, taper, by = "cohort", all.x = TRUE)
      pg[!is.na(w), ratio := (1 - w) + w * ratio]
      pg[, w := NULL]
    }

    last_anchored <- max(yg$cohort)
    tail_ratio <- pg[cohort == last_anchored, ratio]
    pg[cohort > last_anchored, ratio := tail_ratio]

    if(pg[is.na(ratio), .N] > 0) {
      stop("anchor_recent_cohorts (", g, "): undefined ratio for cohorts ",
           paste(pg[is.na(ratio), cohort], collapse = ", "),
           ". The youth series does not connect to the trend-supported ",
           "cohorts.")
    }
    if(pg[ratio <= 0, .N] > 0) {
      stop("anchor_recent_cohorts (", g, "): non-positive anchored ratio.")
    }

    path_list[[g]] <- pg[, .(anchor_grp, cohort, ratio)]
    link_list[[g]] <- data.table(
      anchor_grp = g, link_factor = L,
      overlap_first = min(overlap), overlap_last = max(overlap),
      last_supported_cohort = last_supported,
      last_anchored_cohort = last_anchored, tail_ratio = tail_ratio)
  }

  path <- data.table::rbindlist(path_list)
  links <- data.table::rbindlist(link_list)

  # --- apply --------------------------------------------------------------

  n_before <- trend[, .N]
  trend <- merge(trend, path, by = c("anchor_grp", "cohort"), all.x = FALSE)
  if(trend[, .N] != n_before | trend[is.na(ratio), .N] > 0) {
    stop("anchor_recent_cohorts: ratio path does not cover every cohort in ",
         "the trend output.")
  }
  trend[, fitted_trends := fitted_trends * ratio]
  trend[, c("cohort", "ratio", "anchor_grp") := NULL]

  ever_smoke_data$predicted_values <- trend
  ever_smoke_data$anchor <- list(
    by_sex = sex_mode,
    anchor_age_centre = anchor_age_centre,
    taper_cohorts = taper_cohorts,
    links = links,
    ratio_path = path
  )

  if(!quiet) {
    for(g in links$anchor_grp) {
      lk <- links[anchor_grp == g]
      message("   anchor_recent_cohorts", if(sex_mode) paste0(" (", g, ")") else "",
              ": cohorts ", lk$last_supported_cohort + 1, "+ anchored (link ",
              round(lk$link_factor, 3), ", overlap ", lk$overlap_first, "-",
              lk$overlap_last, "; ratio ", round(lk$tail_ratio, 3),
              " at cohort ", lk$last_anchored_cohort, ", held after)")
    }
  }

  return(ever_smoke_data)
}
