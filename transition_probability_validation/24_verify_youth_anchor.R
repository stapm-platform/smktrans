# Verify the youth-survey anchoring of the ever-smoking targets
# (anchor_recent_cohorts) against the saved outputs of a completed run.
#
# Two kinds of check. Hard checks are exact identities that the anchored
# outputs must satisfy by construction - the link factor recomputed from
# scratch, the anchored targets equalling the linked youth series outside the
# taper, the taper arithmetic, and the structure of the ratio path. Any
# failure stops. Diagnostics compare the adjusted initiation curves implied
# by the anchored run against the youth series itself: these involve an
# approximation (the band-to-cohort mapping), so they are plotted and
# tabulated rather than thresholded, with one exception - the implied
# young-age series must decline over the anchored window, because restoring
# that decline is the reason the anchor exists.
#
# Run after 10_run_smoking_transitions.R. Works from the project root, like
# the other scripts in this folder.

library(data.table)
library(ggplot2)

configs <- readRDS("inst/extdata/report_configs.rds")

out_dir <- "transition_probability_validation/outputs"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tol <- 1e-8

for(country in names(configs)) {

  # Prefer the config saved in the run manifest, which travels atomically with
  # the outputs it produced. The report_configs.rds snapshot is only written
  # by section 4 of 10_run and can go stale between runs; if the manifest
  # predates configs being saved there, the snapshot is used and that is said
  # out loud, because then the checks are against the config as last
  # snapshotted, not the config the run provably used.
  snapshot_config <- configs[[country]]
  manifest_file <- file.path(snapshot_config$path, "outputs",
                             paste0("run_manifest_", country, ".rds"))
  if(!file.exists(manifest_file)) {
    stop("24_verify_youth_anchor: missing ", manifest_file)
  }
  manifest <- readRDS(manifest_file)
  if(!is.null(manifest$config)) {
    config <- manifest$config
    message("24_verify_youth_anchor (", country, "): using the config from ",
            "the run manifest (run of ", format(manifest$run_datetime), ")")
  } else {
    config <- snapshot_config
    message("24_verify_youth_anchor (", country, "): run manifest predates ",
            "config-in-manifest; using the report_configs.rds snapshot. ",
            "Checks are against the snapshot, not the run's proven config.")
  }

  es_file <- file.path(config$path, "outputs",
                       paste0("ever_smoke_data_", config$country, ".rds"))
  if(!file.exists(es_file)) stop("24_verify_youth_anchor: missing ", es_file)
  es <- readRDS(es_file)

  anchored_in_config <- !is.null(config$youth_anchor_file)
  anchored_in_output <- !is.null(es$anchor)

  # ---- 0. Config and output must agree about whether anchoring ran --------
  # With the manifest config this failing means the outputs folder holds
  # files from different runs; with the snapshot config it can also mean the
  # snapshot is stale (re-run section 4 of 10_run to refresh it).
  if(anchored_in_config != anchored_in_output) {
    stop("24_verify_youth_anchor (", country, "): config says anchoring is ",
         if(anchored_in_config) "on" else "off", " but the saved ever_smoke ",
         "output says it is ", if(anchored_in_output) "on" else "off",
         ". Either the outputs folder mixes files from different runs, or ",
         "the config source named above is stale.")
  }
  if(!anchored_in_config) {
    message("24_verify_youth_anchor (", country, "): anchoring not enabled; ",
            "confirmed absent from outputs. Skipping.")
    next
  }

  if(es$anchor$anchor_age_centre != config$youth_anchor_age_centre |
     es$anchor$taper_cohorts != config$youth_anchor_taper) {
    stop("24_verify_youth_anchor (", country, "): anchor parameters in the ",
         "saved output do not match the config.")
  }

  youth <- fread(config$youth_anchor_file)
  trend <- copy(es$predicted_values)
  links <- es$anchor$links
  path  <- es$anchor$ratio_path
  ref_age <- config$ref_age
  centre  <- config$youth_anchor_age_centre
  taper   <- config$youth_anchor_taper

  sex_mode <- es$anchor$by_sex
  if(sex_mode != ("sex" %in% names(youth))) {
    stop("24_verify_youth_anchor (", country, "): by_sex flag does not match ",
         "the input file.")
  }
  youth[, anchor_grp := if(sex_mode) as.character(sex) else "all"]
  trend[, anchor_grp := if(sex_mode) as.character(sex) else "all"]
  trend[, cohort := year - ref_age]
  youth[, cohort := survey_year - centre]

  for(g in links$anchor_grp) {

    lk <- links[anchor_grp == g]
    yg <- youth[anchor_grp == g][order(cohort)]
    tg <- trend[anchor_grp == g]
    pg <- path[anchor_grp == g]

    # ---- 1. last_supported recomputed from the data points ----------------
    last_sup <- if(sex_mode) {
      max(es$data_points[sex == g, year]) - ref_age
    } else {
      max(es$data_points$year) - ref_age
    }
    if(last_sup != lk$last_supported_cohort) {
      stop("24_verify_youth_anchor (", country, "/", g, "): last supported ",
           "cohort recomputes to ", last_sup, ", saved as ",
           lk$last_supported_cohort)
    }

    # ---- 2. Link factor recomputed from scratch ---------------------------
    # Cohorts at or before last_sup carry ratio 1, so the saved (anchored)
    # targets there are the original fitted trend and the link factor can be
    # rebuilt fully independently: interpolated youth series against the mean
    # fitted target over the overlap cohorts.
    grid <- min(yg$cohort):max(yg$cohort)
    yi <- data.table(cohort = grid,
                     p_youth = stats::approx(yg$cohort, yg$p_ever_smoked,
                                             xout = grid)$y)
    tm <- tg[, .(fitted_mean = mean(fitted_trends)), by = cohort]
    overlap <- yi[cohort <= last_sup, cohort]
    L_check <- merge(yi[cohort %in% overlap], tm, by = "cohort")[
      , mean(fitted_mean / p_youth)]
    if(abs(L_check - lk$link_factor) > tol) {
      stop("24_verify_youth_anchor (", country, "/", g, "): link factor ",
           "recomputes to ", L_check, ", saved as ", lk$link_factor)
    }

    # ---- 3. Ratio path structure ------------------------------------------
    if(pg[cohort <= last_sup & ratio != 1, .N] > 0) {
      stop("24_verify_youth_anchor (", country, "/", g, "): ratio differs ",
           "from 1 inside the trend-supported cohorts.")
    }
    if(pg[cohort > lk$last_anchored_cohort &
          abs(ratio - lk$tail_ratio) > tol, .N] > 0) {
      stop("24_verify_youth_anchor (", country, "/", g, "): tail ratio not ",
           "held constant beyond the last anchored cohort.")
    }
    if(pg[!is.finite(ratio) | ratio <= 0, .N] > 0) {
      stop("24_verify_youth_anchor (", country, "/", g, "): non-finite or ",
           "non-positive ratio in the saved path.")
    }
    if(!identical(sort(pg$cohort), min(tm$cohort):max(tm$cohort))) {
      stop("24_verify_youth_anchor (", country, "/", g, "): ratio path does ",
           "not cover the trend cohorts exactly.")
    }

    # ---- 4. Anchored targets equal the linked youth series ----------------
    # Outside the taper and up to the last anchored cohort, the mean target
    # must equal p_youth * L exactly. This is the decisive end-to-end check:
    # every input to the right-hand side is independent of the saved targets.
    anch_cohorts <- (last_sup + taper + 1):lk$last_anchored_cohort
    chk <- merge(tm[cohort %in% anch_cohorts],
                 yi[cohort %in% anch_cohorts], by = "cohort")
    chk[, expected := p_youth * lk$link_factor]
    if(chk[abs(fitted_mean - expected) > tol, .N] > 0) {
      stop("24_verify_youth_anchor (", country, "/", g, "): anchored targets ",
           "do not equal the linked youth series at cohorts ",
           paste(chk[abs(fitted_mean - expected) > tol, cohort], collapse = ", "))
    }

    # ---- 5. Taper arithmetic ----------------------------------------------
    # In the taper, anchored = orig * (1 - w) + w * youth * L with
    # orig = anchored / ratio, which rearranges to
    # anchored * (1 - (1 - w) / ratio) = w * youth * L.
    if(taper > 0) {
      tp <- data.table(cohort = (last_sup + 1):(last_sup + taper),
                       w = seq_len(taper) / (taper + 1))
      tp <- merge(tp, tm, by = "cohort")
      tp <- merge(tp, yi, by = "cohort")
      tp <- merge(tp, pg[, .(cohort, ratio)], by = "cohort")
      tp[, lhs := fitted_mean * (1 - (1 - w) / ratio)]
      tp[, rhs := w * p_youth * lk$link_factor]
      if(tp[abs(lhs - rhs) > tol, .N] > 0) {
        stop("24_verify_youth_anchor (", country, "/", g, "): taper blend ",
             "identity fails at cohorts ",
             paste(tp[abs(lhs - rhs) > tol, cohort], collapse = ", "))
      }
    }

    message("24_verify_youth_anchor (", country, "/", g, "): hard checks ",
            "passed (link ", round(lk$link_factor, 3), ", cohorts ",
            last_sup + 1, "-", lk$last_anchored_cohort, " anchored)")
  }

  # ---- 6. Diagnostics against the adjusted initiation curves --------------
  adj_file <- file.path(config$path, "outputs",
                        paste0("init_data_adj_", config$country, ".rds"))
  if(!file.exists(adj_file)) stop("24_verify_youth_anchor: missing ", adj_file)
  adj <- readRDS(adj_file)

  # Proportion starting between the 11th and 16th birthdays, by period year:
  # the cross-section counterpart of the youth series. Only years in which
  # all five ages are present are kept - early years draw on cohorts that
  # predate the estimation window, and averaging a partial band understates
  # the flow. The comparison is of shape, not level: the implied series is a
  # flow on the calibrated recall scale, the youth series a stock on the
  # child-report scale, and the flow completes at 16 while the stock is
  # observed around 14, so the implied series also trails the youth series by
  # two to four years by construction.
  adj <- adj[order(sex, imd_quintile, cohort, age)]
  adj[, dens := shift(p_ever_smoker_adj, type = "lead") - p_ever_smoker_adj,
      by = .(sex, imd_quintile, cohort)]
  band <- adj[age %in% 11:15 & !is.na(dens)]
  n_strata <- band[, uniqueN(paste(sex, imd_quintile))]
  cover <- band[, .(n_cells = .N), by = year]
  full_years <- cover[n_cells == 5 * n_strata, year]
  dropped <- sort(setdiff(cover$year, full_years))
  if(length(dropped) > 0) {
    message("24_verify_youth_anchor (", country, "): dropping years without ",
            "complete age 11-15 coverage from the diagnostic: ",
            paste(range(dropped), collapse = "-"))
  }
  implied <- band[year %in% full_years,
                  .(implied = mean(dens) * 5), by = .(year, sex)]

  # Years in which every age 11-15 is filled by an anchored cohort.
  first_full <- links[, max(last_supported_cohort + config$youth_anchor_taper)] + 16
  last_full  <- min(links$last_anchored_cohort) + 11
  window <- implied[year >= first_full & year <= last_full]

  # Hard check: the implied series declines over the anchored window. If it
  # does not, the anchoring has not reached the initiation curves and the
  # flat-young-age problem it exists to fix is back.
  decline <- window[, .(first = implied[year == min(year)],
                        last = implied[year == max(year)]), by = sex]
  if(decline[last >= first, .N] > 0) {
    stop("24_verify_youth_anchor (", country, "): implied ages 11-15 ",
         "initiation does not decline over the anchored window (",
         paste(decline[last >= first, sex], collapse = ", "), ").")
  }

  print(dcast(window, year ~ sex, value.var = "implied"), digits = 3)

  plot_dt <- rbind(
    implied[, .(year, sex, value = implied, series = "Implied from adjusted curves")],
    youth[, .(year = survey_year, sex = if(sex_mode) sex else "all",
              value = p_ever_smoked, series = "Youth survey")])

  # Index both series to their own mean over the fully anchored window, so the
  # comparison the plot invites is the one the anchor actually makes: shape.
  # Raw levels differ by instrument and by flow-versus-stock, and a raw-scale
  # plot reads as a misfit when it is a unit mismatch.
  window_mean <- plot_dt[year >= first_full & year <= last_full,
                         .(wm = mean(value)), by = .(sex, series)]
  plot_dt <- merge(plot_dt, window_mean, by = c("sex", "series"))
  if(plot_dt[!is.finite(wm) | wm <= 0, .N] > 0) {
    stop("24_verify_youth_anchor (", country, "): cannot index the ",
         "diagnostic plot - a series has no positive values in the anchored ",
         "window.")
  }
  plot_dt[, index := value / wm]

  p <- ggplot(plot_dt, aes(x = year, y = index, colour = sex,
                           linetype = series)) +
    geom_line(data = plot_dt[series != "Youth survey"]) +
    geom_point(data = plot_dt[series == "Youth survey"], size = 1) +
    annotate("rect", xmin = first_full, xmax = last_full,
             ymin = -Inf, ymax = Inf, alpha = 0.08) +
    labs(title = paste0("Youth anchor: ", country),
         subtitle = paste0("Both series indexed to their mean over the fully ",
                           "anchored window (shaded).\nShape comparison; the ",
                           "implied flow trails the youth stock by 2-4 years ",
                           "by construction."),
         y = "index (anchored window mean = 1)", x = NULL) +
    theme_minimal()

  ggsave(file.path(out_dir, paste0("youth_anchor_", tolower(country), ".png")),
         p, width = 8, height = 4.5, dpi = 150)

  message("24_verify_youth_anchor (", country, "): diagnostics written to ",
          out_dir)
}
