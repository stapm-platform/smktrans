library(testthat)
library(data.table)

# These tests check hawkins_relapse against the paper it is transcribed from:
# Hawkins, Hollingworth & Campbell (2010), Nicotine & Tobacco Research
# 12(12):1228-1235, doi:10.1093/ntr/ntq175.
#
# The published table is independent of our data-raw code, so this is a real
# check and not just an internal consistency test. If data-raw is ever edited
# and these fail, the edit has moved us away from the paper.

# Table 2 as published.
paper_tab2 <- data.table(
  time_since_quit = 1:10,
  n_abstinent     = c(1578, 1128, 832, 645, 535, 410, 317, 254, 213, 180),
  n_relapsing     = c(227, 95, 48, 25, 16, 6, 6, 1, 4, 0),
  p_paper         = c(15.1, 7.9, 4.9, 3.0, 2.3, 1.1, 1.4, 0.3, 1.3, 0.0) / 100
)

# The Table 3 reference person: all odds ratios equal 1, at the age the script
# centres on. This is the person the Table 2 marginal gets assigned to, so their
# probability should BE the marginal.
ref_person <- function(dt, tsq) {
  dt[time_since_quit == tsq & age == 45 & sex == "Female" & degree == "no_degree" &
     relationship_status == "single" & employ2cat == "employed" &
     hse_mental == "no_mental" & income5cat == "1_lowest_income" &
     imd_quintile == "1_least_deprived"]$p_relapse
}


test_that("the structure prep_relapse and the model expect is unchanged", {
  dt <- as.data.table(smktrans::hawkins_relapse)
  expect_equal(sort(unique(dt$time_since_quit)), 0:10)
  expect_equal(sort(unique(dt$age)), 1:89)
  expect_setequal(unique(dt$sex), c("Male", "Female"))
  expect_setequal(unique(dt$imd_quintile),
                  c("1_least_deprived", "2", "3", "4", "5_most_deprived"))
  expect_true("p_relapse" %in% names(dt))
})

test_that("p_relapse is a probability", {
  dt <- as.data.table(smktrans::hawkins_relapse)
  expect_false(anyNA(dt$p_relapse))
  expect_true(all(dt$p_relapse >= 0 & dt$p_relapse <= 1))
})


test_that("years 1-5 carry the Table 2 marginals, calibrated", {
  # Years 1-5 come straight from Table 2 with no pooling. The reference person no
  # longer equals the published percentage, because the calibration moved the
  # marginal off her and onto the cohort. Her odds are the marginal odds times
  # the calibration factor.
  dt <- as.data.table(smktrans::hawkins_relapse)
  ro <- function(p) p/(1-p)
  fac <- ro(ref_person(dt, 1)) / ro(paper_tab2[time_since_quit == 1]$p_paper)
  for (i in 1:5) {
    expect_equal(ro(ref_person(dt, i)),
                 ro(paper_tab2[time_since_quit == i]$p_paper) * fac,
                 tolerance = 1e-6,
                 info = paste("time_since_quit =", i))
  }
})

test_that("years 6-9 are pooled to the rate implied by the paper's counts", {
  # The paper groups 6+ years into one category because the yearly counts are
  # too small - 6, 6, 1 and 4 relapses. We pool the counts, weighting each year
  # by how many people were at risk in it.
  dt <- as.data.table(smktrans::hawkins_relapse)
  ro <- function(p) p/(1-p)
  pooled <- paper_tab2[time_since_quit %in% 6:9, sum(n_relapsing) / sum(n_abstinent)]
  fac <- ro(ref_person(dt, 1)) / ro(paper_tab2[time_since_quit == 1]$p_paper)

  for (i in 6:9) {
    expect_equal(ro(ref_person(dt, i)), ro(pooled) * fac, tolerance = 1e-6,
                 info = paste("time_since_quit =", i))
  }
  # and they must all be the same
  expect_equal(uniqueN(round(sapply(6:9, function(i) ref_person(dt, i)), 10)), 1)
})

test_that("the table reproduces the paper for the paper's own cohort", {
  # The check the pre-calibration version could not pass. Rebuild the cohort
  # Table 1 describes, run it through the lookup, and it has to come out at the
  # 15.1% Table 2 reports. Before calibration it gave 14.28%, i.e. 94.6% of it.
  dt <- as.data.table(smktrans::hawkins_relapse)

  tab1 <- function(never, relapsed) (1147 * never + 431 * relapsed) / 1578
  p_male <- tab1(47.4, 41.6)/100; p_degree <- tab1(12.7, 10.8)/100
  p_unemp <- tab1(3.1, 6.3)/100;  p_married <- tab1(59.0, 46.1)/100
  p_cohab <- tab1(71.6, 63.6)/100; p_mental <- tab1(7.3, 11.2)/100
  p_income <- c(tab1(21.4,21.4), tab1(26.1,28.2), tab1(25.9,24.8), tab1(26.5,25.7))/100
  age_mean <- tab1(44.8, 38.2)
  age_sd <- sqrt((1146*16.7^2 + 430*14.4^2)/1576 +
                 (1147*(44.8-age_mean)^2 + 431*(38.2-age_mean)^2)/1578)

  ages <- 18:89
  mom <- function(par) {
    d <- dnorm(ages, par[1], abs(par[2])); d <- d/sum(d)
    m <- sum(d*ages); c(m, sqrt(sum(d*(ages-m)^2)))
  }
  fit <- optim(c(age_mean, age_sd), function(p) sum((mom(p) - c(age_mean, age_sd))^2),
               control = list(reltol = 1e-13, maxit = 8000))
  age_wt <- data.table(age = ages,
                       w_age = {d <- dnorm(ages, fit$par[1], abs(fit$par[2])); d/sum(d)})

  coh <- CJ(age = ages, male = c(TRUE, FALSE), degree = c(TRUE, FALSE),
            unemp = c(TRUE, FALSE), partner = c("married", "cohab_only", "neither"),
            mental = c(TRUE, FALSE), inc = 1:4)
  coh <- merge(coh, age_wt, by = "age")
  coh[, wt := w_age * fifelse(male, p_male, 1-p_male) *
        fifelse(degree, p_degree, 1-p_degree) * fifelse(unemp, p_unemp, 1-p_unemp) *
        fcase(partner == "married", p_married, partner == "cohab_only", p_cohab - p_married,
              default = 1 - p_cohab) *
        fifelse(mental, p_mental, 1-p_mental) * p_income[inc]]
  coh[, wt := wt/sum(wt)]
  coh[, m := 0.96^(age - 45) * fifelse(male, 1.15, 1) * fifelse(degree, 0.60, 1) *
        fifelse(unemp, 0.58, 1) *
        fcase(partner == "married", 0.60*0.91, partner == "cohab_only", 0.91, default = 1) *
        fifelse(mental, 2.49, 1) * c(1, 0.91, 0.99, 0.87)[inc]]

  # the cohort must be the one Table 1 describes
  expect_equal(coh[, sum(wt * age)], age_mean, tolerance = 0.01)
  expect_equal(coh[male == TRUE, sum(wt)], p_male, tolerance = 1e-6)

  ref <- ref_person(dt, 1)
  o <- (ref/(1-ref)) * coh$m
  expect_equal(sum(coh$wt * o/(1+o)), 0.151, tolerance = 1e-5)
})

test_that("the reference person no longer just IS the Table 2 marginal", {
  # Before calibration the reference person's probability was set equal to the
  # Table 2 marginal, which was the bug. Now the marginal belongs to the cohort
  # and the reference person sits above it, because she is single and most of
  # the cohort were not.
  dt <- as.data.table(smktrans::hawkins_relapse)
  expect_gt(ref_person(dt, 1), 0.151)
  # calibration factor was 1.0726 on the odds
  o_marg <- 0.151/(1-0.151)
  o_ref <- ref_person(dt, 1)/(1 - ref_person(dt, 1))
  expect_equal(o_ref/o_marg, 1.0726, tolerance = 1e-3)
})



test_that("relapse falls with time since quit", {
  # This is what the pooling was for. The old data rose at tsq 7 and tsq 9,
  # inheriting sampling noise from 1 and 4 relapses.
  dt <- as.data.table(smktrans::hawkins_relapse)
  prof <- dt[, .(p = mean(p_relapse)), by = time_since_quit][order(time_since_quit)]
  expect_true(all(diff(prof$p) <= 1e-12))
})

test_that("time_since_quit = 0 exceeds year 1, by the Jackson placebo ratio", {
  # Someone at tsq 0 has been abstinent about 21 weeks rather than 52, so they
  # are earlier on the abstinence curve and at higher risk.
  dt <- as.data.table(smktrans::hawkins_relapse)
  ro <- function(p) p/(1-p)
  p0 <- ref_person(dt, 0); p1 <- ref_person(dt, 1)
  expect_gt(p0, p1)

  wk <- 1:52
  pa <- smktrans::SmkContAbst("placebo", wk)
  dur <- sum(wk * pa) / sum(pa)
  adj0 <- smktrans::SmkContAbst("placebo", dur) / smktrans::SmkContAbst("placebo", 52)
  # tsq 0's percentage is year 1's scaled by adj0, then both go through the same
  # calibration, so the ratio of the underlying percentages survives.
  fac <- ro(p1) / ro(paper_tab2[time_since_quit == 1]$p_paper)
  expect_equal(ro(p0), ro(paper_tab2[time_since_quit == 1]$p_paper * adj0) * fac,
               tolerance = 1e-6)
})

test_that("time_since_quit = 10 is zero", {
  # Table 2 has 0 relapses out of 180 in year 10. prep_relapse also hard-zeros
  # anything at or beyond 10, so the two agree. Note this is a convention rather
  # than a finding: the paper's KM curve is still declining past 10 years.
  dt <- as.data.table(smktrans::hawkins_relapse)
  expect_true(all(dt[time_since_quit == 10]$p_relapse == 0))
})


test_that("the odds ratios are the ones in the paper's Table 3", {
  dt <- as.data.table(smktrans::hawkins_relapse)
  ro <- function(p) p / (1 - p)
  # pick a cell and vary one covariate at a time
  base <- dt[time_since_quit == 1 & age == 45 & sex == "Female" & degree == "no_degree" &
             relationship_status == "single" & employ2cat == "employed" &
             hse_mental == "no_mental" & income5cat == "1_lowest_income" &
             imd_quintile == "1_least_deprived"]$p_relapse

  vary <- function(...) {
    f <- list(...)
    d <- dt[time_since_quit == 1 & age == 45 & imd_quintile == "1_least_deprived"]
    for (nm in names(f)) d <- d[get(nm) == f[[nm]]]
    for (nm in setdiff(c("sex","degree","relationship_status","employ2cat","hse_mental","income5cat"),
                       names(f))) {
      d <- d[get(nm) == c(sex="Female", degree="no_degree", relationship_status="single",
                          employ2cat="employed", hse_mental="no_mental",
                          income5cat="1_lowest_income")[nm]]
    }
    d$p_relapse
  }

  expect_equal(ro(vary(sex = "Male")) / ro(base), 1.15, tolerance = 1e-6)   # Table 3 Male
  expect_equal(ro(vary(degree = "degree")) / ro(base), 0.60, tolerance = 1e-6)
  expect_equal(ro(vary(employ2cat = "unemployed")) / ro(base), 0.58, tolerance = 1e-6)
  expect_equal(ro(vary(hse_mental = "mental")) / ro(base), 2.49, tolerance = 1e-6)

  # Married and Cohabiting are separate binary covariates in Table 3, and
  # Table 1 shows cohabiting includes the married (59.0% + 71.6% > 100%), so a
  # married person carries both odds ratios.
  expect_equal(ro(vary(relationship_status = "married")) / ro(base), 0.60 * 0.91,
               tolerance = 1e-6)
  expect_equal(ro(vary(relationship_status = "cohabit")) / ro(base), 0.91, tolerance = 1e-6)
})

test_that("age enters as the paper's per-year odds ratio", {
  dt <- as.data.table(smktrans::hawkins_relapse)
  ro <- function(p) p / (1 - p)
  cell <- function(a) {
    dt[time_since_quit == 1 & age == a & sex == "Female" & degree == "no_degree" &
       relationship_status == "single" & employ2cat == "employed" &
       hse_mental == "no_mental" & income5cat == "1_lowest_income" &
       imd_quintile == "1_least_deprived"]$p_relapse
  }
  # Table 3: Age (years) 0.96 per single year
  expect_equal(ro(cell(46)) / ro(cell(45)), 0.96, tolerance = 1e-6)
  expect_equal(ro(cell(55)) / ro(cell(45)), 0.96 ^ 10, tolerance = 1e-6)
})

test_that("under-18s take the age-18 values", {
  dt <- as.data.table(smktrans::hawkins_relapse)
  a18 <- dt[age == 18][order(time_since_quit, sex, degree, relationship_status,
                             employ2cat, hse_mental, income5cat, imd_quintile)]$p_relapse
  a12 <- dt[age == 12][order(time_since_quit, sex, degree, relationship_status,
                             employ2cat, hse_mental, income5cat, imd_quintile)]$p_relapse
  expect_equal(a12, a18)
})


test_that("SmkContAbst reproduces its documented values", {
  expect_equal(smktrans::SmkContAbst("placebo", 0:4),
               c(1.0000000, 0.4129423, 0.3152202, 0.2691612, 0.2406239), tolerance = 1e-6)
  expect_equal(smktrans::SmkContAbst("varenicline", 4) / smktrans::SmkContAbst("placebo", 4),
               2.279061, tolerance = 1e-5)
  expect_equal(1000 * (smktrans::SmkContAbst("placebo", 12) / smktrans::SmkContAbst("placebo", 4)),
               651.8131, tolerance = 1e-3)
})

test_that("SmkContAbst complains rather than extrapolating quietly", {
  expect_warning(smktrans::SmkContAbst("placebo", 60), "52 weeks")
  expect_error(smktrans::SmkContAbst("placebo", -1), "negative")
  expect_error(smktrans::SmkContAbst("not_a_treatment", 4))
})
