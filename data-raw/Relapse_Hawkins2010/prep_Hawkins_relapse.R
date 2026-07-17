
# The aim of this code is to prep the estimates of relapse to smoking from
# Hawkins J, Hollingworth W, Campbell R. Long-term smoking relapse:
# a study using the British Household Panel Survey.
# Nicotine & Tobacco Research. 2010 Oct 29;12(12):1228-35.
# https://doi.org/10.1093/ntr/ntq175

# With an adjusted relapse probability added for people who have quit for less than a year
# based on the placebo curve from Jackson et al.
# https://doi.org/10.1111/add.14549

# Two things to hold in mind while reading this.
#
# We take the shape over time since quit from the paper's Table 2, which is the
# raw observed relapse in each year of follow-up, and we take the covariate
# effects from its Table 3, which is a fitted random effects logit model. Those
# are different objects. Table 2 is a marginal over the whole cohort and Table 3
# reports odds ratios relative to a reference person. The paper does not publish
# the intercept of the model, so we cannot look the reference person's rate up.
# We solve for it instead. See the calibration section below.
#
# This script does not run as part of the estimation workflow. estimate_relapse()
# reads smktrans::hawkins_relapse, which is the package data, so nothing changed
# in here reaches the model until use_data() runs at the bottom.

library(data.table)
library(smktrans)

###################################
# The odds ratios from Table 3
#
# Kept here as named values rather than scattered through the code, because the
# calibration further down has to apply exactly the same set. If the two ever
# drift apart the calibration goes stale without anything complaining.

OR_AGE     <- 0.96     # per single year of age
OR_MALE    <- 1.15
OR_DEGREE  <- 0.60
OR_UNEMP   <- 0.58
OR_MARRIED <- 0.60
OR_COHAB   <- 0.91
OR_MENTAL  <- 2.49
OR_INCOME  <- c(1, 0.91, 0.99, 0.87)   # the paper's income quartiles 1 to 4

# The age at which the age odds ratio is 1. This used to matter and no longer
# does, because the calibration solves for a baseline given whatever centring we
# choose here. Set it to 43 and the calibration lands on a baseline that is
# 0.96^2 different, and every probability in the final table comes out
# identical. There is a check at the bottom that this is true.
AGE_CENTRE <- 45

# Read in csv file from Hawkins 2010 paper.
# This is Table 2 as published: the number abstinent at the start of each year
# of follow-up, the number relapsing during it, and the percentage.
relapse <- fread("data-raw/Relapse_Hawkins2010/Smoking_Relapse_Hawkins_percentage.csv")

###################################
# Group the sparse years, as the paper does
#
# The paper's own model groups everything from 6 years of abstinence onwards
# into a single category and says why: "We grouped length of abstinence above
# 5 years because a relatively small number of individuals relapsed in any one
# of these years." The counts bear that out. Years 6 to 9 rest on 6, 6, 1 and 4
# relapses. One person.
#
# We were taking the year specific percentages straight from Table 2, which put
# that noise into the estimates. Relapse went up from year 6 to 7 (1.1% to 1.4%)
# and again from year 8 to 9 (0.3% to 1.3%). A relapse hazard that rises with
# years abstinent is not credible, and because p_smooth in prep_relapse smooths
# within each time since quit and never across them, it went straight into the
# model.
#
# So pool years 6 to 9 into one rate. We pool the counts rather than average the
# percentages, so each year contributes in proportion to how many people were at
# risk in it.
#
# Why 6 to 9 and not 6 to 10. prep_relapse sets time_since_quit >= 10 to a hard
# zero, so year 10 never carries a rate at all. Pooling exactly the years that do
# leaves the structure of this table unchanged at time_since_quit 0 to 10, which
# is what the model is built to receive.
#
# Worth noting in passing that the zero at 10 years rests on 0 relapses out of
# 180 in a single year, and the paper's Kaplan-Meier curve is still declining
# past 10. Relapse stopping at 10 years is our convention, not the paper's
# finding. Left alone because prep_relapse enforces it anyway.

pool_years <- 6:9

pooled_pct <- 100 * relapse[Quit %in% pool_years, sum(Relapse)] /
                    relapse[Quit %in% pool_years, sum(StartAbs)]

message(sprintf("Pooling years %s: %d relapses / %d at risk = %.3f%% (was %s)",
                paste(range(pool_years), collapse = "-"),
                relapse[Quit %in% pool_years, sum(Relapse)],
                relapse[Quit %in% pool_years, sum(StartAbs)],
                pooled_pct,
                paste(relapse[Quit %in% pool_years]$Percentage, collapse = ", ")))

if(pooled_pct >= relapse[Quit == 5, Percentage]) {
  stop("The pooled 6-9 rate (", round(pooled_pct, 3), "%) is not below the year 5 rate (",
       relapse[Quit == 5, Percentage], "%). Pooling was supposed to fix the profile, ",
       "not break it. Check the Table 2 counts.")
}

relapse[Quit %in% pool_years, Percentage := pooled_pct]

###################################
# Insert additional relapse probability for people who have quit for less than a year
#
# Why this is here. The model these data feed is built on annual repeat cross
# sectional survey data, so a person is a smoker at one tick and a non-smoker at
# the next, and we never see when in the year they stopped. Someone who quit at
# some point during that year and is still abstinent at the next tick has been
# abstinent for about 21 weeks on average, not 52. Hawkins only starts at a year
# of abstinence, so we have to extrapolate its year 1 rate back to the shorter
# time step, and the placebo abstinence curve from Jackson et al. is what we use
# to do it.
#
# adj0 is the ratio of the placebo curve at 21 weeks to its value at 52 weeks,
# which is above 1 because abstinence is still falling across that stretch. It
# scales the year 1 rate up to reflect people at time_since_quit 0 being earlier
# and more fragile than the people Hawkins observed. It is a heuristic rather
# than a hazard calculation. Doing it exactly would need the placebo curve past
# 52 weeks and Jackson does not fit it there.

relapse0 <- data.table(Quit = as.integer(0))
relapse <- rbindlist(list(relapse0, relapse), use.names = T, fill = T)

# Calculate the expected duration of time since quitting for people who have quit
# for less than a year, assuming use of NRT
wk <- 1:52
pa <- smktrans::SmkContAbst("placebo", wk)
dur <- sum(wk * pa) / sum(pa)
# 21.2 weeks

# Calculate adjustment factor
adj0 <- smktrans::SmkContAbst("placebo", dur) / smktrans::SmkContAbst("placebo", 52)

if(adj0 <= 1) {
  stop("adj0 came out at ", round(adj0, 4), ", which cannot be right. Someone abstinent ",
       "for ", round(dur, 1), " weeks is not at lower risk than someone abstinent for 52. ",
       "Check SmkContAbst.")
}

# Calculate the percentage who relapse
relapse[Quit == 0, Percentage := relapse[Quit == 1, Percentage] * adj0]

###################################
# Calibrate the baseline
#
# The problem. We hand the Table 2 marginal to the Table 3 reference person: a
# 45 year old woman with no degree, employed, single, no mental health problems,
# in the bottom income group. Every odds ratio is 1 for her, so she gets 15.1%
# at one year. But 15.1% is the average across everybody in the cohort, not her
# rate. She is not the average person. She is fairly typical on sex, education,
# employment and mental health, but she is single, and 69% of the cohort were
# living with a partner, which carries the largest odds ratio in the set after
# mental health.
#
# The consequence is checkable rather than theoretical. Rebuild the cohort that
# Table 1 describes, run it through this lookup table, and we get 14.28%
# relapsing at one year where the paper says 15.10%. We cannot reproduce the
# number we built the table from, applied to the very people we built it on.
# Everything in the table is about 5% low.
#
# It does not wash out downstream. The error is a constant shift in the
# intercept, so every cell is shifted by the same amount on the logit scale.
# When prep_relapse merges this table with the England profile it chooses which
# people get what weight, which is the mix, not the level. The level was fixed
# before any weighting happened. Applying the table to younger, older, more
# deprived or more educated populations all give the same 94% of target, because
# it is a property of the table and not of who we merge it with.
#
# So solve for the baseline rather than assume it. Rebuild the cohort as a
# weighted grid from the marginals in Table 1 and find the baseline odds at one
# year that makes the cohort weighted mean come out at 15.1%.
#
# Three things this rests on.
#
# Table 1 gives marginals and not the joint distribution, so we have to treat the
# covariates as independent. They are not: degree and income obviously go
# together. Forcing correlations in moves the answer by around 4%, which is small
# against the 7% correction.
#
# The age effect is the one that matters, because 0.96 per year compounds across
# a spread of 16 years. We fit the age distribution so that once it is
# discretised over the ages in this table it hits the mean and standard deviation
# Table 1 implies. This is easy to get wrong. Passing the Table 1 mean of 43
# straight into dnorm and truncating at 18 gives a distribution whose mean is
# 44.9, because chopping the left tail off drags it up, and that one mistake
# moves the correction from 1.07 to 1.20. Once the moments are right the shape
# stops mattering: normal, gamma, lognormal, beta and a maximum entropy fit all
# land between 1.072 and 1.076.
#
# We solve once, at one year, and apply the same factor everywhere. Solving
# separately at each time since quit lands exactly on Table 2 every time, but the
# precision would be false. Table 1 describes people who had been abstinent for a
# year. By five years the survivors have been selected towards the lower risk
# end and we have no description of them, so solving per year would be fitting to
# the wrong population for every year but the first. One factor, from the year we
# can actually calibrate, is the honest version.

# The Hawkins cohort, from Table 1. n = 1147 never relapsed, n = 431 did.
tab1 <- function(never, relapsed) (1147 * never + 431 * relapsed) / 1578

p_male    <- tab1(47.4, 41.6) / 100
p_degree  <- tab1(12.7, 10.8) / 100
p_unemp   <- tab1(3.1, 6.3) / 100
p_married <- tab1(59.0, 46.1) / 100
p_cohab   <- tab1(71.6, 63.6) / 100
p_mental  <- tab1(7.3, 11.2) / 100
p_income  <- c(tab1(21.4, 21.4), tab1(26.1, 28.2),
               tab1(25.9, 24.8), tab1(26.5, 25.7)) / 100

# Age. Table 1 reports the mean and sd separately for the two groups, so pool
# them: the within group variance plus the variance between the group means.
age_mean <- tab1(44.8, 38.2)
age_sd <- sqrt(
  (1146 * 16.7 ^ 2 + 430 * 14.4 ^ 2) / 1576 +
  (1147 * (44.8 - age_mean) ^ 2 + 431 * (38.2 - age_mean) ^ 2) / 1578
)

# Fit the age distribution on the discretised support, not the continuous one.
cal_ages <- 18:89
age_moments <- function(par) {
  d <- dnorm(cal_ages, par[1], abs(par[2]))
  d <- d / sum(d)
  m <- sum(d * cal_ages)
  c(m, sqrt(sum(d * (cal_ages - m) ^ 2)))
}
age_fit <- optim(c(age_mean, age_sd),
                 function(par) sum((age_moments(par) - c(age_mean, age_sd)) ^ 2),
                 control = list(reltol = 1e-13, maxit = 8000))

if(sum((age_moments(age_fit$par) - c(age_mean, age_sd)) ^ 2) > 1e-6) {
  stop("Could not fit an age distribution matching Table 1's mean of ", round(age_mean, 2),
       " and sd of ", round(age_sd, 2), " over ages ", min(cal_ages), " to ", max(cal_ages),
       ". The calibration below would be built on the wrong age profile.")
}

age_wt <- data.table(age = cal_ages,
                     w_age = {d <- dnorm(cal_ages, age_fit$par[1], abs(age_fit$par[2])); d / sum(d)})

# Enumerate the cohort. 72 ages by 192 covariate combinations, weighted by the
# Table 1 marginals. Deterministic, so no seed and no simulation error.
cohort <- CJ(age     = cal_ages,
             male    = c(TRUE, FALSE),
             degree  = c(TRUE, FALSE),
             unemp   = c(TRUE, FALSE),
             partner = c("married", "cohab_only", "neither"),
             mental  = c(TRUE, FALSE),
             inc     = 1:4)
cohort <- merge(cohort, age_wt, by = "age")

cohort[, wt := w_age *
         fifelse(male, p_male, 1 - p_male) *
         fifelse(degree, p_degree, 1 - p_degree) *
         fifelse(unemp, p_unemp, 1 - p_unemp) *
         fcase(partner == "married",    p_married,
               partner == "cohab_only", p_cohab - p_married,
               default = 1 - p_cohab) *
         fifelse(mental, p_mental, 1 - p_mental) *
         p_income[inc]]
cohort[, wt := wt / sum(wt)]

# The same odds ratios that get applied to the lookup table below.
cohort[, m := OR_AGE ^ (age - AGE_CENTRE) *
         fifelse(male, OR_MALE, 1) *
         fifelse(degree, OR_DEGREE, 1) *
         fifelse(unemp, OR_UNEMP, 1) *
         fcase(partner == "married",    OR_MARRIED * OR_COHAB,
               partner == "cohab_only", OR_COHAB,
               default = 1) *
         fifelse(mental, OR_MENTAL, 1) *
         OR_INCOME[inc]]

# The rebuilt cohort has to reproduce Table 1 or it is not the Hawkins cohort.
stopifnot(abs(sum(cohort$wt) - 1) < 1e-10)
for(chk in list(list("mean age", cohort[, sum(wt * age)], age_mean, 0.01),
                list("P(male)", cohort[male == TRUE, sum(wt)], p_male, 1e-6),
                list("P(degree)", cohort[degree == TRUE, sum(wt)], p_degree, 1e-6),
                list("P(married)", cohort[partner == "married", sum(wt)], p_married, 1e-6),
                list("P(mental health)", cohort[mental == TRUE, sum(wt)], p_mental, 1e-6))) {
  if(abs(chk[[2]] - chk[[3]]) > chk[[4]]) {
    stop("The rebuilt cohort does not match Table 1 on ", chk[[1]], ": got ",
         round(chk[[2]], 4), ", Table 1 says ", round(chk[[3]], 4), ".")
  }
}

# Solve for the baseline odds that reproduces the one year marginal.
solve_baseline <- function(target, m, wt) {
  f <- function(log_odds) {
    o <- exp(log_odds) * m
    sum(wt * o / (1 + o)) - target
  }
  exp(uniroot(f, c(-12, 5), tol = .Machine$double.eps ^ 0.75)$root)
}

target_1yr <- relapse[Quit == 1, Percentage] / 100
odds_uncal <- target_1yr / (1 - target_1yr)
odds_cal   <- solve_baseline(target_1yr, cohort$m, cohort$wt)

CALIB_FACTOR <- odds_cal / odds_uncal

cohort_before <- {o <- odds_uncal * cohort$m; sum(cohort$wt * o / (1 + o))}
cohort_after  <- {o <- odds_cal   * cohort$m; sum(cohort$wt * o / (1 + o))}

message(sprintf(paste0("Calibration: the uncalibrated table gives the Hawkins cohort %.3f%% at ",
                       "one year against Table 2's %.1f%% (%.1f%% of it).\n",
                       "  Baseline odds %.5f -> %.5f, a factor of %.4f. Calibrated table gives %.3f%%."),
                100 * cohort_before, 100 * target_1yr, 100 * cohort_before / target_1yr,
                odds_uncal, odds_cal, CALIB_FACTOR, 100 * cohort_after))

if(CALIB_FACTOR <= 1) {
  stop("The calibration factor came out at ", round(CALIB_FACTOR, 4), ", at or below 1. ",
       "The reference person is lower risk than the cohort average, so the baseline has to ",
       "go up, not down. Something is wrong with the cohort weights or the odds ratios.")
}
if(CALIB_FACTOR > 1.5) {
  stop("The calibration factor came out at ", round(CALIB_FACTOR, 4), ". Anything much above ",
       "1.1 means the cohort or the odds ratios have changed a lot since this was written ",
       "(it was 1.073). Work out why before letting this through.")
}
if(abs(cohort_after - target_1yr) > 1e-8) {
  stop("The solve did not converge: the calibrated cohort gives ", round(100 * cohort_after, 4),
       "% against a target of ", round(100 * target_1yr, 1), "%.")
}

###################################

# convert the percentage of people in continuous abstinence from percentage to probability and then to odds
relapse[ , Probability := Percentage / 100]

relapse[ , odds := Probability / (1 - Probability)]

# apply the calibration
relapse[ , odds := odds * CALIB_FACTOR]

relapse <- relapse[ , c("Quit", "odds")]
setnames(relapse, "Quit", "time_since_quit")

# expand by covariates
domain <- data.frame(expand.grid(
  time_since_quit = 0:10,
  age = 18:89,
  sex = c("Male", "Female"),
  degree = c("degree", "no_degree"),
  relationship_status = c("single", "married", "sep_div_wid", "cohabit"),
  employ2cat = c("employed", "unemployed"),
  hse_mental = c("mental", "no_mental"),
  income5cat = c("1_lowest_income", "2", "3", "4", "5_highest_income"),
  imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
))
setDT(domain)

# merge with time since quit variation
domain <- merge(domain, relapse, by = "time_since_quit")

# add effect of age
# average age of individuals in study
av_age <- round((1147 * 44.8 + 431 * 38.2) / (1147 + 431), 0)
# = 43. Kept to record what the cohort's average age was. We do not centre on it,
# and since the calibration above it no longer makes any difference which age we
# centre on: change AGE_CENTRE and the solve returns a baseline that offsets it
# exactly. There is a check at the bottom.
domain[ , age_or := OR_AGE ^ (age - AGE_CENTRE)]

# add effect of sex
domain[ , sex_or := 1]
domain[sex == "Male", sex_or := OR_MALE]

# add effect of unemployment
domain[ , employ2cat_or := 1]
domain[employ2cat == "unemployed", employ2cat_or := OR_UNEMP]

# add effect of degree
domain[ , degree_or := 1]
domain[degree == "degree", degree_or := OR_DEGREE]

# add effect of relationship status
#
# Married and Cohabiting are two separate binary covariates in Table 3, not two
# levels of one variable. Table 1 gives Married as 59.0% and 46.1% and
# Cohabiting as 71.6% and 63.6% in the same sample, which comes to about 125%,
# so cohabiting has to include the married. The paper's model therefore hands a
# married person both odds ratios and we were only giving them the 0.60, which
# left married people about 10% too high.
#
# The cohabiting effect is not significant on its own (0.91, CI 0.55 to 1.51).
# We apply it anyway because we are using the model as fitted rather than
# picking out the significant terms, and the same goes for unemployment and
# income below.
domain[ , relationship_status_or := 1]
domain[relationship_status == "married", relationship_status_or := OR_MARRIED * OR_COHAB]
domain[relationship_status == "cohabit", relationship_status_or := OR_COHAB]

# add effects of health
domain[ , mental_health_or := 1]
domain[hse_mental == "mental", mental_health_or := OR_MENTAL]

# effect of income
#
# Table 3 reports income in quartiles and we hold it in quintiles, so there is no
# clean mapping and this is a judgement. Quintiles 1 and 2 both take the
# reference and 3, 4 and 5 take the paper's 2nd, 3rd and 4th quartile odds
# ratios, which puts the reference on the bottom 40% rather than the bottom 25%.
# None of the income effects are significant and they are all close to 1, so it
# makes very little difference, but it is a choice and it should be written down.
domain[ , income_or := 1]
domain[income5cat == "2", income_or := OR_INCOME[1]]
domain[income5cat == "3", income_or := OR_INCOME[2]]
domain[income5cat == "4", income_or := OR_INCOME[3]]
domain[income5cat == "5_highest_income", income_or := OR_INCOME[4]]

# adjust odds for the above effects

domain[ , odds_adj := odds * age_or * sex_or * degree_or * employ2cat_or * relationship_status_or * mental_health_or * income_or]

domain[ , `:=`(age_or = NULL, sex_or = NULL, degree_or = NULL, employ2cat_or = NULL, relationship_status_or = NULL, mental_health_or = NULL, income_or = NULL)]

# covert odds back to probability
domain[ , p_relapse := odds_adj / (odds_adj + 1)]

domain[ , odds := NULL]
domain[ , odds_adj := NULL]

# Assume < 18 year olds have same relapse characteristics as 18 year olds
temp <- copy(domain[age == 18])

for(i in 1:17) {
  domain <- rbindlist(list(
    domain,
    copy(temp[ , age := i])
  ))
}
rm(temp)

###################################
# Checks before saving. If any of these fail do not overwrite the package data.

if(anyNA(domain$p_relapse)) stop("p_relapse contains NAs.")
if(domain[p_relapse < 0 | p_relapse > 1, .N] > 0) stop("p_relapse outside [0,1].")
if(!identical(sort(unique(domain$time_since_quit)), 0:10)) {
  stop("time_since_quit is no longer 0:10. prep_relapse and the model expect that.")
}
if(!identical(sort(unique(domain$age)), 1:89)) stop("age is no longer 1:89.")

# The profile over time since quit has to fall. This is what the pooling was for.
prof <- domain[ , .(p = mean(p_relapse)), by = time_since_quit][order(time_since_quit)]
if(any(diff(prof$p) > 1e-12)) {
  stop("Relapse still rises with time since quit at: ",
       paste(prof$time_since_quit[which(diff(prof$p) > 1e-12) + 1], collapse = ", "),
       ". Pooling years 6-9 was supposed to remove that.")
}
if(uniqueN(round(prof[time_since_quit %in% 6:9]$p, 10)) != 1) {
  stop("Years 6-9 are not pooled: they still hold different rates.")
}

# The table now has to reproduce the paper for the paper's own cohort. This is
# the check the old version could not pass.
ref_1yr <- domain[time_since_quit == 1 & age == AGE_CENTRE & sex == "Female" &
                    degree == "no_degree" & relationship_status == "single" &
                    employ2cat == "employed" & hse_mental == "no_mental" &
                    income5cat == "1_lowest_income" &
                    imd_quintile == "1_least_deprived"]$p_relapse
rebuilt <- {o <- (ref_1yr / (1 - ref_1yr)) * cohort$m; sum(cohort$wt * o / (1 + o))}
if(abs(rebuilt - target_1yr) > 1e-6) {
  stop("The finished table gives the Hawkins cohort ", round(100 * rebuilt, 4),
       "% at one year, not the ", round(100 * target_1yr, 1), "% the paper reports. ",
       "The calibration has not carried through into the lookup table.")
}
message(sprintf("Check: the finished table gives the Hawkins cohort %.4f%% at one year (Table 2: %.1f%%).",
                100 * rebuilt, 100 * target_1yr))

print(prof)

# Save the result to the package data folder
hawkins_relapse <- copy(domain)
usethis::use_data(hawkins_relapse, overwrite = TRUE)
