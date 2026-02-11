# Workflow: Smoking state transition probability estimation

This vignettes builds on our [methodology
report](https://stapm.gitlab.io/projects/smoking-model-development/stpm_tech_report/stpm-technical-report.pdf)
by explaining how to use the functions within the `smktrans` R package
to produce estimates of the probabilities of smoking initiation and quit
in England using data from the Health Survey for England.

## Smoking initiation

For each cohort, we first estimate the age pattern of increase in the
proportion of ever-smokers. This is done by the function
[`smktrans::init_est()`](https://stapm-platform.github.io/smktrans/reference/init_est.md) -
you can see its help file by typing
[`?init_est`](https://stapm-platform.github.io/smktrans/reference/init_est.md).
The function takes the self-reported age that cigarettes were first
smoked and constructs a vector of 0s, 1s and NAs corresponding to each
age for each individual (1 = initiated smoking, NA = censored).

We constructed the variable for the age that cigarettes were first
smoked from the HSE using the `hseclean` R package (see
[`hseclean::smk_life_history()`](https://rdrr.io/pkg/hseclean/man/smk_life_history.html)).

The
[`smktrans::init_est()`](https://stapm-platform.github.io/smktrans/reference/init_est.md)
function first calculates the cohort and strata specific mean of the 0s
and 1s - which gives an estimate of the probability that someone from
that cohort and strata initiated smoking at that age. The function then
calculates the cumulative probability of starting to smoke by age.

The HSE data that the function is applied to should be cleaned in
advance of using the `smktrans`. The cumulative probabilities of
ever-smoking are then estimated by

``` r
# Calculate the cumulative probabilities of starting to smoke for each cohort
init_data_raw <- smktrans::init_est(
  data = hse_data,
  strat_vars = c("sex", "imd_quintile")
)
```

### Adjustment

The main problem with these estimates is that they don’t correspond to
the proportions of ever-smokers ascertained from asking respondents
directly if they had ever smoked cigarettes regularly (this could be due
to a range of factors e.g. recall bias or selective mortality).
Developing the method used by Holford et al.
([2014](#ref-holford2014patterns)), we estimated and applied an
adjustment factor to make these estimates correspond.

The first step in estimation of the adjustment factor is to estimate the
proportions of ever-smokers at a certain index age from the question
that asks directly about ever-smoking. This is done by the function
[`smktrans::ever_smoke()`](https://stapm-platform.github.io/smktrans/reference/ever_smoke.md).
This function uses
[`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html) to
estimate the expected proportions of ever-smokers accounting for survey
weights and the survey sampling design. We filter the data to only ages
25-34 years, and stratify the summary by cohort, sex and IMD quintile.
The function then fits a survey weighted linear regression
(quasibinomial) through the trends, which allows extrapolation to future
cohorts.

``` r
# Estimate the trend in the proportion of people who have ever smoked
# in the age range 25-34
ever_smoke_data <- smktrans::ever_smoke(
  data = hse_data,
  time_horizon = 2200,
  num_bins = 7,
  model = "England"
)
```

The next step is to estimate the adjustment factor and apply the
adjustment. This is done by the function
[`smktrans::init_adj()`](https://stapm-platform.github.io/smktrans/reference/init_adj.md).
This function assigns a reference age to each cohort (default is 30
years). The adjustment factor is then calculated for each cohort and
strata and applied to make the adjustment.

``` r
# Adjust and forecast
init_data_adj <- init_adj(
  init_data = copy(init_data_raw),
  ever_smoke_data = copy(ever_smoke_data$predicted_values),
  ref_age = 30,
  cohorts = 1971:2100,
  period_start = 2001, period_end = 2016)
```

Finally, we use the function
[`smktrans::p_dense()`](https://stapm-platform.github.io/smktrans/reference/p_dense.md)
to convert the probabilities of ever-smoking (the cumulative density
function) into the probabilities of initiating smoking at each age.

``` r
smk_init_data <- p_dense(data = copy(init_data_adj), cum_func_var = "p_ever_smoker_adj",
                     strat_vars = c("cohort", "sex", "imd_quintile"))
```

For our modelling of smoking dynamics, we want initiation probabilities
for ages up to 30 years (after which we assume no new initiation) for
each period modelled from 2001 onwards.

## Relapse to smoking

We consider the probability of relapse to smoking by former smokers from
1 to 10 years since quitting, which we call long-term relapse. To
estimate these probabilities, we combine published estimates of
long-term relapse from Hawkins et al. ([2010](#ref-hawkins2010long))
with the Health Survey for England data. We do this in the function
[`smktrans::prep_relapse()`](https://stapm-platform.github.io/smktrans/reference/prep_relapse.md).

Hawkin’s statistical analysis adjusts for several covariates. We only
adjust for a subset of these covariates as detailed below:

- age - a continuous function  
- sex  
- time since quit - 1, 2, 3, 4, 5, 6+ years  
- degree or not  
- employed or not  
- married, cohabiting or neither  
- mental health condition or not  
- income - we mapped Hawkins income quartiles to equivalised income
  quintiles from the HSE by pooling the lowest two income quintiles.

Note that Hawkins found that physical health / GP visits also explained
statistically significant variation in smoking relapse but we were not
able to adjust for them due to a lack of corresponding variables in the
Health Survey for England.

The first step is to assign each former smoker in the HSE an expected
probability of relapse based on the matching on the time since quit and
the covariates above. The second step is to calculate weighted averages
of the relapse probability for subgroups defined by either age, sex and
IMD quintile (to feed into our estimation of quit probabilities) or age,
sex, IMD quintile and time since quit (to feed into our simulation of
individual smoking histories).

``` r
# Combine published estimates of long-term relapse with the Health Survey for England data to arrive at the expected values for relapse probabilities within defined subgroups.
relapse_data <- smktrans::prep_relapse(
  data = hse_data,
  hawkins_relapse = smktrans::hawkins_relapse,
  lowest_year = 2001,
  highest_year = 2016,
  youngest_age = 11
)
```

## Quitting smoking

The age-specific probabilities that a smoker will quit smoking are
complex to estimate due to an number of potential biases. The formula we
derived (see accompanying article) uses the probabilities of initiation
and relapse. It also uses the following inputs:

### Proportions of current, former and never smokers

We statistically smooth the HSE trends over age and year in the
proportions of current, former and never smokers, using the function
[`smktrans::trend_fit()`](https://stapm-platform.github.io/smktrans/reference/trend_fit.md).
This fits a multinomial linear model to the distribution of people among
current, former and never smoking status, and in this model estimates
the variation in trends by sex and socioeconomic conditions in terms of
quintiles of the Index of Multiple Deprivation. The model has a logit
link function and accounts for the HSE survey weights. Age and year were
converted to z-scores (subtracting their mean and dividing by twice
their standard deviation) before model fitting.

The explanatory variables for age $`a`$ and year $`y`$ are specified in
the form of a cubic response surface, which has the form
$`a+a^2+y+y^2+ay + a^3 + y^3+a^2y+ay^2`$. We added an additional 4th
order term for age $`a^4`$ to better capture the non-linear patterns
over age e.g. as individuals initiate and then quit with age.

Interactions of this surface with sex and IMD quintiles are limited to
3rd order terms. Interactions with the quadratic term are included for
age but not for year to avoid over-fitting variation in the trend over
years, which is more of a linear change than the pattern over age. The
decision to do so was made after visually exploring the fit of simpler
and more complex model specifications to the data.

``` r
# model trends in current, former and never smoking
trend_data <- smktrans::trend_fit(hse_data,
  max_iterations = 1e3,
  age_var = "age",
  year_var = "year",
  sex_var = "sex",
  smoker_state_var = "smk.state",
  imd_var = "imd_quintile",
  weight_var = "wt_int")
```

### Cohort survivorship

We approximating the number of individuals alive at each age within each
cohort with the cohort survivorship function, which is the probability
that an individual will survive to the start of each age interval. We
get these data into the right format with the function
[`smktrans::prep_surv()`](https://stapm-platform.github.io/smktrans/reference/prep_surv.md).

The function
[`smktrans::prep_surv()`](https://stapm-platform.github.io/smktrans/reference/prep_surv.md)
calculates the cohort survivorship function over age, i.e. the
probability that someone from each cohort will survive to the start of
each 1 year age interval, stratified by sex and IMD quintile.

The function takes two data sources as inputs:

- Data from 1922 from the [Human Mortality
  Database](https://www.mortality.org/cgi-bin/hmd/country.php?cntr=GBRCENW&level=1)
  on the 1x1 age and period central death rates (mx) for the England &
  Wales civilian population. Death rates are stratified by sex only. We
  use the HMD data to estimate sex-specific survivorship curves from age
  12 onwards for each cohort that features in our 2001-2016 window.  
- Death rates from 2001-2016 for England by age, sex, year and IMD
  quintile. These are data that we have produced from the Office for
  National Statistics (ONS) mortality microdata (the processing of these
  data is done by the `mort.tools` R package and the data is stored on
  our virtual machine). We use the ONS data to inform the differences by
  IMD quintile in the age- and sex-specific probabilities of surviving
  through each 1 year age interval for years 2001-2016.

[`smktrans::prep_surv()`](https://stapm-platform.github.io/smktrans/reference/prep_surv.md)
calculates survivorship curves that are only partially stratified by IMD
quintile (because we do not use data on IMD quintile differences in
survival before 2001). This partial stratification is sufficient for our
calculation because the formula uses the ratios of people alive at age
$`a+1`$ compared to age $`a`$, i.e. the important thing is that we have
reliably characterised the shapes of the survivorship functions from
2001 to 2016 for each cohort, sex and IMD quintile.

``` r
# Estimate the shape of the cohort survivorship functions
survivorship_data <- smktrans::prep_surv(
  mx_data_hmd = smktrans::hmd_data,
  mx_data_ons = smktrans::tob_mort_data
)
```

### Survival by smoking status

We estimated the age-specific probabilities of surviving from age $`a`$
to age $`a+1`$, stratified by sex, IMD quintile and smoking status
(current, former, never regular cigarette smoker). These estimates are
used to adjust the probability of quitting smoking for the mortality
differentials by smoking status. The estimation of survival by smoking
status is done in the function
[`smktrans::smoke_surv()`](https://stapm-platform.github.io/smktrans/reference/smoke_surv.md).

We do not use individually-linked data that has smoking status linked to
time of death. Instead, we infer the mortality differentials by smoking
status based on the relative differences in risks for smoking-related
diseases among current, former and never smokers. To make these
calculations,
[`smktrans::smoke_surv()`](https://stapm-platform.github.io/smktrans/reference/smoke_surv.md)
calls the functions
[`tobalcepi::RRFunc()`](https://rdrr.io/pkg/tobalcepi/man/RRFunc.html)
and
[`stapmr::SurvFunc()`](https://rdrr.io/pkg/stapmr/man/SurvFunc.html).
Data on the relative risks of disease by smoking status and the rates of
death for each smoking-related disease are also taken from the data
stored in the other packages.

[`tobalcepi::RRFunc()`](https://rdrr.io/pkg/tobalcepi/man/RRFunc.html)
assigns the relative risk for each disease based on whether an
individual is a current, former or never smoker. All current smokers
have the same relative risk regardless of the amount they currently
smoke or have smoked in the past. Former smokers are initially given the
relative risk associated with current smokers, which we then scale
according to a disease-specific function that describes how risk
declines after quitting smoking.

[`stapmr::SurvFunc()`](https://rdrr.io/pkg/stapmr/man/SurvFunc.html)
computes the individual probability of death based on each individual’s
relative risks of disease and the mortality rates for each disease for
each sex and IMD quintile subgroup. The result is individual specific
mortality rates that average to the subgroup rate. The mortality rates
are then converted to the probability of death during the one-year age
interval exponentially during the age-interval. Probabilities of death
are summed across causes to give an overall probability of death.

[`smktrans::smoke_surv()`](https://stapm-platform.github.io/smktrans/reference/smoke_surv.md)
takes these estimated probabilities of death and fits a smooth line
through the age pattern for each year, sex, IMD quintile and smoking
status. The values of this smoothed fit are converted to the
probabilities of surviving from age $`a`$ to age $`a+1`$, stratified by
sex, IMD quintile and smoking status.

``` r
# Estimate age-specific probabilities of death by smoking status
mortality_data <- smktrans::smoke_surv(
  data = hse_data,
  diseases  = unique(tobalcepi::tobacco_relative_risks$condition),
  mx_data = stapmr::tob_mort_data_cause
)
```

### Quit probability calculation

Quit probabilities are calculated by the function
[`smktrans::quit_est()`](https://stapm-platform.github.io/smktrans/reference/quit_est.md),
which applies the equation we derived (see article) using the parameter
estimates above. The result is probabilities of quitting smoking
stratified by single years of age (11-88) and period (2001-2015), sex
and IMD quintile.

``` r
# Calculate quit probabilities
quit_data <- quit_est(
  dataq = hse_data,
  trend_dataq = trend_data,
  survivorship_dataq = survivorship_data,
  mortality_dataq = mortality_data$data_for_quit_ests,
  relapse_dataq = relapse_data$relapse_by_age_imd,
  initiation_dataq = smk_init_data,
  lowest_yearq = 2001,
  highest_yearq = 2016,
  youngest_yearq = 11
)
```

### Quit probability smoothing and forecast

The function
[`smktrans::quit_forecast()`](https://stapm-platform.github.io/smktrans/reference/quit_forecast.md)
is applied separately to each sex and IMD quintile. It first smooths the
pattern in the quit probabilities (a 2-dimensional moving average, where
the values considered form a 7-year-square over the single years of age
and period). As well as smoothing the trends, this also acts to fill-in
missing values based on the information in their neighbouring values by
age and period. We fill-in any remaining missing values by approximating
the age-specific trend over periods.

Second, the function forecasts the trend in the quit probabilities based
on an singular value decomposition (of logit transformed probabilities).
This produces a single vector to describe the trend over period, which
we project forwards based on the fit of a linear model to data from a
selected year onwards (`first_year`). The decomposition also estimates
the age-emphasis of this change i.e. whether the change is relatively
fasters at young or old ages - we fit a linear slope through this
age-pattern of change. The forecast is based on recombining these two
components to predict the trend in future years.

[`smktrans::quit_forecast()`](https://stapm-platform.github.io/smktrans/reference/quit_forecast.md)
has options to adjust the reference years and time horizon. It also has
an option to specify the future year at which the continuing trend in
quit probabilities switches from continuing to stationary (in case you
want to use this function to play with the forecast scenario, but this
might be more easily done outside of the function).

``` r
forecast_data <- quit_forecast(
  data = copy(quit_data),
  forecast_var = "p_quit",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = 2030, # the year at which the forecast becomes stationary
  first_year = 2007, # the earliest year of data on which the forecast is based
  jump_off_year = 2015,
  time_horizon = 2030
)
```

## References

Hawkins, James, William Hollingworth, and Rona Campbell. 2010.
“Long-Term Smoking Relapse: A Study Using the British Household Panel
Survey.” *Nicotine & Tobacco Research* 12 (12): 1228–35.
<https://doi.org/10.1093/ntr/ntq175>.

Holford, Theodore R, David T Levy, Lisa A McKay, Lauren Clarke, Ben
Racine, Rafael Meza, Stephanie Land, Jihyoun Jeon, and Eric J Feuer.
2014. “Patterns of Birth Cohort–Specific Smoking Histories, 1965–2009.”
*American Journal of Preventive Medicine* 46 (2): e31–37.
<https://doi.org/10.1016/j.amepre.2013.10.022>.
