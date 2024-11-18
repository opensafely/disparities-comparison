library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_covid"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2021-09-01"
  study_end_date <- "2022-08-31"
  cohort <- "older_adults"
  codelist_type <- "sensitive"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

if (cohort != "infants" | cohort != "infants_subgroup") {
  
  #run model files
  source(here::here("analysis", "outcome_covid", "further_covid_ethnicity_models.R"))
  source(here::here("analysis", "outcome_covid", "further_covid_ses_models.R"))
  if (study_start_date == as.Date("2020-09-01")) {
    source(here::here("analysis", "outcome_covid", "further_covid_hh_comp_models.R"))
  }
  source(here::here("analysis", "outcome_covid", "further_covid_ethnicity_ses_models.R"))
  if (study_start_date == as.Date("2020-09-01")) {
    source(here::here("analysis", "outcome_covid", "further_covid_ethnicity_hh_comp_models.R"))
    source(here::here("analysis", "outcome_covid", "further_covid_ses_hh_comp_models.R"))
    source(here::here("analysis", "outcome_covid", "further_covid_full_models.R"))
  }
  
}
