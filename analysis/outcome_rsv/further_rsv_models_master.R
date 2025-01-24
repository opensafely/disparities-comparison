library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_rsv"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2020-09-01"
  study_end_date <- "2021-08-31"
  cohort <- "adults"
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

if (cohort != "infants" | cohort != "infants_subgroup") {
  
  #run model files
  source(here::here("analysis", "outcome_rsv", "further_rsv_ethnicity_models.R"))
  source(here::here("analysis", "outcome_rsv", "further_rsv_ses_models.R"))
  if (study_start_date == as.Date("2020-09-01")) {
    source(here::here("analysis", "outcome_rsv", "further_rsv_hh_comp_models.R"))
  }
  source(here::here("analysis", "outcome_rsv", "further_rsv_ethnicity_ses_models.R"))
  if (study_start_date == as.Date("2020-09-01")) {
    source(here::here("analysis", "outcome_rsv", "further_rsv_ethnicity_hh_comp_models.R"))
    source(here::here("analysis", "outcome_rsv", "further_rsv_ses_hh_comp_models.R"))
    source(here::here("analysis", "outcome_rsv", "further_rsv_full_models.R"))
  }
  
}
  