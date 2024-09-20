library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis", "overall_and_all_cause_specific"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants"
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
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_ethnicity_models.R"))
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_ses_models.R"))
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_hh_comp_models.R"))
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_ethnicity_ses_models.R"))
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_ethnicity_hh_comp_models.R"))
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_ses_hh_comp_models.R"))
  source(here("analysis", "overall_and_all_cause_specific", "overall_and_all_cause_full_models.R"))
}
  