library(dplyr)
library(here)
library(arrow)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses", "outcome_covid"))

#define cohort
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cohort <- "infants"
  } else {
    cohort <- args[[1]]
  }
}

if (cohort != "infants" | cohort != "infants_subgroup") {
  
  #run model files
  source(here::here("analysis", "overall_analyses", "outcome_covid",
                    "covid_ethnicity_models.R"))
  source(here::here("analysis", "overall_analyses", "outcome_covid",
                    "covid_ses_models.R"))
  source(here::here("analysis", "overall_analyses", "outcome_covid",
                    "covid_ethnicity_ses_models.R"))
  
}
