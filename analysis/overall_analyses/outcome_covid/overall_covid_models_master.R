library(dplyr)
library(here)
library(arrow)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses", "outcome_covid"))

#define cohort
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

investigation_type <- "primary"

if (cohort != "infants" | cohort != "infants_subgroup") {
  
  #run model files
  source(here::here("analysis", "overall_analyses", "outcome_covid",
                    "covid_ethnicity_models.R"))
  source(here::here("analysis", "overall_analyses", "outcome_covid",
                    "covid_ses_models.R"))
  source(here::here("analysis", "overall_analyses", "outcome_covid",
                    "covid_ethnicity_ses_models.R"))
  
}
