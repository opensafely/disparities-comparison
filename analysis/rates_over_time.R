library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(gt)
library(readr)
library(stringr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants_subgroup"
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
covid_current_vacc_min <- as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min <- as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
                                      year(study_start_date), "_", year(study_end_date), "_",
                                      codelist_type, "_", investigation_type, ".arrow")))

#format data from wide to long 
if (study_start_date == as.Date("2020-09-01")) {
  df_long_date <- df_input %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           composition_category, rurality_classification, ends_with("_date") &
             contains(c("primary", "secondary", "mortality"))) %>%
    distinct() %>%
    pivot_longer(
      cols = ends_with("_date") & contains(c("primary", "secondary", "mortality")) &
        !contains(c("_second_", "_inf_")),
      names_to = "outcome_type",
      values_to = "outcome_date",
      values_drop_na = FALSE
    ) %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           composition_category, rurality_classification, outcome_type, outcome_date)
} else {
  df_long_date <- df_input %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           rurality_classification, ends_with("_date") &
             contains(c("primary", "secondary", "mortality"))) %>%
    distinct() %>%
    pivot_longer(
      cols = ends_with("_date") & contains(c("primary", "secondary", "mortality")) &
        !contains(c("_second_", "_inf_")),
      names_to = "outcome_type",
      values_to = "outcome_date",
      values_drop_na = FALSE
    ) %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           rurality_classification, outcome_type, outcome_date)
}

#add information for survival time 
if (study_start_date == as.Date("2020-09-01")) {
  df_long_surv <- df_input %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           composition_category, rurality_classification, starts_with("time_")) %>%
    distinct() %>%
    pivot_longer(
      cols = starts_with("time_") & !contains("time_since_last_covid_vaccination"),
      names_to = "outcome_type",
      values_to = "survival_time",
      values_drop_na = FALSE
    ) %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           composition_category, rurality_classification, outcome_type, survival_time)
} else {
  df_long_surv <- df_input %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           rurality_classification, starts_with("time_")) %>%
    distinct() %>%
    pivot_longer(
      cols = starts_with("time_") & !contains("time_since_last_covid_vaccination"),
      names_to = "outcome_type",
      values_to = "survival_time",
      values_drop_na = FALSE
    ) %>%
    select(patient_id, age_band, sex, latest_ethnicity_group, imd_quintile,
           rurality_classification, outcome_type, survival_time)
}

#create function to clean up outcome_type column 
clean_outcome_type <- function(df) {
  df %>%
    mutate(outcome_type = str_remove(outcome_type, "_date")) %>%
    mutate(outcome_type = str_remove(outcome_type, "time_")) 
}

df_long_date <- clean_outcome_type(df_long_date) 
df_long_surv <- clean_outcome_type(df_long_surv) 

#now join the survival time to df_long by patient id and outcome type
df_long <- df_long_date %>%
  left_join(df_long_surv %>% select(patient_id, outcome_type, survival_time), 
            by = c("patient_id", "outcome_type"))


