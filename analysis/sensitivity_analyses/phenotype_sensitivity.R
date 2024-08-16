library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2019-09-01"
  study_end_date <- "2020-08-31"
  cohort <- "infants"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input_specific <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             "specific", "_", "primary",".arrow")))

df_input_sensitive <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             "sensitive", "_", "primary",".arrow")))

##count number of patients in cohort which fall into specific categories 

#create text coding for each category
if (study_start_date >= covid_season_min) {
  df_input_specific <- df_input_specific %>%
    mutate(
      rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild", "0"),
      rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe", "0"),
      flu_mild_alt = if_else(flu_primary_inf == 1, "Flu_mild", "0"),
      flu_severe_alt = if_else(flu_secondary_inf == 1, "Flu_Severe", "0"),
      covid_mild_alt = if_else(covid_primary_inf == 1, "COVID_Mild", "0"),
      covid_severe_alt = if_else(covid_secondary_inf == 1, "COVID_Severe", "0")
    )
} else {
  df_input_specific <- df_input_specific %>%
    mutate(
      rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild", "0"),
      rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe", "0"),
      flu_mild_alt = if_else(flu_primary_inf == 1, "Flu_mild", "0"),
      flu_severe_alt = if_else(flu_secondary_inf == 1, "Flu_Severe", "0")
    )
}

#combine text coding for mild
if (study_start_date >= covid_season_min) {
  df_input_specific$mild_combined = paste0(
    df_input_specific$rsv_mild_alt, "_",
    df_input_specific$flu_mild_alt, "_",
    df_input_specific$covid_mild_alt
  )
} else {
  df_input_specific$mild_combined = paste0(
    df_input_specific$rsv_mild_alt, "_",
    df_input_specific$flu_mild_alt
  )
}

#combine text coding for severe
if (study_start_date >= covid_season_min) {
  df_input_specific$severe_combined = paste0(
    df_input_specific$rsv_severe_alt, "_",
    df_input_specific$flu_severe_alt, "_",
    df_input_specific$covid_severe_alt
  )
} else {
  df_input_specific$severe_combined = paste0(
    df_input_specific$rsv_severe_alt, "_",
    df_input_specific$flu_severe_alt
  )
}

#create text coding for each category
if (study_start_date >= covid_season_min) {
  df_input_sensitive <- df_input_sensitive %>%
    mutate(
      rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild", "0"),
      rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe", "0"),
      flu_mild_alt = if_else(flu_primary_inf == 1, "Flu_mild", "0"),
      flu_severe_alt = if_else(flu_secondary_inf == 1, "Flu_Severe", "0"),
      covid_mild_alt = if_else(covid_primary_inf == 1, "COVID_Mild", "0"),
      covid_severe_alt = if_else(covid_secondary_inf == 1, "COVID_Severe", "0"),
      overall_resp_mild_alt = if_else(overall_resp_primary_inf == 1, "Overall_Resp_Mild", "0"),
      overall_resp_severe_alt = if_else(overall_resp_secondary_inf == 1, "Overall_Resp_Severe", "0")
    )
} else {
  df_input_sensitive <- df_input_sensitive %>%
    mutate(
      rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild", "0"),
      rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe", "0"),
      flu_mild_alt = if_else(flu_primary_inf == 1, "Flu_mild", "0"),
      flu_severe_alt = if_else(flu_secondary_inf == 1, "Flu_Severe", "0"),
      overall_resp_mild_alt = if_else(overall_resp_primary_inf == 1, "Overall_Resp_Mild", "0"),
      overall_resp_severe_alt = if_else(overall_resp_secondary_inf == 1, "Overall_Resp_Severe", "0")
    )
}

#combine text coding for mild
if (study_start_date >= covid_season_min) {
  df_input_sensitive$mild_combined = paste0(
    df_input_sensitive$rsv_mild_alt, "_",
    df_input_sensitive$flu_mild_alt, "_",
    df_input_sensitive$covid_mild_alt
  )
} else {
  df_input_sensitive$mild_combined = paste0(
    df_input_sensitive$rsv_mild_alt, "_",
    df_input_sensitive$flu_mild_alt
  )
}

#combine text coding for severe
if (study_start_date >= covid_season_min) {
  df_input_sensitive$severe_combined = paste0(
    df_input_sensitive$rsv_severe_alt, "_",
    df_input_sensitive$flu_severe_alt, "_",
    df_input_sensitive$covid_severe_alt
  )
} else {
  df_input_sensitive$severe_combined = paste0(
    df_input_sensitive$rsv_severe_alt, "_",
    df_input_sensitive$flu_severe_alt
  )
}

#for comparisons with overall resp outcome mild
if (study_start_date >= covid_season_min) {
  df_input_sensitive$mild_combined_overall = paste0(
    df_input_sensitive$rsv_mild_alt, "_",
    df_input_sensitive$flu_mild_alt, "_",
    df_input_sensitive$covid_mild_alt, "_",
    df_input_sensitive$overall_resp_mild_alt
  )
} else {
  df_input_sensitive$mild_combined_overall = paste0(
    df_input_sensitive$rsv_mild_alt, "_",
    df_input_sensitive$flu_mild_alt, "_",
    df_input_sensitive$overall_resp_mild_alt
  )
}

#for comparisons with overall resp outcome severe
if (study_start_date >= covid_season_min) {
  df_input_sensitive$severe_combined_overall = paste0(
    df_input_sensitive$rsv_severe_alt, "_",
    df_input_sensitive$flu_severe_alt, "_",
    df_input_sensitive$covid_severe_alt, "_",
    df_input_sensitive$overall_resp_severe_alt
  )
} else {
  df_input_sensitive$severe_combined_overall = paste0(
    df_input_sensitive$rsv_severe_alt, "_",
    df_input_sensitive$flu_severe_alt, "_",
    df_input_sensitive$overall_resp_severe_alt
  )
}

#count number of patients in each category for specific phenotypes - separately for mild and severe 
patients_specific_mild <- rlang::duplicate(df_input_specific) %>% 
  group_by("combo" = mild_combined) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild", codelist_type = "specific")
patients_specific_severe <- rlang::duplicate(df_input_specific) %>%
  group_by("combo" = severe_combined) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe", codelist_type = "specific")
patients_specific <- full_join(patients_specific_mild, patients_specific_severe)

#count number of patients in each category for sensitive phenotypes - separately for mild and severe
patients_sensitive_mild <- rlang::duplicate(df_input_sensitive) %>% 
  group_by("combo" = mild_combined_overall) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild", codelist_type = "sensitive")
patients_sensitive_severe <- rlang::duplicate(df_input_sensitive) %>%
  group_by("combo" = severe_combined_overall) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe", codelist_type = "sensitive")
patients_sensitive <- full_join(patients_sensitive_mild, patients_sensitive_severe)

#count number of patients in each category for sensitive phenotypes now including overall resp - separately for mild and severe
patients_sensitive_mild_overall <- rlang::duplicate(df_input_sensitive) %>% 
  group_by("combo" = mild_combined_overall) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild_overall", codelist_type = "sensitive")
patients_sensitive_severe_overall <- rlang::duplicate(df_input_sensitive) %>%
  group_by("combo" = severe_combined_overall) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe_overall", codelist_type = "sensitive")
patients_sensitive_overall <- full_join(patients_sensitive_mild_overall, 
                                        patients_sensitive_severe_overall)

#combine as one tibble 
patients_combined <- patients_specific %>%
  full_join(patients_sensitive) %>%
  full_join(patients_sensitive_overall)

## create output directories ----
fs::dir_create(here("output", "sensitivity"))

#write to file
write_csv(patients_combined, paste0(here::here("output", "sensitivity"),
          "/", "phenotype_sensitivity_", cohort, "_", year(study_start_date), "_", 
          year(study_end_date), ".csv"))
