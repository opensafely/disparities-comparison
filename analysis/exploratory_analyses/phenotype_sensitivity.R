library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here::here("analysis", "exploratory_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2020-09-01"
  study_end_date <- "2021-08-31"
  cohort <- "older_adults"
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

#select necessary columns
if (study_start_date >= covid_season_min) {
  df_input_specific <- df_input_specific %>% 
    select(
      patient_id, 
      rsv_primary_inf, 
      flu_primary_inf, 
      covid_primary_inf,
      rsv_secondary_inf,
      flu_secondary_inf,
      covid_secondary_inf,
      rsv_primary_date, 
      flu_primary_date, 
      covid_primary_date,
      rsv_secondary_date,
      flu_secondary_date,
      covid_secondary_date
    )
} else {
  df_input_specific <- df_input_specific %>% 
    select(
      patient_id, 
      rsv_primary_inf, 
      flu_primary_inf, 
      rsv_secondary_inf,
      flu_secondary_inf,
      rsv_primary_date, 
      flu_primary_date,
      rsv_secondary_date,
      flu_secondary_date
    )
}

df_input_sensitive <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             "sensitive", "_", "primary",".arrow")))

#select necessary columns
if (study_start_date >= covid_season_min) {
  df_input_sensitive <- df_input_sensitive %>% 
    select(
      patient_id, 
      rsv_primary_inf, 
      flu_primary_inf, 
      covid_primary_inf, 
      overall_resp_primary_inf, 
      rsv_secondary_inf, 
      flu_secondary_inf, 
      covid_secondary_inf, 
      overall_resp_secondary_inf, 
      rsv_primary_date, 
      flu_primary_date, 
      covid_primary_date, 
      overall_resp_primary_date, 
      rsv_secondary_date, 
      flu_secondary_date, 
      covid_secondary_date, 
      overall_resp_secondary_date
    )
} else {
  df_input_sensitive <- df_input_sensitive %>% 
    select(
      patient_id, 
      rsv_primary_inf, 
      flu_primary_inf, 
      overall_resp_primary_inf, 
      rsv_secondary_inf, 
      flu_secondary_inf, 
      overall_resp_secondary_inf,
      rsv_primary_date, 
      flu_primary_date,
      overall_resp_primary_date,
      rsv_secondary_date, 
      flu_secondary_date,
      overall_resp_secondary_date
    )
}

##define a function to create a text coding for outcomes 
alt_label <- function(input, sensitivity, study_start_date, covid_season_min) {
  
  if (sensitivity == "sensitive_overall") {
    
    if (study_start_date >= covid_season_min) {
      
      input <- input %>%
        mutate(
          rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0_0_0", "0_0_0_0"),
          flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild_0_0", "0_0_0_0"),
          covid_mild_alt = if_else(covid_primary_inf == 1, "0_0_COVID_Mild_0", "0_0_0_0"),
          overall_resp_mild_alt = if_else(overall_resp_primary_inf == 1, "0_0_0_Overall_Resp_Mild", "0_0_0_0"),
          rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0_0_0", "0_0_0_0"),
          flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe_0_0", "0_0_0_0"),
          covid_severe_alt = if_else(covid_secondary_inf == 1, "0_0_COVID_Severe_0", "0_0_0_0"),
          overall_resp_severe_alt = if_else(overall_resp_secondary_inf == 1, "0_0_0_Overall_Resp_Severe", "0_0_0_0"),
          primary_rsv_flu_within_14 = abs(rsv_primary_date - flu_primary_date) <= 14,
          primary_rsv_covid_within_14 = abs(rsv_primary_date - covid_primary_date) <= 14,
          primary_flu_covid_within_14 = abs(flu_primary_date - covid_primary_date) <= 14,
          primary_rsv_overall_within_14 = abs(rsv_primary_date - overall_resp_primary_date) <= 14,
          primary_flu_overall_within_14 = abs(flu_primary_date - overall_resp_primary_date) <= 14,
          primary_covid_overall_within_14 = abs(covid_primary_date - overall_resp_primary_date) <= 14,
          primary_rsv_flu_covid_max = pmax(rsv_primary_date, flu_primary_date,
                                          covid_primary_date, na.rm = T),
          primary_rsv_flu_covid_min = pmin(rsv_primary_date, flu_primary_date,
                                          covid_primary_date, na.rm = T),
          primary_rsv_flu_covid_within_14 = (
            primary_rsv_flu_covid_max - primary_rsv_flu_covid_min) <= 14,
          primary_rsv_flu_overall_max = pmax(rsv_primary_date, flu_primary_date,
                                            overall_resp_primary_date, na.rm = T),
          primary_rsv_flu_overall_min = pmin(rsv_primary_date, flu_primary_date,
                                            overall_resp_primary_date, na.rm = T),
          primary_rsv_flu_overall_within_14 =  (
            primary_rsv_flu_overall_max - primary_rsv_flu_overall_min) <= 14,
          primary_rsv_covid_overall_max = pmax(rsv_primary_date, covid_primary_date,
                                              overall_resp_primary_date, na.rm = T),
          primary_rsv_covid_overall_min = pmin(rsv_primary_date, covid_primary_date,
                                              overall_resp_primary_date, na.rm = T),
          primary_rsv_covid_overall_within_14 =  (
            primary_rsv_covid_overall_max - primary_rsv_covid_overall_min) <= 14,
          primary_flu_covid_overall_max = pmax(flu_primary_date, covid_primary_date,
                                              overall_resp_primary_date, na.rm = T),
          primary_flu_covid_overall_min = pmin(flu_primary_date, covid_primary_date,
                                              overall_resp_primary_date, na.rm = T),
          primary_flu_covid_overall_within_14 =  (
            primary_flu_covid_overall_max - primary_flu_covid_overall_min) <= 14,
          primary_rsv_flu_overall_max = pmax(rsv_primary_date, flu_primary_date,
                                            covid_primary_date,
                                            overall_resp_primary_date, na.rm = T),
          primary_rsv_flu_overall_min = pmin(rsv_primary_date, flu_primary_date,
                                            covid_primary_date,
                                            overall_resp_primary_date, na.rm = T),
          primary_rsv_flu_covid_overall_within_14 =  (
            primary_rsv_flu_overall_max - primary_rsv_flu_overall_min) <= 14,
          secondary_rsv_flu_within_14 = abs(rsv_secondary_date - flu_secondary_date) <= 14,
          secondary_rsv_covid_within_14 = abs(rsv_secondary_date - covid_secondary_date) <= 14,
          secondary_flu_covid_within_14 = abs(flu_secondary_date - covid_secondary_date) <= 14,
          secondary_rsv_overall_within_14 = abs(rsv_secondary_date - overall_resp_secondary_date) <= 14,
          secondary_flu_overall_within_14 = abs(flu_secondary_date - overall_resp_secondary_date) <= 14,
          secondary_covid_overall_within_14 = abs(covid_secondary_date - overall_resp_secondary_date) <= 14,
          secondary_rsv_flu_covid_max = pmax(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_min = pmin(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_within_14 = (
            secondary_rsv_flu_covid_max - secondary_rsv_flu_covid_min) <= 14,
          secondary_rsv_flu_overall_max = pmax(rsv_secondary_date, flu_secondary_date,
                                            overall_resp_secondary_date, na.rm = T),
          secondary_rsv_flu_overall_min = pmin(rsv_secondary_date, flu_secondary_date,
                                            overall_resp_secondary_date, na.rm = T),
          secondary_rsv_flu_overall_within_14 =  (
            secondary_rsv_flu_overall_max - secondary_rsv_flu_overall_min) <= 14,
          secondary_rsv_covid_overall_max = pmax(rsv_secondary_date, covid_secondary_date,
                                              overall_resp_secondary_date, na.rm = T),
          secondary_rsv_covid_overall_min = pmin(rsv_secondary_date, covid_secondary_date,
                                              overall_resp_secondary_date, na.rm = T),
          secondary_rsv_covid_overall_within_14 =  (
            secondary_rsv_covid_overall_max - secondary_rsv_covid_overall_min) <= 14,
          secondary_flu_covid_overall_max = pmax(flu_secondary_date, covid_secondary_date,
                                              overall_resp_secondary_date, na.rm = T),
          secondary_flu_covid_overall_min = pmin(flu_secondary_date, covid_secondary_date,
                                              overall_resp_secondary_date, na.rm = T),
          secondary_flu_covid_overall_within_14 =  (
            secondary_flu_covid_overall_max - secondary_flu_covid_overall_min) <= 14,
          secondary_rsv_flu_overall_max = pmax(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date,
                                            overall_resp_secondary_date, na.rm = T),
          secondary_rsv_flu_overall_min = pmin(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date,
                                            overall_resp_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_overall_within_14 =  (
            secondary_rsv_flu_overall_max - secondary_rsv_flu_overall_min) <= 14,
          across(contains("_within_14"), ~if_else(is.na(.x), FALSE, .x))
        ) %>%
        mutate(
          rsv_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0_0",
            primary_rsv_covid_within_14 ~ "RSV_Mild_0_COVID_Mild_0",
            primary_rsv_overall_within_14 ~ "RSV_Mild_0_0_Overall_Resp_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_0",
            primary_rsv_flu_overall_within_14 ~ "RSV_Mild_Flu_Mild_0_Overall_Resp_Mild",
            primary_rsv_covid_overall_within_14 ~ "RSV_Mild_0_COVID_Mild_Overall_Resp_Mild",
            primary_rsv_flu_covid_overall_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            TRUE ~ rsv_mild_alt
          ),
          flu_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0_0",
            primary_flu_covid_within_14 ~ "0_Flu_Mild_COVID_Mild_0",
            primary_flu_overall_within_14 ~ "0_Flu_Mild_0_Overall_Resp_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_0",
            primary_rsv_flu_overall_within_14 ~ "RSV_Mild_Flu_Mild_0_Overall_Resp_Mild",
            primary_flu_covid_overall_within_14 ~ "0_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            primary_rsv_flu_covid_overall_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            TRUE ~ flu_mild_alt
          ),
          covid_mild_alt_combo = case_when(
            primary_rsv_covid_within_14 ~ "RSV_Mild_0_COVID_Mild_0",
            primary_flu_covid_within_14 ~ "0_Flu_Mild_COVID_Mild_0",
            primary_covid_overall_within_14 ~ "0_0_COVID_Mild_Overall_Resp_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_0",
            primary_rsv_covid_overall_within_14 ~ "RSV_Mild_0_COVID_Mild_Overall_Resp_Mild",
            primary_flu_covid_overall_within_14 ~ "0_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            primary_rsv_flu_covid_overall_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            TRUE ~ covid_mild_alt
          ),
          overall_resp_mild_alt_combo = case_when(
            primary_rsv_overall_within_14 ~ "RSV_Mild_0_0_Overall_Resp_Mild",
            primary_flu_overall_within_14 ~ "0_Flu_Mild_0_Overall_Resp_Mild",
            primary_covid_overall_within_14 ~ "0_0_COVID_Mild_Overall_Resp_Mild",
            primary_rsv_flu_overall_within_14 ~ "RSV_Mild_Flu_Mild_0_Overall_Resp_Mild",
            primary_rsv_covid_overall_within_14 ~ "RSV_Mild_0_COVID_Mild_Overall_Resp_Mild",
            primary_flu_covid_overall_within_14 ~ "0_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            primary_rsv_flu_covid_overall_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild_Overall_Resp_Mild",
            TRUE ~ overall_resp_mild_alt
          ),
          rsv_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0_0",
            secondary_rsv_covid_within_14 ~ "RSV_Severe_0_COVID_Severe_0",
            secondary_rsv_overall_within_14 ~ "RSV_Severe_0_0_Overall_Resp_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_0",
            secondary_rsv_flu_overall_within_14 ~ "RSV_Severe_Flu_Severe_0_Overall_Resp_Severe",
            secondary_rsv_covid_overall_within_14 ~ "RSV_Severe_0_COVID_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_covid_overall_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            TRUE ~ rsv_severe_alt
          ),
          flu_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0_0",
            secondary_flu_covid_within_14 ~ "0_Flu_Severe_COVID_Severe_0",
            secondary_flu_overall_within_14 ~ "0_Flu_Severe_0_Overall_Resp_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_0",
            secondary_rsv_flu_overall_within_14 ~ "RSV_Severe_Flu_Severe_0_Overall_Resp_Severe",
            secondary_flu_covid_overall_within_14 ~ "0_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_covid_overall_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            TRUE ~ flu_severe_alt
          ),
          covid_severe_alt_combo = case_when(
            secondary_rsv_covid_within_14 ~ "RSV_Severe_0_COVID_Severe_0",
            secondary_flu_covid_within_14 ~ "0_Flu_Severe_COVID_Severe_0",
            secondary_covid_overall_within_14 ~ "0_0_COVID_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_0",
            secondary_rsv_covid_overall_within_14 ~ "RSV_Severe_0_COVID_Severe_Overall_Resp_Severe",
            secondary_flu_covid_overall_within_14 ~ "0_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_covid_overall_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            TRUE ~ covid_severe_alt
          ),
          overall_severe_alt_combo = case_when(
            secondary_rsv_overall_within_14 ~ "RSV_Severe_0_0_Overall_Resp_Severe",
            secondary_flu_overall_within_14 ~ "0_Flu_Severe_0_Overall_Resp_Severe",
            secondary_covid_overall_within_14 ~ "0_0_COVID_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_overall_within_14 ~ "RSV_Severe_Flu_Severe_0_Overall_Resp_Severe",
            secondary_rsv_covid_overall_within_14 ~ "RSV_Severe_0_COVID_Severe_Overall_Resp_Severe",
            secondary_flu_covid_overall_within_14 ~ "0_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_covid_overall_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe_Overall_Resp_Severe",
            TRUE ~ overall_resp_severe_alt
          )
        )
      
    } else {
      
      input <- input %>%
        mutate(
          rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0_0", "0_0_0"),
          flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild_0", "0_0_0"),
          overall_resp_mild_alt = if_else(overall_resp_primary_inf == 1, "0_0_Overall_Resp_Mild", "0_0_0"),
          rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0_0", "0_0_0"),
          flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe_0", "0_0_0"),
          overall_resp_severe_alt = if_else(overall_resp_secondary_inf == 1, "0_0_Overall_Resp_Severe", "0_0_0"),
          primary_rsv_flu_within_14 = abs(rsv_primary_date - flu_primary_date) <= 14,
          primary_rsv_overall_within_14 = abs(rsv_primary_date - overall_resp_primary_date) <= 14,
          primary_flu_overall_within_14 = abs(flu_primary_date - overall_resp_primary_date) <= 14,
          primary_rsv_flu_overall_max = pmax(rsv_primary_date, flu_primary_date,
                                            overall_resp_primary_date, na.rm = T),
          primary_rsv_flu_overall_min = pmin(rsv_primary_date, flu_primary_date,
                                            overall_resp_primary_date, na.rm = T),
          primary_rsv_flu_overall_within_14 = (
            primary_rsv_flu_overall_max - primary_rsv_flu_overall_min) <= 14,
          secondary_rsv_flu_within_14 = abs(rsv_secondary_date - flu_secondary_date) <= 14,
          secondary_rsv_overall_within_14 = abs(rsv_secondary_date - overall_resp_secondary_date) <= 14,
          secondary_flu_overall_within_14 = abs(flu_secondary_date - overall_resp_secondary_date) <= 14,
          secondary_rsv_flu_overall_max = pmax(rsv_secondary_date, flu_secondary_date,
                                            overall_resp_secondary_date, na.rm = T),
          secondary_rsv_flu_overall_min = pmin(rsv_secondary_date, flu_secondary_date,
                                            overall_resp_secondary_date, na.rm = T),
          secondary_rsv_flu_overall_within_14 = (
            secondary_rsv_flu_overall_max - secondary_rsv_flu_overall_min) <= 14,
          across(contains("_within_14"), ~if_else(is.na(.x), FALSE, .x))
        ) %>%
        mutate(
          rsv_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0",
            primary_rsv_overall_within_14 ~ "RSV_Mild_0_Overall_Resp_Mild",
            primary_rsv_flu_overall_within_14 ~ "RSV_Mild_Flu_Mild_Overall_Resp_Mild",
            TRUE ~ rsv_mild_alt
          ),
          flu_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0",
            primary_flu_overall_within_14 ~ "0_Flu_Mild_Overall_Resp_Mild",
            primary_rsv_flu_overall_within_14 ~ "RSV_Mild_Flu_Mild_Overall_Resp_Mild",
            TRUE ~ flu_mild_alt
          ),
          overall_resp_mild_alt_combo = case_when(
            primary_rsv_overall_within_14 ~ "RSV_Mild_0_Overall_Resp_Mild",
            primary_flu_overall_within_14 ~ "0_Flu_Mild_Overall_Resp_Mild",
            primary_rsv_flu_overall_within_14 ~ "RSV_Mild_Flu_Mild_Overall_Resp_Mild",
            TRUE ~ overall_resp_mild_alt
          ),
          rsv_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0",
            secondary_rsv_overall_within_14 ~ "RSV_Severe_0_Overall_Resp_Severe",
            secondary_rsv_flu_overall_within_14 ~ "RSV_Severe_Flu_Severe_Overall_Resp_Severe",
            TRUE ~ rsv_severe_alt
          ),
          flu_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0",
            secondary_flu_overall_within_14 ~ "0_Flu_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_overall_within_14 ~ "RSV_Severe_Flu_Severe_Overall_Resp_Severe",
            TRUE ~ flu_severe_alt
          ),
          overall_severe_alt_combo = case_when(
            secondary_rsv_overall_within_14 ~ "RSV_Severe_0_Overall_Resp_Severe",
            secondary_flu_overall_within_14 ~ "0_Flu_Severe_Overall_Resp_Severe",
            secondary_rsv_flu_overall_within_14 ~ "RSV_Severe_Flu_Severe_Overall_Resp_Severe",
            TRUE ~ overall_resp_severe_alt
          )
        )
      
    }
  } else if (sensitivity == "sensitive") {
    
    if (study_start_date >= covid_season_min) {
      
      input <- input %>%
        mutate(
          rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0_0", "0_0_0"),
          flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild_0", "0_0_0"),
          covid_mild_alt = if_else(covid_primary_inf == 1, "0_0_COVID_Mild", "0_0_0"),
          rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0_0", "0_0_0"),
          flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe_0", "0_0_0"),
          covid_severe_alt = if_else(covid_secondary_inf == 1, "0_0_COVID_Severe", "0_0_0"),
          primary_rsv_flu_within_14 = abs(rsv_primary_date - flu_primary_date) <= 14,
          primary_rsv_covid_within_14 = abs(rsv_primary_date - covid_primary_date) <= 14,
          primary_flu_covid_within_14 = abs(flu_primary_date - covid_primary_date) <= 14,
          primary_rsv_flu_covid_max = pmax(rsv_primary_date, flu_primary_date,
                                          covid_primary_date, na.rm = T),
          primary_rsv_flu_covid_min = pmin(rsv_primary_date, flu_primary_date,
                                          covid_primary_date, na.rm = T),
          primary_rsv_flu_covid_within_14 = (
            primary_rsv_flu_covid_max - primary_rsv_flu_covid_min) <= 14,
          secondary_rsv_flu_within_14 = abs(rsv_secondary_date - flu_secondary_date) <= 14,
          secondary_rsv_covid_within_14 = abs(rsv_secondary_date - covid_secondary_date) <= 14,
          secondary_flu_covid_within_14 = abs(flu_secondary_date - covid_secondary_date) <= 14,
          secondary_rsv_flu_covid_max = pmax(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_min = pmin(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_within_14 = (
            secondary_rsv_flu_covid_max - secondary_rsv_flu_covid_min) <= 14,
          across(contains("_within_14"), ~if_else(is.na(.x), FALSE, .x))
        ) %>%
        mutate(
          rsv_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0",
            primary_rsv_covid_within_14 ~ "RSV_Mild_0_COVID_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild",
            TRUE ~ rsv_mild_alt
          ),
          flu_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0",
            primary_flu_covid_within_14 ~ "0_Flu_Mild_COVID_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild",
            TRUE ~ flu_mild_alt
          ),
          covid_mild_alt_combo = case_when(
            primary_rsv_covid_within_14 ~ "RSV_Mild_0_COVID_Mild",
            primary_flu_covid_within_14 ~ "0_Flu_Mild_COVID_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild",
            TRUE ~ covid_mild_alt
          ),
          rsv_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0",
            secondary_rsv_covid_within_14 ~ "RSV_Severe_0_COVID_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe",
            TRUE ~ rsv_severe_alt
          ),
          flu_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0",
            secondary_flu_covid_within_14 ~ "0_Flu_Severe_COVID_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe",
            TRUE ~ flu_severe_alt
          ),
          covid_severe_alt_combo = case_when(
            secondary_rsv_covid_within_14 ~ "RSV_Severe_0_COVID_Severe",
            secondary_flu_covid_within_14 ~ "0_Flu_Severe_COVID_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe",
            TRUE ~ covid_severe_alt
          )
        )
      
    } else {
      
      input <- input %>%
        mutate(
          rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0", "0_0"),
          flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild", "0_0"),
          rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0", "0_0"),
          flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe", "0_0"),
          primary_rsv_flu_within_14 = abs(rsv_primary_date - flu_primary_date) <= 14,
          secondary_rsv_flu_within_14 = abs(rsv_secondary_date - flu_secondary_date) <= 14,
          across(contains("_within_14"), ~if_else(is.na(.x), FALSE, .x))
        ) %>%
        mutate(
          rsv_mild_alt_combo = if_else(
            primary_rsv_flu_within_14 == TRUE, "RSV_Mild_Flu_Mild",
            rsv_mild_alt
          ),
          flu_mild_alt_combo = if_else(
            primary_rsv_flu_within_14 == TRUE, "RSV_Mild_Flu_Mild",
            flu_mild_alt
          ),
          rsv_severe_alt_combo = if_else(
            secondary_rsv_flu_within_14 == TRUE, "RSV_Severe_Flu_Severe",
            rsv_severe_alt
          ),
          flu_severe_alt_combo = if_else(
            secondary_rsv_flu_within_14 == TRUE, "RSV_Severe_Flu_Severe",
            flu_severe_alt
          )
        )
      
    }
    
  } else {
    
    if (study_start_date >= covid_season_min) {
      
      input <- input %>%
        mutate(
          rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0_0", "0_0_0"),
          flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild_0", "0_0_0"),
          covid_mild_alt = if_else(covid_primary_inf == 1, "0_0_COVID_Mild", "0_0_0"),
          rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0_0", "0_0_0"),
          flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe_0", "0_0_0"),
          covid_severe_alt = if_else(covid_secondary_inf == 1, "0_0_COVID_Severe", "0_0_0"),
          primary_rsv_flu_within_14 = abs(rsv_primary_date - flu_primary_date) <= 14,
          primary_rsv_covid_within_14 = abs(rsv_primary_date - covid_primary_date) <= 14,
          primary_flu_covid_within_14 = abs(flu_primary_date - covid_primary_date) <= 14,
          primary_rsv_flu_covid_max = pmax(rsv_primary_date, flu_primary_date,
                                          covid_primary_date, na.rm = T),
          primary_rsv_flu_covid_min = pmin(rsv_primary_date, flu_primary_date,
                                          covid_primary_date, na.rm = T),
          primary_rsv_flu_covid_within_14 = (
            primary_rsv_flu_covid_max - primary_rsv_flu_covid_min) <= 14,
          secondary_rsv_flu_within_14 = abs(rsv_secondary_date - flu_secondary_date) <= 14,
          secondary_rsv_covid_within_14 = abs(rsv_secondary_date - covid_secondary_date) <= 14,
          secondary_flu_covid_within_14 = abs(flu_secondary_date - covid_secondary_date) <= 14,
          secondary_rsv_flu_covid_max = pmax(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_min = pmin(rsv_secondary_date, flu_secondary_date,
                                            covid_secondary_date, na.rm = T),
          secondary_rsv_flu_covid_within_14 = (
            secondary_rsv_flu_covid_max - secondary_rsv_flu_covid_min) <= 14,
          across(contains("_within_14"), ~if_else(is.na(.x), FALSE, .x))
          ) %>%
        mutate(
          rsv_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0",
            primary_rsv_covid_within_14 ~ "RSV_Mild_0_COVID_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild",
            TRUE ~ rsv_mild_alt
          ),
          flu_mild_alt_combo = case_when(
            primary_rsv_flu_within_14 ~ "RSV_Mild_Flu_Mild_0",
            primary_flu_covid_within_14 ~ "0_Flu_Mild_COVID_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild",
            TRUE ~ flu_mild_alt
          ),
          covid_mild_alt_combo = case_when(
            primary_rsv_covid_within_14 ~ "RSV_Mild_0_COVID_Mild",
            primary_flu_covid_within_14 ~ "0_Flu_Mild_COVID_Mild",
            primary_rsv_flu_covid_within_14 ~ "RSV_Mild_Flu_Mild_COVID_Mild",
            TRUE ~ covid_mild_alt
          ),
          rsv_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0",
            secondary_rsv_covid_within_14 ~ "RSV_Severe_0_COVID_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe",
            TRUE ~ rsv_severe_alt
          ),
          flu_severe_alt_combo = case_when(
            secondary_rsv_flu_within_14 ~ "RSV_Severe_Flu_Severe_0",
            secondary_flu_covid_within_14 ~ "0_Flu_Severe_COVID_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe",
            TRUE ~ flu_severe_alt
          ),
          covid_severe_alt_combo = case_when(
            secondary_rsv_covid_within_14 ~ "RSV_Severe_0_COVID_Severe",
            secondary_flu_covid_within_14 ~ "0_Flu_Severe_COVID_Severe",
            secondary_rsv_flu_covid_within_14 ~ "RSV_Severe_Flu_Severe_COVID_Severe",
            TRUE ~ covid_severe_alt
          )
        )
      
    } else {
      
      input <- input %>%
        rowwise() %>% 
        mutate(
          rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0", "0_0"),
          flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild", "0_0"),
          rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0", "0_0"),
          flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe", "0_0"),
          primary_rsv_flu_within_14 = abs(rsv_primary_date - flu_primary_date) <= 14,
          secondary_rsv_flu_within_14 = abs(rsv_secondary_date - flu_secondary_date) <= 14,
          across(contains("_within_14"), ~if_else(is.na(.x), FALSE, .x))
        ) %>%
        mutate(
          rsv_mild_alt_combo = if_else(primary_rsv_flu_within_14 == TRUE, 
                                       "RSV_Mild_Flu_Mild", rsv_mild_alt),
          flu_mild_alt_combo = if_else(primary_rsv_flu_within_14 == TRUE, 
                                       "RSV_Mild_Flu_Mild", flu_mild_alt),
          rsv_severe_alt_combo = if_else(secondary_rsv_flu_within_14 == TRUE, 
                                         "RSV_Severe_Flu_Severe", rsv_severe_alt),
          flu_severe_alt_combo = if_else(secondary_rsv_flu_within_14 == TRUE, 
                                         "RSV_Severe_Flu_Severe", flu_severe_alt)
        )
      
    }
    
  }
  
  return(input)
  
}

#apply function
df_input_specific <- alt_label(df_input_specific, "specific", study_start_date, covid_season_min)
df_input_sensitive <- alt_label(df_input_sensitive, "sensitive", study_start_date, covid_season_min)
df_input_sensitive_overall <- alt_label(df_input_sensitive, "sensitive_overall", study_start_date, covid_season_min)

#select necessary columns for a new dataframe to work on 
if (study_start_date >= covid_season_min) {
  df_spec <- df_input_specific %>%
    select(patient_id, rsv_mild_alt_combo, flu_mild_alt_combo, 
           covid_mild_alt_combo, rsv_severe_alt_combo, flu_severe_alt_combo,
           covid_severe_alt_combo)
  df_sens <- df_input_sensitive %>%
    select(patient_id, rsv_mild_alt_combo, flu_mild_alt_combo, 
           covid_mild_alt_combo, rsv_severe_alt_combo, 
           flu_severe_alt_combo, covid_severe_alt_combo) 
  df_sens_overall <- df_input_sensitive_overall %>%
    select(patient_id, rsv_mild_alt_combo, flu_mild_alt_combo, 
           covid_mild_alt_combo, overall_resp_mild_alt_combo,
           rsv_severe_alt_combo, flu_severe_alt_combo, 
           covid_severe_alt_combo, overall_severe_alt_combo)
} else {
  df_spec <- df_input_specific %>%
    select(patient_id, rsv_mild_alt_combo, flu_mild_alt_combo,
           rsv_severe_alt_combo, flu_severe_alt_combo)
  df_sens <- df_input_sensitive %>%
    select(patient_id, rsv_mild_alt_combo, flu_mild_alt_combo, 
           rsv_severe_alt_combo, flu_severe_alt_combo) 
  df_sens_overall <- df_input_sensitive_overall %>%
    select(patient_id, rsv_mild_alt_combo, flu_mild_alt_combo, 
           overall_resp_mild_alt_combo, rsv_severe_alt_combo,
           flu_severe_alt_combo, overall_severe_alt_combo)
}

#reformat from wide to long
if (study_start_date >= covid_season_min) {
  df_spec_mild <- df_spec %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo, covid_mild_alt_combo), 
                 names_to = "mild_combo", values_to = "combo") %>%
    mutate(codelist_type = "specific") %>%
    select(combo, codelist_type)
  df_spec_severe <- df_spec %>%
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo, covid_severe_alt_combo), 
                 names_to = "severe_combo", values_to = "combo") %>%
    mutate(codelist_type = "specific") %>%
    select(combo, codelist_type)
  df_sens_mild <- df_sens %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo, covid_mild_alt_combo), 
                 names_to = "mild_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
  df_sens_mild_overall <- df_sens_overall %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo, covid_mild_alt_combo, 
                   overall_resp_mild_alt_combo), 
                 names_to = "mild_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
  df_sens_severe <- df_sens %>%
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo, covid_severe_alt_combo), 
                 names_to = "severe_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
  df_sens_severe_overall <- df_sens_overall %>%
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo, covid_severe_alt_combo, 
                   overall_severe_alt_combo), 
                 names_to = "severe_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
} else {
  df_spec_mild <- df_spec %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo),
                 names_to = "mild_combo", values_to = "combo") %>%
    mutate(codelist_type = "specific") %>%
    select(combo, codelist_type)
  df_spec_severe <- df_spec %>% 
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo),
                 names_to = "severe_combo", values_to = "combo") %>%
    mutate(codelist_type = "specific") %>%
    select(combo, codelist_type)
  df_sens_mild <- df_sens %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo), 
                 names_to = "mild_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
  df_sens_mild_overall <- df_sens_overall %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo, 
                   overall_resp_mild_alt_combo), 
                 names_to = "mild_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
  df_sens_severe <- df_sens %>%
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo), 
                 names_to = "severe_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
  df_sens_severe_overall <- df_sens_overall %>%
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo, 
                   overall_severe_alt_combo), 
                 names_to = "severe_combo", values_to = "combo") %>%
    mutate(codelist_type = "sensitive") %>%
    select(combo, codelist_type)
}

##count number of patients in each category for phenotypes - separately for mild and severe 

#specific phenotype
patients_specific_mild <- df_spec_mild %>% 
  group_by(combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild", codelist_type = "specific")
patients_specific_severe <- df_spec_severe %>%
  group_by(combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe", codelist_type = "specific")
patients_specific <- full_join(patients_specific_mild, patients_specific_severe)

#sensitive phenotype
patients_sensitive_mild <- df_sens_mild %>% 
  group_by(combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild", codelist_type = "sensitive")
patients_sensitive_severe <- df_sens_severe %>%
  group_by(combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe", codelist_type = "sensitive")
patients_sensitive <- full_join(patients_sensitive_mild, patients_sensitive_severe)
patients_sensitive_mild_overall <- df_sens_mild_overall %>% 
  group_by(combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild_overall", codelist_type = "sensitive")
patients_sensitive_severe_overall <- df_sens_severe_overall %>%
  group_by(combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe_overall", codelist_type = "sensitive")
patients_sensitive_overall <- full_join(patients_sensitive_mild_overall, patients_sensitive_severe_overall)

#get totals for sets 
if (study_start_date >= covid_season_min) {
  
  patients_specific_totals <- tibble(
    combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
              "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
    n = c(sum(patients_specific_mild[grep("RSV", patients_specific_mild$combo), "n"]),
          sum(patients_specific_mild[grep("Flu", patients_specific_mild$combo), "n"]),
          sum(patients_specific_mild[grep("COVID", patients_specific_mild$combo), "n"]),
          sum(patients_specific_severe[grep("RSV", patients_specific_severe$combo), "n"]),
          sum(patients_specific_severe[grep("Flu", patients_specific_severe$combo), "n"]),
          sum(patients_specific_severe[grep("COVID", patients_specific_severe$combo), "n"])),
    outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe"),
    codelist_type = rep("specific", 6)
  )
  patients_specific <- rbind(patients_specific_totals, patients_specific)
  
  patients_sensitive_totals <- tibble(
    combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
              "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
    n = c(sum(patients_sensitive_mild[grep("RSV", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild[grep("Flu", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild[grep("COVID", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_severe[grep("RSV", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe[grep("Flu", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe[grep("COVID", patients_sensitive_severe$combo), "n"])),
    outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe"),
    codelist_type = rep("sensitive", 6)
  )
  patients_sensitive <- rbind(patients_sensitive_totals, patients_sensitive)
  
  patients_sensitive_totals_overall <- tibble(
    combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
              "Overall_Resp_Mild_Total", "RSV_Severe_Total", "Flu_Severe_Total", 
              "COVID_Severe_Total", "Overall_Resp_Severe_Total"),
    n = c(sum(patients_sensitive_mild[grep("RSV", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild[grep("Flu", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild[grep("COVID", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild_overall[grep("Overall", patients_sensitive_mild_overall$combo), "n"]),
          sum(patients_sensitive_severe[grep("RSV", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe[grep("Flu", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe[grep("COVID", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe_overall[grep("Overall", patients_sensitive_severe_overall$combo), "n"])),
    outcome_type = c(rep("mild_overall", 4), rep("severe_overall", 4)),
    codelist_type = rep("sensitive", 8)
  )
  patients_sensitive_overall <- rbind(patients_sensitive_totals_overall, 
                                      patients_sensitive_overall)
  
} else {
  
  patients_specific_totals <- tibble(
    combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
              "RSV_Severe_Total", "Flu_Severe_Total"),
    n = c(sum(patients_specific_mild[grep("RSV", patients_specific_mild$combo), "n"]),
          sum(patients_specific_mild[grep("Flu", patients_specific_mild$combo), "n"]),
          sum(patients_specific_severe[grep("RSV", patients_specific_severe$combo), "n"]),
          sum(patients_specific_severe[grep("Flu", patients_specific_severe$combo), "n"])),
    outcome_type = c("mild", "mild", "severe", "severe"),
    codelist_type = rep("specific", 4)
  )
  patients_specific <- rbind(patients_specific_totals, patients_specific)
  
  patients_sensitive_totals <- tibble(
    combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
              "RSV_Severe_Total", "Flu_Severe_Total"),
    n = c(sum(patients_sensitive_mild[grep("RSV", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild[grep("Flu", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_severe[grep("RSV", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe[grep("Flu", patients_sensitive_severe$combo), "n"])),
    outcome_type = c("mild", "mild", "severe", "severe"),
    codelist_type = rep("sensitive", 4)
  )
  patients_sensitive <- rbind(patients_sensitive_totals, patients_sensitive)
  
  patients_sensitive_totals_overall <- tibble(
    combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
              "Overall_Resp_Mild_Total", "RSV_Severe_Total", "Flu_Severe_Total", 
              "Overall_Resp_Severe_Total"),
    n = c(sum(patients_sensitive_mild[grep("RSV", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild[grep("Flu", patients_sensitive_mild$combo), "n"]),
          sum(patients_sensitive_mild_overall[grep("Overall", patients_sensitive_mild_overall$combo), "n"]),
          sum(patients_sensitive_severe[grep("RSV", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe[grep("Flu", patients_sensitive_severe$combo), "n"]),
          sum(patients_sensitive_severe_overall[grep("Overall", patients_sensitive_severe_overall$combo), "n"])),
    outcome_type = c(rep("mild_overall", 3), rep("severe_overall", 3)),
    codelist_type = rep("sensitive", 6)
  )
  patients_sensitive_overall <- rbind(patients_sensitive_totals_overall,
                                      patients_sensitive_overall)
  
}

#combine as one 
patients_combined <- patients_specific %>%
  full_join(patients_sensitive) %>%
  full_join(patients_sensitive_overall)

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#write to file
write_csv(patients_combined, paste0(here::here("output", "exploratory"),
          "/", "phenotype_sensitivity_", cohort, "_", year(study_start_date), "_", 
          year(study_end_date), ".csv"))
