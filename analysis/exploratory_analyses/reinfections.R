library(tidyverse)
library(here)
library(arrow)
library(lubridate)

options(scipen = 999)

## create output directories ----
fs::dir_create(here::here("analysis", "exploratory_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2017-09-01"
  study_end_date <- "2018-08-31"
  cohort <- "adults"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
                                      year(study_start_date), "_", year(study_end_date), "_", 
                                      "specific", "_", "primary",".arrow")))

#select necessary columns
if (study_start_date >= covid_season_min) {
  df_input_specific <- df_input %>% 
    select(
      patient_id, 
      rsv_primary_inf,
      rsv_primary_date, 
      rsv_primary_second,
      rsv_primary_second_date, 
      flu_primary_inf,
      flu_primary_date,
      flu_primary_second,
      flu_primary_second_date,
      covid_primary_inf,
      covid_primary_date,
      covid_primary_second,
      covid_primary_second_date,
      rsv_secondary_inf,
      rsv_secondary_date,
      rsv_secondary_second,
      rsv_secondary_second_date,
      flu_secondary_inf,
      flu_secondary_date,
      flu_secondary_second,
      flu_secondary_second_date,
      covid_secondary_inf,
      covid_secondary_date,
      covid_secondary_second,
      covid_secondary_second_date
    ) %>%
    mutate(
      rsv_primary_second = if_else(rsv_primary_second == T, 1, 0),
      flu_primary_second = if_else(flu_primary_second == T, 1, 0),
      covid_primary_second = if_else(covid_primary_second == T, 1, 0),
      rsv_secondary_second = if_else(rsv_secondary_second == T, 1, 0),
      flu_secondary_second = if_else(flu_secondary_second == T, 1, 0),
      covid_secondary_second = if_else(covid_secondary_second == T, 1, 0)
    )
} else {
  df_input_specific <- df_input %>% 
    select(
      patient_id, 
      rsv_primary_inf,
      rsv_primary_date, 
      rsv_primary_second,
      rsv_primary_second_date, 
      flu_primary_inf,
      flu_primary_date,
      flu_primary_second,
      flu_primary_second_date,
      rsv_secondary_inf,
      rsv_secondary_date,
      rsv_secondary_second,
      rsv_secondary_second_date,
      flu_secondary_inf,
      flu_secondary_date,
      flu_secondary_second,
      flu_secondary_second_date
    ) %>%
    mutate(
      rsv_primary_second = if_else(rsv_primary_second == T, 1, 0),
      flu_primary_second = if_else(flu_primary_second == T, 1, 0),
      rsv_secondary_second = if_else(rsv_secondary_second == T, 1, 0),
      flu_secondary_second = if_else(flu_secondary_second == T, 1, 0)
    ) 
}

total_patients <- nrow(df_input_specific)

if (study_start_date >= covid_season_min) { 
  
  total_reinfection <- df_input_specific %>%
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1) | 
        (flu_primary_inf == 1 & flu_primary_second == 1) | 
        (covid_primary_inf == 1 & covid_primary_second == 1) |
        (rsv_secondary_inf == 1 & rsv_secondary_second == 1) | 
        (flu_secondary_inf == 1 & flu_secondary_second == 1) | 
        (covid_secondary_inf == 1 & covid_secondary_second == 1)
    ) %>%
    mutate(
      rsv_reinfection_mild = if_else(rsv_primary_inf == 1 & rsv_primary_second == 1, 1, 0),
      flu_reinfection_mild = if_else(flu_primary_inf == 1 & flu_primary_second == 1, 1, 0),
      covid_reinfection_mild = if_else(covid_primary_inf == 1 & covid_primary_second == 1, 1, 0),
      # total_reinfection_mild = if_else((rsv_primary_inf == 1 & rsv_primary_second == 1)|
      #                          (flu_primary_inf == 1 & flu_primary_second == 1)|
      #                          (covid_primary_inf == 1 & covid_primary_second == 1),
      #                          1, 0),
      rsv_reinfection_severe = if_else(rsv_secondary_inf == 1 & rsv_secondary_second == 1, 1, 0),
      flu_reinfection_severe = if_else(flu_secondary_inf == 1 & flu_secondary_second == 1, 1, 0),
      covid_reinfection_severe = if_else(covid_secondary_inf == 1 & covid_secondary_second == 1, 1, 0)#,
      # total_reinfection_severe = if_else((rsv_secondary_inf == 1 & rsv_secondary_second == 1)|
      #                            (flu_secondary_inf == 1 & flu_secondary_second == 1)|
      #                            (covid_secondary_inf == 1 & covid_secondary_second == 1),
      #                            1, 0)
    ) %>%
    summarise(
      rsv_reinfection_mild = sum(rsv_reinfection_mild, na.rm = TRUE),
      flu_reinfection_mild = sum(flu_reinfection_mild, na.rm = TRUE),
      covid_reinfection_mild = sum(covid_reinfection_mild, na.rm = TRUE),
      # total_reinfection_mild = sum(total_reinfection_mild, na.rm = TRUE),
      rsv_reinfection_severe = sum(rsv_reinfection_severe, na.rm = TRUE),
      flu_reinfection_severe = sum(flu_reinfection_severe, na.rm = TRUE),
      covid_reinfection_severe = sum(covid_reinfection_severe, na.rm = TRUE)#,
      # total_reinfection_severe = sum(total_reinfection_severe, na.rm = TRUE)
    ) 
  
} else {
  
  total_reinfection <- df_input_specific %>%
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1) | 
        (flu_primary_inf == 1 & flu_primary_second == 1) | 
        (rsv_secondary_inf == 1 & rsv_secondary_second == 1) | 
        (flu_secondary_inf == 1 & flu_secondary_second == 1)
    ) %>%
    mutate(
      rsv_reinfection_mild = if_else(rsv_primary_inf == 1 & rsv_primary_second == 1, 1, 0),
      flu_reinfection_mild = if_else(flu_primary_inf == 1 & flu_primary_second == 1, 1, 0),
      rsv_reinfection_severe = if_else(rsv_secondary_inf == 1 & rsv_secondary_second == 1, 1, 0),
      flu_reinfection_severe = if_else(flu_secondary_inf == 1 & flu_secondary_second == 1, 1, 0)
    ) %>%
    summarise(
      rsv_reinfection_mild = sum(rsv_reinfection_mild, na.rm = TRUE),
      flu_reinfection_mild = sum(flu_reinfection_mild, na.rm = TRUE),
      rsv_reinfection_severe = sum(rsv_reinfection_severe, na.rm = TRUE),
      flu_reinfection_severe = sum(flu_reinfection_severe, na.rm = TRUE)
    )
  
}

#calculate proportion of patients with reinfections
 if (study_start_date >= covid_season_min) {
 
  proportions_mild <- df_input_specific %>% 
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1) | 
        (flu_primary_inf == 1 & flu_primary_second == 1) | 
        (covid_primary_inf == 1 & covid_primary_second == 1)
    ) %>% 
    summarise(
      rsv_reinfections = sum(rsv_primary_inf == 1 & rsv_primary_second == 1,
                             na.rm = TRUE),
      flu_reinfections = sum(flu_primary_inf == 1 & flu_primary_second == 1,
                             na.rm = TRUE),
      covid_reinfections = sum(covid_primary_inf == 1 & covid_primary_second == 1,
                               na.rm = TRUE)#,
      # total_reinfections = sum(
      #   (rsv_primary_inf == 1 & rsv_primary_second == 1) | 
      #     (flu_primary_inf == 1 & flu_primary_second == 1) | 
      #     (covid_primary_inf == 1 & covid_primary_second == 1),
      #   na.rm = TRUE
      # )
    ) %>% 
    mutate(
      rsv_reinfections = rsv_reinfections / total_patients,
      flu_reinfections = flu_reinfections / total_patients,
      covid_reinfections = covid_reinfections / total_patients,
      # total_reinfections = total_reinfections / total_patients,
      outcome_type = "mild"
    )
  
  proportions_severe <- df_input_specific %>%
    filter(
      (rsv_secondary_inf == 1 & rsv_secondary_second == 1) | 
        (flu_secondary_inf == 1 & flu_secondary_second == 1) | 
        (covid_secondary_inf == 1 & covid_secondary_second == 1)
    ) %>%
    summarise(
      rsv_reinfections = sum(rsv_secondary_inf == 1 & rsv_secondary_second == 1,
                             na.rm = TRUE),
      flu_reinfections = sum(flu_secondary_inf == 1 & flu_secondary_second == 1,
                             na.rm = TRUE),
      covid_reinfections = sum(covid_secondary_inf == 1 & covid_secondary_second == 1,
                               na.rm = TRUE)#,
      # total_reinfections = sum(
      #   (rsv_secondary_inf == 1 & rsv_secondary_second == 1) | 
      #     (flu_secondary_inf == 1 & flu_secondary_second == 1) | 
      #     (covid_secondary_inf == 1 & covid_secondary_second == 1),
      #   na.rm = TRUE
      # )
    ) %>%
    mutate(
      rsv_reinfections = rsv_reinfections / total_patients,
      flu_reinfections = flu_reinfections / total_patients,
      covid_reinfections = covid_reinfections / total_patients,
      # total_reinfections = total_reinfections / total_patients,
      outcome_type = "severe"
    )
  
  proportions <- bind_rows(proportions_mild, proportions_severe)
  
 } else {
 
  proportions_mild <- df_input_specific %>% 
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1) | 
        (flu_primary_inf == 1 & flu_primary_second == 1)
    ) %>% 
    summarise(
      rsv_reinfections = sum(rsv_primary_inf == 1 & rsv_primary_second == 1,
                             na.rm = TRUE),
      flu_reinfections = sum(flu_primary_inf == 1 & flu_primary_second == 1,
                             na.rm = TRUE)#,
      # total_reinfections = sum(
      #   (rsv_primary_inf == 1 & rsv_primary_second == 1) | 
      #     (flu_primary_inf == 1 & flu_primary_second == 1),
      #   na.rm = TRUE
      # )
    ) %>% 
    mutate(
      rsv_reinfections = rsv_reinfections / total_patients,
      flu_reinfections = flu_reinfections / total_patients,
      # total_reinfections = total_reinfections / total_patients,
      outcome_type = "mild"
    )
  
  proportions_severe <- df_input_specific %>%
    filter(
      (rsv_secondary_inf == 1 & rsv_secondary_second == 1) | 
        (flu_secondary_inf == 1 & flu_secondary_second == 1)
    ) %>%
    summarise(
      rsv_reinfections = sum(rsv_secondary_inf == 1 & rsv_secondary_second == 1,
                             na.rm = TRUE),
      flu_reinfections = sum(flu_secondary_inf == 1 & flu_secondary_second == 1,
                             na.rm = TRUE)#,
      # total_reinfections = sum(
      #   (rsv_secondary_inf == 1 & rsv_secondary_second == 1) | 
      #     (flu_secondary_inf == 1 & flu_secondary_second == 1),
      #   na.rm = TRUE
      # )
    ) %>%
    mutate(
      rsv_reinfections = rsv_reinfections / total_patients,
      flu_reinfections = flu_reinfections / total_patients,
      # total_reinfections = total_reinfections / total_patients,
      outcome_type = "severe"
    )

  proportions <- bind_rows(proportions_mild, proportions_severe)
  
 }

#calculate median time to second infection
if (study_start_date >= covid_season_min) {
  
  time_to_reinfection_mild <- df_input_specific %>% 
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_primary_second_date,
                                rsv_primary_date, units = "days"), na.rm = TRUE),
      flu_time_to_reinfection = median(difftime(flu_primary_second_date,
                                flu_primary_date, units = "days"), na.rm = TRUE),
      covid_time_to_reinfection = median(difftime(covid_primary_second_date,
                                  covid_primary_date, units = "days"), na.rm = TRUE)
    ) %>%
    mutate(outcome_type = "mild")
  
  time_to_reinfection_severe <- df_input_specific %>%
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_secondary_second_date,
                                rsv_secondary_date, units = "days"), na.rm = TRUE),
      flu_time_to_reinfection = median(difftime(flu_secondary_second_date,
                                flu_secondary_date, units = "days"), na.rm = TRUE),
      covid_time_to_reinfection = median(difftime(covid_secondary_second_date,
                                  covid_secondary_date, units = "days"), na.rm = TRUE)
    ) %>%
    mutate(outcome_type = "severe")
  
  time_to_reinfection <- bind_rows(time_to_reinfection_mild,
                                   time_to_reinfection_severe)
  
} else {
  
  time_to_reinfection_mild <- df_input_specific %>% 
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_primary_second_date,
                                rsv_primary_date, units = "days"), na.rm = TRUE),
      flu_time_to_reinfection = median(difftime(flu_primary_second_date,
                                flu_primary_date, units = "days"), na.rm = TRUE)
    ) %>%
    mutate(outcome_type = "mild")
  
  time_to_reinfection_severe <- df_input_specific %>%
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_secondary_second_date,
                                rsv_secondary_date, units = "days"), na.rm = TRUE),
      flu_time_to_reinfection = median(difftime(flu_secondary_second_date,
                                flu_secondary_date, units = "days"), na.rm = TRUE)
    ) %>%
    mutate(outcome_type = "severe")
  
  time_to_reinfection <- bind_rows(time_to_reinfection_mild,
                                   time_to_reinfection_severe)
  
}

#calculate proportion of reinfections which occur within 28 days 
if (study_start_date >= covid_season_min) {
  
  proportions_28_days_mild <- df_input_specific %>% 
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1 & 
         difftime(rsv_primary_second_date, rsv_primary_date,
                  units = "days") <= 28) | 
        (flu_primary_inf == 1 & flu_primary_second == 1 & 
           difftime(flu_primary_second_date, flu_primary_date,
                    units = "days") <= 28) | 
        (covid_primary_inf == 1 & covid_primary_second == 1 & 
           difftime(covid_primary_second_date, covid_primary_date,
                    units = "days") <= 28)
    ) %>% 
    summarise(
      rsv_reinfections_28_days = sum(rsv_primary_inf == 1 & rsv_primary_second == 1 & 
                                      difftime(rsv_primary_second_date,
                                               rsv_primary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE),
      flu_reinfections_28_days = sum(flu_primary_inf == 1 & flu_primary_second == 1 & 
                                      difftime(flu_primary_second_date,
                                               flu_primary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE),
      covid_reinfections_28_days = sum(covid_primary_inf == 1 & covid_primary_second == 1 & 
                                        difftime(covid_primary_second_date,
                                                 covid_primary_date,
                                                 units = "days") <= 28,
                                       na.rm = TRUE)#,
      # total_reinfections_28_days = sum(
      #   (rsv_primary_inf == 1 & rsv_primary_second == 1 & 
      #      difftime(rsv_primary_second_date, rsv_primary_date, units = "days") <= 28) | 
      #     (flu_primary_inf == 1 & flu_primary_second == 1 & 
      #        difftime(flu_primary_second_date, flu_primary_date, units = "days") <= 28) | 
      #     (covid_primary_inf == 1 & covid_primary_second == 1 & 
      #        difftime(covid_primary_second_date, covid_primary_date, units = "days") <= 28),
      #   na.rm = TRUE
      # )
    ) %>% 
    mutate(
      rsv_reinfections_28_days = rsv_reinfections_28_days /
        total_reinfection$rsv_reinfection_mild,
      flu_reinfections_28_days = flu_reinfections_28_days /
        total_reinfection$flu_reinfection_mild,
      covid_reinfections_28_days = covid_reinfections_28_days /
        total_reinfection$covid_reinfection_mild,
      # total_reinfections_28_days = total_reinfections_28_days /
      #   total_reinfection$total_reinfection_mild,
      outcome_type = "mild"
    )
  
  proportions_28_days_severe <- df_input_specific %>%
    filter(
      (rsv_secondary_inf == 1 & rsv_secondary_second == 1 & 
         difftime(rsv_secondary_second_date, rsv_secondary_date,
                  units = "days") <= 28) | 
        (flu_secondary_inf == 1 & flu_secondary_second == 1 & 
           difftime(flu_secondary_second_date, flu_secondary_date,
                    units = "days") <= 28) | 
        (covid_secondary_inf == 1 & covid_secondary_second == 1 & 
           difftime(covid_secondary_second_date, covid_secondary_date,
                    units = "days") <= 28)
    ) %>%
    summarise(
      rsv_reinfections_28_days = sum(rsv_secondary_inf == 1 & rsv_secondary_second == 1 & 
                                      difftime(rsv_secondary_second_date,
                                               rsv_secondary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE),
      flu_reinfections_28_days = sum(flu_secondary_inf == 1 & flu_secondary_second == 1 & 
                                      difftime(flu_secondary_second_date,
                                               flu_secondary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE),
      covid_reinfections_28_days = sum(covid_secondary_inf == 1 & covid_secondary_second == 1 & 
                                        difftime(covid_secondary_second_date,
                                                 covid_secondary_date,
                                                 units = "days") <= 28,
                                       na.rm = TRUE)#,
      # total_reinfections_28_days = sum(
      #   (rsv_secondary_inf == 1 & rsv_secondary_second == 1 & 
      #      difftime(rsv_secondary_second_date, rsv_secondary_date, units = "days") <= 28) | 
      #     (flu_secondary_inf == 1 & flu_secondary_second == 1 & 
      #        difftime(flu_secondary_second_date, flu_secondary_date, units = "days") <= 28) | 
      #     (covid_secondary_inf == 1 & covid_secondary_second == 1 & 
      #        difftime(covid_secondary_second_date, covid_secondary_date, units = "days") <= 28),
      #   na.rm = TRUE
      # )
    ) %>%
    mutate(
      rsv_reinfections_28_days = rsv_reinfections_28_days /
        total_reinfection$rsv_reinfection_severe,
      flu_reinfections_28_days = flu_reinfections_28_days /
        total_reinfection$flu_reinfection_severe,
      covid_reinfections_28_days = covid_reinfections_28_days /
        total_reinfection$covid_reinfection_severe,
      # total_reinfections_28_days = total_reinfections_28_days /
      #   total_reinfection$total_reinfection_severe,
      outcome_type = "severe"
    )
  
  proportions_28_days <- bind_rows(proportions_28_days_mild,
                                   proportions_28_days_severe)
  
} else {
 
  proportions_28_days_mild <- df_input_specific %>% 
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1 & 
         difftime(rsv_primary_second_date, rsv_primary_date,
                  units = "days") <= 28) | 
        (flu_primary_inf == 1 & flu_primary_second == 1 & 
           difftime(flu_primary_second_date, flu_primary_date,
                    units = "days") <= 28)
    ) %>% 
    summarise(
      rsv_reinfections_28_days = sum(rsv_primary_inf == 1 & rsv_primary_second == 1 & 
                                      difftime(rsv_primary_second_date,
                                               rsv_primary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE),
      flu_reinfections_28_days = sum(flu_primary_inf == 1 & flu_primary_second == 1 & 
                                      difftime(flu_primary_second_date,
                                               flu_primary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE)#,
      # total_reinfections_28_days = sum(
      #   (rsv_primary_inf == 1 & rsv_primary_second == 1 & 
      #      difftime(rsv_primary_second_date, rsv_primary_date, units = "days") <= 28) | 
      #     (flu_primary_inf == 1 & flu_primary_second == 1 & 
      #        difftime(flu_primary_second_date, flu_primary_date, units = "days") <= 28),
      #   na.rm = TRUE
      # )
    ) %>% 
    mutate(
      rsv_reinfections_28_days = rsv_reinfections_28_days /
        total_reinfection$rsv_reinfection_mild,
      flu_reinfections_28_days = flu_reinfections_28_days /
        total_reinfection$flu_reinfection_mild,
      # total_reinfections_28_days = total_reinfections_28_days /
      #   total_reinfection$total_reinfection_mild,
      outcome_type = "mild"
    )
  
  proportions_28_days_severe <- df_input_specific %>%
    filter(
      (rsv_secondary_inf == 1 & rsv_secondary_second == 1 & 
         difftime(rsv_secondary_second_date, rsv_secondary_date,
                  units = "days") <= 28) |
        (flu_secondary_inf == 1 & flu_secondary_second == 1 & 
           difftime(flu_secondary_second_date, flu_secondary_date,
                    units = "days") <= 28)
    ) %>%
    summarise(
      rsv_reinfections_28_days = sum(rsv_secondary_inf == 1 & rsv_secondary_second == 1 & 
                                      difftime(rsv_secondary_second_date,
                                               rsv_secondary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE),
      flu_reinfections_28_days = sum(flu_secondary_inf == 1 & flu_secondary_second == 1 & 
                                      difftime(flu_secondary_second_date,
                                               flu_secondary_date,
                                               units = "days") <= 28,
                                     na.rm = TRUE)#,
      # total_reinfections_28_days = sum(
      #   (rsv_secondary_inf == 1 & rsv_secondary_second == 1 & 
      #      difftime(rsv_secondary_second_date, rsv_secondary_date, units = "days") <= 28) | 
      #     (flu_secondary_inf == 1 & flu_secondary_second == 1 & 
      #        difftime(flu_secondary_second_date, flu_secondary_date, units = "days") <= 28),
      #   na.rm = TRUE
      # )
    ) %>%
    mutate(
      rsv_reinfections_28_days = rsv_reinfections_28_days /
        total_reinfection$rsv_reinfection_severe,
      flu_reinfections_28_days = flu_reinfections_28_days /
        total_reinfection$flu_reinfection_severe,
      # total_reinfections_28_days = total_reinfections_28_days /
      #   total_reinfection$total_reinfection_severe,
      outcome_type = "severe"
    )
  
  proportions_28_days <- bind_rows(proportions_28_days_mild,
                                   proportions_28_days_severe)
 
}

#reformat wide to long 
if (study_start_date >= covid_season_min) {

  reinfections_long <- total_reinfection %>% 
    pivot_longer(cols = c(rsv_reinfection_mild, flu_reinfection_mild,
                          covid_reinfection_mild, rsv_reinfection_severe,
                          flu_reinfection_severe, covid_reinfection_severe),
                 names_to = "infection_type", values_to = "number_reinfected") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )
  proportions_long <- proportions %>% 
    rename(rsv = rsv_reinfections, flu = flu_reinfections,
           covid = covid_reinfections) %>%
    pivot_longer(cols = c(rsv, flu, covid), names_to = "infection_type",
                 values_to = "proportion_reinfected")
  time_to_reinfection_long <- time_to_reinfection %>% 
    rename(rsv = rsv_time_to_reinfection, flu = flu_time_to_reinfection,
           covid = covid_time_to_reinfection) %>%
    pivot_longer(cols = c(rsv, flu, covid), names_to = "infection_type",
                 values_to = "median_time_to_reinfection")
  proportions_28_days_long <- proportions_28_days %>%
    rename(rsv = rsv_reinfections_28_days, flu = flu_reinfections_28_days,
           covid = covid_reinfections_28_days) %>%
    pivot_longer(cols = c(rsv, flu, covid), names_to = "infection_type",
                 values_to = "proportion_reinfected_in_28_days")

} else {
  
  reinfections_long <- total_reinfection %>% 
    pivot_longer(cols = c(rsv_reinfection_mild, flu_reinfection_mild,
                          rsv_reinfection_severe, flu_reinfection_severe),
                 names_to = "infection_type", values_to = "number_reinfected") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )
  proportions_long <- proportions %>% 
    rename(rsv = rsv_reinfections, flu = flu_reinfections) %>%
    pivot_longer(cols = c(rsv, flu), names_to = "infection_type",
                 values_to = "proportion_reinfected")
  time_to_reinfection_long <- time_to_reinfection %>% 
    rename(rsv = rsv_time_to_reinfection, flu = flu_time_to_reinfection) %>%
    pivot_longer(cols = c(rsv, flu), names_to = "infection_type",
                 values_to = "median_time_to_reinfection")
  proportions_28_days_long <- proportions_28_days %>%
    rename(rsv = rsv_reinfections_28_days, flu = flu_reinfections_28_days) %>%
    pivot_longer(cols = c(rsv, flu), names_to = "infection_type",
                 values_to = "proportion_reinfected_in_28_days")

}

#combine dataframes
patients <- full_join(reinfections_long, proportions_long, by = c("outcome_type", "infection_type"))
patients <- full_join(patients, time_to_reinfection_long, by = c("outcome_type", "infection_type"))
patients <- full_join(patients, proportions_28_days_long, by = c("outcome_type", "infection_type"))

#reorder columns
patients <- patients[, c("infection_type", "outcome_type", "number_reinfected", 
                         "proportion_reinfected", "median_time_to_reinfection",
                         "proportion_reinfected_in_28_days")]

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#write to file
write_csv(patients, paste0(here::here("output", "exploratory"),
          "/", "reinfections_", cohort, "_", year(study_start_date), "_", 
          year(study_end_date), ".csv"))
