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
  study_start_date <- "2021-09-01"
  study_end_date <- "2022-08-31"
  cohort <- "adults"
  codelist_type <- "specific"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  codelist_type <- args[[4]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

#import redaction function
source(here::here("analysis", "functions", "redaction.R"))

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", "primary",".arrow")))

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
      rsv_reinfection_mild = if_else(rsv_primary_inf == 1 &
                                       rsv_primary_second == 1, 1, 0),
      flu_reinfection_mild = if_else(flu_primary_inf == 1 &
                                       flu_primary_second == 1, 1, 0),
      covid_reinfection_mild = if_else(covid_primary_inf == 1 &
                                         covid_primary_second == 1, 1, 0),
      rsv_reinfection_severe = if_else(rsv_secondary_inf == 1 &
                                         rsv_secondary_second == 1, 1, 0),
      flu_reinfection_severe = if_else(flu_secondary_inf == 1 &
                                         flu_secondary_second == 1, 1, 0),
      covid_reinfection_severe = if_else(covid_secondary_inf == 1 &
                                           covid_secondary_second == 1, 1, 0)
    ) %>%
    summarise(
      rsv_reinfection_mild = sum(rsv_reinfection_mild, na.rm = TRUE),
      flu_reinfection_mild = sum(flu_reinfection_mild, na.rm = TRUE),
      covid_reinfection_mild = sum(covid_reinfection_mild, na.rm = TRUE),
      rsv_reinfection_severe = sum(rsv_reinfection_severe, na.rm = TRUE),
      flu_reinfection_severe = sum(flu_reinfection_severe, na.rm = TRUE),
      covid_reinfection_severe = sum(covid_reinfection_severe, na.rm = TRUE)
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
      rsv_reinfection_mild = if_else(rsv_primary_inf == 1 &
                                       rsv_primary_second == 1, 1, 0),
      flu_reinfection_mild = if_else(flu_primary_inf == 1 &
                                       flu_primary_second == 1, 1, 0),
      rsv_reinfection_severe = if_else(rsv_secondary_inf == 1 &
                                         rsv_secondary_second == 1, 1, 0),
      flu_reinfection_severe = if_else(flu_secondary_inf == 1 &
                                         flu_secondary_second == 1, 1, 0)
    ) %>%
    summarise(
      rsv_reinfection_mild = sum(rsv_reinfection_mild, na.rm = TRUE),
      flu_reinfection_mild = sum(flu_reinfection_mild, na.rm = TRUE),
      rsv_reinfection_severe = sum(rsv_reinfection_severe, na.rm = TRUE),
      flu_reinfection_severe = sum(flu_reinfection_severe, na.rm = TRUE)
    )
  
}

#round the counts
if (study_start_date >= covid_season_min) {

  total_reinfections <- total_reinfection %>%
    mutate(
      rsv_infection_mild_midpoint10 = roundmid_any(
        sum(df_input_specific$rsv_primary_inf == 1, na.rm = TRUE)
      ),
      rsv_reinfection_mild_midpoint10 = roundmid_any(rsv_reinfection_mild),
      flu_infection_mild_midpoint10 = roundmid_any(
        sum(df_input_specific$flu_primary_inf == 1, na.rm = TRUE)
      ),
      flu_reinfection_mild_midpoint10 = roundmid_any(flu_reinfection_mild),
      covid_infection_mild_midpoint10 = roundmid_any(
        sum(df_input_specific$covid_primary_inf == 1, na.rm = TRUE)
      ),
      covid_reinfection_mild_midpoint10 = roundmid_any(covid_reinfection_mild),
      rsv_infection_severe_midpoint10 = roundmid_any(
        sum(df_input_specific$rsv_secondary_inf == 1, na.rm = TRUE)
      ),
      rsv_reinfection_severe_midpoint10 = roundmid_any(rsv_reinfection_severe),
      flu_infection_severe_midpoint10 = roundmid_any(
        sum(df_input_specific$flu_secondary_inf == 1, na.rm = TRUE)
      ),
      flu_reinfection_severe_midpoint10 = roundmid_any(flu_reinfection_severe),
      covid_infection_severe_midpoint10 = roundmid_any(
        sum(df_input_specific$covid_secondary_inf == 1, na.rm = TRUE)
      ),
      covid_reinfection_severe_midpoint10 = roundmid_any(covid_reinfection_severe)
    ) %>%
    select(
      rsv_infection_mild_midpoint10,
      rsv_reinfection_mild_midpoint10,
      flu_infection_mild_midpoint10,
      flu_reinfection_mild_midpoint10,
      covid_infection_mild_midpoint10,
      covid_reinfection_mild_midpoint10,
      rsv_infection_severe_midpoint10,
      rsv_reinfection_severe_midpoint10,
      flu_infection_severe_midpoint10,
      flu_reinfection_severe_midpoint10,
      covid_infection_severe_midpoint10,
      covid_reinfection_severe_midpoint10
    )
  
} else {
  
  total_reinfections <- total_reinfection %>%
    mutate(
      rsv_infection_mild_midpoint10 = roundmid_any(
        sum(df_input_specific$rsv_primary_inf == 1, na.rm = TRUE)
      ),
      rsv_reinfection_mild_midpoint10 = roundmid_any(rsv_reinfection_mild),
      flu_infection_mild_midpoint10 = roundmid_any(
        sum(df_input_specific$flu_primary_inf == 1, na.rm = TRUE)
      ),
      flu_reinfection_mild_midpoint10 = roundmid_any(flu_reinfection_mild),
      rsv_infection_severe_midpoint10 = roundmid_any(
        sum(df_input_specific$rsv_secondary_inf == 1, na.rm = TRUE)
      ),
      rsv_reinfection_severe_midpoint10 = roundmid_any(rsv_reinfection_severe),
      flu_infection_severe_midpoint10 = roundmid_any(
        sum(df_input_specific$flu_secondary_inf == 1, na.rm = TRUE)
      ),
      flu_reinfection_severe_midpoint10 = roundmid_any(flu_reinfection_severe)
    ) %>%
    select(
      rsv_infection_mild_midpoint10,
      rsv_reinfection_mild_midpoint10,
      flu_infection_mild_midpoint10,
      flu_reinfection_mild_midpoint10,
      rsv_infection_severe_midpoint10,
      rsv_reinfection_severe_midpoint10,
      flu_infection_severe_midpoint10,
      flu_reinfection_severe_midpoint10
    )
  
}

#calculate proportion of patients with reinfections
 if (study_start_date >= covid_season_min) {
 
  proportions_mild <- df_input_specific %>% 
    mutate(
      rsv_reinfections_midpoint10_derived =
        total_reinfections$rsv_reinfection_mild_midpoint10 /
        roundmid_any(sum(rsv_primary_inf == 1, na.rm = TRUE)),
      flu_reinfections_midpoint10_derived =
        total_reinfections$flu_reinfection_mild_midpoint10 /
        roundmid_any(sum(flu_primary_inf == 1, na.rm = TRUE)),
      covid_reinfections_midpoint10_derived =
        total_reinfections$covid_reinfection_mild_midpoint10 /
        roundmid_any(sum(covid_primary_inf == 1, na.rm = TRUE)),
      outcome_type = "mild"
    ) %>%
    select(rsv_reinfections_midpoint10_derived,
           flu_reinfections_midpoint10_derived,
           covid_reinfections_midpoint10_derived,
           outcome_type) %>%
    unique()
  
  proportions_severe <- df_input_specific %>%
    mutate(
      rsv_reinfections_midpoint10_derived =
        total_reinfections$rsv_reinfection_severe_midpoint10 /
        roundmid_any(sum(rsv_secondary_inf == 1, na.rm = TRUE)),
      flu_reinfections_midpoint10_derived =
        total_reinfections$flu_reinfection_severe_midpoint10 /
        roundmid_any(sum(flu_secondary_inf == 1, na.rm = TRUE)),
      covid_reinfections_midpoint10_derived =
        total_reinfections$covid_reinfection_severe_midpoint10 /
        roundmid_any(sum(covid_secondary_inf == 1, na.rm = TRUE)),
      outcome_type = "severe"
    ) %>%
    select(rsv_reinfections_midpoint10_derived,
           flu_reinfections_midpoint10_derived,
           covid_reinfections_midpoint10_derived,
           outcome_type) %>%
    unique()
  
  proportions <- bind_rows(proportions_mild, proportions_severe)
  
 } else {
 
  proportions_mild <- df_input_specific %>% 
    mutate(
      rsv_reinfections_midpoint10_derived =
        total_reinfections$rsv_reinfection_mild_midpoint10 /
        roundmid_any(sum(rsv_primary_inf == 1, na.rm = TRUE)),
      flu_reinfections_midpoint10_derived =
        total_reinfections$flu_reinfection_mild_midpoint10 /
        roundmid_any(sum(flu_primary_inf == 1, na.rm = TRUE)),
      outcome_type = "mild"
    ) %>%
    select(rsv_reinfections_midpoint10_derived,
           flu_reinfections_midpoint10_derived,
           outcome_type) %>%
    unique()
  
  proportions_severe <- df_input_specific %>%
    mutate(
      rsv_reinfections_midpoint10_derived =
        total_reinfections$rsv_reinfection_severe_midpoint10 /
        roundmid_any(sum(rsv_secondary_inf == 1, na.rm = TRUE)),
      flu_reinfections_midpoint10_derived =
        total_reinfections$flu_reinfection_severe_midpoint10 /
        roundmid_any(sum(flu_secondary_inf == 1, na.rm = TRUE)),
      outcome_type = "severe"
    ) %>%
    select(rsv_reinfections_midpoint10_derived,
           flu_reinfections_midpoint10_derived,
           outcome_type) %>%
    unique()

  proportions <- bind_rows(proportions_mild, proportions_severe)
  
 }

#calculate median time to second infection
if (study_start_date >= covid_season_min) {
  
  time_to_reinfection_mild <- df_input_specific %>% 
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1)
    ) %>%
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_primary_second_date,
                                rsv_primary_date, units = "days"),
                                na.rm = TRUE)
    ) %>%
    cbind(
      df_input_specific %>%
        filter(
          (flu_primary_inf == 1 & flu_primary_second == 1)
        ) %>%
        summarise(
          flu_time_to_reinfection = median(difftime(flu_primary_second_date,
                                    flu_primary_date, units = "days"),
                                    na.rm = TRUE)
        )
    ) %>%
    cbind(
      df_input_specific %>%
        filter(
          (covid_primary_inf == 1 & covid_primary_second == 1)
        ) %>%
        summarise(
          covid_time_to_reinfection = median(difftime(covid_primary_second_date,
                                      covid_primary_date, units = "days"),
                                      na.rm = TRUE)
        )
    ) %>%
    mutate(outcome_type = "mild")
  
  time_to_reinfection_severe <- df_input_specific %>%
    filter(
      (rsv_secondary_inf == 1 & rsv_secondary_second == 1)
    ) %>%
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_secondary_second_date,
                                rsv_secondary_date, units = "days"),
                                na.rm = TRUE)
    ) %>%
    cbind(
      df_input_specific %>%
        filter(
          (flu_secondary_inf == 1 & flu_secondary_second == 1)
        ) %>%
        summarise(
          flu_time_to_reinfection = median(difftime(flu_secondary_second_date,
                                    flu_secondary_date, units = "days"),
                                    na.rm = TRUE)
        )
    ) %>%
    cbind(
      df_input_specific %>%
        filter(
          (covid_secondary_inf == 1 & covid_secondary_second == 1)
        ) %>%
        summarise(
          covid_time_to_reinfection = median(difftime(covid_secondary_second_date,
                                      covid_secondary_date, units = "days"),
                                      na.rm = TRUE)
        )
    ) %>%
    mutate(outcome_type = "severe")
  
  time_to_reinfection <- bind_rows(time_to_reinfection_mild,
                                   time_to_reinfection_severe)
  
} else {
  
  time_to_reinfection_mild <- df_input_specific %>%
    filter(
      (rsv_primary_inf == 1 & rsv_primary_second == 1)
    ) %>%
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_primary_second_date,
                                rsv_primary_date, units = "days"), na.rm = TRUE)
    ) %>%
    cbind(
      df_input_specific %>%
        filter(
          (flu_primary_inf == 1 & flu_primary_second == 1)
        ) %>%
        summarise(
          flu_time_to_reinfection = median(difftime(flu_primary_second_date,
                                    flu_primary_date, units = "days"),
                                    na.rm = TRUE)
        )
    ) %>%
    mutate(outcome_type = "mild")
  
  time_to_reinfection_severe <- df_input_specific %>%
    filter(
      (rsv_secondary_inf == 1 & rsv_secondary_second == 1)
    ) %>%
    summarise(
      rsv_time_to_reinfection = median(difftime(rsv_secondary_second_date,
                                rsv_secondary_date, units = "days"),
                                na.rm = TRUE)
    ) %>%
    cbind(
      df_input_specific %>%
        filter(
          (flu_secondary_inf == 1 & flu_secondary_second == 1)
        ) %>%
        summarise(
          flu_time_to_reinfection = median(difftime(flu_secondary_second_date,
                                    flu_secondary_date, units = "days"),
                                    na.rm = TRUE)
        )
    ) %>%
    mutate(outcome_type = "severe")
  
  time_to_reinfection <- bind_rows(time_to_reinfection_mild,
                                   time_to_reinfection_severe)
  
}

#calculate proportion of reinfections which occur within 28 days 
if (study_start_date >= covid_season_min) {
  
  proportions_28_days_mild <- df_input_specific %>% 
    summarise(
      rsv_reinfections_28_days_midpoint10 = roundmid_any(sum(
        rsv_primary_inf == 1 & rsv_primary_second == 1 &
          difftime(rsv_primary_second_date, rsv_primary_date,
                   units = "days") <= 28, na.rm = TRUE)),
      flu_reinfections_28_days_midpoint10 = roundmid_any(sum(
        flu_primary_inf == 1 & flu_primary_second == 1 &
          difftime(flu_primary_second_date, flu_primary_date,
                   units = "days") <= 28, na.rm = TRUE)),
      covid_reinfections_28_days_midpoint10 = roundmid_any(sum(
        covid_primary_inf == 1 & covid_primary_second == 1 &
          difftime(covid_primary_second_date, covid_primary_date,
                   units = "days") <= 28, na.rm = TRUE))
    ) %>% 
    mutate(
      rsv_reinfections_28_days_midpoint10_derived =
        rsv_reinfections_28_days_midpoint10 /
        total_reinfections$rsv_reinfection_mild_midpoint10,
      flu_reinfections_28_days_midpoint10_derived =
        flu_reinfections_28_days_midpoint10 /
        total_reinfections$flu_reinfection_mild_midpoint10,
      covid_reinfections_28_days_midpoint10_derived =
        covid_reinfections_28_days_midpoint10 /
        total_reinfections$covid_reinfection_mild_midpoint10,
      outcome_type = "mild"
    ) %>%
    select(rsv_reinfections_28_days_midpoint10,
           flu_reinfections_28_days_midpoint10,
           covid_reinfections_28_days_midpoint10,
           rsv_reinfections_28_days_midpoint10_derived,
           flu_reinfections_28_days_midpoint10_derived,
           covid_reinfections_28_days_midpoint10_derived,
           outcome_type)
  
  proportions_28_days_severe <- df_input_specific %>%
    summarise(
      rsv_reinfections_28_days_midpoint10 = roundmid_any(sum(
        rsv_secondary_inf == 1 & rsv_secondary_second == 1 &
          difftime(rsv_secondary_second_date, rsv_secondary_date,
                   units = "days") <= 28, na.rm = TRUE)),
      flu_reinfections_28_days_midpoint10 = roundmid_any(sum(
        flu_secondary_inf == 1 & flu_secondary_second == 1 &
          difftime(flu_secondary_second_date, flu_secondary_date,
                   units = "days") <= 28, na.rm = TRUE)),
      covid_reinfections_28_days_midpoint10 = roundmid_any(sum(
        covid_secondary_inf == 1 & covid_secondary_second == 1 &
          difftime(covid_secondary_second_date, covid_secondary_date,
                   units = "days") <= 28, na.rm = TRUE))
    ) %>%
    mutate(
      rsv_reinfections_28_days_midpoint10_derived =
        rsv_reinfections_28_days_midpoint10 /
        total_reinfections$rsv_reinfection_severe_midpoint10,
      flu_reinfections_28_days_midpoint10_derived =
        flu_reinfections_28_days_midpoint10 /
        total_reinfections$flu_reinfection_severe_midpoint10,
      covid_reinfections_28_days_midpoint10_derived =
        covid_reinfections_28_days_midpoint10 /
        total_reinfections$covid_reinfection_severe_midpoint10,
      outcome_type = "severe"
    ) %>%
    select(rsv_reinfections_28_days_midpoint10,
           flu_reinfections_28_days_midpoint10,
           covid_reinfections_28_days_midpoint10,
           rsv_reinfections_28_days_midpoint10_derived,
           flu_reinfections_28_days_midpoint10_derived,
           covid_reinfections_28_days_midpoint10_derived,
           outcome_type)
  
  proportions_28_days <- bind_rows(proportions_28_days_mild,
                                   proportions_28_days_severe)
  
} else {
 
  proportions_28_days_mild <- df_input_specific %>%
    summarise(
      rsv_reinfections_28_days_midpoint10 = roundmid_any(sum(
        rsv_primary_inf == 1 & rsv_primary_second == 1 &
          difftime(rsv_primary_second_date, rsv_primary_date,
                   units = "days") <= 28, na.rm = TRUE)),
      flu_reinfections_28_days_midpoint10 = roundmid_any(sum(
        flu_primary_inf == 1 & flu_primary_second == 1 &
          difftime(flu_primary_second_date, flu_primary_date,
                   units = "days") <= 28, na.rm = TRUE))
    ) %>% 
    mutate(
      rsv_reinfections_28_days_midpoint10_derived =
        rsv_reinfections_28_days_midpoint10 /
        total_reinfections$rsv_reinfection_mild_midpoint10,
      flu_reinfections_28_days_midpoint10_derived =
        flu_reinfections_28_days_midpoint10 /
        total_reinfections$flu_reinfection_mild_midpoint10,
      outcome_type = "mild"
    ) %>%
    select(rsv_reinfections_28_days_midpoint10,
           flu_reinfections_28_days_midpoint10,
           rsv_reinfections_28_days_midpoint10_derived,
           flu_reinfections_28_days_midpoint10_derived,
           outcome_type)
  
  proportions_28_days_severe <- df_input_specific %>%
    summarise(
      rsv_reinfections_28_days_midpoint10 = roundmid_any(sum(
        rsv_secondary_inf == 1 & rsv_secondary_second == 1 &
          difftime(rsv_secondary_second_date, rsv_secondary_date,
                   units = "days") <= 28, na.rm = TRUE)),
      flu_reinfections_28_days_midpoint10 = roundmid_any(sum(
        flu_secondary_inf == 1 & flu_secondary_second == 1 &
          difftime(flu_secondary_second_date, flu_secondary_date,
                   units = "days") <= 28, na.rm = TRUE))
    ) %>%
    mutate(
      rsv_reinfections_28_days_midpoint10_derived =
        rsv_reinfections_28_days_midpoint10 /
        total_reinfections$rsv_reinfection_severe_midpoint10,
      flu_reinfections_28_days_midpoint10_derived =
        flu_reinfections_28_days_midpoint10 /
        total_reinfections$flu_reinfection_severe_midpoint10,
      outcome_type = "severe"
    ) %>%
    select(rsv_reinfections_28_days_midpoint10,
           flu_reinfections_28_days_midpoint10,
           rsv_reinfections_28_days_midpoint10_derived,
           flu_reinfections_28_days_midpoint10_derived,
           outcome_type)
  
  proportions_28_days <- bind_rows(proportions_28_days_mild,
                                   proportions_28_days_severe)
 
}

#reformat wide to long 
if (study_start_date >= covid_season_min) {

  infections_long <- total_reinfections %>%
    pivot_longer(cols = c(rsv_infection_mild_midpoint10,
                          flu_infection_mild_midpoint10,
                          covid_infection_mild_midpoint10,
                          rsv_infection_severe_midpoint10,
                          flu_infection_severe_midpoint10,
                          covid_infection_severe_midpoint10),
                 names_to = "infection_type",
                 values_to = "number_infected_midpoint10") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )
  reinfections_long <- total_reinfections %>% 
    pivot_longer(cols = c(rsv_reinfection_mild_midpoint10,
                          flu_reinfection_mild_midpoint10,
                          covid_reinfection_mild_midpoint10,
                          rsv_reinfection_severe_midpoint10,
                          flu_reinfection_severe_midpoint10,
                          covid_reinfection_severe_midpoint10),
                 names_to = "infection_type",
                 values_to = "number_reinfected_midpoint10") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )
  proportions_long <- proportions %>% 
    pivot_longer(cols = c(rsv_reinfections_midpoint10_derived,
                          flu_reinfections_midpoint10_derived,
                          covid_reinfections_midpoint10_derived),
                 names_to = "infection_type",
                 values_to = "proportion_reinfected_midpoint10_derived") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )
  time_to_reinfection_long <- time_to_reinfection %>% 
    pivot_longer(cols = c(rsv_time_to_reinfection,
                          flu_time_to_reinfection,
                          covid_time_to_reinfection),
                 names_to = "infection_type",
                 values_to = "median_time_to_reinfection") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )
  number_reinfected_28_days_long <- proportions_28_days %>%
    pivot_longer(cols = c(rsv_reinfections_28_days_midpoint10,
                          flu_reinfections_28_days_midpoint10,
                          covid_reinfections_28_days_midpoint10),
                 names_to = "infection_type",
                 values_to = "number_reinfected_28_days_midpoint10") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )
  proportions_28_days_long <- proportions_28_days %>%
    pivot_longer(cols = c(rsv_reinfections_28_days_midpoint10_derived,
                          flu_reinfections_28_days_midpoint10_derived,
                          covid_reinfections_28_days_midpoint10_derived),
                 names_to = "infection_type",
                 values_to = "proportion_reinfected_in_28_days_midpoint10_derived") %>%
    mutate(
      outcome_type = c(rep("mild", 3), rep("severe", 3)),
      infection_type = rep(c("rsv", "flu", "covid"), 2)
    )

} else {
  
  infections_long <- total_reinfections %>%
    pivot_longer(cols = c(rsv_infection_mild_midpoint10,
                          flu_infection_mild_midpoint10,
                          rsv_infection_severe_midpoint10,
                          flu_infection_severe_midpoint10),
                 names_to = "infection_type",
                 values_to = "number_infected_midpoint10") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )
  reinfections_long <- total_reinfections %>% 
    pivot_longer(cols = c(rsv_reinfection_mild_midpoint10,
                          flu_reinfection_mild_midpoint10,
                          rsv_reinfection_severe_midpoint10,
                          flu_reinfection_severe_midpoint10),
                 names_to = "infection_type",
                 values_to = "number_reinfected_midpoint10") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )
  proportions_long <- proportions %>% 
    pivot_longer(cols = c(rsv_reinfections_midpoint10_derived,
                          flu_reinfections_midpoint10_derived),
                 names_to = "infection_type",
                 values_to = "proportion_reinfected_midpoint10_derived") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )
  time_to_reinfection_long <- time_to_reinfection %>% 
    pivot_longer(cols = c(rsv_time_to_reinfection,
                          flu_time_to_reinfection),
                 names_to = "infection_type",
                 values_to = "median_time_to_reinfection") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )
  number_reinfected_28_days_long <- proportions_28_days %>%
    pivot_longer(cols = c(rsv_reinfections_28_days_midpoint10,
                          flu_reinfections_28_days_midpoint10),
                 names_to = "infection_type",
                 values_to = "number_reinfected_28_days_midpoint10") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )
  proportions_28_days_long <- proportions_28_days %>%
    pivot_longer(cols = c(rsv_reinfections_28_days_midpoint10_derived,
                          flu_reinfections_28_days_midpoint10_derived),
                 names_to = "infection_type",
                 values_to = "proportion_reinfected_in_28_days_midpoint10_derived") %>%
    mutate(
      outcome_type = c(rep("mild", 2), rep("severe", 2)),
      infection_type = rep(c("rsv", "flu"), 2)
    )

}

#combine dataframes
patients <- full_join(infections_long, reinfections_long,
                      by = c("outcome_type", "infection_type"))
patients <- full_join(patients, proportions_long,
                      by = c("outcome_type", "infection_type"))
patients <- full_join(patients, time_to_reinfection_long,
                      by = c("outcome_type", "infection_type"))
patients <- full_join(patients, number_reinfected_28_days_long,
                      by = c("outcome_type", "infection_type"))
patients <- full_join(patients, proportions_28_days_long,
                      by = c("outcome_type", "infection_type"))

#reorder columns
patients <- patients[, c("infection_type", "outcome_type",
                         "number_infected_midpoint10",
                         "number_reinfected_midpoint10", 
                         "proportion_reinfected_midpoint10_derived",
                         "median_time_to_reinfection",
                         "number_reinfected_28_days_midpoint10",
                         "proportion_reinfected_in_28_days_midpoint10_derived")]

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#write to file
write_csv(patients, paste0(here::here("output", "exploratory"),
          "/", "reinfections_", cohort, "_", year(study_start_date), "_", 
          year(study_end_date), "_", codelist_type, ".csv"))
