library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here::here("analysis", "exploratory_analyses"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
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

##define a function to create a text coding for outcomes 
alt_label <- function(input, sensitivity, study_start_date, covid_season_min) {
  
  if (study_start_date >= covid_season_min) {
      
    input <- input %>%
      mutate(
        rsv_mild_alt = if_else(rsv_primary_inf == 1, "RSV_Mild_0_0", "0_0_0"),
        flu_mild_alt = if_else(flu_primary_inf == 1, "0_Flu_Mild_0", "0_0_0"),
        covid_mild_alt = if_else(covid_primary_inf == 1, "0_0_COVID_Mild", "0_0_0"),
        rsv_severe_alt = if_else(rsv_secondary_inf == 1, "RSV_Severe_0_0", "0_0_0"),
        flu_severe_alt = if_else(flu_secondary_inf == 1, "0_Flu_Severe_0", "0_0_0"),
        covid_severe_alt = if_else(covid_secondary_inf == 1, "0_0_COVID_Severe", "0_0_0"),
        primary_rsv_flu_within_season = if_else(rsv_primary_inf == 1 & flu_primary_inf == 1, TRUE, FALSE),
        primary_rsv_covid_within_season = if_else(rsv_primary_inf == 1 & covid_primary_inf == 1, TRUE, FALSE),
        primary_flu_covid_within_season = if_else(flu_primary_inf == 1 & covid_primary_inf == 1, TRUE, FALSE),
        primary_rsv_flu_covid_within_season = if_else(rsv_primary_inf == 1 & flu_primary_inf == 1 & covid_primary_inf == 1, TRUE, FALSE),
        secondary_rsv_flu_within_season = if_else(rsv_secondary_inf == 1 & flu_secondary_inf == 1, TRUE, FALSE),
        secondary_rsv_covid_within_season = if_else(rsv_secondary_inf == 1 & covid_secondary_inf == 1, TRUE, FALSE),
        secondary_flu_covid_within_season = if_else(flu_secondary_inf == 1 & covid_secondary_inf == 1, TRUE, FALSE),
        secondary_rsv_flu_covid_within_season = if_else(rsv_secondary_inf == 1 & flu_secondary_inf == 1 & covid_secondary_inf == 1, TRUE, FALSE),
        across(contains("_within_season"), ~if_else(is.na(.x), FALSE, .x))
        ) %>%
      mutate(
        rsv_mild_alt_combo = case_when(
          primary_rsv_flu_within_season ~ "RSV_Mild_Flu_Mild_0",
          primary_rsv_covid_within_season ~ "RSV_Mild_0_COVID_Mild",
          primary_rsv_flu_covid_within_season ~ "RSV_Mild_Flu_Mild_COVID_Mild",
          TRUE ~ rsv_mild_alt
        ),
        flu_mild_alt_combo = case_when(
          primary_rsv_flu_within_season ~ "RSV_Mild_Flu_Mild_0",
          primary_flu_covid_within_season ~ "0_Flu_Mild_COVID_Mild",
          primary_rsv_flu_covid_within_season ~ "RSV_Mild_Flu_Mild_COVID_Mild",
          TRUE ~ flu_mild_alt
        ),
        covid_mild_alt_combo = case_when(
          primary_rsv_covid_within_season ~ "RSV_Mild_0_COVID_Mild",
          primary_flu_covid_within_season ~ "0_Flu_Mild_COVID_Mild",
          primary_rsv_flu_covid_within_season ~ "RSV_Mild_Flu_Mild_COVID_Mild",
          TRUE ~ covid_mild_alt
        ),
        rsv_severe_alt_combo = case_when(
          secondary_rsv_flu_within_season ~ "RSV_Severe_Flu_Severe_0",
          secondary_rsv_covid_within_season ~ "RSV_Severe_0_COVID_Severe",
          secondary_rsv_flu_covid_within_season ~ "RSV_Severe_Flu_Severe_COVID_Severe",
          TRUE ~ rsv_severe_alt
        ),
        flu_severe_alt_combo = case_when(
          secondary_rsv_flu_within_season ~ "RSV_Severe_Flu_Severe_0",
          secondary_flu_covid_within_season ~ "0_Flu_Severe_COVID_Severe",
          secondary_rsv_flu_covid_within_season ~ "RSV_Severe_Flu_Severe_COVID_Severe",
          TRUE ~ flu_severe_alt
        ),
        covid_severe_alt_combo = case_when(
          secondary_rsv_covid_within_season ~ "RSV_Severe_0_COVID_Severe",
          secondary_flu_covid_within_season ~ "0_Flu_Severe_COVID_Severe",
          secondary_rsv_flu_covid_within_season ~ "RSV_Severe_Flu_Severe_COVID_Severe",
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
        primary_rsv_flu_within_season = if_else(rsv_primary_inf == 1 & flu_primary_inf == 1, TRUE, FALSE),
        secondary_rsv_flu_within_season = if_else(rsv_secondary_inf == 1 & flu_secondary_inf == 1, TRUE, FALSE),
        across(contains("_within_season"), ~if_else(is.na(.x), FALSE, .x))
      ) %>%
      mutate(
        rsv_mild_alt_combo = if_else(primary_rsv_flu_within_season == TRUE, 
                                     "RSV_Mild_Flu_Mild", rsv_mild_alt),
        flu_mild_alt_combo = if_else(primary_rsv_flu_within_season == TRUE, 
                                     "RSV_Mild_Flu_Mild", flu_mild_alt),
        rsv_severe_alt_combo = if_else(secondary_rsv_flu_within_season == TRUE, 
                                       "RSV_Severe_Flu_Severe", rsv_severe_alt),
        flu_severe_alt_combo = if_else(secondary_rsv_flu_within_season == TRUE, 
                                       "RSV_Severe_Flu_Severe", flu_severe_alt)
      )
    
    }
  
  return(input)
  
}

#apply function
df_input_specific <- alt_label(df_input, "specific", study_start_date, covid_season_min)

#select necessary columns for a new dataframe to work on 
if (study_start_date >= covid_season_min) {
  df_spec <- df_input_specific %>%
    select(patient_id, age_band, sex, latest_ethnicity_group,
           imd_quintile, rurality_classification,
           rsv_mild_alt_combo, flu_mild_alt_combo, 
           covid_mild_alt_combo, rsv_severe_alt_combo,
           flu_severe_alt_combo, covid_severe_alt_combo)
} else {
  df_spec <- df_input_specific %>%
    select(patient_id, age_band, sex, latest_ethnicity_group,
           imd_quintile, rurality_classification,
           rsv_mild_alt_combo, flu_mild_alt_combo,
           rsv_severe_alt_combo, flu_severe_alt_combo)
}

#reformat from wide to long
if (study_start_date >= covid_season_min) {
  df_spec_mild <- df_spec %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo, covid_mild_alt_combo), 
                 names_to = "mild_combo", values_to = "combo") %>%
    select(-c(rsv_severe_alt_combo, flu_severe_alt_combo, covid_severe_alt_combo))
  df_spec_severe <- df_spec %>%
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo, covid_severe_alt_combo), 
                 names_to = "severe_combo", values_to = "combo") %>%
    select(-c(rsv_mild_alt_combo, flu_mild_alt_combo, covid_mild_alt_combo))
} else {
  df_spec_mild <- df_spec %>%
    pivot_longer(c(rsv_mild_alt_combo, flu_mild_alt_combo),
                 names_to = "mild_combo", values_to = "combo") %>%
    select(-c(rsv_severe_alt_combo, flu_severe_alt_combo))
  df_spec_severe <- df_spec %>% 
    pivot_longer(c(rsv_severe_alt_combo, flu_severe_alt_combo),
                 names_to = "severe_combo", values_to = "combo") %>%
    select(-c(rsv_mild_alt_combo, flu_mild_alt_combo))
}

##count number of patients in each category for characteristic - separately for mild and severe 

#age group
patients_specific_mild_age <- df_spec_mild %>% 
  group_by(characteristic = age_band, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild")
patients_specific_severe_age <- df_spec_severe %>%
  group_by(characteristic = age_band, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe")
patients_specific_age <- full_join(patients_specific_mild_age,
                                   patients_specific_severe_age)
age_groups <- levels(unique(patients_specific_age$characteristic))

#get totals for sets 
if (study_start_date >= covid_season_min) {
  
  patients_specific_totals_age <- map_df(age_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_age,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_age,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_mild_age,
                   characteristic == .x & grepl("COVID", combo))$n),
        sum(filter(patients_specific_severe_age,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_age,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_age,
                   characteristic == .x & grepl("COVID", combo))$n)
      ),
      outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe")
    )
  })
  
  patients_specific_age <- bind_rows(patients_specific_totals_age,
                                     patients_specific_age)
  
} else {
  
  patients_specific_totals_age <- map_df(age_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_age,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_age,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_age,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_age,
                   characteristic == .x & grepl("Flu", combo))$n)
      ),
      outcome_type = c("mild", "mild", "severe", "severe")
    )
  })
  
  patients_specific_age <- bind_rows(patients_specific_totals_age,
                                     patients_specific_age)
  
}

#sex
patients_specific_mild_sex <- df_spec_mild %>% 
  group_by(characteristic = sex, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild")
patients_specific_severe_sex <- df_spec_severe %>%
  group_by(characteristic = sex, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe")
patients_specific_sex <- full_join(patients_specific_mild_sex,
                                  patients_specific_severe_sex)
sex_groups <- levels(unique(patients_specific_sex$characteristic))

#get totals for sets 
if (study_start_date >= covid_season_min) {
  
  patients_specific_totals_sex <- map_df(sex_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_sex,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_sex,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_mild_sex,
                   characteristic == .x & grepl("COVID", combo))$n),
        sum(filter(patients_specific_severe_sex,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_sex,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_sex,
                   characteristic == .x & grepl("COVID", combo))$n)
      ),
      outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe")
    )
  })
  
  patients_specific_sex <- bind_rows(patients_specific_totals_sex,
                                     patients_specific_sex)
  
} else {
  
  patients_specific_totals_sex <- map_df(sex_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_sex,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_sex,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_sex,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_sex,
                   characteristic == .x & grepl("Flu", combo))$n)
      ),
      outcome_type = c("mild", "mild", "severe", "severe")
    )
  })
  
  patients_specific_sex <- bind_rows(patients_specific_totals_sex,
                                     patients_specific_sex)
  
}

#ethnicity
patients_specific_mild_ethnicity <- df_spec_mild %>% 
  group_by(characteristic = latest_ethnicity_group, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild")
patients_specific_severe_ethnicity <- df_spec_severe %>%
  group_by(characteristic = latest_ethnicity_group, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe")
patients_specific_ethnicity <- full_join(patients_specific_mild_ethnicity,
                                         patients_specific_severe_ethnicity)
ethnicity_groups <- levels(unique(patients_specific_ethnicity$characteristic))

#get totals for sets 
if (study_start_date >= covid_season_min) {
  
  patients_specific_totals_ethnicity <- map_df(ethnicity_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_ethnicity,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_ethnicity,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_mild_ethnicity,
                   characteristic == .x & grepl("COVID", combo))$n),
        sum(filter(patients_specific_severe_ethnicity,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_ethnicity,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_ethnicity,
                   characteristic == .x & grepl("COVID", combo))$n)
      ),
      outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe")
    )
  })
  
  patients_specific_ethnicity <- bind_rows(patients_specific_totals_ethnicity,
                                           patients_specific_ethnicity)
  
} else {
  
  patients_specific_totals_ethnicity <- map_df(ethnicity_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_ethnicity,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_ethnicity,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_ethnicity,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_ethnicity,
                   characteristic == .x & grepl("Flu", combo))$n)
      ),
      outcome_type = c("mild", "mild", "severe", "severe")
    )
  })
  
  patients_specific_ethnicity <- bind_rows(patients_specific_totals_ethnicity,
                                           patients_specific_ethnicity)
  
}

#IMD
patients_specific_mild_imd <- df_spec_mild %>% 
  group_by(characteristic = imd_quintile, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild")
patients_specific_severe_imd <- df_spec_severe %>%
  group_by(characteristic = imd_quintile, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe")
patients_specific_imd <- full_join(patients_specific_mild_imd,
                                   patients_specific_severe_imd)
imd_groups <- levels(unique(patients_specific_imd$characteristic))

#get totals for sets 
if (study_start_date >= covid_season_min) {
  
  patients_specific_totals_imd <- map_df(imd_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_imd,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_imd,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_mild_imd,
                   characteristic == .x & grepl("COVID", combo))$n),
        sum(filter(patients_specific_severe_imd,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_imd,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_imd,
                   characteristic == .x & grepl("COVID", combo))$n)
      ),
      outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe")
    )
  })
  
  patients_specific_imd <- bind_rows(patients_specific_totals_imd,
                                     patients_specific_imd)
  
} else {
  
  patients_specific_totals_imd <- map_df(imd_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_imd,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_imd,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_imd,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_imd,
                   characteristic == .x & grepl("Flu", combo))$n)
      ),
      outcome_type = c("mild", "mild", "severe", "severe")
    )
  })
  
  patients_specific_imd <- bind_rows(patients_specific_totals_imd,
                                     patients_specific_imd)
  
}

#rurality
patients_specific_mild_rurality <- df_spec_mild %>% 
  group_by(characteristic = rurality_classification, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(outcome_type = "mild")
patients_specific_severe_rurality <- df_spec_severe %>%
  group_by(characteristic = rurality_classification, combo) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(outcome_type = "severe")
patients_specific_rurality <- full_join(patients_specific_mild_rurality,
                                        patients_specific_severe_rurality)
rurality_groups <- levels(unique(patients_specific_rurality$characteristic))

#get totals for sets 
if (study_start_date >= covid_season_min) {
  
  patients_specific_totals_rurality <- map_df(rurality_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", "COVID_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total", "COVID_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_rurality,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_rurality,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_mild_rurality,
                   characteristic == .x & grepl("COVID", combo))$n),
        sum(filter(patients_specific_severe_rurality,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_rurality,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_rurality,
                   characteristic == .x & grepl("COVID", combo))$n)
      ),
      outcome_type = c("mild", "mild", "mild", "severe", "severe", "severe")
    )
  })
  
  patients_specific_rurality <- bind_rows(patients_specific_totals_rurality,
                                          patients_specific_rurality)
  
} else {
  
  patients_specific_totals_rurality <- map_df(rurality_groups, ~ {
    tibble(
      characteristic = .x,
      combo = c("RSV_Mild_Total", "Flu_Mild_Total", 
                "RSV_Severe_Total", "Flu_Severe_Total"),
      n = c(
        sum(filter(patients_specific_mild_rurality,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_mild_rurality,
                   characteristic == .x & grepl("Flu", combo))$n),
        sum(filter(patients_specific_severe_rurality,
                   characteristic == .x & grepl("RSV", combo))$n),
        sum(filter(patients_specific_severe_rurality,
                   characteristic == .x & grepl("Flu", combo))$n)
      ),
      outcome_type = c("mild", "mild", "severe", "severe")
    )
  })
  
  patients_specific_rurality <- bind_rows(patients_specific_totals_rurality,
                                          patients_specific_rurality)
  
}

#combine all characteristics
patients_specific <- rbind(patients_specific_age,
                           patients_specific_sex,
                           patients_specific_ethnicity,
                           patients_specific_imd,
                           patients_specific_rurality,
                           patients_specific_severe_age,
                           patients_specific_severe_sex,
                           patients_specific_severe_ethnicity,
                           patients_specific_severe_imd, 
                           patients_specific_severe_rurality)

## create output directories ----
fs::dir_create(here("output", "exploratory"))

#write to file
write_csv(patients_specific, paste0(here::here("output", "exploratory"),
          "/", "multiple_episodes_", cohort, "_", year(study_start_date), "_", 
          year(study_end_date), ".csv"))
