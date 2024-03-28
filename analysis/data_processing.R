library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(lubridate)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "adults"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type,".arrow")))

#calculate age bands
if(cohort == "older_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 65 & age <= 74 ~ "65-74y",
      age >= 75 & age <= 89 ~ "75-89y",
      age >= 90 ~ "90+y",
      TRUE ~ NA_character_
    ))
} else if(cohort == "adults") {
df_input <- df_input %>%
  mutate(age_band = case_when(
    age >= 18 & age <= 39 ~ "18-29y",
    age >= 40 & age <= 64 ~ "40-64y",
    TRUE ~ NA_character_
  ))
} else if(cohort == "children_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 2 & age <= 5 ~ "2-5y",
      age >= 6 & age <= 9 ~ "6-9y",
      age >= 10 & age <= 13 ~ "10-13y",
      age >= 14 & age <= 17 ~ "14-17y",
      TRUE ~ NA_character_
    ))
} else {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 0 & age <= 2 ~ "0-2m",
      age >= 3 & age <= 5 ~ "3-5m",
      age >= 6 & age <= 11 ~ "6-11m",
      age >= 12 & age <= 23 ~ "12-23m",
      TRUE ~ NA_character_
    ))
}

#data manipulation
df_input <- df_input %>%
  mutate(
    #assign ethnicity group
    latest_ethnicity_group = case_when(
      latest_ethnicity_code == "1" ~ "White",
      latest_ethnicity_code == "2" ~ "Mixed",
      latest_ethnicity_code == "3" ~ "Asian or Asian British",
      latest_ethnicity_code == "4" ~ "Black or Black British",
      latest_ethnicity_code == "5" ~ "Other Ethnic Groups",
      TRUE ~ "Unknown"),
    #calculate IMD quintile
    imd_quintile = case_when(
      imd_rounded >= 0 & imd_rounded < as.integer(32800 * 1 / 5) ~ "1 (most deprived)",
      imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
      imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
      imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
      imd_rounded < as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
      TRUE ~ NA_character_
    )
  )

#reverse order of IMD classifications
recode(df_input$imd_quintile, "1 (most deprived)" = "5 (most deprived)",
          "2" = "4", "3" = "3", "4" = "2",
          "5 (least deprived)" = "1 (least deprived)")

#more data manipulation
df_input <- df_input %>%
  mutate(
    #recode rurality to 5 levels
    rurality_code = recode(rural_urban_classification, "1" = "1", "2" = "2", 
                           "3" = "3", "4" = "3", "5" = "4", "6" = "4", 
                           "7" = "5", "8" = "5"),
    #define household size categories
    household_size_cat = case_when(
      household_size >= 1 & household_size <= 2 ~ "1",
      household_size >= 3 & household_size <= 5 ~ "2",
      household_size >= 6 ~ "3",
      TRUE ~ "Unknown"
    ),
    #assign rurality classification
    rurality_classification = case_when(
      rurality_code == "1" ~ "Urban Major Conurbation",
      rurality_code == "2" ~ "Urban Minor Conurbation",
      rurality_code == "3" ~ "Urban City and Town",
      rurality_code == "4" ~ "Rural Town and Fringe",
      rurality_code == "5" ~ "Rural Village and Dispersed",
      TRUE ~ "Unknown"
    )
  )


#define seasons for covid
covid_season_min = as.Date("2019-09-01", format = "%Y-%m-%d")

#define event time 
if (study_start_date < covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #infer mild case date for rsv 
      rsv_primary_inf_date = case_when(
        is.na(rsv_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(rsv_primary_date) & !is.na(rsv_secondary_date) ~ rsv_secondary_date,
        TRUE ~ rsv_primary_date
      ),
      #assign censoring indicator
      rsv_primary_censor = case_when(
        rsv_primary_inf_date == rsv_primary_date ~ 0,
        rsv_primary_inf_date == rsv_secondary_date ~ 0,
        TRUE ~ 1
      ),
      #infer rsv outcome 
      rsv_primary_inf = ifelse(rsv_primary_censor == 0, TRUE, FALSE),
      #infer mild case date for flu
      flu_primary_inf_date = case_when(
        is.na(flu_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(flu_primary_date) & !is.na(flu_secondary_date) ~ flu_secondary_date,
        TRUE ~ flu_primary_date
      ),
      #assign censoring indicator
      flu_primary_censor = case_when(
        flu_primary_inf_date == flu_primary_date ~ 0,
        flu_primary_inf_date == flu_secondary_date ~ 0,
        TRUE ~ 1
      ),
      #infer flu outcome
      flu_primary_inf = ifelse(flu_primary_censor == 0, TRUE, FALSE)
  )
} else {
  df_input <- df_input %>%
    mutate(
      #infer mild case date for rsv
      rsv_primary_inf_date = case_when(
        is.na(rsv_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(rsv_primary_date) & !is.na(rsv_secondary_date) ~ rsv_secondary_date,
        TRUE ~ rsv_primary_date
      ),
      #assign censoring indicator
      rsv_primary_censor = case_when(
        rsv_primary_inf_date == rsv_primary_date ~ 0,
        TRUE ~ 1
      ),
      #infer rsv outcome 
      rsv_primary_inf = ifelse(rsv_primary_censor == 0, TRUE, FALSE),
      #infer mild case date for flu
      flu_primary_inf_date = case_when(
        is.na(flu_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(flu_primary_date) & !is.na(flu_secondary_date) ~ flu_secondary_date,
        TRUE ~ flu_primary_date
      ),
      #assign censoring indicator
      flu_primary_censor = case_when(
        flu_primary_inf_date == flu_primary_date ~ 0,
        TRUE ~ 1
      ),
      #infer flu outcome
      flu_primary_inf = ifelse(flu_primary_censor == 0, TRUE, FALSE),
      #infer mild case date for covid
      covid_primary_inf_date = case_when(
        is.na(covid_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(covid_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(covid_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(covid_primary_date) & !is.na(covid_secondary_date) ~ covid_secondary_date,
        TRUE ~ covid_primary_date
      ),
      #assign censoring indicator
      covid_primary_censor = case_when(
        covid_primary_inf_date == covid_primary_date ~ 0,
        TRUE ~ 1
      ),
      #infer covid outcome
      covid_primary_inf = ifelse(covid_primary_censor == 0, TRUE, FALSE)
    )  
}

#calculate time to event
if (study_start_date < covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #time until mild RSV outcome
      time_rsv_primary = as.numeric(difftime(rsv_primary_inf_date, 
                         study_start_date, "weeks"))/52.25,
      #time until severe rsv outcome
      time_rsv_secondary = as.numeric(difftime(rsv_secondary_date, 
                           study_start_date, "weeks"))/52.25,
      #time until mild flu outcome
      time_flu_primary = as.numeric(difftime(flu_primary_inf_date, 
                         study_start_date, "weeks"))/52.25,
      #time until severe flu outcome
      time_flu_secondary = as.numeric(difftime(flu_secondary_date, 
                           study_start_date, "weeks"))/52.25
  )
} else {
  df_input <- df_input %>%
    mutate(
      #time until mild RSV outcome
      time_rsv_primary = as.numeric(difftime(rsv_primary_inf_date, 
                         study_start_date, "weeks"))/52.25,
      #time until severe rsv outcome
      time_rsv_secondary = as.numeric(difftime(rsv_secondary_date, 
                           study_start_date, "weeks"))/52.25,
      #time until mild flu outcome
      time_flu_primary = as.numeric(difftime(flu_primary_inf_date, 
                         study_start_date, "weeks"))/52.25,
      #time until severe flu outcome
      time_flu_secondary = as.numeric(difftime(flu_secondary_date, 
                           study_start_date, "weeks"))/52.25,
      #time until mild covid outcome
      time_covid_primary = as.numeric(difftime(covid_primary_inf_date, 
                           study_start_date, "weeks"))/52.25,
      #time until severe covid outcome
      time_covid_secondary = as.numeric(difftime(covid_secondary_date, 
                             study_start_date, "weeks"))/52.25
  )
}

## create output directories ----
fs::dir_create(here("output", "data"))

#write the new input file
write_feather(df_input, here::here("output", "data", 
  paste0("input_processed_", cohort, "_", year(study_start_date),
         "_", year(study_end_date), "_", codelist_type, 
         "_", investigation_type, ".arrow")))
