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

#assign ethnicity group
df_input <- df_input %>%
  mutate(
    latest_ethnicity_group = ifelse(df_input$latest_ethnicity_code == "1", "White",
                             ifelse(df_input$latest_ethnicity_code == "2", "Mixed",
                             ifelse(df_input$latest_ethnicity_code == "3", "Asian or Asian British",
                             ifelse(df_input$latest_ethnicity_code == "4", "Black or Black British",
                             ifelse(df_input$latest_ethnicity_code == "5", "Other Ethnic Groups", "Unknown"))))
    ))

#calculate age bands
if(cohort == "older_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      df_input$age >= 65 & df_input$age <= 74 ~ "65-74y",
      df_input$age >= 75 & df_input$age <= 89 ~ "75-89y",
      df_input$age >= 90 ~ "90+y",
      TRUE ~ NA_character_
    ))
} else if(cohort == "adults") {
df_input <- df_input %>%
  mutate(age_band = case_when(
    df_input$age >= 18 & df_input$age <= 39 ~ "18-29y",
    df_input$age >= 40 & df_input$age <= 64 ~ "40-64y",
    TRUE ~ NA_character_
  ))
} else if(cohort == "children_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      df_input$age >= 2 & df_input$age <= 5 ~ "2-5y",
      df_input$age >= 6 & df_input$age <= 9 ~ "6-9y",
      df_input$age >= 10 & df_input$age <= 13 ~ "10-13y",
      df_input$age >= 14 & df_input$age <= 17 ~ "14-17y",
      TRUE ~ NA_character_
    ))
} else {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      df_input$age >= 0 & df_input$age <= 2 ~ "0-2m",
      df_input$age >= 3 & df_input$age <= 5 ~ "3-5m",
      df_input$age >= 6 & df_input$age <= 11 ~ "6-11m",
      df_input$age >= 12 & df_input$age <= 23 ~ "12-23m",
      TRUE ~ NA_character_
    ))
}

#calculate IMD quintile
df_input <- df_input %>%
  mutate(imd_quintile = case_when(
    df_input$imd_rounded >= 0 & df_input$imd_rounded < as.integer(32800 * 1 / 5) ~ "1 (most deprived)",
    df_input$imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
    df_input$imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
    df_input$imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
    df_input$imd_rounded < as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
    TRUE ~ NA_character_
  ))

#reverse order of IMD classfications
recode(df_input$imd_quintile, "1 (most deprived)" = "5 (most deprived)",
       "2" = "4", "3" = "3", "4" = "2", "5 (least deprived)" = "1 (least deprived)")

#recode rurality to 5 levels
df_input <- df_input %>%
  mutate(
    rurality_code = recode(rural_urban_classification, "1" = "1", "2" = "2", 
                           "3" = "3", "4" = "3", "5" = "4", "6" = "4", 
                           "7" = "5", "8" = "5")
  )

#assign rurality classification 
df_input <- df_input %>%
  mutate(
    rurality_classification = ifelse(df_input$rurality_code == "1", "Urban Major Conurbation",
                              ifelse(df_input$rurality_code == "2", "Urban Minor Conurbation",
                              ifelse(df_input$rurality_code == "3", "Urban City and Town",
                              ifelse(df_input$rurality_code == "4", "Rural Town",
                              ifelse(df_input$rurality_code == "5", "Rural Village", "Unknown"))))
    ))

#define household size categories
df_input <- df_input %>%
  mutate(
    household_size_cat = ifelse(df_input$household_size >= 1 & df_input$household_size <= 2, "1",
                         ifelse(df_input$household_size >= 3 & household_size <= 5, "2",
                         ifelse(df_input$household_size >= 6, "3", "Unknown")))
  )

#define seasons for covid
covid_season_min = as.Date("2019-09-01", format = "%Y-%m-%d")

# #create variable for survival time
# df_input$end_time_mild <- study_end_date
# df_input$end_time_severe <- study_end_date
# 
# #calculate follow-up end date for mild outcomes
# df_input <- df_input %>%
#   rowwise() %>%
#   mutate(end_time_mild = case_when(
#     study_start_date >= covid_season_min & covid_primary ~ covid_primary_date,
#     rsv_primary ~ rsv_primary_date,
#     flu_primary ~ flu_primary_date,
#     TRUE ~ study_end_date
#   ))
# 
# #calculate follow-up end date for severe outcomes 
# df_input <- df_input %>%
#   rowwise() %>%
#   mutate(end_time_severe = case_when(
#     study_start_date >= covid_season_min & covid_secondary ~ covid_secondary_date,
#     rsv_secondary ~ rsv_secondary_date,
#     flu_secondary ~ flu_secondary_date,
#     TRUE ~ study_end_date
#   ))

#calculate survival time for both outcomes (in years)
# df_input$time_mild <- as.numeric(difftime(df_input$end_time_mild, 
#                       study_start_date, df_input, "weeks"))/52.25
# df_input$time_severe <- as.numeric(difftime(df_input$end_time_severe, 
#                         study_start_date, df_input, "weeks"))/52.25
df_input$time_rsv_primary <- as.numeric(difftime(df_input$rsv_primary_date, 
                             study_start_date, df_input, "weeks"))/52.25
df_input$time_rsv_secondary <- as.numeric(difftime(df_input$rsv_secondary_date, 
                               study_start_date, df_input, "weeks"))/52.25
df_input$time_flu_primary <- as.numeric(difftime(df_input$flu_primary_date, 
                             study_start_date, df_input, "weeks"))/52.25
df_input$time_flu_secondary <- as.numeric(difftime(df_input$flu_secondary_date, 
                               study_start_date, df_input, "weeks"))/52.25
df_input$time_covid_primary <- as.numeric(difftime(df_input$covid_primary_date, 
                               study_start_date, df_input, "weeks"))/52.25
df_input$time_covid_secondary <- as.numeric(difftime(df_input$covid_secondary_date, 
                                 study_start_date, df_input, "weeks"))/52.25

## create output directories ----
fs::dir_create(here("output", "data"))

#write the new input file
write_feather(df_input, here::here("output", "data", 
  paste0("input_processed_", cohort, "_", year(study_start_date),
         "_", year(study_end_date), "_", codelist_type, 
         "_", investigation_type, ".arrow")))
