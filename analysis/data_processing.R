library("tidyverse")
library("here")
library("arrow")
library("ggplot2")
library("data.table")
library("lubridate")

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
study_start_date <- as.Date(as.numeric(study_dates[args[2]]), format = "%Y-%m-%d", origin = "1970-01-01")
study_end_date <- as.Date(as.numeric(study_dates[args[3]]), format = "%Y-%m-%d", origin = "1970-01-01")

df_input <- read_feather(
  here::here("output", paste0("input_", args[[1]], "_", year(study_start_date),
                              "_", year(study_end_date), ".arrow")))

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
if(args[[1]] == "older_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      df_input$age >= 65 & df_input$age <= 74 ~ "65-74y",
      df_input$age >= 75 & df_input$age <= 89 ~ "75-89y",
      df_input$age >= 90 ~ "90+y",
      TRUE ~ NA_character_
    ))
} else if(args[[1]] == "adults") {
df_input <- df_input %>%
  mutate(age_band = case_when(
    df_input$age >= 18 & df_input$age <= 39 ~ "18-29y",
    df_input$age >= 40 & df_input$age <= 64 ~ "40-64y",
    TRUE ~ NA_character_
  ))
} else if(args[[1]] == "children_adults") {
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
    df_input$imd_rounded >= 0 & df_input$imd_rounded < as.integer(32800 * 1 / 5) ~ "1",
    df_input$imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
    df_input$imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
    df_input$imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
    df_input$imd_rounded < as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
    TRUE ~ NA_character_
  ))

#write the new input file
write_feather(df_input, here::here("output", 
  paste0("input_processed_", args[[1]], "_", year(study_start_date),
         "_", year(study_end_date), ".arrow")))
