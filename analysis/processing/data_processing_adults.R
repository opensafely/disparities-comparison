library("tidyverse")
library("here")
library("arrow")
library("ggplot2")
library("data.table")

## create output directories ----
fs::dir_create(here("analysis", "processing"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
study_start_date <- as.Date(study_dates$season1_start_date)
study_end_date <- as.Date(study_dates$season1_end_date)

df_input <- read_feather(
  here::here("output", paste0("input_adults", year(study_start_date), "_", year(study_end_date), ".arrow")))

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
df_input <- df_input %>%
  mutate(age_band = case_when(
    df_input$age >= 60 & df_input$age <= 64 ~ "60-64",
    df_input$age >= 65 & df_input$age <= 69 ~ "65-69",
    df_input$age >= 70 & df_input$age <= 74 ~ "70-74",
    df_input$age >= 75 & df_input$age <= 79 ~ "75-79",
    df_input$age >= 80 & df_input$age <= 84 ~ "80-84",
    df_input$age >= 85 & df_input$age <= 89 ~ "85-89",
    df_input$age >= 90 ~ "90+",
    TRUE ~ NA_character_
  ))

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
  paste0("input_processed_adults", year(study_start_date), "_", year(study_end_date), ".arrow")))
