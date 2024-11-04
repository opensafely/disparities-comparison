library(tidyverse)
library(here)
library(arrow)
library(data.table)
library(lubridate)
library(magrittr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2016-09-01")
  study_end_date <- as.Date("2017-08-31")
} else {
  study_start_date <- study_dates[[args[[1]]]]
  study_end_date <- study_dates[[args[[2]]]]
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_household_", year(study_start_date), 
             "_", year(study_end_date), ".arrow")))

#some data manipulation first
df_input <- df_input %>% 
  mutate(
    #recode rurality to 5 levels
    rurality_code = recode(rural_urban_classification, "1" = "1", "2" = "2", 
                           "3" = "3", "4" = "3", "5" = "4", "6" = "4", 
                           "7" = "5", "8" = "5", .missing = NA_character_),
    #assign rurality classification
    rurality_classification = factor(case_when(
      rurality_code == "1" ~ "Urban Major Conurbation",
      rurality_code == "2" ~ "Urban Minor Conurbation",
      rurality_code == "3" ~ "Urban City and Town",
      rurality_code == "4" ~ "Rural Town and Fringe",
      rurality_code == "5" ~ "Rural Village and Dispersed",
      TRUE ~ NA_character_)),
    #assign generation
    generation = factor(case_when(
      age < 18 ~ "Child", 
      age >= 17 & age < 30 ~ "Young Adult",
      age >= 30 & age < 66 ~ "Adult",
      age >= 66 ~ "Older Adult"
    ))
  )

#calculate number of generations in household 
df_input <- df_input %>%
  group_by(household_pseudo_id) %>%
  mutate(
    num_generations = n_distinct(generation)
  ) %>%
  ungroup() %>%
  group_by(household_pseudo_id, generation) %>%
  mutate(
    composition_category = factor(case_when(
      household_size == 1 ~ "Living Alone",
      num_generations == 1 ~ "Multiple of the Same Generation",
      num_generations == 2 ~ "One Other Generation",
      num_generations == 3 ~ "Two Other Generations",
      num_generations == 4 ~ "Three Other Generations",
      TRUE ~ NA_character_
    ))
  ) %>%
  ungroup()

#filter out children living alone
df_filtered <- df_input %>%
  filter(!(household_size == 1 & age < 18))

## create output directories ----
fs::dir_create(here("output", "data"))

#write the new input file
write_feather(df_filtered, here::here("output", "data", 
  paste0("input_household_processed_", year(study_start_date),
        "_", year(study_end_date), ".arrow")))
