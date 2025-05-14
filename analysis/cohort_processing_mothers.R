library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here::here("analysis"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2022-09-01")
  study_end_date <- as.Date("2023-08-31")
} else {
  study_start_date <- study_dates[[args[[1]]]]
  study_end_date <- study_dates[[args[[2]]]]

}

## create output directories ----
fs::dir_create(here::here("output", "flow_chart"))

#import data
df_input <- read_feather(
  here::here("output", "flow_chart", paste0("infants_subgroup",
  year(study_start_date), "_", year(study_end_date), "_flow_chart.arrow")))

#select the variables of interest
df_input <- df_input %>%
  arrange(birth_date) %>%
  select(patient_id = mother_id, index_date = birth_date) %>%
  group_by(patient_id) %>%
  slice_head(n = 1) %>%
  ungroup()

## create output directories ----
fs::dir_create(here::here("output", "flow_chart"))

#write the new input file
write_feather(df_input, here::here("output", "flow_chart",
              paste0("cohort_mothers_processed_", year(study_start_date),
              "_", year(study_end_date), ".arrow")))
