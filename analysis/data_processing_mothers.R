library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(lubridate)
library(magrittr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2022-09-01")
  study_end_date <- as.Date("2023-08-31")
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[1]]]]
  study_end_date <- study_dates[[args[[2]]]]
  codelist_type <- args[[3]]
  investigation_type <- args[[4]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

#import data
df_input <- read_feather(
  here::here("output", "data", paste0("input_infants_subgroup_", 
                                      year(study_start_date), "_", 
                                      year(study_end_date), "_", codelist_type,
                                      "_", investigation_type,".arrow")))

#select the variables of interest
df_input <- df_input %>%
  arrange(birth_date) %>%
  select(patient_id = mother_id, index_date = birth_date) %>%
  group_by(patient_id) %>%
  slice_head(n = 1) %>%
  ungroup()

## create output directories ----
fs::dir_create(here("output", "data"))

#write the new input file
write_feather(df_input, here::here("output", "data", paste0("input_mothers_processed_", 
                                   year(study_start_date), "_", year(study_end_date),
                                   "_", codelist_type, "_", investigation_type,
                                   ".arrow")))
