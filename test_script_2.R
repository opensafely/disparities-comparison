library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(readr)
library(stringr)
library(rlang)

## create output directories ----
fs::dir_create(here::here("analysis"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2019-09-01")
  study_end_date <- as.Date("2020-08-31")
  cohort <- "older_adults"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min <- as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min <- as.Date("2021-09-01", "%Y-%m-%d")

source(here::here("analysis", "functions", "redaction.R"))

columns_needed <- c("patient_id", "patient_index_date", "patient_end_date",
                    "age_band", "sex", "latest_ethnicity_group",
                    "imd_quintile", "rurality_classification")
if (study_start_date == as.Date("2020-09-01")) {
  columns_needed <- c(columns_needed, "composition_category")
}


df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
                                      year(study_start_date), "_", year(study_end_date), "_",
                                      codelist_type, "_", investigation_type, ".arrow")),
  col_select = c(all_of(columns_needed), ((ends_with("_date")) & (contains(c(
    "primary", "secondary", "mortality"))) & (!contains(c(
      "_second_", "_inf_", "patient_")))))
)

#set all NA categories to "Unknown"
df_input <- df_input %>% mutate_if(is.factor,
                                   forcats::fct_explicit_na,
                                   na_level = "Unknown")

# Create 30-day rolling intervals
interval_length <- 30
intervals <- tibble(start_date = seq(study_start_date,
                                     study_end_date - interval_length + 1,
                                     by = "week")) %>%
  mutate(end_date = start_date + interval_length - 1) %>%
  filter(end_date <= study_end_date)
intervals <- intervals %>%
  mutate(
    interval = row_number(),
    start_date = start_date,
    end_date = end_date
  )
intervals_list <- interval(intervals$start_date, intervals$end_date)
rm(intervals)

#calculate total number of patients
total_patients <- as.numeric(nrow(df_input))

#create a function to calculate rolling rates
df_format <- function(df, pathogen, characteristic) {
  
  columns_needed <- c("patient_id", "patient_index_date", "patient_end_date")
  
  df <- df %>%
    select(all_of(columns_needed), all_of(characteristic),
           starts_with(pathogen) & ends_with("_date"))
  
  df_long <- df %>%
    pivot_longer(cols = starts_with(pathogen) & ends_with("date"),
                 names_to = "event", values_to = "date") %>%
    select(contains("patient"), group = all_of(characteristic), event, date)
  
  df_long <- df_long %>%
    filter(!is.na(date)) %>%
    mutate(date = as_datetime(date))
  
  return(df_long)
  
}

df_long <- df_format(df_input, "rsv", "sex")

df_intervals <- tibble()

for (i in seq_along(intervals_list)) {
  
  interval <- intervals_list[[i]]
  #num <- i
  
  # Filter rows where the date falls within the interval
  df_expanded <- df_long %>%
    filter(date <= int_end(interval)) %>%
    mutate(
      in_interval = if_else(date %within% interval, "during", "before")
    ) %>%
    group_by(in_interval, event, group) %>%
    summarise(
      events_in_interval = n(),
      .groups = "drop"
    ) %>%
    complete(in_interval = c("during", "before"), event = unique(event), 
             group = unique(group), fill = list(events_in_interval = 0)) %>%
    pivot_wider(names_from = "in_interval", values_from = "events_in_interval") %>%
    mutate(
      patients_remaining = total_patients - (before + during)
    )
  
  #Append these rows to the expanded data frame
  df_intervals <- bind_rows(df_intervals, df_expanded)
  
}

rm(df_long, df_input, df_expanded)

df_rates <- df_intervals %>%
  group_by(event, group) %>%
  mutate(
    interval = row_number()
  ) %>%
  group_by(interval, event, group) %>%
  summarise(
    total_survival_time = (as.numeric(difftime(int_end(intervals_list[interval]),
                                      study_start_date)) * patients_remaining),
    total_events_midpoint10 = roundmid_any(during),
    rate_1000_py_midpoint10_derived = (total_events_midpoint10 / total_survival_time) * 1000
  )
