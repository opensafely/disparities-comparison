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
  study_start_date <- as.Date("2016-09-01")
  study_end_date <- as.Date("2017-08-31")
  cohort <- "infants"
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

columns_needed <- c("patient_id", "patient_index_date", "patient_end_date")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type, ".arrow")),
  col_select = c(all_of(columns_needed), ((ends_with("_date")) & (contains(c(
    "primary", "secondary", "mortality"))) & (!contains(c(
      "_second_", "_inf_", "patient_")))))
)

#set all NA categories to "Unknown"
df_input <- df_input %>% 
  mutate_if(is.factor, forcats::fct_explicit_na, na_level = "Unknown") %>%
  mutate_if(is.factor, as.character)

## create function to calculate the rolling rates by group for a specified pathogen
calculate_rolling_rates_all <- function(df, pathogen, start = study_start_date,
                                        end = study_end_date,
                                        interval_length = 30,
                                        interval_width = "week") {
  
  if (pathogen == "covid" & study_start_date == covid_season_min) {
    start <- as.Date("2020-03-01")
  }
  
  #create rolling windows
  intervals <- tibble(start_date = seq(start, end - interval_length + 1,
                                       by = interval_width)) %>%
    mutate(end_date = start_date + interval_length - 1) %>%
    filter(end_date <= end)
  
  #get list of intervals
  intervals_list <- interval(intervals$start_date, intervals$end_date)
  
  #calculate total number of patients
  total_patients <- as.numeric(nrow(df))
  
  ##format the data
  df_long <- df %>%
    pivot_longer(cols = starts_with(pathogen) & ends_with("date"),
                 names_to = "event", values_to = "date") %>%
    select(contains("patient"), event, date)
  
  df_long <- df_long %>%
    filter(!is.na(date)) %>%
    mutate(date = as_datetime(date))
  
  #create empty tibble for interval-level data
  df_intervals <- tibble()
  
  for (i in seq_along(intervals_list)) {
    
    interval <- intervals_list[[i]]
    
    #label events as whether they fall in or before the interval
    df_expanded <- df_long %>%
      filter(date <= int_end(interval)) %>%
      mutate(
        in_interval = if_else(date %within% interval, "during", "before")
      ) %>%
      group_by(in_interval, event) %>%
      summarise(
        events_in_interval = n(),
        .groups = "drop"
      ) %>%
      complete(in_interval = c("during", "before"), event = unique(event), 
               fill = list(events_in_interval = 0)) %>%
      pivot_wider(names_from = "in_interval", values_from = "events_in_interval")
    
    if (!"before" %in% names(df_expanded)) {
      df_expanded <- df_expanded %>%
        mutate(before = 0)
    }
    
    df_expanded <- df_expanded %>%
      mutate(
        patients_remaining = total_patients - before
      )
    
    #append these rows to the interval-level data frame
    df_intervals <- bind_rows(df_intervals, df_expanded)
    
  }
  
  ## calculate the rates per event type per group per interval
  
  df_rates <- df_intervals %>%
    group_by(event) %>%
    mutate(
      interval = row_number()
    ) %>%
    group_by(interval, event) %>%
    summarise(
      #time at risk is interval width, then times number of patients
      total_survival_time = (as.numeric(difftime(
        int_end(intervals_list[interval]),
        int_start(intervals_list[interval]))) * patients_remaining),
      total_patients_remaining_midpoint10 = roundmid_any(patients_remaining),
      total_events_midpoint10 = roundmid_any(during),
      rate_1000_py_midpoint10_derived = (
        total_events_midpoint10 / total_survival_time) * 1000,
      rate_midpoint10_derived = (
        total_events_midpoint10 / total_patients_remaining_midpoint10)
    )
  
  #get interval start date 
  df_rates <- df_rates %>%
    mutate(
      interval_start = int_start(intervals_list[[interval]]),
      .after = interval
    )
  
  #save the files
  write_csv(df_rates, here::here("output", "results", "rates", "weekly",
            "all", paste0("rates_over_time_", pathogen, "_", cohort,
            "_", year(study_start_date), "_", year(study_end_date),
            "_", codelist_type, "_", investigation_type, ".csv")))
  
}

## create output directories ----
fs::dir_create(here::here("output", "results", "rates", "weekly", "all"))

##calculate the rates
calculate_rolling_rates_all(df_input, "rsv")
calculate_rolling_rates_all(df_input, "flu")
if (study_start_date >= covid_season_min) {
  calculate_rolling_rates_all(df_input, "covid")
} 
if (codelist_type == "sensitive") {
  calculate_rolling_rates_all(df_input, "overall_resp")
}
