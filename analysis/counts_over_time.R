library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(readr)
library(stringr)
library(rlang)
library(zoo)

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

## create function to calculate the rolling rates by group for a specified pathogen
get_counts_over_time <- function(df, pathogen, interval_length) {
  
  if (pathogen == "covid" & study_start_date == covid_season_min) {
    start <- as.Date("2020-03-01")
  }
  
  int <- if_else(interval_length == "month", "monthly", "weekly")
  
  ##format the data
  df_long <- df %>%
    pivot_longer(cols = starts_with(pathogen) & ends_with("date"),
                 names_to = "event", values_to = "date") %>%
    select(contains("patient"), event, date)
  
  df_long <- df_long %>%
    filter(!is.na(date)) %>%
    mutate(date = as_datetime(date))
  
  df_long <- df_long %>%
    group_by(date, event) %>%
    summarise(
      n = n()
    ) %>%
    mutate(
      month = as.Date(as.yearmon(date)),
      week = floor_date(date, "weeks", week_start = 1)
    )
  
  df_long_int <- df_long %>%
    group_by(!!ensym(interval_length), event) %>%
    summarise(
      n = roundmid_any(sum(n, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    complete(!!ensym(interval_length), event, fill = list(n = 0)) %>%
    arrange(!!ensym(interval_length), event)
  
  #save the files
  write_csv(df_long_int, here::here("output", "results", "counts",
            paste0("counts_over_time_all_", int, "_", pathogen, "_",
            cohort, "_", year(study_start_date), "_", year(study_end_date),
            "_", codelist_type, "_", investigation_type, ".csv")))
  
}

## create output directories ----
fs::dir_create(here::here("output", "results", "counts"))

##get the counts

#monthly
get_counts_over_time(df_input, "rsv", "month")
get_counts_over_time(df_input, "flu", "month")
if (study_start_date >= covid_season_min) {
  get_counts_over_time(df_input, "covid", "month")
} 
if (codelist_type == "sensitive") {
  get_counts_over_time(df_input, "overall_resp", "month")
}

#weekly
get_counts_over_time(df_input, "rsv", "week")
get_counts_over_time(df_input, "flu", "week")
if (study_start_date >= covid_season_min) {
  get_counts_over_time(df_input, "covid", "week")
} 
if (codelist_type == "sensitive") {
  get_counts_over_time(df_input, "overall_resp", "week")
}
