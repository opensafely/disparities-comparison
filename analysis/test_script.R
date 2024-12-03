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

#create function which filters the data and then reshapes it 
df_format <- function(df, pathogen, characteristic) {
  
  columns_needed <- c("patient_id", "patient_index_date", "patient_end_date")
  
  df <- df %>%
    select(all_of(columns_needed), all_of(characteristic),
           starts_with(pathogen) & ends_with("_date")) #%>%
    # mutate(across(ends_with("_date") & !contains("patient"),
    #        ~ as.integer(!is.na(.)), .names = "{.col}_event_flag")) %>%
    # mutate(across(ends_with("_date"), ~ as.numeric(.)))
  
  return(df)
  
}

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

#create a function to calculate rolling rates 
calculate_rolling_rates <- function(df, pathogen, group) {
  
  df_long <- df %>%
    pivot_longer(cols = starts_with(pathogen) & ends_with("date"),
                 names_to = "event", values_to = "date") %>%
    select(contains("patient"), all_of(group), event, date)
  
  df_long <- df_long %>%
    filter(!is.na(date)) %>%
    mutate(date = as_datetime(date))

  return(df_long)
  
}

#define characteristics
characteristics <- c("age_band", "sex", "latest_ethnicity_group", "imd_quintile",
                     "rurality_classification")
if (study_start_date == as.Date("2020-09-01")) {
  characteristics <- c(characteristics, "composition_category")
}

df <- df_format(df_input, "rsv", characteristics)
df_long <- calculate_rolling_rates(df, "rsv", "sex")

df_long <- df_long %>%
  mutate(
    interval_num = list(integer(0))
  )

# Initialize an empty data frame to store results
df_long_expanded <- tibble()

# Iterate through each interval to expand rows for each patient and interval
for (i in seq_along(intervals_list)) {
  interval <- intervals_list[[i]]
  num <- i
  
  # Filter rows where the date falls within the interval
  matching_rows <- df_long %>%
    filter(date %within% interval | is.na(date)) %>% # Include patients without events
    mutate(interval_num = num, interval_start = int_start(interval),
           interval_end = int_end(interval)) # Add interval details
  
  # Append these rows to the expanded data frame
  df_long_expanded <- bind_rows(df_long_expanded, matching_rows)
}

# # Join interval details to the expanded dataset
# df_long_expanded <- df_long_expanded %>%
#   left_join(intervals %>% rename(interval_start = start_date, interval_end = end_date),
#             by = c("interval_num" = "interval"))

# Add total survival time for each patient
df_long_expanded <- df_long_expanded %>%
  group_by(patient_id) %>%
  mutate(
    total_survival_time = as.numeric(difftime(interval_end, study_start_date, units = "days")) / 365.25
  ) %>%
  ungroup()

# Deduct survival time for intervals with outcomes
df_long_expanded <- df_long_expanded %>%
  mutate(
    interval_survival_time = if_else(
      is.na(date) | date > interval_end, # No outcome in this interval
      as.numeric(difftime(interval_end, study_start_date, units = "days")) / 365.25,
      as.numeric(difftime(date, study_start_date, unit = "days")) / 365.25 # Event occurred, so survival time for this interval is 0
    )
  )

# Summarize by outcome type and interval
df_rates <- df_long_expanded %>%
  group_by(interval_num, interval_start, interval_end, event) %>%
  summarize(
    total_survival_time = sum(interval_survival_time, na.rm = TRUE), # Remaining survival time
    total_events = sum(!is.na(date) & date %within% interval(interval_start, interval_end)), # Count of events
    rate_per_1000_py = (total_events / total_survival_time) * 1000, # Rate per 1000 person-years
    .groups = "drop"
  )
