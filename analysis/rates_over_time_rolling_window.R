library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(readr)
library(stringr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
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

roundmid_any <- function(x, to=10){
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

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
df_format <- function(df, filters, characteristic) {
  
  columns_needed <- c("patient_id", "patient_index_date", "patient_end_date")
  
  df <- df %>%
    select(all_of(columns_needed), all_of(characteristic), all_of(filters)) %>%
    mutate(across(ends_with("_date"), ~ as.integer(!is.na(.)), 
                  .names = "{.col}_event_flag")) %>%
    mutate(across(ends_with("_date"), ~ as.numeric(.)))
  
  return(df)
  
}

# Create 30-day rolling intervals
interval_length <- 30
intervals <- tibble(start_date = seq(study_start_date,
                    study_end_date - interval_length + 1, by = "week")) %>%
  mutate(end_date = start_date + interval_length - 1) %>%
  filter(end_date <= study_end_date)
intervals <- intervals %>%
  mutate(
    start_date = as.numeric(start_date),
    end_date = as.numeric(end_date)
  )

# Define the function to calculate 30-day rolling rates per 1000 person-years
calculate_rolling_rates <- function(df, outcomes, group) {
  
  bind_rows(lapply(outcomes, function(outcome) {
    
    # Define the relevant columns for this outcome and characteristic
    outcome_date_col <- paste0(outcome, "_date")
    event_flag_col <- paste0(outcome, "_date_event_flag")
    
    # Calculate rolling rates over time by joining with interval table
    df <- df %>%
      full_join(intervals, by = character()) %>%
      filter(
        patient_index_date <= end_date & 
          patient_end_date >= start_date
      ) %>%
      mutate(
        # Calculate survival time within the interval for each patient
        interval_start = pmax(start_date, patient_index_date),
        interval_end = pmin(end_date, patient_end_date),
        survival_time = (interval_end - interval_start) / 365.25,
        event_flag = if_else(
          !is.na(.data[[outcome_date_col]]) & 
            .data[[outcome_date_col]] >= interval_start & 
            .data[[outcome_date_col]] <= interval_end, 
          1, 0
        )
      )
    
    df <- df %>% 
      group_by(interval_beginning = start_date, group = as.character(.data[[group]])) %>%
      summarise(
        events_midpoint10 = roundmid_any(sum(event_flag, na.rm = TRUE)),
        total_person_years = sum(survival_time, na.rm = TRUE),
        num_people_midpoint10 = roundmid_any(n_distinct(patient_id)), # Count unique individuals contributing to the interval
        .groups = 'drop'
      ) %>%
      right_join(intervals %>% select(start_date),
                 by = c("interval_beginning" = "start_date")) %>%
      mutate(
        events_midpoint10 = replace_na(events_midpoint10, 0),
        total_person_years = replace_na(total_person_years, 0),
        num_people_midpoint10 = replace_na(num_people_midpoint10, 0), # Fill missing counts with 0
        rate_pys_midpoint10_derived = ifelse(total_person_years > 0,
                                      (events_midpoint10/total_person_years)*1000,
                                      NA_real_),
        rate_midpoint10_derived = ifelse(num_people_midpoint10 > 0,
                                  (events_midpoint10/num_people_midpoint10)*1000,
                                  NA_real_)
      ) %>%
      mutate(outcome = outcome, characteristic = char) %>%
      select(interval_beginning, outcome, characteristic, group, events_midpoint10,
             total_person_years, num_people_midpoint10, rate_midpoint10_derived)
      
    return(df)
      
  }))
  
}

#define characteristics
characteristics <- c("age_band", "sex", "latest_ethnicity_group", "imd_quintile",
                     "rurality_classification")
if (study_start_date == as.Date("2020-09-01")) {
  characteristics <- c(characteristics, "composition_category")
}

#define outcomes 
outcomes_rsv <- c("rsv_primary", "rsv_secondary", "rsv_mortality")
outcomes_flu <- c("flu_primary", "flu_secondary", "flu_mortality")
if (study_start_date >= covid_season_min) {
  outcomes_covid <- c("covid_primary", "covid_secondary", "covid_mortality")
}
if (codelist_type == "sensitive") {
  outcomes_overall_resp <- c("overall_resp_primary", "overall_resp_secondary",
                             "overall_resp_mortality")
}
outcomes_all_cause <- c("all_cause_mortality")

#define pathogens
pathogens <- case_when(
  study_start_date >= covid_season_min & codelist_type == "sensitive" ~ list(c(
    "rsv", "flu", "covid", "overall_resp", "all_cause")),
  study_start_date >= covid_season_min ~ list(c("rsv", "flu", "covid",
    "all_cause")),
  codelist_type == "sensitive" ~ list(c("rsv", "flu", "overall_resp",
    "all_cause")),
  TRUE ~ list(c("rsv", "flu", "all_cause"))
)[[1]]

#create empty dataframe to store the rates over time
for (i in 1:length(pathogens)) {  
  
  for (j in 1:length(characteristics)) {
 
    pathogen <- pathogens[i]
    char <- characteristics[j]
    assign(paste0("rates_over_time_", pathogen, "_", char), tibble())

  }
  
}

## create output directories ----
fs::dir_create(here("output", "results", "rates", "weekly"))

#first look at RSV related outcomes 
for (i in 1:length(characteristics)) {
  
  char <- characteristics[i]
  df_rsv <- df_format(df_input, all_of(contains("rsv")), char)
  
  # Calculate rolling rates for each outcome and characteristic
  rates <- calculate_rolling_rates(df_rsv, outcomes_rsv, char)
  
  #remove unnecessary data to save memory
  rm(df_rsv)
  
  # Combine the new rates data with the existing rates dataframe for each characteristic
  assign(paste0("rates_over_time_rsv_", char), rbind(get(paste0(
    "rates_over_time_rsv_", char)), rates)) 
  
  #remove unnecessary data to save memory
  rm(rates)
 
  #write the rates to a feather file
  write_csv(get(paste0("rates_over_time_rsv_", char)),
            path = here("output", "results", "rates", "weekly",
                   paste0("rates_over_time_rolling_rsv_", char, "_",
                   cohort, "_", year(study_start_date), "_",
                   year(study_end_date), "_", codelist_type, "_",
                   investigation_type, ".csv")))
  
  rm(list = paste0("rates_over_time_rsv_", char))
  
}

#next look at flu related outcomes 
for (i in 1:length(characteristics)) {
  
  char <- characteristics[i]
  df_flu <- df_format(df_input, all_of(contains("flu")), char)
  
  # Calculate rolling rates for each outcome and characteristic
  rates <- calculate_rolling_rates(df_flu, outcomes_flu, char)
  
  #remove unnecessary data to save memory
  rm(df_flu)
  
  # Combine the new rates data with the existing rates dataframe for each characteristic
  assign(paste0("rates_over_time_flu_", char), rbind(get(paste0(
    "rates_over_time_flu_", char)), rates)) 
  
  #remove unnecessary data to save memory
  rm(rates)
  
  #write the rates to a feather file
  write_csv(get(paste0("rates_over_time_flu_", char)),
            path = here("output", "results", "rates", "weekly",
                   paste0("rates_over_time_rolling_flu_", char, "_",
                   cohort, "_", year(study_start_date), "_",
                   year(study_end_date), "_", codelist_type, "_",
                   investigation_type, ".csv")))
  
  rm(list = paste0("rates_over_time_flu_", char))
  
}

#now covid outcomes
if (study_start_date >= covid_season_min) {
  
  for (i in 1:length(characteristics)) {
    
    char <- characteristics[i]
    df_covid <- df_format(df_input, all_of(contains("covid")), char)
    
    if (study_start_date == covid_season_min) {
      df_covid <- df_covid %>%
        filter(rowSums(across(contains("covid"), ~ . >= as.numeric(as.Date("2020-03-01")))) > 0)
    }
    
    rates <- calculate_rolling_rates(df_covid, outcomes_covid, char)
    
    #remove unnecessary data to save memory
    rm(df_covid)
    
    # Combine the new rates data with the existing rates dataframe for each characteristic
    assign(paste0("rates_over_time_covid_", char), rbind(get(paste0(
      "rates_over_time_covid_", char)), rates))
    
    #remove unnecessary data to save memory
    rm(rates)
    
    #write the rates to a feather file
    write_csv(get(paste0("rates_over_time_covid_", char)),
              path = here("output", "results", "rates", "weekly",
                     paste0("rates_over_time_rolling_covid_", char, "_",
                     cohort, "_", year(study_start_date), "_",
                     year(study_end_date), "_", codelist_type, "_",
                     investigation_type, ".csv")))
    
    rm(list = paste0("rates_over_time_covid_", char))
    
  }
  
}

#and overall outcomes 
if (codelist_type == "sensitive") {
  
  for (i in 1:length(characteristics)) {
    
    char <- characteristics[i]
    df_overall_resp <- df_format(df_input, all_of(contains("resp")), char)
    
    # Calculate rolling rates for each outcome and characteristic
    rates <- calculate_rolling_rates(df_overall_resp, outcomes_overall_resp, char)
    
    #remove unnecessary data to save memory
    rm(df_overall_resp)
    
    # Combine the new rates data with the existing rates dataframe for each characteristic
    assign(paste0("rates_over_time_overall_resp_", char), rbind(get(paste0(
      "rates_over_time_overall_resp_", char)), rates)) 
    
    #remove unnecessary data to save memory
    rm(rates)
    
    #write the rates to a feather file
    write_csv(get(paste0("rates_over_time_overall_resp_", char)),
              path = here("output", "results", "rates", "weekly",
                     paste0("rates_over_time_rolling_overall_resp_", char, "_",
                     cohort, "_", year(study_start_date), "_",
                     year(study_end_date), "_", codelist_type, "_",
                     investigation_type, ".csv")))
    
    rm(list = paste0("rates_over_time_overall_resp_", char))
    
  }
  
}

#finally all cause mortality
for (i in 1:length(characteristics)) {
  
  char <- characteristics[i]
  df_all_cause <- df_format(df_input, all_of(contains("all_cause")), char)
  
  # Calculate rolling rates for each outcome and characteristic
  rates <- calculate_rolling_rates(df_all_cause, outcomes_all_cause, char)
  
  #remove unnecessary data to save memory
  rm(df_all_cause)
  
  # Combine the new rates data with the existing rates dataframe for each characteristic
  assign(paste0("rates_over_time_all_cause_", char), rbind(get(paste0(
    "rates_over_time_all_cause_", char)), rates)) 
  
  #remove unnecessary data to save memory
  rm(rates)
  
  #write the rates to a feather file
  write_csv(get(paste0("rates_over_time_all_cause_", char)),
            path = here("output", "results", "rates", "weekly",
                   paste0("rates_over_time_rolling_all_cause_", char, "_",
                   cohort, "_", year(study_start_date), "_",
                   year(study_end_date), "_", codelist_type, "_",
                   investigation_type, ".csv")))
  
  rm(list = paste0("rates_over_time_all_cause_", char))
  
}
