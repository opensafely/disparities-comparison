library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(gt)
library(readr)
library(stringr)
library(gtsummary)
library(ggplot2)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2016-09-01")
  study_end_date <- as.Date("2017-08-31")
  cohort <- "older_adults"
  codelist_type <- "sensitive"
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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type, ".arrow")))

#set all NA categories to "Unknown"
df_input <- df_input %>% mutate_if(is.factor,
                                   forcats::fct_explicit_na,
                                   na_level = "Unknown")

if (study_start_date == as.Date("2020-09-01")) {
  df_input <- df_input %>%
    select(patient_id, patient_index_date, patient_end_date, age_band, sex,
           latest_ethnicity_group, imd_quintile, composition_category,
           rurality_classification, ((ends_with("_date")) & (contains(c("primary",
           "secondary", "mortality"))) & (!contains(c("_second_", "_inf_",
           "patient_")))))
} else {
  df_input <- df_input %>%
    select(patient_id, patient_index_date, patient_end_date, age_band, sex,
           latest_ethnicity_group, imd_quintile, rurality_classification,
           (ends_with("_date")) & (contains(c("primary", "secondary",
            "mortality"))) & (!contains(c("_second_", "_inf_", "patient_"))))
}

#add a column to df_long to indicate the interval based on which interval the outcome date falls in
df_wide <- df_input%>%
  mutate(
    across(ends_with("_date"), ~ if_else(is.na(.), 0, 1), .names = "{.col}_event_flag")
  ) 

# Define the function to calculate 30-day rolling rates per 1000 person-years
calculate_rolling_rates <- function(df, outcomes, characteristics) {
  
  bind_rows(lapply(outcomes, function(outcome) {
    
    # Define the relevant columns for this outcome and characteristic
    outcome_date_col <- paste0(outcome, "_date")
    event_flag_col <- paste0(outcome, "_date_event_flag")
    
    # Create 30-day rolling intervals
    interval_length <- 30
    intervals <- tibble(start_date = seq(study_start_date, study_end_date - interval_length + 1, by = "day")) %>%
      mutate(end_date = start_date + interval_length - 1) %>%
      filter(end_date <= study_end_date)
    
    # Calculate rolling rates over time by joining with interval table
    rates <- df %>%
      cross_join(intervals) %>%
      filter(
        patient_index_date <= end_date & 
          patient_end_date >= start_date
      ) %>%
      mutate(
        # Calculate survival time within the interval for each patient
        interval_start = pmax(start_date, patient_index_date),
        interval_end = pmin(end_date, patient_end_date),
        survival_time = as.numeric(time_length(difftime(interval_end, interval_start), "years")),
        event_flag = if_else(
          !is.na(.data[[outcome_date_col]]) & 
            .data[[outcome_date_col]] >= interval_start & 
            .data[[outcome_date_col]] <= interval_end, 
          1, 0
        )
      )
    
    bind_rows(lapply(characteristics, function(char) {
      
      rates <- rates %>% 
        group_by(interval = start_date, group = as.character(.data[[char]])) %>%
        summarise(
          events_midpoint6 = roundmid_any(sum(event_flag, na.rm = TRUE)),
          total_person_years = sum(survival_time, na.rm = TRUE),
          num_people = n_distinct(patient_id), # Count unique individuals contributing to the interval
          .groups = 'drop'
        ) %>%
        right_join(intervals %>% select(start_date), by = c("interval" = "start_date")) %>%
        mutate(
          events_midpoint6 = replace_na(events_midpoint6, 0),
          total_person_years = replace_na(total_person_years, 0),
          num_people = replace_na(num_people, 0), # Fill missing counts with 0
          rate_midpoint6_derived = ifelse(total_person_years > 0,
                                   (events_midpoint6 / total_person_years) * 1000,
                                   NA_real_)
        ) %>%
        mutate(outcome = outcome, characteristic = char) %>%
        select(interval, outcome, characteristic, group, events_midpoint6,
               total_person_years, num_people, rate_midpoint6_derived)
      
      return(rates)
    }))
  }))
}

#define characteristics
characteristics <- c("age_band", "sex", "latest_ethnicity_group", "imd_quintile",
                     "rurality_classification")
if (study_start_date == as.Date("2020-09-01")) {
  characteristics <- c(characteristics, "composition_category")
}

#define outcomes 
outcomes <- case_when(
  study_start_date >= covid_season_min & codelist_type == "sensitive" ~
    list(c("rsv_primary", "flu_primary", "covid_primary", "overall_resp_primary",
           "rsv_secondary", "flu_secondary", "covid_secondary",
           "overall_resp_secondary", "rsv_mortality", "flu_mortality",
           "covid_mortality", "overall_resp_mortality", "all_cause_mortality")),
  study_start_date >= covid_season_min ~
    list(c("rsv_primary", "flu_primary", "covid_primary", "rsv_secondary",
           "flu_secondary", "covid_secondary", "rsv_mortality", "flu_mortality",
           "covid_mortality", "all_cause_mortality")),
  codelist_type == "sensitive" ~ list(c("rsv_primary", "flu_primary",
           "overall_resp_primary", "rsv_secondary", "flu_secondary",
           "overall_resp_secondary", "rsv_mortality", "flu_mortality",
           "overall_resp_mortality", "all_cause_mortality")),
  TRUE ~ list(c("rsv_primary", "flu_primary", "rsv_secondary", "flu_secondary",
                "rsv_mortality", "flu_mortality", "all_cause_mortality"))
)[[1]]

# Calculate rolling rates for each outcome and characteristic
rates_over_time <- calculate_rolling_rates(df_wide, outcomes, characteristics)

# Updated plotting function to use 30-day rolling rates without smoothing
plot_rolling_rates <- function(df, outcome, characteristic) {
  df %>%
    filter(outcome == !!outcome, characteristic == !!characteristic) %>%
    ggplot(aes(x = interval, y = rate_midpoint6_derived, group = group, col = group)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Rate per 1000 person-years\n(Midpoint 6 Derived)", col = "Group")
}

# Define the updated function to create and save plots
create_rolling_plots <- function(df, outcomes, characteristics) {
  plots <- expand.grid(outcome = outcomes, characteristic = characteristics) %>%
    pmap(function(outcome, characteristic) {
      plot <- plot_rolling_rates(df, outcome, characteristic) +
        ggtitle(paste("30-Day Rolling Rate of", str_to_title(gsub("_", " ",
                outcome)), "by", str_to_title(gsub("_", " ", characteristic))))
      print(plot)  # Print or save each plot
      fs::dir_create(here("output", "collated", "plots", paste0(characteristic)))
      ggsave(paste0(here("output", "collated", "plots", paste0(characteristic)),
             "/", "rolling_rates_", year(study_start_date), "_",
             year(study_end_date), "_", cohort, "_", codelist_type, "_",
             investigation_type, "_", characteristic, ".png"), plot)
    })
  return(plots)
}

#run function to create and save plots
create_rolling_plots(rates_over_time, outcomes, characteristics)
