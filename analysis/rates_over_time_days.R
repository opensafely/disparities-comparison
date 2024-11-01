library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(gt)
library(readr)
library(stringr)
library(gtsummary)

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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
                                      year(study_start_date), "_", year(study_end_date), "_",
                                      codelist_type, "_", investigation_type, ".arrow")))

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

# Define the function to calculate rates per 1000 person-years
calculate_rates <- function(df, outcomes, characteristics) {
  bind_rows(lapply(outcomes, function(outcome) {
    bind_rows(lapply(characteristics, function(char) {
      
      # Define the relevant columns for this outcome and characteristic
      outcome_date_col <- paste0(outcome, "_date")
      event_flag_col <- paste0(outcome, "_date_event_flag")
      
      # Create intervals based on study dates and interval length
      interval_length <- "day"
      intervals <- tibble(start_date = seq(study_start_date, study_end_date, by = interval_length)) %>%
        mutate(end_date = lead(start_date, default = study_end_date)) %>%
        filter(start_date < study_end_date)
      
      # Calculate rates over time by joining with interval table
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
        ) %>%
        group_by(interval = start_date, group = as.character(.data[[char]])) %>%
        summarise(
          events = sum(event_flag, na.rm = TRUE),
          total_person_years = sum(survival_time, na.rm = TRUE),
          num_people = n_distinct(patient_id), # Count unique individuals contributing to the interval
          .groups = 'drop'
        ) %>%
        right_join(intervals %>% select(start_date), by = c("interval" = "start_date")) %>%
        mutate(
          events = replace_na(events, 0),
          total_person_years = replace_na(total_person_years, 0),
          num_people = replace_na(num_people, 0), # Fill missing counts with 0
          rate_per_1000_pys = ifelse(total_person_years > 0, (events / total_person_years) * 1000, NA_real_)
        ) %>%
        mutate(outcome = outcome, characteristic = char) %>%
        select(interval, outcome, characteristic, group, events, total_person_years, num_people, rate_per_1000_pys)
      
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

#calculate the rates for all outcomes
rates_over_time <- calculate_rates(df_wide, outcomes, characteristics)

#plot for different characteristics
plot_rates <- function(df, outcome, characteristic) {
  df %>%
    filter(outcome == !!outcome, characteristic == !!characteristic, !is.na(group)) %>%
    ggplot(aes(x = interval, y = rate_per_1000_pys, group = group, col = group)) +
    geom_smooth(method = "loess", span = 0.4, aes(group = group), se = FALSE) +
    theme_minimal() +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Month", y = "Rate per 1000 person-years", col = "Group")
}

#helper function to create and save plots
create_plots <- function(df, outcomes, characteristics) {
  plots <- expand.grid(outcome = outcomes, characteristic = characteristics) %>%
    pmap(function(outcome, characteristic) {
      plot <- plot_rates(df, outcome, characteristic) +
        ggtitle(paste("Rate of", str_to_title(gsub("_", " ", outcome)), "by",
                      str_to_title(gsub("_", " ", characteristic))))
      print(plot)  # Print or save each plot
      ## create output directories ----
      fs::dir_create(here("output", "collated", "plots", paste0(characteristic)))
      ggsave(paste0(here("output", "collated", "plots"), "/", "crude_rates_",
                    year(study_start_date), "_", year(study_end_date),
                    "_", cohort, "_", codelist_type, "_", investigation_type,
                    "_", characteristic, ".png"), plot)
    })
  return(plots)
}

#run function to create and save plots
create_plots(rates_over_time, outcomes, characteristics)
