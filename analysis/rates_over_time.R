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

#format data from wide to long 
if (study_start_date == as.Date("2020-09-01")) {
  df_long_date <- df_input %>%
    pivot_longer(
      cols = ends_with("_date") & contains(c("primary", "secondary", "mortality")) &
        !contains(c("_second_", "_inf_", "patient_")),
      names_to = "outcome_type",
      values_to = "outcome_date",
      values_drop_na = FALSE
    ) %>%
    select(patient_id, patient_index_date, patient_end_date, age_band, sex,
           latest_ethnicity_group, imd_quintile, composition_category,
           rurality_classification, outcome_type, outcome_date)
} else {
  df_long_date <- df_input %>%
    pivot_longer(
      cols = ends_with("_date") & contains(c("primary", "secondary", "mortality")) &
        !contains(c("_second_", "_inf_", "patient_")),
      names_to = "outcome_type",
      values_to = "outcome_date",
      values_drop_na = FALSE
    ) %>%
    select(patient_id, patient_index_date, patient_end_date, age_band, sex,
           latest_ethnicity_group, imd_quintile, rurality_classification,
           outcome_type, outcome_date)
}

#create function to clean up outcome_type column 
clean_outcome_type <- function(df) {
  df %>%
    mutate(outcome_type = str_remove(outcome_type, "_date")) %>%
    mutate(outcome_type = str_remove(outcome_type, "time_")) %>%
    group_by(patient_id) %>%
    distinct()
}

#clean
df_long_date <- clean_outcome_type(df_long_date)

#define month intervals to split the data into
interval_length <- "month"

# Generate the fixed sequence of interval start dates
intervals <- as.Date(seq(study_start_date, study_end_date, by = interval_length))
intervals <- tibble(start_date = intervals, end_date = intervals %m+% months(1) - 1)

#add a column to df_long to indicate the interval based on which interval the outcome date falls in
df_long <- df_long_date %>%
  mutate(
    event_flag = if_else(is.na(outcome_date), 0, 1)
  ) %>%
  cross_join(intervals) %>%
  filter((end_date <= outcome_date|(outcome_date >= start_date &
         outcome_date <= end_date)) & event_flag == 1 | event_flag == 0) %>%
  filter(patient_index_date <= start_date | patient_end_date <= end_date) %>%
  mutate(
    event_flag = case_when(
      is.na(outcome_date) ~ 0,
      outcome_date >= start_date & outcome_date <= end_date ~ 1,
      TRUE ~ 0)
  ) %>%
  mutate(
    survival_time = as.numeric(if_else(event_flag == 0,
                    time_length(difftime(end_date, start_date), "years"),
                    time_length(difftime(end_date, outcome_date), "years"))),
    interval = start_date + (end_date - start_date) / 2
  ) 

#create a function to calculate rate per 1000 person years in each interval
#with respect to different outcomes and characteristics
rates_over_time <- function(df, outcome, characteristic) {
  df %>% 
    filter(outcome_type == !!outcome, characteristic == !!characteristic) %>%
    group_by(interval, group = as.character(.data[[characteristic]])) %>%
    summarise(
      events = sum(event_flag, na.rm = TRUE),
      total_person_years = sum(survival_time, na.rm = TRUE),
      rate_per_1000_pys = (events / total_person_years) * 1000
    ) %>%
    ungroup()
}

#create helper function to calculate rates for different outcomes and characteristics
calculate_rates <- function(df, outcomes, characteristics) {
  bind_rows(lapply(outcomes, function(outcome) {
    bind_rows(lapply(characteristics, function(char) {
      rates_over_time(df, outcome, char) %>%
        mutate(outcome = outcome, characteristic = char) %>%
        select(interval, outcome, characteristic, group, events,
               total_person_years, rate_per_1000_pys) 
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
rates_over_time <- calculate_rates(df_long, outcomes, characteristics)

#plot for different characteristics
plot_rates <- function(df, outcome, characteristic) {
  df %>%
    filter(outcome == !!outcome, characteristic == !!characteristic) %>%
    ggplot(aes(x = interval, y = rate_per_1000_pys, group = group, col = group)) +
    geom_line(stat = "identity") +
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
