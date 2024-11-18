library(tidyverse)
library(here)
library(arrow)
library(lubridate)

source(here("analysis", "design", "design.R"))

## create output directories ----
fs::dir_create(here("post_check", "primary_analyses"))

cohort <- "older_adults"
phenotype <- "specific"

#import collated rates
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                     paste0(cohort, "weekly_rates_primary_collated.csv")))

#helper function to create and save plots
create_summaries <- function(df, outcomes, characteristics, study_start_date,
                             study_end_date, seasons) {
  for (i in length(seasons)) {
  study_start_date <- as.Date(study_dates[[starts[i]]])
  study_end_date <- as.Date(study_dates[[ends[i]]])
  summaries <- expand.grid(outcome = outcomes, characteristic = characteristics,
                           season = seasons) %>%
    pmap(function(outcome, characteristic, season) {
      sum <- df %>%
        filter(outcome == !!outcome, characteristic == !!characteristic,
               subset == !!season) %>%
      print(sum)  # Print or save each plot
      ## create output directories ----
      fs::dir_create(here("post_check", "primary_analyses", "plots", 
                          paste0(characteristic)))
      write_csv(sum, path = paste0(here("post_check", "primary_analyses", "plots", 
                paste0(characteristic)), "/", "weekly_rates_",
                year(start), "_", year(end), "_", cohort, "_", codelist_type,
                "_", investigation_type, "_", characteristic, ".csv"))
    })
  return(summaries)
  }
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

#define study start and end dates
starts <- c("season1_start_date", "season2_start_date", "season3_start_date",
            "season4_start_date", "season5_start_date", "season6_start_date",
            "season7_start_date", "season8_start_date")
ends <- c("season1_end_date", "season2_end_date", "season3_end_date",
          "season4_end_date", "season5_end_date", "season6_end_date",
          "season7_end_date", "season8_end_date")
seasons <- c("2016_17", "2017_18", "2018_19", "2019_20", "2020_21", "2021_22",
             "2022_23", "2023_24")

#run function to create and save plots
create_summaries(df_input, outcomes, characteristics, study_start_date,
                 study_end_date, seasons)

##
#helper function to create and save plots
create_summaries <- function(df, outcomes, characteristics) {
  summaries <- expand.grid(outcome = outcomes, characteristic = characteristics) %>%
    pmap(function(outcome, characteristic) {
      sum <- df %>%
        filter(outcome == !!outcome, characteristic == !!characteristic) 
      print(sum)  # Print or save each plot
      ## create output directories ----
      fs::dir_create(here("output", "results", "rates", "weekly",
                          paste0(characteristic)))
      write_csv(sum, path = paste0(here("output", "results", "rates",
                                        "weekly", paste0(characteristic)), "/", "weekly_rates_",
                                   year(study_start_date), "_", year(study_end_date), "_", cohort,
                                   "_", codelist_type, "_", investigation_type, "_",
                                   characteristic, ".csv"))
    })
  return(summaries)
}

#run function to create and save plots
create_summaries(rates_over_time, outcomes, characteristics)
