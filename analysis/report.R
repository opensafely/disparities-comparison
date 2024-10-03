library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(gt)
library(readr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2022-09-01"
  study_end_date <- "2023-08-31"
  cohort <- "adults"
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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type, ".arrow")))

## create output directories ----
fs::dir_create(here("output", "models"))

#calculate total person-time for each outcome type
if (study_start_date >= covid_season_min) {
  survival <- df_input %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival <- df_input %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival <- df_input %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival <- df_input %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}
  
#get unique rows
survival <- unique(survival)

#reshape
survival <- survival %>%
  pivot_longer(
    cols = colnames(survival),
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type
if (study_start_date >= covid_season_min) {
  events <- df_input %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events <- df_input %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events <- df_input %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events <- df_input %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events <- unique(events)

#reshape
events <- events %>%
  pivot_longer(
    cols = colnames(events),
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results <- merge(survival, events, by = "outcome")

#calculate incidence rate per 1000 person-years
results <- results %>%
  mutate(incidence_rate = events / person_years * 1000)

if (study_start_date >= covid_season_min) {
  row_order <- c("rsv_mild", "rsv_severe", "rsv_mortality",
                 "flu_mild", "flu_severe", "flu_mortality",
                 "covid_mild", "covid_severe", "covid_mortality",
                 "all_cause_mortality")
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  row_order <- c("rsv_mild", "rsv_severe", "rsv_mortality",
                 "flu_mild", "flu_severe", "flu_mortality",
                 "covid_mild", "covid_severe", "covid_mortality",
                 "overall_resp", "overall_resp_severe",
                 "overall_resp_mortality", "all_cause_mortality")
} else if (codelist_type == "sensitive") {
  row_order <- c("rsv_mild", "rsv_severe", "rsv_mortality",
                 "flu_mild", "flu_severe", "flu_mortality",
                 "overall_resp", "overall_resp_severe",
                 "overall_resp_mortality", "all_cause_mortality")
} else {
  row_order <- c("rsv_mild", "rsv_severe", "rsv_mortality",
                 "flu_mild", "flu_severe", "flu_mortality",
                 "all_cause_mortality")
}

results <- results %>%
  slice(match(row_order, results$outcome))

#final results table
if (study_start_date >= covid_season_min) {
  final_results <- data.frame(
    Outcome = c("RSV Mild", "RSV Severe", "RSV Mortality",
                "Flu Mild", "Flu Severe", "Flu Mortality",
                "COVID Mild", "COVID Severe", "COVID Mortality",
                "All-cause mortality"),
    PYears = results$person_years,
    Events = results$events,
    Rate = results$incidence_rate,
    Characteristic = rep("Total", 10),
    Group = rep("All", 10)
  )
} else if (codelist_type == "sensitive") {
  final_results <- data.frame(
    Outcome = c("All-cause mortality", "Flu mild", "Flu mortality",
                "Flu severe", "RSV mortality", "RSV mild", "RSV severe",
                "Overall respiratory mild", "Overall respiratory mortality",
                "Overall respiratory severe"),
    PYears = results$person_years,
    Events = results$events,
    Rate = results$incidence_rate,
    Characteristic = rep("Total", 10),
    Group = rep("All", 10)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- data.frame(
    Outcome = c("RSV Mild", "RSV Severe", "RSV Mortality",
                "Flu Mild", "Flu Severe", "Flu Mortality",
                "COVID Mild", "COVID Severe", "COVID Mortality",
                "Overall respiratory mild", "Overall respiratory mortality",
                "Overall respiratory severe", "All-cause mortality"),
    PYears = results$person_years,
    Events = results$events,
    Rate = results$incidence_rate,
    Characteristic = rep("Total", 13),
    Group = rep("All", 13)
  )
} else {
  final_results <- data.frame(
    Outcome = c("RSV Mild", "RSV Severe", "RSV Mortality",
                "Flu Mild", "Flu Severe", "Flu Mortality",
                "All-cause mortality"),
    PYears = results$person_years,
    Events = results$events,
    Rate = results$incidence_rate,
    Characteristic = c(rep("Total", 7)),
    Group = rep("All", 7)
  )
}

##now do the same by risk groups

#calculate total person-time for each outcome type by age group
if (study_start_date >= covid_season_min) {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}

#get unique rows
survival_age <- unique(survival_age)

#reshape
survival_age <- survival_age %>%
  pivot_longer(
    cols = !age_band,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by age group
if (study_start_date >= covid_season_min) {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events_age <- unique(events_age)

#reshape
events_age <- events_age %>%
  pivot_longer(
    cols = !age_band,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_age <- merge(survival_age, events_age)

#calculate incidence rate per 1000 person-years
results_age <- results_age %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
age_bands <- as.numeric(length(unique(results_age$age_band)))

#reorder rows
results_age <- results_age %>%
  group_by(age_band) %>%
  slice(match(row_order, results_age$outcome))

#add this to final results with 'Group' as Age Group
if (study_start_date >= covid_season_min) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "All-cause mortality"), age_bands),
      PYears = results_age$person_years,
      Events = results_age$events,
      Rate = results_age$incidence_rate,
      Characteristic = rep("Age Group", 10 * age_bands),
      Group = results_age$age_band)
  )
} else if (codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), age_bands),
      PYears = results_age$person_years,
      Events = results_age$events,
      Rate = results_age$incidence_rate,
      Characteristic = rep("Age Group", 10 * age_bands),
      Group = results_age$age_band)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), age_bands),
      PYears = results_age$person_years,
      Events = results_age$events,
      Rate = results_age$incidence_rate,
      Characteristic = rep("Age Group", 13 * age_bands),
      Group = results_age$age_band)
  )
} else {
  final_results <- rbind(
    final_results,
      data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "All-cause mortality"), age_bands),
      PYears = results_age$person_years,
      Events = results_age$events,
      Rate = results_age$incidence_rate,
      Characteristic = rep("Age Group", 7 * age_bands),
      Group = results_age$age_band)
  )
}

#calculate total person-time for each outcome type by sex
if (study_start_date >= covid_season_min) {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}

#get unique rows
survival_sex <- unique(survival_sex)

#reshape
survival_sex <- survival_sex %>%
  pivot_longer(
    cols = !sex,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by sex
if (study_start_date >= covid_season_min) {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events_sex <- unique(events_sex)

#reshape
events_sex <- events_sex %>%
  pivot_longer(
    cols = !sex,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_sex <- merge(survival_sex, events_sex)

#calculate incidence rate per 1000 person-years
results_sex <- results_sex %>%
  mutate(incidence_rate = events / person_years * 1000)

#reorder rows
results_sex <- results_sex %>%
  group_by(sex) %>%
  slice(match(row_order, results_sex$outcome))

#add this to final results with 'Group' as sex
if (study_start_date >= covid_season_min) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "All-cause mortality"), 2),
      PYears = results_sex$person_years,
      Events = results_sex$events,
      Rate = results_sex$incidence_rate,
      Characteristic = rep("Sex", 20),
      Group = results_sex$sex)
  )
} else if (codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), 2),
      PYears = results_sex$person_years,
      Events = results_sex$events,
      Rate = results_sex$incidence_rate,
      Characteristic = rep("Sex", 20),
      Group = results_sex$sex)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), 2),
      PYears = results_sex$person_years,
      Events = results_sex$events,
      Rate = results_sex$incidence_rate,
      Characteristic = rep("Sex", 26),
      Group = results_sex$sex)
  )
} else {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "All-cause mortality"), 2),
      PYears = results_sex$person_years,
      Events = results_sex$events,
      Rate = results_sex$incidence_rate,
      Characteristic = rep("Sex", 14),
      Group = results_sex$sex)
  )
}

#calculate total person-time for each outcome type by ethnicity
if (study_start_date >= covid_season_min) {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}

#get unique rows
survival_ethnicity <- unique(survival_ethnicity)

#reshape
survival_ethnicity <- survival_ethnicity %>%
  pivot_longer(
    cols = !latest_ethnicity_group,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by ethnicity
if (study_start_date >= covid_season_min) {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events_ethnicity <- unique(events_ethnicity)

#reshape
events_ethnicity <- events_ethnicity %>%
  pivot_longer(
    cols = !latest_ethnicity_group,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_ethnicity <- merge(survival_ethnicity, events_ethnicity)

#calculate incidence rate per 1000 person-years
results_ethnicity <- results_ethnicity %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
ethnicity_groups <- as.numeric(length(unique(
  results_ethnicity$latest_ethnicity_group)))

#reorder rows
results_ethnicity <- results_ethnicity %>%
  group_by(latest_ethnicity_group) %>%
  slice(match(row_order, results_ethnicity$outcome))

#add this to final results with 'Group' as Ethnicity
if (study_start_date >= covid_season_min) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                  "Flu Mild", "Flu Severe", "Flu Mortality",
                  "COVID Mild", "COVID Severe", "COVID Mortality",
                  "All-cause mortality"), ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events = results_ethnicity$events,
      Rate = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 10 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} else if (codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
      data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                  "Flu Mild", "Flu Severe", "Flu Mortality",
                  "Overall respiratory mild",
                  "Overall respiratory mortality",
                  "Overall respiratory severe",
                  "All-cause mortality"), ethinicity_groups),
      PYears = results_ethnicity$person_years,
      Events = results_ethnicity$events,
      Rate = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 10 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                  "Flu Mild", "Flu Severe", "Flu Mortality",
                  "COVID Mild", "COVID Severe", "COVID Mortality",
                  "Overall respiratory mild", "Overall respiratory mortality",
                  "Overall respiratory severe",
                  "All-cause mortality"), ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events = results_ethnicity$events,
      Rate = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 13 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} else {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                  "Flu Mild", "Flu Severe", "Flu Mortality",
                  "All-cause mortality"), ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events = results_ethnicity$events,
      Rate = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 7 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
}

#calculate total person-time for each outcome type by socioeconomic status
if (study_start_date >= covid_season_min) {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}

#get unique rows
survival_ses <- unique(survival_ses)

#reshape
survival_ses <- survival_ses %>%
  pivot_longer(
    cols = !imd_quintile,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by socioeconomic status
if (study_start_date >= covid_season_min) {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events_ses <- unique(events_ses)

#reshape
events_ses <- events_ses %>%
  pivot_longer(
    cols = !imd_quintile,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_ses <- merge(survival_ses, events_ses)

#calculate incidence rate per 1000 person-years
results_ses <- results_ses %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
ses_groups <- as.numeric(length(unique(results_ses$imd_quintile)))

#reorder rows
results_ses <- results_ses %>%
  group_by(imd_quintile) %>%
  slice(match(row_order, results_ses$outcome))

#add this to final results with 'Group' as IMD Quintile
if (study_start_date >= covid_season_min) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "All-cause mortality"), ses_groups),
      PYears = results_ses$person_years,
      Events = results_ses$events,
      Rate = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 10 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} else if (codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), ses_groups),
      PYears = results_ses$person_years,
      Events = results_ses$events,
      Rate = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 10 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), ses_groups),
      PYears = results_ses$person_years,
      Events = results_ses$events,
      Rate = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 13 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} else {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "All-cause mortality"), ses_groups),
      PYears = results_ses$person_years,
      Events = results_ses$events,
      Rate = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 7 * ses_groups),
      Group = results_ses$imd_quintile)
  )
}

#calculate total person-time for each outcome type by household composition type
if (study_start_date >= covid_season_min) {
  survival_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}

#get unique rows
survival_hh_comp <- unique(survival_hh_comp)

#reshape
survival_hh_comp <- survival_hh_comp %>%
  pivot_longer(
    cols = !composition_category,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by composition type
if (study_start_date >= covid_season_min) {
  events_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events_hh_comp <- unique(events_hh_comp)

#reshape
events_hh_comp <- events_hh_comp %>%
  pivot_longer(
    cols = !composition_category,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_hh_comp <- merge(survival_hh_comp, events_hh_comp)

#calculate incidence rate per 1000 person-years
results_hh_comp <- results_hh_comp %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
hh_comp_groups <- as.numeric(length(unique(
  results_hh_comp$composition_category)))

#reorder rows
results_hh_comp <- results_hh_comp %>%
  group_by(composition_category) %>%
  slice(match(row_order, results_hh_comp$outcome))

#add this to final results with 'Group' as IMD Quintile
if (study_start_date >= covid_season_min) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "All-cause mortality"), hh_comp_groups),
      PYears = results_hh_comp$person_years,
      Events = results_hh_comp$events,
      Rate = results_hh_comp$incidence_rate,
      Characteristic = rep("Household Composition Category",
                           10 * hh_comp_groups),
      Group = results_hh_comp$composition_category)
  )
} else if (codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), hh_comp_groups),
      PYears = results_hh_comp$person_years,
      Events = results_hh_comp$events,
      Rate = results_hh_comp$incidence_rate,
      Characteristic = rep("Household Composition Category",
                           10 * hh_comp_groups),
      Group = results_hh_comp$composition_category)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), hh_comp_groups),
      PYears = results_hh_comp$person_years,
      Events = results_hh_comp$events,
      Rate = results_hh_comp$incidence_rate,
      Characteristic = rep("Household Composition Category",
                           13 * hh_comp_groups),
      Group = results_hh_comp$composition_category)
  )
} else {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "All-cause mortality"), hh_comp_groups),
      PYears = results_hh_comp$person_years,
      Events = results_hh_comp$events,
      Rate = results_hh_comp$incidence_rate,
      Characteristic = rep("Household Composition Category",
                           7 * hh_comp_groups),
      Group = results_hh_comp$composition_category)
  )
}

#calculate total person-time for each outcome type by rurality classification
if (study_start_date >= covid_season_min) {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T),
      covid_mortality = sum(time_covid_mortality, na.rm = T),
      overall_resp = sum(time_overall_resp_primary, na.rm = T),
      overall_resp_severe = sum(time_overall_resp_secondary, na.rm = T),
      overall_resp_mortality = sum(time_overall_resp_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
} else {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T),
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T),
      all_cause_mortality = sum(time_all_cause_mortality, na.rm = T)
    )
}

#get unique rows
survival_rurality <- unique(survival_rurality)

#reshape
survival_rurality <- survival_rurality %>%
  pivot_longer(
    cols = !rurality_classification,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by rurality classif.
if (study_start_date >= covid_season_min) {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (codelist_type == "sensitive") {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T),
      covid_mortality = sum(covid_mortality_inf, na.rm = T),
      overall_resp = sum(overall_resp_primary_inf, na.rm = T),
      overall_resp_severe = sum(overall_resp_secondary_inf, na.rm = T),
      overall_resp_mortality = sum(overall_resp_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
} else {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T),
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T),
      all_cause_mortality = sum(all_cause_mortality_inf, na.rm = T)
    )
}

#get unique rows
events_rurality <- unique(events_rurality)

#reshape
events_rurality <- events_rurality %>%
  pivot_longer(
    cols = !rurality_classification,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_rurality <- merge(survival_rurality, events_rurality)

#calculate incidence rate per 1000 person-years
results_rurality <- results_rurality %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
rurality_groups <- as.numeric(length(unique(
  results_rurality$rurality_classification)))

#reorder rows
results_rurality <- results_rurality %>%
  group_by(rurality_classification) %>%
  slice(match(row_order, results_rurality$outcome))

#add this to final results with 'Group' as rurality classification
if (study_start_date >= covid_season_min) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = c("RSV Mild", "RSV Severe", "RSV Mortality",
                  "Flu Mild", "Flu Severe", "Flu Mortality",
                  "COVID Mild", "COVID Severe", "COVID Mortality",
                  "All-cause mortality"),
      PYears = results_rurality$person_years,
      Events = results_rurality$events,
      Rate = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 10 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} else if (codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), rurality_groups),
      PYears = results_rurality$person_years,
      Events = results_rurality$events,
      Rate = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 10 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} else if (study_start_date >= covid_season_min &
           codelist_type == "sensitive") {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "COVID Mild", "COVID Severe", "COVID Mortality",
                      "Overall respiratory mild",
                      "Overall respiratory mortality",
                      "Overall respiratory severe",
                      "All-cause mortality"), rurality_groups),
      PYears = results_rurality$person_years,
      Events = results_rurality$events,
      Rate = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 13 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} else {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality",
                      "Flu Mild", "Flu Severe", "Flu Mortality",
                      "All-cause mortality"), rurality_groups),
      PYears = results_rurality$person_years,
      Events = results_rurality$events,
      Rate = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 7 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
}

##insert section for rates for infant subgroup

#gestational age

#maternal age

#maternal smoking status

#maternal drinking

#maternal drug usage

#maternal pertussis vaccination status

#maternal influenza vaccination status

##children/adolescents, adult and older adult cohort specific characteristics

if (cohort == "children_and_adolescents" |
    cohort == "adults" | cohort == "older_adults") {
  if (study_start_date >= covid_prior_vacc_min) {
    survival_time_since_cov_vaccines <- df_input %>%
      group_by(time_since_last_covid_vaccination) %>%
      transmute(
        covid_mild = sum(time_covid_primary, na.rm = T),
        covid_severe = sum(time_covid_secondary, na.rm = T),
        covid_mortality = sum(time_covid_mortality, na.rm = T)
      )
    #get unique rows
    survival_time_since_cov_vaccines <- unique(survival_time_since_cov_vaccines)
    #reshape
    survival_time_since_cov_vaccines <- survival_time_since_cov_vaccines %>%
      pivot_longer(
        cols = !time_since_last_covid_vaccination,
        names_to = "outcome",
        values_to = "person_years"
      )

    #calculate total number of events for each outcome type
    #by time since last covid vaccine
    events_time_since_cov_vaccines <- df_input %>%
      group_by(time_since_last_covid_vaccination) %>%
      transmute(
        covid_mild = sum(covid_primary_inf, na.rm = T),
        covid_severe = sum(covid_secondary_inf, na.rm = T),
        covid_mortality = sum(covid_mortality_inf, na.rm = T)
      )

    #get unique rows
    events_time_since_cov_vaccines <- unique(events_time_since_cov_vaccines)
    #reshape
    events_time_since_cov_vaccines <- events_time_since_cov_vaccines %>%
        pivot_longer(
          cols = !time_since_last_covid_vaccination,
          names_to = "outcome",
          values_to = "events"
        )
    #overall results
    results_time_since_cov_vaccines <- merge(survival_time_since_cov_vaccines,
                                             events_time_since_cov_vaccines)
    #calculate incidence rate per 1000 person-years
    results_time_since_cov_vaccines <- results_time_since_cov_vaccines %>%
      mutate(incidence_rate = events / person_years * 1000)
    #get number of groups
    cov_vaccines_groups <- as.numeric(length(unique(
      results_time_since_cov_vaccines$time_since_last_covid_vaccination)))

    #reorder rows
    results_time_since_cov_vaccines <- results_time_since_cov_vaccines %>%
      group_by(time_since_last_covid_vaccination) %>%
      slice(match(row_order, results_time_since_cov_vaccines$outcome))
    #add this to final results with 'Group' as time since last covid vaccine
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = c(rep(c("COVID mild", "COVID mortality", "COVID severe"),
                        cov_vaccines_groups)),
        PYears = results_time_since_cov_vaccines$person_years,
        Events = results_time_since_cov_vaccines$events,
        Rate = results_time_since_cov_vaccines$incidence_rate,
        Characteristic = c(rep("Time Since Last COVID-19 Vaccination",
                               3 * cov_vaccines_groups)),
        Group = results_time_since_cov_vaccines$
          time_since_last_covid_vaccination)
      )
    #calculate total person-time for each outcome type by
    #current season covid vaccine
    survival_cov_vaccines_mild <- df_input %>%
      group_by(covid_vaccination = covid_vaccination_mild) %>%
      transmute(
        covid_mild = sum(time_covid_primary, na.rm = T)
      )
    survival_cov_vaccines_severe <- df_input %>%
      group_by(covid_vaccination = covid_vaccination_severe) %>%
      transmute(
        covid_severe = sum(time_covid_secondary, na.rm = T),
      )
    survival_cov_vaccines_mortality <- df_input %>%
      group_by(covid_vaccination) %>%
      transmute(
        covid_mortality = sum(time_covid_mortality, na.rm = T)
      )
    #get unique rows
    survival_cov_vaccines_mild <- unique(survival_cov_vaccines_mild)
    survival_cov_vaccines_severe <- unique(survival_cov_vaccines_severe)
    survival_cov_vaccines_mortality <- unique(survival_cov_vaccines_mortality)
    #wide to long
    survival_cov_vaccines_mild <- survival_cov_vaccines_mild %>%
      pivot_longer(
        cols = !covid_vaccination,
        names_to = "outcome",
        values_to = "person_years"
      )
    survival_cov_vaccines_severe <- survival_cov_vaccines_severe %>%
      pivot_longer(
        cols = !covid_vaccination,
        names_to = "outcome",
        values_to = "person_years"
      )
    survival_cov_vaccines_mortality <- survival_cov_vaccines_mortality %>%
      pivot_longer(
        cols = !covid_vaccination,
        names_to = "outcome",
        values_to = "person_years"
      )
    #join the groups
    survival_cov_vaccines <- rbind(survival_cov_vaccines_mild,
                                   survival_cov_vaccines_severe,
                                   survival_cov_vaccines_mortality)
    #calculate total number of events for each outcome type by
    #time since last covid vaccine
    events_cov_vaccines_mild <- df_input %>%
      group_by(covid_vaccination = covid_vaccination_mild) %>%
      transmute(
        covid_mild = sum(covid_primary_inf, na.rm = T)
      )
    events_cov_vaccines_severe <- df_input %>%
      group_by(covid_vaccination = covid_vaccination_severe) %>%
      transmute(
        covid_severe = sum(covid_secondary_inf, na.rm = T),
      )
    events_cov_vaccines_mortality <- df_input %>%
      group_by(covid_vaccination) %>%
      transmute(
        covid_mortality = sum(covid_mortality_inf, na.rm = T)
      )
    #get unique rows
    events_cov_vaccines_mild <- unique(events_cov_vaccines_mild)
    events_cov_vaccines_severe <- unique(events_cov_vaccines_severe)
    events_cov_vaccines_mortality <- unique(events_cov_vaccines_mortality)
    #wide to long
    events_cov_vaccines_mild <- events_cov_vaccines_mild %>%
      pivot_longer(
        cols = !covid_vaccination,
        names_to = "outcome",
        values_to = "events"
      )
    events_cov_vaccines_severe <- events_cov_vaccines_severe %>%
      pivot_longer(
        cols = !covid_vaccination,
        names_to = "outcome",
        values_to = "events"
      )
    events_cov_vaccines_mortality <- events_cov_vaccines_mortality %>%
      pivot_longer(
        cols = !covid_vaccination,
        names_to = "outcome",
        values_to = "events"
      )
    #join the groups
    events_cov_vaccines <- rbind(events_cov_vaccines_mild,
                                 events_cov_vaccines_severe,
                                 events_cov_vaccines_mortality)
    #overall results
    results_cov_vaccines <- merge(survival_cov_vaccines, events_cov_vaccines)
    #calculate incidence rate per 1000 person-years
    results_cov_vaccines <- results_cov_vaccines %>%
      mutate(incidence_rate = events / person_years * 1000)
    #add this to final results with 'Group' as covid vaccination status
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = c(rep(c("COVID mild", "COVID mortality",
                          "COVID severe"), 2)),
        PYears = results_cov_vaccines$person_years,
        Events = results_cov_vaccines$events,
        Rate = results_cov_vaccines$incidence_rate,
        Characteristic = c(rep("Vaccinated against COVID-19 in current season",
                               6)),
        Group = results_cov_vaccines$covid_vaccination)
      )
    }
  #calculate total person-time for each outcome type by flu vaccination status
  survival_prior_flu_vacc <- df_input %>%
    group_by(prior_flu_vaccination) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
    )
  #get unique rows
  survival_prior_flu_vacc <- unique(survival_prior_flu_vacc)
  #reshape
  survival_prior_flu_vacc <- survival_prior_flu_vacc %>%
    pivot_longer(
      cols = !prior_flu_vaccination,
      names_to = "outcome",
      values_to = "person_years"
    )
  #calculate total number of events for each outcome type by
  #flu_vacc classification
  events_prior_flu_vacc <- df_input %>%
    group_by(prior_flu_vaccination) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
    )
  #get unique rows
  events_prior_flu_vacc <- unique(events_prior_flu_vacc)
  #reshape
  events_prior_flu_vacc <- events_prior_flu_vacc %>%
    pivot_longer(
      cols = !prior_flu_vaccination,
      names_to = "outcome",
      values_to = "events"
    )
  #overall results
  results_prior_flu_vacc <- merge(survival_prior_flu_vacc,
                                  events_prior_flu_vacc)
  #calculate incidence rate per 1000 person-years
  results_prior_flu_vacc <- results_prior_flu_vacc %>%
    mutate(incidence_rate = events / person_years * 1000)

  #reorder rows
  results_prior_flu_vacc <- results_prior_flu_vacc %>%
    group_by(prior_flu_vaccination) %>%
    slice(match(row_order, results_prior_flu_vacc$outcome))
  #add this to final results with 'Group' as prior flu vaccination status
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = c(rep(c("Flu mild", "Flu mortality", "Flu severe"), 2)),
        PYears = results_prior_flu_vacc$person_years,
        Events = results_prior_flu_vacc$events,
        Rate = results_prior_flu_vacc$incidence_rate,
        Characteristic = rep("Vaccinated against influenza in previous season",
                             6),
        Group = results_prior_flu_vacc$prior_flu_vaccination)
    )
  #calculate total person-time for each outcome type by flu vaccination status
  survival_flu_vacc_mild <- df_input %>%
    group_by(flu_vaccination = flu_vaccination_mild) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T)
      )
  survival_flu_vacc_severe <- df_input %>%
    group_by(flu_vaccination = flu_vaccination_severe) %>%
    transmute(
      flu_severe = sum(time_flu_secondary, na.rm = T)
      )
  survival_flu_vacc_mortality <- df_input %>%
    group_by(flu_vaccination) %>%
    transmute(
      flu_mortality = sum(time_flu_mortality, na.rm = T)
      )
  #get unique rows
  survival_flu_vacc_mild <- unique(survival_flu_vacc_mild)
  survival_flu_vacc_severe <- unique(survival_flu_vacc_severe)
  survival_flu_vacc_mortality <- unique(survival_flu_vacc_mortality)
  #wide to long
  survival_flu_vacc_mild <- survival_flu_vacc_mild %>%
    pivot_longer(
      cols = !flu_vaccination,
      names_to = "outcome",
      values_to = "person_years"
    )
  survival_flu_vacc_severe <- survival_flu_vacc_severe %>%
    pivot_longer(
      cols = !flu_vaccination,
      names_to = "outcome",
      values_to = "person_years"
    )
  survival_flu_vacc_mortality <- survival_flu_vacc_mortality %>%
    pivot_longer(
      cols = !flu_vaccination,
      names_to = "outcome",
      values_to = "person_years"
    )
  #join the groups
  survival_flu_vacc <- rbind(survival_flu_vacc_mild,
                             survival_flu_vacc_severe,
                             survival_flu_vacc_mortality)
  #calculate total number of events for each outcome type by
  #flu vaccination status
  events_flu_vacc_mild <- df_input %>%
    group_by(flu_vaccination = flu_vaccination_mild) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T)
    )
  events_flu_vacc_severe <- df_input %>%
    group_by(flu_vaccination = flu_vaccination_severe) %>%
    transmute(
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
  events_flu_vacc_mortality <- df_input %>%
    group_by(flu_vaccination) %>%
    transmute(
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
    )
  #get unique rows
  events_flu_vacc_mild <- unique(events_flu_vacc_mild)
  events_flu_vacc_severe <- unique(events_flu_vacc_severe)
  events_flu_vacc_mortality <- unique(events_flu_vacc_mortality)
  #wide to long
  events_flu_vacc_mild <- events_flu_vacc_mild %>%
    pivot_longer(
      cols = !flu_vaccination,
      names_to = "outcome",
      values_to = "events"
    )
  events_flu_vacc_severe <- events_flu_vacc_severe %>%
    pivot_longer(
      cols = !flu_vaccination,
      names_to = "outcome",
      values_to = "events"
    )
  events_flu_vacc_mortality <- events_flu_vacc_mortality %>%
    pivot_longer(
      cols = !flu_vaccination,
      names_to = "outcome",
      values_to = "events"
    )
  #join the groups
  events_flu_vacc <- rbind(events_flu_vacc_mild,
                           events_flu_vacc_severe,
                           events_flu_vacc_mortality)
  #overall results
  results_flu_vacc <- merge(survival_flu_vacc, events_flu_vacc)
  #calculate incidence rate per 1000 person-years
  results_flu_vacc <- results_flu_vacc %>%
    mutate(incidence_rate = events / person_years * 1000)
  #add this to final results with 'Group' as prior flu vaccination status
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = c(rep(c("Flu mild", "Flu mortality", "Flu severe"), 2)),
      PYears = results_flu_vacc$person_years,
      Events = results_flu_vacc$events,
      Rate = results_flu_vacc$incidence_rate,
      Characteristic = rep("Vaccinated against influenza in current season", 6),
      Group = results_flu_vacc$flu_vaccination)
    )
}

## create output directories ----
fs::dir_create(here("output", "results"))

#export

if (cohort == "infants") {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity", "IMD Quintile",
                   "Household Composition Category", "Rurality Classification")
} else if (cohort == "infants_subgroup") {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity", "IMD Quintile",
                   "Household Composition Category", "Rurality Classification",
                   "Gestational Age", "Maternal Age", "Maternal Smoking Status",
                   "Maternal Drinking", "Maternal Drug Usage",
                   "Maternal Pertussis Vaccination Status",
                   "Maternal Influenza Vaccination Status")
} else {
  if (study_start_date >= covid_prior_vacc_min) {
    table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                      "IMD Quintile", "Rurality Classification",
                     "Household Composition Category",
                     "Time Since Last COVID-19 Vaccination",
                     "Vaccinated against COVID-19 in current season",
                     "Vaccinated against influenza in previous season",
                     "Vaccinated against influenza in current season")
  } else if (study_start_date == covid_current_vacc_min) {
    table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                      "IMD Quintile", "Rurality Classification",
                     "Household Composition Category",
                     "Vaccinated against COVID-19 in current season",
                     "Vaccinated against influenza in previous season",
                     "Vaccinated against influenza in current season")
  } else {
    table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                      "IMD Quintile", "Rurality Classification",
                     "Household Composition Category",
                     "Vaccinated against influenza in previous season",
                     "Vaccinated against influenza in current season")
  }
}

## create output directories ----
fs::dir_create(here("output", "results", "rates"))

#export results table to csv
results_table <- final_results %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  select(Outcome, Group, Characteristic, Events, Rate) %>%
  group_by(Characteristic) %>%
  gt(groupname_col = "Characteristic") %>%
  row_group_order(groups = c(table_groups)) %>%
  tab_header(
    title = "Rate per 1000 person-years of outcomes by characteristic",
    subtitle = "Group-wise breakdown"
  )
results_table_frame <- as.data.frame(results_table) %>%
  write_csv(path = paste0(here::here("output", "results", "rates"), "/",
                          "rates_", cohort, "_", year(study_start_date), "_",
                          year(study_end_date), "_", codelist_type, "_",
                          investigation_type, ".csv"))
