library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(gt)
library(readr)
library(stringr)

## create output directories ----
fs::dir_create(here("analysis", "sensitivity_analyses"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2017-09-01"
  study_end_date <- "2018-08-31"
  cohort <- "adults"
  codelist_type <- "specific"
  investigation_type <- "sensitivity"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

roundmid_any <- function(x, to=10){
  # like roundmid_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#set all NA categories to "Unknown"
df_input <- df_input %>% mutate_if(is.factor,
                                   forcats::fct_explicit_na,
                                   na_level = "Unknown")

#calculate total person-time for each outcome type 
if (study_start_date == as.Date("2017-09-01")) {
  survival <- df_input %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival <- df_input %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
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
if (study_start_date == as.Date("2017-09-01")) {
  events <- df_input %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events <- df_input %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
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
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#define row order desired
if (study_start_date == as.Date("2017-09-01")) {
  row_order <- c("rsv_mild", "rsv_severe", "rsv_mortality")
} else if (study_start_date == as.Date("2018-09-01")) {
  row_order <- c("flu_mild", "flu_severe", "flu_mortality")
} 

#reorder rows
results <- results %>%
  slice(match(row_order, results$outcome))

#final results table
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- data.frame(
    Outcome = c("RSV mild", "RSV mortality", "RSV severe"),
    PYears = results$person_years,
    Events_Midpoint6 = results$events,
    Rate_Midpoint6_Derived = results$incidence_rate,
    Characteristic = rep("Total", 3),
    Group = rep("All", 3)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- data.frame(
    Outcome = c("Flu mild", "Flu mortality", "Flu severe"),
    PYears = results$person_years,
    Events_Midpoint6 = results$events,
    Rate_Midpoint6_Derived = results$incidence_rate,
    Characteristic = rep("Total", 3),
    Group = rep("All", 3)
  )
} 

##now do the same by risk groups

#calculate total person-time for each outcome type by age group
if (study_start_date == as.Date("2017-09-01")) {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
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
if (study_start_date == as.Date("2017-09-01")) {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
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
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
age_bands <- as.numeric(length(unique(results_age$age_band)))

#reorder rows
results_age <- results_age %>%
  group_by(age_band) %>%
  slice(match(row_order, results_age$outcome))

#add this to final results with 'Group' as Age Group
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"), age_bands),
      PYears = results_age$person_years,
      Events_Midpoint6 = results_age$events,
      Rate_Midpoint6_Derived = results_age$incidence_rate,
      Characteristic = rep("Age Group", 3 * age_bands),
      Group = results_age$age_band)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"), age_bands),
      PYears = results_age$person_years,
      Events_Midpoint6 = results_age$events,
      Rate_Midpoint6_Derived = results_age$incidence_rate,
      Characteristic = rep("Age Group", 3 * age_bands),
      Group = results_age$age_band)
  )
} 

#calculate total person-time for each outcome type by sex
if (study_start_date == as.Date("2017-09-01")) {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
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
if (study_start_date == as.Date("2017-09-01")) {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
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
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#reorder rows
results_sex <- results_sex %>%
  group_by(sex) %>%
  slice(match(row_order, results_sex$outcome))

#add this to final results with 'Group' as sex
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"), 2),
      PYears = results_sex$person_years,
      Events_Midpoint6 = results_sex$events,
      Rate_Midpoint6_Derived = results_sex$incidence_rate,
      Characteristic = rep("Sex", 6),
      Group = results_sex$sex)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"), 2),
      PYears = results_sex$person_years,
      Events_Midpoint6 = results_sex$events,
      Rate_Midpoint6_Derived = results_sex$incidence_rate,
      Characteristic = rep("Sex", 6),
      Group = results_sex$sex)
  )
} 

#calculate total person-time for each outcome type by ethnicity
if (study_start_date == as.Date("2017-09-01")) {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
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
if (study_start_date == as.Date("2017-09-01")) {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
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
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
ethnicity_groups <- as.numeric(length(unique(
  results_ethnicity$latest_ethnicity_group)))

#reorder rows
results_ethnicity <- results_ethnicity %>%
  group_by(latest_ethnicity_group) %>%
  slice(match(row_order, results_ethnicity$outcome))

#add this to final results with 'Group' as Ethnicity
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                    ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events_Midpoint6 = results_ethnicity$events,
      Rate_Midpoint6_Derived = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 3 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                    ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events_Midpoint6 = results_ethnicity$events,
      Rate_Midpoint6_Derived = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 3 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} 
  

#calculate total person-time for each outcome type by socioeconomic status
if (study_start_date == as.Date("2017-09-01")) {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
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
if (study_start_date == as.Date("2017-09-01")) {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
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
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
ses_groups <- as.numeric(length(unique(results_ses$imd_quintile)))

#reorder rows
results_ses <- results_ses %>%
  group_by(imd_quintile) %>%
  slice(match(row_order, results_ses$outcome))

#add this to final results with 'Group' as IMD Quintile
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"), ses_groups),
      PYears = results_ses$person_years,
      Events_Midpoint6 = results_ses$events,
      Rate_Midpoint6_Derived = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 3 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"), ses_groups),
      PYears = results_ses$person_years,
      Events_Midpoint6 = results_ses$events,
      Rate_Midpoint6_Derived = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 3 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} 

#calculate total person-time for each outcome type by rurality classification
if (study_start_date == as.Date("2017-09-01")) {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T),
      rsv_mortality = sum(time_rsv_mortality, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T),
      flu_mortality = sum(time_flu_mortality, na.rm = T)
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
if (study_start_date == as.Date("2017-09-01")) {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T),
      rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T),
      flu_mortality = sum(flu_mortality_inf, na.rm = T)
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
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
rurality_groups <- as.numeric(length(unique(
  results_rurality$rurality_classification)))

#reorder rows
results_rurality <- results_rurality %>%
  group_by(rurality_classification) %>%
  slice(match(row_order, results_rurality$outcome))

#add this to final results with 'Group' as rurality classification
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                    rurality_groups),
      PYears = results_rurality$person_years,
      Events_Midpoint6 = results_rurality$events,
      Rate_Midpoint6_Derived = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 3 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                    rurality_groups),
      PYears = results_rurality$person_years,
      Events_Midpoint6 = results_rurality$events,
      Rate_Midpoint6_Derived = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 3 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} 

##infants subgroup characteristics
if (cohort == "infants_subgroup") {
  #calculate total person-time for each outcome type
  #for average maternal age (grouped by outcome)
  if (study_start_date == as.Date("2017-09-01")) {
    survival_maternal_age <- df_input %>%
      pivot_longer(
        cols = c(time_rsv_primary, time_rsv_secondary, time_rsv_mortality),
        names_to = "outcome",
        names_pattern = "time_(.*)",
        values_to = "person_years"
      ) %>%
      mutate(outcome = str_replace_all(outcome, c("primary" = "mild",
                                                  "secondary" = "severe"))) %>%
      group_by(outcome) %>%
      summarise(
        avg_maternal_age = mean(maternal_age, na.rm = TRUE),
        person_years = sum(person_years, na.rm = TRUE)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    survival_maternal_age <- df_input %>%
      pivot_longer(
        cols = c(time_flu_primary, time_flu_secondary, time_flu_mortality),
        names_to = "outcome",
        names_pattern = "time_(.*)",
        values_to = "person_years"
      ) %>%
      mutate(outcome = str_replace_all(outcome, c("primary" = "mild",
                                                  "secondary" = "severe"))) %>%
      group_by(outcome) %>%
      summarise(
        avg_maternal_age = mean(maternal_age, na.rm = TRUE),
        person_years = sum(person_years, na.rm = TRUE)
      )
  } 
  
  #calculate total number of events for each outcome type
  #for average maternal age (grouped by outcome)
  if (study_start_date == as.Date("2017-09-01")) {
    events_maternal_age <- df_input %>%
      pivot_longer(
        cols = c(rsv_primary_inf, rsv_secondary_inf, rsv_mortality_inf),
        names_to = "outcome",
        names_pattern = "(.*)_inf",
        values_to = "events"
      ) %>%
      mutate(outcome = str_replace_all(outcome, c("primary" = "mild",
                                                  "secondary" = "severe"))) %>%
      group_by(outcome) %>%
      summarise(
        avg_maternal_age = mean(maternal_age, na.rm = TRUE),
        events = sum(events, na.rm = TRUE)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    events_maternal_age <- df_input %>%
      pivot_longer(
        cols = c(flu_primary_inf, flu_secondary_inf, flu_mortality_inf),
        names_to = "outcome",
        names_pattern = "(.*)_inf",
        values_to = "events"
      ) %>%
      mutate(outcome = str_replace_all(outcome, c("primary" = "mild",
                                                  "secondary" = "severe"))) %>%
      group_by(outcome) %>%
      summarise(
        avg_maternal_age = mean(maternal_age, na.rm = TRUE),
        events = sum(events, na.rm = TRUE)
      )
  } 
  
  #merge survival and events data
  results_maternal_age <- merge(survival_maternal_age, events_maternal_age)
  
  #calculate incidence rate per 1000 person-years
  results_maternal_age <- results_maternal_age %>%
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #reorder rows
  results_maternal_age <- results_maternal_age %>%
    slice(match(row_order, results_maternal_age$outcome))
  
  #add this to final results with 'Group' as maternal age
  if (study_start_date == as.Date("2017-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = c("RSV Mild", "RSV Severe", "RSV Mortality"),
        PYears = results_maternal_age$person_years,
        Events_Midpoint6 = results_maternal_age$events,
        Rate_Midpoint6_Derived = results_maternal_age$incidence_rate,
        Characteristic = rep("Average Maternal Age", 3),
        Group = results_maternal_age$avg_maternal_age)
    )
  } else if (study_start_date == as.Date("2018-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = c("Flu Mild", "Flu Severe", "Flu Mortality"),
        PYears = results_maternal_age$person_years,
        Events_Midpoint6 = results_maternal_age$events,
        Rate_Midpoint6_Derived = results_maternal_age$incidence_rate,
        Characteristic = rep("Average Maternal Age", 3),
        Group = results_maternal_age$avg_maternal_age)
    )
  } 
  
  #calculate total person-time for each outcome type by maternal smoking status
  if (study_start_date == as.Date("2017-09-01")) {
    survival_maternal_smoking <- df_input %>%
      group_by(maternal_smoking_status) %>%
      transmute(
        rsv_mild = sum(time_rsv_primary, na.rm = T),
        rsv_severe = sum(time_rsv_secondary, na.rm = T),
        rsv_mortality = sum(time_rsv_mortality, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    survival_maternal_smoking <- df_input %>%
      group_by(maternal_smoking_status) %>%
      transmute(
        flu_mild = sum(time_flu_primary, na.rm = T),
        flu_severe = sum(time_flu_secondary, na.rm = T),
        flu_mortality = sum(time_flu_mortality, na.rm = T)
      )
  } 
  
  #get unique rows
  survival_maternal_smoking <- unique(survival_maternal_smoking)
  
  #reshape
  survival_maternal_smoking <- survival_maternal_smoking %>%
    pivot_longer(
      cols = !maternal_smoking_status,
      names_to = "outcome",
      values_to = "person_years"
    )
  
  #calculate total number of events for each outcome type by maternal smoking status
  if (study_start_date == as.Date("2017-09-01")) {
    events_maternal_smoking <- df_input %>%
      group_by(maternal_smoking_status) %>%
      transmute(
        rsv_mild = sum(rsv_primary_inf, na.rm = T),
        rsv_severe = sum(rsv_secondary_inf, na.rm = T),
        rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    events_maternal_smoking <- df_input %>%
      group_by(maternal_smoking_status) %>%
      transmute(
        flu_mild = sum(flu_primary_inf, na.rm = T),
        flu_severe = sum(flu_secondary_inf, na.rm = T),
        flu_mortality = sum(flu_mortality_inf, na.rm = T)
      )
  } 
  
  #get unique rows
  events_maternal_smoking <- unique(events_maternal_smoking)
  
  #reshape
  events_maternal_smoking <- events_maternal_smoking %>%
    pivot_longer(
      cols = !maternal_smoking_status,
      names_to = "outcome",
      values_to = "events"
    )
  
  #overall results
  results_maternal_smoking <- merge(survival_maternal_smoking,
                                    events_maternal_smoking)
  
  #calculate incidence rate per 1000 person-years
  results_maternal_smoking <- results_maternal_smoking %>%
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #get number of groups
  maternal_smoking_groups <- as.numeric(length(unique(
    results_maternal_smoking$maternal_smoking_status)))
  
  #reorder rows
  results_maternal_smoking <- results_maternal_smoking %>%
    group_by(maternal_smoking_status) %>%
    slice(match(row_order, results_maternal_smoking$outcome))
  
  #add this to final results with 'Group' as maternal smoking status
  if (study_start_date == as.Date("2017-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                      maternal_smoking_groups),
        PYears = results_maternal_smoking$person_years,
        Events_Midpoint6 = results_maternal_smoking$events,
        Rate_Midpoint6_Derived = results_maternal_smoking$incidence_rate,
        Characteristic = rep("Maternal Smoking Status", 3 * maternal_smoking_groups),
        Group = results_maternal_smoking$maternal_smoking_status)
    )
  } else if (study_start_date == as.Date("2018-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                      maternal_smoking_groups),
        PYears = results_maternal_smoking$person_years,
        Events_Midpoint6 = results_maternal_smoking$events,
        Rate_Midpoint6_Derived = results_maternal_smoking$incidence_rate,
        Characteristic = rep("Maternal Smoking Status", 3 * maternal_smoking_groups),
        Group = results_maternal_smoking$maternal_smoking_status)
    )
  } 
  
  #calculate total person-time for each outcome type by maternal drinking status
  if (study_start_date == as.Date("2017-09-01")) {
    survival_maternal_drinking <- df_input %>%
      group_by(maternal_drinking) %>%
      transmute(
        rsv_mild = sum(time_rsv_primary, na.rm = T),
        rsv_severe = sum(time_rsv_secondary, na.rm = T),
        rsv_mortality = sum(time_rsv_mortality, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    survival_maternal_drinking <- df_input %>%
      group_by(maternal_drinking) %>%
      transmute(
        flu_mild = sum(time_flu_primary, na.rm = T),
        flu_severe = sum(time_flu_secondary, na.rm = T),
        flu_mortality = sum(time_flu_mortality, na.rm = T)
      )
  } 
  
  #get unique rows
  survival_maternal_drinking <- unique(survival_maternal_drinking)
  
  #reshape
  survival_maternal_drinking <- survival_maternal_drinking %>%
    pivot_longer(
      cols = !maternal_drinking,
      names_to = "outcome",
      values_to = "person_years"
    )
  
  #calculate total number of events for each outcome type by maternal drinking status
  if (study_start_date == as.Date("2017-09-01")) {
    events_maternal_drinking <- df_input %>%
      group_by(maternal_drinking) %>%
      transmute(
        rsv_mild = sum(rsv_primary_inf, na.rm = T),
        rsv_severe = sum(rsv_secondary_inf, na.rm = T),
        rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    events_maternal_drinking <- df_input %>%
      group_by(maternal_drinking) %>%
      transmute(
        flu_mild = sum(flu_primary_inf, na.rm = T),
        flu_severe = sum(flu_secondary_inf, na.rm = T),
        flu_mortality = sum(flu_mortality_inf, na.rm = T)
      )
  }
  
  #get unique rows
  events_maternal_drinking <- unique(events_maternal_drinking)
  
  #reshape
  events_maternal_drinking <- events_maternal_drinking %>%
    pivot_longer(
      cols = !maternal_drinking,
      names_to = "outcome",
      values_to = "events"
    )
  
  #overall results
  results_maternal_drinking <- merge(survival_maternal_drinking,
                                     events_maternal_drinking)
  
  #calculate incidence rate per 1000 person-years
  results_maternal_drinking <- results_maternal_drinking %>%
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #get number of groups
  maternal_drinking_groups <- as.numeric(length(unique(
    results_maternal_drinking$maternal_drinking)))
  
  #reorder rows
  results_maternal_drinking <- results_maternal_drinking %>%
    group_by(maternal_drinking) %>%
    slice(match(row_order, results_maternal_drinking$outcome))
  
  #add this to final results with 'Group' as maternal drinking status
  if (study_start_date == as.Date("2017-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                      maternal_drinking_groups),
        PYears = results_maternal_drinking$person_years,
        Events_Midpoint6 = results_maternal_drinking$events,
        Rate_Midpoint6_Derived = results_maternal_drinking$incidence_rate,
        Characteristic = rep("Maternal Drinking", 3 * maternal_drinking_groups),
        Group = results_maternal_drinking$maternal_drinking)
    )
  } else if (study_start_date == as.Date("2018-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                      maternal_drinking_groups),
        PYears = results_maternal_drinking$person_years,
        Events_Midpoint6 = results_maternal_drinking$events,
        Rate_Midpoint6_Derived = results_maternal_drinking$incidence_rate,
        Characteristic = rep("Maternal Drinking", 3 * maternal_drinking_groups),
        Group = results_maternal_drinking$maternal_drinking)
    )
  } 
  
  #calculate total person-time for each outcome type by maternal drug usage status
  if (study_start_date == as.Date("2017-09-01")) {
    survival_maternal_drug_usage <- df_input %>%
      group_by(maternal_drug_usage) %>%
      transmute(
        rsv_mild = sum(time_rsv_primary, na.rm = T),
        rsv_severe = sum(time_rsv_secondary, na.rm = T),
        rsv_mortality = sum(time_rsv_mortality, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    survival_maternal_drug_usage <- df_input %>%
      group_by(maternal_drug_usage) %>%
      transmute(
        flu_mild = sum(time_flu_primary, na.rm = T),
        flu_severe = sum(time_flu_secondary, na.rm = T),
        flu_mortality = sum(time_flu_mortality, na.rm = T)
      )
  } 
  
  #get unique rows
  survival_maternal_drug_usage <- unique(survival_maternal_drug_usage)
  
  #reshape
  survival_maternal_drug_usage <- survival_maternal_drug_usage %>%
    pivot_longer(
      cols = !maternal_drug_usage,
      names_to = "outcome",
      values_to = "person_years"
    )
  
  #calculate total number of events for each outcome type by maternal drug usage status
  if (study_start_date == as.Date("2017-09-01")) {
    events_maternal_drug_usage <- df_input %>%
      group_by(maternal_drug_usage) %>%
      transmute(
        rsv_mild = sum(rsv_primary_inf, na.rm = T),
        rsv_severe = sum(rsv_secondary_inf, na.rm = T),
        rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    events_maternal_drug_usage <- df_input %>%
      group_by(maternal_drug_usage) %>%
      transmute(
        flu_mild = sum(flu_primary_inf, na.rm = T),
        flu_severe = sum(flu_secondary_inf, na.rm = T),
        flu_mortality = sum(flu_mortality_inf, na.rm = T)
      )
  } 
  
  #get unique rows
  events_maternal_drug_usage <- unique(events_maternal_drug_usage)
  
  #reshape
  events_maternal_drug_usage <- events_maternal_drug_usage %>%
    pivot_longer(
      cols = !maternal_drug_usage,
      names_to = "outcome",
      values_to = "events"
    )
  
  #overall results
  results_maternal_drug_usage <- merge(survival_maternal_drug_usage,
                                       events_maternal_drug_usage)
  
  #calculate incidence rate per 1000 person-years
  results_maternal_drug_usage <- results_maternal_drug_usage %>%
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #get number of groups
  maternal_drug_usage_groups <- as.numeric(length(unique(
    results_maternal_drug_usage$maternal_drug_usage)))
  
  #reorder rows
  results_maternal_drug_usage <- results_maternal_drug_usage %>%
    group_by(maternal_drug_usage) %>%
    slice(match(row_order, results_maternal_drug_usage$outcome))
  
  #add this to final results with 'Group' as maternal drug usage status
  if (study_start_date == as.Date("2017-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                      maternal_drug_usage_groups),
        PYears = results_maternal_drug_usage$person_years,
        Events_Midpoint6 = results_maternal_drug_usage$events,
        Rate_Midpoint6_Derived = results_maternal_drug_usage$incidence_rate,
        Characteristic = rep("Maternal Drug Usage", 3 * maternal_drug_usage_groups),
        Group = results_maternal_drug_usage$maternal_drug_usage)
    )
  } else if (study_start_date == as.Date("2018-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                      maternal_drug_usage_groups),
        PYears = results_maternal_drug_usage$person_years,
        Events_Midpoint6 = results_maternal_drug_usage$events,
        Rate_Midpoint6_Derived = results_maternal_drug_usage$incidence_rate,
        Characteristic = rep("Maternal Drug Usage", 3 * maternal_drug_usage_groups),
        Group = results_maternal_drug_usage$maternal_drug_usage)
    )
  } 
  
  #calculate total person-time for each outcome type
  #by maternal pertussis vaccination status
  if (study_start_date == as.Date("2017-09-01")) {
    survival_maternal_pertussis_vacc <- df_input %>%
      group_by(maternal_pertussis_vaccination) %>%
      transmute(
        rsv_mild = sum(time_rsv_primary, na.rm = T),
        rsv_severe = sum(time_rsv_secondary, na.rm = T),
        rsv_mortality = sum(time_rsv_mortality, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    survival_maternal_pertussis_vacc <- df_input %>%
      group_by(maternal_pertussis_vaccination) %>%
      transmute(
        flu_mild = sum(time_flu_primary, na.rm = T),
        flu_severe = sum(time_flu_secondary, na.rm = T),
        flu_mortality = sum(time_flu_mortality, na.rm = T)
      )
  } 
  
  #get unique rows
  survival_maternal_pertussis_vacc <- unique(survival_maternal_pertussis_vacc)
  
  #reshape
  survival_maternal_pertussis_vacc <- survival_maternal_pertussis_vacc %>%
    pivot_longer(
      cols = !maternal_pertussis_vaccination,
      names_to = "outcome",
      values_to = "person_years"
    )
  
  #calculate total number of events for each outcome type
  #by maternal pertussis vaccination status
  if (study_start_date == as.Date("2017-09-01")) {
    events_maternal_pertussis_vacc <- df_input %>%
      group_by(maternal_pertussis_vaccination) %>%
      transmute(
        rsv_mild = sum(rsv_primary_inf, na.rm = T),
        rsv_severe = sum(rsv_secondary_inf, na.rm = T),
        rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    events_maternal_pertussis_vacc <- df_input %>%
      group_by(maternal_pertussis_vaccination) %>%
      transmute(
        flu_mild = sum(flu_primary_inf, na.rm = T),
        flu_severe = sum(flu_secondary_inf, na.rm = T),
        flu_mortality = sum(flu_mortality_inf, na.rm = T)
      )
  } 
  
  #get unique rows
  events_maternal_pertussis_vacc <- unique(events_maternal_pertussis_vacc)
  
  #reshape
  events_maternal_pertussis_vacc <- events_maternal_pertussis_vacc %>%
    pivot_longer(
      cols = !maternal_pertussis_vaccination,
      names_to = "outcome",
      values_to = "events"
    )
  
  #overall results
  results_maternal_pertussis_vacc <- merge(survival_maternal_pertussis_vacc,
                                           events_maternal_pertussis_vacc)
  
  #calculate incidence rate per 1000 person-years
  results_maternal_pertussis_vacc <- results_maternal_pertussis_vacc %>%
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #get number of groups
  maternal_pertussis_vacc_groups <- as.numeric(length(unique(
    results_maternal_pertussis_vacc$maternal_pertussis_vaccination)))
  
  #reorder rows
  results_maternal_pertussis_vacc <- results_maternal_pertussis_vacc %>%
    group_by(maternal_pertussis_vaccination) %>%
    slice(match(row_order, results_maternal_pertussis_vacc$outcome))
  
  #add this to final results with 'Group' as maternal pertussis vaccination status
  if (study_start_date == as.Date("2017-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                      maternal_pertussis_vacc_groups),
        PYears = results_maternal_pertussis_vacc$person_years,
        Events_Midpoint6 = results_maternal_pertussis_vacc$events,
        Rate_Midpoint6_Derived = results_maternal_pertussis_vacc$incidence_rate,
        Characteristic = rep("Maternal Pertussis Vaccination Status",
                             3 * maternal_pertussis_vacc_groups),
        Group = results_maternal_pertussis_vacc$maternal_pertussis_vaccination)
    )
  } else if (study_start_date == as.Date("2018-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                      maternal_pertussis_vacc_groups),
        PYears = results_maternal_pertussis_vacc$person_years,
        Events_Midpoint6 = results_maternal_pertussis_vacc$events,
        Rate_Midpoint6_Derived = results_maternal_pertussis_vacc$incidence_rate,
        Characteristic = rep("Maternal Pertussis Vaccination Status",
                             3 * maternal_pertussis_vacc_groups),
        Group = results_maternal_pertussis_vacc$maternal_pertussis_vaccination)
    )
  } 
  
  #calculate total person-time for each outcome
  #type by maternal influenza vaccination status
  if (study_start_date == as.Date("2017-09-01")) {
    survival_maternal_flu_vacc <- df_input %>%
      group_by(maternal_flu_vaccination) %>%
      transmute(
        rsv_mild = sum(time_rsv_primary, na.rm = T),
        rsv_severe = sum(time_rsv_secondary, na.rm = T),
        rsv_mortality = sum(time_rsv_mortality, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    survival_maternal_flu_vacc <- df_input %>%
      group_by(maternal_flu_vaccination) %>%
      transmute(
        flu_mild = sum(time_flu_primary, na.rm = T),
        flu_severe = sum(time_flu_secondary, na.rm = T),
        flu_mortality = sum(time_flu_mortality, na.rm = T)
      )
  }
  
  #get unique rows
  survival_maternal_flu_vacc <- unique(survival_maternal_flu_vacc)
  
  #reshape
  survival_maternal_flu_vacc <- survival_maternal_flu_vacc %>%
    pivot_longer(
      cols = !maternal_flu_vaccination,
      names_to = "outcome",
      values_to = "person_years"
    )
  
  #calculate total number of events for each outcome type
  #by maternal influenza vaccination status
  if (study_start_date == as.Date("2017-09-01")) {
    events_maternal_flu_vacc <- df_input %>%
      group_by(maternal_flu_vaccination) %>%
      transmute(
        rsv_mild = sum(rsv_primary_inf, na.rm = T),
        rsv_severe = sum(rsv_secondary_inf, na.rm = T),
        rsv_mortality = sum(rsv_mortality_inf, na.rm = T)
      )
  } else if (study_start_date == as.Date("2018-09-01")) {
    events_maternal_flu_vacc <- df_input %>%
      group_by(maternal_flu_vaccination) %>%
      transmute(
        flu_mild = sum(flu_primary_inf, na.rm = T),
        flu_severe = sum(flu_secondary_inf, na.rm = T),
        flu_mortality = sum(flu_mortality_inf, na.rm = T)
      )
  } 
  
  #get unique rows
  events_maternal_flu_vacc <- unique(events_maternal_flu_vacc)
  
  #reshape
  events_maternal_flu_vacc <- events_maternal_flu_vacc %>%
    pivot_longer(
      cols = !maternal_flu_vaccination,
      names_to = "outcome",
      values_to = "events"
    )
  
  #overall results
  results_maternal_flu_vacc <- merge(survival_maternal_flu_vacc,
                                           events_maternal_flu_vacc)
  
  #calculate incidence rate per 1000 person-years
  results_maternal_flu_vacc <- results_maternal_flu_vacc %>%
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #get number of groups
  maternal_flu_vacc_groups <- as.numeric(length(unique(
    results_maternal_flu_vacc$maternal_flu_vaccination)))
  
  #reorder rows
  results_maternal_flu_vacc <- results_maternal_flu_vacc %>%
    group_by(maternal_flu_vaccination) %>%
    slice(match(row_order, results_maternal_flu_vacc$outcome))
  
  #add this to final results with 'Group' as maternal influenza vaccination status
  if (study_start_date == as.Date("2017-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("RSV Mild", "RSV Severe", "RSV Mortality"),
                      maternal_flu_vacc_groups),
        PYears = results_maternal_flu_vacc$person_years,
        Events_Midpoint6 = results_maternal_flu_vacc$events,
        Rate_Midpoint6_Derived = results_maternal_flu_vacc$incidence_rate,
        Characteristic = rep("Maternal Influenza Vaccination Status",
                             3 * maternal_flu_vacc_groups),
        Group = results_maternal_flu_vacc$maternal_flu_vaccination)
    )
  } else if (study_start_date == as.Date("2018-09-01")) {
    final_results <- rbind(
      final_results,
      data.frame(
        Outcome = rep(c("Flu Mild", "Flu Severe", "Flu Mortality"),
                      maternal_flu_vacc_groups),
        PYears = results_maternal_flu_vacc$person_years,
        Events_Midpoint6 = results_maternal_flu_vacc$events,
        Rate_Midpoint6_Derived = results_maternal_flu_vacc$incidence_rate,
        Characteristic = rep("Maternal Influenza Vaccination Status",
                             3 * maternal_flu_vacc_groups),
        Group = results_maternal_flu_vacc$maternal_flu_vaccination)
    )
  } 
}

##children/adolescents, adult and older adult cohort specific characteristics

if ((cohort == "children_and_adolescents"|cohort == "adults"|cohort == "older_adults") &
    study_start_date == as.Date("2018-09-01")) {
  
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
    mutate(events = roundmid_any(events)) %>%
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
      Events_Midpoint6 = results_prior_flu_vacc$events,
      Rate_Midpoint6_Derived = results_prior_flu_vacc$incidence_rate,
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
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  #add this to final results with 'Group' as prior flu vaccination status
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = c(rep(c("Flu mild", "Flu mortality", "Flu severe"), 2)),
      PYears = results_flu_vacc$person_years,
      Events_Midpoint6 = results_flu_vacc$events,
      Rate_Midpoint6_Derived = results_flu_vacc$incidence_rate,
      Characteristic = rep("Vaccinated against influenza in current season",
                           6),
      Group = results_flu_vacc$flu_vaccination)
  )
}

#export
if (cohort == "infants") {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity", "IMD Quintile",
                    "Rurality Classification")
} else if (cohort == "infants_subgroup") {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity", "IMD Quintile",
                   "Rurality Classification", "Average Maternal Age",
                   "Maternal Smoking Status", "Maternal Drinking",
                   "Maternal Drug Usage", "Maternal Pertussis Vaccination Status",
                   "Maternal Influenza Vaccination Status")
} else {
  if (study_start_date == as.Date("2018-09-01")) {
    table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                      "IMD Quintile", "Rurality Classification",
                      "Vaccinated against influenza in previous season",
                      "Vaccinated against influenza in current season")
  } else {
    table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                      "IMD Quintile", "Rurality Classification")
  }
}

## create output directories ----
fs::dir_create(here("output", "results", "rates"))

#export results table to csv
if (length(args) == 0) {
  results_table <- final_results %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    select(Outcome, Group, Characteristic, Events_Midpoint6, Rate_Midpoint6_Derived) %>%
    group_by(Characteristic) %>%
    gt(groupname_col = "Characteristic") %>%
    row_group_order(groups = c(table_groups)) %>%
    tab_header(
      title = "Rate per 1000 person-years of outcomes by characteristic",
      subtitle = "Group-wise breakdown"
    )
  results_table_frame <- as.data.frame(results_table) %>%
    write_csv(file = paste0(here::here("output", "results", "rates"), "/",
                            "rates_", cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type, "_",
                            investigation_type, ".csv"))
} else {
  results_table <- final_results %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    select(Outcome, Group, Characteristic, Events_Midpoint6, Rate_Midpoint6_Derived) %>%
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
}
