library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(gt)
library(readr)
library(stringr)

## create output directories ----
fs::dir_create(here::here("analysis", "secondary_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2020-09-01"
  study_end_date <- "2021-08-31"
  cohort <- "older_adults"
  codelist_type <- "specific"
  investigation_type <- "secondary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

source(here::here("analysis", "functions", "redaction.R"))

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#set all NA categories to "Unknown"
df_input <- df_input %>% mutate_if(is.factor, forcats::fct_explicit_na,
                                   na_level = "Unknown")

#calculate total person-time for each outcome type 
if (study_start_date == as.Date("2017-09-01")) {
  survival <- df_input %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival <- df_input %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival <- df_input %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
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
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events <- df_input %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events <- df_input %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
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
  row_order <- c("rsv_mild", "rsv_severe")
} else if (study_start_date == as.Date("2018-09-01")) {
  row_order <- c("flu_mild", "flu_severe")
} else if (study_start_date == as.Date("2020-09-01")) {
  row_order <- c("covid_mild", "covid_severe")
}

#reorder rows
results <- results %>%
  slice(match(row_order, results$outcome))

#final results table
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- data.frame(
    Outcome = c("RSV Mild", "RSV Severe"),
    PYears = results$person_years,
    Events_Midpoint10 = results$events,
    Rate_Midpoint10_Derived = results$incidence_rate,
    Characteristic = rep("Total", 2),
    Group = rep("All", 2)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- data.frame(
    Outcome = c("Flu Mild", "Flu Severe"),
    PYears = results$person_years,
    Events_Midpoint10 = results$events,
    Rate_Midpoint10_Derived = results$incidence_rate,
    Characteristic = rep("Total", 2),
    Group = rep("All", 2)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- data.frame(
    Outcome = c("COVID Mild", "COVID Severe"),
    PYears = results$person_years,
    Events_Midpoint10 = results$events,
    Rate_Midpoint10_Derived = results$incidence_rate,
    Characteristic = rep("Total", 2),
    Group = rep("All", 2)
  )
}

##now do the same by risk groups

#calculate total person-time for each outcome type by age group
if (study_start_date == as.Date("2017-09-01")) {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
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
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_age <- df_input %>%
    group_by(age_band) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
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
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    age_bands),
      PYears = results_age$person_years,
      Events_Midpoint10 = results_age$events,
      Rate_Midpoint10_Derived = results_age$incidence_rate,
      Characteristic = rep("Age Group", 2 * age_bands),
      Group = results_age$age_band)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    age_bands),
      PYears = results_age$person_years,
      Events_Midpoint10 = results_age$events,
      Rate_Midpoint10_Derived = results_age$incidence_rate,
      Characteristic = rep("Age Group", 2 * age_bands),
      Group = results_age$age_band)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                      age_bands),
      PYears = results_age$person_years,
      Events_Midpoint10 = results_age$events,
      Rate_Midpoint10_Derived = results_age$incidence_rate,
      Characteristic = rep("Age Group", 2 * age_bands),
      Group = results_age$age_band)
  )
}

#calculate total person-time for each outcome type by sex
if (study_start_date == as.Date("2017-09-01")) {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
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
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_sex <- df_input %>%
    group_by(sex) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
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
      Outcome = rep(c("RSV Mild", "RSV Severe"), 2),
      PYears = results_sex$person_years,
      Events_Midpoint10 = results_sex$events,
      Rate_Midpoint10_Derived = results_sex$incidence_rate,
      Characteristic = rep("Sex", 4),
      Group = results_sex$sex)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"), 2),
      PYears = results_sex$person_years,
      Events_Midpoint10 = results_sex$events,
      Rate_Midpoint10_Derived = results_sex$incidence_rate,
      Characteristic = rep("Sex", 4),
      Group = results_sex$sex)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"), 2),
      PYears = results_sex$person_years,
      Events_Midpoint10 = results_sex$events,
      Rate_Midpoint10_Derived = results_sex$incidence_rate,
      Characteristic = rep("Sex", 4),
      Group = results_sex$sex)
  )
}

#calculate total person-time for each outcome type by ethnicity
if (study_start_date == as.Date("2017-09-01")) {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
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
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_ethnicity <- df_input %>%
    group_by(latest_ethnicity_group) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
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
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events_Midpoint10 = results_ethnicity$events,
      Rate_Midpoint10_Derived = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 2 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events_Midpoint10 = results_ethnicity$events,
      Rate_Midpoint10_Derived = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 2 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    ethnicity_groups),
      PYears = results_ethnicity$person_years,
      Events_Midpoint10 = results_ethnicity$events,
      Rate_Midpoint10_Derived = results_ethnicity$incidence_rate,
      Characteristic = rep("Ethnicity", 2 * ethnicity_groups),
      Group = results_ethnicity$latest_ethnicity_group)
  )
}

#calculate total person-time for each outcome type by socioeconomic status
if (study_start_date == as.Date("2017-09-01")) {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
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
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_ses <- df_input %>%
    group_by(imd_quintile) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
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
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    ses_groups),
      PYears = results_ses$person_years,
      Events_Midpoint10 = results_ses$events,
      Rate_Midpoint10_Derived = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 2 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    ses_groups),
      PYears = results_ses$person_years,
      Events_Midpoint10 = results_ses$events,
      Rate_Midpoint10_Derived = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 2 * ses_groups),
      Group = results_ses$imd_quintile)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    ses_groups),
      PYears = results_ses$person_years,
      Events_Midpoint10 = results_ses$events,
      Rate_Midpoint10_Derived = results_ses$incidence_rate,
      Characteristic = rep("IMD Quintile", 2 * ses_groups),
      Group = results_ses$imd_quintile)
  )
}

#calculate total person-time for each outcome type by household composition type
if (study_start_date == as.Date("2020-09-01")) {
  survival_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )

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
  events_hh_comp <- df_input %>%
    group_by(composition_category) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )

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
    mutate(events = roundmid_any(events)) %>%
    mutate(incidence_rate = events / person_years * 1000)
  
  #get number of groups
  hh_comp_groups <- as.numeric(length(unique(
    results_hh_comp$composition_category)))
  
  #reorder rows
  results_hh_comp <- results_hh_comp %>%
    group_by(composition_category) %>%
    slice(match(row_order, results_hh_comp$outcome))
  
  #add this to final results with 'Group' as IMD Quintile
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    hh_comp_groups),
      PYears = results_hh_comp$person_years,
      Events_Midpoint10 = results_hh_comp$events,
      Rate_Midpoint10_Derived = results_hh_comp$incidence_rate,
      Characteristic = rep("Household Composition Category",
                           2 * hh_comp_groups),
      Group = results_hh_comp$composition_category)
    )
}

#calculate total person-time for each outcome type by rurality classification
if (study_start_date == as.Date("2017-09-01")) {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
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
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_rurality <- df_input %>%
    group_by(rurality_classification) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
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
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    rurality_groups),
      PYears = results_rurality$person_years,
      Events_Midpoint10 = results_rurality$events,
      Rate_Midpoint10_Derived = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 2 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    rurality_groups),
      PYears = results_rurality$person_years,
      Events_Midpoint10 = results_rurality$events,
      Rate_Midpoint10_Derived = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 2 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    rurality_groups),
      PYears = results_rurality$person_years,
      Events_Midpoint10 = results_rurality$events,
      Rate_Midpoint10_Derived = results_rurality$incidence_rate,
      Characteristic = rep("Rurality Classification", 2 * rurality_groups),
      Group = results_rurality$rurality_classification)
  )
}

# if (study_start_date == as.Date("2018-09-01")) {
#   #calculate total person-time for each outcome type by flu vaccination status
#   survival_prior_flu_vacc <- df_input %>%
#     group_by(prior_flu_vaccination) %>%
#     transmute(
#       flu_mild = sum(time_flu_primary, na.rm = T),
#       flu_severe = sum(time_flu_secondary, na.rm = T)
#     )
#   #get unique rows
#   survival_prior_flu_vacc <- unique(survival_prior_flu_vacc)
#   #reshape
#   survival_prior_flu_vacc <- survival_prior_flu_vacc %>%
#     pivot_longer(
#       cols = !prior_flu_vaccination,
#       names_to = "outcome",
#       values_to = "person_years"
#     )
#   #calculate total number of events for each outcome type by
#   #flu_vacc classification
#   events_prior_flu_vacc <- df_input %>%
#     group_by(prior_flu_vaccination) %>%
#     transmute(
#       flu_mild = sum(flu_primary_inf, na.rm = T),
#       flu_severe = sum(flu_secondary_inf, na.rm = T)
#     )
#   #get unique rows
#   events_prior_flu_vacc <- unique(events_prior_flu_vacc)
#   #reshape
#   events_prior_flu_vacc <- events_prior_flu_vacc %>%
#     pivot_longer(
#       cols = !prior_flu_vaccination,
#       names_to = "outcome",
#       values_to = "events"
#     )
#   #overall results
#   results_prior_flu_vacc <- merge(survival_prior_flu_vacc,
#                                   events_prior_flu_vacc)
#   #calculate incidence rate per 1000 person-years
#   results_prior_flu_vacc <- results_prior_flu_vacc %>%
#     mutate(events = roundmid_any(events)) %>%
#     mutate(incidence_rate = events / person_years * 1000)
#   
#   #reorder rows
#   results_prior_flu_vacc <- results_prior_flu_vacc %>%
#     group_by(prior_flu_vaccination) %>%
#     slice(match(row_order, results_prior_flu_vacc$outcome))
#   #add this to final results with 'Group' as prior flu vaccination status
#   final_results <- rbind(
#     final_results,
#     data.frame(
#       Outcome = c(rep(c("Flu Mild", "Flu Severe"), 2)),
#                         # "Flu Mortality"), 2)),
#       PYears = results_prior_flu_vacc$person_years,
#       Events_Midpoint10 = results_prior_flu_vacc$events,
#       Rate_Midpoint10_Derived = results_prior_flu_vacc$incidence_rate,
#       Characteristic = rep("Vaccinated against influenza in previous season",
#                            4),
#       Group = results_prior_flu_vacc$prior_flu_vaccination)
#   )
#   #calculate total person-time for each outcome type by flu vaccination status
#   survival_flu_vacc_mild <- df_input %>%
#     group_by(flu_vaccination = flu_vaccination_mild) %>%
#     transmute(
#       flu_mild = sum(time_flu_primary, na.rm = T)
#     )
#   survival_flu_vacc_severe <- df_input %>%
#     group_by(flu_vaccination = flu_vaccination_severe) %>%
#     transmute(
#       flu_severe = sum(time_flu_secondary, na.rm = T)
#     )
#   #get unique rows
#   survival_flu_vacc_mild <- unique(survival_flu_vacc_mild)
#   survival_flu_vacc_severe <- unique(survival_flu_vacc_severe)
#   #wide to long
#   survival_flu_vacc_mild <- survival_flu_vacc_mild %>%
#     pivot_longer(
#       cols = !flu_vaccination,
#       names_to = "outcome",
#       values_to = "person_years"
#     )
#   survival_flu_vacc_severe <- survival_flu_vacc_severe %>%
#     pivot_longer(
#       cols = !flu_vaccination,
#       names_to = "outcome",
#       values_to = "person_years"
#     )
#   #join the groups
#   survival_flu_vacc <- rbind(survival_flu_vacc_mild,
#                              survival_flu_vacc_severe)
#   #calculate total number of events for each outcome type by
#   #flu vaccination status
#   events_flu_vacc_mild <- df_input %>%
#     group_by(flu_vaccination = flu_vaccination_mild) %>%
#     transmute(
#       flu_mild = sum(flu_primary_inf, na.rm = T)
#     )
#   events_flu_vacc_severe <- df_input %>%
#     group_by(flu_vaccination = flu_vaccination_severe) %>%
#     transmute(
#       flu_severe = sum(flu_secondary_inf, na.rm = T)
#     )
#   #get unique rows
#   events_flu_vacc_mild <- unique(events_flu_vacc_mild)
#   events_flu_vacc_severe <- unique(events_flu_vacc_severe)
#   #wide to long
#   events_flu_vacc_mild <- events_flu_vacc_mild %>%
#     pivot_longer(
#     cols = !flu_vaccination,
#       names_to = "outcome",
#       values_to = "events"
#     )
#   events_flu_vacc_severe <- events_flu_vacc_severe %>%
#     pivot_longer(
#       cols = !flu_vaccination,
#       names_to = "outcome",
#       values_to = "events"
#     )
#   #join the groups
#   events_flu_vacc <- rbind(events_flu_vacc_mild,
#                            events_flu_vacc_severe)
#   #overall results
#   results_flu_vacc <- merge(survival_flu_vacc, events_flu_vacc)
#   #calculate incidence rate per 1000 person-years
#   results_flu_vacc <- results_flu_vacc %>%
#     mutate(events = roundmid_any(events)) %>%
#     mutate(incidence_rate = events / person_years * 1000)
#   #add this to final results with 'Group' as prior flu vaccination status
#   final_results <- rbind(
#     final_results,
#     data.frame(
#       Outcome = c(rep(c("Flu Mild", "Flu Severe"), 2)),
#       PYears = results_flu_vacc$person_years,
#       Events_Midpoint10 = results_flu_vacc$events,
#       Rate_Midpoint10_Derived = results_flu_vacc$incidence_rate,
#       Characteristic = rep("Vaccinated against influenza in current season",
#                            4),
#       Group = results_flu_vacc$flu_vaccination)
#   )
# }
#   
# if (study_start_date == as.Date("2020-09-01")) {
#   #calculate total person-time for each outcome type by
#   #current season covid vaccine
#   survival_cov_vaccines_mild <- df_input %>%
#     group_by(covid_vaccination = covid_vaccination_mild) %>%
#     transmute(
#       covid_mild = sum(time_covid_primary, na.rm = T)
#     )
#   survival_cov_vaccines_severe <- df_input %>%
#     group_by(covid_vaccination = covid_vaccination_severe) %>%
#     transmute(
#       covid_severe = sum(time_covid_secondary, na.rm = T)
#     )
#   #get unique rows
#   survival_cov_vaccines_mild <- unique(survival_cov_vaccines_mild)
#   survival_cov_vaccines_severe <- unique(survival_cov_vaccines_severe)
#   #wide to long
#   survival_cov_vaccines_mild <- survival_cov_vaccines_mild %>%
#     pivot_longer(
#       cols = !covid_vaccination,
#       names_to = "outcome",
#       values_to = "person_years"
#     )
#   survival_cov_vaccines_severe <- survival_cov_vaccines_severe %>%
#     pivot_longer(
#       cols = !covid_vaccination,
#       names_to = "outcome",
#       values_to = "person_years"
#     )
#   #join the groups
#   survival_cov_vaccines <- rbind(survival_cov_vaccines_mild,
#                                  survival_cov_vaccines_severe)
#   #calculate total number of events for each outcome type by
#   #time since last covid vaccine
#   events_cov_vaccines_mild <- df_input %>%
#     group_by(covid_vaccination = covid_vaccination_mild) %>%
#     transmute(
#       covid_mild = sum(covid_primary_inf, na.rm = T)
#     )
#   events_cov_vaccines_severe <- df_input %>%
#     group_by(covid_vaccination = covid_vaccination_severe) %>%
#     transmute(
#       covid_severe = sum(covid_secondary_inf, na.rm = T)
#     )
#   #get unique rows
#   events_cov_vaccines_mild <- unique(events_cov_vaccines_mild)
#   events_cov_vaccines_severe <- unique(events_cov_vaccines_severe)
#   #wide to long
#   events_cov_vaccines_mild <- events_cov_vaccines_mild %>%
#     pivot_longer(
#       cols = !covid_vaccination,
#       names_to = "outcome",
#       values_to = "events"
#     )
#   events_cov_vaccines_severe <- events_cov_vaccines_severe %>%
#     pivot_longer(
#       cols = !covid_vaccination,
#       names_to = "outcome",
#       values_to = "events"
#     )
#   #join the groups
#   events_cov_vaccines <- rbind(events_cov_vaccines_mild,
#                                events_cov_vaccines_severe)
#   #overall results
#   results_cov_vaccines <- merge(survival_cov_vaccines, events_cov_vaccines)
#   #calculate incidence rate per 1000 person-years
#   results_cov_vaccines <- results_cov_vaccines %>%
#     mutate(events = roundmid_any(events)) %>%
#     mutate(incidence_rate = events / person_years * 1000)
#   #add this to final results with 'Group' as covid vaccination status
#   final_results <- rbind(
#     final_results,
#     data.frame(
#       Outcome = c(rep(c("COVID Mild", "COVID Severe"), 2)),
#       PYears = results_cov_vaccines$person_years,
#       Events_Midpoint10 = results_cov_vaccines$events,
#       Rate_Midpoint10_Derived = results_cov_vaccines$incidence_rate,
#       Characteristic = c(rep("Vaccinated against COVID-19 in current season",
#                              4)),
#       Group = results_cov_vaccines$covid_vaccination)
#   )
#  }

#calculate total person-time for each outcome type by asthma status
if (study_start_date == as.Date("2017-09-01")) {
  survival_asthma <- df_input %>%
    group_by(has_asthma) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_asthma <- df_input %>%
    group_by(has_asthma) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_asthma <- df_input %>%
    group_by(has_asthma) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_asthma <- unique(survival_asthma)

#reshape
survival_asthma <- survival_asthma %>%
  pivot_longer(
    cols = !has_asthma,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by asthma status
if (study_start_date == as.Date("2017-09-01")) {
  events_asthma <- df_input %>%
    group_by(has_asthma) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_asthma <- df_input %>%
    group_by(has_asthma) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_asthma <- df_input %>%
    group_by(has_asthma) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_asthma <- unique(events_asthma)

#reshape
events_asthma <- events_asthma %>%
  pivot_longer(
    cols = !has_asthma,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_asthma <- merge(survival_asthma, events_asthma)

#calculate incidence rate per 1000 person-years
results_asthma <- results_asthma %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
asthma_groups <- as.numeric(length(unique(
  results_asthma$has_asthma)))

#reorder rows
results_asthma <- results_asthma %>%
  group_by(has_asthma) %>%
  slice(match(row_order, results_asthma$outcome))

#add this to final results with 'Group' as asthma
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    asthma_groups),
      PYears = results_asthma$person_years,
      Events_Midpoint10 = results_asthma$events,
      Rate_Midpoint10_Derived = results_asthma$incidence_rate,
      Characteristic = rep("Has Asthma", 2 * asthma_groups),
      Group = results_asthma$has_asthma)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    asthma_groups),
      PYears = results_asthma$person_years,
      Events_Midpoint10 = results_asthma$events,
      Rate_Midpoint10_Derived = results_asthma$incidence_rate,
      Characteristic = rep("Has Asthma", 2 * asthma_groups),
      Group = results_asthma$has_asthma)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    asthma_groups),
      PYears = results_asthma$person_years,
      Events_Midpoint10 = results_asthma$events,
      Rate_Midpoint10_Derived = results_asthma$incidence_rate,
      Characteristic = rep("Has Asthma", 2 * asthma_groups),
      Group = results_asthma$has_asthma)
  )
}

#calculate total person-time for each outcome type by COPD
if (study_start_date == as.Date("2017-09-01")) {
  survival_copd <- df_input %>%
    group_by(has_copd) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_copd <- df_input %>%
    group_by(has_copd) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_copd <- df_input %>%
    group_by(has_copd) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_copd <- unique(survival_copd)

#reshape
survival_copd <- survival_copd %>%
  pivot_longer(
    cols = !has_copd,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by copd 
if (study_start_date == as.Date("2017-09-01")) {
  events_copd <- df_input %>%
    group_by(has_copd) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_copd <- df_input %>%
    group_by(has_copd) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_copd <- df_input %>%
    group_by(has_copd) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_copd <- unique(events_copd)

#reshape
events_copd <- events_copd %>%
  pivot_longer(
    cols = !has_copd,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_copd <- merge(survival_copd, events_copd)

#calculate incidence rate per 1000 person-years
results_copd <- results_copd %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
copd_groups <- as.numeric(length(unique(
  results_copd$has_copd)))

#reorder rows
results_copd <- results_copd %>%
  group_by(has_copd) %>%
  slice(match(row_order, results_copd$outcome))

#add this to final results with 'Group' as COPD
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    copd_groups),
      PYears = results_copd$person_years,
      Events_Midpoint10 = results_copd$events,
      Rate_Midpoint10_Derived = results_copd$incidence_rate,
      Characteristic = rep("Has COPD", 2 * copd_groups),
      Group = results_copd$has_copd)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    copd_groups),
      PYears = results_copd$person_years,
      Events_Midpoint10 = results_copd$events,
      Rate_Midpoint10_Derived = results_copd$incidence_rate,
      Characteristic = rep("Has COPD", 2 * copd_groups),
      Group = results_copd$has_copd)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    copd_groups),
      PYears = results_copd$person_years,
      Events_Midpoint10 = results_copd$events,
      Rate_Midpoint10_Derived = results_copd$incidence_rate,
      Characteristic = rep("Has COPD", 2 * copd_groups),
      Group = results_copd$has_copd)
  )
}

#calculate total person-time for each outcome type by cystic fibrosis
if (study_start_date == as.Date("2017-09-01")) {
  survival_cystic_fibrosis <- df_input %>%
    group_by(has_cystic_fibrosis) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_cystic_fibrosis <- df_input %>%
    group_by(has_cystic_fibrosis) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_cystic_fibrosis <- df_input %>%
    group_by(has_cystic_fibrosis) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_cystic_fibrosis <- unique(survival_cystic_fibrosis)

#reshape
survival_cystic_fibrosis <- survival_cystic_fibrosis %>%
  pivot_longer(
    cols = !has_cystic_fibrosis,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by cystic fibrosis 
if (study_start_date == as.Date("2017-09-01")) {
  events_cystic_fibrosis <- df_input %>%
    group_by(has_cystic_fibrosis) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_cystic_fibrosis <- df_input %>%
    group_by(has_cystic_fibrosis) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_cystic_fibrosis <- df_input %>%
    group_by(has_cystic_fibrosis) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_cystic_fibrosis <- unique(events_cystic_fibrosis)

#reshape
events_cystic_fibrosis <- events_cystic_fibrosis %>%
  pivot_longer(
    cols = !has_cystic_fibrosis,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_cystic_fibrosis <- merge(survival_cystic_fibrosis, events_cystic_fibrosis)

#calculate incidence rate per 1000 person-years
results_cystic_fibrosis <- results_cystic_fibrosis %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
cystic_fibrosis_groups <- as.numeric(length(unique(
  results_cystic_fibrosis$has_cystic_fibrosis)))

#reorder rows
results_cystic_fibrosis <- results_cystic_fibrosis %>%
  group_by(has_cystic_fibrosis) %>%
  slice(match(row_order, results_cystic_fibrosis$outcome))

#add this to final results with 'Group' as cystic fibrosis
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    cystic_fibrosis_groups),
      PYears = results_cystic_fibrosis$person_years,
      Events_Midpoint10 = results_cystic_fibrosis$events,
      Rate_Midpoint10_Derived = results_cystic_fibrosis$incidence_rate,
      Characteristic = rep("Has Cystic Fibrosis", 2 * cystic_fibrosis_groups),
      Group = results_cystic_fibrosis$has_cystic_fibrosis)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    cystic_fibrosis_groups),
      PYears = results_cystic_fibrosis$person_years,
      Events_Midpoint10 = results_cystic_fibrosis$events,
      Rate_Midpoint10_Derived = results_cystic_fibrosis$incidence_rate,
      Characteristic = rep("Has Cystic Fibrosis", 2 * cystic_fibrosis_groups),
      Group = results_cystic_fibrosis$has_cystic_fibrosis)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    cystic_fibrosis_groups),
      PYears = results_cystic_fibrosis$person_years,
      Events_Midpoint10 = results_cystic_fibrosis$events,
      Rate_Midpoint10_Derived = results_cystic_fibrosis$incidence_rate,
      Characteristic = rep("Has Cystic Fibrosis", 2 * cystic_fibrosis_groups),
      Group = results_cystic_fibrosis$has_cystic_fibrosis)
  )
}

#calculate total person-time for each outcome type by other resp. diseases
if (study_start_date == as.Date("2017-09-01")) {
  survival_other_resp <- df_input %>%
    group_by(has_other_resp) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_other_resp <- df_input %>%
    group_by(has_other_resp) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_other_resp <- df_input %>%
    group_by(has_other_resp) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_other_resp <- unique(survival_other_resp)

#reshape
survival_other_resp <- survival_other_resp %>%
  pivot_longer(
    cols = !has_other_resp,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by other resp. diseases
if (study_start_date == as.Date("2017-09-01")) {
  events_other_resp <- df_input %>%
    group_by(has_other_resp) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_other_resp <- df_input %>%
    group_by(has_other_resp) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_other_resp <- df_input %>%
    group_by(has_other_resp) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_other_resp <- unique(events_other_resp)

#reshape
events_other_resp <- events_other_resp %>%
  pivot_longer(
    cols = !has_other_resp,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_other_resp <- merge(survival_other_resp, events_other_resp)

#calculate incidence rate per 1000 person-years
results_other_resp <- results_other_resp %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
other_resp_groups <- as.numeric(length(unique(
  results_other_resp$has_other_resp)))

#reorder rows
results_other_resp <- results_other_resp %>%
  group_by(has_other_resp) %>%
  slice(match(row_order, results_other_resp$outcome))

#add this to final results with 'Group' as other resp. diseases
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    other_resp_groups),
      PYears = results_other_resp$person_years,
      Events_Midpoint10 = results_other_resp$events,
      Rate_Midpoint10_Derived = results_other_resp$incidence_rate,
      Characteristic = rep("Has Other Resp. Diseases", 2 * other_resp_groups),
      Group = results_other_resp$has_other_resp)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    other_resp_groups),
      PYears = results_other_resp$person_years,
      Events_Midpoint10 = results_other_resp$events,
      Rate_Midpoint10_Derived = results_other_resp$incidence_rate,
      Characteristic = rep("Has Other Resp. Diseases", 2 * other_resp_groups),
      Group = results_other_resp$has_other_resp)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    other_resp_groups),
      PYears = results_other_resp$person_years,
      Events_Midpoint10 = results_other_resp$events,
      Rate_Midpoint10_Derived = results_other_resp$incidence_rate,
      Characteristic = rep("Has Other Resp. Diseases", 2 * other_resp_groups),
      Group = results_other_resp$has_other_resp)
  )
}

#calculate total person-time for each outcome type by diabetes
if (study_start_date == as.Date("2017-09-01")) {
  survival_diabetes <- df_input %>%
    group_by(has_diabetes) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_diabetes <- df_input %>%
    group_by(has_diabetes) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_diabetes <- df_input %>%
    group_by(has_diabetes) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_diabetes <- unique(survival_diabetes)

#reshape
survival_diabetes <- survival_diabetes %>%
  pivot_longer(
    cols = !has_diabetes,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by diabetes
if (study_start_date == as.Date("2017-09-01")) {
  events_diabetes <- df_input %>%
    group_by(has_diabetes) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_diabetes <- df_input %>%
    group_by(has_diabetes) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_diabetes <- df_input %>%
    group_by(has_diabetes) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_diabetes <- unique(events_diabetes)

#reshape
events_diabetes <- events_diabetes %>%
  pivot_longer(
    cols = !has_diabetes,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_diabetes <- merge(survival_diabetes, events_diabetes)

#calculate incidence rate per 1000 person-years
results_diabetes <- results_diabetes %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
diabetes_groups <- as.numeric(length(unique(
  results_diabetes$has_diabetes)))

#reorder rows
results_diabetes <- results_diabetes %>%
  group_by(has_diabetes) %>%
  slice(match(row_order, results_diabetes$outcome))

#add this to final results with 'Group' as diabetes
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    diabetes_groups),
      PYears = results_diabetes$person_years,
      Events_Midpoint10 = results_diabetes$events,
      Rate_Midpoint10_Derived = results_diabetes$incidence_rate,
      Characteristic = rep("Has Diabetes", 2 * diabetes_groups),
      Group = results_diabetes$has_diabetes)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    diabetes_groups),
      PYears = results_diabetes$person_years,
      Events_Midpoint10 = results_diabetes$events,
      Rate_Midpoint10_Derived = results_diabetes$incidence_rate,
      Characteristic = rep("Has Diabetes", 2 * diabetes_groups),
      Group = results_diabetes$has_diabetes)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    diabetes_groups),
      PYears = results_diabetes$person_years,
      Events_Midpoint10 = results_diabetes$events,
      Rate_Midpoint10_Derived = results_diabetes$incidence_rate,
      Characteristic = rep("Has Diabetes", 2 * diabetes_groups),
      Group = results_diabetes$has_diabetes)
  )
}

#calculate total person-time for each outcome type by Addison's disease
if (study_start_date == as.Date("2017-09-01")) {
  survival_addisons <- df_input %>%
    group_by(has_addisons) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_addisons <- df_input %>%
    group_by(has_addisons) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_addisons <- df_input %>%
    group_by(has_addisons) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_addisons <- unique(survival_addisons)

#reshape
survival_addisons <- survival_addisons %>%
  pivot_longer(
    cols = !has_addisons,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by Addison's disease
if (study_start_date == as.Date("2017-09-01")) {
  events_addisons <- df_input %>%
    group_by(has_addisons) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_addisons <- df_input %>%
    group_by(has_addisons) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_addisons <- df_input %>%
    group_by(has_addisons) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_addisons <- unique(events_addisons)

#reshape
events_addisons <- events_addisons %>%
  pivot_longer(
    cols = !has_addisons,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_addisons <- merge(survival_addisons, events_addisons)

#calculate incidence rate per 1000 person-years
results_addisons <- results_addisons %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
addisons_groups <- as.numeric(length(unique(
  results_addisons$has_addisons)))

#reorder rows
results_addisons <- results_addisons %>%
  group_by(has_addisons) %>%
  slice(match(row_order, results_addisons$outcome))

#add this to final results with 'Group' as Addison's disease
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    addisons_groups),
      PYears = results_addisons$person_years,
      Events_Midpoint10 = results_addisons$events,
      Rate_Midpoint10_Derived = results_addisons$incidence_rate,
      Characteristic = rep("Has Addison's Disease", 2 * addisons_groups),
      Group = results_addisons$has_addisons)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    addisons_groups),
      PYears = results_addisons$person_years,
      Events_Midpoint10 = results_addisons$events,
      Rate_Midpoint10_Derived = results_addisons$incidence_rate,
      Characteristic = rep("Has Addison's Disease", 2 * addisons_groups),
      Group = results_addisons$has_addisons)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    addisons_groups),
      PYears = results_addisons$person_years,
      Events_Midpoint10 = results_addisons$events,
      Rate_Midpoint10_Derived = results_addisons$incidence_rate,
      Characteristic = rep("Has Addison's Disease", 2 * addisons_groups),
      Group = results_addisons$has_addisons)
  )
}

#calculate total person-time for each outcome type by severe obesity
if (study_start_date == as.Date("2017-09-01")) {
  survival_severe_obesity <- df_input %>%
    group_by(severe_obesity) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_severe_obesity <- df_input %>%
    group_by(severe_obesity) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_severe_obesity <- df_input %>%
    group_by(severe_obesity) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_severe_obesity <- unique(survival_severe_obesity)

#reshape
survival_severe_obesity <- survival_severe_obesity %>%
  pivot_longer(
    cols = !severe_obesity,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by severe obesity
if (study_start_date == as.Date("2017-09-01")) {
  events_severe_obesity <- df_input %>%
    group_by(severe_obesity) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_severe_obesity <- df_input %>%
    group_by(severe_obesity) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_severe_obesity <- df_input %>%
    group_by(severe_obesity) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_severe_obesity <- unique(events_severe_obesity)

#reshape
events_severe_obesity <- events_severe_obesity %>%
  pivot_longer(
    cols = !severe_obesity,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_severe_obesity <- merge(survival_severe_obesity, events_severe_obesity)

#calculate incidence rate per 1000 person-years
results_severe_obesity <- results_severe_obesity %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
severe_obesity_groups <- as.numeric(length(unique(
  results_severe_obesity$severe_obesity)))

#reorder rows
results_severe_obesity <- results_severe_obesity %>%
  group_by(severe_obesity) %>%
  slice(match(row_order, results_severe_obesity$outcome))

#add this to final results with 'Group' as severe obesity
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    severe_obesity_groups),
      PYears = results_severe_obesity$person_years,
      Events_Midpoint10 = results_severe_obesity$events,
      Rate_Midpoint10_Derived = results_severe_obesity$incidence_rate,
      Characteristic = rep("Severely Obese", 2 * severe_obesity_groups),
      Group = results_severe_obesity$severe_obesity)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    severe_obesity_groups),
      PYears = results_severe_obesity$person_years,
      Events_Midpoint10 = results_severe_obesity$events,
      Rate_Midpoint10_Derived = results_severe_obesity$incidence_rate,
      Characteristic = rep("Severely Obese", 2 * severe_obesity_groups),
      Group = results_severe_obesity$severe_obesity)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    severe_obesity_groups),
      PYears = results_severe_obesity$person_years,
      Events_Midpoint10 = results_severe_obesity$events,
      Rate_Midpoint10_Derived = results_severe_obesity$incidence_rate,
      Characteristic = rep("Severely Obese", 2 * severe_obesity_groups),
      Group = results_severe_obesity$severe_obesity)
  )
}

#calculate total person-time for each outcome type by CHD
if (study_start_date == as.Date("2017-09-01")) {
  survival_chd <- df_input %>%
    group_by(has_chd) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_chd <- df_input %>%
    group_by(has_chd) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_chd <- df_input %>%
    group_by(has_chd) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_chd <- unique(survival_chd)

#reshape
survival_chd <- survival_chd %>%
  pivot_longer(
    cols = !has_chd,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by CHD
if (study_start_date == as.Date("2017-09-01")) {
  events_chd <- df_input %>%
    group_by(has_chd) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_chd <- df_input %>%
    group_by(has_chd) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_chd <- df_input %>%
    group_by(has_chd) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_chd <- unique(events_chd)

#reshape
events_chd <- events_chd %>%
  pivot_longer(
    cols = !has_chd,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_chd <- merge(survival_chd, events_chd)

#calculate incidence rate per 1000 person-years
results_chd <- results_chd %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
chd_groups <- as.numeric(length(unique(
  results_chd$has_chd)))

#reorder rows
results_chd <- results_chd %>%
  group_by(has_chd) %>%
  slice(match(row_order, results_chd$outcome))

#add this to final results with 'Group' as CHD
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    chd_groups),
      PYears = results_chd$person_years,
      Events_Midpoint10 = results_chd$events,
      Rate_Midpoint10_Derived = results_chd$incidence_rate,
      Characteristic = rep("Has CHD", 2 * chd_groups),
      Group = results_chd$has_chd)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    chd_groups),
      PYears = results_chd$person_years,
      Events_Midpoint10 = results_chd$events,
      Rate_Midpoint10_Derived = results_chd$incidence_rate,
      Characteristic = rep("Has CHD", 2 * chd_groups),
      Group = results_chd$has_chd)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    chd_groups),
      PYears = results_chd$person_years,
      Events_Midpoint10 = results_chd$events,
      Rate_Midpoint10_Derived = results_chd$incidence_rate,
      Characteristic = rep("Has CHD", 2 * chd_groups),
      Group = results_chd$has_chd)
  )
}

#calculate total person-time for each outcome type by CKD
if (study_start_date == as.Date("2017-09-01")) {
  survival_ckd <- df_input %>%
    group_by(has_ckd) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_ckd <- df_input %>%
    group_by(has_ckd) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_ckd <- df_input %>%
    group_by(has_ckd) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_ckd <- unique(survival_ckd)

#reshape
survival_ckd <- survival_ckd %>%
  pivot_longer(
    cols = !has_ckd,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by CKD
if (study_start_date == as.Date("2017-09-01")) {
  events_ckd <- df_input %>%
    group_by(has_ckd) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_ckd <- df_input %>%
    group_by(has_ckd) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_ckd <- df_input %>%
    group_by(has_ckd) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_ckd <- unique(events_ckd)

#reshape
events_ckd <- events_ckd %>%
  pivot_longer(
    cols = !has_ckd,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_ckd <- merge(survival_ckd, events_ckd)

#calculate incidence rate per 1000 person-years
results_ckd <- results_ckd %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
ckd_groups <- as.numeric(length(unique(
  results_ckd$has_ckd)))

#reorder rows
results_ckd <- results_ckd %>%
  group_by(has_ckd) %>%
  slice(match(row_order, results_ckd$outcome))

#add this to final results with 'Group' as CKD
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    ckd_groups),
      PYears = results_ckd$person_years,
      Events_Midpoint10 = results_ckd$events,
      Rate_Midpoint10_Derived = results_ckd$incidence_rate,
      Characteristic = rep("Has CKD", 2 * ckd_groups),
      Group = results_ckd$has_ckd)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    ckd_groups),
      PYears = results_ckd$person_years,
      Events_Midpoint10 = results_ckd$events,
      Rate_Midpoint10_Derived = results_ckd$incidence_rate,
      Characteristic = rep("Has CKD", 2 * ckd_groups),
      Group = results_ckd$has_ckd)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    ckd_groups),
      PYears = results_ckd$person_years,
      Events_Midpoint10 = results_ckd$events,
      Rate_Midpoint10_Derived = results_ckd$incidence_rate,
      Characteristic = rep("Has CKD", 2 * ckd_groups),
      Group = results_ckd$has_ckd)
  )
}

#calculate total person-time for each outcome type by CLD
if (study_start_date == as.Date("2017-09-01")) {
  survival_cld <- df_input %>%
    group_by(has_cld) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_cld <- df_input %>%
    group_by(has_cld) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_cld <- df_input %>%
    group_by(has_cld) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_cld <- unique(survival_cld)

#reshape
survival_cld <- survival_cld %>%
  pivot_longer(
    cols = !has_cld,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by CLD
if (study_start_date == as.Date("2017-09-01")) {
  events_cld <- df_input %>%
    group_by(has_cld) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_cld <- df_input %>%
    group_by(has_cld) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_cld <- df_input %>%
    group_by(has_cld) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_cld <- unique(events_cld)

#reshape
events_cld <- events_cld %>%
  pivot_longer(
    cols = !has_cld,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_cld <- merge(survival_cld, events_cld)

#calculate incidence rate per 1000 person-years
results_cld <- results_cld %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
cld_groups <- as.numeric(length(unique(
  results_cld$has_cld)))

#reorder rows
results_cld <- results_cld %>%
  group_by(has_cld) %>%
  slice(match(row_order, results_cld$outcome))

#add this to final results with 'Group' as CLD
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    cld_groups),
      PYears = results_cld$person_years,
      Events_Midpoint10 = results_cld$events,
      Rate_Midpoint10_Derived = results_cld$incidence_rate,
      Characteristic = rep("Has CLD", 2 * cld_groups),
      Group = results_cld$has_cld)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    cld_groups),
      PYears = results_cld$person_years,
      Events_Midpoint10 = results_cld$events,
      Rate_Midpoint10_Derived = results_cld$incidence_rate,
      Characteristic = rep("Has CLD", 2 * cld_groups),
      Group = results_cld$has_cld)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    cld_groups),
      PYears = results_cld$person_years,
      Events_Midpoint10 = results_cld$events,
      Rate_Midpoint10_Derived = results_cld$incidence_rate,
      Characteristic = rep("Has CLD", 2 * cld_groups),
      Group = results_cld$has_cld)
  )
}

#calculate total person-time for each outcome type by CND
if (study_start_date == as.Date("2017-09-01")) {
  survival_cnd <- df_input %>%
    group_by(has_cnd) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_cnd <- df_input %>%
    group_by(has_cnd) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_cnd <- df_input %>%
    group_by(has_cnd) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_cnd <- unique(survival_cnd)

#reshape
survival_cnd <- survival_cnd %>%
  pivot_longer(
    cols = !has_cnd,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by CND
if (study_start_date == as.Date("2017-09-01")) {
  events_cnd <- df_input %>%
    group_by(has_cnd) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_cnd <- df_input %>%
    group_by(has_cnd) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_cnd <- df_input %>%
    group_by(has_cnd) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_cnd <- unique(events_cnd)

#reshape
events_cnd <- events_cnd %>%
  pivot_longer(
    cols = !has_cnd,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_cnd <- merge(survival_cnd, events_cnd)

#calculate incidence rate per 1000 person-years
results_cnd <- results_cnd %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
cnd_groups <- as.numeric(length(unique(
  results_cnd$has_cnd)))

#reorder rows
results_cnd <- results_cnd %>%
  group_by(has_cnd) %>%
  slice(match(row_order, results_cnd$outcome))

#add this to final results with 'Group' as CND
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    cnd_groups),
      PYears = results_cnd$person_years,
      Events_Midpoint10 = results_cnd$events,
      Rate_Midpoint10_Derived = results_cnd$incidence_rate,
      Characteristic = rep("Has CND", 2 * cnd_groups),
      Group = results_cnd$has_cnd)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    cnd_groups),
      PYears = results_cnd$person_years,
      Events_Midpoint10 = results_cnd$events,
      Rate_Midpoint10_Derived = results_cnd$incidence_rate,
      Characteristic = rep("Has CND", 2 * cnd_groups),
      Group = results_cnd$has_cnd)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    cnd_groups),
      PYears = results_cnd$person_years,
      Events_Midpoint10 = results_cnd$events,
      Rate_Midpoint10_Derived = results_cnd$incidence_rate,
      Characteristic = rep("Has CND", 2 * cnd_groups),
      Group = results_cnd$has_cnd)
  )
}

#calculate total person-time for each outcome type by cancer within 3 years
if (study_start_date == as.Date("2017-09-01")) {
  survival_cancer <- df_input %>%
    group_by(has_cancer) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_cancer <- df_input %>%
    group_by(has_cancer) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_cancer <- df_input %>%
    group_by(has_cancer) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_cancer <- unique(survival_cancer)

#reshape
survival_cancer <- survival_cancer %>%
  pivot_longer(
    cols = !has_cancer,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by cancer within 3 years
if (study_start_date == as.Date("2017-09-01")) {
  events_cancer <- df_input %>%
    group_by(has_cancer) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_cancer <- df_input %>%
    group_by(has_cancer) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_cancer <- df_input %>%
    group_by(has_cancer) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_cancer <- unique(events_cancer)

#reshape
events_cancer <- events_cancer %>%
  pivot_longer(
    cols = !has_cancer,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_cancer <- merge(survival_cancer, events_cancer)

#calculate incidence rate per 1000 person-years
results_cancer <- results_cancer %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
cancer_groups <- as.numeric(length(unique(
  results_cancer$has_cancer)))

#reorder rows
results_cancer <- results_cancer %>%
  group_by(has_cancer) %>%
  slice(match(row_order, results_cancer$outcome))

#add this to final results with 'Group' as cancer within 3 years
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    cancer_groups),
      PYears = results_cancer$person_years,
      Events_Midpoint10 = results_cancer$events,
      Rate_Midpoint10_Derived = results_cancer$incidence_rate,
      Characteristic = rep("Had Cancer Within 3 Years", 2 * cancer_groups),
      Group = results_cancer$has_cancer)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    cancer_groups),
      PYears = results_cancer$person_years,
      Events_Midpoint10 = results_cancer$events,
      Rate_Midpoint10_Derived = results_cancer$incidence_rate,
      Characteristic = rep("Had Cancer Within 3 Years", 2 * cancer_groups),
      Group = results_cancer$has_cancer)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    cancer_groups),
      PYears = results_cancer$person_years,
      Events_Midpoint10 = results_cancer$events,
      Rate_Midpoint10_Derived = results_cancer$incidence_rate,
      Characteristic = rep("Had Cancer Within 3 Years", 2 * cancer_groups),
      Group = results_cancer$has_cancer)
  )
}

#calculate total person-time for each outcome type by immunosuppressed
if (study_start_date == as.Date("2017-09-01")) {
  survival_immunosuppressed <- df_input %>%
    group_by(immunosuppressed) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_immunosuppressed <- df_input %>%
    group_by(immunosuppressed) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_immunosuppressed <- df_input %>%
    group_by(immunosuppressed) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_immunosuppressed <- unique(survival_immunosuppressed)

#reshape
survival_immunosuppressed <- survival_immunosuppressed %>%
  pivot_longer(
    cols = !immunosuppressed,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by immunosuppressed
if (study_start_date == as.Date("2017-09-01")) {
  events_immunosuppressed <- df_input %>%
    group_by(immunosuppressed) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_immunosuppressed <- df_input %>%
    group_by(immunosuppressed) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_immunosuppressed <- df_input %>%
    group_by(immunosuppressed) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_immunosuppressed <- unique(events_immunosuppressed)

#reshape
events_immunosuppressed <- events_immunosuppressed %>%
  pivot_longer(
    cols = !immunosuppressed,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_immunosuppressed <- merge(survival_immunosuppressed, events_immunosuppressed)

#calculate incidence rate per 1000 person-years
results_immunosuppressed <- results_immunosuppressed %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
immunosuppressed_groups <- as.numeric(length(unique(
  results_immunosuppressed$immunosuppressed)))

#reorder rows
results_immunosuppressed <- results_immunosuppressed %>%
  group_by(immunosuppressed) %>%
  slice(match(row_order, results_immunosuppressed$outcome))

#add this to final results with 'Group' as immunosuppressed
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    immunosuppressed_groups),
      PYears = results_immunosuppressed$person_years,
      Events_Midpoint10 = results_immunosuppressed$events,
      Rate_Midpoint10_Derived = results_immunosuppressed$incidence_rate,
      Characteristic = rep("Immunosuppressed", 2 * immunosuppressed_groups),
      Group = results_immunosuppressed$immunosuppressed)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    immunosuppressed_groups),
      PYears = results_immunosuppressed$person_years,
      Events_Midpoint10 = results_immunosuppressed$events,
      Rate_Midpoint10_Derived = results_immunosuppressed$incidence_rate,
      Characteristic = rep("Immunosuppressed", 2 * immunosuppressed_groups),
      Group = results_immunosuppressed$immunosuppressed)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    immunosuppressed_groups),
      PYears = results_immunosuppressed$person_years,
      Events_Midpoint10 = results_immunosuppressed$events,
      Rate_Midpoint10_Derived = results_immunosuppressed$incidence_rate,
      Characteristic = rep("Immunosuppressed", 2 * immunosuppressed_groups),
      Group = results_immunosuppressed$immunosuppressed)
  )
}

#calculate total person-time for each outcome type by sickle cell disease
if (study_start_date == as.Date("2017-09-01")) {
  survival_sickle_cell <- df_input %>%
    group_by(has_sickle_cell) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_sickle_cell <- df_input %>%
    group_by(has_sickle_cell) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_sickle_cell <- df_input %>%
    group_by(has_sickle_cell) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_sickle_cell <- unique(survival_sickle_cell)

#reshape
survival_sickle_cell <- survival_sickle_cell %>%
  pivot_longer(
    cols = !has_sickle_cell,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by sickle cell disease
if (study_start_date == as.Date("2017-09-01")) {
  events_sickle_cell <- df_input %>%
    group_by(has_sickle_cell) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_sickle_cell <- df_input %>%
    group_by(has_sickle_cell) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_sickle_cell <- df_input %>%
    group_by(has_sickle_cell) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_sickle_cell <- unique(events_sickle_cell)

#reshape
events_sickle_cell <- events_sickle_cell %>%
  pivot_longer(
    cols = !has_sickle_cell,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_sickle_cell <- merge(survival_sickle_cell, events_sickle_cell)

#calculate incidence rate per 1000 person-years
results_sickle_cell <- results_sickle_cell %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
sickle_cell_groups <- as.numeric(length(unique(
  results_sickle_cell$has_sickle_cell)))

#reorder rows
results_sickle_cell <- results_sickle_cell %>%
  group_by(has_sickle_cell) %>%
  slice(match(row_order, results_sickle_cell$outcome))

#add this to final results with 'Group' as sickle cell disease
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    sickle_cell_groups),
      PYears = results_sickle_cell$person_years,
      Events_Midpoint10 = results_sickle_cell$events,
      Rate_Midpoint10_Derived = results_sickle_cell$incidence_rate,
      Characteristic = rep("Has Sickle Cell Disease", 2 * sickle_cell_groups),
      Group = results_sickle_cell$has_sickle_cell)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    sickle_cell_groups),
      PYears = results_sickle_cell$person_years,
      Events_Midpoint10 = results_sickle_cell$events,
      Rate_Midpoint10_Derived = results_sickle_cell$incidence_rate,
      Characteristic = rep("Has Sickle Cell Disease", 2 * sickle_cell_groups),
      Group = results_sickle_cell$has_sickle_cell)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    sickle_cell_groups),
      PYears = results_sickle_cell$person_years,
      Events_Midpoint10 = results_sickle_cell$events,
      Rate_Midpoint10_Derived = results_sickle_cell$incidence_rate,
      Characteristic = rep("Has Sickle Cell Disease", 2 * sickle_cell_groups),
      Group = results_sickle_cell$has_sickle_cell)
  )
}

#calculate total person-time for each outcome type by smoking status
if (study_start_date == as.Date("2017-09-01")) {
  survival_smoking <- df_input %>%
    group_by(smoking_status) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_smoking <- df_input %>%
    group_by(smoking_status) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_smoking <- df_input %>%
    group_by(smoking_status) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_smoking <- unique(survival_smoking)

#reshape
survival_smoking <- survival_smoking %>%
  pivot_longer(
    cols = !smoking_status,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by smoking status
if (study_start_date == as.Date("2017-09-01")) {
  events_smoking <- df_input %>%
    group_by(smoking_status) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_smoking <- df_input %>%
    group_by(smoking_status) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_smoking <- df_input %>%
    group_by(smoking_status) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_smoking <- unique(events_smoking)

#reshape
events_smoking <- events_smoking %>%
  pivot_longer(
    cols = !smoking_status,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_smoking <- merge(survival_smoking, events_smoking)

#calculate incidence rate per 1000 person-years
results_smoking <- results_smoking %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
smoking_groups <- as.numeric(length(unique(
  results_smoking$smoking_status)))

#reorder rows
results_smoking <- results_smoking %>%
  group_by(smoking_status) %>%
  slice(match(row_order, results_smoking$outcome))

#add this to final results with 'Group' as smoking status
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    smoking_groups),
      PYears = results_smoking$person_years,
      Events_Midpoint10 = results_smoking$events,
      Rate_Midpoint10_Derived = results_smoking$incidence_rate,
      Characteristic = rep("Smoking Status", 2 * smoking_groups),
      Group = results_smoking$smoking_status)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    smoking_groups),
      PYears = results_smoking$person_years,
      Events_Midpoint10 = results_smoking$events,
      Rate_Midpoint10_Derived = results_smoking$incidence_rate,
      Characteristic = rep("Smoking Status", 2 * smoking_groups),
      Group = results_smoking$smoking_status)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    smoking_groups),
      PYears = results_smoking$person_years,
      Events_Midpoint10 = results_smoking$events,
      Rate_Midpoint10_Derived = results_smoking$incidence_rate,
      Characteristic = rep("Smoking Status", 2 * smoking_groups),
      Group = results_smoking$smoking_status)
  )
}

#calculate total person-time for each outcome type by hazardous drinking
if (study_start_date == as.Date("2017-09-01")) {
  survival_drinking <- df_input %>%
    group_by(hazardous_drinking) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_drinking <- df_input %>%
    group_by(hazardous_drinking) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_drinking <- df_input %>%
    group_by(hazardous_drinking) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_drinking <- unique(survival_drinking)

#reshape
survival_drinking <- survival_drinking %>%
  pivot_longer(
    cols = !hazardous_drinking,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by hazardous drinking
if (study_start_date == as.Date("2017-09-01")) {
  events_drinking <- df_input %>%
    group_by(hazardous_drinking) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_drinking <- df_input %>%
    group_by(hazardous_drinking) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_drinking <- df_input %>%
    group_by(hazardous_drinking) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_drinking <- unique(events_drinking)

#reshape
events_drinking <- events_drinking %>%
  pivot_longer(
    cols = !hazardous_drinking,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_drinking <- merge(survival_drinking, events_drinking)

#calculate incidence rate per 1000 person-years
results_drinking <- results_drinking %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
drinking_groups <- as.numeric(length(unique(
  results_drinking$hazardous_drinking)))

#reorder rows
results_drinking <- results_drinking %>%
  group_by(hazardous_drinking) %>%
  slice(match(row_order, results_drinking$outcome))

#add this to final results with 'Group' as hazardous drinking
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    drinking_groups),
      PYears = results_drinking$person_years,
      Events_Midpoint10 = results_drinking$events,
      Rate_Midpoint10_Derived = results_drinking$incidence_rate,
      Characteristic = rep("Hazardous Drinking", 2 * drinking_groups),
      Group = results_drinking$hazardous_drinking)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    drinking_groups),
      PYears = results_drinking$person_years,
      Events_Midpoint10 = results_drinking$events,
      Rate_Midpoint10_Derived = results_drinking$incidence_rate,
      Characteristic = rep("Hazardous Drinking", 2 * drinking_groups),
      Group = results_drinking$hazardous_drinking)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    drinking_groups),
      PYears = results_drinking$person_years,
      Events_Midpoint10 = results_drinking$events,
      Rate_Midpoint10_Derived = results_drinking$incidence_rate,
      Characteristic = rep("Hazardous Drinking", 2 * drinking_groups),
      Group = results_drinking$hazardous_drinking)
  )
}

#calculate total person-time for each outcome type by drug usage
if (study_start_date == as.Date("2017-09-01")) {
  survival_drugs <- df_input %>%
    group_by(drug_usage) %>%
    transmute(
      rsv_mild = sum(time_rsv_primary, na.rm = T),
      rsv_severe = sum(time_rsv_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  survival_drugs <- df_input %>%
    group_by(drug_usage) %>%
    transmute(
      flu_mild = sum(time_flu_primary, na.rm = T),
      flu_severe = sum(time_flu_secondary, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  survival_drugs <- df_input %>%
    group_by(drug_usage) %>%
    transmute(
      covid_mild = sum(time_covid_primary, na.rm = T),
      covid_severe = sum(time_covid_secondary, na.rm = T)
    )
}

#get unique rows
survival_drugs <- unique(survival_drugs)

#reshape
survival_drugs <- survival_drugs %>%
  pivot_longer(
    cols = !drug_usage,
    names_to = "outcome",
    values_to = "person_years"
  )

#calculate total number of events for each outcome type by drug usage
if (study_start_date == as.Date("2017-09-01")) {
  events_drugs <- df_input %>%
    group_by(drug_usage) %>%
    transmute(
      rsv_mild = sum(rsv_primary_inf, na.rm = T),
      rsv_severe = sum(rsv_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  events_drugs <- df_input %>%
    group_by(drug_usage) %>%
    transmute(
      flu_mild = sum(flu_primary_inf, na.rm = T),
      flu_severe = sum(flu_secondary_inf, na.rm = T)
    )
} else if (study_start_date == as.Date("2020-09-01")) {
  events_drugs <- df_input %>%
    group_by(drug_usage) %>%
    transmute(
      covid_mild = sum(covid_primary_inf, na.rm = T),
      covid_severe = sum(covid_secondary_inf, na.rm = T)
    )
}

#get unique rows
events_drugs <- unique(events_drugs)

#reshape
events_drugs <- events_drugs %>%
  pivot_longer(
    cols = !drug_usage,
    names_to = "outcome",
    values_to = "events"
  )

#overall results
results_drugs <- merge(survival_drugs, events_drugs)

#calculate incidence rate per 1000 person-years
results_drugs <- results_drugs %>%
  mutate(events = roundmid_any(events)) %>%
  mutate(incidence_rate = events / person_years * 1000)

#get number of groups
drugs_groups <- as.numeric(length(unique(
  results_drugs$drug_usage)))

#reorder rows
results_drugs <- results_drugs %>%
  group_by(drug_usage) %>%
  slice(match(row_order, results_drugs$outcome))

#add this to final results with 'Group' as drug usage
if (study_start_date == as.Date("2017-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("RSV Mild", "RSV Severe"),
                    drugs_groups),
      PYears = results_drugs$person_years,
      Events_Midpoint10 = results_drugs$events,
      Rate_Midpoint10_Derived = results_drugs$incidence_rate,
      Characteristic = rep("Drug Usage", 2 * drugs_groups),
      Group = results_drugs$drug_usage)
  )
} else if (study_start_date == as.Date("2018-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("Flu Mild", "Flu Severe"),
                    drugs_groups),
      PYears = results_drugs$person_years,
      Events_Midpoint10 = results_drugs$events,
      Rate_Midpoint10_Derived = results_drugs$incidence_rate,
      Characteristic = rep("Drug Usage", 2 * drugs_groups),
      Group = results_drugs$drug_usage)
  )
} else if (study_start_date == as.Date("2020-09-01")) {
  final_results <- rbind(
    final_results,
    data.frame(
      Outcome = rep(c("COVID Mild", "COVID Severe"),
                    drugs_groups),
      PYears = results_drugs$person_years,
      Events_Midpoint10 = results_drugs$events,
      Rate_Midpoint10_Derived = results_drugs$incidence_rate,
      Characteristic = rep("Drug Usage", 2 * drugs_groups),
      Group = results_drugs$drug_usage)
  )
}

#export
if (study_start_date == as.Date("2018-09-01")) {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                    "IMD Quintile", "Rurality Classification",
                    "Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                    "Has Other Resp. Diseases",  "Has Diabetes",
                    "Has Addison's Disease", "Severely Obese", "Has CHD",
                    "Has CKD", "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                    "Immunosuppressed", "Has Sickle Cell Disease",
                    "Smoking Status", "Hazardous Drinking", "Drug Usage")#,
                    # "Vaccinated against influenza in previous season",
                    # "Vaccinated against influenza in current season")
} else if (study_start_date == as.Date("2020-09-01")) {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                    "IMD Quintile", "Household Composition Category",
                    "Rurality Classification", "Has Asthma", "Has COPD",
                    "Has Cystic Fibrosis", "Has Other Resp. Diseases",
                    "Has Diabetes", "Has Addison's Disease", "Severely Obese",
                    "Has CHD", "Has CKD", "Has CLD", "Has CND",
                    "Had Cancer Within 3 Years", "Immunosuppressed",
                    "Has Sickle Cell Disease", "Smoking Status",
                    "Hazardous Drinking", "Drug Usage")#,
                    # "Vaccinated against COVID-19 in current season")
} else {
  table_groups <- c("Total", "Age Group", "Sex", "Ethnicity",
                    "IMD Quintile", "Rurality Classification",
                    "Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                    "Has Other Resp. Diseases", "Has Diabetes",
                    "Has Addison's Disease", "Severely Obese", "Has CHD",
                    "Has CKD", "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                    "Immunosuppressed", "Has Sickle Cell Disease",
                    "Smoking Status", "Hazardous Drinking", "Drug Usage")
}

## create output directories ----
fs::dir_create(here::here("output", "results", "rates"))

#export results table to csv
if (length(args) == 0) {
  results_table <- final_results %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    select(Outcome, Characteristic, Group, PYears, Events_Midpoint10,
           Rate_Midpoint10_Derived) %>%
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
    select(Outcome, Characteristic, Group, PYears, Events_Midpoint10,
           Rate_Midpoint10_Derived) %>%
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
