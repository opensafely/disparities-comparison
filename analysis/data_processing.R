library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(lubridate)
library(magrittr)

## create output directories ----
fs::dir_create(here::here("analysis"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2016-09-01")
  study_end_date <- as.Date("2017-08-31")
  cohort <- "adults"
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
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type,".arrow")))

max(df_input$patient_index_date)

if (study_start_date == as.Date("2020-09-01") &
    cohort != "infants" & cohort != "infants_subgroup") {
  
  df_household <- read_feather(
    here::here("output", "data", paste0("input_household_processed_", 
               year(study_start_date), "_", year(study_end_date), ".arrow")))
  
  household_comp_vars <- tibble(
    "patient_id" = df_household$patient_id,
    "num_generations"= df_household$num_generations,
    "composition_category" = df_household$composition_category)
  
  df_input <- merge(df_input, household_comp_vars, by = "patient_id")
  
}

if (cohort == "infants_subgroup") {
  df_input_mothers <- read_feather(here::here("output", "data", 
                                   paste0("input_maternal_infants_subgroup_",
                                   year(study_start_date), "_",
                                   year(study_end_date), "_", codelist_type,
                                   "_", investigation_type,".arrow")))
  df_input_mothers <- df_input_mothers %>%
    mutate(mother_id = patient_id) %>%
    select(-patient_id)
  df_input <- merge(df_input, df_input_mothers, by = "mother_id")
  print(nrow(df_input))
}

#adjust 'patient_end_date' to account for death and deregistration
df_input <- df_input %>%
  mutate(
    patient_end_date = pmin(patient_end_date, death_date, deregistration_date,
                            na.rm = TRUE),
    patient_index_date = patient_index_date - days(1)
  )

max(df_input$patient_index_date)

#create time dependency
if(cohort == "infants" | cohort == "infants_subgroup") {
  df_input <- df_input %>%
    rowwise() %>%
    mutate(
      date = map2(patient_index_date, patient_end_date, ~seq.Date(.x, .y, by = "month"))
    ) %>%
    unnest(date) %>%
    mutate(
      age = age + as.numeric((date - patient_index_date)/30.44)
    ) %>%
    ungroup()
}

max(df_input$patient_index_date)
length(unique(df_input$patient_id))

#calculate age bands
if(cohort == "older_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age > 64 & age < 75 ~ "65-74y",
      age > 74 & age < 90 ~ "75-89y",
      age > 89 ~ "90y+",
      TRUE ~ NA_character_)
    )
} else if(cohort == "adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age > 17 & age < 40 ~ "18-39y",
      age > 39 & age < 65 ~ "40-64y",
      TRUE ~ NA_character_)
    )
} else if(cohort == "children_and_adolescents") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age > 1 & age < 6 ~ "2-5y",
      age > 5 & age < 10 ~ "6-9y",
      age > 9 & age < 14 ~ "10-13y",
      age > 13 & age < 18 ~ "14-17y",
      TRUE ~ NA_character_)
    )
} else {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 0 & age < 3 ~ "0-2m",
      age > 2 & age < 6 ~ "3-5m",
      age > 5 & age < 12 ~ "6-11m",
      age > 11 & age < 24 ~ "12-23m",
      TRUE ~ NA_character_)
    ) %>%
    filter(!is.na(age_band))
}

df_input$age_band <- factor(df_input$age_band)

max(df_input$patient_index_date)

#data manipulation
df_input <- df_input %>%
  mutate(
    #assign ethnicity group
    latest_ethnicity_group = fct_relevel(latest_ethnicity_group,
                                         c("1", "2", "3", "4", "5")),
    #calculate IMD quintile
    imd_quintile = factor(case_when(
      imd_rounded >= 0 & imd_rounded < as.integer(32800 * 1 / 5) ~ "1 (most deprived)",
      imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
      imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
      imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
      imd_rounded <= as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
      TRUE ~ NA_character_)),
    #format sex
    sex = relevel(factor(case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      TRUE ~ NA_character_)), ref = "Female")
  )

#identify columns with logical values, excluding specified columns
logical_cols <- which(sapply(df_input, is.logical) &
                      !grepl("primary|secondary|mortality|registered",
                             names(df_input)))

max(df_input$patient_index_date)

#apply mutation to convert logical columns to factors
df_input <- df_input %>%
  mutate(across(
    .cols = all_of(logical_cols), 
    .fns = ~relevel(factor(case_when(
      . == FALSE ~ "No",
      . == TRUE ~ "Yes",
      TRUE ~ NA_character_
    )), ref = "No")
  ))

#more data manipulation
df_input <- df_input %>%
  mutate(
    #add labels to ethnicity
    latest_ethnicity_group = relevel(factor(latest_ethnicity_group,
                                     levels = c("1", "2", "3", "4", "5"),
                                     labels = c("White", "Mixed",
                                                "Asian or Asian British",
                                                "Black or Black British",
                                                "Other Ethnic Groups"),
                                     ordered = FALSE), ref = "White"),
    #recode imd quintile 
    imd_quintile = relevel(recode(imd_quintile,
                           "1 (most deprived)" = "5 (most deprived)",
                           "2" = "4", "3" = "3", "4" = "2",
                           "5 (least deprived)" = "1 (least deprived)"),
                           ref = "1 (least deprived)"),
    #recode rurality to 5 levels
    rurality_code = recode(rural_urban_classification, "1" = "1", "2" = "2", 
                           "3" = "3", "4" = "3", "5" = "4", "6" = "4", 
                           "7" = "5", "8" = "5", .missing = NA_character_),
    #assign rurality classification
    rurality_classification = factor(case_when(
      rurality_code == "1" ~ "Urban Major Conurbation",
      rurality_code == "2" ~ "Urban Minor Conurbation",
      rurality_code == "3" ~ "Urban City and Town",
      rurality_code == "4" ~ "Rural Town and Fringe",
      rurality_code == "5" ~ "Rural Village and Dispersed",
      TRUE ~ NA_character_))
  )

max(df_input$patient_index_date)

#ethnicity from HES
if (cohort == "infants" | cohort == "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      latest_ethnicity_group_hes = relevel(factor(recode(
        latest_ethnicity_group_hes, "A" = "White", "B" = "White", "C" = "White",
        "D" = "Mixed", "E" = "Mixed", "F" = "Mixed", "G" = "Mixed",
        "H" = "Asian or Asian British", "J" = "Asian or Asian British",
        "K" = "Asian or Asian British", "L" = "Asian or Asian British",
        "M" = "Black or Black British", "N" = "Black or Black British",
        "P" = "Black or Black British", "R" = "Other Ethnic Groups",
        "S" = "Other Ethnic Groups"), ordered = F), ref = "White")
    ) %>%
    mutate(
      latest_ethnicity_group = if_else(is.na(latest_ethnicity_group),
                                       latest_ethnicity_group_hes,
                                       latest_ethnicity_group)
    )
}

max(df_input$patient_index_date)

#household variables for when they are included (2020-21)
if (study_start_date == as.Date("2020-09-01") &
    cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #define household size categories
      household_size_cat = factor(case_when(
        household_size >= 1 & household_size <= 2 ~ "1",
        household_size >= 3 & household_size <= 5 ~ "2",
        household_size >= 6 ~ "3",
        TRUE ~ NA_character_)),
      composition_category = fct_relevel(composition_category,
                                         c("Multiple of the Same Generation",
                                           "Living Alone", "One Other Generation",
                                           "Two Other Generations",
                                           "Three Other Generations"))
    ) %>% arrange(composition_category)
}

#maternal characteristics
if (cohort == "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #create smoking status factor
      maternal_smoking_status = relevel(factor(maternal_smoking_status,
                                        ordered = FALSE), ref = "Never")
    )
}

#flu vaccination
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #assign flu vaccination status
      flu_vaccination_immunity_date = if_else(
        flu_vaccination_date + days(10) < patient_end_date,
        flu_vaccination_date + days(10), NA_Date_)
    )
}

#define two vaccinations categories for each outcome type, set vaccination to null
#if immunity date occurs after outcome date
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #define flu_vaccination_mild
      flu_vaccination_mild = relevel(factor(case_when(
        flu_vaccination_immunity_date > flu_primary_date ~ "No",
        is.na(flu_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")), ref = "No"),
      #define flu_vaccination severe
      flu_vaccination_severe = relevel(factor(case_when(
        flu_vaccination_immunity_date > flu_secondary_date ~ "No",
        is.na(flu_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")), ref = "No")
    )
}

#covid vaccination 
if (study_start_date >= covid_prior_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      time_since_last_covid_vaccination = relevel(factor(case_when(
        time_length(difftime(patient_index_date + days(1),
                             last_covid_vaccination_date, 
                             units = "days"), "months") >= 0 &
          time_length(difftime(patient_index_date + days(1),
                               last_covid_vaccination_date,
                               units = "days"), "months") < 6 ~ "0-6m",
        time_length(difftime(patient_index_date + days(1),
                             last_covid_vaccination_date,
                             units = "days"), "months") >= 6 &
          time_length(difftime(patient_index_date + days(1),
                               last_covid_vaccination_date, 
                               units = "days"), "months") < 12 ~ "6-12m",
        time_length(difftime(patient_index_date + days(1),
                             last_covid_vaccination_date,
                             units = "days"), "months") >= 12 ~ "12m+",
        TRUE ~ "12m+")), ref = "0-6m")
    )
}
if (study_start_date >= covid_current_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>% 
    mutate(
      covid_vaccination_immunity_date = if_else(
        covid_vaccination_date + days(10) < patient_end_date,
        covid_vaccination_date + days(10), NA_Date_)
    )
}

#define two vaccinations categories for each outcome type, set vaccination to null
#if immunity date occurs after outcome date
if (study_start_date >= covid_current_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #define covid_vaccination_mild
      covid_vaccination_mild = relevel(factor(case_when(
        covid_vaccination_immunity_date > covid_primary_date ~ "No",
        is.na(covid_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")), ref = "No"),
      #define covid_vaccination severe
      covid_vaccination_severe = relevel(factor(case_when(
        covid_vaccination_immunity_date > covid_secondary_date ~ "No",
        is.na(covid_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")), ref = "No")
    )
}

max(df_input$patient_index_date)

#set covid date to missing if existing date occurs before March 1st 2020
if (study_start_date >= covid_season_min) {
  df_input <- df_input %>%
    mutate(
      covid_primary_date = if_else(
        covid_primary_date < as.Date("2020-03-01"),
        NA_Date_, covid_primary_date),
      covid_primary_second_date = if_else(
        covid_primary_date < as.Date("2020-03-01"),
        NA_Date_, covid_primary_second_date),
      covid_secondary_date = if_else(
        covid_secondary_date < as.Date("2020-03-01"),
        NA_Date_, covid_secondary_date),
      covid_secondary_second_date = if_else(
        covid_secondary_date < as.Date("2020-03-01"),
        NA_Date_, covid_secondary_second_date)
    )
}

#infer outcomes from event dates 
df_input <- df_input %>%
  mutate(
    #infer presence of mild rsv
    rsv_primary = if_else(
      !is.na(rsv_primary_date), TRUE, FALSE),
    #infer presence of second episode of mild rsv
    rsv_primary_second = if_else(
      !is.na(rsv_primary_second_date), TRUE, FALSE),
    #infer presence of severe rsv
    rsv_secondary = if_else(
      !is.na(rsv_secondary_date), TRUE, FALSE),
    #infer presence of second episode of severe rsv
    rsv_secondary_second = if_else(
      !is.na(rsv_secondary_second_date), TRUE, FALSE),
    #infer presence of mild flu
    flu_primary = if_else(
      !is.na(flu_primary_date), TRUE, FALSE),
    #infer presence of second episode of mild flu
    flu_primary_second = if_else(
      !is.na(flu_primary_second_date), TRUE, FALSE),
    #infer presence of severe flu
    flu_secondary = if_else(
      !is.na(flu_secondary_date), TRUE, FALSE),
    #infer presence of second episode of severe flu
    flu_secondary_second = if_else(
      !is.na(flu_secondary_second_date), TRUE, FALSE)
  )

if (study_start_date >= covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #infer presence of mild covid
      covid_primary = if_else(
        !is.na(covid_primary_date), TRUE, FALSE),
      #infer presence of second episode of mild covid
      covid_primary_second = if_else(
        !is.na(covid_primary_second_date), TRUE, FALSE),
      #infer presence of severe covid
      covid_secondary = if_else(
        !is.na(covid_secondary_date), TRUE, FALSE),
      #infer presence of second episode of severe covid
      covid_secondary_second = if_else(
        !is.na(covid_secondary_second_date), TRUE, FALSE)
    )
}

if (codelist_type == "sensitive") {
  df_input <- df_input %>%
    mutate(
      #infer presence of mild overall respiratory
      overall_resp_primary = if_else(
        !is.na(overall_resp_primary_date), TRUE, FALSE),
      #infer presence of second episode of mild overall respiratory
      overall_resp_primary_second = if_else(
        !is.na(overall_resp_primary_second_date), TRUE, FALSE),
      #infer presence of severe overall respiratory
      overall_resp_secondary = if_else(
        !is.na(overall_resp_secondary_date), TRUE, FALSE),
      #infer presence of second episode of severe overall respiratory
      overall_resp_secondary_second = if_else(
        !is.na(overall_resp_secondary_second_date), TRUE, FALSE)
    )
}

#define event time 
if (study_start_date < covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #infer mild case date for rsv 
      rsv_primary_inf_date = pmin(
        rsv_primary_date, rsv_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      rsv_primary_censor = if_else(
        rsv_primary_inf_date < rsv_primary_date, 1, 0, missing = 1),
      #infer mild rsv outcome 
      rsv_primary_inf = if_else(rsv_primary_censor == 0, 1, 0),
      #infer severe case date for rsv
      rsv_secondary_inf_date = pmin(
        rsv_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      rsv_secondary_censor = if_else(
        rsv_secondary_inf_date < rsv_secondary_date, 1, 0, missing = 1),
      #infer severe rsv outcome
      rsv_secondary_inf = if_else(rsv_secondary_censor == 0, 1, 0),
      #infer mild case date for flu
      flu_primary_inf_date = pmin(
        flu_primary_date, flu_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      flu_primary_censor = if_else(
        flu_primary_inf_date < flu_primary_date, 1, 0, missing = 1),
      #infer mild flu outcome
      flu_primary_inf = if_else(flu_primary_censor == 0, 1, 0),
      #infer severe case date for flu
      flu_secondary_inf_date = pmin(
        flu_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      flu_secondary_censor = if_else(
        flu_secondary_inf_date < flu_secondary_date, 1, 0, missing = 1),
      #infer severe flu outcome
      flu_secondary_inf = if_else(flu_secondary_censor == 0, 1, 0)
    )
  #for sensitive analyses define overall respiratory outcomes
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #infer mild case date for overall respiratory
        overall_resp_primary_inf_date = pmin(
          overall_resp_primary_date, overall_resp_secondary_date,
          deregistration_date, death_date, patient_end_date, na.rm = TRUE),
        #assign censoring indicator
        overall_resp_primary_censor = if_else(
          overall_resp_primary_inf_date < overall_resp_primary_date, 1, 0,
          missing = 1),
        #infer mild overall respiratory outcome
        overall_resp_primary_inf = if_else(
          overall_resp_primary_censor == 0, 1, 0),
        #infer severe case date for overall respiratory
        overall_resp_secondary_inf_date = pmin(
          overall_resp_secondary_date, deregistration_date,
          death_date, patient_end_date, na.rm = TRUE),
        #assign censoring indicator
        overall_resp_secondary_censor = if_else(
          overall_resp_secondary_inf_date < overall_resp_secondary_date,
          1, 0, missing = 1),
        #infer severe overall respiratory outcome
        overall_resp_secondary_inf = if_else(
          overall_resp_secondary_censor == 0, 1, 0)
      )
  }
} else {
  df_input <- df_input %>%
    mutate(
      #infer mild case date for rsv
      rsv_primary_inf_date = pmin(
        rsv_primary_date, rsv_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      rsv_primary_censor = if_else(
        rsv_primary_inf_date < rsv_primary_date, 1, 0, missing = 1),
      #infer mild rsv outcome 
      rsv_primary_inf = if_else(rsv_primary_censor == 0, 1, 0),
      #infer severe case date for rsv
      rsv_secondary_inf_date = pmin(
        rsv_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      rsv_secondary_censor = if_else(
        rsv_secondary_inf_date < rsv_secondary_date, 1, 0, missing = 1),
      #infer severe rsv outcome
      rsv_secondary_inf = if_else(rsv_secondary_censor == 0, 1, 0),
      #infer mild case date for flu
      flu_primary_inf_date = pmin(
        flu_primary_date, flu_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      flu_primary_censor = if_else(
        flu_primary_inf_date < flu_primary_date, 1, 0, missing = 1),
      #infer mild flu outcome
      flu_primary_inf = if_else(flu_primary_censor == 0, 1, 0),
      #infer severe case date for flu
      flu_secondary_inf_date = pmin(
        flu_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      flu_secondary_censor = if_else(
        flu_secondary_inf_date < flu_secondary_date, 1, 0, missing = 1),
      #infer severe flu outcome
      flu_secondary_inf = if_else(flu_secondary_censor == 0, 1, 0),
      #infer mild case date for covid
      covid_primary_inf_date = pmin(
        covid_primary_date, covid_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      covid_primary_censor = if_else(
        covid_primary_inf_date < covid_primary_date, 1, 0, missing = 1),
      #infer mild covid outcome
      covid_primary_inf = if_else(covid_primary_censor == 0, 1, 0),
      #infer severe case date for covid
      covid_secondary_inf_date = pmin(
        covid_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      covid_secondary_censor = if_else(
        covid_secondary_inf_date < covid_secondary_date, 1, 0, missing = 1),
      #infer severe covid outcome
      covid_secondary_inf = if_else(covid_secondary_censor == 0, 1, 0)
    )
  #for sensitive analyses define overall respiratory outcomes
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #infer mild case date for overall respiratory
        overall_resp_primary_inf_date = pmin(
          overall_resp_primary_date, overall_resp_secondary_date,
          deregistration_date, death_date, patient_end_date, na.rm = TRUE),
        #assign censoring indicator
        overall_resp_primary_censor = if_else(
          overall_resp_primary_inf_date < overall_resp_primary_date, 1, 0,
          missing = 1),
        #infer overall respiratory outcome
        overall_resp_primary_inf = if_else(
          overall_resp_primary_censor == 0, 1, 0),
        #infer severe case date for overall respiratory
        overall_resp_secondary_inf_date = pmin(
          overall_resp_secondary_date, deregistration_date,
          death_date, patient_end_date, na.rm = TRUE),
        #assign censoring indicator
        overall_resp_secondary_censor = if_else(
          overall_resp_secondary_inf_date < overall_resp_secondary_date,
          1, 0, missing = 1),
        #infer severe overall respiratory outcome
        overall_resp_secondary_inf = if_else(
          overall_resp_secondary_censor == 0, 1, 0)
      )
  }
}

#calculate time to event
if (study_start_date < covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #time until mild rsv outcome
      time_rsv_primary = time_length(difftime(rsv_primary_inf_date, 
                         patient_index_date, "weeks"), "years"),
      #time until severe rsv outcome
      time_rsv_secondary = time_length(difftime(rsv_secondary_inf_date, 
                           patient_index_date, "weeks"), "years"),
      #time until mild flu outcome
      time_flu_primary = time_length(difftime(flu_primary_inf_date, 
                         patient_index_date, "weeks"), "years"),
      #time until severe flu outcome
      time_flu_secondary = time_length(difftime(flu_secondary_inf_date, 
                           patient_index_date, "weeks"), "years")
    )
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #time until mild overall respiratory outcome
        time_overall_resp_primary = time_length(
          difftime(overall_resp_primary_inf_date, patient_index_date,
                   "weeks"), "years"),
        #time until severe overall respiratory outcome
        time_overall_resp_secondary = time_length(
          difftime(overall_resp_secondary_inf_date, patient_index_date,
                   "weeks"), "years")
      )
  }
} else {
  df_input <- df_input %>%
    mutate(
      #time until mild rsv outcome
      time_rsv_primary = time_length(difftime(rsv_primary_inf_date, 
                         patient_index_date, "weeks"), "years"),
      #time until severe rsv outcome
      time_rsv_secondary = time_length(difftime(rsv_secondary_inf_date, 
                           patient_index_date, "weeks"), "years"),
      #time until mild flu outcome
      time_flu_primary = time_length(difftime(flu_primary_inf_date, 
                         patient_index_date, "weeks"), "years"),
      #time until severe flu outcome
      time_flu_secondary = time_length(difftime(flu_secondary_inf_date, 
                           patient_index_date, "weeks"), "years"),
      #time until mild covid outcome
      time_covid_primary = time_length(difftime(covid_primary_inf_date, 
                           patient_index_date, "weeks"), "years"),
      #time until severe covid outcome
      time_covid_secondary = time_length(difftime(covid_secondary_inf_date, 
                             patient_index_date, "weeks"), "years")
    )
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #time until mild overall respiratory outcome
        time_overall_resp_primary = time_length(
          difftime(overall_resp_primary_inf_date, patient_index_date,
                   "weeks"), "years"),
        #time until severe overall respiratory outcome
        time_overall_resp_secondary = time_length(
          difftime(overall_resp_secondary_inf_date, patient_index_date,
                   "weeks"), "years")
      )
  }
}

max(df_input$patient_index_date)
length(unique(df_input$patient_id))

## create output directories ----
fs::dir_create(here::here("output", "data"))

#write the new input file
write_feather(df_input, here::here("output", "data", 
              paste0("input_processed_", cohort, "_", year(study_start_date),
              "_", year(study_end_date), "_", codelist_type, 
              "_", investigation_type, ".arrow")))
