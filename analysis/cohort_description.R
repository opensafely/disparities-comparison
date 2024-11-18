library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(gtsummary)
library(stringr)
library(purrr)

## create output directories ----
fs::dir_create(here::here("analysis"))

#import redaction functions
source(here::here("analysis", "functions", "redaction.R"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants_subgroup"
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

df_input <- read_feather(
  here::here("output", "data", paste0("cohort_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#set all NA categories to "Unknown"
df_input <- df_input %>% mutate_if(is.factor,
                                   forcats::fct_explicit_na,
                                   na_level = "Unknown")

if (study_start_date == as.Date("2020-09-01")) {
  if (cohort == "infants") {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             composition_category, rurality_classification) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, "Household Composition" = composition_category,
             Rurality = rurality_classification)
  } else if (cohort == "infants_subgroup") {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             composition_category, rurality_classification, maternal_age,
             maternal_smoking_status, maternal_drinking, maternal_drug_usage,
             maternal_flu_vaccination, maternal_pertussis_vaccination) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, "Household Composition" = composition_category,
             Rurality = rurality_classification, "Maternal Age" = maternal_age,
             "Maternal Smoking Status" = maternal_smoking_status,
             "Maternal Drinking" = maternal_drinking,
             "Maternal Drug Usage" = maternal_drug_usage,
             "Maternal Flu Vaccination" = maternal_flu_vaccination,
             "Maternal Pertussis Vaccination" = maternal_pertussis_vaccination)
  } else if (cohort == "children_and_adolescents") {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      mutate(Reactive_Airway = ifelse(age <= 5, has_asthma_reactive_airway, "No")) %>%
      mutate(Asthma = ifelse(age > 5, has_asthma_reactive_airway, "No")) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             composition_category, rurality_classification, Asthma,
             Reactive_Airway, has_asthma_reactive_airway, prior_flu_vaccination) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, "Household Composition" = composition_category,
             Rurality = rurality_classification, "Reactive Airway" = Reactive_Airway,
             "Asthma or Reactive Airway" = has_asthma_reactive_airway,
             "Prior Flu Vaccine" = prior_flu_vaccination)
    if (study_start_date >= covid_prior_vacc_min) {
      table <- table %>%
        mutate(time_since_last_covid_vaccination = df_input$time_since_last_covid_vaccination) %>%
        rename("Time Since Last Covid Vaccine" = time_since_last_covid_vaccination)
    }
  } else {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             composition_category, rurality_classification, smoking_status,
             hazardous_drinking, drug_usage, has_asthma, has_copd,
             has_cystic_fibrosis, has_other_resp, has_diabetes, has_addisons,
             severe_obesity, has_chd, has_ckd, has_cld, has_cnd, has_cancer,
             immunosuppressed, has_sickle_cell, prior_flu_vaccination) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, "Household Composition" = composition_category,
             Rurality = rurality_classification, "Smoking Status" = smoking_status,
             "Hazardous Drinking" = hazardous_drinking, "Drug Usage" = drug_usage,
             Asthma = has_asthma, COPD = has_copd,
             "Cystic Fibrosis" = has_cystic_fibrosis,
             "Other Chronic Respiratory Diseases" = has_other_resp,
             Diabetes = has_diabetes, Addisons = has_addisons,
             "Severe Obesity" = severe_obesity, "Chronic Heart Diseases" = has_chd,
             "Chronic Kidney Disease" = has_ckd, "Chronic Liver Disease" = has_cld,
             "Chronic Neurological Disease" = has_cnd,
             "Cancer Within 3 Years" = has_cancer,
             Immunosuppressed = immunosuppressed,
             "Sickle Cell Disease" = has_sickle_cell,
             "Prior Flu Vaccine" = prior_flu_vaccination)
    if (study_start_date >= covid_prior_vacc_min) {
      table <- table %>%
        mutate(time_since_last_covid_vaccination = df_input$time_since_last_covid_vaccination) %>%
        rename("Time Since Last Covid Vaccine" = time_since_last_covid_vaccination)
    }
   }
} else {
  if (cohort == "infants") {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             rurality_classification) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, Rurality = rurality_classification)
  } else if (cohort == "infants_subgroup") {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             rurality_classification, maternal_age, maternal_smoking_status,
             maternal_drinking, maternal_drug_usage, maternal_flu_vaccination,
             maternal_pertussis_vaccination) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, Rurality = rurality_classification,
             "Maternal Age" = maternal_age,
             "Maternal Smoking Status" = maternal_smoking_status,
             "Maternal Drinking" = maternal_drinking,
             "Maternal Drug Usage" = maternal_drug_usage,
             "Maternal Flu Vaccination" = maternal_flu_vaccination,
             "Maternal Pertussis Vaccination" = maternal_pertussis_vaccination)
  } else if (cohort == "children_and_adolescents") {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      mutate(Reactive_Airway = ifelse(age <= 5, has_asthma_reactive_airway, "No")) %>%
      mutate(Asthma = ifelse(age > 5, has_asthma_reactive_airway, "No")) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             rurality_classification, Asthma, Reactive_Airway,
             has_asthma_reactive_airway, prior_flu_vaccination) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, Rurality = rurality_classification,
             "Reactive Airway" = Reactive_Airway,
             "Asthma or Reactive Airway" = has_asthma_reactive_airway,
             "Prior Flu Vaccine" = prior_flu_vaccination)
    if (study_start_date >= covid_prior_vacc_min) {
      table <- table %>%
        mutate(time_since_last_covid_vaccination = df_input$time_since_last_covid_vaccination)  %>%
        rename("Time Since Last Covid Vaccine" = time_since_last_covid_vaccination)
    }
  } else {
    table <- df_input %>%
      mutate(Total = n_distinct(patient_id)) %>%
      select(Total, age_band, sex, latest_ethnicity_group, imd_quintile,
             rurality_classification, smoking_status, hazardous_drinking,
             drug_usage, has_asthma, has_copd, has_cystic_fibrosis,
             has_other_resp, has_diabetes, has_addisons, severe_obesity,
             has_chd, has_ckd, has_cld, has_cnd, has_cancer, immunosuppressed,
             has_sickle_cell, prior_flu_vaccination) %>%
      rename("Age Group" = age_band, Sex = sex, Ethnicity = latest_ethnicity_group,
             IMD = imd_quintile, Rurality = rurality_classification,
             "Smoking Status" = smoking_status,
             "Hazardous Drinking" = hazardous_drinking, "Drug Usage" = drug_usage,
             Asthma = has_asthma, COPD = has_copd,
             "Cystic Fibrosis" = has_cystic_fibrosis,
             "Other Chronic Respiratory Diseases" = has_other_resp,
             Diabetes = has_diabetes, Addisons = has_addisons,
             "Severe Obesity" = severe_obesity, "Chronic Heart Diseases" = has_chd,
             "Chronic Kidney Disease" = has_ckd, "Chronic Liver Disease" = has_cld,
             "Chronic Neurological Disease" = has_cnd,
             "Cancer Within 3 Years" = has_cancer,
             Immunosuppressed = immunosuppressed,
             "Sickle Cell Disease" = has_sickle_cell,
             "Prior Flu Vaccine" = prior_flu_vaccination)
    if (study_start_date >= covid_prior_vacc_min) {
      table <- table %>%
        mutate(time_since_last_covid_vaccination = df_input$time_since_last_covid_vaccination) %>%
        rename("Time Since Last Covid Vaccine" = time_since_last_covid_vaccination)
    }
  }
}

## create output directories ----
fs::dir_create(here::here("output", "table1"))

#export
# table %>%
#   tbl_summary() %>% 
#   as_gt() %>%
#   gt::gtsave(filename = paste0("table1_", cohort, "_", year(study_start_date),
#              "_", year(study_end_date), ".html"), 
#              path = here::here("output", "table1"))
theme_gtsummary_language("en", big.mark = "")
stats <- c("N" = "{n}",
           "%" = "{p}%")
if (length(args) == 0) {
  purrr::imap(
    stats,
    ~table %>%
      tbl_summary(statistic = ~.x) %>%
      modify_header(all_stat_cols() ~ stringr::str_glue("**{.y}**"))
  ) %>%
  tbl_merge(tab_spanner = FALSE) %>%
  modify_footnote(~NA) %>%
  as_tibble() %>%
  write_csv(file = paste0(here::here("output", "table1"), "/", "table1_", 
                          cohort, "_", year(study_start_date), "_",
                          year(study_end_date),".csv"))
} else {
  tab1 <- table %>% tbl_summary(statistic = list(all_categorical() ~ "{n}")) %>%
    modify_header(stat_0 = "N") %>%
    as_tibble()
  tab2 <- table %>% tbl_summary(statistic = list(all_categorical() ~ "{p}%")) %>%
    # modify_footnote(~NA) %>%
    modify_header(stat_0 = "%") %>%
    as_tibble()
  cbind(tab1, tab2) %>%
    select("**Characteristic**", "N", "%") %>%
  write_csv(path = paste0(here::here("output", "table1"), "/", "table1_", 
                          cohort, "_", year(study_start_date), "_",
                          year(study_end_date),".csv"))
}
