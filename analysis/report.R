library("tidyverse")
library("here")
library("arrow")
library("ggplot2")
library("data.table")
library("gtsummary")

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
study_start_date <- study_dates[[args[[2]]]]
study_end_date <- study_dates[[args[[3]]]]
cohort <- args[[1]]
codelist_type <- args[[4]]
investigation_type <- args[[5]]
covid_season_min <- as.Date("2019-09-01")

df_input <- read_feather(
  here::here("output", paste0("input_processed_", cohort, "_", year(study_start_date),
             "_", year(study_end_date), "_", codelist_type, "_", 
             investigation_type,".arrow")))

lab <- ifelse(cohort == "infants", "Age (Months)", 
       ifelse(cohort == "infants_subgroup", "Age (Months)", "Age (Years)"))

plot_age <- ggplot(data = df_input, aes(age, frequency(age))) + geom_col(width = 0.9) +
  xlab(lab) + ylab("Frequency")

ggsave(
  plot = plot_age,
  filename = paste0("descriptive_", cohort, "_", year(study_start_date),
    "_", year(study_end_date), "_", codelist_type, "_", 
    investigation_type,".png"), path = here::here("output"),
)

df_datatable <- as.data.table(df_input)

if (cohort == "infants") {
  table <- df_datatable[registered == TRUE, Total := n_distinct(patient_id)]
  table <- df_datatable[registered == TRUE, .(Total, age_band, sex, 
                        latest_ethnicity_group, imd_quintile, 
                        rurality_classification)]
  setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
           "imd_quintile", "rurality_classification"),
           c("Age Group", "Sex", "Ethnicity", "IMD", "Rurality"))
} else if (cohort == "children_and_adolescents") {
  if(investigation_type == "primary") {
    table <- df_datatable[registered == TRUE, Total := n_distinct(patient_id)]
    table <- df_datatable[registered == TRUE, .(Total, age_band, sex, 
                          latest_ethnicity_group, imd_quintile, 
                          rurality_classification, flu_vaccination)]
    setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
                      "imd_quintile", "rurality_classification",
                      "flu_vaccination"),
              c("Age Group", "Sex", "Ethnicity", "IMD", "Rurality",
              "Flu Vaccine"))
    if (study_start_date >= covid_season_min) {
      table[, covid_vaccination_count := df_datatable$covid_vaccination_count]
      setnames(table, "covid_vaccination_count", "Covid Vaccine Doses")
    }
  } else {
    table <- df_datatable[registered == TRUE, Total := n_distinct(patient_id)]
    table <- df_datatable[registered == TRUE, Reactive_Airway := ifelse(age <= 5, has_asthma_reactive_airway, F)]
    table <- df_datatable[registered == TRUE, Asthma := ifelse(age > 5, has_asthma_reactive_airway, F)]
    table <- df_datatable[registered == TRUE, .(Total, age_band, sex, 
                          latest_ethnicity_group, imd_quintile, 
                          rurality_classification, Asthma, Reactive_Airway,
                          flu_vaccination)]
    setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
                      "imd_quintile", "rurality_classification",
                      "Reactive_Airway", "flu_vaccination"),
             c("Age Group", "Sex", "Ethnicity", "IMD", "Rurality",
               "Reactive Airway", "Flu Vaccine"))
    if (study_start_date >= covid_season_min) {
      table[, covid_vaccination_count := df_datatable$covid_vaccination_count]
      setnames(table, "covid_vaccination_count", "Covid Vaccine Doses")
    }
  }
} else {
  if (investigation_type == "primary") {
    table <- df_datatable[registered == TRUE, Total := n_distinct(patient_id)]
    table <- df_datatable[registered == TRUE, .(Total, age_band, sex, 
                          latest_ethnicity_group, imd_quintile, 
                          rurality_classification, flu_vaccination)]
    setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
                      "imd_quintile", "rurality_classification",
                      "flu_vaccination"),
             c("Age Group", "Sex", "Ethnicity", "IMD", "Rurality",
               "Flu Vaccine"))
    if (study_start_date >= covid_season_min) {
      table[, covid_vaccination_count := df_datatable$covid_vaccination_count]
      setnames(table, "covid_vaccination_count", "Covid Vaccine Doses")
    }
  } else {
    table <- df_datatable[registered == TRUE, Total := n_distinct(patient_id)]
    table <- df_datatable[registered == TRUE, .(Total, age_band, sex, 
                          latest_ethnicity_group, imd_quintile, 
                          rurality_classification, smoking_status,
                          hazardous_drinking, drug_usage, has_asthma, 
                          has_copd, has_pulmonary_fibrosis, 
                          has_cystic_fibrosis, has_diabetes, has_addisons,
                          severe_obesity, has_chd, has_ckd, has_cld, has_cnd,
                          has_cancer, immunosuppressed, has_sickle_cell, 
                          has_heart_failure, has_coronary_heart_disease, 
                          flu_vaccination)]
    setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
                      "imd_quintile", "rurality_classification",
                      "smoking_status", "hazardous_drinking", "drug_usage",
                      "has_asthma", "has_copd", "has_pulmonary_fibrosis", 
                      "has_cystic_fibrosis", "has_diabetes", "has_addisons" ,
                      "severe_obesity", "has_chd", "has_ckd", "has_cld", 
                      "has_cnd", "has_cancer", "immunosuppressed", 
                      "has_sickle_cell", "has_heart_failure",
                      "has_coronary_heart_disease", "flu_vaccination"),
             c("Age_Group", "Sex", "Ethnicity", "IMD", "Rurality",
               "Smoking Status", "Hazardous Drinking", "Drug Usage",
               "Asthma", "COPD", "Pulmonary Fibrosis", "Cystic Fibrosis",
               "Diabetes", "Addisons", "Severe Obesity", "Chronic Heart Disease",
               "Chronic Kidney Disease", "Chronic Liver Disease", 
               "Chronic Neurological Disease", "Cancer Within 3 Years",
               "Immunosuppressed", "Sickle Cell Disease", "Heart Failure",
               "Coronary Heart Disease", "Flu Vaccine"))
    if (study_start_date >= covid_season_min) {
      table[, covid_vaccination_count := df_datatable$covid_vaccination_count]
      setnames(table, "covid_vaccination_count", "Covid Vaccine Doses")
    }
  }
}

table %>%
  tbl_summary() %>% 
  as_gt() %>%
  gt::gtsave(filename = paste0("table1_", cohort, "_", year(study_start_date),
              "_", year(study_end_date), "_", codelist_type, "_", 
              investigation_type,".html"), path = here::here("output"))
