library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(gtsummary)

## create output directories ----
fs::dir_create(here("analysis"))

#import redaction functions
source(here("analysis", "functions", "redaction.R"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

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
  table <- df_datatable[registered == TRUE, Total := n_distinct(patient_id)]
  table <- df_datatable[registered == TRUE, Reactive_Airway := ifelse(age <= 5, has_asthma_reactive_airway, "No")]
  table <- df_datatable[registered == TRUE, Asthma := ifelse(age > 5, has_asthma_reactive_airway, "No")]
  table <- df_datatable[registered == TRUE, .(Total, age_band, sex, 
                        latest_ethnicity_group, imd_quintile, 
                        rurality_classification, Asthma, Reactive_Airway,
                        flu_vaccination)]
  setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
                    "imd_quintile", "rurality_classification",
                    "Reactive_Airway", "flu_vaccination"),
            c("Age Group", "Sex", "Ethnicity", "IMD", "Rurality",
              "Asthma or Reactive Airway", "Flu Vaccine"))
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
                        has_copd, has_cystic_fibrosis, 
                        has_other_resp, has_diabetes, has_addisons,
                        severe_obesity, has_chd, has_ckd, has_cld, has_cnd,
                        has_cancer, immunosuppressed, has_sickle_cell,  
                        flu_vaccination)]
  setnames(table, c("age_band", "sex", "latest_ethnicity_group", 
                    "imd_quintile", "rurality_classification",
                    "smoking_status", "hazardous_drinking", "drug_usage",
                    "has_asthma", "has_copd", "has_cystic_fibrosis", 
                    "has_other_resp", "has_diabetes", "has_addisons" ,
                    "severe_obesity", "has_chd", "has_ckd", "has_cld", 
                    "has_cnd", "has_cancer", "immunosuppressed", 
                    "has_sickle_cell", "flu_vaccination"),
            c("Age Group", "Sex", "Ethnicity", "IMD", "Rurality",
              "Smoking Status", "Hazardous Drinking", "Drug Usage",
              "Asthma", "COPD", "Cystic Fibrosis", "Other Chronic Respiratory Diseases",
              "Diabetes", "Addisons", "Severe Obesity", "Chronic Heart Diseases",
              "Chronic Kidney Disease", "Chronic Liver Disease", 
              "Chronic Neurological Disease", "Cancer Within 3 Years",
              "Immunosuppressed", "Sickle Cell Disease", "Flu Vaccine"))
  if (study_start_date >= covid_season_min) {
    table[, covid_vaccination_count := df_datatable$covid_vaccination_count]
    setnames(table, "covid_vaccination_count", "Covid Vaccine Doses")
  }
 }

## create output directories ----
fs::dir_create(here("output", "table1"))

#export
table %>%
  tbl_summary() %>% 
  as_gt() %>%
  gt::gtsave(filename = paste0("table1_", cohort, "_", year(study_start_date),
             "_", year(study_end_date), ".html"), 
             path = here::here("output", "table1"))

table %>%
  tbl_summary() %>%
  as_tibble() %>%
  write_csv(path = paste0(here::here("output", "table1"), "/", "table1_", 
            cohort, "_", year(study_start_date), "_",
            year(study_end_date),".csv"))
