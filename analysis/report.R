library("tidyverse")
library("here")
library("arrow")
library("ggplot2")
library("data.table")
library("gtsummary")

## create output directories ----
fs::dir_create(here("analysis"))

#import redaction functions
source(here("analysis", "functions", "redaction.R"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
study_start_date <- study_dates[[args[[2]]]]
study_end_date <- study_dates[[args[[3]]]]
cohort <- args[[1]]
codelist_type <- args[[4]]
investigation_type <- args[[5]]
covid_season_min <- as.Date("2019-09-01")

# roundmid_any <- function(x, to=6){
#   # like round_any, but centers on (integer) midpoint of the rounding points
#   ceiling(x/to)*to - (floor(to/2)*(x!=0))
# }

df_input <- read_feather(
  here::here("output", paste0("input_processed_", cohort, "_", year(study_start_date),
             "_", year(study_end_date), "_", codelist_type, "_", 
             investigation_type,".arrow")))

# lab <- ifelse(cohort == "infants", "Age (Months)", 
#        ifelse(cohort == "infants_subgroup", "Age (Months)", "Age (Years)"))
# 
# plot_age <- ggplot(data = df_input, aes(age, frequency(age))) + geom_col(width = 0.9) +
#   xlab(lab) + ylab("Frequency")
# 
# ggsave(
#   plot = plot_age,
#   filename = paste0("descriptive_", cohort, "_", year(study_start_date),
#     "_", year(study_end_date), "_", codelist_type, "_", 
#     investigation_type,".png"), path = here::here("output"),
# )

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

table %>%
  tbl_summary() %>%
  as_tibble() %>%
  write_csv(path = paste0(here::here("output"), "/", "table1_", cohort, "_", year(study_start_date),
            "_", year(study_end_date), "_", codelist_type, "_",
            investigation_type,".csv"))

# table_sum <- tbl_summary(table)
# table_redacted <- redact_tblsummary(table_sum, 7)
# var_labels <- colnames(table_sum)
# raw_stats <- table_sum$meta_data %>%
#   select(var_label, df_stats) %>%
#   unnest(df_stats)
# threshold = 7
# raw_stats_redacted <- raw_stats %>%
#   mutate(
#     n = roundmid_any(n, threshold),
#     N = roundmid_any(N, threshold),
#     p = n / N,
#     N_miss = roundmid_any(N_miss, threshold),
#     N_obs = roundmid_any(N_obs, threshold),
#     p_miss = N_miss / N_obs,
#     N_nonmiss = roundmid_any(N_nonmiss, threshold),
#     p_nonmiss = N_nonmiss / N_obs,
#     var_label = factor(var_label, levels = map_chr(var_labels[-c(1, 2)], ~ last(as.character(.)))),
#     variable_levels = replace_na(as.character(variable_levels), "")
#   )
# 
# raw_stats_redacted %>%
#   write.csv(filename = paste0("table1_raw_", cohort, "_", year(study_start_date),
#              "_", year(study_end_date), "_", codelist_type, "_", 
#              investigation_type,".xlsx"), path = here::here("output"))
