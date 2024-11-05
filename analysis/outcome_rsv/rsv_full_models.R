library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis", "outcome_rsv"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    study_start_date <- "2016-09-01"
    study_end_date <- "2017-08-31"
    cohort <- "infants"
    codelist_type <- "sensitive"
    investigation_type <- "primary"
  } else {
    study_start_date <- study_dates[[args[[2]]]]
    study_end_date <- study_dates[[args[[3]]]]
    cohort <- args[[1]]
    codelist_type <- args[[4]]
    investigation_type <- args[[5]]
  }
}
covid_season_min <- as.Date("2019-09-01")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow"))) 

#remove rows with missing values in any of the variables using in models
if (cohort == "infants_subgroup") {
  df_input <- df_input %>% 
    filter(!is.na(rsv_primary_inf), !is.na(rsv_secondary_inf), 
           !is.na(rsv_mortality), !is.na(latest_ethnicity_group),
           !is.na(imd_quintile), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(maternal_age), !is.na(maternal_smoking_status),
           !is.na(maternal_drinking), !is.na(maternal_drug_usage),
           !is.na(maternal_flu_vaccination))
} else {
  df_input <- df_input %>% 
    filter(!is.na(rsv_primary_inf), !is.na(rsv_secondary_inf), 
           !is.na(rsv_mortality), !is.na(latest_ethnicity_group),
           !is.na(imd_quintile), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification))
}

if (cohort == "infants_subgroup") {
  
  #rsv primary by ethnicity, socioeconomic status and household composition
  rsv_mild_full <- glm(rsv_primary_inf ~ latest_ethnicity_group + 
                         imd_quintile + composition_category + age_band +
                         sex + rurality_classification +
                         maternal_age + maternal_smoking_status +
                         maternal_drinking + maternal_drug_usage + 
                         maternal_flu_vaccination + 
                         maternal_pertussis_vaccination +
                         offset(log(time_rsv_primary)), 
                       data = df_input, family = poisson)
  rsv_mild_full_output <- tidy(rsv_mild_full)
  
  #rsv secondary by ethnicity, socioeconomic status and household composition
  rsv_severe_full <- glm(rsv_secondary_inf ~ latest_ethnicity_group + 
                           imd_quintile + composition_category + age_band +
                           sex + rurality_classification +
                           maternal_age + maternal_smoking_status +
                           maternal_drinking + maternal_drug_usage + 
                           maternal_flu_vaccination + 
                           maternal_pertussis_vaccination +
                           offset(log(time_rsv_secondary)),
                         data = df_input, family = poisson)
  rsv_severe_full_output <- tidy(rsv_severe_full)
  
  #rsv mortality by ethnicity, socioeconomic status and household composition
  rsv_mortality_full <- glm(rsv_mortality ~ latest_ethnicity_group + 
                              imd_quintile + composition_category + age_band +
                              sex + rurality_classification +
                              maternal_age + maternal_smoking_status +
                              maternal_drinking + maternal_drug_usage + 
                              maternal_flu_vaccination + 
                              maternal_pertussis_vaccination +
                              offset(log(time_rsv_mortality)),
                            data = df_input, family = poisson)
  rsv_mortality_full_output <- tidy(rsv_mortality_full)
  
} else {
  
  #rsv primary by ethnicity, socioeconomic status and household composition
  rsv_mild_full <- glm(rsv_primary_inf ~ latest_ethnicity_group +
                         imd_quintile + composition_category + age_band +
                         sex + rurality_classification + 
                         offset(log(time_rsv_primary)), 
                       data = df_input, family = poisson)
  rsv_mild_full_output <- tidy(rsv_mild_full)
  
  #rsv secondary by ethnicity, socioeconomic status and household composition
  rsv_severe_full <- glm(rsv_secondary_inf ~ latest_ethnicity_group + 
                           imd_quintile + composition_category + age_band +
                           sex + rurality_classification + 
                           offset(log(time_rsv_secondary)),
                         data = df_input, family = poisson)
  rsv_severe_full_output <- tidy(rsv_severe_full)
  
  #rsv mortality by ethnicity, socioeconomic status and household composition
  rsv_mortality_full <- glm(rsv_mortality ~ latest_ethnicity_group +
                              imd_quintile + composition_category + age_band +
                              sex + rurality_classification + 
                              offset(log(time_rsv_mortality)),
                            data = df_input, family = poisson)
  rsv_mortality_full_output <- tidy(rsv_mortality_full)
 
}

#define a vector of names for the model outputs
model_names <- c("Mild RSV by Ethnicity, IMD Quintile and Household Composition", 
                 "Severe RSV by Ethnicity, IMD Quintile and Household Composition",
                 "RSV Mortality By Ethnicity, IMD Quintile and Household Composition")

#create the model outputs list
model_outputs_list <- list(rsv_mild_full_output, 
                           rsv_severe_full_output,
                           rsv_mortality_full_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here("output", "results", "models", paste0("rsv_", investigation_type)))

#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "rsv_full_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "rsv_full_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}
