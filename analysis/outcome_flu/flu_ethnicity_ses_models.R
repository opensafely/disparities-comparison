library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis", "outcome_flu"))

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

if (cohort == "infants_subgroup") {
  
  #flu primary by ethnicity and socioeconomic status
  flu_mild_ethnicity_ses <- glm(flu_primary_inf ~ latest_ethnicity_group + 
                                  imd_quintile + age_band + sex + 
                                  rurality_classification + 
                                  maternal_age + maternal_smoking_status +
                                  maternal_drinking + maternal_drug_usage + 
                                  maternal_flu_vaccination + 
                                  maternal_pertussis_vaccination +
                                  offset(log(time_flu_primary)), 
                                data = df_input, family = poisson)
  flu_mild_ethnicity_ses_output <- tidy(flu_mild_ethnicity_ses)
  
  #flu secondary by ethnicity and socioeconomic status
  flu_severe_ethnicity_ses <- glm(flu_secondary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex + 
                                    rurality_classification + 
                                    maternal_age + maternal_smoking_status +
                                    maternal_drinking + maternal_drug_usage + 
                                    maternal_flu_vaccination + 
                                    maternal_pertussis_vaccination +
                                    offset(log(time_flu_secondary)),
                                  data = df_input, family = poisson)
  flu_severe_ethnicity_ses_output <- tidy(flu_severe_ethnicity_ses)
  
  #flu mortality by ethnicity and socioeconomic status
  flu_mortality_ethnicity_ses <- glm(flu_mortality ~ latest_ethnicity_group + 
                                       imd_quintile + age_band + sex + 
                                       rurality_classification + 
                                       maternal_age + maternal_smoking_status +
                                       maternal_drinking + maternal_drug_usage + 
                                       maternal_flu_vaccination + 
                                       maternal_pertussis_vaccination +
                                       offset(log(time_flu_mortality)),
                                     data = df_input, family = poisson)
  flu_mortality_ethnicity_ses_output <- tidy(flu_mortality_ethnicity_ses)
  
} else {
  
  #flu primary by ethnicity and socioeconomic status
  flu_mild_ethnicity_ses <- glm(flu_primary_inf ~ latest_ethnicity_group +
                                  imd_quintile + age_band + sex + 
                                  rurality_classification + 
                                  offset(log(time_flu_primary)), 
                                data = df_input, family = poisson)
  flu_mild_ethnicity_ses_output <- tidy(flu_mild_ethnicity_ses)
  
  #flu secondary by ethnicity and socioeconomic status
  flu_severe_ethnicity_ses <- glm(flu_secondary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex + 
                                    rurality_classification + 
                                    offset(log(time_flu_secondary)),
                                  data = df_input, family = poisson)
  flu_severe_ethnicity_ses_output <- tidy(flu_severe_ethnicity_ses)
  
  #flu mortality by ethnicity and socioeconomic status
  flu_mortality_ethnicity_ses <- glm(flu_mortality ~ latest_ethnicity_group + 
                                       imd_quintile + age_band + sex + 
                                       rurality_classification + 
                                       offset(log(time_flu_mortality)),
                                     data = df_input, family = poisson)
  flu_mortality_ethnicity_ses_output <- tidy(flu_mortality_ethnicity_ses)

}

#define a vector of names for the model outputs
model_names <- c("Mild Influenza by Ethnicity and IMD Quintile", 
                 "Severe Influenza by Ethnicity and IMD Quintile",
                 "Influenza Mortality By Ethnicity and IMD Quintile")

#create the model outputs list
model_outputs_list <- list(flu_mild_ethnicity_ses_output, 
                           flu_severe_ethnicity_ses_output,
                           flu_mortality_ethnicity_ses_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here("output", "results", "models"))

#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("flu_", investigation_type)), "/", 
                            "flu_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("flu_", investigation_type)), "/", 
                            "flu_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}
