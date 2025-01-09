library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_overall_and_all_cause"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
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

#remove rows with missing values in any of the variables used in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
if (cohort == "infants_subgroup") {
  
  df_input <- df_input %>% 
    filter(!is.na(imd_quintile), !is.na(age_band), !is.na(sex),
           !is.na(rurality_classification), !is.na(maternal_age),
           !is.na(maternal_smoking_status), !is.na(maternal_drinking),
           !is.na(maternal_drug_usage), !is.na(maternal_flu_vaccination),
           !is.na(maternal_pertussis_vaccination))
  
} else {
  
  df_input <- df_input %>% 
    filter(!is.na(imd_quintile), !is.na(age_band),
           !is.na(sex), !is.na(rurality_classification))
  
}


if (cohort == "infants_subgroup") {
  
  if (codelist_type == "sensitive") {
    
    #overall_resp primary by socioeconomic status
    overall_resp_mild_ses <- glm(overall_resp_primary_inf ~ imd_quintile +
                                   age_band + sex + rurality_classification +
                                   maternal_age + maternal_smoking_status +
                                   maternal_drinking + maternal_drug_usage +
                                   maternal_flu_vaccination +
                                   maternal_pertussis_vaccination +
                                   offset(log(time_overall_resp_primary*1000)),
                                 data = df_input, family = poisson)
    overall_resp_mild_ses_output <- tidy(overall_resp_mild_ses, confint = TRUE)
    
    #overall_resp secondary by socioeconomic status
    overall_resp_severe_ses <- glm(overall_resp_secondary_inf ~ imd_quintile +
                                     age_band + sex + rurality_classification +
                                     maternal_age + maternal_smoking_status +
                                     maternal_drinking + maternal_drug_usage +
                                     maternal_flu_vaccination +
                                     maternal_pertussis_vaccination +
                                     offset(log(time_overall_resp_secondary*1000)),
                                   data = df_input, family = poisson)
    overall_resp_severe_ses_output <- tidy(overall_resp_severe_ses, confint = TRUE)
    
    # #overall_resp mortality by socioeconomic status
    # overall_resp_mortality_ses <- glm(overall_resp_mortality_inf ~ imd_quintile +
    #                                     age_band + sex + rurality_classification +
    #                                     maternal_age + maternal_smoking_status +
    #                                     maternal_drinking + maternal_drug_usage +
    #                                     maternal_flu_vaccination +
    #                                     maternal_pertussis_vaccination +
    #                                     offset(log(time_overall_resp_mortality*1000)),
    #                                   data = df_input, family = poisson)
    # overall_resp_mortality_ses_output <- tidy(overall_resp_mortality_ses, confint = TRUE)
    
  }
  
  # #all cause mortality by socioeconomic status
  # all_cause_mortality_ses <- glm(all_cause_mortality_inf ~ imd_quintile +
  #                                  age_band + sex + rurality_classification +
  #                                  maternal_age + maternal_smoking_status +
  #                                  maternal_drinking + maternal_drug_usage +
  #                                  maternal_flu_vaccination +
  #                                  maternal_pertussis_vaccination +
  #                                  offset(log(time_all_cause_mortality*1000)),
  #                                data = df_input, family = poisson)
  # all_cause_mortality_ses_output <- tidy(all_cause_mortality_ses, confint = TRUE)
  
} else {
  
  if (codelist_type == "sensitive") {
  
    #overall_resp primary by socioeconomic status
    overall_resp_mild_ses <- glm(overall_resp_primary_inf ~ imd_quintile + 
                                   age_band + sex + rurality_classification + 
                                   offset(log(time_overall_resp_primary*1000)),
                                 data = df_input, family = poisson)
    overall_resp_mild_ses_output <- tidy(overall_resp_mild_ses)
    
    #overall_resp secondary by socioeconomic status
    overall_resp_severe_ses <- glm(overall_resp_secondary_inf ~ imd_quintile + 
                                     age_band + sex + rurality_classification + 
                                     offset(log(time_overall_resp_secondary*1000)),
                                   data = df_input, family = poisson)
    overall_resp_severe_ses_output <- tidy(overall_resp_severe_ses, confint = TRUE)
    
    # #overall_resp mortality by socioeconomic status
    # overall_resp_mortality_ses <- glm(overall_resp_mortality_inf ~ imd_quintile + 
    #                                     age_band + sex +
    #                                     rurality_classification + 
    #                                     offset(log(time_overall_resp_mortality*1000)),
    #                                   data = df_input, family = poisson)
    # overall_resp_mortality_ses_output <- tidy(overall_resp_mortality_ses, confint = TRUE)
  
  }
  
  # #all cause mortality by socioeconomic status
  # all_cause_mortality_ses <- glm(all_cause_mortality_inf ~ imd_quintile + 
  #                                  age_band + sex + rurality_classification + 
  #                                  offset(log(time_all_cause_mortality*1000)),
  #                                data = df_input, family = poisson)
  # all_cause_mortality_ses_output <- tidy(all_cause_mortality_ses, confint = TRUE)

}

# #define a vector of names for the model outputs
# if (codelist_type == "sensitive") {
#   model_names <- c("Mild Overall Respiratory Virus by IMD Quintile",
#                    "Severe Overall Respiratory Virus by IMD Quintile",
#                    "Overall Respiratory Virus Mortality by IMD Quintile",
#                    "All Cause Mortality by IMD Quintile")
# } else {
#   model_names <- c("All Cause Mortality by IMD Quintile")
# }

#define a vector of names for the model outputs
model_names <- c("Mild Overall Respiratory Virus by IMD Quintile",
                 "Severe Overall Respiratory Virus by IMD Quintile")

# #create the model outputs list
# model_outputs_list <- list(all_cause_mortality_ses_output)
# 
# #adjust the model outputs list based on the conditions
# if (codelist_type == "sensitive") {
#   model_outputs_list <- c(model_outputs_list,
#                           list(overall_resp_mild_ses_output,
#                                overall_resp_severe_ses_output,
#                                overall_resp_mortality_ses_output))
# }

#create the model outputs list
model_outputs_list <- list(overall_resp_mild_ses_output,
                           overall_resp_severe_ses_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("overall_and_all_cause_", investigation_type)))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("overall_and_all_cause_", investigation_type)),
                            "/", "overalL_and_all_cause_ses_model_outputs_",
                            cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("overall_and_all_cause_", investigation_type)),
                            "/", "overall_and_all_cause_ses_model_outputs_",
                            cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
  
}
