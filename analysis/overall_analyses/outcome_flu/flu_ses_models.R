library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses", "outcome_flu"))

#define cohort
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cohort <- "infants"
  } else {
    cohort <- args[[1]]
  }
}

df_input <- read_feather(
  here::here("output", "data", "overall_analyses",
             paste0("input_processed_combined_", cohort,
                    "_specific_primary.arrow")))

#remove rows with missing values in any of the variables using in models
if (cohort == "infants_subgroup") {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(age_band), !is.na(sex),
           !is.na(rurality_classification), !is.na(maternal_age),
           !is.na(maternal_smoking_status), !is.na(maternal_drinking),
           !is.na(maternal_drug_usage), !is.na(maternal_flu_vaccination),
           !is.na(maternal_pertussis_vaccination))
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(age_band), !is.na(sex),
           !is.na(rurality_classification), !is.na(has_asthma),
           !is.na(has_copd), !is.na(has_cystic_fibrosis),
           !is.na(has_other_resp), !is.na(has_diabetes), !is.na(has_addisons),
           !is.na(severe_obesity), !is.na(has_chd), !is.na(has_ckd),
           !is.na(has_cld), !is.na(has_cnd), !is.na(has_cancer),
           !is.na(immunosuppressed), !is.na(has_sickle_cell),
           !is.na(smoking_status), !is.na(hazardous_drinking),
           !is.na(drug_usage))
  
} else {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(age_band),
           !is.na(sex), !is.na(rurality_classification))
  
}

if (cohort == "infants_subgroup") {
  
  #flu primary by ses
  flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                        age_band + sex + rurality_classification + 
                        maternal_age + maternal_smoking_status +
                        maternal_drinking + maternal_drug_usage + 
                        maternal_flu_vaccination + 
                        maternal_pertussis_vaccination +
                        offset(log(time_flu_primary*1000)),
                      data = df_input, family = poisson)
  flu_mild_ses_output <- tidy(flu_mild_ses, confint = TRUE)
  
  #flu secondary by ses
  flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                          age_band + sex + rurality_classification + 
                          maternal_age + maternal_smoking_status +
                          maternal_drinking + maternal_drug_usage + 
                          maternal_flu_vaccination + 
                          maternal_pertussis_vaccination +
                          offset(log(time_flu_secondary*1000)),
                        data = df_input, family = poisson)
  flu_severe_ses_output <- tidy(flu_severe_ses, confint = TRUE)
  
  # #flu mortality by ses
  # flu_mortality_ses <- glm(flu_mortality_inf ~ imd_quintile + 
  #                            age_band + sex + rurality_classification + 
  #                            maternal_age + maternal_smoking_status +
  #                            maternal_drinking + maternal_drug_usage + 
  #                            maternal_flu_vaccination + 
  #                            maternal_pertussis_vaccination +
  #                            offset(log(time_flu_mortality*1000)),
  #                          data = df_input, family = poisson)
  # flu_mortality_ses_output <- tidy(flu_mortality_ses, confint = TRUE)
  
} else {
  
  #flu primary by ses
  flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                        age_band + sex + rurality_classification + 
                        offset(log(time_flu_primary*1000)),
                      data = df_input, family = poisson)
  flu_mild_ses_output <- tidy(flu_mild_ses, confint = TRUE)
  
  #flu secondary by ses
  flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                          age_band + sex + rurality_classification + 
                          offset(log(time_flu_secondary*1000)),
                        data = df_input, family = poisson)
  flu_severe_ses_output <- tidy(flu_severe_ses, confint = TRUE)
  
  # #flu mortality by ses
  # flu_mortality_ses <- glm(flu_mortality_inf ~ imd_quintile + 
  #                            age_band + sex + rurality_classification + 
  #                            offset(log(time_flu_mortality*1000)),
  #                          data = df_input, family = poisson)
  # flu_mortality_ses_output <- tidy(flu_mortality_ses, confint = TRUE)

}

#define a vector of names for the model outputs
model_names <- c("Mild Influenza by IMD Quintile",
                 "Severe Influenza by IMD Quintile")#,
                 # "Influenza Mortality by IMD Quintile")


#create the model outputs list
model_outputs_list <- list(flu_mild_ses_output,
                           flu_severe_ses_output)#, 
                           # flu_mortality_ses_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models", "flu_overall"))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            "flu_overall"), "/", "flu_ses_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}  else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            "flu_overall"), "/", "flu_ses_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}
