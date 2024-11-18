library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_rsv"))

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

#remove rows with missing values in any of the variables using in models
if (cohort == "infants_subgroup") {
  
  df_input <- df_input %>% 
    filter(!is.na(rsv_primary_inf), !is.na(rsv_secondary_inf), 
           !is.na(rsv_mortality), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(maternal_age), !is.na(maternal_smoking_status),
           !is.na(maternal_drinking), !is.na(maternal_drug_usage),
           !is.na(maternal_flu_vaccination))
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  df_input <- df_input %>% 
    filter(!is.na(rsv_primary_inf), !is.na(rsv_secondary_inf), 
           !is.na(rsv_mortality), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(has_asthma), !is.na(has_copd), !is.na(has_cystic_fibrosis),
           !is.na(has_other_resp), !is.na(has_diabetes), !is.na(has_addisons),
           !is.na(severe_obesity), !is.na(has_chd), !is.na(has_ckd),
           !is.na(has_cld), !is.na(has_cnd), !is.na(has_cancer),
           !is.na(immunosuppressed), !is.na(has_sickle_cell),
           !is.na(smoking_status), !is.na(hazardous_drinking),
           !is.na(drug_usage))

} else {
  
  df_input <- df_input %>% 
    filter(!is.na(rsv_primary_inf), !is.na(rsv_secondary_inf), 
           !is.na(rsv_mortality), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification))
  
}

if (cohort == "infants_subgroup") {
  
  #rsv primary by household composition
  rsv_mild_hh_comp <- glm(rsv_primary_inf ~ composition_category + 
                            age_band + sex + rurality_classification +
                            maternal_age + maternal_smoking_status +
                            maternal_drinking + maternal_drug_usage + 
                            maternal_flu_vaccination + 
                            maternal_pertussis_vaccination +
                            offset(log(time_rsv_primary)), 
                          data = df_input, family = poisson)
  rsv_mild_hh_comp_output <- tidy(rsv_mild_hh_comp)
  
  #rsv secondary by household composition
  rsv_severe_hh_comp <- glm(rsv_secondary_inf ~ composition_category +
                              age_band + sex + rurality_classification + 
                              maternal_age + maternal_smoking_status +
                              maternal_drinking + maternal_drug_usage + 
                              maternal_flu_vaccination + 
                              maternal_pertussis_vaccination +
                              offset(log(time_rsv_secondary)),
                            data = df_input, family = poisson)
  rsv_severe_hh_comp_output <- tidy(rsv_severe_hh_comp)
  
  #rsv mortality by household composition
  rsv_mortality_hh_comp <- glm(rsv_mortality ~ composition_category + 
                                 age_band + sex + rurality_classification + 
                                 maternal_age + maternal_smoking_status +
                                 maternal_drinking + maternal_drug_usage + 
                                 maternal_flu_vaccination + 
                                 maternal_pertussis_vaccination +
                                 offset(log(time_rsv_mortality)),
                               data = df_input, family = poisson)
  rsv_mortality_hh_comp_output <- tidy(rsv_mortality_hh_comp)
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  #rsv primary by household composition
  rsv_mild_hh_comp <- glm(rsv_primary_inf ~ composition_category + 
                            age_band + sex + rurality_classification +
                            has_asthma + has_copd + has_cystic_fibrosis +
                            has_other_resp + has_diabetes +
                            has_addisons + severe_obesity +
                            has_chd + has_ckd + has_cld + has_cnd +
                            has_cancer + immunosuppressed +
                            has_sickle_cell + smoking_status +
                            hazardous_drinking + drug_usage +
                            offset(log(time_rsv_primary)),
                          data = df_input, family = poisson)
  rsv_mild_hh_comp_output <- tidy(rsv_mild_hh_comp)
  
  #rsv secondary by household composition
  rsv_severe_hh_comp <- glm(rsv_secondary_inf ~ composition_category + 
                              age_band + sex + rurality_classification +
                              has_asthma + has_copd + has_cystic_fibrosis +
                              has_other_resp + has_diabetes +
                              has_addisons + severe_obesity +
                              has_chd + has_ckd + has_cld + has_cnd +
                              has_cancer + immunosuppressed +
                              has_sickle_cell + smoking_status +
                              hazardous_drinking + drug_usage +
                              offset(log(time_rsv_secondary)),
                            data = df_input, family = poisson)
  rsv_severe_hh_comp_output <- tidy(rsv_severe_hh_comp)
  
  #rsv mortality by household composition
  rsv_mortality_hh_comp <- glm(rsv_mortality ~ composition_category + 
                                 age_band + sex + rurality_classification +
                                 has_asthma + has_copd + has_cystic_fibrosis +
                                 has_other_resp + has_diabetes +
                                 has_addisons + severe_obesity +
                                 has_chd + has_ckd + has_cld + has_cnd +
                                 has_cancer + immunosuppressed +
                                 has_sickle_cell + smoking_status +
                                 hazardous_drinking + drug_usage +
                                 offset(log(time_rsv_mortality)),
                               data = df_input, family = poisson)
  rsv_mortality_hh_comp_output <- tidy(rsv_mortality_hh_comp)
  
} else {
  
  #rsv primary by household composition
  rsv_mild_hh_comp <- glm(rsv_primary_inf ~ composition_category + 
                            age_band + sex + rurality_classification + 
                            offset(log(time_rsv_primary)),
                          data = df_input, family = poisson)
  rsv_mild_hh_comp_output <- tidy(rsv_mild_hh_comp)
  
  #rsv secondary by household composition
  rsv_severe_hh_comp <- glm(rsv_secondary_inf ~ composition_category + 
                              age_band + sex + rurality_classification + 
                              offset(log(time_rsv_secondary)),
                            data = df_input, family = poisson)
  rsv_severe_hh_comp_output <- tidy(rsv_severe_hh_comp)
  
  #rsv mortality by household composition
  rsv_mortality_hh_comp <- glm(rsv_mortality ~ composition_category + 
                                 age_band + sex + rurality_classification + 
                                 offset(log(time_rsv_mortality)),
                               data = df_input, family = poisson)
  rsv_mortality_hh_comp_output <- tidy(rsv_mortality_hh_comp)
  
}

#define a vector of names for the model outputs
model_names <- c("Mild RSV by Household Composition",
                 "Severe RSV by Household Composition",
                 "RSV Mortality by Household Composition")

#create the model outputs list
model_outputs_list <- list(rsv_mild_hh_comp_output, rsv_severe_hh_comp_output,
                           rsv_mortality_hh_comp_output)


#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("rsv_", investigation_type)))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "rsv_hh_comp_model_outputs_", cohort, "_", year(study_start_date), 
                            "_", year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "rsv_hh_comp_model_outputs_", cohort, "_", year(study_start_date),
                            "_", year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
  
}
