library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_covid"))

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
    filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(maternal_age), !is.na(maternal_smoking_status),
           !is.na(maternal_drinking), !is.na(maternal_drug_usage),
           !is.na(maternal_flu_vaccination))
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {

  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
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
    filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification))
  
}
  
if (cohort == "infants_subgroup") {
  
  #covid primary by ethnicity and socioeconomic status
  covid_mild_ethnicity_ses <- glm(covid_primary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex +
                                    rurality_classification +
                                    maternal_age + maternal_smoking_status +
                                    maternal_drinking + maternal_drug_usage + 
                                    maternal_flu_vaccination + 
                                    maternal_pertussis_vaccination +
                                    offset(log(time_covid_primary)),
                                  data = df_input, family = poisson)
  covid_mild_ethnicity_ses_output <- tidy(covid_mild_ethnicity_ses)
  
  #covid secondary by ethnicity and socioeconomic status
  covid_severe_ethnicity_ses <- glm(covid_secondary_inf ~ latest_ethnicity_group +
                                      imd_quintile + age_band + sex +
                                      rurality_classification +
                                      maternal_age + maternal_smoking_status +
                                      maternal_drinking + maternal_drug_usage + 
                                      maternal_flu_vaccination + 
                                      maternal_pertussis_vaccination +
                                      offset(log(time_covid_secondary)),
                                    data = df_input, family = poisson)
  covid_severe_ethnicity_ses_output <- tidy(covid_severe_ethnicity_ses)
  
  #covid mortality by ethnicity and socioeconomic status
  covid_mortality_ethnicity_ses <- glm(covid_mortality_inf ~ latest_ethnicity_group +
                                         imd_quintile + age_band + sex +
                                         rurality_classification +
                                         maternal_age + maternal_smoking_status +
                                         maternal_drinking + maternal_drug_usage + 
                                         maternal_flu_vaccination + 
                                         maternal_pertussis_vaccination +
                                         offset(log(time_covid_mortality)),
                                       data = df_input, family = poisson)
  covid_mortality_ethnicity_ses_output <- tidy(covid_mortality_ethnicity_ses)
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  #covid primary by ethnicity and socioeconomic status
  covid_mild_ethnicity_ses <- glm(covid_primary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex +
                                    rurality_classification + has_asthma +
                                    has_copd + has_cystic_fibrosis +
                                    has_other_resp + has_diabetes +
                                    has_addisons + severe_obesity + has_chd +
                                    has_ckd + has_cld + has_cnd +
                                    has_cancer + immunosuppressed +
                                    has_sickle_cell + smoking_status +
                                    hazardous_drinking + drug_usage +
                                    offset(log(time_covid_primary)),
                                  data = df_input, family = poisson)
  covid_mild_ethnicity_ses_output <- tidy(covid_mild_ethnicity_ses)
  
  #covid secondary by ethnicity and socioeconomic status
  covid_severe_ethnicity_ses <- glm(covid_secondary_inf ~ latest_ethnicity_group +
                                      imd_quintile + age_band + sex +
                                      rurality_classification + has_asthma +
                                      has_copd + has_cystic_fibrosis +
                                      has_other_resp + has_diabetes +
                                      has_addisons + severe_obesity + has_chd +
                                      has_ckd + has_cld + has_cnd +
                                      has_cancer + immunosuppressed +
                                      has_sickle_cell + smoking_status +
                                      hazardous_drinking + drug_usage +
                                      offset(log(time_covid_secondary)),
                                    data = df_input, family = poisson)
  covid_severe_ethnicity_ses_output <- tidy(covid_severe_ethnicity_ses)
  
  #covid mortality by ethnicity and socioeconomic status
  covid_mortality_ethnicity_ses <- glm(covid_mortality_inf ~ latest_ethnicity_group +
                                         imd_quintile + age_band + sex +
                                         rurality_classification + has_asthma +
                                         has_copd + has_cystic_fibrosis +
                                         has_other_resp + has_diabetes +
                                         has_addisons + severe_obesity + has_chd +
                                         has_ckd + has_cld + has_cnd +
                                         has_cancer + immunosuppressed +
                                         has_sickle_cell + smoking_status +
                                         hazardous_drinking + drug_usage +
                                         offset(log(time_covid_mortality)),
                                       data = df_input, family = poisson)
  covid_mortality_ethnicity_ses_output <- tidy(covid_mortality_ethnicity_ses)
  
} else {
  
  #covid primary by ethnicity and socioeconomic status
  covid_mild_ethnicity_ses <- glm(covid_primary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex +
                                    rurality_classification +
                                    offset(log(time_covid_primary)),
                                  data = df_input, family = poisson)
  covid_mild_ethnicity_ses_output <- tidy(covid_mild_ethnicity_ses)
  
  #covid secondary by ethnicity and socioeconomic status
  covid_severe_ethnicity_ses <- glm(covid_secondary_inf ~ latest_ethnicity_group +
                                      imd_quintile + age_band + sex +
                                      rurality_classification +
                                      offset(log(time_covid_secondary)),
                                    data = df_input, family = poisson)
  covid_severe_ethnicity_ses_output <- tidy(covid_severe_ethnicity_ses)
  
  #covid mortality by ethnicity and socioeconomic status
  covid_mortality_ethnicity_ses <- glm(covid_mortality_inf ~ latest_ethnicity_group +
                                         imd_quintile + age_band + sex +
                                         rurality_classification +
                                         offset(log(time_covid_mortality)),
                                       data = df_input, family = poisson)
  covid_mortality_ethnicity_ses_output <- tidy(covid_mortality_ethnicity_ses)
  
}

#define a vector of names for the model outputs
model_names <- c("Mild COVID-19 by Ethnicity and IMD Quintile", 
                 "Severe COVID-19 by Ethnicity and IMD Quintile",
                 "COVID-19 Mortality by Ethnicity and IMD Quintile")
  
#create the model outputs list
model_outputs_list <- list(covid_mild_ethnicity_ses_output, 
                           covid_severe_ethnicity_ses_output,
                           covid_mortality_ethnicity_ses_output)
  
#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))
  
## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("covid_", investigation_type)))
  
#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "covid_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date),
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "covid_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
}
