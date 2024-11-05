library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis", "outcome_covid"))

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
    filter(!is.na(covid_primary_inf), !is.na(covid_secondary_inf), 
           !is.na(covid_mortality), !is.na(latest_ethnicity_group), 
           !is.na(imd_quintile), !is.na(age_band),
           !is.na(sex), !is.na(rurality_classification),
           !is.na(maternal_age), !is.na(maternal_smoking_status),
           !is.na(maternal_drinking), !is.na(maternal_drug_usage),
           !is.na(maternal_flu_vaccination))
} else {
  df_input <- df_input %>% 
    filter(!is.na(covid_primary_inf), !is.na(covid_secondary_inf), 
           !is.na(covid_mortality), !is.na(latest_ethnicity_group),
           !is.na(imd_quintile), !is.na(age_band),
           !is.na(sex), !is.na(rurality_classification))
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
  covid_mortality_ethnicity_ses <- glm(covid_mortality ~ latest_ethnicity_group +
                                         imd_quintile + age_band + sex +
                                         rurality_classification +
                                         maternal_age + maternal_smoking_status +
                                         maternal_drinking + maternal_drug_usage + 
                                         maternal_flu_vaccination + 
                                         maternal_pertussis_vaccination +
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
  covid_mortality_ethnicity_ses <- glm(covid_mortality ~ latest_ethnicity_group +
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
fs::dir_create(here("output", "results", "models", paste0("covid_", investigation_type)))
  
#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "covid_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date),
                            "_", codelist_type, "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "covid_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}
