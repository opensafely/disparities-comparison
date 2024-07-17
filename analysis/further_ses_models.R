library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis"))

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

# #add models for infants subgroup
# if (cohort == "infants_subgroup") {

# } else {

#rsv primary by ses
rsv_mild_ses_further <- glm(rsv_primary_inf ~ imd_quintile +
                              age_band + sex + 
                              rurality_classification + 
                              prior_flu_vaccination +
                              flu_vaccination +
                              offset(log(time_rsv_primary)),
                            data = df_input, family = poisson)
rsv_mild_ses_further_output <- tidy(rsv_mild_ses_further)

#rsv secondary by ses
rsv_severe_ses_further <- glm(rsv_secondary_inf ~ imd_quintile + 
                                age_band + sex + 
                                rurality_classification + 
                                prior_flu_vaccination +
                                flu_vaccination +
                                offset(log(time_rsv_secondary)),
                              data = df_input, family = poisson)
rsv_severe_ses_further_output <- tidy(rsv_severe_ses_further)

#rsv mortality by ses
rsv_mortality_ses_further <- glm(rsv_mortality ~ imd_quintile + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   prior_flu_vaccination +
                                   flu_vaccination +
                                   offset(log(time_rsv_mortality)),
                                 data = df_input, family = poisson)
rsv_mortality_ses_further_output <- tidy(rsv_mortality_ses_further)

#flu primary by ses
flu_mild_ses_further <- glm(flu_primary_inf ~ imd_quintile + 
                              age_band + sex + 
                              rurality_classification + 
                              prior_flu_vaccination +
                              flu_vaccination +
                              offset(log(time_flu_primary)),
                            data = df_input, family = poisson)
flu_mild_ses_further_output <- tidy(flu_mild_ses_further)

#flu secondary by ses
flu_severe_ses_further <- glm(flu_secondary_inf ~ imd_quintile + 
                                age_band + sex + 
                                rurality_classification + 
                                prior_flu_vaccination +
                                flu_vaccination +
                                offset(log(time_flu_secondary)),
                              data = df_input, family = poisson)
flu_severe_ses_further_output <- tidy(flu_severe_ses_further)

#flu mortality by ses
flu_mortality_ses_further <- glm(flu_mortality ~ imd_quintile + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   prior_flu_vaccination +
                                   flu_vaccination +
                                   offset(log(time_flu_mortality)),
                                 data = df_input, family = poisson)
flu_mortality_ses_further_output <- tidy(flu_mortality_ses_further)

if (codelist_type == "sensitive") {
  #overall_resp primary by ses
  overall_resp_mild_ses_further <- glm(overall_resp_primary_inf ~ imd_quintile + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         prior_flu_vaccination +
                                         flu_vaccination +
                                         offset(log(time_overall_resp_primary)),
                                       data = df_input, family = poisson)
  overall_resp_mild_ses_further_output <- tidy(overall_resp_mild_ses_further)
  
  #overall_resp secondary by ses
  overall_resp_severe_ses_further <- glm(overall_resp_secondary_inf ~ imd_quintile + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           prior_flu_vaccination +
                                           flu_vaccination +
                                           offset(log(time_overall_resp_secondary)),
                                         data = df_input, family = poisson)
  overall_resp_severe_ses_further_output <- tidy(overall_resp_severe_ses_further)
  
  #overall_resp mortality by ses
  overall_resp_mortality_ses_further <- glm(overall_resp_mortality ~ imd_quintile + 
                                              age_band + sex + 
                                              rurality_classification + 
                                              prior_flu_vaccination +
                                              flu_vaccination +
                                              offset(log(time_overall_resp_mortality)),
                                            data = df_input, family = poisson)
  overall_resp_mortality_ses_further_output <- tidy(overall_resp_mortality_ses_further)
}

#all cause mortality by ses
all_cause_mortality_ses_further <- glm(all_cause_mortality ~ imd_quintile + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         prior_flu_vaccination +
                                         flu_vaccination +
                                         offset(log(time_all_cause_mortality)),
                                       data = df_input, family = poisson)
all_cause_mortality_ses_further_output <- tidy(all_cause_mortality_ses_further)

if (study_start_date >= covid_season_min) {
  #rsv primary by ses
  rsv_mild_ses_further <- glm(rsv_primary_inf ~ imd_quintile + 
                                age_band + sex + 
                                rurality_classification + 
                                prior_flu_vaccination +
                                flu_vaccination +
                                time_since_last_covid_vaccination +
                                covid_vaccination +
                                offset(log(time_rsv_primary)),
                              data = df_input, family = poisson)
  rsv_mild_ses_further_output <- tidy(rsv_mild_ses_further)
  
  #rsv secondary by ses
  rsv_severe_ses_further <- glm(rsv_secondary_inf ~ imd_quintile +
                                  age_band + sex + 
                                  rurality_classification +
                                  prior_flu_vaccination +
                                  flu_vaccination +
                                  time_since_last_covid_vaccination +
                                  covid_vaccination +
                                  offset(log(time_rsv_secondary)),
                                data = df_input, family = poisson)
  rsv_severe_ses_further_output <- tidy(rsv_severe_ses_further)
  
  #rsv mortality by ses
  rsv_mortality_ses_further <- glm(rsv_mortality ~ imd_quintile + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     prior_flu_vaccination +
                                     flu_vaccination +
                                     time_since_last_covid_vaccination +
                                     covid_vaccination +
                                     offset(log(time_rsv_mortality)),
                                   data = df_input, family = poisson)
  rsv_severe_ses_further_output <- tidy(rsv_mortality_ses_further)
  
  #flu primary by ses
  flu_mild_ses_further <- glm(flu_primary_inf ~ imd_quintile + 
                                age_band + sex + 
                                rurality_classification + 
                                prior_flu_vaccination +
                                flu_vaccination +
                                time_since_last_covid_vaccination +
                                covid_vaccination +
                                offset(log(time_flu_primary)),
                              data = df_input, family = poisson)
  flu_mild_ses_further_output <- tidy(flu_mild_ses_further)
  
  #flu secondary by ses
  flu_severe_ses_further <- glm(flu_secondary_inf ~ imd_quintile + 
                                  age_band + sex + 
                                  rurality_classification +
                                  prior_flu_vaccination +
                                  flu_vaccination +
                                  time_since_last_covid_vaccination +
                                  covid_vaccination +
                                  offset(log(time_flu_secondary)),
                                data = df_input, family = poisson)
  flu_severe_ses_further_output <- tidy(flu_severe_ses_further)
  
  #flu mortality by ses
  flu_mortality_ses_further <- glm(flu_mortality ~ imd_quintile + 
                                     age_band + sex + 
                                     rurality_classification +
                                     prior_flu_vaccination +
                                     flu_vaccination +
                                     time_since_last_covid_vaccination +
                                     covid_vaccination +
                                     offset(log(time_flu_mortality)),
                                   data = df_input, family = poisson)
  flu_severe_mild_ses_further_output <- tidy(flu_mortality_ses_further)
  
  #covid primary by ses
  covid_mild_ses_further <- glm(covid_primary_inf ~ imd_quintile + 
                                  age_band + sex + 
                                  rurality_classification + 
                                  prior_flu_vaccination +
                                  flu_vaccination +
                                  time_since_last_covid_vaccination +
                                  covid_vaccination +
                                  offset(log(time_covid_primary)),
                                data = df_input, family = poisson)
  covid_mild_ses_further_output <- tidy(covid_mild_ses_further)
  
  #covid secondary by ses
  covid_severe_ses_further <- glm(covid_secondary_inf ~ imd_quintile + 
                                    age_band + sex + 
                                    rurality_classification + 
                                    prior_flu_vaccination +
                                    flu_vaccination +
                                    time_since_last_covid_vaccination +
                                    covid_vaccination +
                                    offset(log(time_covid_secondary)),
                                  data = df_input, family = poisson)
  covid_severe_ses_further_output <- tidy(covid_severe_ses_further)
  
  #covid mortality by ses
  covid_mortality_ses_further <- glm(covid_mortality ~ imd_quintile + 
                                       age_band + sex + 
                                       rurality_classification + 
                                       prior_flu_vaccination +
                                       flu_vaccination +
                                       time_since_last_covid_vaccination +
                                       covid_vaccination +
                                       offset(log(time_covid_mortality)),
                                     data = df_input, family = poisson)
  covid_mortality_ses_further_output <- tidy(covid_mortality_ses_further)
  
  if (codelist_type == "sensitive") {
    #overall_resp primary by ses
    overall_resp_mild_ses_further <- glm(overall_resp_primary_inf ~ imd_quintile + 
                                           age_band + sex + 
                                           rurality_classification +
                                           prior_flu_vaccination +
                                           flu_vaccination +
                                           time_since_last_covid_vaccination +
                                           covid_vaccination +
                                           offset(log(time_overall_resp_primary)),
                                         data = df_input, family = poisson)
    overall_resp_mild_ses_further_output <- tidy(overall_resp_mild_ses_further)
    
    #overall_resp secondary by ses
    overall_resp_severe_ses_further <- glm(overall_resp_secondary_inf ~ imd_quintile + 
                                             age_band + sex + 
                                             rurality_classification + 
                                             prior_flu_vaccination +
                                             flu_vaccination +
                                             time_since_last_covid_vaccination +
                                             covid_vaccination +
                                             offset(log(time_overall_resp_secondary)),
                                           data = df_input, family = poisson)
    overall_resp_severe_ses_further_output <- tidy(overall_resp_severe_ses_further)
    
    #overall_resp mortality by ses
    overall_resp_mortality_ses_further <- glm(overall_resp_mortality ~ imd_quintile + 
                                                age_band + sex + 
                                                rurality_classification + 
                                                prior_flu_vaccination +
                                                flu_vaccination +
                                                time_since_last_covid_vaccination +
                                                covid_vaccination +
                                                offset(log(time_overall_resp_mortality)),
                                              data = df_input, family = poisson)
    overall_resp_mortality_ses_further_output <- tidy(overall_resp_mortality_ses_further)
  }
  
  #all cause mortality by ses
  all_cause_mortality_ses_further <- glm(all_cause_mortality ~ imd_quintile + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           prior_flu_vaccination +
                                           flu_vaccination +
                                           time_since_last_covid_vaccination +
                                           covid_vaccination +
                                           offset(log(time_all_cause_mortality)),
                                         data = df_input, family = poisson)
  all_cause_mortality_ses_further_output <- tidy(all_cause_mortality_ses_further)
  
}
# }

#define a vector of names for the model outputs
if (study_start_date < covid_season_min) {
  model_names <- c("Mild RSV by IMD Quintile",
                   "Severe RSV by IMD Quintile", "RSV Mortality by IMD Quintile",
                   "Mild Influenza by IMD Quintile", "Severe Influenza by IMD Quintile",
                   "Influenza Mortality by IMD Quintile", "All Cause Mortlality by IMD Quintile")
} else if (codelist_type == "sensitive") {
  model_names <- c("Mild RSV by IMD Quintile",
                   "Severe RSV by IMD Quintile", "RSV Mortality by IMD Quintile",
                   "Mild Influenza by IMD Quintile", "Severe Influenza by IMD Quintile",
                   "Influenza Mortality by IMD Quintile", 
                   "Mild Overall Respiratory Virus by IMD Quintile",
                   "Severe Overall Respiratory Virus by IMD Quintile",
                   "Overall Respiratory Virus Mortality by IMD Quintile",
                   "All Cause Mortlality by IMD Quintile")
} else if (study_start_date >= covid_season_min) {
  model_names <- c("Mild RSV by IMD Quintile", "Severe RSV by IMD Quintile", 
                   "RSV Mortality by IMD Quintile", "Mild Influenza by IMD Quintile", 
                   "Severe Influenza by IMD Quintile", "Influenza Mortality by IMD Quintile", 
                   "Mild COVID-19 by IMD Quintile", "Severe COVID-19 by IMD Quintile",
                   "COVID-19 Mortality by IMD Quintile", "All Cause Mortlality by IMD Quintile")
} else {
  model_names <- c("Mild RSV by IMD Quintile", "Severe RSV by IMD Quintile", 
                   "RSV Mortality by IMD Quintile", "Mild Influenza by IMD Quintile", 
                   "Severe Influenza by IMD Quintile", "Influenza Mortality by IMD Quintile", 
                   "Mild COVID-19 by IMD Quintile", "Severe COVID-19 by IMD Quintile",
                   "COVID-19 Mortality by IMD Quintile", 
                   "Mild Overall Respiratory Virus by IMD Quintile",
                   "Severe Overall Respiratory Virus by IMD Quintile",
                   "Overall Respiratory Virus Mortality by IMD Quintile",
                   "All Cause Mortlality by IMD Quintile")
}

#create the model outputs list
model_outputs_list <- list(rsv_mild_ses_further_output, rsv_severe_ses_further_output, 
                           rsv_mortality_ses_further_output, flu_mild_ses_further_output, 
                           flu_severe_ses_further_output, flu_mortality_ses_further_output, 
                           all_cause_mortality_ses_further_output)

#adjust the model outputs list based on the conditions
if (study_start_date >= covid_season_min) {
  model_outputs_list <- c(model_outputs_list, list(covid_mild_ses_further_output,
                                                   covid_severe_ses_further_output,
                                                   covid_mortality_ses_further_output))
}
if (codelist_type == "sensitive") {
  model_outputs_list <- c(model_outputs_list, list(overall_resp_mild_ses_further_output,
                                                   overall_resp_severe_ses_further_output,
                                                   overall_resp_mortality_ses_further_output))
}
if (study_start_date >= covid_season_min & codelist_type == "sensitive") {
  model_outputs_list <- c(model_outputs_list, list(covid_mild_ses_further_output,
                                                   covid_severe_ses_further_output,
                                                   covid_mortality_ses_further_output,
                                                   overall_resp_mild_ses_further_output,
                                                   overall_resp_severe_ses_further_output,
                                                   overall_resp_mortality_ses_further_output))
}

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here("output", "results", "models"))

#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models"), "/", 
                            "further_ses_model_outputs_", cohort, "_", year(study_start_date), 
                            "_", year(study_end_date), "_", codelist_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models"), "/", 
                            "further_ses_model_outputs_", cohort, "_", year(study_start_date),
                            "_", year(study_end_date), "_", codelist_type, ".csv"))
}